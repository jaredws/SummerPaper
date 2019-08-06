## For compiling and interpreating the output data

library(dplyr)
library(ggplot2)
library(zoo)
library(imputeTS)
library(ssh)

session <- ssh_connect("jaredws@catan.eecis.udel.edu")


runNames <- c("MaxProfit","NoRealtor", "PerfectInfo")
randomness <- list("MaxProfit" == FALSE,"NoRealtor" = TRUE, "PerfectInfo" = FALSE)
lagPlay <- c(TRUE, FALSE)
iterations <- 50

realizedData <- list()

RUNS <- seq(1, 20)

house_sales_compiled <- as.data.frame(matrix(ncol = 18, nrow = 0))
iteration_stats_compiled <-
  as.data.frame(matrix(ncol = 12, nrow = 0))

for (lag in lagPlay) {
  for (version in runNames) {
    for (run in RUNS) {

      
      run <- as.numeric(run)
      ## Name the index in the list as the type of run
      ## Example:
      ## NoRealtor_TRUE_1
      ## Would be the first run of the NoRealtor version, with lagPlay active
      
        executing <- paste0(version, "_", lag, "_", run)
        fileName <-
          paste0("/usa/jaredws/", executing, "_realizedData.RData")
        
        ## Download the output data
        scp_download(session, fileName)
        
        load(paste0(getwd(), "/", executing, "_realizedData.RData"))
        
        cnames <- names(realizedData[[4]])
        cnames[4] <- "TotalCommission"
        colnames(realizedData[[4]]) <- cnames
        
        realizedData[[4]]$Realtor <- version
        realizedData[[4]]$LagPlay <-
          ifelse(lag, "LagPlay", "PlayEveryRound")
        realizedData[[4]]$Run <- run
        iteration_stats_compiled <-
          rbind(iteration_stats_compiled, realizedData[[4]])
        
        realizedData[[5]]$LagPlay <-
          ifelse(lag, "LagPlay", "PlayEveryRound")
        realizedData[[5]]$Run <- run
        house_sales_compiled <-
          rbind(house_sales_compiled, realizedData[[5]])
      
      ## Note: each execution of the system returns a list of the form:
      # RETURN <- list(
      #1   "BuyerList" <- BuyerList_noRealtor,
      #2   "SellerList" <- SellerList_noRealtor,
      #3   "RealtorList" <- RealtorList_noRealtor,
      #4   "Iteration_stats" <- iteration_stats_noRealtor,
      #5   "House_sale_stats" <- house_sales_noRealtor,
      #6   "BuyerInactive" <- BuyerInactive_noRealtor,
      #7   "SellerInactive" <- SellerInactive_noRealtor
      # )
    }
  }
}

ssh_disconnect(session)

save(iteration_stats_compiled,
     file = paste0(getwd(), "/iteration_stats_compiled_raw.RData"))
save(house_sales_compiled,
     file = paste0(getwd(), "/house_sales_compiled_raw.RData"))

load(paste0(getwd(), "/iteration_stats_compiled_raw.RData"))


house_sales_compiled %>%
  group_by(Realtor, LagPlay) %>%
  summarise(
    mean(BuyerSatisfaction),
    mean(SellerSatisfaction),
    mean(PriceIncreases),
    mean(TimeOnMarket)
  )

## Let's put TimeOnMarket and Average Bid (SalePrice) into the Iteration stats
## Take an average across any sales that occured
house_sales_av_ToM <- house_sales_compiled %>%
  ungroup() %>%
  group_by(Realtor, LagPlay, Run, Iteration) %>%
  summarise(
    av_Iter_ToM = mean(TimeOnMarket, na.rm = TRUE),
    av_SalePrice = mean(Bid, na.rm = TRUE)
  ) %>%
  arrange(Realtor, LagPlay, Run, Iteration) %>%
  select(Realtor, LagPlay, Run, Iteration, av_Iter_ToM, av_SalePrice)

iteration_stats_compiled <- iteration_stats_compiled %>%
  group_by(Realtor, LagPlay, Run) %>%
  arrange(Realtor, LagPlay, Run, Iteration) %>%
  left_join(house_sales_av_ToM,
            by = c("Realtor", "LagPlay", "Run", "Iteration"))

##na_ma replaces NA values with the weighted average of the k elements on both sides
iteration_stats_compiled <- iteration_stats_compiled %>%
  mutate(
    av_Iter_ToM_na_ma = na_ma(av_Iter_ToM, k = 1, weighting = "simple"),
    av_Iter_SalePrice_na_ma = na_ma(av_SalePrice, k = 1, weighting = "simple")
  ) %>%
  mutate(
    rollSales = cumsum(Sales),
    rollOffers = cumsum(Offers),
    rollPriceIncreases = cumsum(PriceIncreases),
    roll_av_ToM = cummean(av_Iter_ToM_na_ma),
    roll_av_SalePrice = cummean(av_Iter_SalePrice_na_ma)
  )


## Generate iteration averages across each run
### This will be very complicated, or could be, if I want to show Address wise averages

## for now, Focus on Iteration Statistics.

iteration_stats_Iter_Averages <- iteration_stats_compiled %>%
  ungroup() %>%
  group_by(Realtor, LagPlay, Iteration) %>%
  mutate(
    iter_av_Sales = mean(Sales),
    iter_av_Offers = mean(Offers),
    iter_av_PriceIncreases = mean(PriceIncreases),
    iter_av_TotalCommission = mean(TotalCommission)
  )

## Find 99% Confidence Intervals for the Total Commission plots
commissionCI <- iteration_stats_compiled %>%
  select(Realtor, LagPlay, Run, Iteration, TotalCommission) %>%
  group_by(Realtor, LagPlay, Iteration) %>%
  summarise(
    TC_mean = mean(TotalCommission),
    TC_stdev = sqrt(var(TotalCommission)),
    TC_error = qt(0.99, df = max(RUNS) - 1) * TC_stdev / max(RUNS),
    TC_lower = TC_mean - TC_error,
    TC_upper = TC_mean + TC_error
  )


## Commparing Rolling number of offers to rolling number of Sales
## need to gather on rollSales and rollOffers to get a comparable plot
iteration_stats_rollGather <- iteration_stats_compiled %>%
  group_by(Realtor, LagPlay, Run) %>%
  gather(rollOffers, rollSales, key = "rollStatType", value = "rollStatValue") %>%
  group_by(Realtor, LagPlay, Run, rollStatType) %>%
  mutate(rollTrend = (rollStatValue - min(rollStatValue))/ (max(rollStatValue) - min(rollStatValue)))


ggplot(iteration_stats_rollGather) +
  geom_smooth(aes(x = Iteration, y = rollTrend, color = Realtor)) +
  facet_grid(rows = vars(LagPlay), col = vars(rollStatType))
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = rollSales, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))


## Compare the rolling average time on the market both with respect to the Realtor's
## and to LagPlay
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = roll_av_ToM, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(
    x = Iteration,
    y = roll_av_ToM,
    color = Realtor,
    linetype = LagPlay
  ))

## Compare rolling average Sale Price and Total Commission
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = roll_av_SalePrice, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = TotalCommission, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))


ggplot(iteration_stats_Iter_Averages) +
  geom_line(aes(x = Iteration, y = iter_av_Sales, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(iteration_stats_Iter_Averages) +
  geom_line(aes(x = Iteration, y = iter_av_Offers, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(iteration_stats_Iter_Averages) +
  geom_line(aes(x = Iteration, y = iter_av_PriceIncreases, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(commissionCI,
       aes(
         x = Iteration,
         y = TC_mean,
         color = Realtor,
         group = Realtor
       )) +
  geom_line(aes(x = Iteration, y = TC_mean, color = Realtor)) +
  geom_ribbon(aes(x = Iteration, ymin = TC_lower, ymax = TC_upper)) +
  facet_grid(rows = vars(LagPlay))

house_sales_Iter_Averages <- house_sales_compiled %>%
  ungroup() %>%
  #filter(Run < 2) %>%
  group_by(Realtor, LagPlay, Iteration) %>%
  mutate(
    iter_av_SellerSatisfaction = mean(SellerSatisfaction),
    iter_av_BuyerSatisfaction = mean(BuyerSatisfaction),
    iter_av_SalePrice = mean(Bid)
  )

ggplot(house_sales_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = SellerSatisfaction, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(house_sales_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = BuyerSatisfaction, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(house_sales_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = Bid, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))


## Using the T test, we can clearly see a statistical difference in the SalePrice
x <-
  filter(house_sales_Iter_Averages, Realtor == "MaxProfit", LagPlay == "LagPlay")$iter_av_SalePrice
y <-
  filter(house_sales_Iter_Averages, Realtor == "PerfectInfo", LagPlay == "LagPlay")$iter_av_SalePrice
t.test(x, y)

## Using the T test, we can clearly see a statistical difference in the BuyerSatisfaction
x <-
  filter(house_sales_Iter_Averages, Realtor == "MaxProfit", LagPlay == "LagPlay")$iter_av_BuyerSatisfaction
y <-
  filter(house_sales_Iter_Averages, Realtor == "PerfectInfo", LagPlay == "LagPlay")$iter_av_BuyerSatisfaction
t.test(x, y)


## Using the T test, we can clearly see a statistical difference in the SellerSatisfaction
x <-
  filter(house_sales_Iter_Averages, Realtor == "MaxProfit")$iter_av_SellerSatisfaction
y <-
  filter(house_sales_Iter_Averages, Realtor == "PerfectInfo")$iter_av_SellerSatisfaction
t.test(x, y)

## Using the T test, we can clearly see a statistical difference in the number of Offers
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$Offers
y <-
  filter(iteration_stats_compiled,
         Realtor == "PerfectInfo",
         LagPlay == "LagPlay")$Offers
t.test(x, y)

## To show the same number of buyers and sellers are generated for each LagPlay and Realtor per Run,
## I need to show the sum of Buyers less the sales...
## need to think more
iteration_stats_summary <- iteration_stats_compiled %>%
  ungroup() %>%
  arrange(Run, Realtor, LagPlay, Iteration) %>%
  group_by(Run, Realtor, LagPlay, Iteration) %>%
  mutate(
    gBuyers = nBuyers - lag(nBuyers, 1, order_by = c(Iteration)) + lag(Sales, 1, order_by = c(Iteration)),
    gSellers = nSellers - lag(nSellers, 1, order_by = c(Iteration)) + lag(Sales, 1, order_by = c(Iteration))
  ) %>%
  summarise(tBuyers = sum(gBuyers),
            tSellers = sum(gSellers))

ggplot(iteration_stats_summary) +
  geom_point(aes(x = Run, y = tBuyers, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
