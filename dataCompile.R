## For compiling and interpreating the output data

library(dplyr)
library(ggplot2)
library(zoo)
library(imputeTS)
library(ssh)
library(ggthemes)
library(tidyverse)
theme_set(theme_bw())


runNames <- c("MaxProfit", "PerfectInfo")#, "RandomDraw5","NoRealtor")
randomness <- list("MaxProfit" == FALSE, "RandomDraw5" = TRUE,"NoRealtor" = TRUE, "PerfectInfo" = FALSE)
lagPlay <- c(TRUE, FALSE)
iterations <- 100

realizedData <- list()

RUNS <- seq(1, 20)

#### Downlowd and Compile the Data ####
session <- ssh_connect("jaredws@catan.eecis.udel.edu")

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
          paste0("/usa/jaredws/", executing, "_realizedData_newDraws.RData")
        
        ## Download the output data
        scp_download(session, fileName)
        
        load(paste0(getwd(), "/", executing, "_realizedData_newDraws.RData"))
        
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
     file = paste0(getwd(), "/iteration_stats_compiled_raw_newDraws.RData"))
save(house_sales_compiled,
     file = paste0(getwd(), "/house_sales_compiled_raw_newDraws.RData"))

#load(paste0(getwd(), "/iteration_stats_compiled_raw.RData"))

#### Clean and Organize the Data ####

load(paste0(getwd(), "/iteration_stats_compiled_raw_newDraws.RData"))
load(paste0(getwd(), "/house_sales_compiled_raw_newDraws.RData"))


load(paste0(getwd(), "/iteration_stats_compiled_raw.RData"))
load(paste0(getwd(), "/house_sales_compiled_raw.RData"))

## Rename PerfectInfo as MaxSatisfaction Realtor
levels(house_sales_compiled$Realtor) <- c("MaxProfit", "RandomDraw3", "MaxSatisfaction", "RandomDraw5")
##  "MaxProfit"   "NoRealtor"   "PerfectInfo"
iteration_stats_compiled$Realtor <- as.factor(iteration_stats_compiled$Realtor)
levels(iteration_stats_compiled$Realtor) <- c("MaxProfit", "RandomDraw3", "MaxSatisfaction", "RandomDraw5")
## "MaxProfit"   "NoRealtor"   "PerfectInfo"

house_sales_compiled %>%
  dplyr::group_by(Realtor, LagPlay) %>%
  summarise(
    mean(BuyerSatisfaction),
    mean(SellerSatisfaction),
    mean(PriceIncreases),
    mean(TimeOnMarket)
  )

## Let's put TimeOnMarket and Average Bid (SalePrice) into the Iteration stats
## Take an average across any sales that occured
house_sales_av_ToM <- house_sales_compiled %>%
  dplyr::group_by(Realtor, LagPlay, Run, Iteration) %>%
  dplyr::summarise(
    av_Iter_ToM = mean(TimeOnMarket, na.rm = TRUE),
    av_SalePrice = mean(Bid, na.rm = TRUE)
  ) %>%
  dplyr::arrange(Realtor, LagPlay, Run, Iteration) %>%
  dplyr::select(Realtor, LagPlay, Run, Iteration, av_Iter_ToM, av_SalePrice)

iteration_stats_compiled <- iteration_stats_compiled %>%
  dplyr::group_by(Realtor, LagPlay, Run) %>%
  dplyr::arrange(Realtor, LagPlay, Run, Iteration) %>%
  inner_join(house_sales_av_ToM,
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
  dplyr::group_by(Realtor, LagPlay, Iteration) %>%
  mutate(
    iter_av_Sales = mean(Sales),
    iter_av_Offers = mean(Offers),
    iter_av_PriceIncreases = mean(PriceIncreases),
    iter_av_TotalCommission = mean(TotalCommission)
  )

## Find 99% Confidence Intervals for the Total Commission plots
commissionCI <- iteration_stats_compiled %>%
  dplyr::select(Realtor, LagPlay, Run, Iteration, TotalCommission) %>%
  dplyr::group_by(Realtor, LagPlay, Iteration) %>%
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
  dplyr::group_by(Realtor, LagPlay, Run) %>%
  gather(rollOffers, rollSales, key = "rollStatType", value = "rollStatValue") %>%
  dplyr::group_by(Realtor, LagPlay, Run, rollStatType) %>%
  mutate(rollTrend = (rollStatValue - min(rollStatValue))/ (max(rollStatValue) - min(rollStatValue)))


house_sales_Iter_Averages <- house_sales_compiled %>%
  ungroup() %>%
  #filter(Run < 2) %>%
  dplyr::group_by(Realtor, LagPlay, Iteration) %>%
  mutate(
    iter_av_SellerSatisfaction = mean(SellerSatisfaction),
    iter_av_BuyerSatisfaction = mean(BuyerSatisfaction),
    iter_av_SalePrice = mean(Bid)
  )


#### PLOTS ####

ggplot(iteration_stats_rollGather) +
  geom_smooth(aes(x = Iteration, y = rollTrend, color = Realtor)) +
  facet_grid(rows = vars(LagPlay), col = vars(rollStatType))


## Not used, iter_av_Sales_n_Offers
#####
ggplot(iteration_stats_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = iter_av_Sales, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(iteration_stats_Iter_Averages) +
  geom_line(aes(x = Iteration, y = iter_av_Offers, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
####    ####

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




## To show the same number of buyers and sellers are generated for each LagPlay and Realtor per Run,
## I need to show the sum of Buyers less the sales...
## need to think more
# iteration_stats_summary <- iteration_stats_compiled %>%
#   ungroup() %>%
#   arrange(Run, Realtor, LagPlay, Iteration) %>%
#   group_by(Run, Realtor, LagPlay, Iteration) %>%
#   mutate(
#     gBuyers = nBuyers - lag(nBuyers, 1, order_by = c(Iteration)) + lag(Sales, 1, order_by = c(Iteration)),
#     gSellers = nSellers - lag(nSellers, 1, order_by = c(Iteration)) + lag(Sales, 1, order_by = c(Iteration))
#   ) %>%
#   summarise(tBuyers = sum(gBuyers),
#             tSellers = sum(gSellers))
# 
# ggplot(iteration_stats_summary) +
#   geom_point(aes(x = Run, y = tBuyers, color = Realtor)) +
#   facet_grid(rows = vars(LagPlay))




#### In Paper Order: #### 
#'Cumulative number of sales 
#'cumulative number of offers
#'cumulative number of price increases
#'
#'rolling average sale price
#'iteration average sale price
#'iteration average bid price
#'cumulative commision
#'
#'average time on market
#'average seller satisfaction
#'average buyer satisfaction
#'

## Cumulative Number of Sales

ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = rollSales, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))

## Using the T test, we can clearly see a statistical difference in the number of Sales
# LagPlay
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$Sales
y <-
  filter(iteration_stats_compiled,
         Realtor == "MaxSatisfaction",
         LagPlay == "LagPlay")$Sales
t.test(x, y)


## Play every round
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "PlayEveryRound")$Sales
y <-
  filter(iteration_stats_compiled,
         Realtor == "MaxSatisfaction",
         LagPlay == "PlayEveryRound")$Sales
t.test(x, y)


## Cumulative Number of Offers ##
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = rollOffers, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))

## Using the T test, we can clearly see a statistical difference in the number of Offers
# LagPlay
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$Offers
y <-
  filter(iteration_stats_compiled,
         Realtor == "MaxSatisfaction",
         LagPlay == "LagPlay")$Offers
t.test(x, y)
## Play every round
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "PlayEveryRound")$Offers
y <-
  filter(iteration_stats_compiled,
         Realtor == "MaxSatisfaction",
         LagPlay == "PlayEveryRound")$Offers
t.test(x, y)
##

## Cumulative Number of price increases ##
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = rollPriceIncreases, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))

## Using the T test, we can clearly see a statistical difference in the number of Offers
# LagPlay
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$PriceIncreases
y <-
  filter(iteration_stats_compiled,
         Realtor == "MaxSatisfaction",
         LagPlay == "LagPlay")$PriceIncreases
t.test(x, y)
## Play every round
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "PlayEveryRound")$PriceIncreases
y <-
  filter(iteration_stats_compiled,
         Realtor == "MaxSatisfaction",
         LagPlay == "PlayEveryRound")$PriceIncreases
t.test(x, y)
##

## Rolling average Sale price
## Compare rolling average Sale Price and Total Commission
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = roll_av_SalePrice, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))


## Iteration average sale price
ggplot(house_sales_Iter_Averages) +
  #geom_smooth(aes(x = Iteration, y = Price, color = Realtor)) +
  geom_point(aes(x = Iteration, y = Price, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
## Using the T test, we cannot see a statistical difference in the sale price between the two realtors
# LagPlay
x <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$Price
y <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxSatisfaction",
         LagPlay == "LagPlay")$Price
t.test(x, y)
## Play every round
x <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxProfit",
         LagPlay == "PlayEveryRound")$Price
y <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxSatisfaction",
         LagPlay == "PlayEveryRound")$Price
t.test(x, y)
##


## Iteration average bid price
ggplot(house_sales_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = Bid, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
## Using the T test, we cannot see a statistical difference in the bid price
# LagPlay
x <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$Bid
y <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxSatisfaction",
         LagPlay == "LagPlay")$Bid
t.test(x, y)
## Play every round
x <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxProfit",
         LagPlay == "PlayEveryRound")$Bid
y <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxSatisfaction",
         LagPlay == "PlayEveryRound")$Bid
t.test(x, y)
##


## Cumulative average Total Commission
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = TotalCommission, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))


####

## Iteration Average Time on Market (2 plots)
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

## Using the T test, we cannot see a statistical difference in the rolling average time on market
# LagPlay
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$roll_av_ToM
y <-
  filter(iteration_stats_compiled,
         Realtor == "MaxSatisfaction",
         LagPlay == "LagPlay")$roll_av_ToM
t.test(x, y)
## Play every round
x <-
  filter(iteration_stats_compiled,
         Realtor == "MaxProfit",
         LagPlay == "PlayEveryRound")$roll_av_ToM
y <-
  filter(iteration_stats_compiled,
         Realtor == "MaxSatisfaction",
         LagPlay == "PlayEveryRound")$roll_av_ToM
t.test(x, y)


## Iteration average Seller Satisfaction
ggplot(house_sales_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = SellerSatisfaction, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
## Using the T test, we cannot see a statistical difference in the seller satisfaction
# LagPlay
x <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$SellerSatisfaction
y <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxSatisfaction",
         LagPlay == "LagPlay")$SellerSatisfaction
t.test(x, y)
## Play every round
x <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxProfit",
         LagPlay == "PlayEveryRound")$SellerSatisfaction
y <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxSatisfaction",
         LagPlay == "PlayEveryRound")$SellerSatisfaction
t.test(x, y)



## Iteration average Buyer Satisfaction
ggplot(house_sales_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = BuyerSatisfaction, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
## Using the T test, we cannot see a statistical difference in the buyer satisfaction
# LagPlay
x <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxProfit",
         LagPlay == "LagPlay")$BuyerSatisfaction
y <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxSatisfaction",
         LagPlay == "LagPlay")$BuyerSatisfaction
t.test(x, y)
## Play every round
x <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxProfit",
         LagPlay == "PlayEveryRound")$BuyerSatisfaction
y <-
  filter(house_sales_Iter_Averages,
         Realtor == "MaxSatisfaction",
         LagPlay == "PlayEveryRound")$BuyerSatisfaction
t.test(x, y)



####
