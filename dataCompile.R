## For compiling and interpreating the output data

library(dplyr)
library(ggplot2)
library(zoo)
library(imputeTS)
library(ssh)
library(ggthemes)
library(ggpubr)
library(tidyverse)
library(scales)
theme_set(theme_economist())


runNames <- c("MaxProfit", "PerfectInfo", "RandomDraw5","NoRealtor")
randomness <- list("MaxProfit" == FALSE, "RandomDraw5" = TRUE,"NoRealtor" = TRUE, "PerfectInfo" = FALSE)
lagPlay <- c(TRUE, FALSE)
iterations <- 100

dataToDraw <- read.csv("dataToDraw.csv")

strategies <- c("MaxProfit", "MaxSatisfaction", "RandomDraw5", "RandomDraw3")
scenarios <- c("LagPlay", "PlayEveryRound")

strategyTest <- function(data,
                         varsToTest,
                         strategyList,
                         scenarioList) {
  testDF <- data.frame()
  for (i in 1:(length(strategyList) - 1)) {
    for (j in (i + 1):length(strategyList)) {
      for (s in 1:length(scenarioList)) {
        row <- list()
        strat1 <- strategyList[i]
        strat2 <- strategyList[j]
        scenario <- scenarioList[s]
        row["Strategy 1"] <- strat1
        row["Strategy 2"] <- strat2
        row["Scenario"] <- scenario
        for (var in varsToTest) {
          x <-   data %>%
            ungroup() %>%
            dplyr::filter(Realtor == strat1,
                          LagPlay == scenario) %>%
            dplyr::select(var) %>%
            as_vector()
          y <- data %>%
            ungroup() %>%
            dplyr::filter(Realtor == strat2,
                          LagPlay == scenario) %>%
            dplyr::select(var) %>%
            as_vector()
          row[paste0("p-", var)] <-
            round(as.numeric(t.test(x, y)$p.val), 4)
          meanDiff <-
            round(as.numeric(t.test(x, y)$estimate[1]) - as.numeric(t.test(x, y)$estimate[2]),
                  4)
          row[paste0("Mean Diff. ", var)] <- meanDiff
        }
        testDF <- rbind(testDF, as.data.frame(row))
      }
    }
  }
  
  return(testDF)
  
}

##### Other #####
dataMeanSalePrice <- dataToDraw %>%
  as.data.frame() %>%
  filter(Event == "Sold") %>%
  summarise(av_Price_Sold = mean(Price)) %>%
  as.numeric()
dataMeanListPrice <- dataToDraw %>%
  as.data.frame() %>%
  filter(Event == "Listed for sale") %>%
  summarise(av_Price_Sold = mean(Price)) %>%
  as.numeric()

dataMeans <- dataToDraw %>%
  dplyr::select(Date,Event,Price) %>%
  group_by(Date, Event) %>%
  mutate(
    rMean_Price = roll_mean(Price, 5)
  )

ggplot(dataMeans) +
  geom_line(aes(x = Date, y = rMean_Price, color = Event))


## Statistical difference between list and sale price in Newark
x <- dataToDraw %>%
  as.data.frame() %>%
  dplyr::filter(Event == "Listed for sale") %>%
  dplyr::select(Price)
y <- dataToDraw %>%
  as.data.frame() %>%
  dplyr::filter(Event == "Sold") %>%
  dplyr::select(Price)
dataPriceTest <- t.test(x, y)

dataListTest <- t.test(x)
dataSaleTest <- t.test(y)

RUNS <- seq(1, 20)

#### Downlowd and Compile the Data ####
session <- ssh_connect("jaredws@catan.eecis.udel.edu")
realizedData <- list()
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

levels(house_sales_compiled$Realtor)

## Check for repeat sales within a system
houseSalesDistinct <- house_sales_compiled %>%
  distinct(Realtor,LagPlay,Run,Address, Seller, .keep_all = T) %>%
  select(Realtor, LagPlay, Run, Address, Seller, Buyer, Iteration)

houseSaleRepeat <- anti_join(house_sales_compiled, houseSalesDistinct, by = colnames(houseSalesDistinct))

hsc_filtered <- house_sales_compiled %>%
  filter(Realtor == "MaxProfit",
         LagPlay == "LagPlay",
         Run == 1,
         Iteration <= 25) %>%
  select(Realtor, Iteration, Address, Seller, Buyer, PriceIncreases, Price, Bid)

# houseSales <- houseSales %>%
#   select(Realtor, Iteration, Address, Seller, Buyer, PriceIncreases, Price, Bid)


#load(paste0(getwd(), "/iteration_stats_compiled_raw.RData"))
#load(paste0(getwd(), "/house_sales_compiled_raw.RData"))

## Rename PerfectInfo as MaxSatisfaction Realtor

levels(house_sales_compiled$Realtor) <- c("MaxProfit", "MaxSatisfaction", "RandomDraw5", "RandomDraw3")
##  "MaxProfit"   "NoRealtor"   "PerfectInfo"
iteration_stats_compiled$Realtor <- as.factor(iteration_stats_compiled$Realtor)
levels(iteration_stats_compiled$Realtor)
levels(iteration_stats_compiled$Realtor) <- c("MaxProfit", "RandomDraw3", "MaxSatisfaction", "RandomDraw5")
## "MaxProfit"   "NoRealtor"   "PerfectInfo"

##### Organize and Compile The Data #####
house_sales_compiled %>%
  dplyr::group_by(Realtor, LagPlay) %>%
  summarise(
    mean(BuyerSatisfaction),
    mean(SellerSatisfaction),
    mean(PriceIncreases),
    mean(TimeOnMarket)
  )

## Let's put TimeOnMarket and Average Bid (SalePrice) into the Iteration stats
## Let's also consider the number of times houses sold for less than listing
## Take an average across any sales that occured


house_sales_compiled <- house_sales_compiled %>%
  mutate(
    price_bid_diff = Price - Bid,
    ListPrice = Price/((1.05)^(PriceIncreases)),
    bid_list_diff = Bid - ListPrice,
    soldUnderAsk = ifelse(price_bid_diff > 0, 1, 0),
    soldAtAsk = ifelse(price_bid_diff == 0, 1, 0),
    soldOverAsk = ifelse(price_bid_diff < 0, 1, 0),
    soldUnderListing = ifelse(bid_list_diff < 0, 1 , 0),
    soldAtListing = ifelse(bid_list_diff == 0, 1, 0),
    soldOverListing = ifelse(bid_list_diff > 0, 1, 0)
  )

house_sales_compiled %>%
  dplyr::group_by(Realtor, LagPlay) %>%
  dplyr::select(soldUnderAsk, soldAtAsk, soldOverAsk, soldUnderListing, soldAtListing, soldOverListing) %>%
  summarise_all(list(mean))

##### More data manipulation #####

house_sales_av_ToM <- house_sales_compiled %>%
  dplyr::group_by(Realtor, LagPlay, Run, Iteration) %>%
  dplyr::summarise(
    av_Iter_ToM = mean(TimeOnMarket, na.rm = TRUE),
    av_SalePrice = mean(Bid, na.rm = TRUE),
    av_SalesUnder = mean(soldUnderAsk, na.rm = TRUE),
    av_SaleUnderAmmount = mean(soldUnderAsk*price_bid_diff/soldUnderAsk, na.rm = TRUE)
  ) %>%
  dplyr::arrange(Realtor, LagPlay, Run, Iteration) %>%
  dplyr::select(Realtor, LagPlay, Run, Iteration, av_Iter_ToM, av_SalePrice,av_SalesUnder,av_SaleUnderAmmount)

iteration_stats_compiled <- iteration_stats_compiled %>%
  dplyr::group_by(Realtor, LagPlay, Run) %>%
  dplyr::arrange(Realtor, LagPlay, Run, Iteration) %>%
  inner_join(house_sales_av_ToM,
            by = c("Realtor", "LagPlay", "Run", "Iteration"))

##na_ma replaces NA values with the weighted average of the k elements on both sides
iteration_stats_compiled <- iteration_stats_compiled %>%
  dplyr::group_by(Realtor, LagPlay, Run) %>%
  dplyr::arrange(Realtor, LagPlay, Run, Iteration) %>%
  mutate(
    av_Iter_ToM_na_ma = na_ma(av_Iter_ToM, k = 1, weighting = "simple"),
    av_Iter_SalePrice_na_ma = na_ma(av_SalePrice, k = 1, weighting = "simple"),
    av_Iter_SalesUnder_na_ma = na_ma(av_SalesUnder, k = 1, weighting = "simple"),
    av_Iter_SaleUnderAmmount_na_ma = na_ma(av_SaleUnderAmmount, k = 1, weighting = "simple")
  ) %>%
  mutate(
    cumulativeSales = cumsum(Sales),
    cumulativeOffers = cumsum(Offers),
    cumulativePriceIncreases = cumsum(PriceIncreases),
    cumulativeSalesUnder = cumsum(av_SalesUnder),
    cmean_av_ToM = cummean(av_Iter_ToM_na_ma),
    cmean_av_SalePrice = cummean(av_Iter_SalePrice_na_ma),
    cmean_av_SaleUnderAmmount = cummean(av_SaleUnderAmmount),
    DataMeanListPrice = dataMeanListPrice,
    DataMeanSalePrice = dataMeanSalePrice
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
    iter_av_nSellers = mean(nSellers),
    iter_av_nBuyers = mean(nBuyers),
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
## need to gather on cumulativeSales and cumulativeOffers to get a comparable plot
iteration_stats_rollGather <- iteration_stats_compiled %>%
  dplyr::group_by(Realtor, LagPlay, Run) %>%
  gather(cumulativeOffers, cumulativeSales, key = "rollStatType", value = "rollStatValue") %>%
  dplyr::group_by(Realtor, LagPlay, Run, rollStatType) %>%
  mutate(rollTrend = (rollStatValue - min(rollStatValue))/ (max(rollStatValue) - min(rollStatValue)))


house_sales_Iter_Averages <- house_sales_compiled %>%
  ungroup() %>%
  dplyr::group_by(Realtor, LagPlay, Iteration) %>%
  mutate(
    iter_av_SellerSatisfaction = mean(SellerSatisfaction),
    iter_av_BuyerSatisfaction = mean(BuyerSatisfaction),
    iter_av_SalePrice = mean(Bid),
    iter_av_SalesUnder = mean(soldUnderAsk),
    DataMeanListPrice = dataMeanListPrice,
    DataMeanSalePrice = dataMeanSalePrice
  )

#### PLOTS ####

ggplot(iteration_stats_rollGather) +
  geom_smooth(aes(x = Iteration, y = rollTrend, color = Realtor)) +
  facet_grid(rows = vars(LagPlay), col = vars(rollStatType))


## Not used, iter_av_Sales_n_Offers
##### Average Number of Sales and Offers by Iteration #####
ggplot(iteration_stats_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = iter_av_Sales, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
ggplot(iteration_stats_Iter_Averages) +
  geom_line(aes(x = Iteration, y = iter_av_Offers, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))
#### Manual CI on Price Increases   ####

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


##### Unused #####

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

##### Cumulative Number of Sales #####
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = cumulativeSales, color = Realtor), size = 2) +
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Cumulative Number of Sales") +
  scale_x_continuous(name = "Iteration") +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))


## Using the T test
## Statistical difference between iteration Average number of houses sold by either maximizing realtor in LagPlay

n_salesTest <- strategyTest(iteration_stats_Iter_Averages,
                          c("iter_av_Sales"),
                          c("MaxProfit","MaxSatisfaction","RandomDraw5","RandomDraw3"),
                          c("LagPlay"))
view(n_salesTest)
write_csv(n_salesTest, path = paste0(getwd(),"salesTest.csv"))

### Need to include 
#### Average number of buyer's to be expected}
#### Average number of seller's to be expected} generated.

##### Buyer and Seller Drawing Frequency #####
buyerDraws <- read_csv("saleFrequency.csv")
sellerDraws <- read_csv("listingFrequency.csv")

numBuyerExpectation <- as.numeric(t(buyerDraws$freq) %*% buyerDraws$SalesToday)
numBuyerExpectation
numSellerExpectation <- as.numeric(t(sellerDraws$freq) %*% sellerDraws$ListingsToday)
numSellerExpectation

ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = Sales, color = Realtor)) +
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


##### Cumulative Number of Offers #####
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = cumulativeOffers, color = Realtor), size = 2) +
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Cumulative Number of Offers") +
  scale_x_continuous(name = "Iteration") +
  theme(axis.title.y = element_text(face = "bold", size = 14),
        axis.title.x = element_text(face = "bold", size = 14),
        legend.title = element_text(face = "bold", size = 14))
n_offersTest <- strategyTest(iteration_stats_compiled,
                            c("Offers"),
                            strategies,
                            scenarios)
view(n_offersTest)

##### Cumulative Number of price increases ##### 
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = PriceIncreases, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))

n_priceIncreasesTest <- strategyTest(iteration_stats_compiled,
                             c("PriceIncreases"),
                             strategies,
                             scenarios)
view(n_priceIncreasesTest)


##### Sales Under the List Price ##### 
#### REALLY NEED TO ACCOUNT FOR INCREASES IN PRICE HERE
#### MUST FIND THE BASE-LIST PRICE OF EVERY PROPERTY, WHICH I SHOULD HAVE STORED AT ORIGINATION
#### I CAN PULL IT OUT USING THE NUMBER OF PRICE INCREASES AND THE KNOWN 5% INCREASE
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = cumulativeSalesUnder, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))

ggplot(iteration_stats_compiled) +
  geom_line(aes(x = Iteration, y = cmean_av_SaleUnderAmmount, color = Realtor)) +
  facet_grid(rows = vars(LagPlay))

n_priceIncreasesTest <- strategyTest(iteration_stats_compiled,
                                     c("PriceIncreases"),
                                     strategies,
                                     scenarios)
view(n_priceIncreasesTest)

##### Rolling average Sale price #####
## Compare rolling average Sale Price and Total Commission
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = cmean_av_SalePrice, color = Realtor)) +
  #geom_point(aes(x = Iteration, y = cmean_av_SalePrice, color = Realtor)) +
  geom_line(aes(x=Iteration, y = DataMeanListPrice*0.9, color = "Data Mean List Price")) +
  geom_line(aes(x=Iteration, y = DataMeanSalePrice*0.9, color = "Data Mean Sale Price")) + 
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Cumulative Mean Sale Price", labels = dollar_format(prefix = "$")) +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))


## Iteration average sale price
ggplot(house_sales_compiled) +
  geom_smooth(aes(x = Iteration, y = Price, color = Realtor), size = 2) +
  #geom_point(aes(x = Iteration, y = Price, color = Realtor)) +
  # geom_line(
  #   aes(
  #     x = Iteration,
  #     y = dataMeanListPrice * 0.9,
  #     color = "Data Mean List Price"
  #   ),
  #   size = 2
  # ) +
  # geom_line(
  #   aes(
  #     x = Iteration,
  #     y = dataMeanSalePrice * 0.9,
  #     color = "Data Mean Sale Price"
  #   ),
  #   size = 2
  # ) +
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Iteration Mean Sale Price", labels = dollar_format(prefix = "$")) +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))


## Using the T test, we can see a statistical difference in the sale price between the two realtors
# LagPlay

vars2test <- c("Price", "BuyerSatisfaction", "SellerSatisfaction", "PriceIncreases","soldOverListing", "soldAtAsk", "soldAtListing")

stratTest <- strategyTest(house_sales_compiled,
                          vars2test,
                          strategies,
                          scenarios)

write_csv(stratTest, path = paste0(getwd(),"strategyTest.csv"))

##### Sale price #####
### GEOM_SMOOTH IS NOT GOOD ENOUHG
### CALCULATE OUT THE CONFIDENCES AND WHATNOT.
## Find 99% Confidence Intervals for the Total Commission plots
salePriceCI <- house_sales_compiled %>%
  #filter(Price < 500000) %>%
  dplyr::select(Realtor, LagPlay, Run, Iteration, Price) %>%
  dplyr::group_by(Realtor, LagPlay, Iteration) %>%
  summarise(
    Av_Price = mean(Price),
    Price_stdev = sqrt(var(Price)),
    Price_error = qt(0.95, df = max(RUNS) - 1) * Price_stdev / max(RUNS),
    Price_lower = Av_Price - Price_error,
    Price_upper = Av_Price + Price_error
  )

  ggplot(salePriceCI,
         aes(
           x = Iteration,
           y = Av_Price,
           color = Realtor,
           group = Realtor
         )) +
    geom_line(aes(x = Iteration, y = Av_Price, color = Realtor)) +
    geom_line(aes(x=Iteration, y = dataMeanPrice)) + 
    geom_ribbon(aes(x = Iteration, ymin = Price_lower, ymax = Price_upper)) +
    facet_grid(rows = vars(LagPlay))

salePriceSummary <- house_sales_compiled %>%
  ungroup() %>%
  dplyr::select(Realtor, LagPlay, Run, Iteration, Price) %>%
  dplyr::group_by(Realtor, LagPlay) %>%
  summarise(
    Av_Price = mean(Price),
    Price_stdev = sqrt(var(Price)),
    Price_error = qt(0.9, df = max(RUNS) - 1) * Price_stdev / max(RUNS),
    Price_lower = Av_Price - Price_error,
    Price_upper = Av_Price + Price_error
  )

### What if I observed the frequencies?

ggplot(salePriceCI) +
  geom_smooth(aes(x = Iteration, y = Av_Price, color = Realtor)) +
  #geom_point(aes(x = Iteration, y = Price, color = Realtor)) +
  #geom_line(aes(x=Iteration, y = DataMeanPrice)) + 
  facet_grid(rows = vars(LagPlay))


priceTest <- strategyTest(house_sales_compiled,
                                     c("Price"),
                                     strategies,
                                     scenarios)
view(priceTest)
##


##### Iteration average bid price #####
ggplot(house_sales_compiled) +
  geom_smooth(aes(x = Iteration, y = Bid, color = Realtor), size = 2) +
  scale_y_continuous(name = "Iteration Mean Bid Price", labels = dollar_format(prefix = "$")) +
  facet_grid(rows = vars(LagPlay)) +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))

bidTest <- strategyTest(house_sales_compiled,
                          c("Bid"),
                          strategies,
                          scenarios)
view(bidTest)

#### Cumulative average Total Commission ####
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = TotalCommission, color = Realtor), size = 2) +
  scale_y_continuous(name = "Iteration Average Total Commission", labels = dollar_format(prefix = "$")) +
  facet_grid(rows = vars(LagPlay)) +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))


#### Number of Buyers and Sellers in the market ####
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = nSellers, color = Realtor, linetype = "dashed"), size = 2) +
  geom_smooth(aes(x = Iteration, y = nBuyers, color = Realtor, linetype = "solid"), size = 2) +
  scale_y_continuous(name = "Active Agents: Seller=Dashed, Buyer=Solid") +
  facet_grid(rows = vars(LagPlay)) +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))



##### Iteration Average Time on Market (2 plots) #####
## Compare the rolling average time on the market both with respect to the Realtor's
## and to LagPlay
ggplot(iteration_stats_compiled) +
  geom_smooth(aes(x = Iteration, y = cmean_av_ToM, color = Realtor), size = 2) +
  scale_y_continuous(name = "Cumulative Average Time on Market") +
  facet_grid(rows = vars(LagPlay)) +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))

ggplot(iteration_stats_compiled) +
  geom_smooth(aes(
    x = Iteration,
    y = cmean_av_ToM,
    color = Realtor,
    linetype = LagPlay
  ), size = 2) +
  scale_y_continuous(name = "Cumulative Average Time on Market") +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))

## Using the T test, we can see a statistical difference in the rolling average time on market
# LagPlay
av_ToMTest <- strategyTest(iteration_stats_compiled,
                                     c("av_Iter_ToM_na_ma"),
                                     strategies,
                                     scenarios)
view(av_ToMTest)


##### Iteration average Seller Satisfaction #####
ggplot(house_sales_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = SellerSatisfaction, color = Realtor), size = 2) +
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Iteration Mean Seller Satisfaction") +
  scale_x_continuous(name = "Iteration") +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))
## Using the T test, we cannot see a statistical difference in the seller satisfaction
# LagPlay

sellerSatisfactionTest <- strategyTest(house_sales_compiled,
                                     c("SellerSatisfaction"),
                                     strategies,
                                     scenarios)
view(sellerSatisfactionTest)


##### Iteration average Buyer Satisfaction #####
ggplot(house_sales_Iter_Averages) +
  geom_smooth(aes(x = Iteration, y = BuyerSatisfaction, color = Realtor)) +
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Iteration Mean Buyer Satisfaction") +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))

buyerSatisfactionTest <- strategyTest(house_sales_compiled,
                                       c("BuyerSatisfaction"),
                                       strategies,
                                       scenarios)
view(buyerSatisfactionTest)



##### Percent Under Original Listing #####
ggplot(house_sales_compiled) +
  geom_smooth(aes(x = Iteration, y = soldUnderListing, color = Realtor), size = 2) +
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Sold Under Listing", labels = scales::percent) +
  scale_x_continuous(name = "Iteration") +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))

soldUnderListTest <- strategyTest(house_sales_compiled,
                                       c("soldUnderListing"),
                                       strategies,
                                       scenarios)
view(soldUnderListTest)

##### Percent Under Day's Asking ####
ggplot(house_sales_compiled) +
  geom_smooth(aes(x = Iteration, y = soldUnderAsk, color = Realtor), size = 2) +
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Sold Under Asking", labels = scales::percent) +
  scale_x_continuous(name = "Iteration") +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))


soldUnderAskTest <- strategyTest(house_sales_compiled,
                                  c("soldUnderAsk"),
                                  strategies,
                                  scenarios)
view(soldUnderAskTest)


##### Amount sold over Listing
ggplot(house_sales_compiled) +
  geom_smooth(aes(x = Iteration, y = bid_list_diff, color = Realtor), size = 2) +
  facet_grid(rows = vars(LagPlay)) +
  scale_y_continuous(name = "Amount over Orignal Listing", labels = dollar_format(prefix = "$")) +
  scale_x_continuous(name = "Iteration") +
  theme(axis.title.y = element_text(face = "bold", size = 18),
        axis.title.x = element_text(face = "bold", size = 18),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        strip.text = element_text(size = 18),
        legend.text = element_text(size = 18),
        legend.title = element_text(face = "bold", size = 18))


bid_list_diffTest <- strategyTest(house_sales_compiled,
                                 c("bid_list_diff"),
                                 strategies,
                                 scenarios)
view(bid_list_diffTest)




####











