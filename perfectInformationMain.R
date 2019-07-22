## Main System executable
## RUN THIS ONE WITH PERFECT INFORMATION

RANDOM = FALSE
LagPlay = TRUE
seed = 1234

## Number of iterations to run
iter <- 20

## TODO
## Maybe we need to update sellers on buyers who have left as well. 
## Current belief: Since newly added buyers and sellers are given the current
## market averages, the history is preserved - it may be more witholding than anything
## Buyer Seller Inactive
## I need a way of preserving the original price for a house => use the original dataset

library(dplyr)
library(FuzzyNumbers)
library(tidyverse)

source("requirementClass.R")
source("buyerClass.R")
source("realtorClass.R")
source("sellerClass.R")
source("generateBuyersAndSellers.R")

start <- ()


set.seed(seed)

suppressWarnings("In if (!is.na(offer)) { : the condition has length > 1 and only the first element will be used")
suppressWarnings("In max(seller@CurrentOffers$AV) : no non-missing arguments to max; returning -Inf")
suppressWarnings("In max(seller@CurrentOffers$Bid) : no non-missing arguments to max; returning -Inf")

## Still some data cleaning around prices and organizing the list-sale pairs
## Will do that later 
## TODO
dataToDraw <- read.csv(paste0(getwd(), "/Data/dataToDraw.csv"))
dataToDraw <- filter(dataToDraw, Price <= 500000)
## Be sure date is in the proper format
dataToDraw$Date <- as.Date.factor(dataToDraw$Date)
dataToDraw$SqrFt <- as.numeric(dataToDraw$SqrFt)


## Parameters for Feedback and initial information at the Realtor's disposal
## Can have variants within the system
Clarity <- 3
## Giving up on learning, so this is immaterial
Responsiveness <- 3

## Input for Fuzziness
fuzzy_param <- list(
  "S1min" = 0.9,
  "S1max" = 0.9,
  "S2min" = 1.10,
  "S2max" = 1.10
)

## Parameters for Normal distributions of Preferences
seller_params <- list(
  Price = list("Mean" = 0.6, "Stdev" = 0.08),
  Time = list("Mean" = 0.4, "Stdev" = 0.08),
  MinQoS = list("Mean" = 0.90, "Stdev" = 0.04)
)

buyer_params <- list(
  "Price" = list("Mean" = 0.3, "Stdev" = 0.08),
  "Beds" = list("Mean" = 0.15, "Stdev" = 0.08),
  "Baths" = list("Mean" = 0.15, "Stdev" = 0.08),
  "SqrFt" = list("Mean" = 0.05, "Stdev" = 0.08),
  "LotSize" = list("Mean" = 0.10, "Stdev" = 0.08),
  "Time" = list("Mean" =  0.25, "Stdev" = 0.08),
  "MinQoS" = seller_params$MinQoS ## For simplicity
)

## Lists for storing the agents
BuyerList_perfectInfo <- list()
SellerList_perfectInfo <- list()
RealtorList_perfectInfo <- list()

## Initialize a Realtor
Realtor1 <- Realtor(
  Name = "Realtor1",
  Mkt_Fuzziness_Buyers = list(
    "Price" = Requirement(0.9, 1, 1, 1.1),
    "Beds" = Requirement(0.9, 1, 1, 1.1),
    "Baths" = Requirement(0.9, 1, 1, 1.1),
    "SqrFt" = Requirement(0.9, 1, 1, 1.1),
    "LotSize" = Requirement(0.9, 1, 1, 1.1),
    "Time" = Requirement(0.9, 1, 1, 1.1)
  ),
  Mkt_Fuzziness_Sellers = list(
    "Price" = Requirement(0.9, 1, 1, 1.1),
    "Time" = Requirement(0.9, 1, 1, 1.1)
  ),
  Mkt_Av_Preferences_Buyers = list(
    "Price" = c(0.3),
    "Beds" = c(0.15),
    "Baths" = c(0.15),
    "SqrFt" = c(0.05),
    "LotSize" = c(0.1),
    "Time" = c(0.25)
  ),
  Mkt_Av_Preferences_Sellers = list("Price" = c(0.6),
                                    "Time" = c(0.4)),
  CommissionPct = 0.05,
  TimeCurrent = 1
)
## Always name the index, the name of the agent
RealtorList_perfectInfo[[Realtor1@Name]] = Realtor1

BuyerInactive_perfectInfo <- list()
SellerInactive_perfectInfo <- list()


### Collective data of this run ###
## Each row is an iteration
iteration_stats_perfectInfo <-
  as.data.frame(matrix(nrow = 0, ncol = 6 + length(RealtorList_perfectInfo) * 6))

## Each row is a house sale
house_sales_perfectInfo <- as.data.frame(matrix(nrow = 0, ncol = 10))


## Run!
for (t in 1:iter) {
  ## 0 create iteration row and stats
  t <- as.numeric(t)
  iter_row <- list("Iteration" = t)
  saleCount <- 0
  offerCount <- 0
  avBuyerSatisfaction <- 0
  avSellerSatisfaction <- 0
  
  ## 1 add buyers and sellers to the system by name
  ## since names are generated using ComputerTimeStamps as strings for simplicity,
  ## and are thus unique, there is no need to worry about overwritting
  
  genBuyers <-
    generateBuyers(
      t,
      names(RealtorList_perfectInfo),
      dataToDraw,
      Clarity,
      Responsiveness,
      buyer_params,
      fuzzy_param
    )
  dataToDraw <- genBuyers[[2]]
  genSellers <-
    generateSellers(t,
                    names(RealtorList_perfectInfo),
                    dataToDraw,
                    Clarity,
                    seller_params,
                    fuzzy_param)
  dataToDraw <- genSellers[[2]]
  
  newBuyers <- genBuyers[[1]]
  newSellers <- genSellers[[1]]
  
  BuyerList_perfectInfo <- append(BuyerList_perfectInfo, newBuyers)
  SellerList_perfectInfo <- append(SellerList_perfectInfo, newSellers)
  
  iter_row$nBuyers <- length(BuyerList_perfectInfo)
  iter_row$nSellers <- length(SellerList_perfectInfo)
  
  ## 2 Loop through realtors
  ## 2.1 Inform realtor of their new clients
  ## 2.2 Matchmake within Realtor
  ## 2.3 inform Buyers
  ## 2.3.1 buyers make offers & generate feedback
  ## 2.3.2 Realtors get feedback and offers from Buyers
  RealtorsRunning <- list()
  for (r in RealtorList_perfectInfo) {
    r@TimeCurrent = t
    ## 2.1 Inform realtors of their new clients
    for (b in newBuyers)
      if (b@Realtor == r@Name) {
        inform <- informRealtor(b)
        r <- informRealtorFromBuyer(r, b, inform)
      }
    for (s in newSellers)
      if (s@Realtor == r@Name) {
        inform <- informRealtor(s)
        r <- informRealtorFromSeller(r, s, inform)
      }
    
    # 2.2 Matchmake
    r <- matchMake(r)
    if(nrow(r@BuyerHouseMatch) == 0){
      ## if there are no matches, just loop again, everyone will have their time updated when necessary
      RealtorsRunning[[r@Name]] <- r
      break
    }
    
    # 2.3 inform Buyers
    # 2.3.1 buyers make offers & generate feedback
    # 2.3.2 Realtors get feedback and offers from Buyers
    BuyersRunning <- list()
    for (b in BuyerList_perfectInfo) {
      ## For multiple Realtor Scenario
      if (b@Realtor != r@Name) {
        BuyersRunning[[b@Name]] <- b
        next
      }
      b@TimeCurrent = t
      
      ##
      ### CHANGE
      ### Arranging the houses by descending Buyer_AV instead of Estimated Bid
      ### I hypothesised that Estimated bid would influence the market (which it does seem to do)
      ### But it is having a large effect on sales that occur
      ##

      houses <- filter(r@BuyerHouseMatch, Buyer == b@Name) %>%
        arrange(desc(TimeStamp),desc(Buyer_AV)) %>%
        select(Address, Price, Beds, Baths, SqrFt, LotSize, TimeStamp)
      colnames(houses) <-
        c("Address",
          "Price",
          "Beds",
          "Baths",
          "SqrFt",
          "LotSize",
          "Time")
      ## Only inform on the top 3
      ## Gain information every turn, even if no action is taken
      if(nrow(houses) > 3){ 
        houses <- houses[1:3, ]
      }
      ## Else, we just offer as many houses as found
      b <- informBuyer(b, houses)
      ## If the buyer is not acting this turn.
      if(b@PlayLag > t && LagPlay){
        BuyersRunning[[b@Name]] <- b
        next
      }
      ## else
      ## Let the wait time be commonly within a week, often twice a week
      b@PlayLag = t + ceiling(rexp(1,1/4))
      b <- makeOffer(b)
      if (nrow(b@CurrentOffer) == 1) {
        offerCount  <- offerCount + 1
      }
      #b <- giveFeedback(b)
      r <- getFeedback(r, b, RANDOM)
      BuyersRunning[[b@Name]] <-  b
    }
    BuyerList_perfectInfo <- BuyersRunning
    
    ## 4 Inform sellers of her new offers and let her accept them
    SellersRunning <- list()
    for (s in SellerList_perfectInfo) {
      s@TimeCurrent = t
      if (r@Name == s@Realtor) {
        if(nrow(r@NewOffers) == 0 ){
          ## If there are no offers this round, we 'should' update every Seller's current time
          SellersRunning[[s@Name]] <- s
          next
        }
        s <- informSeller(s, filter(r@NewOffers, as.character(s@House$Address) == Address))
        if(nrow(s@CurrentOffers) == 0){
          SellersRunning[[s@Name]] <- s
          next
        }
        s <- acceptOffer(s)
        if (nrow(s@AcceptedOffer) > 0 && !is.na(s@AcceptedOffer)) {
          ## If there are multiple offers for the same price on the same house,
          ## we must increase the price and reinform the realtor
          if (s@AcceptedOffer[1] == "Price Increase") {
            ## Need to add these houses to another dataset and the realtor must be notified to update the buyers
            ## Could re-use the code for removing houes from buyers
            ## and simply re-match on the next iteration
            #print(paste("Price Increase on", s@House$Address))
            inform <- informRealtor(s)
            r <- informRealtorFromSeller(r, s, inform)
            SellersRunning[[s@Name]] <- s
            next
          }
          
          ## Since buyers can only commit to one offer,
          ## There can be no overlap in these statements
          
          ## 4.1 Extract data before the agents are removed
          ## There may be more data to remove
          
          ##print(s@AcceptedOffer)
          
          saleCount <- saleCount + 1
          avBuyerSatisfaction <-
            avBuyerSatisfaction + BuyerList_perfectInfo[[as.character(s@AcceptedOffer$Buyer)]]@Satisfaction
          avSellerSatisfaction <-
            avSellerSatisfaction + s@Satisfaction
          
          ## Add the home to houe_sales
          house_entry <-
            as.data.frame(c(
              t,
              as.numeric(s@AcceptedOffer$Bid),
              s@House,
              s@Name,
              as.character(s@AcceptedOffer$Buyer),
              s@Realtor,
              BuyerList_perfectInfo[[as.character(s@AcceptedOffer$Buyer)]]@Satisfaction,
              s@Satisfaction,
              nrow(s@PriceIncreases),
              as.numeric(t - s@Time@core1)
            ))
          names(house_entry) <-
            c(
              "Iteration",
              "Bid",
              "Address",
              "Price",
              "Beds",
              "Baths",
              "SqrFt",
              "LotSize",
              "Time",
              "Seller",
              "Buyer",
              "Realtor",
              "BuyerSatisfaction",
              "SellerSatisfaction",
              "PriceIncreases",
              "TimeOnMarket"
            )
          house_sales_perfectInfo <- rbind(house_sales_perfectInfo, house_entry)
          
          ## Before executing the sale, update Market averages
          #r <- updateMarketAverages(r)
          ## No longer learning, so no need to update averages
          
          r <- executeSale(r, s@AcceptedOffer, RANDOM)
          
          ## 4.2 Remove agents of completed transaction
          SellerInactive_perfectInfo[[s@Name]] <- s
          BuyerInactive_perfectInfo[[as.character(s@AcceptedOffer$Buyer)]] <-
            BuyerList_perfectInfo[[as.character(s@AcceptedOffer$Buyer)]]
          ##print("")
          ##print(paste("Accepted Offer Buyer",as.character(s@AcceptedOffer$Buyer)))
          
          ## Removing the Buyer is a bit trickier
          ## First re-assign the last Buyer in the list to the named index we are removing
          ## Get the last buyer
          replace = BuyerList_perfectInfo[[length(BuyerList_perfectInfo)]]
          ## Set the value of the last item to NULL
          BuyerList_perfectInfo[length(BuyerList_perfectInfo)] <- NULL
          
          #print(paste("Removing",as.character(s@AcceptedOffer$Buyer),"by replacing with",replace@Name))
          
          if (replace@Name == as.character(s@AcceptedOffer$Buyer)) {
            ## We are done!
          }
          else{
            ## Re-set the name of re-indexed buyer
            names(BuyerList_perfectInfo)[which(names(BuyerList_perfectInfo) == as.character(s@AcceptedOffer$Buyer))] <-
              replace@Name
            BuyerList_perfectInfo[replace@Name] <- replace
          }
          
          
          ## Go through each Buyer's dataset and drop houses that are no longer on the market
          buyerNotify <- r@BuyerHouseMatch %>%
            filter(Address == as.character(s@House$Address))
          buyerNotify <- as.list(buyerNotify$Buyer)
          for (b in buyerNotify) {
            b <- as.character(b)
            BuyerList_perfectInfo[[b]] <-
              removeHouse(BuyerList_perfectInfo[[b]], as.character(s@House$Address))
          }
          ## Call the next Seller, so we do not append this seller to SellersRunning
          ## and it is removed from Seller List
          next
        }
      }
      SellersRunning[[s@Name]] <- s
    }
    
    SellerList_perfectInfo <- SellersRunning
    ##print(BuyerList_perfectInfo)
    
    RealtorsRunning[[r@Name]] <- r
    
  }
  RealtorList_perfectInfo <- RealtorsRunning

  
  ## Add Realtor Learning to the iteration data
  #iter_row[[paste0(r@Name, "_SaleCount")]] == r@SaleCount
  iter_row[[paste0(r@Name, "_TotalCommission")]] = r@TotalCommission
  
  
  ## 6 Other iteration stats
  iter_row$Sales <- saleCount
  iter_row$Offers <- offerCount
  # Returns 'Inf' if saleCount=0
  iter_row$AverageBuyerSatisfaction <-
    avBuyerSatisfaction / saleCount
  iter_row$AverageSellerSatisfaction <-
    avSellerSatisfaction / saleCount
  
  ## Need probabilities to add Buyers and Sellers to the system
  ## Need functions for initializing them on random
  
  if ((t * 100 / iter) %% (10) == 0) {
    print(paste0("Complete: ", (t * 100 / iter), "% ", Sys.time()))
  }
  
  iteration_stats_perfectInfo <- rbind(iteration_stats_perfectInfo, as.data.frame(iter_row))
  
}

end <- Sys.time()

print(paste("Run-time:",end-start))

view(iteration_stats_perfectInfo)

ggplot(iteration_stats_perfectInfo) +
  geom_col(aes(x = Iteration, y = Sales)) + 
  ggtitle("Perfect Information")