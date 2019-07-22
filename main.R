## Main System executable
## RUN THIS ONE WITH PERFECT INFORMATION

RANDOM = FALSE


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

set.seed(1234)

suppressWarnings("In if (!is.na(offer)) { : the condition has length > 1 and only the first element will be used")
supressWarnings("In max(seller@CurrentOffers$AV) : no non-missing arguments to max; returning -Inf")
supressWarnings("In max(seller@CurrentOffers$Bid) : no non-missing arguments to max; returning -Inf")

## Still some data cleaning around prices and organizing the list-sale pairs
## Will do that later 
## TODO
dataToDraw <- read.csv(paste0(getwd(), "/Data/dataToDraw.csv"))

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
  "S1min" = 0.90,
  "S1max" = 1.00,
  "S2min" = 1.00,
  "S2max" = 1.10
)

## Parameters for Normal distributions of Preferences
seller_params <- list(
  Price = list("Mean" = 0.6, "Stdev" = 0.08),
  Time = list("Mean" = 0.4, "Stdev" = 0.08),
  MinQoS = list("Mean" = 0.92, "Stdev" = 0.08)
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
BuyerList <- list()
SellerList <- list()
RealtorList <- list()

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
RealtorList[[Realtor1@Name]] = Realtor1

BuyerInactive <- list()
SellerInactive <- list()


## Number of iterations to run
iter <- 10

### Collective data of this run ###
## Each row is an iteration
iteration_stats <-
  as.data.frame(matrix(nrow = 0, ncol = 6 + length(RealtorList) * 6))

## Each row is a house sale
house_sales <- as.data.frame(matrix(nrow = 0, ncol = 10))


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
      names(RealtorList),
      dataToDraw,
      Clarity,
      Responsiveness,
      buyer_params,
      fuzzy_param
    )
  dataToDraw <- genBuyers[[2]]
  genSellers <-
    generateSellers(t,
                    names(RealtorList),
                    dataToDraw,
                    Clarity,
                    seller_params,
                    fuzzy_param)
  dataToDraw <- genSellers[[2]]
  
  newBuyers <- genBuyers[[1]]
  newSellers <- genSellers[[1]]
  
  BuyerList <- append(BuyerList, newBuyers)
  SellerList <- append(SellerList, newSellers)
  
  iter_row$nBuyers <- length(BuyerList)
  iter_row$nSellers <- length(SellerList)
  
  ## 2 Loop through realtors
  ## 2.1 Inform realtor of their new clients
  ## 2.2 Matchmake within Realtor
  ## 2.3 inform Buyers
  ## 2.3.1 buyers make offers & generate feedback
  ## 2.3.2 Realtors get feedback and offers from Buyers
  RealtorsRunning <- list()
  for (r in RealtorList) {
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
    
    # 2.3 inform Buyers
    # 2.3.1 buyers make offers & generate feedback
    # 2.3.2 Realtors get feedback and offers from Buyers
    BuyersRunning <- list()
    for (b in BuyerList) {
      if (b@Realtor != r@Name) {
        next
      }
      b@TimeCurrent = t
      houses <- filter(r@BuyerHouseMatch, Buyer == b@Name) %>%
        arrange(desc(Estimated_Bid)) %>%
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
      houses <- houses[1:3, ]
      b <- informBuyer(b, houses)
      b <- makeOffer(b)
      if (length(b@CurrentOffer) > 0 || !is.na(b@CurrentOffer)) {
        offerCount  <- offerCount + 1
      }
      b <- giveFeedback(b)
      r <- getFeedback(r, b, RANDOM)
      BuyersRunning[[b@Name]] <-  b
    }
    BuyerList <- BuyersRunning
    
    ## 4 Inform sellers of her new offers and let her accept them
    SellersRunning <- list()
    for (s in SellerList) {
      s@TimeCurrent = t
      if (r@Name == s@Realtor) {
        s <-
          informSeller(s, filter(r@NewOffers, as.character(s@House$Address) == Address))
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
            avBuyerSatisfaction + BuyerList[[as.character(s@AcceptedOffer$Buyer)]]@Satisfaction
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
              s@Realtor
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
              "Realtor"
            )
          house_sales <- rbind(house_sales, house_entry)
          
          ## Before executing the sale, update Market averages
          r <- updateMarketAverages(r)
          r <- executeSale(r, s@AcceptedOffer, RANDOM)
          
          ## 4.2 Remove agents of completed transaction
          SellerInactive[[s@Name]] <- s
          BuyerInactive[[as.character(s@AcceptedOffer$Buyer)]] <-
            BuyerList[[as.character(s@AcceptedOffer$Buyer)]]
          ##print("")
          ##print(paste("Accepted Offer Buyer",as.character(s@AcceptedOffer$Buyer)))
          
          ## Removing the Buyer is a bit trickier
          ## First re-assign the last Buyer in the list to the named index we are removing
          ## Get the last buyer
          replace = BuyerList[[length(BuyerList)]]
          ## Set the value of the last item to NULL
          BuyerList[length(BuyerList)] <- NULL
          
          #print(paste("Removing",as.character(s@AcceptedOffer$Buyer),"by replacing with",replace@Name))
          
          if (replace@Name == as.character(s@AcceptedOffer$Buyer)) {
            ## We are done!
          }
          else{
            ## Re-set the name of re-indexed buyer
            names(BuyerList)[which(names(BuyerList) == as.character(s@AcceptedOffer$Buyer))] <-
              replace@Name
            BuyerList[replace@Name] <- replace
          }
          
          
          ## Go through each Buyer's dataset and drop houses that are no longer on the market
          buyerNotify <- r@BuyerHouseMatch %>%
            filter(Address == as.character(s@House$Address))
          buyerNotify <- as.list(buyerNotify$Buyer)
          for (b in buyerNotify) {
            b <- as.character(b)
            BuyerList[[b]] <-
              removeHouse(BuyerList[[b]], as.character(s@House$Address))
          }
          ## Call the next Seller, so we do not append this seller to SellersRunning
          ## and it is removed from Seller List
          next
        }
      }
      SellersRunning[[s@Name]] <- s
    }
    
    SellerList <- SellersRunning
    ##print(BuyerList)
    
    ## Add Realtor Learning to the iteration data
    iter_row[[paste0(r@Name, "_Mkt_Fuzziness_Buyers")]] = I(list(c(r@Mkt_Fuzziness_Buyers)))
    iter_row[[paste0(r@Name, "_Mkt_Fuzziness_Sellers")]] = I(list(c(r@Mkt_Fuzziness_Sellers)))
    iter_row[[paste0(r@Name, "_Mkt_Av_Preferences_Buyers")]] = I(list(c(r@Mkt_Av_Preferences_Buyers)))
    iter_row[[paste0(r@Name, "_Mkt_Av_Preferences_Sellers")]] = I(list(c(r@Mkt_Av_Preferences_Sellers)))
    iter_row[[paste0(r@Name, "_SaleCount")]] == r@SaleCount
    iter_row[[paste0(r@Name, "_TotalCommission")]] = r@TotalCommission
    
    RealtorsRunning[[r@Name]] <- r
    
  }
  RealtorList <- RealtorsRunning
  
  
  ## 6 Other iteration stats
  iter_row$Sales <- saleCount
  iter_row$Offers <- offerCount
  # Returns 'Inf' if saleCount=0
  iter_row$AverageBuyerSatisfaction <-
    avBuyerSatisfaction / saleCount
  iter_row$AverageSellerSatisfaction <-
    avSellerSatisfaction / saleCount
  
  
  ## Loop
  
  ## Need probabilities to add Buyers and Sellers to the system
  ## Need functions for initializing them on random
  
  if ((t * 100 / iter) %% (10) == 0) {
    print(paste0("Complete: ", (t * 100 / iter), "%"))
  }
  
  iteration_stats <- rbind(iteration_stats, as.data.frame(iter_row))
  
}
