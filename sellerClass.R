## Seller class

library(FuzzyNumbers)
library(dplyr)


Seller <- setClass(
  Class = "Seller",
  slots = c(
    Name  = "character",
    ## A unique Identifier
    Price = "Requirement",
    Time  = "Requirement",
    House = "data.frame",
    ## A dataframe of the house for sale, including list price
    Preferences = "list",
    ## The Weights the seller puts on Price and Time,
    Clarity = "numeric",
    ## The Level of clarity with the Realtor on Price and Time Fuzziness
    MinQoS = "numeric",
    ## The Threshold an offer must make to be accepted
    Satisfaction = "numeric",
    ## The final satisfaction received on Sale
    Realtor = "character",
    ## The name of the Realtor the Seller is working with
    PastOffers = "data.frame",
    CurrentOffers = "data.frame",
    AcceptedOffer = "data.frame",
    TimeCurrent = "numeric",
    TimeRemaining = "numeric",
    PlayLag = "numeric", ## The time between executed actions - a random draw from an exponential distribution
    PriceIncreases = "data.frame"
    )
  
)



## Function to inform sellers of their new offers

setGeneric("informSeller",
           function(seller, offers) {
             standardGeneric("informSeller")
           })
setMethod("informSeller",
          signature("Seller", "data.frame"),
          function(seller, offers) {
            ## Given a list of offers, add them to current offers, but first, push all current offers to Past offers
            if (nrow(seller@CurrentOffers) > 0) {
              seller@PastOffers <- rbind(seller@PastOffers, seller@CurrentOffers) %>%
                arrange(desc(Time)) %>%
                distinct(Buyer,Time, .keep_all = TRUE)
            }
            ## Refresh Current Offers
            seller@CurrentOffers <-
              as.data.frame(matrix(ncol = 7, nrow = 0))
            colnames(seller@CurrentOffers) <-
              c("Time",
                "Buyer",
                "Address",
                "Bid",
                "Price_AV",
                "Time_AV",
                "AV")
            
            if (nrow(offers) > 0) {
              for (r in 1:nrow(offers)) {
                offer <- offers[r,]
                ## Generate Fuzzy Number Representation of Bid and Time
                price_FN <-
                  PiecewiseLinearFuzzyNumber(offer$Bid, offer$Bid, offer$Bid, offer$Bid)
                time_FN <-
                  PiecewiseLinearFuzzyNumber(offer$Time,
                                             offer$Time,
                                             offer$Time,
                                             offer$Time)
                
                ## Price exceeding our desired price
                offer$Price_AV = seller@Preferences$Price * possibilityExceedance(price_FN, as.plFN(seller@Price))
                
                ## Time within our time constraint
                offer$Time_AV = seller@Preferences$Time * possibilityUndervaluation(time_FN, as.plFN(seller@Time))
                
                offer$AV = sum(offer$Price_AV, offer$Time_AV)
  
                
                seller@CurrentOffers <-
                  rbind(seller@CurrentOffers, offer)
              }
              seller@CurrentOffers <-
                seller@CurrentOffers %>% filter(AV >= seller@MinQoS) %>% arrange(desc(Time)) %>% distinct(Buyer, .keep_all = TRUE)
            }
            return(seller)
          })

## Function for accepting an offer
setGeneric("acceptOffer",
           function(seller) {
             standardGeneric("acceptOffer")
           })
setMethod("acceptOffer",
          signature("Seller"),
          function(seller) {
            ## Go Through the dataframe of NewOffers
            ## and sort by their
            accept <- seller@CurrentOffers %>% arrange(desc(AV))
            bestAV <- max(seller@CurrentOffers$AV)
            bestAVOffers <- seller@CurrentOffers %>% filter(AV == bestAV) %>% arrange(desc(Bid))
            bestPrice <- max(seller@CurrentOffers$Bid)
            bestOffers <- bestAVOffers %>% filter(Bid == bestPrice) %>% arrange(desc(AV),desc(Bid))
            ## If multiple offers have the same AV, then we need to raise the price and reinform the realtor!
            ## Who then needs to reinform the buyers of the increased price
            if (nrow(bestOffers) == 1) {
              accept <-
                accept[1, c("Time", "Buyer", "Address", "Bid", "AV")]
              seller@Satisfaction <- as.numeric(accept$AV)
              seller@AcceptedOffer <- accept[1, c("Time", "Buyer", "Address", "Bid")]
            }
            ## If there are no Best Offers, we accept none.
            else if(nrow(bestOffers) == 0){
              seller@AcceptedOffer <- as.data.frame(NA)
            }
            ## Else, there must be multiple rows of best offers.
            else {
              ## Now check for Similiar AV then Price
              ## Since all of the AV's and the Prices are the 'best' we know we need to increase the price
              seller@AcceptedOffer <- bestOffers
              ## Arbitrairily raise the price by 5%
              seller@House$Price <- seller@House$Price * ( 1 + 0.05)
              seller@Price <- percentIncrease(seller@Price,0.05)
              ## I am assuming that a house will not drop in listed value in this scenario ..
              ## Future research may be able to add this feature

              entry <- list("Iteration" = seller@TimeCurrent, "NumberEqualOffers" = nrow(bestOffers))
              
              seller@PriceIncreases <- rbind(seller@PriceIncreases,as.data.frame(entry))
            }

            return(seller)
          })


## Function for informing Realtor on Requirements
## Clarity parameter determines how much knowledge is revealed
## Generic Function exists in Buyer Class, may re-organize
setMethod("informRealtor",
          signature("Seller"),
          function(agent) {
            inform = list()
            if (agent@Clarity == 1) {
              ## Inform on the complete Requirements, 
              ## Not the Preferences or MinQoS 
              inform$Price = list(agent@Price@core1, agent@Price@core2)
              inform$Time = list(agent@Time@core1, agent@Time@core2)     
            }
            if (agent@Clarity == 2) {
              ## Everything except the MinQoS threshold
              inform$Price = list(agent@Price@core1, agent@Price@core2)
              inform$Time = list(agent@Time@core1, agent@Time@core2)
              inform$Preferences = agent@Preferences
              }
            if (agent@Clarity == 3) {
              ## Literally Everything
              inform$Price = list(agent@Price@core1, agent@Price@core2)
              inform$Time = list(agent@Time@core1, agent@Time@core2)
              inform$Preferences = agent@Preferences
              inform$MinQoS = agent@MinQoS
            }
            
            return (inform)
          })

## Inform the realtor of accepted Sales
