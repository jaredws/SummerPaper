## Buyer Class

library(FuzzyNumbers)
library(dplyr)
library(tidyverse)


Buyer <- setClass(
  Class = "Buyer",
  slots = c(
    Name  = "character",      ## A unique identifier
    Price = "Requirement", ## Each of the requirements are fuzzy numbers, 
    Beds  = "Requirement", ## in this implimentation, we can use either trapezoidal or triangular FNs
    Baths = "Requirement", ## Triangular and Trapezoidal only differ by the a2 and a3 values being equal or not resp.
    SqrFt = "Requirement",
    LotSize = "Requirement",
    Time  = "Requirement",
    MinQoS = "numeric",   ## The minimum threshold of satisfaction this buyer will consider for bidding on a house
    Requirements = "list",
    Preferences = "list", ## A list of weights that sum to 1 on how important each attribute is
    TimeRemain = "numeric",
    TimeCurrent = "numeric",
    Realtor = "character", ## name of realtor
    HousesKnown = "data.frame", ## Houses the buyer knows about
    Satisfaction = "numeric", # the final satisfaction after purchasing
    Clarity = "numeric",    ## Parameter for setting communication variants in modeling initial Realtor knowledge
    RealtorKnows = "data.frame",  ## list of requirement desires and/or ranges depending on Clarity
    Feedback = "data.frame",   ## Feedback on the most lacking component of the best house(s), for realtor learning ,
    Responsiveness = "numeric", ## Parameter for setting feedback quantitiy 
    CurrentOffer = "data.frame", ## Place for storing the Buyer's current offer
    PlayLag = "numeric" ## The time between plays - a random draw from an exponential distribution
  )
  
)

## Basic Initialize Function
## Randomization of initialization of system will be handled in another file
setMethod("initialize", "Buyer",
          function(.Object,
                   Name,
                   Price,
                   Beds,
                   Baths,
                   SqrFt,
                   LotSize,
                   Time,
                   MinQoS,
                   Preferences,
                   Realtor,
                   Clarity,
                   TimeCurrent,
                   Responsiveness,
                   PlayLag) {
            .Object@Name = Name
            .Object@Price = Price
            .Object@Beds = Beds
            .Object@Baths = Baths
            .Object@SqrFt = SqrFt
            .Object@LotSize = LotSize
            .Object@Time = Time
            .Object@MinQoS = MinQoS
            .Object@Preferences = Preferences
            .Object@Realtor = Realtor
            .Object@Clarity = Clarity
            .Object@Responsiveness = Responsiveness
            .Object@TimeCurrent = TimeCurrent
            .Object@Requirements = c(Price, Beds, Baths, SqrFt, LotSize, Time)
            .Object@TimeRemain = Time@support2 - TimeCurrent
            HousesKnown = data.frame()
            Satisfaction = 0
            .Object@PlayLag = PlayLag
            RealtorKnows = data.frame()
            Feedback = data.frame(matrix(ncol=2))
            colnames(Feedback) <- c("Address","Lacking")
            .Object
          })

setMethod("show","Buyer",
          function(object){
            return(print(object@Name))
          })


setGeneric("makeOffer", function(buyer) {
  standardGeneric("makeOffer")
})

## Function for choosing a known house to bid on and how much
setMethod("makeOffer", 
          signature("Buyer"),
          function(buyer) {
            ## Sort Descending by BV
            ## BV is the min of list price and AV*PriceFuzziness, so the buyer will offer on the 'best value' house
            
            if(nrow(buyer@HousesKnown) == 0){
              buyer@CurrentOffer <- as.data.frame(matrix(nrow = 0, ncol = 4))
              return (buyer)
            }
            
            
            buyer@HousesKnown <- dplyr::arrange(buyer@HousesKnown,desc(BV))
            ## Choose top house
            house <- buyer@HousesKnown[1,]
            buyer@Satisfaction <- house$AV
            ## If top house is not above the threshold, return the buyer now with no changes
            if(house$AV < buyer@MinQoS){
              buyer@CurrentOffer <- as.data.frame(matrix(nrow = 0, ncol = 4))
              return (buyer)
            }
            ## Else, compute offer as the minimum between Buyer Value and 
            ## House list Price
            bid = min(house$Price,house$BV)
            
            
            offer = as.data.frame(list("Time"=buyer@TimeCurrent,"Buyer"=buyer@Name,"Address" = as.character(house$Address),"Bid"=bid))
            buyer@CurrentOffer <- offer
            return(buyer)
          })


setGeneric("informBuyer", function(buyer,houses) {
  standardGeneric("informBuyer")
})
## Function for the Realtor to inform the Buyer of houses on the market
setMethod("informBuyer",
          signature("Buyer","list"),
          function(buyer,houses){
            ## First calculate the AV for the new house(s)
            ## Houses are ordered as 
              # Address
              # Price
              # Beds
              # Baths
              # SqrFt
              # LotSize
              # Time
            if(nrow(houses) == 0){
              buyer@CurrentOffer <- as.data.frame(matrix(nrow = 0, ncol = 4))
              return (buyer)
            }
            for (h in 1:nrow(houses)){
              house <- houses[h,]
              ## For Beds, Baths, SqrFt, and LotSize, we know the exact value
              ## So the Fuzzy number comparison is simple
              ## We do skip price and Time in this first iteration,
              ## we want all attributes to exceed our expecations except Price and Time
              
              ## Create FN representations of each attribute of the house
              ## Using the Buyer's initialized Fuzziness? Should there be antoher 'fuzzy' perspective?
              ## TODO
              price_FN <- PiecewiseLinearFuzzyNumber(house$Price,house$Price,house$Price,house$Price)
              beds_FN <- PiecewiseLinearFuzzyNumber(house$Beds,house$Beds,house$Beds,house$Beds)
              baths_FN <- PiecewiseLinearFuzzyNumber(house$Baths,house$Baths,house$Baths,house$Baths)
              sqrFt_FN <- PiecewiseLinearFuzzyNumber(house$SqrFt,house$SqrFt,house$SqrFt,house$SqrFt)
              lotSize_FN <- PiecewiseLinearFuzzyNumber(house$LotSize,house$LotSize,house$LotSize,house$LotSize)
              time_FN <- PiecewiseLinearFuzzyNumber(house$Time,house$Time,house$Time,house$Time)
              
              ## For Price and Time, we want undervaluation
              house$Price_AV = buyer@Preferences$Price * possibilityUndervaluation(price_FN,as.plFN(buyer@Price))
              
              ## For Beds, Baths, SqrFt, and LotSize, we want exceedence
              house$Beds_AV = buyer@Preferences$Beds * possibilityExceedance(beds_FN,as.plFN(buyer@Beds))
              house$Baths_AV = buyer@Preferences$Baths * possibilityExceedance(baths_FN,as.plFN(buyer@Baths))
              house$SqrFt_AV = buyer@Preferences$SqrFt * possibilityExceedance(sqrFt_FN,as.plFN(buyer@SqrFt))
              house$LotSize_AV = buyer@Preferences$LotSize * possibilityExceedance(lotSize_FN,as.plFN(buyer@LotSize))

              ## Time
              house$Time_AV = buyer@Preferences$Time * possibilityUndervaluation(time_FN,as.plFN(buyer@Time))
              
              ## Set the house's Appropriateness Value (cite) (AV)
              house$AV = sum(house$Price_AV,house$Beds_AV,house$Baths_AV,house$SqrFt_AV,house$LotSize_AV,house$Time_AV)
              
              ## Now compute Buyer Value
              ## for now, compute Buyer value as the product of AV and the full 
              ## Range of the Buyer's Price
              ## We have already computed how much we like the price.. 
              ## May change in the future...
              house$BV = min(buyer@Price@core2 * house$AV, buyer@Price@support2)
              buyer@HousesKnown <- rbind(buyer@HousesKnown,house)
            }
            ## Append the new houses to Houses Known
            ## Will need to remove duplicate addresses..
            ## distinct keeps the first instance so descending time will
            ## keep the most recent update
            buyer@HousesKnown <- buyer@HousesKnown %>% arrange(desc(Time)) %>% distinct(Address,.keep_all=TRUE) %>% arrange(desc(BV))
            return (buyer)
          })

setGeneric("informRealtor", 
           function(agent) {
             standardGeneric("informRealtor")
})

## Function for informing Realtor on Requirements
## Clarity parameter determines how much knowledge is revealed
setMethod("informRealtor",
          signature("Buyer"),
          function(agent){
            inform = list()
            if(agent@Clarity == 1){
              ## Desired Number = midpoint of core
              ## For Triangular, a2 = a3 or we want the mean if it's Trapezoidal
              inform$Price = list(mean(c(agent@Price@core1,agent@Price@core2)))
              inform$Beds = list(mean(c(agent@Beds@core1,agent@Beds@core2)))
              inform$Baths = list(mean(c(agent@Baths@core1,agent@Baths@core2)))
              inform$SqrFt = list(mean(c(agent@SqrFt@core1,agent@SqrFt@core2)))
              inform$LotSize = list(mean(c(agent@LotSize@core1,agent@LotSize@core2)))
              inform$Time = list(mean(c(agent@Time@core1,agent@Time@core2)))
              
            }
            if(agent@Clarity == 2){
              ## Core Price range, Desired number on the rest
              inform$Price = list(agent@Price@core1,agent@Price@core2)
              inform$Beds = list(mean(c(agent@Beds@core1,agent@Beds@core2)))
              inform$Baths = list(mean(c(agent@Baths@core1,agent@Baths@core2)))
              inform$SqrFt = list(mean(c(agent@SqrFt@core1,agent@SqrFt@core2)))
              inform$LotSize = list(mean(c(agent@LotSize@core1,agent@LotSize@core2)))
              inform$Time = list(mean(c(agent@Time@core1,agent@Time@core2)))
            }
            if(agent@Clarity == 3){
              ## Core Price range and Core range on rest
              inform$Price = list(agent@Price@core1,agent@Price@core2)
              inform$Beds = list(agent@Beds@core1,agent@Beds@core2)
              inform$Baths = list(agent@Baths@core1,agent@Baths@core2)
              inform$SqrFt = list(agent@SqrFt@core1,agent@SqrFt@core2)
              inform$LotSize = list(agent@LotSize@core1,agent@LotSize@core2)
              inform$Time = list(agent@Time@core1,agent@Time@core2)
              inform$Preferences = agent@Preferences
              inform$MinQoS = agent@MinQoS
            }
            return (inform)
          })

## Function for informing Realtor on poor matches
## Responsiveness parameter determines how much knowledge is revealed

setGeneric("giveFeedback", 
           function(buyer) {
             standardGeneric("giveFeedback")
           })
setMethod("giveFeedback", 
          signature("Buyer"),
          function(buyer) {
            buyer@HousesKnown = dplyr::arrange(buyer@HousesKnown,desc(AV))
            ## Reset the Feedback
            Feedback = data.frame(matrix(ncol=2))
            colnames(Feedback) <- c("Address","Lacking")
            
            if (buyer@Responsiveness == 1) {
              ## Only the most offesnive requirement of the closest'best' house
              rows = 1
              
            }
            if (buyer@Responsiveness == 2) {
              ## Only the most offensive requirement of the closest 'best' 3 houses, may repeat
              rows = 3
            }
            if (buyer@Responsiveness == 3) {
              ## The most offensive requirement of every house presented
              rows = nrow(buyer@HousesKnown)
            }
            AVs <- buyer@HousesKnown[1:rows,c("Price_AV","Beds_AV","Baths_AV","SqrFt_AV","LotSize_AV","Time_AV")]
            for (r in 1:rows) {
              AVr <- AVs[r,]
              prefs <- as.data.frame(buyer@Preferences)
              diff <- prefs - AVr
              lacking <-
                colnames(diff)[max.col(diff, ties.method = "first")]
              entry <-
                as.data.frame(matrix(c(as.character(buyer@HousesKnown[r,"Address"]), lacking),ncol=2))
              colnames(entry) <- c("Address","Lacking")
              buyer@Feedback <- rbind(buyer@Feedback, entry)
            }
            return (buyer)
          })

## Functionality to remove the house from the Buyer's Houses Known 
setGeneric("removeHouse", 
           function(buyer,address) {
             standardGeneric("removeHouse")
           })
setMethod("removeHouse", 
          signature("Buyer","character"),
          function(buyer,address) {
            buyer@HousesKnown <- filter(buyer@HousesKnown, Address != address)
            
            return (buyer)
          })

          