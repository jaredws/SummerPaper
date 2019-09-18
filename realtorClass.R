## Realtor Class

library(FuzzyNumbers)
library(dplyr)
#library(tidyverse)


Realtor <- setClass(
  Class = "Realtor",
  slots = c(
    Name  = "character",
    ## A unique Identifier
    Buyers = "data.frame",
    ## A list of Buyers
    Sellers = "data.frame",
    ## A list of Sellers
    NewOffers = "data.frame",
    ## A dataframe of new offers this time step
    BuyerHouseMatch = "data.frame",
    ## A dataframe of house offers to learn from
    Houses = "data.frame",
    ## A dataframe of Houses (from Sellers),
    Mkt_Fuzziness_Buyers = "list",
    Mkt_Fuzziness_Sellers = "list",
    Mkt_Av_Preferences_Buyers = "list",
    Mkt_Av_Preferences_Sellers = "list",
    TimeCurrent = "numeric",
    SaleCount = "numeric",
    CommissionPct = "numeric",
    TotalCommission = "numeric"
    
    ## Need knowledge base on fuzzy number settings and so on.
    ## What are they learning?
  )
  
)

setMethod("initialize", "Realtor",
          function(.Object,
                   Name,
                   Mkt_Fuzziness_Buyers,
                   Mkt_Fuzziness_Sellers,
                   Mkt_Av_Preferences_Buyers,
                   Mkt_Av_Preferences_Sellers,
                   CommissionPct,
                   TimeCurrent) {
            .Object@Name = Name
            .Object@Mkt_Fuzziness_Buyers = Mkt_Fuzziness_Buyers
            .Object@Mkt_Fuzziness_Sellers = Mkt_Fuzziness_Sellers
            .Object@Mkt_Av_Preferences_Buyers = Mkt_Av_Preferences_Buyers
            .Object@Mkt_Av_Preferences_Sellers = Mkt_Av_Preferences_Sellers
            .Object@TimeCurrent = TimeCurrent
            
            .Object@NewOffers <-
              as.data.frame(matrix(ncol = 4, nrow = 0))
            
            .Object@BuyerHouseMatch <-
              as.data.frame(matrix(ncol = 17, nrow = 0))
            
            .Object@Buyers <-
              as.data.frame(matrix(ncol = 13, nrow = 0))
            
            .Object@Sellers <-
              as.data.frame(matrix(ncol = 10, nrow = 0))
            
            .Object@Houses <-
              as.data.frame(matrix(ncol = 9, nrow = 0))
            .Object@CommissionPct = CommissionPct
            .Object@TotalCommission = 0
            .Object@SaleCount = 0
            .Object
          })

## Function to get information from Buyers
setGeneric("informRealtorFromBuyer",
           function(realtor, buyer, inform) {
             standardGeneric("informRealtorFromBuyer")
           })

setMethod("informRealtorFromBuyer",
          signature("Realtor", "Buyer", "list"),
          function(realtor, buyer, inform) {
            ## Called from informRealtor in the Buyer Class
            
            ## If clarity = 3, then the buyer & seller also informs us of prices
            pref <- inform$Preferences
            if(is.null(pref)){
              pref <- realtor@Mkt_Av_Preferences_Buyers
            }
            minQoS <- inform$MinQoS
            if(is.null(minQoS)){
              minQoS <- 0.8
            }
     
            entry <- data.frame(
              Name = buyer@Name,
              Price = I(list(c(inform$Price))),
              Beds = I(list(c(inform$Beds))),
              Baths = I(list(c(inform$Baths))),
              SqrFt = I(list(c(inform$SqrFt))),
              LotSize = I(list(c(inform$LotSize))),
              Time = I(list(c(inform$Time))),
              Fuzziness = I(list(c(
                realtor@Mkt_Fuzziness_Buyers
              ))),
              Preferences = I(list(c(
                pref
              ))),
              Offers = I(list(c(
                list(
                  "Time" = "",
                  "Buyer" = "",
                  "Address" = "",
                  "Bid" = ""
                )
              ))),
              EntryTime = realtor@TimeCurrent,
              UpdateTime = realtor@TimeCurrent + 0.1,
              MinQoS = minQoS
            )
            realtor@Buyers <-
              rbind(realtor@Buyers, entry) %>% distinct(Name, .keep_all = TRUE)
            
            return (realtor)
          })


## Function to get information from Sellers
setGeneric("informRealtorFromSeller",
           function(realtor, seller, inform) {
             standardGeneric("informRealtorFromSeller")
           })
setMethod("informRealtorFromSeller",
          signature("Realtor", "Seller", "list"),
          function(realtor, seller, inform) {
            ## Called from informRealtor in the Seller Class

            pref <- inform$Preferences
            if(is.null(pref)){
              pref <- realtor@Mkt_Av_Preferences_Buyers
            }
            
            minQoS <- inform$MinQoS
            if(is.null(minQoS)){
              minQoS <- 0.8
            }
            
            entry <- data.frame(
              Name = seller@Name,
              Price = I(list(c(inform$Price))),
              Time = I(list(c(inform$Time))),
              Fuzziness = I(list(c(
                realtor@Mkt_Fuzziness_Sellers
              ))),
              Preferences = I(list(
                c(pref)
              )),
              House = seller@House$Address[[1]],
              Offers = I(list("")),
              EntryTime = as.numeric(realtor@TimeCurrent),
              UpdateTime = as.numeric(realtor@TimeCurrent + 0.1),
              MinQoS = minQoS
            )
            h <- seller@House
            h$UpdateTime <- realtor@TimeCurrent + 0.1
            h$EntryTime <- realtor@TimeCurrent
            
            realtor@Houses <-
              rbind(realtor@Houses, h) %>%
              dplyr::arrange(desc(UpdateTime)) %>%
              distinct(Address, .keep_all = TRUE)
            
            realtor@Sellers <-
              rbind(realtor@Sellers, entry) %>%
              dplyr::arrange(desc(UpdateTime)) %>%
              distinct(Name, .keep_all = TRUE)
            
            return (realtor)
          })

## Function to receive feedback from the buyer (seller?)
### and so update the knowledge of his preferences
## If learning,
### Update his weights
### Update his fuzzy bounds

setGeneric("getFeedback",
           function(realtor, client, random) {
             standardGeneric("getFeedback")
           })
setMethod("getFeedback",
          signature("Realtor", "Buyer", "logical"),
          function(realtor, client, random) {
            ## First, find the buyer in our list
            
            if(nrow(client@CurrentOffer) ==0){
              return(realtor)
            }
            
            buyerEntry <-
              filter(realtor@Buyers, Name == client@Name)
            
            #feedback <- client@Feedback
            offer <- client@CurrentOffer
            
            if (!is.na(offer)) {
              ## If there is an offer from this buyer,
              ## Then we can extract information from the buyer's offer and must inform the seller
              
              ## Append the new offer to existing offers
              buyerEntry$Offers[[1]] <-
                append(buyerEntry$Offers, as.list(offer), after = length(buyerEntry$Offers))
              #offer_house <- as.data.frame(offer) %>%
              #  full_join(realtor@Houses, by = "Address")
              
              ## Find the original match entry and add the offer
              ## If we are not discovering houses randomly
              if (!random) {
                bhmEntry <- realtor@BuyerHouseMatch %>%
                  filter(Address == as.character(offer$Address),
                         Buyer == as.character(offer$Buyer))
                
                
                bhmEntry <- arrange(bhmEntry, desc(TimeStamp))[1,]
                
                ## To calculate the Offer's AV, we need to take a few things into account
                ## No one will offer more than list price
                ## First run the experiment where we just assume that an offer at list price is equivalent to AV = 1
                ## So I only need to ratio of Bid to ListPrice
                
                ## Additional learning may be possible given every buyer's highest price affinity
                ## TODO
                ## but this funcitonality will be left to future research ATM
                ## buyerEntry$Price[[1]][[2]]
                
                bhmEntry$Offer_AV <- offer$Bid / bhmEntry$Price
                bhmEntry$Offer <- offer$Bid
                bhmEntry$Offer_Time <- offer$Time
                
                realtor@BuyerHouseMatch <-
                  rbind(realtor@BuyerHouseMatch, bhmEntry) %>% dplyr::arrange(desc(Offer_Time)) %>% distinct(Buyer, Address, .keep_all = TRUE)
              }
              realtor@NewOffers <- 
                rbind(realtor@NewOffers, offer) %>% filter(Time >= realtor@TimeCurrent) %>% distinct(Buyer, .keep_all = TRUE)
              
              # 
              # if (length(realtor@NewOffers) != 0) {
              #   realtor@NewOffers <-
              #     rbind(realtor@NewOffers, offer) %>% distinct(Buyer, .keep_all = TRUE)
              # }
              # else {
              #   realtor@NewOffers <- offer
              # }
              # 
              ## Cast to data.frame to regress and replace buyer Preferences
              ## buyer_offers <- as.data.frame(buyerEntry$Offers)
              
              
              ## TODO
              ## Must update individual preferences based on Offer(s) and the Buyer's Feedback
              
              # database <-
              #   filter(realtor@BuyerHouseMatch,
              #          Buyer == client@Name,!is.na(Offer_AV))
              
              ## Current problem: Many Poss's are 1, limiting the regression's abilitiy to
              ## approximate the preferences
              
              ## Therefore, I cannot use this method
              
              ## Thus, I will need to update the weights more maticulously
              ## For the attribute who is in the Feedback, reduce the weight by some unit and increase the other
              ## weights proportionally to themselves until the predicted AV is equal to the Offer AV
              ## This still lacks any influence from the fuzziness but that will be solved later
              ## TODO
              
              ## Decided to show two cases:
              #### One with perfect infomraiton
              #### One without perfect information and no learning
              ####    The code is retained for record
              
              
              # if (!random) {
              #   pred_AV <- bhmEntry$Buyer_AV
              #   possVec <- as.numeric(bhmEntry[12:17])
              #   weights <- as.numeric(buyerEntry$Preferences[[1]])
              #   loops <- 10
              #   l <- 1
              #   ## Maybe review this and use Fuzzy logic to
              #   ## shrink the computation time
              #   ## TODO
              #   while (pred_AV != bhmEntry$Offer_AV &&
              #          l <= loops && sum(weights) != 6) {
              #     ## Adjusting every parameter may not be the best course of action
              #     ## More research is necessary to find a better solution
              #     ## TODO
              #     for (w in 1:length(weights)) {
              #       ## Using the mean to signify low or high values
              #       ## Using 0.05 as an update parameter ... conveniently
              #       
              #       ## When the prediced AV is less than the Offer AV,
              #       ## Increase the weight of the high Poss Vec items
              #       ## Decrease the weight of the low Poss Vec items
              #       if (pred_AV < bhmEntry$Offer_AV) {
              #         if (possVec[w] >= mean(possVec)) {
              #           weights[w] <- weights[w] * 1.05
              #           #break
              #         }
              #         else{
              #           weights[w] <- weights[w] * 0.95
              #           #break
              #         }
              #       }
              #       ## When the predicted AV is greater than the Offer AV
              #       ## Decrease the weight of the high possVec items
              #       ## Increase the weight of the low possVec items
              #       else{
              #         if (possVec[w] <= mean(possVec)) {
              #           weights[w] <- weights[w] * 1.05
              #           #break
              #         }
              #         else{
              #           weights[w] <- weights[w] * 0.95
              #           #break
              #         }
              #       }
              #       
              #       l <- l + 1
              #     }
              #     
              #     ## normalize to 1
              #     weights <- weights / sqrt(sum(weights ^ 2))
              #     ## Recalculate
              #     pred_AV <- weights %*% possVec
              #     
              #   }
              #   weight_list <- as.list(weights)
              #   names(weight_list) <-
              #     names(realtor@Mkt_Av_Preferences_Buyers)
              #   buyerEntry$Preferences[[1]] <- weight_list
              # }
              buyerEntry$UpdateTime <- realtor@TimeCurrent + 0.1
              
              realtor@Buyers <-
                rbind(realtor@Buyers, buyerEntry) %>% dplyr::arrange(desc(UpdateTime)) %>% distinct(Name, .keep_all = TRUE)
            }
            return (realtor)
          })


## Create a function for multiplying a Requirement and a numeric
setGeneric("realtorGuess",
           function(given, belief) {
             standardGeneric("realtorGuess")
           })

setMethod("realtorGuess",
          signature("list", "Requirement"),
          function(given, belief) {
            if (length(given) == 2) {
              fn <- PiecewiseLinearFuzzyNumber(
                belief@support1 * given[[1]],
                belief@core1    * given[[1]],
                belief@core2    * given[[2]],
                belief@support2 * given[[2]]
              )
            }
            else{
              fn <- PiecewiseLinearFuzzyNumber(
                belief@support1 * given[[1]],
                belief@core1    * given[[1]],
                belief@core2    * given[[1]],
                belief@support2 * given[[1]]
              )
            }
            return (fn)
          })

## Function to make matches
setGeneric("matchMake",
           function(realtor) {
             standardGeneric("matchMake")
           })
setMethod("matchMake",
          signature("Realtor"),
          function(realtor) {
            ## Go Through the lists of buyers and houses
            ## Calcualte the Appropriteness Values of every house for every buyer
            ## Weight Appropriatness values by expected sale price
            ## inform all Buyers of 3 top choices for them,
            ## There may be overlap, Realtor may learn to create competition

            for (b in 1:nrow(realtor@Buyers)) {
              byr <- realtor@Buyers[b,]
              
              ## Columns 2-7 are the requirements
              ## Column 8 Fuzziness
              ## 9 Preferences
              
              #filter(Price <= byr$Price[[1]][[1]] * byr$Fuzziness[[1]]$Price@support2) %>%
              houses <- realtor@Houses %>%
                select(Address, Price, Beds, Baths, SqrFt, LotSize) %>%
                filter(Price <= byr$Price[[1]][[1]] * (byr$Fuzziness[[1]]$Price@support2 + 0.1))
              ## Even though the realtor knows the exact fuzziness of the buyer in my model, 
              ## it is worthwhile to incrase the price fuzziness a little bit more given the 
              ## Requirement/Preference strucutre I laid in.
              
              if(nrow(houses) == 0){
                ## If there no houses within the price range for this buyer, 
                ## jump to the next one
                next
              }

              buyerhousematch <-
                as.data.frame((matrix(ncol = 20, nrow = 0)))
              for (h in 1:nrow(houses)) {
                house <- houses[h, ]
                buyerhousematchEntry <-
                  list(
                    "TimeStamp" = realtor@TimeCurrent,
                    "Buyer" = byr$Name,
                    "Offer" = NA,
                    "Offer_AV" = NA,
                    "Offer_Time" = NA
                  )
                buyerhousematchEntry <-
                  append(buyerhousematchEntry, house)
                buyerhousematchEntry$Address <- house$Address
                
                AV_buyer <- 0
                AV_seller <- 0
                seller <-
                  filter(realtor@Sellers, House == house$Address)
                
                buyerhousematchEntry$Seller <- seller$Name
                
                seller_price_fn <-
                  realtorGuess(seller$Price[[1]], seller$Fuzziness[[1]]$Price)
                byr_price_fn <-
                  realtorGuess(byr$Price[[1]], byr$Fuzziness[[1]]$Price)
                
                ## Run recommendation calculations
                for (req in names(realtor@Mkt_Av_Preferences_Buyers)) {
                  ## For Price and Time we need to Match the seller's numbers
                  ## For now, assuming that as long as we maximize the buyer's AV,
                  ## The sellers will also have maximized AV ...
                  ## I doubt this assumption and would like to exapnd on it to balance
                  ## While sellers also want Time to Undervaluation, they would rather see
                  ## Price Exceed - so would the Realtor
                  
                  
                  ## While the seller wants a higher price, they still want Buyers to
                  ## be able to afford their house
                  if (req == "Price") {
                    
                    ## If the realtor evaluates how likely the buyer's price range 
                    ## is below the seller's price range as part of the likelihood of sale,
                    ## then there are often missed opportunities of buyer valuation
                    ## In early tests, this hindered numbers of sales as only buyers and sellers
                    ## of 'agreeable' price fuzziness were able to exchange
                    
                    ## I have decided to reproduce this as the list price comparison for now
                    ## This may change for future research
                    
                    ## How likely is the house price under the buyer's fuzzy number
                    h_FN <-
                      PiecewiseLinearFuzzyNumber(house[[req]], house[[req]], house[[req]], house[[req]])
                    poss = possibilityUndervaluation(h_FN, byr_price_fn)
                    AV_buyer <-
                      AV_buyer + byr$Preferences[[1]][[req]] * poss
                    buyerhousematchEntry$Price_Poss = poss
                    ## Seller's price AV is calculated and added in after the Buyer's Bid is generated
                    
                  }
                  else if (req == "Time") {
                    ## Generate the Fuzzy Number representation for the buyer
                    ## Note: The sale would happen SOON, unless we know the buyer's 'PlayLag';
                    ## or if 'PlayLag' was generated from the Buyer's Time parameter
                    ## But since we're matching AT PRESENT the Time requirement will likely be 1
                    b_FN <-
                      realtorGuess(byr[[req]][[1]], byr$Fuzziness[[1]][[req]])
                    s_FN <-
                      realtorGuess(seller[[req]][[1]], seller$Fuzziness[[1]][[req]])
                    now_FN <- 
                      PiecewiseLinearFuzzyNumber(realtor@TimeCurrent,realtor@TimeCurrent,realtor@TimeCurrent,realtor@TimeCurrent)
                    
                    poss_b_s = possibilityUndervaluation(b_FN, s_FN)
                    poss_n_s = possibilityUndervaluation(now_FN, s_FN)
                    poss_n_b = possibilityUndervaluation(now_FN, b_FN)
                    
                    poss_s = max(poss_b_s,poss_n_s)
                    poss_b = max(poss_b_s,poss_n_b)
                    AV_buyer <-
                      AV_buyer + byr$Preferences[[1]][[req]] * poss_b
                    AV_seller <-
                      AV_seller + seller$Preferences[[1]][[req]] * poss_s
                    buyerhousematchEntry$Time_Poss = poss_b
                  }
                  else{
                    b_FN <- realtorGuess(byr[[req]][[1]], byr$Fuzziness[[1]][[req]])
                    h_FN <-
                      PiecewiseLinearFuzzyNumber(house[[req]], house[[req]], house[[req]], house[[req]])
                    poss <- possibilityExceedance(h_FN, b_FN)
                    AV_buyer <-
                      AV_buyer + byr$Preferences[[1]][[req]] * poss
                    buyerhousematchEntry[[paste0(req, "_Poss")]] = poss
                    
                  }
                }
                
                ## Now with Both AV's we must calculate the best matches for every Buyer that Maximize
                ## Sale Price - since I assume that the Realtor's commission is an increasing function
                ## of sale price
                ## This requires the AV_seller and AV_buyer are acceptable to BOTH parties
                ## => the resultant price generated by our expecatoin about the Buyer is larger
                ##    than or equal to the acceptable price of the seller
                
                ## 0.02 was chosen arbitarily to accomidate lack of confidence
                ## will investigate other numbers in the future
                
                buyer_value  <-
                  min( (AV_buyer) * byr_price_fn@a3,
                       byr_price_fn@a4)
                bid = min(house$Price, buyer_value)
                
                byr_bid_fn <- PiecewiseLinearFuzzyNumber(bid,bid,bid,bid)
                poss = possibilityExceedance(byr_bid_fn, seller_price_fn)
                AV_seller <-
                  AV_seller + seller$Preferences[[1]][["Price"]] * poss
                
                if(AV_seller < seller$MinQoS){
                  next
                }

                #if(AV_buyer >= byr$MinQoS && AV_seller >= seller$MinQoS){

                  buyerhousematchEntry$Buyer_AV <- AV_buyer
                  buyerhousematchEntry$Buyer_Value = buyer_value
                  buyerhousematchEntry$Estimated_Bid = as.numeric(bid)
                  buyerhousematchEntry$Seller_AV <- AV_seller
                  
                  buyerhousematchEntry_DF <-
                    as.data.frame(buyerhousematchEntry)
                  
                  buyerhousematch <-
                    rbind(buyerhousematch, buyerhousematchEntry_DF)
                #}
                  
                ## So long as the seller's valuye is larger than her minimum support
                ## and the expected bid is larger than or equal to the seller value,
                ## The Realtor believes this transaction is possible
                ## and will suggest the home to the buyer
                ## Externally.
                ## The Realtor will offer the top 3 matches, regardless of how good they actually are
                ## to give the Realtor more learning
              }
              ## Now arrange them all by descending Estimated_Bid,
              ## and return only the top 3
              ## May parameterize the 3
              if(nrow(buyerhousematch) <= 0){
                ## If there no house matches for this buyer, 
                ## jump to the next one
                next
              }
              
              buyerhousematch <- buyerhousematch %>%
                arrange(desc(TimeStamp)) %>%
                distinct(Buyer, Address, .keep_all = TRUE)
            
              
              realtor@BuyerHouseMatch <-
                rbind(realtor@BuyerHouseMatch, buyerhousematch)
              
              realtor@Buyers[b, "UpdateTime"] <- realtor@TimeCurrent
              
              
            }
            
            #print(head(realtor@BuyerHouseMatch))
            return(realtor)
            
            ## In the step function or higher, update all the Realtor's Buyers on their Matches
          })

## Update the current time
## Recalculate the Market Averages for Preferences and Fuzziness


setGeneric("updateMarketAverages",
           function(realtor,buyers,sellers) {
             standardGeneric("updateMarketAverages")
           })
setMethod("updateMarketAverages",
          signature("Realtor","list","list"),
          function(realtor,buyers,sellers) {
            ## Create Place-holders for the averages
            ## Since I stored the data in a (suboptimal) way, I need to loop
            ## Through it all ....
            ## May change it up if necessary to improve efficiency
            ## Would undo the condensed formatting I have it all in in the data.frames and expand it out
            ## to make better use of R's vectorization
            ## Honestly, there will seldom be even 100 buyers or sellers for a realtor to manage so .. this may not be that bad
            
            ## Rearange and clean for multiple uses: internal buyer updating and external
            ## or just make it external.
            
            
            b_pref_av <- rep(0, 6)
            b_fuzzy_av <- list(
              "Price" = Requirement(0, 0, 0, 0),
              "Beds" = Requirement(0, 0, 0, 0),
              "Baths" = Requirement(0, 0, 0, 0),
              "SqrFt" = Requirement(0, 0, 0, 0),
              "LotSize" = Requirement(0, 0, 0, 0),
              "Time" = Requirement(0, 0, 0, 0)
            )
            
            s_pref_av <- rep(0, 2)
            s_fuzzy_av <- list("Price" = Requirement(0, 0, 0, 0),
                               "Time" = Requirement(0, 0, 0, 0))
            
            ## Average all buyer and seller preferences
            ## inside these loops, loop the list of Fuzzy requirements
            ## Then loop the final fuzzy requiremetns to get their averages
            
            
            ## For Buyer Preferences & Fuzziness
            if (nrow(buyers) > 0) {
              for (buyer in buyers) {
                b_pref_av <-
                  b_pref_av + as.numeric(buyer$Preferences)
                for (item in names(b_fuzzy_av)) {
                  b_fuzzy_av[[item]] <-
                    sumRequirements(b_fuzzy_av[[item]], attr(buyer,item))
                }
              }
              #Average each preference
              b_pref <- b_pref_av / nrow(realtor@Buyers)
              # Normalize to 1
              b_pref <- b_pref / norm(as.matrix(b_pref, nrow = 1))
              #Coerce to list
              b_pref <- as.list(b_pref)
              names(b_pref) <-
                names(realtor@Mkt_Av_Preferences_Buyers)
              realtor@Mkt_Av_Preferences_Buyers <- b_pref
              
            }
            
            ## For seller Preferences & Fuzziness
            if (nrow(sellers) > 0) {
              for (seller in sellers) {
                s_pref_av <-
                  s_pref_av + as.numeric(seller@Preferences)
                for (item in names(s_fuzzy_av)) {
                  s_fuzzy_av[[item]] <-
                    sumRequirements(s_fuzzy_av[[item]], attr(seller,item))
                }
              }
              #Average each preference
              s_pref <- s_pref_av / nrow(realtor@Sellers)
              #Normalize to 1
              s_pref <- s_pref / norm(as.matrix(s_pref, nrow = 1))
              #Coerce to a list
              s_pref <- as.list(s_pref)
              names(s_pref) <-
                names(realtor@Mkt_Av_Preferences_Sellers)
              realtor@Mkt_Av_Preferences_Sellers <- s_pref
              
            }
            
            ## Averaging the Fuzzy parameters
            for (item in names(b_fuzzy_av)) {
              b_fuzzy_av[[item]] <-
                averageRequirements(b_fuzzy_av[[item]], nrow(realtor@Buyers))
              if (item == "Price" | item == "Time") {
                s_fuzzy_av[[item]] <-
                  averageRequirements(s_fuzzy_av[[item]], nrow(realtor@Sellers))
              }
            }
            
            realtor@Mkt_Fuzziness_Buyers <- b_fuzzy_av
            realtor@Mkt_Fuzziness_Sellers <- s_fuzzy_av
            
            return(realtor)
          })


## Inform Realtor of Accepted Offers
setGeneric("executeSale",
           function(realtor, offer, random) {
             standardGeneric("executeSale")
           })
setMethod("executeSale",
          signature("Realtor", "list", "logical"),
          function(realtor, offer, random) {
            ## Go Through the dataframe of NewOffers and inform the buyers of their
            ## concluded sale
            ## Remove the house from houses known
            ## Remove the buyer and seller from realtor's lists
            #print(s@Name)
            ## Remove all offers on this House
            realtor@NewOffers <-
              filter(realtor@NewOffers, Address != as.character(offer$Address))
            #print("sold")
            #print(as.character(offer$Address))
            #print("to")
            ## Remove this buyer
            realtor@Buyers <-
              filter(realtor@Buyers, Name != as.character(offer$Buyer))
            #print(as.character(offer$Buyer))
            if (!random) {
              realtor@BuyerHouseMatch <-
                filter(realtor@BuyerHouseMatch,
                       Buyer != as.character(offer$Buyer))
            }
            
            ## Remove this seller
            realtor@Sellers <-
              filter(realtor@Sellers, House != as.character(offer$Address))
            
            ## Remove the house
            realtor@Houses <-
              filter(realtor@Houses, Address != as.character(offer$Address))
            
            realtor@TotalCommission <-
              realtor@TotalCommission + realtor@CommissionPct * offer$Bid
            
            return(realtor)
            
            
          })