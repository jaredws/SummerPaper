## Main System executable
## Using random encounter matching

## I still employ the realtor framework, but there is no matchMaking, only the random draw

main <-
  function(ITERATIONS = 10,
           SEED = 1234,
           RANDOM = TRUE,
           LAGPLAY = FALSE,
           RUN_NAME = "DEFAULT") {
    start <- Sys.time()
    
    print(paste("Start time:", start))
    
    if (!is.null(SEED)) {
      set.seed(SEED)
    }
    
    
    
    suppressWarnings(
      "In if (!is.na(offer)) { :
                 the condition has length > 1 and only the first element will be used"
    )
    ## Still some data cleaning around prices and organizing
    ## Will do that later
    dataToDraw <- read.csv("dataToDraw.csv")
    dataToDraw <- filter(dataToDraw, Price <= 500000)
    ## Be sure date is in the proper format
    dataToDraw$Date <- as.Date.factor(dataToDraw$Date)
    dataToDraw$SqrFt <- as.numeric(dataToDraw$SqrFt)
    
    
    
    ## Parameters for Feedback and initial information at the Realtor's disposal
    ## Can have variants within the system
    Clarity <- 3
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
    
    BuyerList_noRealtor <- list()
    SellerList_noRealtor <- list()
    RealtorList_noRealtor <- list()
    BuyerInactive_noRealtor <- list()
    SellerInactive_noRealtor <- list()
    
    REALTOR <- Realtor(
      Name = RUN_NAME,
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
        "Price" = c(1 / 6),
        "Beds" = c(1 / 6),
        "Baths" = c(1 / 6),
        "SqrFt" = c(1 / 6),
        "LotSize" = c(1 / 6),
        "Time" = c(1 / 6)
      ),
      Mkt_Av_Preferences_Sellers = list("Price" = c(0.5),
                                        "Time" = c(0.5)),
      CommissionPct = 0.05,
      TimeCurrent = 1
    )
    
    RealtorList_noRealtor[[REALTOR@Name]] = REALTOR
    
    
    ## The collective data of this run
    ## Each row is an iteration
    iteration_stats_noRealtor <-
      as.data.frame(matrix(
        nrow = 0,
        ncol = 6 + length(RealtorList_noRealtor) * 6
      ))
    
    ## Each row is a house sale
    house_sales_noRealtor <-
      as.data.frame(matrix(nrow = 0, ncol = 10))
    
    for (t in 1:ITERATIONS) {
      ## 0 create iteration row and stats
      t <- as.numeric(t)
      iter_row <- list("Iteration" = t)
      saleCount <- 0
      offerCount <- 0
      avBuyerSatisfaction <- 0
      avSellerSatisfaction <- 0
      priceIncreases <- 0
      
      ## 1 add buyers and sellers to the system by name
      ## since names are generated using ComputerTimeStamps as strings for simplicity,
      ## and are thus unique, there is no need to worry about overwritting
      
      genBuyers <-
        generateBuyers(
          t,
          names(RealtorList_noRealtor),
          dataToDraw,
          Clarity,
          Responsiveness,
          buyer_params,
          fuzzy_param
        )
      dataToDraw <- genBuyers[[2]]
      genSellers <-
        generateSellers(
          t,
          names(RealtorList_noRealtor),
          dataToDraw,
          Clarity,
          seller_params,
          fuzzy_param
        )
      dataToDraw <- genSellers[[2]]
      
      newBuyers <- genBuyers[[1]]
      newSellers <- genSellers[[1]]
      
      BuyerList_noRealtor <- append(BuyerList_noRealtor, newBuyers)
      SellerList_noRealtor <-
        append(SellerList_noRealtor, newSellers)
      
      iter_row$nBuyers <- length(BuyerList_noRealtor)
      iter_row$nSellers <- length(SellerList_noRealtor)
      
      ## 2 Loop through realtors
      ## 2.1 Inform realtor of their new clients
      ## 2.2 Matchmake within Realtor
      ## 2.3 inform Buyers
      ## 2.3.1 buyers make offers & generate feedback
      ## 2.3.2 Realtors get feedback and offers from Buyers
      ##### May need to break this out again for cross-realtor matching?
      RealtorsRunning <- list()
      for (r in RealtorList_noRealtor) {
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
        if (!RANDOM) {
          r <- matchMake(r)
          if (nrow(r@BuyerHouseMatch) == 0) {
            ## if there are no matches, just loop again, everyone will have their time updated when necessary
            RealtorsRunning[[r@Name]] <- r
            break
          }
        }
        
        ## 2.2 Random Discovery
        ### Takes place below in the sampling of the realtor's houses
        
        # 2.3 Inform Buyers
        # 2.3.1 Buyers discover houses
        # 2.3.2 Buyers make offers
        # 2.3.3 Realtors get feedback and offers from Buyers
        BuyersRunning <- list()
        for (b in BuyerList_noRealtor) {
          if (b@Realtor != r@Name) {
            BuyersRunning[[b@Name]] <- b
            next
          }
          
          b@TimeCurrent = t
          
          ## Randomly sample 3 hosues from those on the market
          ## There may be 'reapeat discoveries'
          ## Since some houses get updated on their price, this should be fine
          if (RANDOM) {
            n <- 3;
            if(r@Name == "RandomDraw5"){
              n <- 5;
            }
            n <- min(n,nrow(r@Houses))
            
            houses <- sample_n(r@Houses, n, replace = FALSE) %>%
              select(Address,
                     Price,
                     Beds,
                     Baths,
                     SqrFt,
                     LotSize,
                     UpdateTime)
          }
          
          if (r@Name == "PerfectInfo") {
            houses <- filter(r@BuyerHouseMatch, Buyer == b@Name) %>%
              arrange(desc(TimeStamp), desc(Buyer_AV)) %>%
              select(Address, Price, Beds, Baths, SqrFt, LotSize, TimeStamp)
          }
          ## Else, sort descending by Buyer Value to max profit
          if (r@Name == "MaxProfit") {
            houses <- filter(r@BuyerHouseMatch, 
                             Buyer == b@Name,
                             Buyer_AV >= b@MinQoS) %>%
              arrange(desc(TimeStamp), desc(Buyer_Value)) %>%
              select(Address, Price, Beds, Baths, SqrFt, LotSize, TimeStamp)
          }
          ## Only inform on the top 3
          ## Gain information every turn, even if no action is taken
          if (nrow(houses) > 3) {
            houses <- houses[1:3,]
          }
          
          colnames(houses) <-
            c("Address",
              "Price",
              "Beds",
              "Baths",
              "SqrFt",
              "LotSize",
              "Time")
          
          ## Always inform buyer's of new houses for sale
          b <- informBuyer(b, houses)
          
          ## If the buyer is not acting this turn.
          ## He does not offer,
          if (b@PlayLag > t && LAGPLAY) {
            BuyersRunning[[b@Name]] <- b
            next
          }
          
          ## else...
          ## Let the next wait time be commonly within a week
          b@PlayLag = t + ceiling(rexp(1, 1 / 4))
          
          ## Buyer is acting so he formulates an offer
          b <- makeOffer(b)
          
          if (nrow(b@CurrentOffer) == 1) {
            offerCount  <- offerCount + 1
          }
          
          ## giveFeedback() was for learning funcitonality but such functionality is broken and will not be used
          ## getFeedback() takes the current offer as well so we use that
          #b <- giveFeedback(b)
          r <- getFeedback(r, b, RANDOM)
          
          BuyersRunning[[b@Name]] <-  b
        }
        BuyerList_noRealtor <- BuyersRunning
        
        ## 3 Inform Sellers
        ## 3.1 Sellers hear about new offers
        ## 3.2 Sellers either raise the price, accept an offer, or decline all offers
        SellersRunning <- list()
        for (s in SellerList_noRealtor) {
          s@TimeCurrent = t
          if (r@Name == s@Realtor) {
            if (nrow(r@NewOffers) == 0) {
              ## If there are no offers this round, we 'should' update every Seller's current time
              SellersRunning[[s@Name]] <- s
              next
            }
            s <-
              informSeller(s, filter(
                r@NewOffers,
                as.character(s@House$Address) == Address
              ))
            if (nrow(s@CurrentOffers) == 0) {
              SellersRunning[[s@Name]] <- s
              next
            }
            s <- acceptOffer(s)
            if (nrow(s@AcceptedOffer) > 0 &&
                !is.na(s@AcceptedOffer)) {
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
                priceIncreases <- priceIncreases + 1
                next
              }
              
              ## Since buyers can only commit to one offer,
              ## There can be no overlap in these statements
              
              ## 4.1 Extract data before the agents are removed
              ## There may be more data to remove
              
              ##print(s@AcceptedOffer)
              
              saleCount <- saleCount + 1
              avBuyerSatisfaction <-
                avBuyerSatisfaction + BuyerList_noRealtor[[as.character(s@AcceptedOffer$Buyer)]]@Satisfaction
              avSellerSatisfaction <-
                avSellerSatisfaction + s@Satisfaction
              
              ## Add the home to houe_sales
              house_entry <-
                as.data.frame(
                  c(
                    t,
                    as.numeric(s@AcceptedOffer$Bid),
                    s@House,
                    s@Name,
                    as.character(s@AcceptedOffer$Buyer),
                    s@Realtor,
                    BuyerList_noRealtor[[as.character(s@AcceptedOffer$Buyer)]]@Satisfaction,
                    s@Satisfaction,
                    nrow(s@PriceIncreases),
                    as.numeric(t - s@Time@core1)
                  )
                )
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
              house_sales_noRealtor <-
                rbind(house_sales_noRealtor, house_entry)
              
              ## Before executing the sale, update Market averages
              #r <- updateMarketAverages(r)
              ## Not learning so no need to update market averages
              
              ## Still need to remove agents from the system
              r <- executeSale(r, s@AcceptedOffer, RANDOM)
              
              ## 4.2 Remove agents of completed transaction
              SellerInactive_noRealtor[[s@Name]] <- s
              BuyerInactive_noRealtor[[as.character(s@AcceptedOffer$Buyer)]] <-
                BuyerList_noRealtor[[as.character(s@AcceptedOffer$Buyer)]]
              ##print("")
              ##print(paste("Accepted Offer Buyer",as.character(s@AcceptedOffer$Buyer)))
              
              ## Removing the Buyer is a bit trickier
              ## First re-assign the last Buyer in the list to the named index we are removing
              ## Get the last buyer
              replace = BuyerList_noRealtor[[length(BuyerList_noRealtor)]]
              ## Set the value of the last item to NULL
              BuyerList_noRealtor[length(BuyerList_noRealtor)] <-
                NULL
              
              #print(paste("Removing",as.character(s@AcceptedOffer$Buyer),"by replacing with",replace@Name))
              
              if (replace@Name == as.character(s@AcceptedOffer$Buyer)) {
                ## We are done!
              }
              else{
                ## Re-set the name of re-indexed buyer
                names(BuyerList_noRealtor)[which(names(BuyerList_noRealtor) == as.character(s@AcceptedOffer$Buyer))] <-
                  replace@Name
                BuyerList_noRealtor[replace@Name] <- replace
              }
              
              
              ## Go through each Buyer's dataset and drop houses that are no longer on the market
              buyerNotify <- r@BuyerHouseMatch %>%
                filter(Address == as.character(s@House$Address))
              buyerNotify <- as.list(buyerNotify$Buyer)
              for (b in buyerNotify) {
                b <- as.character(b)
                BuyerList_noRealtor[[b]] <-
                  removeHouse(BuyerList_noRealtor[[b]],
                              as.character(s@House$Address))
              }
              ## Call the next Seller, so we do not append this seller to SellersRunning
              ## and it is removed from Seller List
              next
            }
          }
          SellersRunning[[s@Name]] <- s
        }
        
        SellerList_noRealtor <- SellersRunning
        ##print(BuyerList_noRealtor)
        
        
        RealtorsRunning[[r@Name]] <- r
        
      }
      RealtorList_noRealtor <- RealtorsRunning
      
      ## Add Realtor Learning to the iteration data
      #iter_row[[paste0(r@Name, "_SaleCount")]] == r@SaleCount
      iter_row[["TotalCommission"]] = r@TotalCommission
      
      
      ## 6 Other iteration stats
      iter_row$Sales <- saleCount
      iter_row$Offers <- offerCount
      # Returns 'Inf' if saleCount=0
      iter_row$AverageBuyerSatisfaction <-
        avBuyerSatisfaction / saleCount
      iter_row$AverageSellerSatisfaction <-
        avSellerSatisfaction / saleCount
      
      iter_row$PriceIncreases <- priceIncreases
      
      
      ## Loop
      
      ## Need probabilities to add Buyers and Sellers to the system
      ## Need functions for initializing them on random
      
      if ((t * 100 / ITERATIONS) %% (10) == 0) {
        print(paste0("Complete: ", (t * 100 / ITERATIONS), "% ", Sys.time()))
      }
      
      iteration_stats_noRealtor <-
        rbind(iteration_stats_noRealtor, as.data.frame(iter_row))
      
    }
    
    end <- Sys.time()
    
    print(paste("Run-time:", end - start))
    
    # view(iteration_stats_noRealtor)
    #
    # ggplot(iteration_stats_noRealtor) +
    #   geom_col(aes(x = Iteration, y = Sales)) +
    #   ggtitle("Random Draw")
    
    RETURN <- list(
      "BuyerList" <- BuyerList_noRealtor,
      "SellerList" <- SellerList_noRealtor,
      "RealtorList" <- RealtorList_noRealtor,
      "Iteration_stats" <- iteration_stats_noRealtor,
      "House_sale_stats" <- house_sales_noRealtor,
      "BuyerInactive" <- BuyerInactive_noRealtor,
      "SellerInactive" <- SellerInactive_noRealtor
    )
    
    return(RETURN)
    
    
  }