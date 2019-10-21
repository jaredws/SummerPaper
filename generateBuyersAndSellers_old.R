## Generate Buyers and Sellers


library(dplyr)
library(FuzzyNumbers)
#library(tidyverse)
#library(ggplot2)
library(stats)
## Time is created from the order of the dates of postings in


generateSellers <- function(t,
                            realtorNames,
                            house_data,
                            clarity,
                            seller_params,
                            fuzzy_param,
                            seed = NULL) {
  ## Number of sellers to add
  if(!is.null(seed)){
    set.seed(seed)
  }
  listings <-
    filter(house_data, Event == "Listed for sale") %>%
    select(Address,
           Price,
           Beds,
           Baths,
           SqrFt,
           LotSize,
           Date,
           Time,
           RollTimeAv,
           Event) %>%
    arrange(Date)
  n_Sellers <- min(max(floor(rnorm(1, 6, 2)),0), nrow(listings))
  sellers <- list()
  if (n_Sellers > 0) {
    house_draws <- dplyr::sample_n(
      tbl = listings,
      size = n_Sellers,
      replace = FALSE,
      weight = 1 / (Time - t + 1)
    )
    
    
    for (r in 1:nrow(house_draws)) {
      entry <- house_draws[r,]
      realtorName <- realtorNames[runif(1, 1, length(realtorNames))]
      pref <-
        c(
          abs(rnorm(
            1,
            mean = seller_params$Price$Mean,
            sd = seller_params$Price$Stdev
          )),
          abs(rnorm(1, mean = seller_params$Time$Mean, sd = seller_params$Time$Stdev)
        ))
      pref <- pref / norm(as.matrix(pref, nrow = 1))
      
      fuzzy <- c(
        runif(1, fuzzy_param$S1min, fuzzy_param$S1max),
        runif(1, fuzzy_param$S2min, fuzzy_param$S2max)
      )
      
      s <- Seller(
        Name = paste("S",t,r),
        Price = Requirement(
          entry$Price * fuzzy[1],
          entry$Price,
          entry$Price,
          entry$Price * fuzzy[2]
        ),
        Time = Requirement(t,
                           t,
                           max(c(entry$Time, t)),
                           entry$RollTimeAv + max(c(entry$Time, t))),
        House = entry,
        Preferences = list("Price" = pref[1], "Time" = pref[2]),
        ## The Weights the seller puts on Price and Time,
        Clarity = clarity,
        ## The Level of clarity with the Realtor on Price and Time Fuzziness
        MinQoS = rnorm(
          1,
          seller_params$MinQoS$Mean,
          seller_params$MinQoS$Stdev
        ),
        ## The Threshold an offer must make to be accepted
        Realtor = realtorName,
        ## The name of the Realtor the Seller is working with
        TimeCurrent = t,
        PriceIncreases = as.data.frame(matrix(nrow = 0, ncol = 2), col.names = c("Iteration", "NumberEqualOffers"))
      )
      
      house <- as.data.frame(
        list(
          "Address" = as.character(entry$Address),
          "Price" = as.numeric(entry$Price),
          "Beds" = as.numeric(entry$Beds),
          "Baths" = as.numeric(entry$Baths),
          "SqrFt" = as.numeric(entry$SqrFt),
          "LotSize" = as.numeric(entry$LotSize),
          "Time" = as.numeric(mean(c(s@Time@core1, s@Time@core2)))
        )
      )
      s@House <- house
      sellers[[s@Name]] <- s
      
    }
    ## Remove the drawn houses from the pool
    listings_remain <-
      anti_join(house_data, house_draws, by = c("Address","Event"))
  }
  
  else{
    listings_remain <- house_data
  }
  
  return (list("SellerList" = sellers, "HouseData" = listings_remain))
}



generateBuyers <- function(t,
                           realtorNames,
                           house_data,
                           clarity,
                           responsiveness,
                           buyer_params,
                           fuzzy_param,
                           seed = NULL) {
  
  if(!is.null(seed)){
    set.seed(seed)
  }
  
  ## Only sample the sales
  sales <- filter(house_data, Event == "Sold") %>%
    select(Address,
           Price,
           Beds,
           Baths,
           SqrFt,
           LotSize,
           Date,
           Time,
           RollTimeAv,
           Event) %>%
    arrange(Date)
  
  ## Number buyers to add
  n_Buyers <- min(max(floor(rnorm(1, 6, 2)),0) , nrow(sales))
  buyers <- list()
  if (n_Buyers > 0) {
  
    ## Draw again with replacement and similiar weighting for the buyer's
    buyer_draws <- dplyr::sample_n(
      tbl = sales,
      size = n_Buyers,
      replace = TRUE,
      weight = 1 / (Time - t + 1)
    )

    
    ## Now the buyers
    
    for (r in 1:nrow(buyer_draws)) {
      entry <- buyer_draws[r,]
      realtorName <- realtorNames[runif(1, 1, length(realtorNames))]
      
      pref = c(
        abs(rnorm(1, buyer_params$Price$Mean, buyer_params$Price$Stdev)),
        abs(rnorm(1, buyer_params$Beds$Mean, buyer_params$Beds$Stdev)),
        abs(rnorm(1, buyer_params$Baths$Mean, buyer_params$Baths$Stdev)),
        abs(rnorm(1, buyer_params$SqrFt$Mean, buyer_params$SqrFt$Stdev)),
        abs(rnorm(
          1,
          buyer_params$LotSize$Mean,
          buyer_params$LotSize$Stdev
        )),
        abs(rnorm(1, buyer_params$Time$Mean, buyer_params$Time$Stdev))
      )
      # Normalize to 1
      pref <- pref / norm(as.matrix(pref, nrow = 1))
      
      fuzzy <- c(
        runif(1, fuzzy_param$S1min, fuzzy_param$S1max),
        runif(1, fuzzy_param$S2min, fuzzy_param$S2max)
      )
      
      ## The Time parameter is tricky
      ## According to the NAR in 2018, the median time searching for a buyer is 12
      ## weeks with a realtor and 3 weeks before notifying a realtor
      ## I will assume a normal distribution with mean of 12 and standard deviation of 7 (1 week)
      ## in both cases where the buyer goes through a realtor and the buyer does not
      
      # Time Core 2
      tcore2 = max(rnorm(1,12*7, 7),7)
      
      b <- Buyer(
        Name = paste("B",t,r),
        Price = Requirement(
          entry$Price * fuzzy[1],
          entry$Price,
          entry$Price,
          entry$Price * fuzzy[2]
        ),
        Beds = Requirement(
          entry$Beds * fuzzy[1],
          entry$Beds,
          entry$Beds,
          entry$Beds * fuzzy[2]
        ),
        Baths = Requirement(
          entry$Baths * fuzzy[1],
          entry$Baths,
          entry$Baths,
          entry$Baths * fuzzy[2]
        ),
        SqrFt = Requirement(
          entry$SqrFt * fuzzy[1],
          entry$SqrFt,
          entry$SqrFt,
          entry$SqrFt * fuzzy[2]
        ),
        LotSize = Requirement(
          entry$LotSize * fuzzy[1],
          entry$LotSize,
          entry$LotSize,
          entry$LotSize * fuzzy[2]
        ),
        Time = Requirement(t* fuzzy[1],
                           t + 7,
                           t + tcore2,
                           (t + tcore2) * fuzzy[2]),
        MinQoS = rnorm(1, buyer_params$MinQoS$Mean, buyer_params$MinQoS$Stdev),
        
        Preferences = list(
          "Price" = pref[1],
          "Beds" = pref[2],
          "Baths" = pref[3],
          "SqrFt" = pref[4],
          "LotSize" = pref[5],
          "Time" = pref[6]
        ),
        Realtor = realtorName,
        Clarity = clarity,
        Responsiveness = responsiveness,
        TimeCurrent = t,
        PlayLag = t + floor(rexp(1,1/4)) ## Here we want the floor to allow immediate play
      )
      buyers[[b@Name]] <- b
    }
    ## Remove the drawn houses from the pool
    listings_remain <-
      anti_join(house_data, buyer_draws, by = c("Address","Event"))
    
  }
  else{
    listings_remain <- house_data
  }

  return (list("BuyerList" = buyers, "HouseData" = listings_remain))
}



# ## Some idea of the distribution of attributes
#
# ggplot(listings, aes(x = Baths)) + geom_histogram(binwidth = 0.5)
# ggplot(listings, aes(x = Beds)) + geom_histogram(binwidth = 1)
# ggplot(listings, aes(x = Price)) + geom_histogram(binwidth = 5000)
# ggplot(listings, aes(x = SqrFt)) + geom_histogram(binwidth = 100)
# ggplot(listings, aes(x = LotSize)) + geom_histogram(binwidth = 1000)
#
# ggplot(listings, aes(x = Date)) + geom_histogram(binwidth = 100)
