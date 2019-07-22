## Generate Buyers and Sellers


library(dplyr)
library(FuzzyNumbers)
library(tidyverse)
library(ggplot2)
library(stats)

## Time is created from the order of the dates of postings in 


generateBuyersAndSellers <- function(t, realtors, house_data,
                                     clarity, responsiveness) {
  listings <- filter(house_data, Event == "Listed for sale") %>%
    select(Address, Price, Beds, Baths, SqrFt, LotSize, Date, Time) %>%
    arrange(Date)
  
  house_draws <- dplyr::sample_n(
    tbl = listings,
    size = abs(floor(rnorm(1, 8, 3))),
    replace = FALSE,
    weight = 1000 / (as.numeric(Date))
  )
  ## Remove the drawn houses from the pool
  listings_remain <-
    anti_join(listings, house_draws, by = c("Address"))
  
  ## Draw again with replacement and similiar weighting for the buyer's
  buyer_draws <- dplyr::sample_n(
    tbl = listings,
    size = abs(floor(rnorm(1, 10, 4))),
    replace = TRUE,
    weight = 1000 / (as.numeric(Date))
  )
  
  sellers <- list()
  buyers <- list()
  
  ## TODO still need the input for Fuzziness
  fuzzy_param <- list("S1Mean" = 0.95,
                      "S1Stdev" = 0.02,
                      "S2Mean" = 1.05,
                      "S2Stdev" = 0.02)
  
  ## Parameters for Normal distributions of Preferences
  price_param_s <- list("Mean" = 0.6, "Stdev" = 0.2)
  time_param_s <- list("Mean" = 0.4, "Stdev" = 0.2)
  minQoS_param_s <- list("Mean" = 0.92, "Stdev"= 0.3)
  
  price_param_b <- list("Mean" = 0.3, "Stdev" = 0.05)
  beds_param_b <- list("Mean" = 0.15, "Stdev" = 0.05)
  baths_param_b <- list("Mean" = 0.15, "Stdev" = 0.05)
  sqrft_param_b <- list("Mean" = 0.05, "Stdev" = 0.05)
  lotsize_param_b <- list("Mean" = 0.10, "Stdev" = 0.05)
  time_param_b <- list("Mean" =  0.25, "Stdev" = 0.05)
  minQos_param_b <- minQoS_param_s ## For simplicity
  
  for (r in house_draws) {
    entry <- house_draws[r,]
    pref <- c(rnorm(1, mean = price_param_s$Mean, sd = price_param_s$Stdev),
              rnorm(1,mean = time_param_s$Mean, sd = time_param_s$Stdev))
    pref <- pref/sqrt(sum(pref^2)) ## normalie to 1
    
    fuzzy <- c(
      rnorm(1,fuzzy_param$S1Mean,fuzzy_param$S1Stdev),
      rnorm(1,fuzzy_param$S2Mean,fuzzy_param$S2Stdev)
    )
    
    s <- Seller(
      Name = as.character(Sys.time()),
      Price = Requirement(entry$price*fuzzy[1], entry$price, entry$price, entry$price*fuzzy[2]),
      Time = Requirement(min(entry$Time,t), max(entry$Time,t), entry$RollTimeAv + min(entry$Time,t), entry$RollTimeAv + max(entry$Time,t)),
      House = Bev,
      Preferences = list("Price" = pref[1], "Time" = pref[2]),
      ## The Weights the seller puts on Price and Time,
      Clarity = clarity,
      ## The Level of clarity with the Realtor on Price and Time Fuzziness
      MinQoS = rnorm(1,minQoS_param_s$Mean,minQoS_param_s$Stdev),
      ## The Threshold an offer must make to be accepted
      Realtor = "Torterella",
      ## The name of the Realtor the Seller is working with
      TimeCurrent = t
    )
    
    house <- as.data.frame(
      list(
        "Address" = entry$Address,
        "Price" = entry$Price,
        "Beds" = entry$Beds,
        "Baths" = entry$Baths,
        "SqrFt" = entry$SqrFt,
        "LotSize" = entry$LotSize,
        "Time" = mean(s@Time@core1,s@Time@core2)
      )
    )
  ## Now the buyers
    
    for (r in buyer_draws){
      entry <- buyer_draws[r,]
      
      pref = c(rnorm(1,price_param_b$Mean,price_param_b$Stdev),
               rnorm(1,beds_param_b$Mean,beds_param_b$Stdev),
               rnorm(1,baths_param_b$Mean,baths_param_b$Stdev),
               rnorm(1,sqrft_param_b$Mean,sqrft_param_b$Stdev),
               rnorm(1,lotsize_param_b$Mean,lotsize_param_b$Stdev),
               rnorm(1,time_param_b$Mean,time_param_b$Stdev))
      pref <- pref/sqrt(sum(pref^2)) ## normalie to 1
      
      fuzzy <- c(
        rnorm(1,fuzzy_param$S1Mean,fuzzy_param$S1Stdev),
        rnorm(1,fuzzy_param$S2Mean,fuzzy_param$S2Stdev))
      
      b <- Buyer(
        Name = as.character(Sys.time()),
        Price = Requirement(entry$price*fuzzy[1], entry$price, entry$price, entry$price*fuzzy[2]),
        Beds = Requirement(entry$Beds*fuzzy[1],entry$Beds,entry$Beds,entry$Beds*fuzzy_param[2]),
        Baths = Requirement(entry$Baths*fuzzy[1],entry$Baths,entry$Baths,entry$Baths*fuzzy_param[2]),
        SqrFt = Requirement(entry$SqrFt*fuzzy[1],entry$SqrFt,entry$SqrFt,entry$SqrFt*fuzzy_param[2]),
        LotSize = Requirement(entry$LotSize*fuzzy[1],entry$LotSize,entry$LotSize,entry$LotSize*fuzzy_param[2]),
        Time = Requirement(min(entry$Time,t), max(entry$Time,t), entry$RollTimeAv + min(entry$Time,t), entry$RollTimeAv + max(entry$Time,t)),
        MinQoS = rnorm(1,minQos_param_b$Mean,minQos_param_b$Stdev),
        
        Preferences = list(
          "Price" = pref[1],
          "Beds" = pref[2],
          "Baths" = pref[3],
          "SqrFt" = pref[4],
          "LotSize" = pref[5],
          "Time" = pref[6]
        ),
        Realtor = "Torterella",
        Clarity = clarity,
        Responsiveness = responsiveness,
        TimeCurrent = t
      )
    }

  }
  
  return ( list("SellerList" = sellers ,"BuyerList" = buyers, "HouseData" = listings_remain))
}



## Some idea of the distribution of attributes

ggplot(listings, aes(x = Baths)) + geom_histogram(binwidth = 0.5)
ggplot(listings, aes(x = Beds)) + geom_histogram(binwidth = 1)
ggplot(listings, aes(x = Price)) + geom_histogram(binwidth = 5000)
ggplot(listings, aes(x = SqrFt)) + geom_histogram(binwidth = 100)
ggplot(listings, aes(x = LotSize)) + geom_histogram(binwidth = 1000)

ggplot(listings, aes(x = Date)) + geom_histogram(binwidth = 100)
