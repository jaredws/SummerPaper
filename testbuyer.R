## Testing the Buyer Class

library(dplyr)
library(FuzzyNumbers)
library(tidyverse)

source("requirementClass.R")
source("buyerClass.R")
source("realtorClass.R")
source("sellerClass.R")

John <- Buyer(
  Name = "John",
  Price = Requirement(0, 220000, 250000, 260000),
  Beds = Requirement(2, 3, 3, 4),
  Baths = Requirement(1, 1, 2, 3),
  SqrFt = Requirement(1000, 1500, 2000, 2500),
  LotSize = Requirement(10000, 15000, 20000, 22000),
  Time = Requirement(0, 10, 60, 75),
  MinQoS = 0.9,
  Preferences = list(
    "Price" = 0.2,
    "Beds" = 0.25,
    "Baths" = 0.25,
    "SqrFt" = 0.05,
    "LotSize" = 0.05,
    "Time" = 0.2
  ),
  Realtor = "Torterella",
  Clarity = 2,
  Responsiveness = 1,
  TimeCurrent = 1
)

Torterella <- Realtor(
  Name = "Torterella",
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

Bev <- as.data.frame(
  list(
    "Address" = "410 Beverly Rd",
    "Price" = 245000,
    "Beds" = 3,
    "Baths" = 1,
    "SqrFt" = 1075,
    "LotSize" = 43650 * 0.42,
    "Time" = 10
  )
)

Jessie <- Seller(
  Name = "Jessie",
  Price = Requirement(0, 240000, 250000, 265000),
  Time = Requirement(0, 10, 90, 105),
  House = Bev,
  Preferences = list("Price" = 0.65, "Time" = 0.35),
  ## The Weights the seller puts on Price and Time,
  Clarity = 2,
  ## The Level of clarity with the Realtor on Price and Time Fuzziness
  MinQoS = 0.9,
  ## The Threshold an offer must make to be accepted
  Realtor = "Torterella",
  ## The name of the Realtor the Seller is working with
  TimeCurrent = 1
)

inform_B <- informRealtor(John)
inform_B
Torterella <- informRealtorFromBuyer(Torterella, John, inform_B)
tb <- Torterella@Buyers

inform_S <- informRealtor(Jessie)
inform_S
Torterella <- informRealtorFromSeller(Torterella, Jessie, inform_S)
Torterella@Sellers

Torterella <- matchMake(Torterella)
Torterella@Buyers$Matches

John <- informBuyer(John, Bev)
John@HousesKnown

John <- makeOffer(John)
John@CurrentOffer

John <- giveFeedback(John)
John@Feedback

Torterella <- getFeedback(Torterella,John)
Torterella@NewOffers

Jessie <- informSeller(Jessie, filter(
  Torterella@NewOffers,
  Address == Jessie@House$Address)
)
Jessie@CurrentOffers

Jessie <- acceptOffer(Jessie)
Jessie@AcceptedOffer
