### SSH script for running the main function
### executibles on the catan.eecis.udel.edu

#install.packages("dplyr")
#install.packages("FuzzyNumbers")

library(dplyr)
library(FuzzyNumbers)
#library(tidyverse)

source("requirementClass.R")
source("buyerClass.R")
source("realtorClass.R")
source("sellerClass.R")
source("generateBuyersAndSellers.R")
source("mainExecutable.R")

seedList <- read.csv("seedList.csv", col.names = c("Run", "Seed"))

runNames <- c("MaxProfit")
randomness <- list("NoRealtor" = TRUE, "PerfectInfo" = FALSE, "MaxProfit" = FALSE)
lagPlay <- c(TRUE,FALSE)
iterations <- 50

realizedData <- list()

RUNS <- seq(1,20)

for (lag in lagPlay) {
  for (version in runNames) {
    for (run in RUNS) {
      run <- as.numeric(run)
      ## Name the index in the list as the type of run
      ## Example:
      ## NoRealtor_TRUE_1
      ## Would be the first run of the NoRealtor version, with lagPlay active
      
      executing <- paste0(version, "_", lag, "_", run)
      print(paste0("Now running: ",executing))
      
      ## This is going to get large ... very fast
      ## I may want to write to disk more often, but we will see...
      realizedData <-
        main(ITERATIONS = iterations,
             SEED = seedList[run, "Seed"],
             RANDOM = randomness[[version]], ## Randomness depends on the version.
             LAGPLAY = lag,
             RUN_NAME = version)
      
      fileName <- paste0(executing,"_realizedData.RData")
      
      save(realizedData, file = eval(fileName))
    }
  }
}

## Note: each execution of the system returns a list of the form:
# RETURN <- list(
#   "BuyerList" <- BuyerList_noRealtor,
#   "SellerList" <- SellerList_noRealtor,
#   "RealtorList" <- RealtorList_noRealtor,
#   "Iteration_stats" <- iteration_stats_noRealtor,
#   "House_sale_stats" <- house_sales_noRealtor,
#   "BuyerInactive" <- BuyerInactive_noRealtor,
#   "SellerInactive" <- SellerInactive_noRealtor
# )

#load(file = "realizedData.RData")
