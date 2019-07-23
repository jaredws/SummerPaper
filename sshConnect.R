### SSH script for running the main function
### executibles on the hoec server

library(ssh)
library(dplyr)
library(FuzzyNumbers)
library(tidyverse)


## Files needed:

# dataToDraw.csv
# seedList.csv
# requirementClass.R
# buyerClass.R
# realtorClass.R
# sellerClass.R
# generateBuyersAndSellers.R
# mainExecutable.R


source("requirementClass.R")
source("buyerClass.R")
source("realtorClass.R")
source("sellerClass.R")
source("generateBuyersAndSellers.R")
source("mainExecutable.R")


session <- ssh_connect("jaredws@hoek.eecis.udel.edu")
#print(session)

### Generate 50 seeds to use in every Run and version of the program for comparison and reproduction
#seedList <- as.data.frame(floor(runif(50,1,9999)))
#colnames(seedList) <- c("Seed")

#write.csv(seedList, file = "seedList.csv")

seedList <- read.csv("seedList.csv", col.names = c("Run", "Seed"))

runNames <- c("NoRealtor", "PerfectInfo")
randomness <- list("NoRealtor" = TRUE, "PerfectInfo" = FALSE)
lagPlay <- c(TRUE, FALSE)
iterations <- 5

realizedData <- list()

for (lag in lagPlay) {
  for (version in runNames) {
    for (run in 1:2) {
      ## Name the index in the list as the type of run
      ## Example:
      ## NoRealtor_TRUE_1
      ## Would be the first run of the NoRealtor version, with lagPlay active
      
      executing <- paste0(version, "_", lag, "_", run)
      print(paste0("Now running: ",executing))
      
      ## This is going to get large ... very fast
      ## I may want to write to disk more often, but we will see...
      realizedData[[executing]] <-
        main(iterations,
             seedList[run, "Seed"],
             randomness[[version]], ## Randomness depends on the version.
             lag,
             version)
    }
    
  }
}



## UPload all the files first, to account for any changes
## Then run each main, taking the output data
