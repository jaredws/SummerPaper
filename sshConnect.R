## To execute the entire program over SSH

session <- ssh_connect("jaredws@catan.eecis.udel.edu")
print(session)

## to run as a batch, run this in the terminal
## R CMD BATCH "sshExecutable.R"

## Libraries needed
# TODO libraries? maybe upload them? install them? 
# TODO will I need to install R on the remote machine?
# library(dplyr)
# library(FuzzyNumbers)

## Files needed:

# dataToDraw.csv
# seedList.csv
# requirementClass.R
# buyerClass.R
# realtorClass.R
# sellerClass.R
# generateBuyersAndSellers.R
# mainExecutable.R
# sshExecutable.R

scp_upload(session, "dataToDraw.csv")
scp_upload(session, "seedList.csv")
scp_upload(session, "requirementClass.R")
scp_upload(session, "buyerClass.R")
scp_upload(session, "realtorClass.R")
scp_upload(session, "sellerClass.R")
scp_upload(session, "generateBuyersAndSellers.R")
scp_upload(session, "mainExecutable.R")
scp_upload(session, "sshExecutable.R")

## Call R to the terminal
ssh_exec_wait(session, command = "R")
## Execute the sshExecutable to execute all the runs!
ssh_exec_wait(session, command = "sshExecutable.R")

## Download back the final results
scp_download(session, "RealizedData.RData")

### Generate 50 seeds to use in every Run and version of the program for comparison and reproduction
#seedList <- as.data.frame(floor(runif(50,1,9999)))
#colnames(seedList) <- c("Seed")
#write.csv(seedList, file = "seedList.csv")



