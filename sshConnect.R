## To execute the entire program over SSH

library(ssh)

session <- ssh_connect("jaredws@catan.eecis.udel.edu")
print(session)

## to run as a batch, run this in the terminal
## R CMD BATCH "sshExecutable.R"

## hoek.eecis.udel.edu and cata.eecis.udel.edu have the same directory of files ..
## unsure of their computing difference

## Libraries needed
## Had to install libraries to a user file, I beleive they will remain there within my account
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


## create a tmux session 'remote' execute the script
## in absence of my login to the server
# ssh_exec_wait(session, command = c(
#  "tmux a -t sharpe"
# )
# )

ssh_exec_wait(session, command = c(
  "tmux ls"
))

## Remove old files
ssh_exec_wait(session, command = c(
  "rm -Rf requirementClass.R",
  "rm -Rf dataToDraw.csv",
  "rm -Rf seedList.csv",
  "rm -Rf saleFrequency.csv",
  "rm -Rf listingFrequency.csv",
  "rm -Rf buyerClass.R",
  "rm -Rf realtorClass.R",
  "rm -Rf sellerClass",
  "rm -Rf generateBuyersAndSellers.R",
  "rm -Rf mainExecutable.R",
  "rm -Rf sshExecutable.R"
  ))

## Upload new ones
scp_upload(session, "dataToDraw.csv")
scp_upload(session, "seedList.csv")
scp_upload(session, "saleFrequency.csv")
scp_upload(session, "listingFrequency.csv")
scp_upload(session, "requirementClass.R")
scp_upload(session, "buyerClass.R")
scp_upload(session, "realtorClass.R")
scp_upload(session, "sellerClass.R")
scp_upload(session, "generateBuyersAndSellers.R")
scp_upload(session, "mainExecutable.R")
scp_upload(session, "sshExecutable.R")

## 


## Call R to the terminal
## Execute the sshExecutable to execute all the runs!
## Still something wrong with the R command

## Download back the final results
#scp_download(session, "/usa/jaredws/realizedData.RData")
## consier renaming the file to it's executed time and description

ssh_disconnect(session)
### Generate 50 seeds to use in every Run and version of the program for comparison and reproduction
#seedList <- as.data.frame(floor(runif(50,1,9999)))
#colnames(seedList) <- c("Seed")
#write.csv(seedList, file = "seedList.csv")



