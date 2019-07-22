buyerSatisfaction <- select(house_sales_noRealtor, Buyer, BuyerSatisfaction)
buyerSatisfaction$Group = "NoRealtor"

bs2 <- select(house_sales_perfectInfo, Buyer, BuyerSatisfaction)
bs2$Group = "PerfectInfo"

buyerSatisfaction <- rbind(buyerSatisfaction, bs2)

wilcox.test(formula = BuyerSatisfaction ~ Group, data = buyerSatisfaction)

## Technically, I should compare each buyer's individual satisfaction
## So long as the seeds are the same for both runs, each buyer is identical to his named twin
## But this will require many runs to get a large enough sample for all buyer's to have exited the market
## But it is possible! 