## Data description plots for final paper

library(dplyr)
library(ggplot2)
library(zoo)
library(imputeTS)
library(ggthemes)
library(roll)
library(scales)
library(MASS)
library(tidyverse)

theme_set(theme_economist())

dataToDraw1 <- read.csv("dataToDraw.csv")
dataToDraw1 <- filter(dataToDraw1, Price <= 500000)
## Be sure date is in the proper format
dataToDraw1$Date <- as.Date.factor(dataToDraw1$Date)
dataToDraw1$SqrFt <- as.numeric(dataToDraw1$SqrFt)

names(dataToDraw1)

minDate <- min(dataToDraw1$Date)
maxDate <- max(dataToDraw1$Date)


ggplot(dataToDraw1) +
  geom_density(aes(x = Date))

dataToDraw2 <- dataToDraw1 %>%
  dplyr::select(Address,Zip,Date,Event,Price,Beds,Baths,SqrFt,LotSize,Time,DaysOnMarket,RollTimeAv) %>%
  dplyr::filter(Date >= as.Date("2016-06-04")) %>%
  dplyr::mutate(isListing = ifelse(Event == "Listed for sale", 1,0)) %>%
  dplyr::mutate(isSale = ifelse(Event == "Sold", 1,0)) %>%
  dplyr::mutate(Year = substr(Date,1,4)) %>%
  dplyr::mutate(Month = substr(Date,6,7)) %>%
  dplyr::arrange(Date)

minDate <- min(dataToDraw2$Date)
maxDate <- max(dataToDraw2$Date)

numDays <- as.numeric(difftime(maxDate,minDate, units = c("days"))) + 1

## Denisty post June 4 2016
dataToDraw2 %>%
  nrow() / 5190
## 80.75% 

ggplot(dataToDraw2) +
  geom_density(aes(x = Date))

## Rolling Average Number of Sales and Listings
timeLine <- dataToDraw2 %>% 
  dplyr::select(Date,Year,Month,Price,DaysOnMarket,isListing,isSale) %>%
  dplyr::group_by(Date) %>%
  dplyr::summarise(ListingsToday = sum(isListing),
            SalesToday = sum(isSale),
            avPrice = mean(Price))
head(timeLine)

allDays <- as.data.frame(seq(from = minDate, to = maxDate, by = 1))
colnames(allDays) <- c("Date")
  
timeLineFull <- full_join(timeLine,allDays, by = c("Date")) %>%
  dplyr::arrange(Date) %>%
  dplyr::mutate(ListingsToday = ifelse(is.na(ListingsToday), 0, ListingsToday)) %>%
  dplyr::mutate(SalesToday = ifelse(is.na(SalesToday), 0, SalesToday)) %>%
  dplyr::mutate(RollAvWeeklyListings = roll_mean(as.matrix(ListingsToday),7)) %>%
  dplyr::mutate(RollAvWeeklySales = roll_mean(as.matrix(SalesToday),7)) %>%
  dplyr::mutate(RollSdWeeklySales = roll_sd(as.matrix(SalesToday),7)) %>%
  dplyr::mutate(RollSdWeeklyListings = roll_sd(as.matrix(ListingsToday),7)) %>%
  dplyr::filter(Date >= as.Date("2016-06-10"))
head(timeLineFull,10)

timeLineGather <- timeLineFull %>%
  gather(RollStat,Value,4:8)
head(timeLineGather)

ggplot(timeLineFull) +
  geom_smooth(aes(x=Date,y=avPrice))



###### Plot Average Price by Listing or Sale #####
dataToDraw2 %>%
  filter(Date <= min(dataToDraw2$Date) + 100) %>%
  group_by(Date, Event) %>%
  mutate(av_Price = mean(Price)) %>%
  ggplot() +
  #geom_point(aes(x=Date, y = av_Price,color = Event)) +
  geom_smooth(aes(x=Date, y = av_Price, color = Event)) +
  scale_y_continuous(name = "Average Price", labels = dollar_format(prefix = "$")) +
  theme(axis.title.x = element_text(face="bold",size=14),
        axis.title.y = element_text(face="bold",size=14))



##### #####

dataToDraw2 %>%
  group_by(Event) %>%
  ggplot() +
  geom_density((aes(x=Price, color = Event))) +
  scale_x_continuous(labels = dollar_format(prefix = "$"))
  

timeLineGatherLS <- timeLineFull %>%
  dplyr::select(Date,ListingsToday,SalesToday) %>%
  gather(Event, Value, 2:3)
head(timeLineGatherLS)

timeLineGatherLS_roll <- timeLineGatherLS %>%
  group_by(Event) %>%
  mutate(twoDaySum = roll_sum(as.matrix(Value),2),
         weekSum = roll_sum(as.matrix(Value),7),
         twoDayAverage = roll_mean(as.matrix(Value),2),
         weekAverage = roll_mean(as.matrix(Value),7))

## Number of Sales and Listings
filter(timeLineGatherLS,Date <= min(Date) + 100) %>%
  ggplot() +
  geom_smooth(aes(x=Date,y=Value,color = Event))

filter(timeLineGatherLS,Date <= min(Date) + 100) %>%
  ggplot() +
  geom_point(aes(x=Date,y=weekSum,color = Event)) +
  geom_smooth(aes(x=Date,y=weekSum,color = Event),method = 'gam', formula = 'y ~ s(x)')

timeLineGatherLS_roll %>%
  ggplot() +
  geom_smooth(aes(x=Date,y=weekAverage, color = Event))


### Test Distributions that Fit Days on Market (How long are sellers willing to wait) ####
weibullFit <- fitdistr(filter(dataToDraw2,DaysOnMarket >0)$DaysOnMarket, "weibull")
wFitParam <- as.list(weibullFit$estimate)
gammaFit <- fitdistr(filter(dataToDraw2,DaysOnMarket >0)$DaysOnMarket, "gamma")
gFitParam <- as.list(gammaFit$estimate)
dataToFit <- dataToDraw2 %>% 
  dplyr::select(Date,DaysOnMarket)
dataToFit$weibullDraws <- rweibull(nrow(dataToFit),wFitParam$shape,wFitParam$scale)
dataToFit$gammaDraws <- rgamma(nrow(dataToFit), gFitParam$shape,gFitParam$rate)
  
ggplot(dataToFit) +
  geom_density(aes(x=DaysOnMarket, color = "DaysOnMarket")) +
  geom_density(aes(x=weibullDraws, color = "WeibullDraws")) +
  geom_density(aes(x=gammaDraws, color = "GammaDraws"))



mean(dataToDraw2$DaysOnMarket)
sqrt(var(dataToDraw2$DaysOnMarket))


## Number of Listings
uniqueListings <- dataToDraw2 %>%
  filter(Date >= as.Date("2016-06-01")) %>%
  filter(Event == "Listed for sale") %>%
  arrange(Address,desc(Date)) %>%
  distinct(Address, Year, Price)
head(uniqueListings,5)

## Number of Sales
uniqueSales <- dataToDraw2 %>%
  filter(Event == "Sold") %>%
  arrange(Address,desc(Date)) %>%
  distinct(Address, Year, Price)
head(uniqueSales,5)

### end ### 


x <- anti_join(uniqueSales, uniqueListings, by = c("Address")) # Sales without a Listing
## Were probably listed before I took in the date.
## Merge in List Price and List Date!

y<- anti_join(uniqueListings,uniqueSales, by = c("Address")) # Listings without a Sale

unique_Pairs <- inner_join(uniqueSales, uniqueListings, by = c("Address")) %>%
  distinct(Address)
  
unique_Pairs %>%
  nrow()