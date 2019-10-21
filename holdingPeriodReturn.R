### Let's see if I can get holding-period return 
library(readxl)
library(rex)
library(stringr)
library(tidyr)
library(scales)
library(plyr)
library(ggplot2)
library(PerformanceAnalytics)
library(tidyverse)
library(dplyr)

read.csv(merged, paste0(getwd(), "/Data/merged_Data.csv"))


## Take Clean, Filter down to only those sold

sold <- clean %>%
  select(Address, Date, Price, Event) %>%
  filter(Event == "Sold") %>%
  mutate(Date = as.Date(Date, "%m/%d/%y")) %>%
  group_by(Address) %>%
  add_tally() %>%
  filter(n >=2) %>%
  arrange(Address, Date) %>%
  mutate(PurchasePrice = ifelse(Address == lag(Address), lag(Price, n = 1, default = NA), NA),
         PurchaseDate  = ifelse(Address == lag(Address), lag(Date, n = 1, default = NA), NA)) %>%
  filter(!is.na(PurchasePrice)) %>%
  mutate(HPR = (Price - PurchasePrice)/PurchasePrice,
         Period = as.numeric(Date - PurchaseDate)) %>%
  mutate(AHPR = HPR * 365 / Period,
         Year = substr(Date,1,4),
         Month = substr(Date,6,7)) %>%
  mutate(Year = as.factor(Year),
         Month = as.factor(Month))

minDate <- min(sold$Date)
maxDate <- max(sold$Date)
allDays <- as.data.frame(seq(from = minDate, to = maxDate, by = 1))
colnames(allDays) <- c("Date")


## Look up exand
sold_Year <- sold %>%
  arrange(Date, Year) %>%
  expand(key = Year, value = AHPR)