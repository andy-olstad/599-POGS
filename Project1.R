## Project 1 Code

## Question: How different are the household incomes (mean/median/etc) for 
## households that own/rent/etc in California

## Start with 1 year data, we move to 3 yr / 5 yr 
## Data used will be AMS 2012 1 Year California .csv

## Andy's Code Below






## Jasmine's Code Below





## Sarah's Code Below





# Tim's Code Below
library(dplyr)
??dplyr
setwd("C:/Documents and Settings/Tim Skalland/Desktop/ST 599 - Big Data/csv_hca")
hca2012 <- read.csv("ss12hca.csv", header=T, nrows=1000, stringsAsFactors = FALSE)
hca2012_df <- tbl_df(hca2012)
hca2012_df <- select(hca2012_df, TEN, HINCP)
hca2012_df

## Note for TEN variable: 
#1 .Owned with mortgage or loan (include home equity loans)
#2 .Owned free and clear
#3 .Rented
#4 .Occupied without payment of rent

hca2012_rm.na <- filter(hca2012_df, TEN != "NA") 
acs_tenure <- group_by(hca2012_rm.na, TEN)
summarize(acs_tenure, 
          avg_inc = mean(HINCP, na.rm = TRUE))














