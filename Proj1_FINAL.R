## Loading the data from each of our computers
library(dplyr)

## ANDY LOAD
## SET YOUR WORKING DIRECTORY MANUALLY

## JASMINE LOAD
setwd("~/Grad School/ST 599/Project_1_Data/csv")

## SARAH LOAD
setwd("~/Dropbox/Work/Classes/Spring 2014/Big data")

## TIM LOAD
setwd("C:/Documents and Settings/Tim Skalland/Desktop/ST 599 - Big Data/Project 1/Data")

ushouseholds <- read.csv("cut_ss12hus.csv", header = T, stringsAsFactors = FALSE)
ushouseholds_df <- tbl_df(ushouseholds)

TEN_codes <- c("1" = "Owned with Mortgage or Loan",
               "2" = "Owned Free and Clear",
               "3" = "Rented",
               "4" = "Occupied without Payment of Rent")
ushouseholds_df <- mutate(ushouseholds_df, Housing_Payment = TEN_codes[as.character(TEN)])

# Removing the missing data from the TEN variable
ushouseholds_df <- filter(hca2012_df, TEN != "NA") 

# Grouping by the TEN variable and calculating the average household income by group
acs_state_tenure <- group_by(ushouseholds_df, ST, TEN)
summarize(acs_state_tenure, 
          avg_inc = mean(HINCP, na.rm = TRUE)
          med_inc = median(HINCP, na.rm=TRUE)
          n <- n())



