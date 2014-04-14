## Project 1 Code

## Question: How different are the household incomes (mean/median/etc) for 
## households that own/rent/etc in California

## Start with 1 year data, we move to 3 yr / 5 yr 
## Data used will be AMS 2012 1 Year California .csv

## Andy's Code Below

#attach dplyr
library(dplyr)

#read in the tables
hca<-read.csv("c:\\Users\\costco682\\Desktop\\ss12hca.csv")
pca<-read.csv("c:\\Users\\costco682\\Desktop\\ss12pca.csv")

#poking around
head(hca)
dim(hca)

#make it dplyr-able
hca_df<- tbl_df(hca)

#poking around again
dim(hca_df)
names(hca_df)

#find the average income for all respondents
summarise(hca_df, avg_income = mean(HINCP, na.rm = TRUE))
#  avg_income
#1   83191.91

#find average income for subsets

#TEN==1 hold mortgages
#filter(hca_df, TEN == 1)
summarise(filter(hca_df, TEN == 1), avg_income_mortgagees = mean(HINCP, na.rm = TRUE))
#  avg_income_mortgagees
#1   114452.3

#TEN==2 own outright
summarise(filter(hca_df, TEN == 2), avg_income_clear_owners = mean(HINCP, na.rm = TRUE))
#   avg_income_clear_owners
#1                75645.99


#TEN==3 rent
summarise(filter(hca_df, TEN == 3), avg_income_renters = mean(HINCP, na.rm = TRUE))
# avg_income_renters
#1           53588.75

#TEN==4 occupy without rent
summarise(filter(hca_df, TEN == 4), avg_income_squatters = mean(HINCP, na.rm = TRUE))
# avg_income_squatters
#1             45386.33



#cleanup
rm(hca,hca_df,pca)





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














