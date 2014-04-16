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
# don't forget to check the working directory!
#read california csv file only look at first 10 rows
or_sm <- read.csv("ss12hca.csv", nrows = 10,
                  stringsAsFactors = FALSE)
str(or_sm)

# subset only the columns that we need
library(dplyr)
income_own <- select(or_sm, TEN, HINCP)

# 'tbl_df allows for a cleaner display of the data
income_owndf <- tbl_df(income_own)
head(income_owndf)

#find the mean income for each ownership group
#groub by TEN or house ownership group
income_ten <- group_by(income_owndf, TEN)
#finding the mean for each group 
summarize(income_ten, avg_income = mean(HINCP, na.rm= TRUE))
summarize(income_ten, med_income = median(HINCP, na.rm= TRUE))

# what kind of summaries do we need to find

### abreviations 
# Coding the ADJHSG variable to specifications above
ADJHSG_codes <- c("1053092" = "2010",
                  "1020890" = "2011",
                  "1000000" = "2012")
ca_3yr_df <- mutate(ca_3yr_df, Year = ADJHSG_codes[as.character(ADJHSG)])

# Coding the ST variable to specifications above
ST_codes <- c("1" = "AL",
              "2" = "AK",
              "4" = "AZ",
              "5" = "AR",
              "6" = "CA",
              "8" = "CO",
              "9" = "CT",
              "11" = "DC" ,
              "12" = "FL" ,
              "13" = "GA" ,
              "15" = "HI" ,
              "16" = "ID" ,
              "17" = "IL" ,
              "18" = "IN" ,
              "19" = "IA" ,
              "20" = "KS" ,
              "21" = "KY" ,
              "22" = "LA" ,
              "23" = "ME" ,
              "24" = "MD" ,
              "25" = "MA" ,
              "26" = "MI" ,
              "27" = "MN" ,
              "28" = "MS" ,
              "29" = "MO" ,
              "30" = "MT" ,
              "31" = "NE" ,
              "32" = "NV" ,
              "33" = "NH" ,
              "34" = "NJ" ,
              "35" = "NM" ,
              "36" = "NY" ,
              "37" = "NC" ,
              "38" = "ND" ,
              "39" = "OH" ,
              "40" = "OK" ,
              "41" = "OR" ,
              "42" = "PA" ,
              "44" = "RI" ,
              "45" = "SC" ,
              "46" = "SD" ,
              "47" = "TN" ,
              "48" = "TX" ,
              "49" = "UT" ,
              "50" = "VT" ,
              "51" = "VA" ,
              "53" = "WA" ,
              "54" = "WV" ,
              "55" = "WI" ,
              "56" = "WY" , 
              "72" = "PR" ,)
ca_3yr_df <- mutate(ca_3yr_df, States = ST_codes[as.character(ST)])





## Sarah's Code Below
setwd("~/Dropbox/Work/Classes/Spring 2014/Big data")
library(dplyr)

#variables: TEN, tenure of home; HINCP, household income in past 12 months

cal_households2012 <- read.csv("ss12hca.csv", header = T, stringsAsFactors = FALSE)
cal_households2012_df <- tbl_df(cal_households2012)
tenure_by_income2012_df <- select(cal_households2012_df, TEN, HINCP)
head(tenure_by_income2012_df)

#1 - Owned with mortgage or loan (include home equity loans)
#2 - Owned free and clear
#3 - Rented
#4 - Occupied without payment of rent 

tenure_by_income2012_mean <- summarise(group_by(tenure_by_income2012_df, TEN), mean(HINCP, na.rm = TRUE))
tenure_by_income2012_median <- summarise(group_by(tenure_by_income2012_df, TEN), median(HINCP, na.rm = TRUE))

tenure_by_income2012_mean
tenure_by_income2012_median

rm(cal_households2012, cal_households2012_df, tenure_by_income2012, tenure_by_income2012_df, tenure_by_income2012_mean, tenure_by_income2012_median)







# Tim's Code Below
library(dplyr)
??dplyr

# Reading in the data, putting it into dplyr table, and selecting the variables of interest
setwd("C:/Documents and Settings/Tim Skalland/Desktop/ST 599 - Big Data")

dwnld <- function(state_name){
  download.file(paste("http://www2.census.gov/acs2012_3yr/pums/csv_h",state_name,".zip", sep=""),
              paste(destfile = "Data/csv_h",state_name,".zip", sep=""))
}

#states <- c("ak", "al", "ar", "az", "ca", "co", "ct", "dc", "de", "fl", "ga", "hi", "ia", "id", "il", "in",
#             "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt", "nc", "nd", "ne", "nh", "nj",
#              "nm", "nv", "ny", "oh", "ok", "or", "pa", "pr", "ri", "sc", "sd", "tn", "tx", "")

us_data <- read.csv(unz("Data/csv_hus.zip", "ss12husa.csv"), nrows = 10,
                       stringsAsFactors = FALSE)
tbl_df(us_data)

proj1.income.own <- function(state_name){
  state_data <- read.csv(unz(paste("Data/csv_h",state_name,".zip", sep=""), paste("ss12h",state_name,".csv", sep="")), nrows = 10,
                  stringsAsFactors = FALSE)
  invisible(state_data)
}

proj1.data <- proj1("ca")


hca2012 <- proj1.data
hca2012_df <- tbl_df(hca2012)

# Finding out what are the column numbers for our variables of interest
which(names(hca2012_df) %in% c("TEN", "HINCP"))

hca2012_df <- select(hca2012_df, TEN, HINCP)
hca2012_df

## Note for TEN variable: 
#1 .Owned with mortgage or loan (include home equity loans)
#2 .Owned free and clear
#3 .Rented
#4 .Occupied without payment of rent

# Coding the TEN variable to specifications above
TEN_codes <- c("1" = "Owned with Mortgage or Loan",
               "2" = "Owned Free and Clear",
               "3" = "Rented",
               "4" = "Occupied without Payment of Rent")
hca2012_df <- mutate(hca2012_df, Housing_Payment = TEN_codes[as.character(TEN)])

# Removing the missing data from the TEN variable
hca2012_rm.na <- filter(hca2012_df, TEN != "NA") 

# Grouping by the TEN variable and calculating the average household income by group
acs_tenure <- group_by(hca2012_rm.na, TEN)
summarize(acs_tenure, 
          avg_inc = mean(HINCP, na.rm = TRUE))














