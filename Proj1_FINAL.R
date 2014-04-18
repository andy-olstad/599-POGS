## Loading the data from each of our computers
library(dplyr)

## ANDY LOAD
## SET YOUR WORKING DIRECTORY MANUALLY

## JASMINE LOAD
setwd("~/Grad School/ST 599/Project_1_Data/csv")

## SARAH LOAD
setwd("~/Dropbox/Work/Classes/Spring 2014/Big data")
setwd("~/Dropbox/Work/Classes/Spring 2014/Big data/599-POGS")

## TIM LOAD
setwd("C:/Documents and Settings/Tim Skalland/Desktop/ST 599 - Big Data/Project 1/Data")




## BEGIN DATA ANALYSIS
ushouseholds <- read.csv("cut_ss12hus.csv", header = T)
ushouseholds_df <- tbl_df(ushouseholds)

# We need to code HINCP as a numeric
ushouseholds_df$HINCP <- with(ushouseholds_df, as.numeric(HINCP))

# Removing the missing data from the TEN variable
ushouseholds_df <- filter(ushouseholds_df, TEN != "") 

# Grouping by the ST, TEN variable and calculating the average household income by group
acs_state_tenure <- group_by(ushouseholds_df, ST, TEN)
state_ten <- summarise(acs_state_tenure, 
                            Average_Income = mean(HINCP, na.rm = TRUE),
                            Median_Income = median(HINCP, na.rm = TRUE),
                            Count = n())

tail(acs_state_tenure)
# Recoding Tenure Codes and State Codes
TEN_codes <- c("1" = "Owned with Mortgage or Loan",
               "2" = "Owned Free and Clear",
               "3" = "Rented",
               "4" = "Occupied without Payment of Rent")
state_ten <- mutate(state_ten, Housing_Payment = TEN_codes[as.character(TEN)])


# Coding the ST variable to specifications above
ST_codes <- c("01" = "AL",
              "02" = "AK",
              "04" = "AZ",
              "05" = "AR",
              "06" = "CA",
              "08" = "CO",
              "09" = "CT",
              "10" = "DE",
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
              "72" = "PR")

state_ten <- mutate(state_ten, State = ST_codes[as.character(ST)])
state_ten[1:100,]



