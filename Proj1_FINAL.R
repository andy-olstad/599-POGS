## Loading the data from each of our computers
library(dplyr)

## ANDY LOAD
setwd("C:/Users/costco682/Documents/GitHub/599-POGS")

## JASMINE LOAD
setwd("~/Grad School/ST 599/Project_1_Data/csv")

## SARAH LOAD
setwd("~/Dropbox/Work/Classes/Spring 2014/Big data/599-POGS")

## TIM LOAD
setwd("C:/Documents and Settings/Tim Skalland/Desktop/ST 599 - Big Data/Project 1/Data")



## BEGIN DATA ANALYSIS
ushouseholds <- read.csv("cut_ss12hus.csv", header = T, stringsAsFactors=FALSE)

# We have to first make a data matrix in order to get things into numeric form (mainly for HINCP)
ushouseholds <- data.matrix(ushouseholds)

# Then we revert back into a data frame and then into the nice tbl_df for of dplyr
ushouseholds <- data.frame(ushouseholds)
ushouseholds_df <- tbl_df(ushouseholds)
rm(ushouseholds)

# Now we should make ST, TEN and ADJHSG back as factor variables
ushouseholds_df$ST <- with(ushouseholds_df, as.factor(ST))
ushouseholds_df$TEN <- with(ushouseholds_df, as.factor(TEN))
ushouseholds_df$ADJHSG <- with(ushouseholds_df, as.factor(ADJHSG))
str(ushouseholds_df)

# Removing the missing data from the TEN variable (Note: the data.matrix() argument made missing values as NA)
ushouseholds_df <- filter(ushouseholds_df, TEN != "NA") 
max(ushouseholds_df$HINCP)

# Grouping by the ST, TEN variable and calculating the average household income by group
acs_state_tenure <- group_by(ushouseholds_df, ST, TEN)
state_ten <- summarise(acs_state_tenure, 
                            Average_Income = mean(HINCP, na.rm = TRUE),
                            Median_Income = median(HINCP, na.rm = TRUE),
                            Count = n())

# Recoding Tenure Codes and State Codes
TEN_codes <- c("1" = "Mortgage or Loan",
               "2" = "Free and Clear",
               "3" = "Rented",
               "4" = "NoPayment of Rent")
state_ten <- mutate(state_ten, Housing_Payment = TEN_codes[as.character(TEN)])


# Coding the ST variable to specifications above (Note: Jasmine's original coding is ok now since 01 became 1)
ST_codes <- c("1" = "AL",
              "2" = "AK",
              "4" = "AZ",
              "5" = "AR",
              "6" = "CA",
              "8" = "CO",
              "9" = "CT",
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

state_ten

# A Possible graphic to include, although I can't seem to get the State Abrev. on the X-axis
library(ggplot2)
Housing_Ownership <- with(state_ten, as.character(Housing_Payment))
state_ten$ST <- with(state_ten, as.numeric(ST))
qplot(ST, Average_Income, data=state_ten, colour=Housing_Payment_Type) + geom_line() 

