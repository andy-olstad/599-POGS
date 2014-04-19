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

tail(state_ten)

# Notice the last row is unusual because when we merged the data sets it added
# the headers again (3 times)
# Lets remove that last now
state_ten <- state_ten[-nrow(state_ten),]

# plot of mean income of all states by housing payment
library(ggplot2)
# unpolished
qplot(State, Average_Income, data = state_ten, color = Housing_Payment)
qplot(Housing_Payment, Average_Income, data = state_ten, color = State)

#attempt to polish and add title
qplot(State, Average_Income, data = state_ten, color = Housing_Payment) + ggtitle("Mean Household Income based on Housing Ownership by State")
qplot(Housing_Payment, Average_Income, data = state_ten, color = State) + ggtitle("Mean Household Income based on Housing Ownership by State")

#add line
# need to add group=Housing_Payment because the previous "group by" function separated all states
qplot(State, Average_Income, data = state_ten, color = Housing_Payment, group = Housing_Payment) +geom_line() + ggtitle("Mean Household Income based on Housing Ownership by State")
qplot(Housing_Payment, Average_Income, data = state_ten, color = State, group = State) +geom_line() + ggtitle("Mean Household Income based on Housing Ownership by State")

# reorder?
qplot(reorder(State, Average_Income, order = T), Average_Income, data = state_ten, color = Housing_Payment, group = Housing_Payment) +geom_line() + ggtitle("Mean Household Income based on Housing Ownership by State")
# ordered by overall state average income
# would like to see how things behave when you force order only one line (ie top)

##plotting on a map:
library(maps)
data(stateMapEnv)
data(state.fips)

#map('state',col=rainbow(6),fill=TRUE) #note this colors Michigan unevenly...

#list of colors
colors<-c("dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4")
#for debugging easier to use colors<-c("red","yellow","green","blue")
state_ten_1<-filter(state_ten,TEN ==1)
state_ten_1$colorBuckets <- as.numeric(cut(state_ten_1$Average_Income, c(10000, 11672, 12440, 14049, 20000)))
 leg.txt <- c("first quartile", "second quartile", "third quartile", "fourth quartile")

#assign colors to states
st.fips <- state.fips$fips[match(map("state", plot=FALSE)$names,
    state.fips$polyname)]
st.abb <- state.fips$abb[match(map("state", plot=FALSE)$names,
    state.fips$polyname)]
colorsmatched <- state_ten_1$colorBuckets [match(st.abb, state_ten_1$State)]

map("state", col = colors[colorsmatched], fill = TRUE)


