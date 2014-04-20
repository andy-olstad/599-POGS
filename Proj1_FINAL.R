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

state_ten[1:100,]
state_ten[101:200,]
state_ten[201:204,]

TEN1 <- filter(state_ten, TEN==1)
TEN2 <- filter(state_ten, TEN==2)
TEN3 <- filter(state_ten, TEN==3)
TEN4 <- filter(state_ten, TEN==4)

arrange(TEN1, desc(Average_Income))
arrange(TEN2, desc(Average_Income))
arrange(TEN3, desc(Average_Income))
arrange(TEN4, desc(Average_Income))



# A Possible graphic to include, although I can't seem to get the State Abrev. on the X-axis
library(ggplot2)
Housing_Ownership <- with(state_ten, as.character(Housing_Payment))
state_ten$ST <- with(state_ten, as.numeric(ST))
qplot(ST, Average_Income, data=state_ten, colour=Housing_Payment_Type) + geom_line() 

# ordered by overall state average income
qplot(reorder(State, Average_Income, order = T), Average_Income, data = state_ten, color = Housing_Payment, group = Housing_Payment) + geom_line() + 
  ggtitle("Mean Household Income based on Housing Ownership by State") +
  xlab("State") + ylab("Average income")

##plotting on a map:
library(maps)
data(stateMapEnv)
data(state.fips)

#map('state',col=rainbow(6),fill=TRUE) #note this colors Michigan unevenly...

#list of colors
#colors<-c("dodgerblue1","dodgerblue2","dodgerblue3","dodgerblue4") # hard to distinguish
#for debugging easier to use colors<-c("red","yellow","green","blue")
colors <- c("cadetblue1","darkturquoise","dodgerblue","blue2")

#separately for each of the tenure categories:
state_ten_1<-filter(state_ten,TEN ==1)
state_ten_1$colorBuckets <- as.numeric(cut(state_ten_1$Average_Income, quantile(state_ten_1[,3],c(0,.25,.5,.75,1))-1))

state_ten_2<-filter(state_ten,TEN ==2)
state_ten_2$colorBuckets <- as.numeric(cut(state_ten_2$Average_Income, quantile(state_ten_2[,3],c(0,.25,.5,.75,1))-1))

state_ten_3<-filter(state_ten,TEN ==3)
state_ten_3$colorBuckets <- as.numeric(cut(state_ten_3$Average_Income, quantile(state_ten_3[,3],c(0,.25,.5,.75,1))-1))

state_ten_4<-filter(state_ten,TEN ==4)
state_ten_4$colorBuckets <- as.numeric(cut(state_ten_4$Average_Income, quantile(state_ten_4[,3],c(0,.25,.5,.75,1))-1))
 leg.txt <- c("first quartile", "second quartile","third quartile", "fourth quartile")


#assign colors to states
st.fips <- state.fips$fips[match(map("state", plot=FALSE)$names,
    state.fips$polyname)]
st.abb <- state.fips$abb[match(map("state", plot=FALSE)$names,
    state.fips$polyname)]
colorsmatched1 <- state_ten_1$colorBuckets [match(st.abb, state_ten_1$State)]
colorsmatched2 <- state_ten_2$colorBuckets [match(st.abb, state_ten_2$State)]
colorsmatched3 <- state_ten_3$colorBuckets [match(st.abb, state_ten_3$State)]
colorsmatched4 <- state_ten_4$colorBuckets [match(st.abb, state_ten_4$State)]

map("state", col = colors[colorsmatched1], fill = TRUE)
title("Mean Income for Mortgage or Loan")
legend("bottomleft", leg.txt, fill = colors, cex=0.54)

map("state", col = colors[colorsmatched2], fill = TRUE)
title("Mean Income for Free and Clear")
legend("bottomleft", leg.txt, fill = colors, cex=0.54)

map("state", col = colors[colorsmatched3], fill = TRUE)
title("Mean Income for Rented")
legend("bottomleft", leg.txt, fill = colors, cex=0.54)

map("state", col = colors[colorsmatched4], fill = TRUE)
title("Mean Income for No Payment of Rent")
legend("bottomleft", leg.txt, fill = colors, cex=0.54)

##Now trying a grayscale with more detail:

#Create the scale:
grayvector<-rep(NA,100)
for (i in 1:100){
grayvector[i]<-paste("gray",i,sep="")
}

graycuts1<-rep(NA,100)
range<-max(state_ten_1$Average_Income)-min(state_ten_1$Average_Income)
for (i in 1:100){
graycuts1[i]<-min(state_ten_1$Average_Income)+i*range/100
}
graycuts1[1]<-0

state_ten_1$grayBuckets <- as.numeric(cut(state_ten_1$Average_Income, graycuts1))
graymatch1<-state_ten_1$grayBuckets [match(st.abb, state_ten_1$State)]

map("state", col = grayvector[graymatch1], fill = TRUE)
title("Mean Income for Mortgage or Loan",sub="5 example shades given in legend")
graylegend.txt<-c(paste(as.integer(graycuts1[2])),paste(as.integer(graycuts1[25])),paste(as.integer(graycuts1[50])),paste(as.integer(graycuts1[75])),paste(as.integer(graycuts1[99])))
  legend("bottomright", graylegend.txt, cex=0.62, fill = c(grayvector[2],grayvector[25],grayvector[50],grayvector[75],grayvector[99]))


