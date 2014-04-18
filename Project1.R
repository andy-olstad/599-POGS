## Project 1 Code

## Question: How different are the household incomes (mean/median/etc) for 
## households that own/rent/etc in California

## Start with 1 year data, we move to 3 yr / 5 yr 
## Data used will be AMS 2012 1 Year California .csv

## Andy's Code Below

#attach dplyr
library(dplyr)

##read in the tables
#hca<-read.csv("c:\\Users\\costco682\\Desktop\\ss12hca.csv")
#pca<-read.csv("c:\\Users\\costco682\\Desktop\\ss12pca.csv")
#Table 1 for all us
hus1<-read.csv("c:\\Users\\costco682\\Desktop\\csv_hus\\ss12husa.csv")
hus2<-read.csv("c:\\Users\\costco682\\Desktop\\csv_hus\\ss12husb.csv")
#at this point I got the error message referring me to help(memory.size)
#I tried rm(), but ended up just starting a new session for the second half
hus3<-read.csv("c:\\Users\\costco682\\Desktop\\csv_hus\\ss12husc.csv")
hus4<-read.csv("c:\\Users\\costco682\\Desktop\\csv_hus\\ss12husd.csv")

#poking around
#head(hca)
#dim(hca)
head(hus1)
dim(hus1)

#make it dplyr-able
#hca_df<- tbl_df(hca)
hus1_df<- tbl_df(hus1)
hus2_df<- tbl_df(hus2)
hus3_df<- tbl_df(hus3)
hus4_df<- tbl_df(hus4)


#poking around again
#dim(hca_df)
#names(hca_df)
dim(hus1_df)
names(hus1_df)
summarize(hus1_df, states = n_distinct(ST))


#do the "group by" thing
states1<-group_by(hus1_df,ST)
states2<-group_by(hus2_df,ST)
states3<-group_by(hus3_df,ST)
states4<-group_by(hus4_df,ST)

#find the average income for all respondents
#summarise(hca_df, avg_income = mean(HINCP, na.rm = TRUE))
#  avg_income
#1   83191.91

#do the same thing for the first section of US data
summarise(hus1_df, avg_income = mean(HINCP, na.rm = TRUE))
#  avg_income
#1   72509.22

#break it down by state
summarise(states1, avg_income = mean(HINCP, na.rm = TRUE))
#   ST avg_income
#1   1   56076.18
#2   2   76007.64
#3   4   63560.54
#4   5   51276.36
#5   6   82125.22
#6   8   74853.27
#7   9   97412.12
#8  10   74710.82
#9  11  101091.47
#10 12   64454.97
#11 13   63228.79
#12 15   82340.14
summarise(states2, avg_income = mean(HINCP, na.rm = TRUE))
#   ST avg_income
#1  16   56728.47
#2  17   72444.31
#3  18   58677.72
#4  19   60577.21
#5  20   62618.53
#6  21   56417.27
#7  22   59174.78
#8  23   57959.39
#9  24   93764.27
#10 25   88462.89
#11 26   60691.67
#12 27   71428.56
#13 28   50543.66
summarise(states3, avg_income = mean(HINCP, na.rm = TRUE))
#   ST avg_income
#1  29   58804.67
#2  30   58107.37
#3  31   62148.79
#4  32   66296.61
#5  33   80342.25
#6  34   94641.75
#7  35   58573.59
#8  36   80179.74
#9  37   60978.57
#10 38   67183.51
#11 39   60709.76
#12 40   56479.21
#13 41   64006.85
summarise(states4, avg_income = mean(HINCP, na.rm = TRUE))
#   ST avg_income
#1  42   66201.68
#2  44   74033.83
#3  45   57369.91
#4  46   59272.42
#5  47   58015.65
#6  48   69137.27
#7  49   71210.56
#8  50   67290.69
#9  51   83956.90
#10 53   74646.33
#11 54   51758.17
#12 55   64193.90
#13 56   66405.33


#find average income for subsets

#TEN==1 hold mortgages
#filter(hca_df, TEN == 1)
#summarise(filter(hca_df, TEN == 1), avg_income_mortgagees = mean(HINCP, na.rm = TRUE))
#  avg_income_mortgagees
#1   114452.3


#TEN==2 own outright
#summarise(filter(hca_df, TEN == 2), avg_income_clear_owners = mean(HINCP, na.rm = TRUE))
#   avg_income_clear_owners
#1                75645.99


#TEN==3 rent
#summarise(filter(hca_df, TEN == 3), avg_income_renters = mean(HINCP, na.rm = TRUE))
# avg_income_renters
#1           53588.75

#TEN==4 occupy without rent
#summarise(filter(hca_df, TEN == 4), avg_income_squatters = mean(HINCP, na.rm = TRUE))
# avg_income_squatters
#1             45386.33

states1TENURE<-group_by(hus1_df,ST,TEN)
summary1<-summarise(states1TENURE, average_income = mean(HINCP, na.rm=TRUE))
states2TENURE<-group_by(hus2_df,ST,TEN)
summary2<-summarise(states2TENURE, average_income = mean(HINCP, na.rm=TRUE))
states3TENURE<-group_by(hus3_df,ST,TEN)
summary3<-summarise(states3TENURE, average_income = mean(HINCP, na.rm=TRUE))
states4TENURE<-group_by(hus4_df,ST,TEN)
summary4<-summarise(states4TENURE, average_income = mean(HINCP, na.rm=TRUE))
#I think this gets us what we want, but it's very messy...

combined<-rbind(summary1,summary2,summary3,summary4)
ten1<-combined[which(combined$TEN==1),3]
ten2<-combined[which(combined$TEN==2),3]
ten3<-combined[which(combined$TEN==3),3]
ten4<-combined[which(combined$TEN==4),3]
boxplot(ten1, ten2, ten3, ten4, main = "Average Income by TENURE")

#cleanup
rm()







## Jasmine's Code Below

# i was not able to merge the 4 csv files so i just kept them all separate
# i ran the code for only one csv and it worked it took a long time so i 
# didn't try it for the others

# don't forget to check the working directory!
setwd("~/Grad School/ST 599/Project_1_Data/csv")


# download US 3 year data file csv
download.file("http://www2.census.gov/acs2012_3yr/pums/csv_hus.zip", 
              destfile = "csv_hus.zip")
# unzip csv file
unzip("csv_hus.zip", list = TRUE)
# had to manually unzip file and put into a folder 


# had trouble merging all the file together and getting r to read as one master file
# since none of the states are spilt into two different csv files i will just repeat the steps
# for each csv file and keep as 4 different dataframes, drawback will have 4 differnt plots
# later in the code you can see which states belong to what csv file

#read each csv file individually
# only the last letter will be different a, b, c, and d!
husa_3yr <- (read.csv("ss12husa.csv", header = T, stringsAsFactors = FALSE))
husb_3yr <- (read.csv("ss12husb.csv", header = T, stringsAsFactors = FALSE))
husc_3yr <- (read.csv("ss12husc.csv", header = T, stringsAsFactors = FALSE))
husd_3yr <- (read.csv("ss12husd.csv", header = T, stringsAsFactors = FALSE))

# load dplyr in order to format the data (make it nice)
library(dplyr)

# subset only the columns that we need
project1_a <- select(husa_3yr, TEN, HINCP, ST, ADJHSG)
project1_b <- select(husb_3yr, TEN, HINCP, ST, ADJHSG)
project1_c <- select(husc_3yr, TEN, HINCP, ST, ADJHSG)
project1_d <- select(husd_3yr, TEN, HINCP, ST, ADJHSG)


# 'tbl_df allows for a cleaner display of the data
project1__a_df <- tbl_df(project1_a)
project1__b_df <- tbl_df(project1_b)
project1__c_df <- tbl_df(project1_c)
project1__d_df <- tbl_df(project1_d)

# Coding the TEN variable to specifications above
TEN_codes <- c("1" = "Owned with Mortgage or Loan",
               "2" = "Owned Free and Clear",
               "3" = "Rented",
               "4" = "Occupied without Payment of Rent")
# add a new column with the codes from above to each data frame
project1_a_df <- mutate(project1__a_df, Housing_Payment = TEN_codes[as.character(TEN)])
project1_b_df <- mutate(project1__b_df, Housing_Payment = TEN_codes[as.character(TEN)])
project1_c_df <- mutate(project1__c_df, Housing_Payment = TEN_codes[as.character(TEN)])
project1_d_df <- mutate(project1__d_df, Housing_Payment = TEN_codes[as.character(TEN)])

# Removing the missing data from the TEN variable
project1_a_df <- filter(project1_a_df, TEN != "NA")
project1_b_df <- filter(project1_b_df, TEN != "NA")
project1_c_df <- filter(project1_c_df, TEN != "NA")
project1_d_df <- filter(project1_d_df, TEN != "NA")

# Coding the ADJHSG variable to specifications above
ADJHSG_codes <- c("1053092" = "2010",
                  "1020890" = "2011",
                  "1000000" = "2012")
# add a new column with the codes from above to each data frame
project1_a_df <- mutate(project1_a_df, Year = ADJHSG_codes[as.character(ADJHSG)])
project1_b_df <- mutate(project1_b_df, Year = ADJHSG_codes[as.character(ADJHSG)])
project1_c_df <- mutate(project1_c_df, Year = ADJHSG_codes[as.character(ADJHSG)])
project1_d_df <- mutate(project1_d_df, Year = ADJHSG_codes[as.character(ADJHSG)])

# Coding the ST variable to specifications above
# states included in the csv a file
ST_codesa <- c("1" = "AL",
               "2" = "AK",
               "4" = "AZ",
               "5" = "AR",
               "6" = "CA",
               "8" = "CO",
               "9" = "CT",
               "11" = "DC" ,
               "12" = "FL" )
project1_a_df <- mutate(project1_a_df, States = ST_codesa[as.character(ST)])

# states included in the csv b file
ST_codesb < - c("16" = "ID" ,
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
                "28" = "MS" )
project1_b_df <- mutate(project1_b_df, States = ST_codesb[as.character(ST)])

# states included in the csv c file
ST_codesc <- c("29" = "MO" ,
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
               "41" = "OR" )
project1_c_df <- mutate(project1_c_df, States = ST_codesc[as.character(ST)])

# states included in the csv d file
ST_codesd <- c("42" = "PA" ,
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
               "55" = "WI" )
project1_d_df <- mutate(project1_d_df, States = ST_codesd[as.character(ST)])

# warning missing GA, HI, WY and PR


#find the mean income for each ownership group by state
#groub by state
income_state_a <- group_by(project1_a_df, States)
income_state_b <- group_by(project1_b_df, States)
income_state_c <- group_by(project1_c_df, States)
income_state_d <- group_by(project1_d_df, States)

#group by Housing_Payment or type of ownership
income_ten_a <- group_by(income_state_a, Housing_Payment)
income_ten_b <- group_by(income_state_b, Housing_Payment)
income_ten_c <- group_by(income_state_c, Housing_Payment)
income_ten_d <- group_by(income_state_d, Housing_Payment)


#finding the mean HH income for each state split by housing payment group
mean_a <- summarize(income_ten_a, avg_income = mean(HINCP, na.rm= TRUE))
mean_b <- summarize(income_ten_b, avg_income = mean(HINCP, na.rm= TRUE))
mean_c <- summarize(income_ten_c, avg_income = mean(HINCP, na.rm= TRUE))
mean_d <- summarize(income_ten_d, avg_income = mean(HINCP, na.rm= TRUE))
# can do similar calculation for other summaries such as median if need/want

# to view the mean income summaries
mean_a
mean_b
mean_c
mean_d

# plot the mean for each state and use color to differentiate the type of housing payment
library(ggplot2)
qplot(States, avg_income, data = mean_a, color = Housing_Payment)
qplot(States, avg_income, data = mean_b, color = Housing_Payment)
qplot(States, avg_income, data = mean_c, color = Housing_Payment)
qplot(States, avg_income, data = mean_d, color = Housing_Payment)

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

################################ 1yr CA only dataset #################################################
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


################################ 3yr all US dataset #################################################

# set up
rm(list = ls())
setwd("~/Dropbox/Work/Classes/Spring 2014/Big data")
library(dplyr)

# get data
download.file("http://www2.census.gov/acs2012_3yr/pums/csv_hus.zip", 
              destfile = "csv_hus.zip")
unzip("csv_hus.zip", list = TRUE)

# concatenate all US csv files into 1
system("cat csv_hus/ss12husa.csv csv_hus/ss12husb.csv csv_hus/ss12husc.csv csv_hus/ss12husd.csv > ss12hus.csv")

### OPTION 1
### import bulk, select cols 
ushouseholds <- read.csv("ss12hus.csv", header = T, stringsAsFactors = FALSE)
ushouseholds_df <- tbl_df(ushouseholds)
tenure_by_income_df <- select(ushouseholds_df, TEN, HINCP, ST, ADJHSG)

### OPTION 2
### only import select cols because the dataset is friggin big and there are 4,360,176 rows in it
# find cols in shell:
# head -n 1 ss12hus.csv | tr ',' '\n' | nl | grep -w 'TEN\|HINCP\|ST\|ADJHSG' | less -S

# head: in header line, 
# tr: move each colname to new line, 
# nl: add line numbers, 
# grep: search for our terms

# 7  ST
# 8  ADJHSG
# 40  TEN
# 54  HINCP

ushouseholds <- read.csv(pipe("cut -d, -f7,8,40,54 ss12hus.csv"), header = T, stringsAsFactors = FALSE)
ushouseholds_df <- tbl_df(ushouseholds)

# test dataset
# ushouseholds <- read.csv(pipe("cut -d, -f 7,8,40,54 test_ss12hus.csv"), header = T, stringsAsFactors = FALSE)
# ushouseholds_df <- tbl_df(ushouseholds)

# check
print(ushouseholds_df)

# use Jasmine's code to recode vars
ADJHSG_codes <- c("1053092" = "2010",
                  "1020890" = "2011",
                  "1000000" = "2012")
ushouseholds_df <- mutate(ushouseholds_df, Year = ADJHSG_codes[as.character(ADJHSG)])

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
              "72" = "PR" )
ushouseholds_df <- mutate(ushouseholds_df, States = ST_codes[as.character(ST)])

TEN_codes <- c('1' = 'Owned with mortgage or loan', 
               '2' = 'Owned free and clear',  
               '3' = 'Rented',
               '4' = 'Occupied without payment of rent')
ushouseholds_df <- mutate(ushouseholds_df, Tenure = TEN_codes[as.character(TEN)])

# calculate stuff

tenure_by_income_mean <- summarise(group_by(ushouseholds_df, Tenure), mean(HINCP, na.rm = TRUE))
tenure_by_income_median <- summarise(group_by(ushouseholds_df, Tenure), median(HINCP, na.rm = TRUE))


###########################################################



# Tim's Code Below
library(dplyr)
??dplyr

# Reading in the data, putting it into dplyr table, and selecting the variables of interest
setwd("C:/Documents and Settings/Tim Skalland/Desktop/ST 599 - Big Data")

#dwnld <- function(state_name){
#  download.file(paste("http://www2.census.gov/acs2012_3yr/pums/csv_h",state_name,".zip", sep=""),
#              paste(destfile = "Data/csv_h",state_name,".zip", sep=""))
#}


usa_data <- read.csv("Data/csv_hus/ss12husa.csv", stringsAsFactors = FALSE)
usb_data <- read.csv("Data/csv_hus/ss12husb.csv", stringsAsFactors = FALSE)
usc_data <- read.csv("Data/csv_hus/ss12husc.csv", stringsAsFactors = FALSE)
usd_data <- read.csv("Data/csv_hus/ss12husd.csv", stringsAsFactors = FALSE)

head(usb_data)

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
















