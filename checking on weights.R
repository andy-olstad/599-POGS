####testing the weight thing
setwd("C:/Users/Costco682/Documents/GitHub/599-POGS/")
library(dplyr)
hus1<-read.csv("c:\\Users\\costco682\\Desktop\\csv_hus\\ss12husa.csv")
hus1_df<- tbl_df(hus1)
alabama<-filter(hus1_df,ST==1)
AL<-select(alabama,WGTP,HINCP,TEN)
dim(AL)
AL<-mutate(AL,wt.hinc=WGTP*HINCP)
summarise(AL,avg=mean(HINCP,na.rm=TRUE))
ALtotal<-sum(as.numeric(AL$WGTP,rm=TRUE),na.rm=TRUE)

sum1<-summarise(AL, count=n(), 
totalweights=sum(WGTP),
sum_income = sum(as.numeric(wt.hinc),na.rm=TRUE))
sum1$sum_income/sum1$totalweights

AL_ten<-group_by(AL,TEN)
sum_AL<-summarise(AL_ten, unwt.avg=mean(HINCP,na.rm=TRUE),count=n(), 
totalweights=sum(WGTP),
sum_income = sum(as.numeric(wt.hinc),na.rm=TRUE),
wt.avg=sum_income/totalweights)

sum_AL
> sum_AL
#Source: local data frame [5 x 6]
#
#  TEN unwt.avg count totalweights  sum_income   wt.avg
#1   1 76461.47 24468       760350 59110393056 77741.03
#2   2 50264.62 19227       517159 26223411537 50706.67
#3   3 31425.43 13404       497347 16322764228 32819.67
#4   4 28451.93  2050        62971  1804941420 28663.06
#5  NA      NaN 12075       343987           0     0.00

project<-read.table(file="state_ten",header=TRUE,sep=",")