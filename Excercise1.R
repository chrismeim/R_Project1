Introduction

The aim of this excercise is to create a multiple linear regression model that will explain the "excess returns" of a stock,
taking into account the 3 factor fama-french model (Market Risk, Size Premium and Value Premium) 

In the first part of the excercise I calculate daily returns for up to six stocks in the data set,
and based on a simple metric, I will choose one stock for further analysis.

In the second part of the excercise I load and prepare prepare the `fama_french_daily.csv` data,
merge the two dataframes, and perform multiple regression on the chosen stock
using the Fama-French 3 Factors model.

Load R packages
'''{}
load(dplyr)
load(PerformanceAnalytics)
'''

Load Trading Data & perform some data changes to the Date format
'''{}
compustat_data <- read.csv("compustat_food_bev.csv")                                                #download data from the file "ompustat_food_bev.csv"
colnames(compustat_data)[3] <- "Date"
compustat_data$Date <- as.Date(compustat_data$Date, format = "%d/%m/%Y")                            #Changing the format of the Date column into R-Studio
head(compustat_data)
'''


Perform an explanatory analysis of the following stocks (MCD, SBUX, PBPB, CMG, TXRH, ) and calculate the mean daily return of each stock

'''{}
MCD_data <- compustat_data[compustat_data$tic == "MCD", ]                  #MCD_data
MCD_data <- MCD_data[, c("Date", "conm", "prccd")]
MCD_data$returns <- (MCD_data$prccd / lag(MCD_data$prccd)) - 1
MCD_data <- na.omit(MCD_data)
MCD_mean <- mean(MCD_data$returns)
print(MCD_mean)




