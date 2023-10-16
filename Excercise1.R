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

Part I
Perform an explanatory analysis of the following stocks (MCD, SBUX, PBPB, CMG, TXRH, ) and calculate the mean daily return of each stock

'''{}
MCD_data <- compustat_data[compustat_data$tic == "MCD", ]                  #MCD_data
MCD_data <- MCD_data[, c("Date", "conm", "prccd")]
MCD_data$returns <- (MCD_data$prccd / lag(MCD_data$prccd)) - 1
MCD_data <- na.omit(MCD_data)
MCD_mean <- mean(MCD_data$returns)
print(MCD_mean)

SBUX_data <- compustat_data[compustat_data$tic == "SBUX", ]               #SBUX_data
SBUX_data <- SBUX_data[, c("Date", "conm", "prccd")]
SBUX_data$returns <- (SBUX_data$prccd / lag(SBUX_data$prccd)) - 1
SBUX_mean <- mean(SBUX_data$returns)
print(SBUX_mean)

PBPB_data <- compustat_data[compustat_data$tic == "PBPB", ]               #PBPB_data
PBPB_data <-PBPB_data[,c("Date", "conm", "prccd") ]
PBPB_data$returns <- (PBPB_data$prccd / lag(PBPB_data$prccd)) - 1
PBPB_data <- na.omit(PBPB_data)
PBPB_mean <- mean(PBPB_data$returns)
head(PBPB_mean)

CMG_data <- compustat_data[compustat_data$tic == "CMG", ]                #CMG_data
CMG_data <- CMG_data[, c("Date", "conm", "prccd")]
CMG_data$returns <- (CMG_data$prccd / lag(CMG_data$prccd)) - 1
CMG_data <- na.omit(CMG_data)
CMG_mean <- mean(CMG_data$returns)
print(CMG_mean)

TXRH_data <- compustat_data[compustat_data$tic == "TXRH", ]
TXRH_data <- TXRH_data[, c("Date", "conm", "prccd")]
TXRH_data$returns <- (TXRH_data$prccd / lag(TXRH_data$prccd)) - 1
TXRH_data <- na.omit(TXRH_data)
TXRH_mean <- mean(TXRH_data$returns)
print(TXRH_mean)
'''

Part II

Highet mean stock
'''{}
x <- max(TXRH_mean, CMG_mean, MCD_mean, PBPB_mean, SBUX_mean)
print(paste("The highest value is PorkBelly (PBPB) with a mean of ", round(x,6)))
'''

Convert Dates
'''{}
PBPB_data$Date <- as.Date(PBPB_data$Date, format = "%Y-%m-%d")
'''

Plot Stock
'''{}
plot(x = PBPB_data$Date,
     y = PBPB_data$prccd,
     xlab = "Date",
     ylab = "Price" 
     main = "PBPB Stock Price",
     type = "l")
'''


Load the Fama-French 3 factors daily data
'''{}
ff3 <- read.csv("fama_french_daily.csv", skip = 4)
ff3 <- na.omit(ff3)
ff3[-1] <- ff3[-1] / 100               #this scales my data to decimals, the data was downloaded in percentages
'''

Clean the column containing dates (rename the column and convert to `Date` type)
'''{}
colnames(ff3)[1] <- "Date"
ff3$Date <- na.omit(ff3$Date)
ff3$Date <- as.Date(ff3$Date , format = "%Y%m%d")
'''

Merge the two dataframes
'''{}
merged_data <- merge(PBPB_data, ff3, by = "Date")
print(merged_data)
'''

Calculate Excess Returns in a new column
'''{}
merged_data$excess_returns <- merged_data$returns - merged_data$RF
'''

Apply regression analysis based on Fama-French 3 factors model
'''{}
fama_french_model <- lm(excess_returns ~ Mkt.RF +SMB + HML, data = merged_data)
model_summary <- summary(fama_french_model)
print(model_summary)
'''

Analysis of the Results
From the p-value tests we can conclude that the three variables are all significant to the model with a significance level of 1%.
Regarding the adjusted R-squared, it is around 11.28%, which is very low and implies that around 89% of the
variability in our model cannot be explained by the explanatory variables and that only around 11% can.








