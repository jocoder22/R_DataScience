# load required packages
library(quantmod, quietly = T)
library(forecast, quietly = T)
library(xts, quietly = T)
library(glue, quietly = T)
library(psych, quietly = T)
library(pastecs, quietly = T)
library(fUnitRoots)
library(tseries)
library(PerformanceAnalytics)
library(fGarch)

# Dataset: Apple Stock Prices
# Source: Yahoo Finance
# Frequency: daily 
# Period: January 1st 2000 – September 29th 2019


# Download datasets
startDate <- "2000-01-01"
endDate <- "2019-09-27"


getSymbols("AAPL", src="yahoo", from=startDate, to=endDate)


# Calculate Apple returns
apple <- CalculateReturns(AAPL[, "AAPL.Adjusted"])[-1]
colnames(apple) <- "Returns"



# Explore the dataset
head(apple)
str(apple)


# get statistics
summary(apple)
psych::describe(apple)
round(stat.desc(apple), 4)


# Calculate daily volatility
ohlc <- AAPL[,c("AAPL.Open", "AAPL.High", "AAPL.Low", "AAPL.Close")]
ohlc2 <- AAPL[,"AAPL.Close"]
vClose <- volatility(ohlc2, calc="close")
vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
vGK <- volatility(ohlc, calc="garman")
vParkinson <- volatility(ohlc, calc="parkinson")
vRS <- volatility(ohlc, calc="rogers")
vCK <- chaikinVolatility(AAPL[,c("AAPL.High", "AAPL.Low")])
