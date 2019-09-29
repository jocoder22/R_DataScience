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
appleData <- AAPL[, "AAPL.Adjusted"] 


# Calculate Apple returns
apple <- CalculateReturns(appleData)[-1]
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


# plot the volatility
# plot the volatility
apple$sd <- sd(apple$Returns) * sqrt(252)
plot(vCK)
vCK$sd <- mean(na.omit(vCK$EMA))
lines(vCK$sd, col="red")
addSeries(apple$sd, col = "green")



# plot the annualised volatility
rollsd <- rollapply(apple[,"Returns"], width =7 , FUN = "sd")
plot(rollsd)
addSeries(apple$sd, col = "red")
skewness(na.omit(rollsd)) # 3.679407
kurtosis(na.omit(rollsd)) # 34.19827
acf2(rollsd)




# plot the graphs
par(mfrow = c(2,1))
par(mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(appleData)
plot(apple)

# plot the acf and pacf
acf2(apple)
acf2(apple$Returns)
acf2(apple$Returns^2)




# quick check using auto arima
auto.arima(apple)


# formal testing for stationarity
# using user defined fuction allTest
allTest <- function(series){
  x <- na.omit(series)
  testVector <- c("adf", "pp", "kpss")
  for (val in testVector){
    stationary.test(series, method = val);
    cat("\n\n\n##########################################\n")
  }
}


allTest(apple$Returns)

adf.test(apple$Returns)
