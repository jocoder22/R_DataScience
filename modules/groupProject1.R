
library(xlsx)
library(TTR)
library(zoo)
library(xts)
library(quantmod)
library(PerformanceAnalytics)
library(tidyverse)
library(stats)
library(readxl)
library(tseries)
library(forcast)

startDate <- "2018-02-01"
endDate <- "2018-12-30"
getSymbols("JPM", src="yahoo", from=startDate, to=endDate )
JP_AdjClose <- JPM[, "JPM.Adjusted", drop = FALSE]

getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
SP500_AdjClose <- GSPC[, "GSPC.Adjusted", drop=FALSE]

allData <- data.frame(JP_AdjClose, SP500_AdjClose)

# visualize the datasets
head(JP_AdjClose)
head(SP500_AdjClose)
head(allData)


# Save dataset for future analysis
# Save dataset to excel

# write.xlsx(JP_AdjClose,  file="myworkbook.xlsx", sheetName ="Sheet1", 
#            col.names=TRUE, row.names=TRUE, append=FALSE)
# 
# write.xlsx(allData,  file="myworkbook2.xlsx", sheetName="Sheet1", 
#            col.names=TRUE, row.names=TRUE, append=FALSE)


# Calculate basic statistics
mean(JP_AdjClose)
var(JP_AdjClose)
sd(JP_AdjClose)
StdDev(JP_AdjClose)


# calculate daily stock return
JPM_return <- CalculateReturns(JP_AdjClose)[-1]
JP_CloseReturns <- diff(JP_AdjClose)/lag(JP_AdjClose)



# Plot the graph
plot(JP_AdjClose, type="l", col="blue", lwd=2, 
     ylab = "Adjusted close", main = "Daily Adjusted price of JP Morgan Stock")

plot(JPM_return, type="l", col="blue", lwd=2, 
     ylab="Daily Returns", main="Daily stock return of JP Morgan")


# Implement a two-variable regression
fit <- lm(JPM.Adjusted ~ GSPC.Adjusted, data=allData)
summary(fit)


# Univariate Time Series Analysis
startdate <- "1978-01-01"
getSymbols("CSUSHPINSA", src='FRED', from = "1978-01-01" )

# Exploratory data analysis
head(CSUSHPINSA)
str(CSUSHPINSA)
attr(CSUSHPINSA, "dimnames")
class(CSUSHPINSA)
summary(CSUSHPINSA)


# plot the monthly home price index
par(mfrow=c(3, 1))
plot(CSUSHPINSA, type="l", col="blue", lwd=2, ylab="National Home Price Index", 
     main="S&P/Case-Shiller U.S National Home Price Index")


# plot the acf and pacf 
acf(CSUSHPINSA)
pacf(CSUSHPINSA)

na.omit(diff(CSUSHPINSA))
CSUSHPINSA[complete.cases(log(diff(CSUSHPINSA))), ]


# plot the detrending transformations
par(mfrow=c(4, 1))
plot(log(CSUSHPINSA))
plot(diff(log(CSUSHPINSA)))
plot(diff(CSUSHPINSA, lag=1,differences=2))
plot(diff(CSUSHPINSA, lag=1))


