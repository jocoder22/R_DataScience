
# load required libraries
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
library(forecast)
library(tidyr)

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
JPM_return <- CalculateReturns(JP_AdjClose)
JP_CloseReturns <- diff(JP_AdjClose)/lag(JP_AdjClose)


simple_jp <- JP_AdjClose[-1]/JP_AdjClose[-229] - 1
jp_comp <- diff(log(JP_AdjClose))
head(JPM_return)
head(JP_CloseReturns)




jp1 <- JP_AdjClose[-1]
jp2 <- JP_AdjClose[-229]


index(jp2) <- index(jp1)
head(jp1)
head(jp2)
jpp <- jp1/jp2 - 1
head(jpp)


length(JP_AdjClose)
head(JP_AdjClose[-1])
head(JP_AdjClose[-229])

tail(JP_CloseReturns)
tail(jpp)
tail(JP_CloseReturns)


all(jpp == JPM_return)
all(JP_CloseReturns == JPM_return)
all(jpp == JP_CloseReturns)


typeof(jpp)
class(jpp)
class(JP_CloseReturns)
class(JPM_return)


typeof(jpp)
typeof(JP_CloseReturns)
typeof(JPM_return)
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


# perform Augmented Dickey-Fuller test
adf.test(na.omit(diff(CSUSHPINSA, lag=1,differences=2)))
adf.test(na.omit(diff(CSUSHPINSA, lag=1)))
adf.test(log(CSUSHPINSA))
adf.test(na.omit(diff(log(CSUSHPINSA))))


adf.test(diff(CSUSHPINSA, lag=1) %>% drop_na())
index(CSUSHPINSA)

# evaluate acf and pacf
CSUSHPINSA_sta <- na.omit(diff(CSUSHPINSA, lag=1,differences=2))
par(mfrow=c(2, 1))
acf(CSUSHPINSA_sta)
pacf(CSUSHPINSA_sta)