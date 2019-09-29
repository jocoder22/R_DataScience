# load required packages
library(quantmod, quietly = T)
library(forecast, quietly = T)
library(lmtest, quietly = T)
library(xts, quietly = T)
library(aTSA, quietly = T)
library(glue, quietly = T)
library(psych, quietly = T)
library(pastecs, quietly = T)
library(fUnitRoots)
library(tseries)
library(PerformanceAnalytics)

# Dataset: Apple Stock Prices
# Source: Yahoo Finance
# Frequency: daily 
# Period: January 1st 2000 – September 29th 2019


# Download datasets
startDate <- "2000-01-01"
endDate <- "2019-09-27"


getSymbols("AAPL", src="yahoo", from=startDate, to=endDate)
apple <- AAPL[, "AAPL.Adjusted", drop = FALSE]


# Calculate Apple returns
appleReturn <- CalculateReturns(apple)[-1]
colnames(appleReturn) <- "Returns"



# Explore the dataset
head(appleReturn)
str(appleReturn)



# get statistics
summary(appleReturn)
psych::describe(appleReturn)
round(stat.desc(appleReturn), 4)


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
plot(vCK)
vCK$sd <- mean(na.omit(vCK$EMA))
lines(vCK$sd, col="red")


# plot the annualised volatility
appleReturn$sd <- sd(appleReturn) * sqrt(252)
chart.RollingPerformance(R = appleReturn$Returns,
                         width = 24,
                         scale = 252,
                         FUN = "sd.annualized",
                         main = "Rolling Monthly Annualised volatility")
lines(appleReturn$sd, col="red")
addSeries(appleReturn$sd, col = "red")



# plot the graphs
par(mfrow = c(2,1))
par(mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(apple)
plot(appleReturn)

# plot the acf and pacf
acf2(apple)
acf2(appleReturn$Returns)
acf2(appleReturn$Returns^2)

# quick check using auto arima
auto.arima(appleReturn)


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


allTest(appleReturn$Returns)

adf.test(appleReturn$Returns)

# graph the distribution
chart.Histogram(appleReturn, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Empirical Distribution vs Normal Distribution")

adfTest(appleReturn$Returns)


tseries::adf.test(appleReturn$Returns)
 

library(fGarch)


model1 <- garchFit(formula = ~ garch(3,0), data = appleReturn$Returns, trace = F)
summary(model1)


model2 <- garchFit(formula = ~ arma(0,0) + garch(1, 2), data = appleReturn$Returns, 
                   trace = F, cond.dist="snorm")
# 'arg' should be one of “norm”, “snorm”, “ged”, “sged”, “std”, “sstd”, “snig”, “QMLE”
summary(model2)


model3 <- garchFit(formula = ~ arma(0,0) + garch(1, 2), data = appleReturn$Returns, 
                   trace = F, cond.dist="sstd")
# 'arg' should be one of “norm”, “snorm”, “ged”, “sged”, “std”, “sstd”, “snig”, “QMLE”
summary(model3)

coef(model2)
mytable <- as.data.frame(coef(model3))
mytable$model3 <- coef(model3)






garchspec <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="sGARCH", garchOrder=c(1,2)),
                         distribution.model = "std")

model4 <- ugarchfit(spec=garchspec, data=appleReturn$Returns)
summary(model4)
model4
uncov <- sqrt(coef(model4)[1]/(1 - sum(coef(model4)[2:5])))
predvol <- xts(sqrt(252) * model4@fit$sigma, order.by = time(appleReturn))
uncon <- xts(rep(1.18172, length(predvol)),  order.by = time(appleReturn))
plot(predvol)
lines(uncon, type='l', col="red")
