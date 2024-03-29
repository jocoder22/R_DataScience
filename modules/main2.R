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
library(astsa)

# Dataset: Apple Stock Prices
# Source: Yahoo Finance
# Frequency: daily 
# Period: January 1st 2000 – September 29th 2019


# Download datasets
startDate <- "2000-01-01"
endDate <- "2019-09-27"


getSymbols("AAPL", src="yahoo", from=startDate, to=endDate)
appleData <- AAPL[, "AAPL.Adjusted"] 

appleR <- dailyReturn(Ad(AAPL), type = "arithmetic")
Lo(AAPL)
closeC <- ClCl()

leadd <- OHLC(AAPL)
head(leadd)

# Calculate Apple returns
apple <- CalculateReturns(appleData)[-1]
colnames(apple) <- "Returns"

head(apple)
head(appleR)
head(closeC)


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
acf2(appleR)



# quick check using auto arima
mod1 <- auto.arima(apple$Returns)
mod2 <- auto.arima(appleR, stationary = TRUE, seasonal = TRUE, ic="aic")
tsdiag(mod2)

mod3 <- sarima(appleR,0,0,0)
mod3 <- sarima(appleR,5,0,0)
ts.diag(mod3)
ts.diag(mod1)

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



# graph the distribution
chart.Histogram(apple, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Empirical Distribution vs Normal Distribution")

adfTest(apple$Returns)


tseries::adf.test(apple$Returns)





# model the data using fGarch::
model1 <- garchFit(formula = ~ garch(5,0), data = apple$Returns, trace = F)
summary(model1)


model2 <- garchFit(formula = ~ arma(0,0) + garch(1, 2), data = apple$Returns, 
                   trace = F, cond.dist="norm")
# 'arg' should be one of “norm”, “snorm”, “ged”, “sged”, “std”, “sstd”, “snig”, “QMLE”
summary(model2)
show(model2)


model3 <- garchFit(formula = ~ arma(0,0) + garch(1, 2), data = apple$Returns, 
                   trace = F, cond.dist="sstd")
summary(model3)

coef(model2)
mytable <- as.data.frame(coef(model3))
mytable$model3 <- coef(model3)



# model using rugarch::
garchspec <- ugarchspec(mean.model = list(armaOrder=c(2,2)),
                        variance.model = list(model="gjrGARCH", garchOrder=c(1,1)),
                        distribution.model = "sstd")

model4 <- ugarchfit(spec=garchspec, data=appleR)
summary(model4)
model4


# uncov <- sqrt(coef(model4)[1]/(1 - sum(coef(model4)[2:5])))
# predvol <- xts(sqrt(252) * model4@fit$sigma, order.by = time(apple))
# uncon <- xts(rep(uncov, length(predvol)),  order.by = time(apple))
# plot(predvol)
# lines(uncon, type='l', col="red")




