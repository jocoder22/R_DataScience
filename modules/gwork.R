#########################################################################################
#########################################################################################
########## Analysis plan ################################################################
# Load necessary packages
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(forecast, quietly = T)
library(xts, quietly = T)
library(FinTS, quietly = T)
library(fGarch, quietly = T)
library(astsa, quietly = T)
library(rugarch, quietly = T)
library(forecast, quietly = T)



filepath <- getwd()
sink(file.path(filepath, "modules", "submission22.doc"),  
     append=FALSE, split=FALSE, type = c("output", "message"))


# Data source: Yahoo finance
# Period considered in the analysis: January 2010 -  September 27, 2019
# Frequency: Daily

# Download datasets
startDate <- "2010-01-01"
endDate <- "2019-09-27"


getSymbols("AAPL", src="yahoo", from=startDate, to=endDate)
apple <- AAPL[, "AAPL.Adjusted"] 


# Calculate Apple returns
appleReturns <- CalculateReturns(apple)[-1]
colnames(appleReturns) <- "Returns"


# plot the graphs
par(mfrow = c(2,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(apple, main="Apple stock Adjusted Close price")
plot(appleReturns, main="Apple stock Returns")

head(apple)
tail(apple)
# compute basic statistics
returnMean <- mean(appleReturns)
returnVolatility <- sd(appleReturns)
returnSkewness <- skewness(appleReturns)
returnKurtosis <- kurtosis(appleReturns)

returnMean 
returnVolatility 
returnSkewness 
returnKurtosis 


# Perform adf test and Box-Test on Apple Returns
tseries::adf.test(appleReturns)
Box.test(appleReturns, type="Ljung-Box", lag=12)


#########################################################
# plot acf and pacf of apple returns and squared returns
acf2(appleReturns, main=" Apple Stock Returns ")
acf2(appleReturns^2, main=" Squared Apple Stock Returns ")

frequency(apple)
Box.test(appleReturns, type="Ljung-Box", lag=12)
Box.test(appleReturns^2, type="Ljung-Box", lag=12)

ArchTest(coredata(appleReturns))
adf.test(appleReturns)


tseries::adf.test(appleReturns)
auto.arima(appleReturns)

# Visualized distribution of rolling window volatility
par(mfrow = c(3,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
rollweekly <- rollapply(appleReturns, width = 5 , FUN = "sd.annualized")
rollmonthly <- rollapply(appleReturns, width = 22 , FUN = "sd.annualized")
rollquarterly <- rollapply(appleReturns, width = 65 , FUN = "sd.annualized")


plot(rollweekly, main="Apple Stock Weekly Returns Volatility")
plot(rollmonthly, main="Apple Stock Monthly Returns Volatility")
plot(rollquarterly, main="Apple Stock Quarterly Returns Volatility")



# plot the residuals
predError <- appleReturns - returnMean
par(mfrow = c(2,1), mar = c(4,3,3,3), oma = c(1, 1, 1, 1))
plot(abs(predError), main="Absolute Prediction Error")
acf(abs(predError), main="ACF of Absolute Prediction Error")



# plot the graphs
par(mfrow = c(2,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(appleReturns, main="Apple stock Returns")
plot(rollVol, main="Apple stock Returns: 1 Month rolling Volatility")


# compute formal statistical test: Autocorrelation and ARCH test
Box.test(appleReturns^2, type = "Ljung-Box", lag = 30)
ArchTest(appleReturns)


source(file.path(filepath, "modules","garchAuto.R"))


auto.arima(appleReturns)
autoarfima(appleReturns, ar.max = 5, ma.max=5, criterion = "AIC", method="full")
# spy = getSymbols("SPY", auto.assign=FALSE)
# rets = ROC(Cl(spy), na.pad=FALSE)
# fit = garchAuto(appleReturns, cores=1, trace=TRUE)
# spy = getSymbols("SPY", auto.assign=FALSE)
# rets = ROC(Cl(spy), na.pad=FALSE)
# fit = garchAuto(rets, cores=1, trace=TRUE)

# # Analyzing (5,2,1,1) with sged distribution done.Good model. AIC = -5.567287, forecast: 0.0024
#

# model using rugarch::
garchspec <- ugarchspec(mean.model = list(armaOrder=c(2,1)),
                        variance.model = list(model="sGARCH"),
                        distribution.model = "sstd")

model4 <- ugarchfit(spec=garchspec, data=appleReturns)
# summary(model4)
model4


stdret <- residuals(model4, standardize = TRUE)
#
#
# plot the histograms
chart.Histogram(stdret, methods = c("add.normal","add.density" ),
                colorset = c("gray","red","blue"),
                main="Model Predicted Residuals under Normal Distribution")


chart.Histogram(predError, methods = c("add.normal","add.density" ),
                colorset = c("gray","red","blue"),
                main="Apple Returns under Normal Distribution")

#
sink()



