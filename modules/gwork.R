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
sink(file.path(filepath, "modules", "submission2.doc"),  
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
appleR <- CalculateReturns(apple)[-1]
colnames(appleR) <- "Returns"


# plot the graphs
par(mfrow = c(2,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(apple, main="Apple stock Adjusted Close price")
plot(appleR, main="Apple stock Returns")


# compute basic statistics
returnMean <- mean(appleR)
returnVolatility <- sd(appleR)
returnSkewness <- skewness(appleR)
returnKurtosis <- kurtosis(appleR)

# plot acf and pacf of apple returns and squared returns
acf2(appleR, main="Apple Stock Returns")
acf2(appleR^2, main="Apple Stock Returns Squared")



# Visualized distribution of rolling window volatility
rollVol <- rollapply(appleR, width = 22 , FUN = "sd.annualized")
rollVol <- na.omit(rollVol)
names(rollVol) <- "rollingVol"


rollVol$longVol <- returnVolatility
volplot <- plot(rollVol[, "rollingVol"], main="Apple Stock Returns Volatility")
volplot <- addSeries(rollVol[, "longVol"], col="red")
volplot


# plot the residuals
predError <- appleR - returnMean
par(mfrow = c(2,1), mar = c(4,3,3,3), oma = c(1, 1, 1, 1))
plot(abs(predError), main="Absolute Prediction Error")
acf(abs(predError), main="ACF of Absolute Prediction Error")



# plot the graphs
par(mfrow = c(2,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(appleR, main="Apple stock Returns")
plot(rollVol, main="Apple stock Returns: 1 Month rolling Volatility")


# compute formal statistical test: Autocorrelation and ARCH test
Box.test(appleR^2, type = "Ljung-Box", lag = 30)
ArchTest(appleR)


source(file.path(filepath, "modules","garchAuto.R"))

# spy = getSymbols("SPY", auto.assign=FALSE) 
# rets = ROC(Cl(spy), na.pad=FALSE)
fit = garchAuto(appleR, cores=1, trace=TRUE)
# spy = getSymbols("SPY", auto.assign=FALSE)
# rets = ROC(Cl(spy), na.pad=FALSE)
# fit = garchAuto(rets, cores=1, trace=TRUE)

# Analyzing (5,2,1,1) with sged distribution done.Good model. AIC = -5.567287, forecast: 0.0024


# model using rugarch::
garchspec <- ugarchspec(mean.model = list(armaOrder=c(3,2)),
                        variance.model = list(model="sGARCH", garchOrder=c(1,1)),
                        distribution.model = "sstd")

model4 <- ugarchfit(spec=garchspec, data=appleR)
summary(model4)
model4


stdret2 <- residuals(model4, standardize = TRUE)


# plot the histograms
chart.Histogram(stdret, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals under Normal Distribution")

 
sink()



 