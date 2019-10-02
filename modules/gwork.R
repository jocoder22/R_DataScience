#########################################################################################
#########################################################################################
########## Analysis plan ################################################################
# Load necessary packages
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
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
colnames(apple) <- "Returns"


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

plot(modelvol[, "Sigma"], main="New York Times Returns GARCH Volatility")
lines(modelvol[, "hhh"], col="red")



sink()

