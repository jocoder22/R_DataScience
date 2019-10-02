#########################################################################################
#########################################################################################
########## Analysis plan ################################################################
# Load necessary packages
library(quantmod)
library(PerformanceAnalytics)



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
par(mfrow = c(2,1))
par(mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(apple, main="Apple stock Adjusted Close price")
plot(appleR, main="Apple stock Returns")


# compute basic statistics
returnMean <- mean(appleR)
returnVolatility <- sd(appleR)
returnSkewness <- skewness(appleR)
returnKurtosis <- kurtosi(appleR)




sink()

