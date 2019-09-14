install.packages("PerformanceAnalytics")

library(TTR)
library(zoo)
library(xts)
library(quantmod)
library(PerformanceAnalytics)

getSymbols("JPM", src="yahoo", from = "2018-02-01", to = "2018-12-30")
JP_AdjClose <- JPM[, "JPM.Adjusted", drop = FALSE]

# Calculate basic statistics
mean(JP_AdjClose)
var(JP_AdjClose)
sd(JP_AdjClose)
StdDev(JP_AdjClose)


# calculate daily stock return
JPM_return <- CalculateReturns(JP_AdjClose)[-1]
JP_AdjClose$Returns <- diff(JP_AdjClose)/lag(JP_AdjClose)
JP_AdjClose <- JP_AdjClose[-1]


# Plot the graph
plot(JP_AdjClose, type = "l", col = "blue", lwd = 2, 
     ylab = "Adjusted close", main = "Daily Adjusted price of JP Morgan Stock")

plot(JPM_return, type = "l", col = "blue", lwd = 2, 
     ylab = "Daily Returns", main = "Daily stock return of JP Morgan")