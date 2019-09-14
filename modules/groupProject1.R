install.packages("PerformanceAnalytics")

library(TTR)
library(zoo)
library(xts)
library(quantmod)
library(PerformanceAnalytics)

getSymbols("JPM", src="yahoo", from = "2018-02-01", to = "2018-12-30")
JP_AdjClose <- JPM[, "JPM.Adjusted", drop = FALSE]
JP_AdjClose$Returns <- diff(JP_AdjClose)/lag(JP_AdjClose)
JPM_Returns <- JP_AdjClose[-1]

var(JP_AdjClose)
sd(JP_AdjClose)
StdDev(JP_AdjClose)