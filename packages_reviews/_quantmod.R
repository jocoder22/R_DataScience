
# Load the required packages
library(quantmod, quietly=T)
library(PerformanceAnalytics, quietly = TRUE)
library(timeSeries)
library(tseries)
library(forecast)
library(astsa)
require(TTR)
library(fpp2)

# set my file path
currentdir <- getwd()
filepath = file.path(currentdir,"packages_reviews", "quantmod.rda")

# Download datasets
startDate <- "2018-02-01"
endDate <- "2018-12-30"

getSymbols("JPM", src="yahoo", from=startDate, to=endDate )
getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
getSymbols("DEXUSEU", src="FRED")
getSymbols("USD/CNH", src="oanda")
getSymbols("XPT/USD", src="oanda")


# set multiple dataset lookup  
setSymbolLookup(GOOG="yahoo", XPTUSD=list(name="XPT/USD",src="oanda"))
setSymbolLookup(DEXUSEU="FRED", AMZN="yahoo", CSUSHPINSA="FRED")
saveSymbolLookup(file=filepath)

load("~/R_DataScience/packages_reviews/quantmod.rda")
# use the lookup.list
getSymbols(c("GOOG", "AMZN", "XPTUSD", "CSUSHPINSA", "DEXUSEU"))

allvect  <- list(GOOG, AMZN, XPTUSD, CSUSHPINSA, DEXUSEU)



# Calcuate periodicity and number of periods
sapply(allvect, FUN=periodicity)
sapply(allvect, FUN=nyears)
sapply(allvect, FUN=nmonths())
sapply(allvect, FUN=ndays())

# Creating charts and graphs
barChart(GOOG)

# create candleChart
candleChart(AMZN)


# Create series charts
chartSeries(XPTUSD,name="Platinum (.oz) in $USD")
chartSeries(to.weekly(XPTUSD),up.col='green',dn.col='red')



# Technical analysis chart
chartSeries(AMZN)
addMACD()
addBBands()



library(qmao)

tickers = c('AMZN','AAPL','MSFT')
getSymbols(tickers,from='2005-01-01') 


tickers2  <- c("SPY", "AGG", "VNQ", "GSG")
getSymbols(tickers2)
plot(SPY$SPY.Adjusted)
lines(AGG$AGG.Adjusted, col="red")
axis(side = 4, at = pretty(AGG$AGG.Adjusted), col="red")

returns_equities  <- Return.calculate(SPY$SPY.Adjusted)[-1]
returns_bonds  <-  returns0(AGG$AGG.Adjusted)[-1]

# Create a grid
grid <- seq(0, 1, 0.01)

# Initialize an empty vector for Sharpe ratios
vsharpe <- rep(NA, times = length(grid))

# Create a for loop to calculate Sharpe ratios
for(i in 1:length(grid)) {
  weight <- grid[i]
  preturns <- weight * returns_equities + (1 - weight) * returns_bonds
  vsharpe[i] <- SharpeRatio.annualized(preturns)
}

# Plot weights and Sharpe ratio
plot(grid, vsharpe, xlab = "Weights", ylab = "Ann. Sharpe ratio")
abline(v = grid[which.max(vsharpe)], lty = 3)
       



SPYRet  <- returns0(Ad(SPY))
AGGRet  <- returns0(Ad(AGG))
VNQRet  <- returns0(Ad(VNQ))
GSGRet  <- returns0(Ad(GSG))


portfolio  <- Ad(merge(SPY, AGG, VNQ, GSG))
portfolio2  <- apply(portfolio, 2, "returns0")
portfolio3  <-  CalculateReturns(portfolio)[-1]
plot(portfolio)
n  <- length(portfolio3[1,])
weights  <- c(rep(1/n, n))
returnP  <- Return.portfolio(portfolio3, verbose = TRUE)
returnB  <- Return.portfolio(portfolio3, weights = weights, verbose = TRUE,
                             rebalance_on = "months")

summary(returnP)



# Create end of eop_weight_bh
eop_weight_bh <- returnP$EOP.Weight

# Create eop_weight_reabl
eop_weight_rebal <- returnB$EOP.Weight
colnames(eop_weight_bh)

# Plot end of period weights
par(mfrow = c(2, 1), mar = c(2, 4, 2, 2))
plot.zoo(eop_weight_bh$SPY.Adjusted)
plot.zoo(eop_weight_rebal$SPY.Adjusted)


par(mfrow = c(2, 2), mex = 0.8, cex=0.6)
for(col in colnames(eop_weight_bh)){
  
  plot.zoo(eop_weight_bh[, col], ylab = col)
  # print(head(eop_weight_bh[,col]))
}



fullreturn  <- returnP$returns
gg  <-  fullreturn + 1
gmm  <- cumprod(gg)
plot(gmm)




# Create volatility budget
vol_budget <- StdDev(portfolio3, portfolio_method = "component", weights = weights)

# Make a table of weights and risk contribution
weights_percrisk <- cbind(weights, vol_budget$pct_contrib_StdDev)
colnames(weights_percrisk) <- c("weights", "perc vol contrib")

# Print the table
print(weights_percrisk)
