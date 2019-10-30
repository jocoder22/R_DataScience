# Load the required packages
library(quantmod, quietly=T)
library(PerformanceAnalytics, quietly = TRUE)
library(ROI, quietly = TRUE)
library(timeSeries)
library(tseries)
library(xts)
library(xtsExtra)

# Download stocks
tickers = c('AMZN','AAPL','MSFT',"SPY", "AGG", "VNQ", "GSG","JPM", "^GSPC")
getSymbols(tickers, from='2007-01-03') 


# Merge them together
names = c('AMZN','AAPL','MSFT',"SPY","AGG", "VNQ", "GSG","JPM", "GSPC")
portfolio  <- Ad(merge(AMZN, AAPL, MSFT, SPY, AGG, VNQ, GSG, JPM, GSPC))
colnames(portfolio) <- names


# compute return of assets
portfolioReturn <- Return.calculate(portfolio)[-1]
num <- ncol(portfolio)



# compute the optimium portfolio
opt_port <- portfolio.optim(portfolioReturn)
opt_weights <- opt_port$pw

# Assign asset names
names(opt_weights) <- colnames(portfolioReturn)

# Bar plot of opt_weights
barplot(opt_weights)

