# Load the required packages
library(quantmod, quietly=T)
library(PerformanceAnalytics, quietly = TRUE)
library(ROI, quietly = TRUE)
library(timeSeries)
library(tseries)



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


# create equal weights
weight_eq <- rep(1/num, num)

benchmark <- Return.portfolio(R = portfolioReturn,
                              weights = weight_eq, rebalance_on = "years")

colnames(benchmark) <- "benchmark"


# Benchmark PerformanceAnalytics
table.AnnualizedReturns(benchmark)



# Constant weight
constant_weight <- Return.portfolio(R = portfolioReturn,
                                    weights = weight_eq, verbose = TRUE)


rebalancing_weight <- Return.portfolio(R = portfolioReturn, verbose = TRUE,
                                       weights = weight_eq, rebalance_on = "months")


# create End of periods weights
eop_cont <- constant_weight$EOP.Weight
eop_rebal <- rebalancing_weight$EOP.Weight

# plot the weights
plot(eop_cont, main = "Plot of Portfolio Returns with Constant Weights")
