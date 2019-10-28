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

summary(coredata(portfolio))
plot(portfolio[,2:8])
plot(portfolio[, c(1,9)])


# Computer returns
portfolioReturn  <- returns0(portfolio, method = "simple")
portfolioReturn  <- xts(portfolioReturn, order.by = time(portfolio))

r  <- rnorm(500)


# create a model for the optimization
# Create the portfolio specification
model <- portfolio.spec(colnames(portfolioReturn[,3:8]))

# Add a full investment constraint such that the weights sum to 1
model <- add.constraint(portfolio = model, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
model <- add.constraint(portfolio = model, type = "long_only")

# Add an objective to maximize portfolio returns
model <- add.objective(portfolio = model, type = "return", name = "mean")

# Add an objective to minimize portfolio standard deviation
model <- add.objective(portfolio = model, type = "risk", name = "StdDev")

# Solve the optimization problem
optModel <- optimize.portfolio(portfolioReturn[,3:8], portfolio = model, trace = TRUE, 
                               optimize_method= "ROI", rp = r)


print(model)
# Extract the optimal weights
extractWeights(optModel)

# Chart the optimal weights
chart.Weights(optModel)

dim(portfolioReturn)

optReModel <- optimize.portfolio(portfolioReturn[,3:8], portfolio = model, optimize_method= "random",
                                 trace = TRUE, search_size = 1000, rebalance_on = "quarters", 
                                 training_period = 50, rolling_window = 50, rp = r)

# Extract the optimal weights
extractWeights(optReModel)

# Chart the optimal weights
chart.Weights(optReModel)

