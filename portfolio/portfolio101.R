# Load the required packages
library(quantmod, quietly=T)
library(PerformanceAnalytics, quietly = TRUE)
library(ROI, quietly = TRUE)
library(timeSeries)
library(tseries)
library(PortfolioAnalytics)
library(tidyr)


# Download stocks
tickers = c('AMZN','AAPL','MSFT',"SPY", "AGG", "VNQ", "GSG","JPM", "^GSPC")
getSymbols(tickers, from='2007-01-03') 


# Merge them together
names = c('AMZN','AAPL','MSFT',"SPY","AGG", "VNQ", "GSG","JPM", "GSPC")
portfolio  <- Ad(merge(AMZN, AAPL, MSFT, SPY, AGG, VNQ, GSG, JPM, GSPC))
colnames(portfolio) <- names

# get summary statistics
summary(coredata(portfolio))
plot(portfolio[,2:8])
plot(portfolio[, c(1,9)])

# Obtain Treasury yield data
treasury10yr <- getSymbols(Symbols = "DGS10", src = "FRED", from='2007-01-03', auto.assign = FALSE)
plot(treasury10yr, main = "Ten year US Treasury bill yield")




# Computer returns
# portfolioReturn  <- returns0(portfolio, method = "simple")
# portfolioReturn  <- xts(portfolioReturn, order.by = time(portfolio))
# 
# r  <- rnorm(500)
portfolioReturn <- Return.calculate(portfolio)[-1]

# create a model for the optimization
# Create the portfolio specification
model <- PortfolioAnalytics::portfolio.spec(colnames(portfolioReturn[,3:8]))

# Add a full investment constraint such that the weights sum to 1
model <- PortfolioAnalytics::add.constraint(portfolio = model, type = "full_investment")

# Add a long only constraint such that the weight of an asset is between 0 and 1
model <- PortfolioAnalytics::add.constraint(portfolio = model, type = "long_only")

# Add an objective to maximize portfolio returns
model <- PortfolioAnalytics::add.objective(portfolio = model, type = "return", name = "mean")

# Add an objective to minimize portfolio standard deviation
model <- PortfolioAnalytics::add.objective(portfolio = model, type = "risk", name = "StdDev")

# Solve the optimization problem
optModel <- PortfolioAnalytics::optimize.portfolio(portfolioReturn[,3:8], portfolio = model, trace = TRUE, 
                               optimize_method= "ROI", rp = r)


print(model)
# Extract the optimal weights
extractWeights(optModel)

# Chart the optimal weights
chart.Weights(optModel)

dim(portfolioReturn)


# new_portfolioReturn <- merge(portfolioReturn, treasury10yr, all = FALSE) %>% na.locf()
portfolioReturn$R <- 0.0
optReModel <- optimize.portfolio(portfolioReturn[,3:8], portfolio = model, optimize_method= "random",
                                 trace = TRUE, search_size = 1000, rebalance_on = "quarters", 
                                 training_period = 50, rolling_window = 50, rp = portfolioReturn$R)




# Extract the optimal weights
extractWeights(optReModel)

# Chart the optimal weights
chart.Weights(optReModel)



plot.zoo(portfolio, plot.type = "single", col = 1:11)
legend(julian(x = as.Date("2009-01-01")), y = 3000, legend = names(portfolioReturn)[1:9], fill = 1:9)


plot.zoo(portfolioReturn["2008/2012",1:4], type = "h")


apply.quarterly(portfolioReturn, colSums)


# calculate skewness and kurtosis of portfolio assets 
sk <- apply(portfolioReturn[, 1:9], 2, skewness)
kurt <-  apply(portfolioReturn[, 1:9], 2, kurtosis)

# plot their skewness and kurtosis
plot(sk, kurt, type = "n")
text(sk, kurt, names(sk), cex = 0.6)


# do formal normal test
apply(portfolioReturn[,1:9], 2, jarque.bera.test)

