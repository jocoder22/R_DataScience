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


# compute the average returns
row_mean <- apply(portfolioReturn, 1, mean)
portfolioRowMean <- as.xts(row_mean, order.by = index(portfolioReturn))
colnames(portfolioRowMean) <- "RowMeans"
plot(portfolioRowMean, legend.loc = "topright")


# compute the optimium portfolio
opt_port <- portfolio.optim(portfolioReturn)
opt_weights <- opt_port$pw

# Assign asset names
names(opt_weights) <- colnames(portfolioReturn)

# Bar plot of opt_weights
barplot(opt_weights)


# compare with mean portfolio returns
opt_port_returns <- as.xts(opt_port$px, order.by = index(portfolioReturn))
par(mfrow=c(2,1))
plot(portfolioRowMean, main="Average Portfolio Returns")
plot(opt_port_returns,col=2, main="Optimized Portfolio Returns")

par(mfrow=c(1,1))



# Changing the target returns 
# Create portfolio with target return of average returns 
portfolio_mean <- portfolio.optim(portfolioReturn, pm = mean(portfolioReturn))

# Create portfolio with target return 10% greater than average returns
portfolio_10 <- portfolio.optim(portfolioReturn, pm = 1.1 * mean(portfolioReturn))

# Print the standard deviations of both portfolios
portfolio_mean$ps
portfolio_10$ps


# Calculate the proportion increase in standard deviation
(portfolio_10$ps - portfolio_mean$ps) / (portfolio_mean$ps)


# Measure the effects of changes in max weights 
# Create vectors of maximum weights
max_weights100 <- rep(1, ncol(portfolioReturn))
max_weights50 <- rep(0.5, ncol(portfolioReturn))
max_weights25 <- rep(0.23, ncol(portfolioReturn))

# Create an optimum portfolio with max weights of 100%
optimized1 <- portfolio.optim(portfolioReturn, reshigh = max_weights100)

# Create an optimum portfolio with max weights of 50%
optimized2 <- portfolio.optim(portfolioReturn, reshigh = max_weights50)

# Create an optimum portfolio with max weights of 25%
optimized3 <- portfolio.optim(portfolioReturn, reshigh = max_weights25)

# Calculate how many assets have a weight that is greater than 1% for each portfolio
sum(optimized1$pw > .01)
sum(optimized2$pw > .01)
sum(optimized3$pw > .01)


# Print portfolio volatilites 
optimized1$ps
optimized2$ps
optimized3$ps
