# Load the required packages
library(quantmod, quietly=T)
library(PerformanceAnalytics, quietly = TRUE)
library(ROI, quietly = TRUE)
library(timeSeries)
library(tseries)
library(xts)


# Set working directory
setwd("D:/R_DataScience/portfolio")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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

# save portofolio returns
saveRDS(portfolioReturn, "Returns.rds")


patht <- "D:\\PythonDataScience\\importingData\\localData"
write.csv(portfolioReturn, file.path(patht, "portfolios.csv"), row.names = FALSE)

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
plot(opt_port_returns, col=2, main="Optimized Portfolio Returns")

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



# plot porfolio gross returns
# investing one dollar
grossR <- 1 + portfolioReturn


# Future returns 
futureR <- cumprod(grossR)

plot(futureR,legend.loc = "topleft", main="Future Returns")


# compute the optimium portfolio
opt_port <- portfolio.optim(portfolioReturn, shorts = TRUE)

# compute optimized return
opt_port_returns <- as.xts(opt_port$px, order.by = index(portfolioReturn))


# Future returns 
future_m <- cumprod(1 + portfolioRowMean)
future_opt <- cumprod(1 + opt_port_returns)

# compare with mean portfolio returns
par(mfrow=c(2,1))
future_merge <- merge(future_m, future_opt)
colnames(future_merge) <- c("Future Portofolio Mean Returns", "Future Optimized Portfolio Returns")
add_lowPer <- merge(future_merge, futureR[, 6:9])
plot(add_lowPer, main="Mean Portofolio Returns vs. Optimized Portfolio Returns",
     legend.loc = "topleft")

# add amazon stock returns
add_amazon <- merge(future_merge, futureR[, "AMZN"])
plot(add_amazon, main="Mean Portofolio Returns vs. Optimized Portfolio Returns",
     legend.loc = "topleft")

par(mfrow=c(1,1))
