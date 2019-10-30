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
plot(eop_cont, auto.legend = TRUE,legend.loc = "topleft",
         main = "Plot of Portfolio Returns with Constant Weights")

plot(eop_rebal, legend.loc = "topleft",  auto.legend = TRUE,
     main = "Plot of Portfolio Returns with Constant Weights")


plot(eop_cont, main = "Plot of Portfolio Returns with Constant Weights")
addLegend("topleft", on=1, 
          legend.names = names, 
          lty=rep(9,num), lwd=rep(3,num),
          col=seq(1,num,1))


# plot Amazon only
par(mfrow=c(2,1))
plot(eop_cont[,1],  legend.loc = "topleft",
     main = "Plot of Portfolio Returns with Constant Weights")
plot(eop_rebal[,1], legend.loc = "topleft",
     main = "Plot of Portfolio Returns with Rebalancing Weights")

dev.off()



# Calculate the SemiDeviation
SemiDeviation(benchmark)

# Calculate the value at risk
VaR(benchmark,p = 0.05)
VaR(benchmark,p = 0.025)

# Calculate the expected shortfall
ES(benchmark, p = 0.05)
ES(benchmark, p = 0.025)


# Table of drawdowns
table.Drawdowns(benchmark)

# Plot of drawdowns
chart.Drawdown(benchmark)


plot(eop_cont, main = "Plot of Portfolio Returns with Constant Weights")
addLegend("topleft", on=1, 
          legend.names = names, 
          lty=rep(9,num), lwd=rep(3,num),
          col=seq(1,num,1))



# Chart the correlations
chart.Correlation(portfolioReturn)

# chart rolling correlation between 2 assets
chart.RollingCorrelation(portfolioReturn, portfolioReturn, width = 66)





# Create a vector of returns 
means <- apply(portfolioReturn, 2, "mean")

# Create a vector of standard deviation
sds <- apply(portfolioReturn, 2, "sd")

# Create a scatter plot
plot(sds, means)
text(sds, means, labels = colnames(portfolioReturn), cex = 0.7)
abline(h = 0, lty = 3)





# Create volatility budget
final_weight <- eop_cont[dim(eop_cont)[1],]
budget_volatility <- StdDev(portfolioReturn, portfolio_method = "component", 
                     weights = final_weight)

# Make a table of weights and risk contribution
weights_Risk <- cbind(c(coredata(final_weight)), budget_volatility$contribution,
                          budget_volatility$pct_contrib_StdDev *1e2)
                          
colnames(weights_Risk) <- c("weights", "Vol. Contrib", "% vol, Cont")

# Print the table
print(weights_Risk)
print(vol_budget)




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



opt_port_returns <- as.xts(opt_port$px, order.by = index(portfolioReturn))
par(mfrow=c(2,1))
plot(portfolioRowMean, main="Average Portfolio Returns")
plot(opt_port_returns,col=2, main="Optimized Portfolio Returns")

dev.off()


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

