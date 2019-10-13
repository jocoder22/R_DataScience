library(timeSeries)

head(EuStockMarkets)
euro_diff  <- diff(EuStockMarkets)
is.ts(EuStockMarkets)


ts.plot(EuStockMarkets, col=4:7, ylab="Index Value", xlab="Year",
        main="Major European stock indexes")
legend("topleft", colnames(EuStockMarkets), col = 4:7,
       lty = 1, bty = "n")



# Compute returns and log returns
returns  <- diff(EuStockMarkets)
log_returns <- diff(log(EuStockMarkets))


# # Plot returns and log returns
par(mfrow=c(2,1))
ts.plot(log_returns, col=4:7, ylab="Index LOg REturns", xlab="Year",
        main="Log Returns of Major European stock indexes")
ts.plot(returns, col=4:7, ylab="Index Simple REturns", xlab="Year",
        main="Simple Returns of Major European stock indexes")


plot(log_returns, col="red", ylab="Index LOg REturns", xlab="Year",
        main="Log Returns of Major European stock indexes")


plot(returns, col=4:7, ylab="Index Simple REturns", xlab="Year",
        main="Simple Returns of Major European stock indexes")



# Plot graphs again
par(mfrow=c(1,1))
ts.plot(log_returns, col=4:7, ylab="Index LOg REturns", xlab="Year",
        main="Log Returns of Major European stock indexes")
legend("topleft", colnames(EuStockMarkets), col = 4:7,
       lty = 1, bty = "n")

ts.plot(returns, col=4:7, ylab="Index Simple REturns", xlab="Year",
        main="Simple Returns of Major European stock indexes")
legend("topleft", colnames(EuStockMarkets), col = 4:7,
       lty = 1, bty = "n")



# Compute means and standard deviations
# Compute means from log_returns
colMeans(log_returns)

# apply to calculate sample variance from log_returns
apply(log_returns, MARGIN = 2, FUN = var)

# apply to calculate standard deviation from log_returns
apply(log_returns, MARGIN = 2, FUN = sd)

# plot histogram of percent returns for each index
par(mfrow = c(2,2))
apply(log_returns, MARGIN = 2, FUN = hist, main = "", xlab = "Log Return")


# Display normal quantile plots of percent returns for each index
par(mfrow = c(2,2))
for(col in colnames(log_returns)){

  # apply(log_returns, MARGIN = 2, FUN = qqnorm)
  # qqline(log_returns)
  qqnorm(log_returns[,col], main = sprintf("Normal Q-Q Plot for %s", col))
  qqline(log_returns[,col])

  
}

par(mfrow=c(1,1))

