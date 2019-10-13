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

