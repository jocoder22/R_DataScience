library(timeSeries)
library(astsa)
library(quantmod)


# Download stocks
symbols <- c("AMZN", "AAPL", "TSLA", "MSFT")

getSymbols(symbols)
amazon  <- getSymbols("AMZN", auto.assign = F)$`AMZN.Adjusted`
getSymbols(symbols, from="2016-10-29")

stocks <- Ad(merge(AMZN, AAPL, TSLA, MSFT))
returns  <- returns0(EuStockMarkets)
len <- length(EuStockMarkets[, 1]) 



# Compute correlations with lag 1
cor(EuStockMarkets[-len, 1], EuStockMarkets[-1, 1])


# Compute correlations with lag 1
cor(EuStockMarkets[-((len - 1):len), 1], EuStockMarkets[-(1:2), 1])

# use acf to compute the correlations for many lags
acf(EuStockMarkets, lag.max = 6, type = "correlation", plot = FALSE)
acf(amazon, lag.max = 6, plot = FALSE)
acf(stocks, lag.max = 6, plot = FALSE)

