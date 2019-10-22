library(forecast)
library(tseries)


# Using the EuStockMarkets datasets
# plot the series
eu  <- EuStockMarkets

autoplot(eu, facets = TRUE)


# fit the model with xreg
model1  <- auto.arima(eu[, "DAX"], xreg = eu[, c(2:4)], stationary = TRUE)
