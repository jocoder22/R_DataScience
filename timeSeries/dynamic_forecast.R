library(forecast)
library(tseries)


# Using the EuStockMarkets datasets
# plot the series
ap  <- EuStockMarkets

autoplot(ap, facets = TRUE)

auto.arima(ap)
