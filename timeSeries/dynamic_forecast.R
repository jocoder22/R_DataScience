library(forecast)
library(tseries)
library(rugarch)
library(xts)


# Using the EuStockMarkets datasets
# plot the series
Start = c(1991, 130) 
End = c(1998, 169) 
Frequency = 260 


# dates <- seq(as.Date("2016-01-01"), length = 5, by = "days")
# bday <- as.POSIXct("1899-05-08")
# dates <- as.Date("2016-01-01") + 0:4
# Convert to zoo object
eu  <- zoo(EuStockMarkets)
autoplot(eu, facets = FALSE)
autoplot(eu, facets = TRUE)
autoplot(eu)

index(eu)
# convert to xts
# dates <- seq(as.Date("1991-05-10"), length = 1860, by = "days")
dates <- as.Date("1991-05-10") + 1:1860
eux  <- xts(eu,  order.by = dates)
plot(eux[, "CAC"], main = "CAC Index")
plot(eux, main = "European Index")
plot(eux, multi.panel = TRUE, main = "European Index")

# fit the model with xreg, multivariate ARIMA
# auto.arima(eu[, "DAX"], order = c(0,0,5), xreg = eu[, c(2:4)])
model1  <- Arima(eu[, "DAX"], order = c(0,0,5), xreg = eu[, c(2:4)])





