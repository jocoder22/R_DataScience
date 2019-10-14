library(forecast)
library(quantmod)
library(Quandl)


# set the dates
startdate = "1946-01-01"
enddate = "2018-12-31"

# download gdp data
gdp <- Quandl("FRED/GDP", start_date=startdate, type="ts")


# Split data into training set
trainsize  <- as.integer((length(gdp) * 0.8))
atrain  <- subset(gdp, end = trainsize)
h1  <- as.integer(length(gdp) - trainsize)


# Build the models
model1  <- naive(atrain, h = h1)
model2  <-  snaive(atrain, h = h1)
model3 <- tsCV(atrain, forecastfunction = naive, h = h1)


# Check accuracy
accuracy(model1, gdp)["Test set", "MAPE"]
accuracy(model2, gdp)["Test set", "MAPE"]
accuracy(model3, gdp)["Test set", "MAPE"]







apple  <- getSymbols("AAPL", auto.assign = FALSE)$`AAPL.Adjusted`
amazon  <- getSymbols("AMZN", auto.assign = F)$`AMZN.Adjusted`


apple <- Quandl("WIKI/AAPL", type = "ts")
amazon <- Quandl("WIKI/AMZN", type = "ts")
facebook <- Quandl("WIKI/FB", type = "ts")
head(facebook)

amatrain  <- window(amazon, end = "2019-01-10")
h2  <- as.integer(length(amazon) - length(amatrain))
