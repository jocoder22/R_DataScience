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







# apple  <- getSymbols("AAPL", auto.assign = FALSE)$`AAPL.Adjusted`
# amazon  <- getSymbols("AMZN", auto.assign = F)$`AMZN.Adjusted`


apple <- Quandl("WIKI/AAPL", collapse = "monthly", type = "ts")

facebook <- Quandl("WIKI/FB", collapse = "monthly", type = "xts")
head(facebook)




amazon <- Quandl("WIKI/AMZN.11", collapse = "monthly", type = "xts")
head(amazon)
amatrain  <- window(amazon, start = "1997-05-01", end = "2017-01-01")
head(amatrain)
h2  <- length(amazon) - length(amatrain)


model2  <-  tsCV(amatrain, forecastfunction = snaive, h = h2)
model3 <- tsCV(amatrain, forecastfunction = naive, h = h2)


# Check accuracy
accuracy(model1, amazon)["Test set", "MAPE"]
accuracy(model2, amazon)["Test set", "MAPE"]
accuracy(model3, amazon)["Test set", "MAPE"]
