library(forecast)
library(quantmod)

apple  <- getSymbols("AAPL", auto.assign = FALSE)$`AAPL.Adjusted`
amazon  <- getSymbols("AMZN", auto.assign = F)$`AMZN.Adjusted`


model1  <- naive(apple)
model2  <-  snaive(apple)
model3 <- tsCV(apple, forecastfunction = naive, h = 8)