library(Quandl)
library(PerformanceAnalytics)

data1 <- Quandl("FRED/GDP", type="ts")
head(data1)

data2 <- Quandl("WIKI/FB", type = "xts")
head(data)

plot(data2$`Adj. Close`)

