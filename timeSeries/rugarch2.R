# Here we explore the rugarch package
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(rugarch, quietly = T)

# download dataset
getSymbols("NYT", src="yahoo")

# select required feature
nyt <- NYT[,"NYT.Adjusted"]


# calculate the daily simple return
nytReturn <- CalculateReturns(nyt)[-1]
colnames(nytReturn) <- "Price"


# compute basic statistics
summary(nytReturn)
skewness(nytReturn) # [1] 0.5283861
kurtosis(nytReturn)


# plot the graphs 
plot(nyt,  main=" NYT Close Price")
plot(diff(nyt), main="First Differencing of NYT Close Price")
plot(nytReturn,  main=" NYT Returns")


# plot acf and pacf
acf2(nyt, main=" NYT Close Price")
acf2(nytReturn, main=" NYT Returns")
acf2(diff(nyt),  main="First Differencing of NYT Close Price")

