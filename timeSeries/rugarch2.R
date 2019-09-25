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
plot(diff(nyt), main="First Differencing of NYT Close Price")
plot(nytReturn)
acf2(nytReturn)
acf2(diff(nyt))

