# Here we explore Garch-in-mean model using rugarch package
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(rugarch, quietly = T)
library(dplyr)

# download dataset
getSymbols("NYT", src="yahoo")

# select required feature
nyt <- NYT[,"NYT.Adjusted"]


# calculate the daily simple return
nytReturn <- CalculateReturns(nyt)[-1]
colnames(nytReturn) <- "Price"


# compute basic statistics
summary(nytReturn)
rskew <- skewness(nytReturn) # [1] 0.5283861
kurtosis(nytReturn)


# create model specs  
garchspec <- ugarchspec(mean.model = list(armaOrder=c(0,0), archm=TRUE, archpow=2),
                        variance.model = list(model="sGARCH"),
                        distribution.model = "sstd")

model1 <- ugarchfit(data=nytReturn, spec=garchspec)


coef(model1)