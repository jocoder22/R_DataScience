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
nytReturn$Price <- CalculateReturns(nyt)[-1]


 
# compute basic statistics
summary(nytReturn)
rskew <- skewness(nytReturn) # [1] 0.5283861
kurtosis(nytReturn)


# create model specs  
garchspec <- ugarchspec(mean.model = list(armaOrder=c(0,0), archm=TRUE, archpow=2),
                        variance.model = list(model="gjrGARCH"),
                        distribution.model = "sstd")

model1 <- ugarchfit(data=nytReturn, spec=garchspec)


# get the parameter coefficients
# the archm parameter coefficients is the increase in return per unit increase in risk
# archm quantify risk-reward trade off
round(coef(model1), 4)


# plot the estimated returns
plot(fitted(model1))


# $robust.matcoef
# $matcoef
# $residuals
round(model1@fit$robust.matcoef, 5)
round(model1@fit$matcoef, 5)
model1@fit$residuals # the same result as residuals(model1)




 # AR(1) model
arspec <- ugarchspec(mean.model = list(armaOrder=c(1,0)),
                     variance.model = list(model="gjrGARCH"),
                     distribution.model = "sstd")
model2 <- ugarchfit(data=nytReturn, spec=arspec)

round(coef(model2), 4)


# if ar1 = 0, then use constant mean: armOrder=c(0,0)
# if gamma1 = 0, there is no Garch-in-mean then use sGARCH
round(model2@fit$robust.matcoef, 5)
round(model2@fit$matcoef, 5)


