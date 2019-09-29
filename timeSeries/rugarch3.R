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



par(mfrow = c(2,1))
par(mar = c(1,1,1,1), oma = c(1, 1, 1, 1))






library(fGarch)
spec <- garchSpec(model=list(omega=0.001, ar=c(0.5, 0.2, -0.1),
                             alpha=c(0.3, 0.2),
                             beta=c(0.2, 0.1)))

process <- garchSim(spec=spec, n=50000)
plot(process)
plot(diff(process))

acf2(process)


model1 <- garchFit(formula = ~ garch(3,0), data = process, trace = F)
summary(model1)


model2 <- garchFit(formula = ~ arma(3,0) + garch(2,2), data = process, trace = F)
summary(model2)
