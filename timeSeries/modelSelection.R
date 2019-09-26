###########################################################
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
nytReturn <- na.omit(CalculateReturns(nyt))
colnames(nytReturn) <- "Price"
head(nytReturn)


# create model specs  
garchspec1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                        variance.model = list(model="sGARCH"),
                        distribution.model = "norm")

garchspec2 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "sstd")

garchspec3 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "sstd")

garchspec4 <- ugarchspec(mean.model = list(armaOrder=c(0,0), archm=TRUE, archpow=2),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "sstd")

model1 <- ugarchfit(data=nytReturn, spec=garchspec1)
model2 <- ugarchfit(data=nytReturn, spec=garchspec2)
model3 <- ugarchfit(data=nytReturn, spec=garchspec3)
model4 <- ugarchfit(data=nytReturn, spec=garchspec4)


# Compare the volatility of the unrestricted and restriced GARCH models
plotvol <- plot(abs(nytReturn), auto.legend = TRUE,col = "grey", main="Volatility of Unrestricted and Restriced GARCH models")
plotvol <- addSeries(sigma(model1), col = "black", lwd = 4, on=1 )
plotvol <- addSeries(sigma(model2), col = "red", on=1)
plotvol <- addLegend("topleft", on=1, lty=c(3,3,3), lwd=c(3,3,3),cex=2,
                     legend.names = c("NYT Return", "Model 1", "Model 2"), 
                     col=c("grey", "black", "red"))
plotvol


# calculate the standard residuals
resid1 <- residuals(model1, standardize = TRUE)
resid2 <- residuals(model2, standardize = TRUE)
resid3 <- residuals(model3, standardize = TRUE)
resid4 <- residuals(model4, standardize = TRUE)


# plot the returns and residuals
par(mfrow=c(2,1))
plot(nytReturn, main="NYT Daily Returns")
plot(resid1, main="Residual Model 1")


acf2(resid1)
