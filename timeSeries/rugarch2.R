# Here we explore the rugarch package
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(rugarch, quietly = T)

# download dataset
getSymbols("New York Times", src="yahoo")

# select required feature
nyt <- New York Times[,"New York Times.Adjusted"]


# calculate the daily simple return
nytReturn <- CalculateReturns(nyt)[-1]
colnames(nytReturn) <- "Price"


# compute basic statistics
summary(nytReturn)
rskew <- skewness(nytReturn) # [1] 0.5283861
kurtosis(nytReturn)


# plot the graphs 
plot(nyt,  main=" New York Times Close Adjusted Price")
plot(diff(nyt), main="First Differencing of New York Times Close Adjusted Price")
plot(nytReturn,  main=" New York Times Returns")


# plot acf and pacf
acf2(nyt, main=" New York Times Close Adjusted Price")
acf2(nytReturn, main=" New York Times Returns")
acf2(diff(nyt),  main="First Differencing of New York Times Close Adjusted Price")


# create GARCH model
# create model specs  
garchspec <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                    variance.model = list(model="sGARCH"),
                    distribution.model = "norm")

tspec <- ugarchspec(distribution.model ="sstd", 
                    variance.model=list(variance.targeting=TRUE))
setfixed(tspec) <- list(skew =rskew)
tspec
model <- ugarchfit(data=nytReturn, spec=garchspec)
model2 <- ugarchfit(data=nytReturn, spec=tspec)

# Compute estimated coefficients
cofficients <- coef(model)
coef(model)
coef(model2)
sumAlphaBeta <- cofficients[3] + cofficients[4]
sumAlphaBeta # 0.9843281, less than one, there the returns are mean reverting


# Compute the standardized returns
dev.off()
par(mfrow = c(2,1))
stdret <- residuals(model, standardize = TRUE)
stdret2 <- residuals(model2, standardize = TRUE)
chart.Histogram(stdret, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals under Normal Distribution")

chart.Histogram(stdret2, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals under skewed t-Distribution")
