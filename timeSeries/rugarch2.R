# Here we explore the rugarch package
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(rugarch, quietly = T)
library(dplyr)

# download dataset
getSymbols("NYT", src="yahoo")

# select required feature
nyt <- NYT[,"NYT.Adjusted"]


# calculate the daily simple return
nytReturn$Price<- CalculateReturns(nyt)[-1]



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

# variance targeting means the variance is mean-reverting 
# around the sample sd
tspec <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                    distribution.model ="sstd", 
                    variance.model=list(model="sGARCH", 
                                        variance.targeting=TRUE))
# setfixed(tspec) <- list(skew =rskew)
model <- ugarchfit(data=nytReturn, spec=garchspec)
model2 <- ugarchfit(data=nytReturn, spec=tspec)


# check mean-reverting
all.equal(uncvariance(model2), sd(nytReturn)^2, tol=1e-03)
uncvariance(model2) - sd(nytReturn)^2

modelvol$Sigma = sigma(model2)
modelvol$hhh <- sd(nytReturn)


# plot mean-reverting volatility
dev.off()
plot(modelvol[, "Sigma"], main="New York Times Returns GARCH Volatility")
lines(modelvol[, "hhh"], col="red")



# Compute estimated coefficients
cofficients <- coef(model)
m1 <- data.frame(coef(model))
m2 <- data.frame(coef(model2))

# make index a column
m1<- m1 %>% rownames_to_column("Parameters")
m2<- m2 %>% rownames_to_column("Parameters")


# merge the results
merge(m2, m1, by.x ="Parameters", by.y= "Parameters", all = TRUE)
merge(m2, m1, all.x = TRUE, sort = FALSE)


coef(model2)
sumAlphaBeta <- cofficients[3] + cofficients[4]
sumAlphaBeta # 0.9843281, less than one, there the returns are mean reverting


# Compute the standardized returns
dev.off()
par(mfrow = c(2,1))
stdret <- residuals(model, standardize = TRUE)
stdret2 <- residuals(model2, standardize = TRUE)


# plot the histograms
chart.Histogram(stdret, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals under Normal Distribution")

chart.Histogram(stdret2, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals under skewed t-Distribution")



# Need some data to play with
df1 <- data.frame(LETTERS, dfindex = 1:26)
df2 <- data.frame(letters, dfindex = c(1:10,15,20,22:35))

# INNER JOIN: returns rows when there is a match in both tables.
merge(df1, df2, by="dfindex")


x <- data.frame(k1 = c(NA,NA,3,4,5), k2 = c(1,NA,NA,4,5), data = 1:5)
y <- data.frame(k1 = c(NA,2,NA,4,5), k2 = c(NA,NA,3,4,5), data = 1:5)
merge(x, y, by = c("k1","k2")) # NA's match
merge(x, y, by = "k1") # NA's match, so 6 rows
merge(x, y, by = "k2", incomparables = NA) # 2 rows


