#########################################################################################
#########################################################################################
########## Analysis plan ################################################################
# Load necessary packages
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(forecast, quietly = T)
library(xts, quietly = T)
library(FinTS, quietly = T)
library(fGarch, quietly = T)
library(rugarch, quietly = T)
library(car)
library(nortest)


filepath <- getwd()
sink(file.path(filepath, "modules", "submission22.doc"),  
     append=FALSE, split=FALSE, type = c("output", "message"))


# Data source: Yahoo finance
# Period considered in the analysis: January 2010 -  September 27, 2019
# Frequency: Daily

# Download datasets
startDate <- "2010-01-01"
endDate <- "2019-09-27"


getSymbols("AAPL", src="yahoo", from=startDate, to=endDate)
apple <- AAPL[, "AAPL.Adjusted"] 


# Calculate Apple returns
appleReturns <- CalculateReturns(apple)[-1]
colnames(appleReturns) <- "Returns"


# plot the graphs
par(mfrow = c(2,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(apple, main="Apple stock Adjusted Close price")
plot(appleReturns, main="Apple stock Returns")



# compute basic statistics
returnMean <- mean(appleReturns)
returnVolatility <- sd(appleReturns)
returnSkewness <- skewness(appleReturns)
returnKurtosis <- kurtosis(appleReturns)



# Perform adf test and Box-Test on Apple Returns
tseries::adf.test(appleReturns)
Box.test(appleReturns, type="Ljung-Box", lag=12)



# Visualized distribution of rolling window volatility
par(mfrow = c(3,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
rollweekly <- rollapply(appleReturns, width = 5 , FUN = "sd.annualized")
rollmonthly <- rollapply(appleReturns, width = 22 , FUN = "sd.annualized")
rollquarterly <- rollapply(appleReturns, width = 65 , FUN = "sd.annualized")


plot(rollweekly, main="Apple Stock Weekly Returns Volatility")
plot(rollmonthly, main="Apple Stock Monthly Returns Volatility")
plot(rollquarterly, main="Apple Stock Quarterly Returns Volatility")



# compute formal statistical test: Autocorrelation and ARCH test
appleReturnsSquared <- appleReturns^2
Box.test(appleReturnsSquared, type = "Ljung-Box", lag = 30)
ArchTest(appleReturns)



#########################################################
# plot acf and pacf of apple returns and squared returns
par(mfrow = c(2,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
acf2(appleReturns, main=" Apple Stock Returns ")
acf2(appleReturnsSquared, main=" Squared Apple Stock Returns ")



# plot the qqplots to investigate the model distribution
qqnormPlot(appleReturns, title=FALSE, main=" NORM QQ PLOT: Apple Returns")
qqnormPlot(na.omit(rollweekly), title=FALSE,main="NORM QQ PLOT: Apple Returns Volatility")
qqghtPlot(appleReturns, pch=14, sub="GHT QQ PLOT: Apple Returns")
qqghtPlot(na.omit(rollweekly), pch=14, sub="GHT QQ PLOT: Apple Returns Volatility")



# more plot to investigate the underlying distribution
par(mfrow = c(2,1), mar = c(4,4,3,3), oma = c(1, 1, 1, 1))
applenorm<-(appleReturns-mean(appleReturns))/sd(appleReturns) 
qqnorm(applenorm) 
abline(0,1) 

chart.Histogram(z.norm, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Returns under Normal Distribution")



# statistical test for normality
sf.test(coredata(appleReturns))
ad.test(coredata(appleReturns))
cvm.test(coredata(appleReturns))
lillie.test(coredata(appleReturns))
pearson.test(coredata(appleReturns))

# investigate the armaOrder with auto.arima() and autoarfima()
auto.arima(appleReturns)
# Series: appleReturns 
# ARIMA(0,0,0) with non-zero mean

autoarfima(appleReturns, ar.max = 4, ma.max=4, criterion = "HQIC", method="full")
# Mean Model	: ARFIMA(3,0,3)
# Distribution	: norm 



# create model specs  
garchspec1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "norm")

garchspec2 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "std")

garchspec3 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "sstd")

garchspec4 <- ugarchspec(mean.model = list(armaOrder=c(0,0), archm=TRUE, archpow=2),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "sstd")

garchspec5 <- ugarchspec(mean.model = list(armaOrder=c(0,0), archm=TRUE, archpow=2),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "ghyp")

model1 <- ugarchfit(data=appleReturns, spec=garchspec1)
model2 <- ugarchfit(data=appleReturns, spec=garchspec2)
model3 <- ugarchfit(data=appleReturns, spec=garchspec3)
model4 <- ugarchfit(data=appleReturns, spec=garchspec4)
model5 <- ugarchfit(data=appleReturns, spec=garchspec5)



# calculate the standard residuals
resid1 <- residuals(model1, standardize = TRUE)
resid2 <- residuals(model2, standardize = TRUE)
resid3 <- residuals(model3, standardize = TRUE)
resid4 <- residuals(model4, standardize = TRUE)
resid5 <- residuals(model5, standardize = TRUE)


# plot the returns and residuals
dev.off()
par(mfrow=c(2,1))
par(mar = c(1,1,1,1), oma = c(1, 1, 1, 1))
plot(appleReturns, main="Apple Daily Returns")
plot(resid1, main="Residual Model 1")


# check correlations
acf(appleReturns, main="ACF Apple Daily Returns")
acf(abs(resid1), main="ACF Residual Model 1")


# Box test on absolute residuals 
Box.test(abs(resid1), 22, type = "Ljung-Box")
Box.test(abs(resid2), 22, type = "Ljung-Box")
Box.test(abs(resid3), 22, type = "Ljung-Box")
# Box.test(abs(resid3), 22, type = "Ljung-Box")$p.value
Box.test(abs(resid4), 22, type = "Ljung-Box")
Box.test(abs(resid5), 22, type = "Ljung-Box")


modelSelection <- function(m1, m2){
  res1 <- residuals(m1);
  res2 <- residuals(m2);
  rms1 <- mean(res1^2); 
  rms2 <- mean(res2^2);
  
  lik1 <- likelihood(m1);
  lik2 <- likelihood(m2);
  
  n1 <- length(coef(m1));
  n2 <- length(coef(m2));
  
  aic1 <- infocriteria(m1)["Akaike",];
  aic2 <- infocriteria(m2)["Akaike",];
  
  bic1 <- infocriteria(m1)["Bayes",];
  bic2 <- infocriteria(m2)["Bayes",];
  
  m11 <- deparse(substitute(m1));
  m22 <- deparse(substitute(m2));
  
  resid1 <- residuals(m1, standardize = TRUE)
  resid2 <- residuals(m2, standardize = TRUE)
  bmol1 <- Box.test(abs(resid1), 22, type = "Ljung-Box")$p.value;
  bmol2 <- Box.test(abs(resid2), 22, type = "Ljung-Box")$p.value;
  
  cat(sprintf("Ljung Box p-value for %s is %.3f\n", m11, bmol1))
  cat(sprintf("Ljung Box p-value for %s is %.3f\n", m22, bmol2))
  cat("Based on RMSE: ")
  if (rms1 > rms2){
    cat(sprintf("%s with %s parameter is better\n", m22, n2));
  }
  else cat(sprintf("%s with %s parameter is better\n", m11, n1));
  
  
  cat("Based on Likelihood: ")
  if (lik1 > lik2){
    cat(sprintf("%s with %s parameter is better\n", m11, n1));
  }
  else cat(sprintf("%s with %s parameter is better\n", m22, n2));
  
  cat("Based on Akaike Criteria (AIC): ")
  if (aic2 > aic1){
    cat(sprintf("%s with %s parameter is better\n", m11, n1));
  }
  else cat(sprintf("%s with %s parameter is better\n", m22, n2));
  
  
  cat("Based on Bayes Creteria (BIC): ")
  if (bic2 > bic1){
    cat(sprintf("%s with %s parameter is better\n", m11, n1));
  }
  else cat(sprintf("%s with %s parameter is better\n", m22, n2));
  
}

modelSelection(model1, model3)
modelSelection(model2, model3)
modelSelection(model4, model3)
modelSelection(model5, model3)
# model3 is the winner here!



# model specification
garchspecAR22 <- ugarchspec(mean.model = list(armaOrder=c(2,2)),
                           variance.model = list(model="gjrGARCH"),
                           distribution.model = "sstd")
garchspecAR23 <- ugarchspec(mean.model = list(armaOrder=c(2,3)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")
garchspecAR24 <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")
garchspecAR32 <- ugarchspec(mean.model = list(armaOrder=c(3,2)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")
garchspecAR33 <- ugarchspec(mean.model = list(armaOrder=c(3,3)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")
garchspecAR34 <- ugarchspec(mean.model = list(armaOrder=c(3,4)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")


ar22 <- ugarchfit(data=appleReturns, spec=garchspecAR22)
ar23 <- ugarchfit(data=appleReturns, spec=garchspecAR23)
ar24 <- ugarchfit(data=appleReturns, spec=garchspecAR24)
ar32 <- ugarchfit(data=appleReturns, spec=garchspecAR32)
ar33 <- ugarchfit(data=appleReturns, spec=garchspecAR33)
ar34 <- ugarchfit(data=appleReturns, spec=garchspecAR34)

modelSelection(ar22, ar32)
modelSelection(ar23, ar32)
modelSelection(ar24, ar32)
modelSelection(ar33, ar32)
modelSelection(ar34, ar32)
modelSelection(ar32, model3)

# armOrder(3,2) is better


garchspecA0 <- ugarchspec(mean.model = list(armaOrder=c(2,3)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd", fixed.pars = list(alpha1=0))
A0 <- ugarchfit(data=appleReturns, spec=garchspecA0)

garchspecA02 <- ugarchspec(mean.model = list(armaOrder=c(3,2)),
                          variance.model = list(model="gjrGARCH"),
                          distribution.model = "sstd", fixed.pars = list(alpha1=0))
A02 <- ugarchfit(data=appleReturns, spec=garchspecA0)

modelSelection(A02, ar32)
modelSelection(A0, A02)
# A02 better




# fit different variance model
garchspecI<- ugarchspec(mean.model = list(armaOrder=c(2,3)),
                            variance.model = list(model="iGARCH"),
                            distribution.model = "sstd")
garchspecS <- ugarchspec(mean.model = list(armaOrder=c(2,3)),
                            variance.model = list(model="sGARCH"),
                            distribution.model = "sstd")
garchspecE <- ugarchspec(mean.model = list(armaOrder=c(2,3)),
                            variance.model = list(model="eGARCH"),
                            distribution.model = "sstd")
garchspecT <- ugarchspec(mean.model = list(armaOrder=c(3,2)),
                            variance.model = list(model="fGARCH", submodel="TGARCH"),
                            distribution.model = "sstd")

I <- ugarchfit(data=appleReturns, spec=garchspecI)
S <- ugarchfit(data=appleReturns, spec=garchspecS)
E <- ugarchfit(data=appleReturns, spec=garchspecE)
TT <- ugarchfit(data=appleReturns, spec=garchspecT)

modelSelection(E, TT)
modelSelection(S, TT)
modelSelection(I, TT)
# TT is better!
modelSelection(A02, TT)
modelSelection(A0, TT)
modelSelection(model3, TT)

# E, TT, A0, A02


# Backtesting

modelBackTesting <- function(mod1, mod2, ddata){
  
  start <- length(ddata) - 1000;
  
  params <- list(data=ddata, n.start = start,
                 refit.window = "moving", refit.every = 200)
  
  result1 <- do.call(ugarchroll, c(mod1, params));
  result2 <- do.call(ugarchroll, c(mod2, params));
  
  
  pred1 <- as.data.frame(result1);
  pred2 <- as.data.frame(result2);
  
  
  e1 <- mean((pred1$Realized - pred1$Mu)^2);
  e2 <- mean((pred2$Realized - pred2$Mu)^2);
  
  m11 <- deparse(substitute(mod1));
  m22 <- deparse(substitute(mod2));
  
  
  if (e1 > e2){
    cat(sprintf("%s is the better model.\n", m22));
  }
  else cat(sprintf("%s is the better model.\n", m11));
  
  cat(sprintf("%s RMSE: %s\n", m11, e1));
  cat(sprintf("%s RMSE: %s\n", m22, e2))
}


# garchspecE,  garchspecT, garchspecA02, garchspecA0
modelBackTesting(garchspecA02, garchspecA0, appleReturns)
modelBackTesting(garchspecA02, garchspecT, appleReturns)
modelBackTesting(garchspecA02, garchspecE, appleReturns)
modelBackTesting(garchspecA02, garchspec3, appleReturns)
 

# show and plot the preferred model
show(A02)
A02
par(mfrow = c(2,2), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
# plot(A02)
for(x in c(1:12)){plot(A02, which=x)} 

# forecast next 22 apple daily returns
par(mfrow = c(2,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
forca <- ugarchforecast(A02, n.ahead = 22)
fitted(forca)
for(x in c(1,3)){plot(forca, which=x)} 





fit = ugarchfit(data=appleReturns, spec=garchspecA0, out.sample=100)
forc = ugarchforecast(fit, n.ahead=100)
r1 <- round(fpm(forc), 8)


fit2 = ugarchfit(data=appleReturns, spec=garchspecA02, out.sample=100)
forc2 = ugarchforecast(fit2, n.ahead=100)
r2 <- round(fpm(forc2), 8)


fit4 = ugarchfit(data=appleReturns, spec=garchspecT, out.sample=100)
forc4 = ugarchforecast(fit4, n.ahead=100)
r4 <- round(fpm(forc4), 8)

modelPerf <- data.frame(cbind(r1,r2,r4))
names(modelPerf) <- c("Model 1", "Model 2", "Model 3")
modelPerf


sink()



 