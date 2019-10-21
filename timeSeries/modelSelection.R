
###########################################################
# Here we explore Garch-in-mean model using rugarch package
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(rugarch, quietly = T)
library(dplyr, quietly = T)
library(glue, quietly = T)
library(forecast, quietly = T)


library(quantmod, quietly = T)
library(forecast, quietly = T)
library(lmtest, quietly = T)
library(xts, quietly = T)
library(aTSA, quietly = T)
library(glue, quietly = T)

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
dev.off()
par(mfrow=c(2,1))
par(mar = c(1,1,1,1), oma = c(1, 1, 1, 1))
plot(nytReturn, main="NYT Daily Returns")
plot(resid1, main="Residual Model 1")


# check correlations
acf(nytReturn, main="ACF NYT Daily Returns")
acf(abs(resid1), main="ACF Residual Model 1")


# Box test on absolute residuals 
Box.test(abs(resid1), 22, type = "Ljung-Box")
Box.test(abs(resid2), 22, type = "Ljung-Box")
Box.test(abs(resid3), 22, type = "Ljung-Box")$p.value


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


modelSelection(model1, model2)

