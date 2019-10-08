#########################################################################################
#########################################################################################
##########            Analysis plan                  ####################################
#########################################################################################



######################################################################
######################################################################
######                                                  ##############
######    section 3.2.1.1 and 3.2.1.1                   ##############
######                                                  ##############
######################################################################
######################################################################
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
sink(file.path(filepath, "modules", "submission222.doc"),  
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

head(appleReturns)
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

autoarfima(appleReturns, ar.max = 2, ma.max=2, distribution.model="sstd",
           criterion = "HQIC", method="full")
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
    cat(sprintf("%s with %s parameters is better\n", m22, n2));
  }
  else cat(sprintf("%s with %s parameters is better\n", m11, n1));
  
  
  cat("Based on Likelihood: ")
  if (lik1 > lik2){
    cat(sprintf("%s with %s parameters is better\n", m11, n1));
  }
  else cat(sprintf("%s with %s parameters is better\n", m22, n2));
  
  cat("Based on Akaike Criteria (AIC): ")
  if (aic2 > aic1){
    cat(sprintf("%s with %s parameters is better\n", m11, n1));
  }
  else cat(sprintf("%s with %s parameters is better\n", m22, n2));
  
  
  cat("Based on Bayes Criteria (BIC): ")
  if (bic2 > bic1){
    cat(sprintf("%s with %s parameters is better\n", m11, n1));
  }
  else cat(sprintf("%s with %s parameters is better\n", m22, n2));
  
  cat("\n\n\n");
  
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



modelSelection(ar24, ar33)
modelSelection(ar34, ar24)
modelSelection(ar24, ar33)


# armOrder(2,4) is better


garchspecA0 <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                          variance.model = list(model="gjrGARCH"),
                          distribution.model = "std")
A0 <- ugarchfit(data=appleReturns, spec=garchspecA0)
A0


modelSelection(A0, ar24)
# A0 better
A0
ar24



# fit different variance model
garchspecI<- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                        variance.model = list(model="iGARCH"),
                        distribution.model = "std")
garchspecS <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "std")
garchspecE <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                         variance.model = list(model="eGARCH"),
                         distribution.model = "std")
garchspecT <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                         variance.model = list(model="fGARCH", submodel="TGARCH"),
                         distribution.model = "std")


I <- ugarchfit(data=appleReturns, spec=garchspecI)
S <- ugarchfit(data=appleReturns, spec=garchspecS)
E <- ugarchfit(data=appleReturns, spec=garchspecE)
TT <- ugarchfit(data=appleReturns, spec=garchspecT)

modelSelection(E, TT)
modelSelection(S, TT)
modelSelection(I, TT)
# TT is better!


modelSelection(A0, TT)
modelSelection(ar24, TT)

# Out best three models for backtesting and diagnostic analysis
# TT, A0, AR32


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
  cat(sprintf("%s RMSE: %s\n", m22, e2));
  cat("\n\n\n")
}

# TT, A0, AR24

# garchspecE,  garchspecT, garchspecA02, garchspecA0
modelBackTesting(garchspecA0, garchspecAR24, appleReturns)
modelBackTesting(garchspecA0, garchspecT, appleReturns)

 

# show and plot the preferred model: A0
show(A0)
A0
par(mfrow = c(2,2), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(A0)
for(x in c(1:12)){plot(A0, which=x)} 

# forecast next 22 apple daily returns
par(mfrow = c(2,1), mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
forca <- ugarchforecast(A0, n.ahead = 22)

print("##################################")
dev.off()
plot(forca)
for(x in c(1,3)){plot(forca, which=x)} 




# In-sample validation testing using ugarchforecast
fit = ugarchfit(data=appleReturns, spec=garchspecA0, out.sample=100)
forc = ugarchforecast(fit, n.ahead=100)
r1 <- round(fpm(forc), 8)


fit2 = ugarchfit(data=appleReturns, spec=garchspecAR24, out.sample=100)
forc2 = ugarchforecast(fit2, n.ahead=100)
r2 <- round(fpm(forc2), 8)


fit3 = ugarchfit(data=appleReturns, spec=garchspecT, out.sample=100)
forc3 = ugarchforecast(fit3, n.ahead=100)
r3 <- round(fpm(forc3), 8)

modelPerf <- data.frame(cbind(r1,r2,r3))
names(modelPerf) <- c("Model 1", "Model 2", "Model 3")
modelPerf




######################################################################
######################################################################
######                                                  ##############
######    section 3.2.2.4                               ##############
######                                                  ##############
######################################################################
######################################################################

beerdata <- read_excel("H:\\Google Drive\\Learning\\Econ-Fin\\MScFE\\610_Econometrics\\A5-GWA\\BEER_Data_Clean.xlsx")
currdata <- read_excel("H:\\Google Drive\\Learning\\Econ-Fin\\MScFE\\610_Econometrics\\A5-GWA\\CNDOLL_Data.xlsx")



# Convert and clean up dates
beer_dates <- c(beerdata$Date)
beerdata$Date <- as.Date(as.yearqtr(beer_dates, format="Q%q%y%y"), frac = 1)
curr_dates <- c(currdata$Date)
currdata$Date <- as.Date(as.yearqtr(curr_dates, format="Q%q%y%y"), frac = 1)
beerdata <- left_join(beerdata, currdata, by=c("Date","Date"))

# Clean up column names
colnames(beerdata)[colnames(beerdata)=="CNXTW..RF"] <- "CNXTWRF"
colnames(beerdata)[colnames(beerdata)=="USM2....B"] <- "USM2B"
colnames(beerdata)[colnames(beerdata)=="CNM2....B"] <- "CNM2B"

# Prepare data for analysis. 
# All data expressed in log differentials 
beerdata$lnomq <- log(beerdata$`CNDOLL$I`)
beerdata$ldm2 <- log(beerdata$`CNM2B`*beerdata$`CNDOLL$I`) - log(beerdata$`USM2B`)
beerdata$ldcpi <- log(beerdata$`CNOCFRCPE`) - log(beerdata$`USOCFRCPE`)
beerdata$dr <- (beerdata$`CNGBILL3`) - (beerdata$`USGBILL3`)
beerdata$ldprod <- log(beerdata$CNPRODVTQ*beerdata$`CNDOLL$I`) - 
  log(beerdata$`USOPHBS.G`)
df <- data.frame("Date" = beerdata$Date, "lnomq" = beerdata$lnomq
                 ,"ldm2" = beerdata$ldm2,"ldcpi" = beerdata$ldcpi
                 , "dr" = beerdata$dr, "ldprod" = beerdata$ldprod)
rownames(df) <- df$Date
d_lnomq <- diff(beerdata$lnomq, trim=TRUE)
d_ldm2 <- diff(beerdata$ldm2, trim=TRUE)
d_ldcpi <- diff(beerdata$ldcpi, trim=TRUE)
d_dr <- diff(beerdata$dr, trim=TRUE)
d_ldprod <- diff(beerdata$ldprod, trim=TRUE)

#Create the endogenous and exogenous variable matrices
df_end <- data.frame("Date" = df$Date, "lnomq" = df$lnomq
                     ,"ldm2" = df$ldm2,"ldcpi" = df$ldcpi)
rownames(df_end) <- df_end$Date
df_exo <- data.frame("Date" = df$Date, "dr" = df$dr, "ldprod" = df$ldprod)
rownames(df_exo) <- df_exo$Date
df <- subset(df, select = -c(Date))
df_end <- subset(df_end, select = -c(Date))
df_exo <- subset(df_exo, select = -c(Date))
df_pre <- df[c(1:64),]
df_end_pre <- df_end[c(1:64),]
df_exo_pre <- df_exo[c(1:64),]

# We take a first look at the dataset.

# Explore the dataset
head(df)
str(df)
summary(df)
class(df)

# using composite test
allTest <- function(xx){
  x <- na.omit(xx)
  testVector <- c("adf", "pp", "kpss")
  for (val in testVector){
    stationary.test(x, method = val);
    cat("\n\n\n##########################################\n")
  }
}
allTest(df$lnomq)

# simple correlations
cor(df)

# A first look at the correlation matrix yields some strong positive linear
# relationships between the exchange rates and other variables, except for
# interest rates.

#Plot the series
plot(df$lnomq)
plot(df$ldm2)
plot(df$ldcpi)
plot(df$dr)
plot(df$ldprod)

#Plot ACF and PACF
acf(df$lnomq)
pacf(df$lnomq)
acf(df$ldm2)
pacf(df$ldm2)
acf(df$ldcpi)
pacf(df$ldcpi)
acf(df$dr)
pacf(df$dr)
acf(df$ldprod)
pacf(df$ldprod)

# We plot the ACF and PACF all variables and notice that all variables decay slowly.

# Implement Augmented Dickey-Fuller test
adf.test(df$lnomq)
adf.test(df$ldm2)
adf.test(df$ldcpi)
adf.test(df$dr)
adf.test(df$ldprod)

# We fit a VAR Model with labour productivity and interest rates as exogenous 
# variables. The estimation results suggest up to 3 lags are significant and
# should be included. 

# estimate a Vector Auto Regression
VAR_model <- VAR(df_end,lag.max=12,type=c("const"),ic="AIC", exogen=df_exo)
summary(VAR_model)

# We also plot the impulse response functions and the forecast error variance
# decomposition of the fitted VAR model. The impulse response function of the 
# variables don't reveal any significant spikes. 

# compute and plot the impulse response functions 
VAR_irf <- irf(VAR_model,n.ahead = 19,boot = TRUE, ci = 0.95) 
plot(VAR_irf)

# compute and plot the forecast error variance decomposition
VAR_fevd <- fevd(VAR_model,n.ahead=19)
plot(VAR_fevd)

resids = residuals(VAR_model)
resid1 = resids[,1]
resid2 = resids[,2]

# view cross correlogram:

ccf(resid1, resid2, lag.max=18, type = "correlation", plot=TRUE)

# The cross correlogram looks normal.

# We run the VARselect function with a model with a constant and a model with both 
# constant and trend. We see that 12 lags are appropriate in each case but we will
# have to analyze the model results to determine the optimal lags. 

# VARselect(df_end, lag.max=12, type=c("const"), exogen=df_exo)
VARselect(df_end, lag.max=12, type=c("both"), exogen=df_exo)
VARselect(df_end, lag.max=12, type=c("const"), exogen=df_exo)

# Next, we implement a Johansen test for co-integration to determine the optimal
# order of co-integration. All tests suggest r = 1 is appropriate.

# Johansen test for co-integration
jotest = ca.jo(df_end, type="eigen", K=12, ecdet="const", spec="longrun")
summary(jotest)
jotest2 = ca.jo(df_end, type="trace", K=12, ecdet="const", spec="longrun")
summary(jotest2)

# Next, we fit VEC Models with a constant and both constant and trend:

# Fit co-integrated VECM
# Contentious model #1 with both constant and trend, and pre-crisis dataset:
VECM_fit = VECM(df_end, 12, r=1, include="both"
                , estim="ML", LRinclude="both", exogen=df_exo)
summary(VECM_fit)
# Contentious (optimal) model #2 with constant and exogenous variables:
VECM_fit = VECM(df_end,3, r=1, include="const"
                , estim="ML", LRinclude="const", exogen=df_exo)
summary(VECM_fit)


# lnomq     ldm2    ldcpi    const
# r1     1 -3.83587 6.695509 16.01606
#


sink()




 