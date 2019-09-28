# Here we explore the rugarch package
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(rugarch, quietly = T)

# download dataset
getSymbols("JPM", src="yahoo")

# select required feature
jpClose <- JPM[,"JPM.Adjusted"]


# calculate the daily simple return
jpReturn <- CalculateReturns(jpClose)[-1]


# create model specs  
specs <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                    variance.model = list(model="sGARCH"),
                    distribution.model = "norm")

model <- ugarchfit(data=jpReturn, spec=specs)

pred_volatility <- sigma(model)


# Plot the predicted volatility 
par(mfrow = c(1,1))
plot(pred_volatility, main = "GARCH Predicted JP Morgan Predicted Volatility")



# Compute unconditional variance
uncvariance(model)


# Compute long run standard deviation
sqrt(uncvariance(model))  # [1] 0.02109116

# Compute estimated coefficients
cofficients <- coef(model)
cofficients
sumAlphaBeta <- cofficients[3] + cofficients[4]
sumAlphaBeta # 0.9843281, less than one, there the returns are mean reverting


# Compute estimated mean
fitted(model)[1]

# Print last 10 ones in pred_volatility
tail(pred_volatility, 1)
# 2019-09-20 0.01356278


mean(jpReturn)
# [1] 0.0006994094



# Forecast volatility 5 days ahead and add 
gforecast <- ugarchforecast(fitORspec = model, 
                                n.ahead = 5)

# Extract the predicted volatilities and print them
print(sigma(gforecast))


# Extract the predicted mean and print them
print(fitted(gforecast)) # constant mean assumption, arimaOrder=c(0,0)


# Compute the annualized volatility
annualvol <- sqrt(252) * sigma(model)

# Compute the 5% vol target weights  
vt_weights <- 0.05 / annualvol

# Compare the annualized volatility to the portfolio weights in a plot
plot(merge(annualvol, vt_weights), multi.panel = TRUE)
# the model recommends a portfolio exposure to the JP Morgan of about 25%.



# create model2 with skewed t-distribution
# create model specs  
specs2 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                    variance.model = list(model="sGARCH"),
                    distribution.model = "sstd")

model2 <- ugarchfit(data=jpReturn, spec=specs2)
coef(model2)
coef(model)

# Compute the standardized returns
stdret2 <- residuals(model2, standardize = TRUE)




# Compute the standardized returns
stdret <- residuals(model, standardize = TRUE)

# Compute the standardized returns using fitted() and sigma()
stdretCal <- (jpReturn - fitted(model)) / sigma(model)


par(mfrow = c(2,1))
chart.Histogram(stdret, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals")


chart.Histogram(stdretCal, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Calculated Predicted Residuals")



par(mfrow = c(2,1))
chart.Histogram(stdret, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals under Normal Distribution")

chart.Histogram(stdret2, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals under skewed t-Distribution")



# create model2 with skewed t-distribution and gjrGARCH variance model
# create model specs  
specs3 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                     variance.model = list(model="gjrGARCH"),
                     distribution.model = "sstd")

model3 <- ugarchfit(data=jpReturn, spec=specs3)
coef(model3)
# Compute the standardized returns
stdret3 <- residuals(model3, standardize = TRUE)
chart.Histogram(stdret3, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Model Predicted Residuals under skewed t-Distribution")

par(mfrow = c(1,1))
out <- newsimpact(model2)
plot(out$zx, out$zy, xlab="Prediction Error", ylab="Predicted Variance")

?rugarch

