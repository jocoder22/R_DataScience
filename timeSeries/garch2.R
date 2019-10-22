# Here we explore Autocorrelation of volatility 
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = TRUE)
library(rugarch, quietly = TRUE)

# download dataset
getSymbols("JPM", src="yahoo")



# select required feature the Adjusted close
jpClose <- Ad(JPM)

# explore dataset
str(jpClose)
head(jpClose)
summary(jpclose)
class(jpClose)


# calculate the daily simple return
jpReturn <- CalculateReturns(jpClose)[-1]

# calculate mean and sd daily return
meanReturn <- mean(jpReturn)
sdReturn <- sd(jpReturn)



# Define the series of prediction errors
pred_error <- jpReturn - meanReturn
error2 <- pred_error ^ 2

# commpute standandized errors
std_error <- pred_error / meanReturn

# Plot the absolute value of the standardized prediction errors
par(mfrow = c(3,1),mar = c(3, 2, 2, 2))
plot(std_error, main = "Standardized Prediction errors")

# Plot the absolute value of the standandized prediction errors
plot(abs(pred_error), main = "Prediction errors (Absolute value)")


# Plot the acf of the absolute prediction errors
acf(abs(pred_error), main = "Absolute prediction errors ACF")


# Define variables
num <- length(jpReturn)
alpha <- 0.091
beta <- 0.834
omega <- var(jpReturn) * (1 - alpha - beta)
predvar <- rep(NA, num)  

# Compute the predicted variances
predvar[1] <- var(jpReturn) 
for(t in 2:num){
  predvar[t] <- omega + alpha * error2[t-1] + beta * predvar[t-1]
}


# Create annualized predicted volatility
annualizedpredvol <- xts(sqrt(252) * sqrt(predvar), order.by = time(jpReturn))

# Plot the annual predicted volatility 
par(mfrow = c(1,1))
plot(annualizedpredvol, main = "Annualized JP Morgan Predicted Volatility")

annualizedpredvol
