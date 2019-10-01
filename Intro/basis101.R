
# install.packages("matlib", dependencies=TRUE, repos='http://cran.rstudio.com/')
library(stats)
library(matlib)
a <- rnorm(100)
b <- rnorm(100)
c <- rnorm(100)

# create a matrix
X = cbind(a, b, c)
X


# Element by element multiplication
X*X


# Inner product X'X, 
# t() is the transpose function
# %*% denotes matrix multiplication
X_prime_X <- t(X)%*%X
X_prime_X


# for XX'
XX_prime <- X%*%t(X)
XX_prime


# find the determinant
det(X_prime_X)
inverse <- inv(X_prime_X)
 solve(X_prime_X)


# inv(inv(A)) = A
# inv(A) is symmetric if and only if A is symmetric
# inv(t(A)) = t(inv(A))
# inv( k*A ) = (1/k) * inv(A)
# inv(A * B) = inv(B) %*% inv(A)


####################################################
# diagonal matrix
D <- diag(c(1, 2, 4))
D


# inverse of diagonal matrix = diag( 1/ diagonal)
inv(D)
diag(1 / diag(D))


# The determinant of an inverse is the inverse (reciprocal) of the determinant
# det(AI) =   1 / det(A)
det(inv(D))
1/det(D) 












# plot the volatility
plot(vCK)
vCK$sd <- mean(na.omit(vCK$EMA))
lines(vCK$sd, col="red")


# plot the annualised volatility
apple$sd <- sd(apple$Returns) * sqrt(252)

addSeries(apple$sd, col = "red")



# plot the graphs
par(mfrow = c(2,1))
par(mar = c(2,3,3,3), oma = c(1, 1, 1, 1))
plot(apple)
plot(apple)

# plot the acf and pacf
acf2(apple)
acf2(apple$Returns)
acf2(apple$Returns^2)


# graph the distribution
chart.Histogram(apple, methods = c("add.normal","add.density" ), 
                colorset = c("gray","red","blue"), 
                main="Empirical Distribution vs Normal Distribution")

adfTest(apple$Returns)


tseries::adf.test(apple$Returns)




# model the data using fGarch::
model1 <- garchFit(formula = ~ garch(3,0), data = apple$Returns, trace = F)
summary(model1)


model2 <- garchFit(formula = ~ arma(0,0) + garch(1, 2), data = apple$Returns, 
                   trace = F, cond.dist="snorm")
# 'arg' should be one of “norm”, “snorm”, “ged”, “sged”, “std”, “sstd”, “snig”, “QMLE”
summary(model2)


model3 <- garchFit(formula = ~ arma(0,0) + garch(1, 2), data = apple$Returns, 
                   trace = F, cond.dist="sstd")
# 'arg' should be one of “norm”, “snorm”, “ged”, “sged”, “std”, “sstd”, “snig”, “QMLE”
summary(model3)

coef(model2)
mytable <- as.data.frame(coef(model3))
mytable$model3 <- coef(model3)






garchspec <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                        variance.model = list(model="sGARCH", garchOrder=c(1,2)),
                        distribution.model = "std")

model4 <- ugarchfit(spec=garchspec, data=apple$Returns)
summary(model4)
model4


uncov <- sqrt(coef(model4)[1]/(1 - sum(coef(model4)[2:5])))
predvol <- xts(sqrt(252) * model4@fit$sigma, order.by = time(apple))
uncon <- xts(rep(uncov, length(predvol)),  order.by = time(apple))
plot(predvol)
lines(uncon, type='l', col="red")
