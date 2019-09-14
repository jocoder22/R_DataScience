
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
inverse%*%X_prime_X
solve(X_prime_X)
