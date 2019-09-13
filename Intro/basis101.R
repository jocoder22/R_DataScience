library(stats)
a <- rnorm(100)
b <- rnorm(100)

# create a matrix
X = cbind(a, b)
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