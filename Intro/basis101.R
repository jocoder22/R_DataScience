
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
