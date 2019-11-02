
futurevalue <- function(p, r , t){
  
  p * (1 + r) ^ t
}

cashflow <- function(p, cr, ttm){
  
  cf <- c(rep(p * cr, ttm - 1), p * (1 + cr))
  
}



bondprice <- function(p, cr, ttm, y){
  
  # create a vector of bond payment layout:  the bond cash flow
  cp <- c(rep(p * cr, ttm - 1), p * (1 + cr))
  
  # create sequence of time horizon
  tim <-  seq(1, ttm)
  
  # create a series of time multiplier
  fac <-  1 / (1 + y)^tim
  
  # compute series of present value per year
  psv <- cp * fac
  
  # sum the yearly present value 
  sum(psv)
  
}




# Create bondyield() function using uniroot
bondyield <- function(price, p , cr, ttm){
  
  # compute the bond cash flow
  bondcashflow  <- c(-price, rep(p * cr, ttm - 1), p * (1 + cr))
  
  # Create bond valuation function
  bvalue <- function(i, bondcashflow){
    tt = seq(along = bondcashflow)
    sum(bondcashflow / (1 + i)^tt)
  }
  
  # solve the using uniroot
  uniroot(bvalue, c(0, 1), bondcashflow = bondcashflow)$root
}





bondprice(1100, 0.051, 10, 0.07)

bondyield(98.79, 200, 0.05, 15)
