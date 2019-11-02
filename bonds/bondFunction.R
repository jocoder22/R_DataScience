

bondprice <- function(p, cr, ttm, y){
  
  # create a vector of coupon payment layout
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




bondprice(1100, 0.051, 10, 0.07)


