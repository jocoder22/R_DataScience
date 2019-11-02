library(jrvFinance)

# setwd("~/R_DataScience/bonds")
source("bondFunction.R")

# Value bond using 5% yield
bondprice(p = 100, cr = 0.05, ttm = 5, y = 0.05)

# Value bond using 7% yield
bondprice(p = 100, cr = 0.05, ttm = 5, y = 0.07)

# Value bond using 6% yield

bondprice(p = 100, cr = 0.05, ttm = 5, y = 0.06)



bondyield(price = 98.79, p = 200, cr = 0.05, ttm = 15)
bondyield(price = 95.79, p = 100, cr = 0.05, ttm = 5)
bondyield(price = 95.79, p = 100, cr = 0.05, ttm = 10)
bondyield(price = 95.79, p = 100, cr = 0.05, ttm = 3)


# Calculate PV01 of a 10% bond
# You can calculate the PV01 by calculating the value of a bond and the value of the 
# same bond with a one basis point change in yield
# Remember that PV01 is equivalent to the value of one bond minus the value of the other bond.

PV01 <- abs(bondprice(p = 100, cr = 0.05, ttm = 5, y = 0.1001)-
              bondprice(p = 100, cr = 0.05, ttm = 5, y = 0.10))
PV01


# Duration of a zero-coupon bond
# Duration can sometimes be thought of as the weighted-average time to maturity 
# of the bond. Because of interim cash flows, the duration of a coupon bond is less 
# than its time to maturity. Based on that reasoning, what is the duration of a 
# zero-coupon bond with three years to maturity? Three




settle <- "2000-01-01"
mature <- "2011-01-01"

bond.price(settle = settle, mature = mature, coupon = 0.05, freq = 1, yield = 0.06, 
           convention = "ACT/ACT", comp.freq = 12, redemption_value = 200)

bond.duration(settle = settle, mature = mature, coupon = 0.05, freq = 1, 0.06, 
           convention = "ACT/ACT", comp.freq = 1)


