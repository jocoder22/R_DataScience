library(jrvFinance)

# setwd("~/R_DataScience/bonds")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
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
bondyield(price = 98.79, p = 100, cr = 0.05, ttm = 11)

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


aaa_yield <- 0.0341
settle <- "2000-01-01"
mature <- "2011-01-01"


# compute using jrvFinance:: 
bond.price(settle = settle, mature = mature, coupon = 0.05, freq = 1, yield = aaa_yield, 
           convention = "ACT/ACT", comp.freq = 1, redemption_value = 100)

bond.duration(settle = settle, mature = mature, coupon = 0.05, freq = 1, yield = aaa_yield, 
           convention = "ACT/ACT", comp.freq = 1, modified = TRUE)

bond.yield(settle = settle, mature = mature, coupon = 0.05, freq = 1, price = 98.79, 
              convention = "ACT/ACT", comp.freq = 1)

bond.TCF(settle = settle, mature = mature, coupon = 0.05, freq = 1, convention = "ACT/ACT")



# compare cash flows
print(sum(cashflow(100, 0.05, 11)))
print(futurevalue(100, 0.05,11))


# compute duration
# This is a approx measure
# Calculate bond price when yield increases
ppx <- bondprice(p = 100, cr = 0.05, ttm = 11, y = aaa_yield)


# Calculate bond price when yield increases by 1%
price_up <- bondprice(p = 100, cr = 0.05, ttm = 11, y = aaa_yield + 0.01)

# Calculate bond price when yield decreases by 1%
price_down <- bondprice(p = 100, cr = 0.05, ttm = 11, y = aaa_yield - 0.01)

# Calculate duration
# This is an approx measure, good estimaate when ttm is short
duration <- (price_down - price_up) / (2 * ppx * 0.01)
duration


# Calculate percentage effect of duration on price
duration_pct_change <- -duration * 0.01
duration_pct_change

# Calculate dollar effect of duration on price
duration_dollar_change <- -duration * 0.01 * ppx
duration_dollar_change


# Calculate convexity measure
# This is an approx measure
convexity <- (price_down + price_up - 2 * ppx) / (ppx * (0.01)^2)

# Calculate percentage effect of convexity on price
convexity_pct_change <- 0.5 * convexity * 0.01^2
convexity_pct_change

# Calculate dollar effect of convexity on price
convexity_dollar_change <- convexity_pct_change * ppx
convexity_dollar_change



# Estimate price_change
price_change <- duration_dollar_change + convexity_dollar_change
price_change

# Estimate new_price
new_price <- duration_dollar_change + convexity_dollar_change + ppx
new_price
