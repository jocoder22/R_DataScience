# Load the required packages
library(quantmod, quietly=T)
library(PerformanceAnalytics, quietly = T)
library(TTR, quietly = T)
library(glue, quietly = T)

# set my file path
currentdir <- getwd()
filepath = file.path(currentdir,"timeSeries", "garch.rds")

# Download datasets
startDate <- "1989-02-01"
endDate <- "2018-12-30"

getSymbols("^GSPC", src="yahoo", from=startDate)

sp500 <- GSPC$GSPC.Adjusted
names(sp500) <- "PriceClose"

sp500$Returns <- CalculateReturns(sp500$PriceClose)


# Create series charts
chartSeries(sp500$Returns,name="S&P 500 Adjusted Close Price $USD")


# estimated volatility
# using volatility() from TTR package
# first form the ohlc object
ohlc <- GSPC[,c("GSPC.Open", "GSPC.High", "GSPC.Low", "GSPC.Close")]
vClose <- volatility(ohlc, calc="close")
vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
vGK <- volatility(ohlc, calc="garman")
vParkinson <- volatility(ohlc, calc="parkinson")
vRS <- volatility(ohlc, calc="rogers")
vGKy <- volatility(ohlc, calc="gk.yz")
vYZ <- volatility(ohlc, calc="yang.zhang")
vMC <- chaikinVolatility(GSPC[,c("GSPC.High", "GSPC.Low")])

vtable <- data.frame(vClose, vClose0, vGK, vParkinson, vRS, vGKy, vYZ, vMC)
tail(vtable)


par(mfrow=c(3, 1))
for(val in c("vClose", "vClose0", "vGK")){
  string="Volatility plot for {val}."
  ts.plot(vtable[val], main=glue(string), ylab="Volatility")
}

plot(merge(vClose, vClose0, vGK, vParkinson, vRS, vGKy, vYZ, vMC), multi.panel = TRUE)
# Volatility skewness is a similar measure to omega but using the second 
# partial moment. It's the ratio of the upside variance compared to the 
# downside variance. Variability skewness is the ratio of the upside risk 
# compared to the downside risk.


MAR = 0.005
print(VolatilitySkewness(sp500[,2], MAR, stat="volatility")) 
print(VolatilitySkewness(sp500[,2], MAR, stat="variability")) 


# calculate simple volatility, return volatility: Greek letter sigma 
return_volatility <- sd(sp500$Returns[-1])
vol_simple
# 0.01094313 

annualized_volatility <- sqrt(252) * return_volatility
annualized_volatility
# 0.1737168


# chart rolling performace  
chart.RollingPerformance(R=sp500$Returns[-1], width=22, scale=252, 
                         FUN="sd.annualized", main="Rolling 1 month Volatility")

chart.RollingPerformance(R=sp500$Returns[-1], width=22, scale=252, 
                         colorset = (1:12), FUN = "Return.annualized", 
                         na.pad = TRUE, type = "l", main="Rolling 1 month Return")

plot(sp500$Returns,name="S&P 500 Adjusted Close Price $USD")






