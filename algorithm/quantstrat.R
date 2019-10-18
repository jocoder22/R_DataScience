# install.packages("remotes")
# 
# install.packages("FinancialInstrument")
# install.packages("PerformanceAnalytics")
# install.packages("foreach")
# 
# install.packages("devtools")
# require(devtools)
# 
# remotes::install_github("braverock/blotter") # dependency
# remotes::install_github("braverock/quantstrat")


# Load the required packages
library(quantmod, quietly = TRUE)
library(PerformanceAnalytics, quietly = TRUE)
library(quantstrat, quietly = TRUE)
library(blotter, quietly = TRUE)
library(TTR, quietly = TRUE)
library(tseries, quietly = TRUE)
library(timeSeries, quietly = TRUE)
library(glue, quietly = T)



# Download stocks
tickers  <- "AMZN"
initDate  <- "2008-01-01"
fromDate  <-  "2010-01-01"
toDate  <-  "2019-10-15"

getSymbols(tickers, from=initDate, to=toDate, src =  "yahoo", adjust =  TRUE)


# estimated volatility
# using volatility() from TTR package
# first form the ohlc object
ohlc <- OHLC(AMZN)
vClose <- volatility(ohlc, calc="close")
vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
vGK <- volatility(ohlc, calc="garman")
vParkinson <- volatility(ohlc, calc="parkinson")
vRS <- volatility(ohlc, calc="rogers")
vGKy <- volatility(ohlc, calc="gk.yz")
vYZ <- volatility(ohlc, calc="yang.zhang")
vMC <- chaikinVolatility(ohlc[,2:3])    #(GSPC[,c("GSPC.High", "GSPC.Low")])

vtable <- data.frame(vClose, vClose0, vGK, vParkinson, vRS, vGKy, vYZ, vMC)




for(val in c(1 : dim(vtable)[2])){
  par(mfrow=c(2,1))
  plot(index(ohlc), ohlc[,4], type="l",main = "Amazon Close Price", ylab="Price", xlab="Date")
  string="Volatility plot for {colnames(vtable)[val]}."
  plot(index(ohlc),vtable[,val], type="l", main=glue(string), 
       ylab="Volatility", col = val, xlab="Date")
}


# Plot multiplot
par(mfrow=c(1,1))
plot(merge(vClose, vClose0, vGK, vParkinson, vRS, vGKy, vYZ, vMC), 
     main = "Price Volatililty", multi.panel = TRUE)
plot(merge(vClose, vClose0, vGK, vParkinson, vRS, vGKy, vYZ, vMC), main = "Price Volatililty")


# plot the close price 
plot(Cl(AMZN), main = "Amazon Close prices")


# add indicators
lines(SMA(Cl(AMZN), n = 200), col = "blue")
lines(SMA(Cl(AMZN), n = 50), col = "red")


lines(RSI(Cl(AMZN), n = 126), col = "yellow")
lines(RSI(Cl(AMZN), n = 5), col = "green")



###########################################################
########
########   Setup for quantstrat
########
###########################################################

# Clean up the environment
# rm(strategy.one)
try(rm("account.one","portfolio.one", "strategy.one"), silent=TRUE)  

# .blotter <- new.env()
# .strategy <- new.env()

if (!exists('.blotter')) .blotter <- new.env()
if (!exists('.strategy')) .strategy <- new.env()



# Set the timezone to UTC
Sys.setenv(TZ="UTC")

# Set the currency to USD 
currency("USD")

# initialize the stock
stock("AMZN", currency="USD", multiplier=1)

# Define your trade size and initial equity
tradesize <- 1000000
initeq <- 1000000

# Define the names of strategy, portfolio and account
# account.st<- portfolio.st<- strategy.one<- "algorithm1"
strategy.one <- "algorithm1"
portfolio.one <- "algorithm1"
account.one <- "algorithm1"

# Remove the existing strategy if it exists
# rm.strat(strategy.am)


#############################################################
######### Initialize the portfolio
#############################################################

initPortf(portfolio.one, symbols = "AMZN", initDate = initDate, currency = "USD")

# Initialize the account
initAcct(account.one, portfolios = portfolio.one, initDate = initDate, currency = "USD", initEq = initeq)

# Initialize the orders
initOrders(portfolio.one, initDate = initDate)

# Store the strategy
strategy(strategy.one, store = TRUE)



#############################################################
######### Add trading indicators to the strategy
#############################################################

# Add a 200-day SMA indicator to strategy.one
add.indicator(strategy = strategy.one, 
              
              # Add the SMA function
              name = "SMA", 
              
              # Create a lookback period
              arguments = list(x=quote(Cl(mktdata)), n = 200), 
              
              # Label your indicator SMA200
              label = "SMA200")


# Add a 50-day SMA indicator to strategy.one
add.indicator(strategy = strategy.one, 
              
              # Add the SMA function
              name = "SMA", 
              
              # Create a lookback period
              arguments = list(x=quote(Cl(mktdata)), n = 50), 
              
              # Label your indicator SMA200
              label = "SMA50")


# Add an RSI 3 indicator to strategy.one
add.indicator(strategy = strategy.one, 
              
              # Add the RSI function
              name = "RSI", 
              
              # Create a lookback period
              arguments = list(price = quote(Cl(mktdata)), n = 3), 
              
              # Label your indicator RSI_3
              label = "RSI3")


# Add an RSI 6 indicator to strategy.one
add.indicator(strategy = strategy.one, 
              
              # Add the RSI function
              name = "RSI", 
              
              # Create a lookback period
              arguments = list(price = quote(Cl(mktdata)), n = 6), 
              
              # Label your indicator RSI_6
              label = "RSI6")



# Write the RSI_dynamic function
RSI_dynamic <- function(price, n1, n2) {
  
  # RSI1 takes an input of the price and n1
  RSI1 <- RSI(price = price, n = n1)
  
  # RSI2 takes an input of the price and n2
  RSI2 <- RSI(price = price, n = n2)
  
  # RSI_avg is the third of the sum of RSI1 and RSI2
  RSIavg <- (RSI1 + RSI2)/3
  
  # Name the column as RSI_avg
  colnames(RSIavg) <- "AvgRSI"
  
  return(RSIavg)
}



# Add this function as RSI_dynamic to your strategy with n1 = 2 and n2 = 5
add.indicator(strategy.one, 
              
              # Add the RSI_dynamic function
              name = "RSI_dynamic", 
              
              # Create a lookback periods
              arguments = list(price = quote(Cl(mktdata)), n1 = 2, n2 = 5), 
              
              # Label your indicator RSI_2.5
              label = "2.5")



# Declare the smaRatio function
smaRatio <- function(HLC, navg = 2, percentlookback = 200) {
  
  # Compute the ratio between closing prices to the sum of high and low
  ratio <- Cl(HLC)/(Hi(HLC) + Lo(HLC))
  
  # Smooth out the ratio outputs using a moving average
  avgratio <- SMA(ratio, n = navg)
  
  # Convert ratio into a 0-100 value using runPercentRank()
  resultgg <- runPercentRank(avgratio, n = percentlookback, exact.multiplier = 1) * 100
  
  # Name the column as sma_Ratio
  colnames(resultgg) <- "smaRatio"
  
  return(resultgg)
}



# Add this function as smaRatio to your strategy with navg = 3, percentlookback = 84
add.indicator(strategy.one, 
              
              # Add the smaRatio function
              name = "smaRatio", 
              
              # Create a lookback periods and percentlookback
              arguments = list(HLC = quote(HLC(mktdata)), navg = 3, percentlookback = 84), 
              
              # Label your indicator sma_3.84
              label = "3.84")



# Test the indicators
test  <-  applyIndicators(strategy = strategy.one, mktdata = OHLC(AMZN))
head(test)
tail(test)


#############################################################
######### Add signals to the strategy
#############################################################

# Add a sigComparison to check when SMA50 greater than SMA200, call it longRun
add.signal(strategy.one, name = "sigComparison", 
           
           # The relationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"), 
                            
                            # When the SMA50 is greater than the SMA200
                            relationship = "gt"),
           
           # Label this signal longRun
           label = "longRun")



# Add a sigCrossover checking when SMA50 less than SMA200, call it shortRun
add.signal(strategy.one, name = "sigCrossover", 
           
           # Eelationship between the SMA50 and the SMA200
           arguments = list(columns = c("SMA50", "SMA200"), 
                            
                            # When the SMA50 is less than the SMA200
                            relationship = "lt"),
           
           # Label this signal shortRun
           label = "shortRun")


# Add sigThreshold which specifies that DVO_2_126 must be less than 20, label it longthreshold
add.signal(strategy.st, name = "sigThreshold", 
           
           # Use the DVO_2_126 column
           arguments = list(column = "DVO_2_126", 
                            
                            # The threshold is 20
                            threshold = 20, 
                            
                            # We want the oscillator to be under this value
                            relationship = "lt", 
                            
                            # We're interested in every instance that the oscillator is less than 20
                            cross = FALSE), 
           
           # Label it longthreshold
           label = "longthreshold")

#############################################################
######### Initialize the portfolio
#############################################################








#############################################################
######### Initialize the portfolio
#############################################################









# https://www.howtobuildsoftware.com/index.php/how-do/bNZc/r-quantstrat-quanstrat-strategy-error
findK  <- function(HLC){
  n  <- dim(HLC)[1]
  f  <-  frequency(HLC)
  k  <- 12 * (n / 100) ^ 0.25
  return(floor(k))
}
findK(AMZN)
dim(AMZN)
