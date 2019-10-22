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
library(glue, quietly = TRUE)
library(fpp2, quietly = TRUE)
library(ggplot2, quietly = TRUE)
library(forecast, quietly = TRUE)


# Download stocks
tickers  <- "AMZN"
initDate  <- "2008-01-01"
toDate  <-  "2019-10-15"

stockTrade  <- getSymbols(tickers, from=initDate, to=toDate,  auto.assign = FALSE,
                          src =  "yahoo", adjust =  TRUE)



# estimated volatility
# using volatility() from TTR package
# first form the ohlc object
ohlc <- OHLC(stockTrade)
vClose <- volatility(ohlc, calc="close")
vClose0 <- volatility(ohlc, calc="close", mean0=TRUE)
vGK <- volatility(ohlc, calc="garman")
vParkinson <- volatility(ohlc, calc="parkinson")
vRS <- volatility(ohlc, calc="rogers")
vGKy <- volatility(ohlc, calc="gk.yz")
vYZ <- volatility(ohlc, calc="yang.zhang")
vMC <- chaikinVolatility(ohlc[,2:3])    #(GSPC[,c("GSPC.High", "GSPC.Low")])


# Merge and replace NA
vtable <- merge(vClose, vClose0, vGK, vParkinson, vRS, vGKy, vYZ, vMC)
vtable[is.na(vtable)] <- 0


for(val in c(1 : dim(vtable)[2])){
  par(mfrow=c(2,1))
  plot(index(ohlc), ohlc[,4], type="l",main = paste(tickers, "Close Price", ylab="Price", xlab="Date"))
  string="Volatility plot for {colnames(vtable)[val]}."
  plot(index(ohlc),vtable[,val], type="l", main=glue(string), 
       ylab="Volatility", col = val, xlab="Date")
}

# plot the graphs
autoplot(vtable, facets = TRUE)
autoplot(vtable)



# Plot multiplot
par(mfrow=c(1,1))
plot(merge(vClose, vClose0, vGK, vParkinson, vRS, vGKy, vYZ, vMC), 
     main = "Price Volatililty", multi.panel = TRUE, facets = TRUE)
plot(merge(vClose, vClose0, vGK, vParkinson, vRS, vGKy, vYZ, vMC), main = "Price Volatililty")


# plot the close price 
plot(Cl(stockTrade), main = paste(tickers, "Close prices"))


# add indicators
lines(SMA(Cl(stockTrade), n = 200), col = "blue")
lines(SMA(Cl(stockTrade), n = 50), col = "red")


lines(RSI(Cl(stockTrade), n = 126), col = "yellow")
lines(RSI(Cl(stockTrade), n = 5), col = "green")



###########################################################
########
########   Setup for quantstrat
########
###########################################################

# Download stocks
tickers  <- "JPM" # "AMZN" #"SPY" #
initDate  <- "2008-01-01"
fromDate  <-  "2010-01-01"
toDate  <-  "2019-10-15"

getSymbols(tickers, from=fromDate,   src =  "yahoo", adjust =  TRUE)
stockTrade  <- getSymbols(tickers, from=fromDate,  auto.assign = FALSE,
                          src =  "yahoo", adjust =  TRUE)


###########################################################
########
########   Setup for quantstrat
########
###########################################################

# Clean up the environment
rm(strategy.one)
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
stock(tickers, currency="USD", multiplier=1)

# Define your trade size and initial equity
tradesize <- 1000000
initeq <- 1000000

# Define the names of strategy, portfolio and account
# account.st<- portfolio.st<- strategy.one<- "algorithm1"
strategy.one <- "algorithm1"
portfolio.one <- "algorithm1"
account.one <- "algorithm1"

# Remove the existing strategy if it exists
rm.strat(strategy.one)


#############################################################
######### Initialize the portfolio
#############################################################

initPortf(portfolio.one, symbols = tickers, initDate = initDate, currency = "USD")

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
test  <-  applyIndicators(strategy = strategy.one, mktdata = OHLC(stockTrade))
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



# Add sigThreshold which specifies that sma_Ratio.3.84 must be greater than 80, label it shortthreshold
add.signal(strategy.one, name = "sigThreshold", 
           
           # Use the DVO_2_126 column
           arguments = list(column = "smaRatio.3.84", 
                            
                            # The threshold is 80
                            threshold = 80, 
                            
                            # We want the oscillator to be under this value
                            relationship = "gt", 
                            
                            # We're interested in every instance that the oscillator is greater than 80
                            cross = FALSE), 
           
           # Label it shortthreshold
           label = "shortthreshold")


# Add sigThreshold which specifies that sma_Ratio.3.84 must be less than 20, label it longthreshold
add.signal(strategy.one, name = "sigThreshold", 
           
           # Use the DVO_2_126 column
           arguments = list(column = "smaRatio.3.84", 
                            
                            # The threshold is 20
                            threshold = 20, 
                            
                            # We want the oscillator to be under this value
                            relationship = "lt", 
                            
                            # We're interested in every instance that the oscillator is less than 20
                            cross = FALSE), 
           
           # Label it longthreshold
           label = "longthreshold")


# Add a sigFormula signal to your code specifying that both longfilter and longthreshold must be TRUE, label it longentry
add.signal(strategy.one, name = "sigFormula",
           
           # Specify that longfilter and longthreshold must be TRUE
           arguments = list(formula = "longRun & longthreshold", 
                            
                            # Specify that cross must be TRUE
                            cross = TRUE),
           
           # Label it longentry
           label = "longentry")



test2 <- applyIndicators(strategy.one, mktdata = OHLC(stockTrade))
test3 <- applySignals(strategy = strategy.one, mktdata = test2)
head(test3)
tail(test3)


#############################################################
#########  Implement Rules
#############################################################



osMaxDollar  <-  function(data, timestamp, orderqty, ordertype, orderside,
                           portfolio, symbol, prefer = "Open", tradeSize,
                           maxSize, integerQty = TRUE, ...) {
  
    pos <- getPosQty(portfolio, symbol, timestamp)
    
    if(prefer == "Close") {
      price <- as.numeric(Cl(mktdata[timestamp,]))
    } else {
      price <- as.numeric(Op(mktdata[timestamp,]))
    }
    
    posVal <- pos*price
    
    if (orderside=="short") {
      dollarsToTransact <- max(tradeSize, maxSize-posVal)
      #If our position is profitable, we don't want to cover needlessly.
      if(dollarsToTransact > 0) {dollarsToTransact = 0}
    } else {
      dollarsToTransact <- min(tradeSize, maxSize-posVal)
      #If our position is profitable, we don't want to sell needlessly.
      if(dollarsToTransact < 0) {dollarsToTransact = 0}
    }
    
    qty <- dollarsToTransact/price
    
    if(integerQty) {
      qty <- trunc(qty)
    }
    return(qty)
}

# Fill in the rule's type as exit
add.rule(strategy.one, name = "ruleSignal", 
         arguments = list(sigcol = "shortRun", sigval = TRUE, orderqty = "all", 
                          ordertype = "market", orderside = "long", 
                          replace = FALSE, prefer = "Open"), 
         type = "exit")



# Create an entry rule of 1 share when all conditions line up to enter into a position
add.rule(strategy.one, name = "ruleSignal", 
         
         # Use the longentry column as the sigcol
         arguments=list(sigcol = "longRun", 
                        
                        # Set sigval to TRUE
                        sigval = TRUE, 
                        
                        # Set orderqty to 1
                        # orderqty = 1,
                        # Use the osFUN called osMaxDollar
                        osFUN = osMaxDollar,
                        
                        # Use a market type of order
                        ordertype = "market",
                        
                        tradeSize = tradesize,
                        
                        # The maxSize argument should be equal to tradesize as well
                        maxSize = tradesize,
                        
                        # Take the long orderside
                        orderside = "long",
                        
                        # Do not replace other signals
                        replace = FALSE, 
                        
                        # Buy at the next day's opening price
                        prefer = "Open"),
         
         # This is an enter type rule, not an exit
         type = "enter")


# pos <- getPosQty(portfolio.one, tickers, timestamp)
# pos
#############################################################
######### Run our strategy
#############################################################


# Use applyStrategy() to apply your strategy. Save this to out
out <- applyStrategy(strategy = strategy.one, portfolios = portfolio.one)

# Update your portfolio (portfolio.st)
updatePortf(portfolio.one)
daterange <- time(getPortfolio(portfolio.one)$summary)[-1]

# Update your account (account.st)
updateAcct(account.one, daterange)
updateEndEq(account.one)



#############################################################
######### Evaluate our strategy
#############################################################

# Get the tradeStats for your portfolio
tstats <- tradeStats(Portfolios = portfolio.one)
tstats2  <- t(tstats) 
View(tstats)


# Print the profit factor
tstats$Profit.Factor
tstats$Percent.Positive
View(getOrderBook(portfolio = portfolio.one))
oderBook  <-  as.data.frame(getOrderBook(portfolio = portfolio.one)[["algorithm1"]][[tickers]])



#############################################################
######### Evaluate our strategy
#############################################################

chart.Posn(Portfolio = portfolio.one, Symbol = tickers)


# Compute the SMA50
sma50 <- SMA(x = Cl(stockTrade), n = 50)

# Compute the SMA200
sma200 <- SMA(x = Cl(stockTrade), n = 200)

# Compute the DVO_2_126 with an navg of 2 and a percentlookback of 126
dvo <- smaRatio(HLC = HLC(stockTrade), navg = 3, percentlookback = 84)

# Recreate the chart.Posn of the strategy from the previous exercise
chart.Posn(Portfolio = portfolio.one, Symbol = tickers)

# Overlay the SMA50 on your plot as a blue line
add_TA(sma50, on = 1, col = "blue")

# Overlay the SMA200 on your plot as a red line
add_TA(sma200, on = 1, col = "red")

# Add the DVO_2_126 to the plot in a new window
add_TA(dvo)



# get the net trading returns
portpl <- .blotter$portfolio.algorithm1$summary$Net.Trading.PL
SharpeRatio.annualized(portpl, geometric=FALSE)


# Get instrument returns
instrets <- PortfReturns(portfolio.one)

# Compute Sharpe ratio from returns
SharpeRatio.annualized(instrets, geometric = FALSE)








# https://www.howtobuildsoftware.com/index.php/how-do/bNZc/r-quantstrat-quanstrat-strategy-error
findK  <- function(HLC){
  n  <- dim(HLC)[1]
  f  <-  frequency(HLC)
  k  <- 12 * (n / 100) ^ 0.25
  return(floor(k))
}
findK(stockTrade)
dim(stockTrade)
