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



# Download stocks
tickers  <- "AMZN"
initDate  <- "2008-01-01"
fromDate  <-  "2010-01-01"
toDate  <-  "2019-10-15"

getSymbols(tickers, from=initDate, to=toDate, src =  "yahoo", adjust =  TRUE)


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
# rm(strategy.st)
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
# account.st<- portfolio.st<- strategy.st<- "algorithm1"
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
              arguments = list(x=quote(Cl(AMZN)), n = 200), 
              
              # Label your indicator SMA200
              label = "SMA200")


# Add a 50-day SMA indicator to strategy.one
add.indicator(strategy = strategy.one, 
              
              # Add the SMA function
              name = "SMA", 
              
              # Create a lookback period
              arguments = list(x=quote(Cl(AMZN)), n = 50), 
              
              # Label your indicator SMA200
              label = "SMA50")


#############################################################
######### Initialize 
#############################################################








#############################################################
######### Initialize the portfolio
#############################################################








#############################################################
######### Initialize the portfolio
#############################################################