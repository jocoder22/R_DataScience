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
initDate  <- "2008-01-10"
fromDate  <-  "2010-01-10"
toDate  <-  "2019-10-15"

amazon  <- getSymbols(tickers, from=initDate, auto.assign = FALSE, src =  "yahoo", adjust =  TRUE)


# plot the close price 
plot(Cl(amazon), main = "Amazon Close prices")


# add indicators
lines(SMA(Cl(amazon), n = 200), col = "blue")
lines(SMA(Cl(amazon), n = 50), col = "red")


lines(RSI(Cl(amazon), n = 126), col = "yellow")
lines(RSI(Cl(amazon), n = 5), col = "green")



###########################################################
########
########   Setup for quantstrat
########
###########################################################

# Set the timezone to UTC
Sys.setenv(TZ="UTC")

# Set the currency to USD 
currency("USD")

# initialize the stock
stock("amazon", currency="USD", multiplier=1)

# Define your trade size and initial equity
tradesize <- 1000000
initeq <- 1000000

# Define the names of strategy, portfolio and account
# account.am <- portfolio.am <- strategy.am <- "algorithm1"
strategy.am <- "algorithm1"
portfolio.am <- "algorithm1"
account.am <- "algorithm1"

# Remove the existing strategy if it exists
rm.strat(strategy.am)

