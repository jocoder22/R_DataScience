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
tickers = "AMZN"
initDate  <- "2008-01-10"
fromDate  <-  "2010-01-10"
toDate  <-  "2019-10-15"

getSymbols(tickers, from=initDate, src =  "yahoo", adjust =  TRUE)

