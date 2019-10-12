library(Quandl)
library(PerformanceAnalytics)
library(timeSeries)

startdate = "2009-01-01"
enddate = "2018-12-31"

# formas:  “raw”, “ts”, “zoo”, “xts”, “timeSeries”
# Request data as ts 
data_raw <- Quandl("FRED/GDP", start_date=startdate, end_date=enddate, type="raw")
head(data_raw)

data_ts <- Quandl("FRED/GDP", start_date=startdate, end_date=enddate, type="ts")
head(data_ts)


data_xts <- Quandl("FRED/GDP", start_date=startdate, end_date=enddate, type="xts")
head(data_xts)

data_zoo <- Quandl("FRED/GDP", start_date=startdate, end_date=enddate, type="zoo")
head(data_zoo)


data_timeSeries <- Quandl("FRED/GDP", start_date=startdate, end_date=enddate, type="timeSeries")
head(data_timeSeries)



Facebook <- Quandl("WIKI/FB", type = "xts")
head(Facebook$`Adj. Close`)



# Both data had difference frequency
# GDP is done quaarterly but apple is done daily
appleGDP <- Quandl(c("FRED/GDP", "WIKI/AAPL"), 
                   start_date=startdate, end_date=enddate)
head(appleGDP)



# so we use aggregation: collapse
# “daily”, “weekly”, “monthly”, “quarterly”, “annual”
appleGDP <- Quandl(c("FRED/GDP", "WIKI/AAPL"), 
                   start_date=startdate, end_date=enddate, collapse = "quarterly")
head(appleGDP)



# get the dataset separately
apple <- Quandl("WIKI/AAPL", start_date=startdate, end_date=enddate)
GDP <- Quandl("FRED/GDP", start_date=startdate, end_date=enddate)
head(GDP)
head(apple)

appleQ 

# use apply.quarterly on apple data
appleQ <- 
# Specify a particular column
Facebook_AdjClose <- Quandl("WIKI/FB.11", type = "xts")
head(Facebook_AdjClose)

Quandl("FRED/GDP", start_date=startdate, end_date=enddate)



plot(data2$`Adj. Close`)
plot(data2$Open)
plot(data2$High)



