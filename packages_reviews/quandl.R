library(Quandl)
library(PerformanceAnalytics)
library(timeSeries)
library(xts)
library(quantmod)

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
# quarterly etc. takes value of the last date in the peroid
appleGDP <- Quandl(c("FRED/GDP", "WIKI/AAPL"), 
                   start_date=startdate, end_date=enddate, collapse = "quarterly")


# get the dataset separately
apple <- Quandl("WIKI/AAPL", start_date=startdate, end_date=enddate, type="xts")
GDP <- Quandl("FRED/GDP", start_date=startdate, end_date=enddate, type="xts")



# format the date
# index(GDP) <- as.yearmon(index(GDP))
index(GDP) <- as.Date(index(GDP), format = "%Y-%b-%d")
head(GDP)


# apple quarterly aggregation for subset
appleQ_AdjClose <- apply.quarterly(apple$`Adj. Close`, mean, na.rm=TRUE)


# Create regular xts series with the start and end dates
start  <- start(apple)
end  <- end(GDP)
rindex <- seq(from = start, to = end, by = "day")

# Create a zero-width xts object
regular  <- xts(, order.by = rindex )



# merge both timeseries datasets
appleGdp2  <-  merge(GDP, appleQ_AdjClose, fill = na.locf)
apple44 <-  appleGdp2[index(appleQ_AdjClose)]  
head(apple44)



# frorm last moving forward
apple55  <- na.locf(merge(GDP, appleQ_AdjClose),
                    fromLast = TRUE, na.rm = FALSE)[index(GDP)]

head(apple55)




# Specify a particular column
Facebook_AdjCloseDirect <- Quandl("WIKI/FB.11", type = "xts")
head(Facebook_AdjCloseDirect)

Facebook<- Quandl("WIKI/FB", type = "xts")
Facebook_AdjCloseLater  <-  Facebook$`Adj. Close` 
head(Facebook_AdjCloseLater)



# Calculate returns
# “cumul” gives the cumulative sum, and “rdiff_from” gives each value 
# as the percent difference between itself and the last value in the series. 
# “diff”, “rdiff”, “normalize”, “cumul”, “rdiff_from”
Facebook2 <- Quandl("WIKI/FB.11", transform = "rdieff", type = "xts")
head(Facebook2)
plot(Facebook2)


Quandl("FRED/GDP", start_date=startdate, end_date=enddate)



plot(data2$`Adj. Close`)
plot(data2$Open)
plot(data2$High)



