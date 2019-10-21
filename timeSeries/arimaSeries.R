# load requried library  
# install.packages('astsa')
library(tseries)
library(aTSA)
library(astsa)


# Compute the k
findK  <- function(HLC){
  n  <- dim(HLC)[1]
  f  <-  frequency(HLC)
  k  <- 12 * (n / 100) ^ 0.25
  return(floor(k))
}


# explore the time series dataset 
start(sunspots) # shows the start time
end(sunspots)   # shows the end time
frequency(sunspots)   # shows the reoccuring frequency


# deltat is the fraction of the sampling period between successive observations; 
# e.g., 1/12 for monthly data
deltat(sunspots)      # shows the time interval between 2 measurement 
time(sunspots)
cycle(sunspots)


# do more exploration
head(sunspots)
class(sunspots)
str(sunspots)
length(sunspots)

# Check the class and plot the dataset
is.ts(sunspots)
ts.plot(sunspots)


# subset the dataset
sunspot_trun <- sunspots[c(1759:1800)] 

# subset of dataset is not a ts object
is.ts(sunspot_trun)
class(sunspot_trun)

# convert to ts object and plot the subset
sunspot_trun <- ts(sunspot_trun, start=1759, frequency = 1)
ts.plot(sunspot_trun)


# the dataset have seasonal components
# Remove the seasonal element using diff and lag
without_seasons <- diff(sunspots)
sun_lag12 <- diff(sunspots, lag=12)
ts.plot(without_seasons)


# plot acf and pacf
acf2(sunspots, max.lag = 60)
acf(sunspots, lag.max = 40)
pacf(sunspots, lag.max = 40)

acf2(without_seasons, max.lag = 60)
acf(without_seasons, lag.max = 40)
pacf(without_seasons, lag.max = 40)

acf2(diff(sun_lag12), max.lag = 60)
acf(sun_lag12, lag.max = 40)
pacf(sun_lag12, lag.max = 40)

# do stationarity tests 
adf.test(sunspots)
kpss.test(sunspots)


adf.test(without_seasons)
kpss.test(without_seasons)

adf.test(sun_lag12)
kpss.test(sun_lag12)

# using composite test using defined function
allTest <- function(x){
  testVector <- c("adf", "pp", "kpss")
  for (val in testVector){
  stationary.test(x, method = val)
  }
}



# Apply composite test
allTest(without_seasons)
allTest(sunspots)
allTest(sun_lag12)

sunlog <- log(sunspots)
sarima(sunspots,2,1,2,0,1,1,12)

sarima(sunspots,2,1,2,0,1,3,36)

frequency(sunspots)

plot(sunspots)
plot(log(sunspots))
plot(diff(diff(sunspots), lag=12))

df <- diff(diff(sunspots), lag=12)
acf2(df, max.lag = 60)
