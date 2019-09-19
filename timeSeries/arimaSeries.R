# load requried library  
library(ggplot2)

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