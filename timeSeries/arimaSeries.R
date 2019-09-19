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
