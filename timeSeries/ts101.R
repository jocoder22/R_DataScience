
options("getSymbols.warning4.0"=FALSE)
par(opar)
# load library
library("quantmod")


# download Dow Jones data
getSymbols("^DJI", src="yahoo")


dow_jones <- DJI[, "DJI.Close"]


# Plot the DJI close
plot(dow_jones)
par(mfrow=c(2,1))
acf(dow_jones)
pacf(dow_jones)


# plot the detrending transformations
plot(diff(dow_jones))
par(mfrow=c(2,1))
plot(log(dow_jones))
plot(diff(log(dow_jones)))


# plot the diff(1)
dow_diff <- diff(dow_jones)
dow_diff <- na.omit(dow_diff)
par(mfrow=c(2,1))
acf(dow_diff)
pacf(dow_diff)



# plot the diff(log(x))
dow_difflog <- na.omit(diff(log(dow_jones)))
par(mfrow=c(2,1))
acf(dow_difflog)
pacf(dow_difflog)