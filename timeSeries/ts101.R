
options("getSymbols.warning4.0"=FALSE)

library("quantmod")

getSymbols("^DJI", src="yahoo")


dow_jones <- DJI[, "DJI.Close"]

dow_jones

plot(dow_jones)