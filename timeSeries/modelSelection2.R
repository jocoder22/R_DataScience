###########################################################
# Here we explore Garch-in-mean model using rugarch package
library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(rugarch, quietly = T)
library(dplyr, quietly = T)
library(glue, quietly = T)
library(forecast, quietly = T)
library(glue, quietly = T)

# download dataset
getSymbols("TSLA", src="yahoo")

dev.off()
# select required feature
par(mfrow = c(2,1))
par(mar = c(1,1,1,1), oma = c(1, 1, 1, 1))
tesla <- TSLA[,"TSLA.Adjusted"]



# calculate the daily simple return
teslaReturn <- na.omit(CalculateReturns(tesla, method="simple"))
colnames(teslaReturn) <- "Return"
head(teslaReturn)


# plot close price and Returns
plot(tesla)
plot(teslaReturn)
acf2(teslaReturn)
dev.off()

# create model specs  
garchspec1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "norm")

garchspec2 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "sstd")

garchspec3 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "sstd")

garchspec4 <- ugarchspec(mean.model = list(armaOrder=c(0,0), archm=TRUE, archpow=2),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "sstd")

params <- list(data=teslaReturn, n.start = 2000,
                 refit.window = "moving", refit.every = 100)
