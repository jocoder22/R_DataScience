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

# dev.off()
# select required feature
par(mfrow = c(2,1))
par(mar = c(1,1,1,1), oma = c(1, 1, 1, 1))
tesla <- TSLA[,"TSLA.Adjusted"]



# calculate the daily simple return
teslaReturn <- na.omit(CalculateReturns(tesla, method="simple"))
colnames(teslaReturn) <- "Return"
head(teslaReturn)


# plot close price and Return
plot(teslaReturn)
acf2(teslaReturn)
dev.off()

# create model specs  
model1 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "norm")

model2 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "sstd")

model3 <- ugarchspec(mean.model = list(armaOrder=c(0,0)),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "sstd")

model4 <- ugarchspec(mean.model = list(armaOrder=c(0,0), archm=TRUE, archpow=2),
                         variance.model = list(model="gjrGARCH"),
                         distribution.model = "sstd")

params <- list(data=teslaReturn, n.start = 2000,
                 refit.window = "moving", refit.every = 100)


# report(model1)
# summary(model1)
# coef(model1)
# show(model1)

betterModel <- function(mod1, mod2, ddata){
  
  start <- length(ddata) - 1000;
  
  params <- list(data=ddata, n.start = start,
                 refit.window = "moving", refit.every = 100)
  
  result1 <- do.call(ugarchroll, c(mod1, params));
  result2 <- do.call(ugarchroll, c(mod2, params));
  
  
  pred1 <- as.data.frame(result1);
  pred2 <- as.data.frame(result2);
  
  
  e1 <- mean((pred1$Realized - pred1$Mu)^2);
  e2 <- mean((pred2$Realized - pred2$Mu)^2);
  
  m11 <- deparse(substitute(mod1));
  m22 <- deparse(substitute(mod2));
  
  
  if (e1 > e2){
    cat(sprintf("%s is the better model.\n", m22));
  }
  else cat(sprintf("%s is the better model.\n", m11));
  
  
}

betterModel(model4, model3, teslaReturn)
