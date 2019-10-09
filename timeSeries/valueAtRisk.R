library(quantmod, quietly = T)
library(PerformanceAnalytics, quietly = T)
library(forecast, quietly = T)
library(xts, quietly = T)
library(FinTS, quietly = T)
library(fGarch, quietly = T)
library(rugarch, quietly = T)
library(e1071, quietly = T)

# Download datasets
startDate <- "2010-01-01"
endDate <- "2019-09-27"


apple <- getSymbols("AAPL", src="yahoo", auto.assign=F, 
                    from=startDate, to=endDate)$AAPL.Adjusted

head(apple)

kurtosis(apple)
skewness(apple)
e1071::kurtosis(apple)
e1071::skewness(apple)
mean(apple)
sd(apple)


appleR <- dailyReturn(apple)
kurtosis(appleR)
skewness(appleR)
e1071::kurtosis(appleR)
e1071::skewness(appleR)
mean(appleR)
sd(appleR)



# model specification
garchspecAR22 <- ugarchspec(mean.model = list(armaOrder=c(2,2)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")

# 
# garchspecAR23 <- ugarchspec(mean.model = list(armaOrder=c(2,3)),
#                             variance.model = list(model="gjrGARCH"),
#                             distribution.model = "sstd")

garchspecAR24 <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")

garchspecAR32 <- ugarchspec(mean.model = list(armaOrder=c(3,2)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")

garchspecAR33 <- ugarchspec(mean.model = list(armaOrder=c(3,3)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")

garchspecAR34 <- ugarchspec(mean.model = list(armaOrder=c(3,4)),
                            variance.model = list(model="gjrGARCH"),
                            distribution.model = "sstd")

garchspecA0 <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                          variance.model = list(model="gjrGARCH"),
                          distribution.model = "std")

garchspecI<- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                        variance.model = list(model="iGARCH"),
                        distribution.model = "std")

garchspecS <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                         variance.model = list(model="sGARCH"),
                         distribution.model = "std")

garchspecE <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                         variance.model = list(model="eGARCH"),
                         distribution.model = "std")

garchspecT <- ugarchspec(mean.model = list(armaOrder=c(2,4)),
                         variance.model = list(model="fGARCH", submodel="TGARCH"),
                         distribution.model = "std")


models <- c(garchspecAR22, garchspecAR24, garchspecAR33, garchspecAR32, garchspecAR34, 
            garchspecA0, garchspecI, garchspecS, garchspecE, garchspecT)



ValueR <- function(v, ddata){
  n <- 1 ;
  for(specv in v){

    params <- list(data=ddata, n.start = 1800,
                   refit.window = "moving", refit.every = 100);

    garchrollm <- do.call(ugarchroll, c(specv, params));
    vv <- deparse(substitute(v[n]));

    garchvar <- quantile(garchrollm, prob = 0.05);
    actual <- as.data.frame(garchrollm)$Realized;
    vmean <- mean(actual < garchvar);
    
    cat(sprintf("Value at Risk for %s is %s.\n\n", vv, round(vmean, 4)));
    n <- n + 1;
    
  }
}

ValueR(models, appleR)



