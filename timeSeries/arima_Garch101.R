require(quantmod)
require(PerformanceAnalytics)
require(astsa)
require(tseries)
require(forecast)
getSymbols("^FTSE")
 
ftrt = diff(Cl(FTSE))



plot(Cl(FTSE))
aplot(ftrt)
    

autoArima2 <- function(xtsx){  
    ft <- as.numeric(xtsx)
    ft <- ft[!is.na(ft)]
    
    final.aic <- Inf
    final.order <- c(0,0,0)
    for (p in 1:4) for (d in 0:1) for (q in 1:4) {
    #   ftcurrent.aic <- AIC(arima(ft, order=c(p, d, q)))
    #   if (ftcurrent.aic < ftfinal.aic) {
    #     ftfinal.aic <- ftcurrent.aic
    #     ftfinal.order <- c(p, d, q)
    #     ftfinal.arima <- arima(ft, order=ftfinal.order)
    #   }
    # }
    
    
    
        arimaFit = tryCatch(arima(ft, order=c(p, d, q)),
                        error=function( err ) FALSE,
                        warning=function( err ) FALSE )

        if(!is.logical(arimaFit)){
             current.aic <- AIC(arimaFit)
             if (current.aic < final.aic) {
               final.aic <- current.aic
               final.order <- c(p, d, q)
               # final.arima <- arima(ft, order=final.order)
             }
        }
         # } else {
         #   next
         # }
         # 
    }
    
    final.arima <- arima(ft, order=final.order)
    cat("Final ARIMA Order:\n ArmaOrder[",final.order[1],",",final.order[2],",", 
        final.order[3],"]\n\n")
    
  
    print(final.arima)
    
    par(mfrow=c(2,2))
    acf(resid(final.arima))
    acf(resid(final.arima)^2)
    pacf(resid(final.arima))
    pacf(resid(final.arima)^2)
    
    par(mfrow=c(1,1))
}   


