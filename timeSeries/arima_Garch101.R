require(quantmod)
require(PerformanceAnalytics)
require(astsa)
require(tseries)
require(forecast)



ticker_s <- "MSFT" ##"^FTSE"
stock  <- getSymbols(ticker_s, auto.assign = FALSE) %>% Ad()
 
ft = na.omit(diff(stock))

plot(ft)
plot(ft)
    

autoArima2 <- function(xtsx){  
    fttt <- as.numeric(xtsx)
    fttt <- fttt[!is.na(ft)]
    
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
    
    
    
        arimaFit = tryCatch(arima(fttt, order=c(p, d, q)),
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
    
    final.arima <- arima(fttt, order=final.order)
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



autoArima2(ft)
ft.garch <- garch(ft, trace=F)
ft.res <- ft.garch$res[-1]
ft.garch

acf2(ft.res)


ppp  <- auto.arima(ft, stepwise = FALSE)
auto.arima(ft, stepwise = FALSE)
ppp$arma
  
ppp$arma
