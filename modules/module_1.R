
# Load required packages
library(readxl)
library(TTR)
library(zoo)
library(xts)
library(quantmod)

# Import dataset
FinData <- read_excel("~/WorldQuant/MScFE 610 Econometrics/Datasets/WQU_Econometrics_Module1_Data.xlsx", 
                        col_types = c("date", "numeric", "numeric", "numeric", 
                                      "numeric", "numeric", "numeric", "numeric", "numeric"))

# view the dataset
View(FinData)

# find mean of MSFTclosing price
mean(FinData$MSFT)

var(FinData$MSFT)
sd(FinData$MSFT)

n <- nrow(FinData)
FinData$MSFT[2:n]
FinData$returns <- ROC(FinData$MSFT)
FinData$returns <- FinData$returns[-1]

# FinData$returns2 <- ((FinData$MSFT[2:n] - FinData$MSFT[1:(n-1)])/FinData$MSFT[1:(n-1)])
cbind(FinData[-1,1],apply(FinData[,-1],2,function(x) diff(x)/head(x,-1)))
apply(FinData$MSFT[,-1],1,function(x) diff(x)/head(x,-1))
dailyReturn(FinData$MSFT,  subset=NULL, type='arithmetic',leading=TRUE)

x <- xts(FinData[,-1], FinData[,1])
periodReturn(x,period='daily')

Return.calculate(xts(FinData$MSFT), method="discrete")

FinData$returns2 <- diff(FinData$MSFT)/lag(FinData$MSFT)







require(quantmod)
getSymbols("IBM", from = "1999-01-01", to = "2007-01-01")
prices <- IBM[, "IBM.Close"]
Returns <- diff(prices)/lag(prices)
Returns <- Returns[-1]



# }
# NOT RUN {

# }
# NOT RUN {
R.IBM = Return.calculate(prices, method="discrete")
colnames(R.IBM)="IBM"
chart.CumReturns(R.IBM,legend.loc="topleft", main="Cumulative Daily Returns for IBM")
round(R.IBM,2)
# }