# load required packages
library(quantmod)


# Download datasets
startDate <- "2018-02-01"
endDate <- "2018-12-30"

getSymbols("JPM", src="yahoo", from=startDate, to=endDate )
JPMorgan <- JPM[, "JPM.Adjusted", drop = FALSE]

getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
SP500 <- GSPC[, "GSPC.Adjusted", drop=FALSE]

allData <- data.frame(JPMorgan, SP500)


# Explore the dataset
head(allData)
str(allData)
summary(allData)

# 1.1 Calculate Average stock value
average_value <- mean(JPMorgan)
average_value
# [1] 107.2015

# 1.2 Calculate Stock volatility
stock_volatility <- sd(JPMorgan)
stock_volatility
# [1] 4.56665


# 1.3 Calculate Daily stock returns
# 1.3.1 Daily simple returns
simple_returns <- diff(JPMorgan)/lag(JPMorgan)[-1]
names(simple_returns) <- "JPM.simpleReturns"
head(simple_returns)

#             JPM.simpleReturns
# 2018-02-02      -0.022161369
# 2018-02-05      -0.047952357
# 2018-02-06       0.030422834
# 2018-02-07       0.006779018
# 2018-02-08      -0.044210207
# 2018-02-09       0.020022238


# 1.3.2 Daily continous returns
cont_returns <- diff(log(JPMorgan))[-1]
names(cont_returns) <- "JPM.contReturns"
head(cont_returns)
#            JPM.contReturns
# 2018-02-02    -0.022410622
# 2018-02-05    -0.049140201
# 2018-02-06     0.029969236
# 2018-02-07     0.006756144
# 2018-02-08    -0.045217271
# 2018-02-09     0.019824429