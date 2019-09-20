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
# 1.2 Calculate Stock volatility
# 1.3 Calculate Daily stock returns
