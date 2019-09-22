# Load the required packages
library(quantmod, quietly=T)
library(PerformanceAnalytics, quietly = T)
require(TTR)

# set my file path
currentdir <- getwd()
filepath = file.path(currentdir,"timeSeries", "garch.rds")

# Download datasets
startDate <- "2018-02-01"
endDate <- "2018-12-30"

getSymbols("^GSPC", src="yahoo", from=startDate)

sp500 <- GSPC$GSPC.Adjusted

head(sp500)
