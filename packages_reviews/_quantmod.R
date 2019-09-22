library(quantmod)


# Download datasets
startDate <- "2018-02-01"
endDate <- "2018-12-30"

getSymbols("JPM", src="yahoo", from=startDate, to=endDate )
getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
getSymbols("DEXUSEU",src="FRED")
getSymbols("XPT/USD",src="Oanda")