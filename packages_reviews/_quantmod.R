
# Load the required packages
library(quantmod, quietly=T)



# Download datasets
startDate <- "2018-02-01"
endDate <- "2018-12-30"

getSymbols("JPM", src="yahoo", from=startDate, to=endDate )
getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
getSymbols("DEXUSEU", src="FRED")
getSymbols("USD/CNH", src="oanda")
getSymbols("XPT/USD", src="oanda")


# set multiple dataset lookup  
setSymbolLookup(YHOO="google", GOOG="yahoo", CHF/USD="oanda")
setSymbolLookup(DEXUSJP="FRED", AMZN="yahoo", CSUSHPINSA="FRED")
saveSymbolLookup(file=)