
# Load the required packages
library(quantmod, quietly=T)

# set my file path
currentdir <- getwd()
filepath = file.path(currentdir,"packages_reviews", "quantmod.rda")

# Download datasets
startDate <- "2018-02-01"
endDate <- "2018-12-30"

getSymbols("JPM", src="yahoo", from=startDate, to=endDate )
getSymbols("^GSPC", src="yahoo", from=startDate, to=endDate)
getSymbols("DEXUSEU", src="FRED")
getSymbols("USD/CNH", src="oanda")
getSymbols("XPT/USD", src="oanda")


# set multiple dataset lookup  
setSymbolLookup(GOOG="yahoo", XPTUSD=list(name="XPT/USD",src="oanda"))
setSymbolLookup(DEXUSEU="FRED", AMZN="yahoo", CSUSHPINSA="FRED")
saveSymbolLookup(file=filepath)

use the lookup.list
getSymbols(c("GOOG", "AMZN", "XPTUSD", "CSUSHPINSA", "DEXUSEU"))




