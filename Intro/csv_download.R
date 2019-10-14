library(tseries)
library (RCurl)


data  <- read.csv("http://www.nasdaq.com/quotes/nasdaq-100-stocks.aspx?render=download")
download2 <- read.csv("http://apps.fs.fed.us/fiadb-downloads/CSV/LICHEN_SPECIES_SUMMARY.csv")


download <- getURL("https://data.kingcounty.gov/api/views/yaai-7frk/rows.csv?accessType=DOWNLOAD")
data2 <- read.csv(text = download)

  
