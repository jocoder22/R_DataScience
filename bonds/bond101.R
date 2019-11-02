library(Quandl)
library(xts)
library(quantmod)



setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("bondFunction.R")

bondprice(1100, 0.051, 10, 0.07)


# Obtain Moody's Baa  and Aaa index data
startdate = "2003-01-01"
baa <- Quandl("FED/RIMLPBAAR_N_M", start_date=startdate)
aaa <- Quandl("FED/RIMLPAAAR_N_M", start_date=startdate)



# Moody's Seasoned Aaa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity
aaa10ym <- Quandl("FRED/AAA10YM", start_date=startdate, type="xts")
plot(aaa10ym, main = "Moody's Seasoned Aaa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity")


# Moody's Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity
baa10ym <- Quandl("FRED/BAA10YM", type="xts")
plot(baa10ym, main = "Seasoned Baa Corporate Bond Yield Relative to Yield on 10-Year Treasury Constant Maturity")


baa_aaa <- baa10ym - aaa10ym
plot(baa_aaa, main = 'Spread: Baa vs Aaa difference')

# Form xts object
rating <- as.xts(data.frame(aaa$Value, baa$Value), order.by = aaa$Date) / 100
colnames(rating) <- c("Aaa", "Baa")

# plot the rating yields
plot(rating, legend.loc = "topright", main = "Aaa rated Bond Yield vs Baa rated Bond Yield")


# compute rating difference
rating$Diff <- rating$Baa - rating$Aaa 

plot(rating$Diff, main = " Difference in Yield: Aaa rated Bond vs Baa rated Bond")

# Identify 2018-06-24 yield
yield20180624 <- subset(rating, index(rating) == "2015-05-31")

yield20180624



# Obtain Treasury yield data
treasury10yr <- getSymbols(Symbols = "DGS10", src = "FRED", auto.assign = FALSE)
plot(treasury10yr, main = "Ten year US Treasury bill yield")
