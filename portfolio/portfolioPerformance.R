# Load the required packages
library(quantmod, quietly = TRUE)
library(PerformanceAnalytics, quietly = TRUE)
library(ROI, quietly = TRUE)
library(timeSeries, quietly = TRUE)
library(tseries, quietly = TRUE)
library(xts, quietly = TRUE)
library(PortfolioAnalytics, quietly = TRUE)


# load the portfolio returns
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
returns <- readRDS("Returns.rds")
head(returns)

# Specify the portfolio optimization
pf_spec <- portfolio.spec(colnames(returns))


# add constraints 
pf_spec  <- add.constraint(portfolio = pf_spec, type = "full_investment")
pf_spec <- add.constraint(portfolio = pf_spec, type = "long_only")


# add objectives
pf_spec <- add.objective(portfolio = pf_spec, type = "risk", name = "StdDev")
pf_spec <- add.objective(portfolio = pf_spec, type = "return", name = "mean")


# solve the portfolio optimization
opt_port <- optimize.portfolio(R = returns, portfolio = pf_spec, optimize_method = "ROI")


# view the portfolio specification and optimium 
print(pf_spec)
print(opt_port)


# Extract the optimal weights
extractWeights(opt_port)

# Chart the optimal weights
chart.Weights(opt_port)


# compute portfolio returns
opt_pfr <- Return.portfolio(returns, weights = extractWeights(opt_port))


charts.PerformanceSummary(opt_pfr, mar = c(4,4,0,0), oma = c(1, 0, 0, 0))
