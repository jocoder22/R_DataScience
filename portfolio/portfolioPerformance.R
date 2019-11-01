# Load the required packages
library(quantmod, quietly = TRUE)
library(PerformanceAnalytics, quietly = TRUE)
library(ROI, quietly = TRUE)
library(timeSeries, quietly = TRUE)
library(tseries, quietly = TRUE)
library(xts, quietly = TRUE)
library(PortfolioAnalytics, quietly = TRUE)
library(MASS, quietly = TRUE)

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

# plot portfolio perfomance
charts.PerformanceSummary(opt_pfr, mar = c(4,4,0,0), oma = c(1, 0, 0, 0))


# Print the portfolio specification object
print(pf_spec)


# Design assets moments
# Fit a statistical factor model with k = 4 factors to the asset returns
fitss <- statistical.factor.model(R = returns, k = 4)

# compute the portfolio moments using the "boudt" method with k = 4 factors
moments_boudt <- set.portfolio.moments(R = returns, portfolio = pf_spec, method = "boudt", k = 4)

# compute the portfolio moments using the "black_litterman" method 
moments_bk <- set.portfolio.moments(R = returns, portfolio = pf_spec, method = "black_litterman")

# compute the portfolio moments using the "meucci" method 
moments_meucci <- set.portfolio.moments(R = returns, portfolio = pf_spec, method = "meucci")


# Check if the covariance matrix extracted from the model fit is equal to the estimate in moments_boudt
moments_boudt$sigma == extractCovariance(fitss)


# view mean and standard deviations
moments_boudt$mu; moments_boudt$sigma
moments_meucci$mu; moments_meucci$sigma
moments_bk$mu; moments_bk$sigma
