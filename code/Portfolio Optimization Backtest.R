
# ------------------------------------------------------------------------- #

# Portfolio Optimization Backtest (Quant Finance with R Part 4)
# Youtube Search: "Quant Finance with R Part 4 Portfolio Optimization Backtest"
# Code was also provided on Github

# Walk-forward optimization
# - Multiple time slices were created with each slice containing an 
#   Optimization Period (training set) and Run Period (test set)
# - Portfolio performance was computed on the test set
# - Includes rebalancing, transaction costs
# - Optimal weight is recomputed for each time slice on the training data

# ------------------------------------------------------------------------- #

# Load packages
library(quantmod)
library(PerformanceAnalytics)
library(PortfolioAnalytics)

tickers <- c("FB", "AAPL", "AMZN")

portfolioPrices <- NULL
for(ticker in tickers) {
    portfolioPrices <- cbind(portfolioPrices,
                             getSymbols.yahoo(ticker, from = '2016-01-03', 
                                              periodicity = "daily", auto.assign = FALSE)[, 4])
}

portfolioReturns <- na.omit(ROC(portfolioPrices))

portf <- portfolio.spec(colnames(portfolioReturns))

portf <- add.constraint(portf, type = "weight_sum", min_sum = 0.99, max_sum = 1.01)
portf <- add.constraint(portf, type = "transaction_cost", ptc = 0.001)
portf <- add.constraint(portf, type = "box", min = 0.10, max = 0.40)

portf <- add.obective(portf, type = "return", name = "mean")
portf <- add.obective(portf, type = "risk", name = "StdDev", target = 0.005) # Daily volatility target

# Get optimal portfolio weights - using ROI optimizer
# optPort <- optimize.portfolio(portfolioReturns, portf, optimize_method = "ROI", trace = TRUE)

# Generat 10,000 random portfolios
rp <- random_portfolios(portf, 10000, "sample")

opt_rebal <- optimize.portfolio.rebalancing(portfolioReturns,
                                            portf,
                                            search_size = 5000, # default is 20000, so shrink to speed up process
                                            optimize_method = "random",
                                            rp = rp,
                                            rebalance_on = "months",
                                            training_period = 1, # months
                                            rolling_window = 10) # months

# chart.Weights(optPort)
# 
# ef <- extractEfficientFrontier(optPort, match.col = "StdDev", n.portfolios = 25,
#                                risk_aversion = NULL)
# 
# chart.EfficientFrontier(ef,
#                         match.col = "StdDev", n.portfolios = 25, xlim = NULL, ylim = NULL,
#                         cex.axis = 0.8, element.color = "darkgray", 
#                         main = "Efficient Frontier", RAR.text = "SR", rf = 0, 
#                         tangent.line = TRUE, cex.legend = 0.8, 
#                         chart.assets = TRUE, labels.assets = TRUE, pch.assets = 21,
#                         cex.assets = 0.8)

# Create equal weight portfolio
equal_weight <- rep(1 / ncol(portfolioReturns), ncol(portfolioReturns))

benchmark <- Return.portfolio(portfolioReturns, weights = equal_weight)
colnames(benchmark) <- "Benchmark Portfolio"

sp500prices <- getSymbols("SPY", from = '2016-01-03', 
                          periodicity = "daily", auto.assign = FALSE)[, 4]
sp500Rets <- na.omit(ROC(sp500Prices))
sp500Rets <- as.xts(sp500Rets)


chart.Weights(opt_rebal, main = "Rebalanced Weights Over Time")

rebal_weights <- extractWeights(opt_rebal)
rebal_returns <- Return.portfolio(portfolioReturns, weights = rebal_weights)

rets_df <- cbind(rebal_returns, benchmark, sp500Rets)

charts.PerformanceSummary(rets_df, main = "P/L Over Time")