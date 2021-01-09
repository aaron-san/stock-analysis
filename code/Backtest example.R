
# Backtest

# Step 1: Load libraries and data
library(quantmod)
library(tidyverse)
library(PerformanceAnalytics)

# symbols_yahoo <- c("SPY")
# symbols_fred <- c("NFCI")

# prices_fred <- getSymbols(symbols_fred, src = 'FRED', from = '1999-12-31')
# prices_fred <- prices_fred %>% map(~get(.)) %>% reduce(merge) %>% setNames(symbols_fred)
# saveRDS(prices_fred, "data/prices_fred.rds")
# prices_fred <- read_rds("data/prices_fred.rds")

# prices_yahoo <- getSymbols(symbols_yahoo, src = 'yahoo', from = '1999-12-31')
# prices_yahoo <- prices_yahoo %>% map(~Ad(get(.))) %>% reduce(merge) %>% setNames(symbols_yahoo)
# saveRDS(prices_yahoo, "data/prices_yahoo.rds")
# prices_yahoo <- read_rds("data/prices_yahoo.rds")

adj_prices <- read_rds("data/adj_prices.rds") %>% na.omit()


data <- 
    adj_prices %>% 
    .[endpoints(., on = "months")]
tzone(data) <- "UTC"

returns <- ROC(data, type = "discrete")

cash_returns <- returns[, "SHV"]
asset_returns <- returns[, setdiff(colnames(adj_prices), "SHV")]

# saveRDS(cash_returns, "data/cash_returns.rds")
# saveRDS(asset_returns, "data/asset_returns.rds")




# Define signal
get_signal <- function(x) {
    if_else(x < 0, 1, 0)
}

# Step 2: Create your indicator
signals <-
    asset_returns %>%
    apply(., 2, get_signal) %>%
    as.xts(order.by = index(data))

get_weights <- function(x, data) {
    x / rowSums(data)
}

wts <-
    signals %>%
    apply(., 2, function(x) get_weights(x, data = signals)) %>%
    # round(3) %>%
    as.xts(order.by = index(data))
wts[is.nan(wts)] <- 0
wts <- wts %>% na.omit()

# rowSums(wts)
wts$CASH <- 1 - rowSums(wts)


# Signals are generated the monthly before performance is earned
strat_returns <-
    (merge(asset_returns, cash_returns) * stats::lag(wts) - 0.0005) %>%
    rowSums() %>%
    xts(order.by = index(wts)) %>%
    na.omit()
colnames(strat_returns) <- "Strategy"



# Step 3: Use indicator to create equity curves
rets <- merge(returns$SPY, strat_returns) %>% na.omit()

# # Step 4: Evaluate strategy performance
# table.DownsideRisk(rets)
table.Stats(rets)
charts.PerformanceSummary(rets, wealth.index = TRUE)
# chart.RelativePerformance(rets[ , 2], rets[ ,1])
# chart.RiskReturnScatter(rets)

Return.annualized(rets)
SharpeRatio.annualized(rets)
Return.annualized(rets)/maxDrawdown(rets)
maxDrawdown(rets)



apply.yearly(rets, Return.cumulative) # geometric annual returns
apply.yearly(rets, maxDrawdown)
apply.yearly(rets, SharpeRatio.annualized)

cor(rets)

hist(rets$SPY, col="grey")
hist(rets$strat_returns, col="grey")
qqplot(rets$SPY, rets$strat_returns)
qqnorm(rets$SPY)
qqline(rets$strat_returns)


                                                                                      