

# Prepare workspace -------------------------------------------------------

# Clear workspace
rm(list = ls(all = TRUE))

library(tidyverse)
library(lubridate)
library(quantmod)
library(PerformanceAnalytics)
library(slider)
library(glue)

# load custom functions ---------------------------------------------------
source("code/backtest_function.R")

# Backtest parameters -----------------------------------------------------
# start_date  <- as.Date("2007-04-01")
# end_date    <- as.Date("2014-12-31")

## Define frictions (as percentage of trade) (% of returns???)
commission_pct <- c(.0003)
slippage_pct <- c(.0005)
friction_pct <- commission_pct + slippage_pct

## Portfolio Symbols
universe <- c("SPY", "MDY", "IWM",      # Large-Mid-Small Cap equities
              "EFA", "EEM",             # Intl.and emerging markets
              "AGG","TIP","TLT", "LQD", # Bonds
              "GSG",                    # Commodities
              "RWR", "RWX",  "MBB")     # Real Estate

## Cash 
cash_symbol <- c("SHV")

## Portfolio
portfolio_symbols <- c(universe, cash_symbol)

# Get prices from Yahoo!
# prices <- getSymbols(portfolio_symbols, from = "1999-12-31")
# close_prices <- prices %>% map(~Cl(get(.))) %>% reduce(merge) %>% setNames(portfolio_symbols)
# adj_prices <- prices %>% map(~Ad(get(.))) %>% reduce(merge) %>% setNames(portfolio_symbols)
# saveRDS(close_prices, "data/close_prices.rds")
# saveRDS(adj_prices, "data/adj_prices.rds")


close_prices_raw <- read_rds("data/close_prices.rds")
# tail(close_prices_raw)

# write_csv(as.data.frame(close_prices) %>% rownames_to_column("date"), "excel model/close prices.csv")
# write_csv(as.data.frame(adj_prices) %>% rownames_to_column("date"), "excel model/adjusted prices.csv")

close_prices <-
    close_prices_raw %>% 
    as_tibble() %>%
    mutate(date = index(close_prices_raw)) %>%
    pivot_longer(-date, names_to = "ticker", values_to = "close")
        
adj_prices_raw <- read_rds("data/adj_prices.rds") 
# tail(adj_prices_raw)
adj_prices <-
    adj_prices_raw %>% 
    as_tibble() %>%
    mutate(date = index(adj_prices_raw)) %>%
    pivot_longer(-date, names_to = "ticker", values_to = "adj_close")

# Cash (SHV) returns
cash_prices_monthly <-
    inner_join(close_prices, adj_prices, by = c("date", "ticker")) %>% 
    filter(ticker == cash_symbol) %>% 
    slice(endpoints(date, on = "months")) %>% 
    mutate(cash_return_daily = ROC(adj_close)) %>%
    drop_na() %>%
    pivot_wider(names_from = "ticker", values_from = "cash_return_daily") %>% 
    select(-close, -adj_close)

cash_rets_monthly <-
    cash_prices_monthly %>% 
    select(-date) %>% 
    xts(., order.by = cash_prices_monthly$date)
    
daily_data <- 
    inner_join(close_prices, adj_prices, by = c("date", "ticker")) %>% 
    filter(ticker != cash_symbol) %>% 
    arrange(ticker, date) %>% 
    group_by(ticker) %>% 
    mutate(# Calculate daily returns
           return_daily = ROC(adj_close)) %>% 
    drop_na() %>% 
    mutate(# Calculate momentum (4-month SMA of daily returns)
           sma_daily = SMA(return_daily, n = 84),
           # Calculate rolling volatility (4-month running SD of daily returns)
           runSD_daily = runSD(return_daily, n = 84)) %>%
    drop_na() 

daily_data <-
    daily_data %>% 
    group_by(date) %>% 
    arrange(date) %>% 
    mutate(# Lower volatility is better
           vol_rank = row_number(-runSD_daily),
           # Higher SMA is better
           mom_rank = row_number(sma_daily)) %>% 
    group_by(ticker) %>% 
    filter(date >= slice(., 1) %>% pull(date) %>% max(),
           date <= slice(., n()) %>% pull(date) %>% min())


# daily_data %>% View()


monthly_data <-
    daily_data %>% 
    # Convert to monthly
    slice(endpoints(date, on = "months")) %>% 
    group_by(ticker) %>% 
    # Calculate monthly returns
    mutate(return_monthly = ROC(adj_close)) %>% 
    drop_na() %>% 
    arrange(ticker, date)

# monthly_data %>% pull(date) %>% unique()

# Market timing -----------------------------------------------------------

## Highest Momentum (ROC), 
## Lowest Volatility (SD), 
## Lowest Avg. Correlation
## 4-month look back (21 days per month * 4 = 84) or (252 days * 4/12 = 84 days)

mom_rank <- 
    daily_data %>% 
    select(date, ticker, mom_rank) %>% 
    pivot_wider(names_from = ticker, values_from = mom_rank)

vol_rank <-
    daily_data %>% 
    select(date, ticker, vol_rank) %>% 
    pivot_wider(names_from = ticker, values_from = vol_rank)


# 4-month rolling "average" correlation of each asset paired with each of the other assets

# combinations by date
pairwise_combos <- 
    daily_data %>%
    ungroup() %>% 
    select(ticker, date, return_daily) %>%
    full_join(., y = ., by = "date") %>%
    # drop diagonal 
    filter(ticker.x != ticker.y) %>%
    # remove duplicate pairs (eg A-AAL, AAL-A)
    mutate(tickers = ifelse(ticker.x < ticker.y,
                            glue("{ticker.x}, {ticker.y}"),
                            glue("{ticker.y}, {ticker.x}"))) %>%
    # Remove duplicate date-ticker pairs
    distinct(date, tickers, .keep_all = TRUE)


pairwise_corrs <- 
    pairwise_combos %>% 
    arrange(tickers, date) %>% 
    group_by(tickers) %>% 
    mutate(rolling_cor = slider::slide2_dbl(
        .x = return_daily.x, 
        .y = return_daily.y, 
        .f = ~suppressWarnings(cor(.x, .y)), 
        .before = 83, 
        .complete = TRUE)
    ) %>%
    ungroup() %>% 
    drop_na() %>% 
    select(date, tickers, return_daily.x, return_daily.y, rolling_cor) %>% 
    drop_na()
    
# pairwise_corrs %>% View()



rolling_correlations_step_1 <- 
    pairwise_corrs %>% 
    separate(tickers, into = c("ticker.x", "ticker.y"))

rolling_correlations <-
    rolling_correlations_step_1 %>% 
    select(-return_daily.x, -return_daily.y) %>% 
    filter(ticker.y == portfolio_symbols %>% max()) %>% 
    set_names("date", "ticker.y", "ticker.x", "rolling_cor") %>% 
    bind_rows(rolling_correlations_step_1, .) %>% 
    group_by(ticker.x, date) %>%
    mutate(mean_cor = mean(rolling_cor, na.rm = TRUE)) %>% 
    distinct(date, ticker.x, mean_cor) %>% 
    arrange(ticker.x) %>% 
    rename(ticker = "ticker.x")



# rolling_correlations %>%
#     ggplot(aes(date, mean_cor, color = ticker)) +
#     geom_line() +
#     theme_bw()

corr_rank <-
    rolling_correlations %>% 
    group_by(date) %>% 
    # Lower correlation is better
    mutate(corr_rank = row_number(-mean_cor)) %>%
    ungroup() %>% 
    arrange(date, ticker) %>%
    select(-mean_cor) %>% 
    pivot_wider(names_from = ticker, values_from = corr_rank)


# Subset the objects
min_date <- corr_rank %>% pull(date) %>% min()
max_date <- corr_rank %>% pull(date) %>% max()

volatility <-
    daily_data %>% 
    select(date, ticker, runSD_daily) %>% 
    pivot_wider(names_from = ticker, values_from = runSD_daily) %>% 
    filter(date >= min_date, date <= max_date) %>% 
    select(-date)


mom_rank <- mom_rank %>% filter(date >= min_date, date <= max_date)
vol_rank <- vol_rank %>% filter(date >= min_date, date <= max_date)

# rownames(all_ranks) <- mom_rank[, 1] %>% pull(date)
# all_ranks %>% View()


# Weight the rankings
all_ranks <- (.50 * mom_rank[, -1]) + (.35 * vol_rank[, -1]) + (.15 * corr_rank[, -1])

# rownames(all_ranks) <- mom_rank[, 1] %>% pull(date)
# all_ranks %>% View()


# corr_rank %>% View()
# Filters
## Returns a matrix of 0s and 1s indicating whether the 4-month SMA daily return
## is positive (1) or negative (0)
moment_filter <-
    daily_data %>%
    select(date, ticker, sma_daily) %>% 
    pivot_wider(names_from = ticker, values_from = sma_daily) %>% 
    filter(date >= min_date, date <= max_date) %>% 
    mutate(across(-date, ~ifelse(.x < 0, 0, 1))) %>% 
    select(-date)

monthly_rets <- 
    monthly_data %>%
    select(date, ticker, return_monthly) %>% 
    pivot_wider(names_from = ticker, values_from = return_monthly)

monthly_rets <- monthly_rets %>% select(-date) %>% xts(., order.by = monthly_rets %>% pull(date))
# saveRDS(monthly_rets, "data/monthly_rets.rds")

# View(monthly_rets)

# charts.PerformanceSummary(port_returns_report, wealth.index = TRUE)
# 
# # Use custom plotting function
# source("ggplot2 version of charts performanceSummary.R")
# data <- port_returns_report #%>% #["2007-04-30/2020-03-31"]
#     # as_tibble() %>% 
#     # drop_na() %>% 
#     # as.xts()
# 
# gg.charts.PerformanceSummary(data)
# 
# 
# 
# 
# 
# 
# MVC_filter_minus_buy_hold <- data[, "MVC_filter"] - data[, "buy_hold"]
# risk_parity_minus_buy_hold <- data[, "risk_parity"] - data[, "buy_hold"]
# risk_parity_minus_MVC_filter <- data[, "risk_parity"] - data[, "MVC_filter"]
# buy_hold_minus_MVC_filter <- data[, "buy_hold"] - data[, "MVC_filter"]
# buy_hold_minus_risk_parity <- data[, "buy_hold"] - data[, "risk_parity"]
# t.test(MVC_filter_minus_buy_hold)$p.value
# t.test(risk_parity_minus_buy_hold)$p.value 
# t.test(risk_parity_minus_MVC_filter)$p.value
# ggplot(data = data %>% as.data.frame(), aes(x = index(data), y = risk_parity - MVC_filter)) +
#     geom_line() +
#     geom_hline(yintercept = 0, color = "red")
# 
# t.test(buy_hold_minus_MVC_filter)$p.value
# t.test(buy_hold_minus_risk_parity)$p.value
# 
# drawdowns_list <- list(strat1 = table.Drawdowns(strat1_returns),
#                        strat2 = table.Drawdowns(strat2_returns),
#                        bh     = table.Drawdowns(bh_returns))
# 
# monthly_ret_list <- list(strat1 = table.CalendarReturns(strat1_returns, digits = 2),
#                          strat2 = table.CalendarReturns(strat2_returns, digits = 2),
#                          bh     = table.CalendarReturns(bh_returns, digits = 2))
# 
# 
# # final numeric tables printed to the console
# rowBinding
# drawdowns_list
# monthly_ret_list