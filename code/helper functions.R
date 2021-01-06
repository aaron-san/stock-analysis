


# Rank the ranks
## Momentum rank (higher SMA is better) .50
## volatility rank (lowest SD is better) .35
## Correlation rank (lower correlation is better) .15
get_composite_rank_filter <- function(all_ranks, mom_rank, min_rank) {
    
    #---#
    # min_percent_rank <- 9
    #---#
    
    composite_rank_filter <-
        all_ranks %>% 
        as_tibble() %>% 
        mutate(date = mom_rank %>% pull(date)) %>% 
        select(date, everything()) %>% 
        pivot_longer(-date, names_to = "ticker", values_to = "rank") %>% 
        group_by(date) %>% 
        # Compute composite rank
        mutate(composite_rank = row_number(rank)) %>% 
        select(-rank) %>% 
        ungroup() %>% 
        pivot_wider(names_from = ticker, values_from = composite_rank) %>% 
        # Assign a value of 1 to tickers whose composite rank is at least 9 (and 0 to 
        # tickers whose isn't) (Why 9?)
        mutate(across(-date, ~ifelse(.x < min_rank, 0, 1))) %>% 
        select(-date)   
}




# System 1: MVC + Momentum filter ----------------------------------------- #
get_mvc_ew_data <- function(composite_rank_filter, moment_filter,
                         mom_rank, monthly_rets, friction_pct,
                         cash_rets_monthly) {
    
    
    strategy1 <- composite_rank_filter * moment_filter
    # Allocate equally among surviving tickers
    strategy1 <- strategy1 * (1 / rowSums(strategy1)) 
    
    mvc_ew_weights <-
        strategy1 %>% 
        as_tibble() %>% 
        mutate(across(everything(), ~ifelse(is.nan(.x), 0, .x)),
               date = mom_rank %>% pull(date)) %>% 
        arrange(date) %>% 
        select(date, everything()) %>% 
        slice(endpoints(date, on = "months")) %>% 
        drop_na()
    
    mvc_ew_weights <- 
        mvc_ew_weights %>% 
        select(-date) %>% 
        xts(., order.by = mvc_ew_weights %>% pull(date))
    
    mvc_ew_returns <- backtest_engine(strategy = mvc_ew_weights, 
                                   strategy_name = "MVC equal weights", 
                                   monthly_rets = monthly_rets,
                                   friction_pct = friction_pct,
                                   cash_rets_monthly = cash_rets_monthly)
    
    return(list(mvc_ew_weights = mvc_ew_weights, 
                mvc_ew_returns = mvc_ew_returns))
}

# Strategy 2: Risk Parity ------------------------------------------------- #
## Invest in an asset at a weight equal to its standard deviation relative
## to the total (summed) standard deviation of all portfolio assets
## Plus apply composite and moment filters

get_mvc_rp_data <- function(composite_rank_filter, moment_filter, volatility,
                        mom_rank, monthly_rets, friction_pct, 
                        cash_rets_monthly) {
    
    mvc_rp_buy <- composite_rank_filter * moment_filter # Remove negative momementum tickers
    kt_buy <- 1/rowSums(mvc_rp_buy * volatility) # reciprocal of surviving ticker SD's
    kt_buy[is.nan(kt_buy)] <- 0
    kt_buy[kt_buy == Inf] <- 0
    mvc_rp_weights <- kt_buy * (mvc_rp_buy * volatility) # w(i) = SD(i) / sum(SD(i))
    
    mvc_rp_weights <- 
        mvc_rp_weights %>% 
        as_tibble() %>% 
        mutate(date = mom_rank %>% pull(date)) %>% 
        select(date, everything()) %>% 
        arrange(date) %>% 
        slice(endpoints(date, on = "months")) %>% 
        drop_na()
    
    mvc_rp_weights <- mvc_rp_weights %>% select(-date) %>% xts(., order.by = mvc_rp_weights %>% pull(date))
    
    mvc_rp_returns <- backtest_engine(strategy = mvc_rp_weights, 
                                  strategy_name = "MVC - Risk parity weights", 
                                  monthly_rets = monthly_rets,
                                  friction_pct = friction_pct,
                                  cash_rets_monthly = cash_rets_monthly)
    return(list(mvc_rp_weights = mvc_rp_weights, 
                mvc_rp_returns = mvc_rp_returns))
}



# Buy and Hold ------------------------------------------------------------ #
# Invest in each asset at an equal weight
get_ew_data <- function(universe, monthly_rets, friction_pct, 
                        cash_rets_monthly) {
    
    ew_allocation <- 1/length(universe) # risky assets
    ew_initial <- matrix(1, nrow = nrow(monthly_rets), ncol = ncol(monthly_rets))
    ew_weights <- ew_initial * ew_allocation
    ew_weights <- xts(ew_weights, order.by = index(monthly_rets))
    ew_weights <- ew_weights[endpoints(index(ew_weights), on = "months"), ]
    colnames(ew_weights) <- colnames(monthly_rets)
    
    ew_returns <- backtest_engine(strategy = ew_weights, 
                                  strategy_name = "equal_weights", 
                                  monthly_rets = monthly_rets,
                                  friction_pct = friction_pct,
                                  cash_rets_monthly = cash_rets_monthly)    
    
    return(list(ew_weights = ew_weights, 
                ew_returns = ew_returns))
}



# Output ------------------------------------------------------------------ #
# port_returns_report <- cbind(mvc_returns, bh_returns, rp_returns)


# aa  <- table.AnnualizedReturns(cbind(port_returns_report))
# bb  <- table.DownsideRisk(cbind(port_returns_report))
# a   <- table.Stats(cbind(port_returns_report))
# b   <- table.Variability(cbind(port_returns_report))
# c   <- table.Distributions(cbind(port_returns_report))
# d   <- table.DrawdownsRatio(cbind(port_returns_report))
# e   <- table.DownsideRiskRatio(cbind(port_returns_report))
# f   <- table.TrailingPeriods(cbind(port_returns_report))
# stats <- rbind(aa, bb, a, b, c, d, e, f)
# View(rowBinding)