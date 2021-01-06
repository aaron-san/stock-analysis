## backtesting function
backtest_engine <- function(strategy, strategy_name, 
                            monthly_rets, friction_pct,
                            cash_rets_monthly){
  
  ####
  # For testing
  ## xts of asset weights
  # strategy <- bh_weights
  # strategy_name <- "MVC_filter"
  # monthly_rets <- monthly_rets
  # friction_pct <- friction_pct
  # cash_rets_monthly <- cash_rets_monthly
  #---#
  
  
  # allocation matrix
  cash <- ifelse(rowSums(strategy) == 0, 1, 1 - rowSums(strategy))
  allocations <- cbind(strategy, cash)
  
  monthly_rets <- monthly_rets[index(allocations)]
  cash_rets_monthly <- cash_rets_monthly[index(allocations)]
  colnames(cash_rets_monthly) <- "cash"
  
  ## trade matrix
  prelim_trade <- allocations
  prelim_trade[is.na(prelim_trade)] <- 0
  # It's not the change in the weights that incurs friction costs, its the
  #  trade that is needed either to achieve a new weight or maintain the 
  #  same weight given that disproportionate returns have altered the weight
  wts_change <- prelim_trade - stats::lag(prelim_trade)
  
  ## friction matrix
  ## Assumes that the friction costs are incurred per month on realized returns
  wts_risky_assets <- wts_change[, colnames(strategy)]
  trade_costs  <- abs(wts_change) * friction_pct
  
  # Signal lags realized returns by one period
  # Transaction costs are subtracted from returns
  wheninvested_rets <- cbind(monthly_rets, cash_rets_monthly) * stats::lag(allocations) - trade_costs

  ## Calculate portfolio returns vector
  port_rets     <- rowSums(wheninvested_rets)
  port_rets_xts <- xts(port_rets, order.by = index(wheninvested_rets))
  port_returns  <- port_rets_xts[is.na(port_rets_xts) == FALSE]
  colnames(port_returns) <- strategy_name
  
  port_returns
}
