
# VIX strategy


rm(list = ls())
# data contains the historical prices of the VIX and of the S&P500 as well as the date
library(tidyverse)
# library(lubridate)
library(quantmod)

## Portfolio Symbols
universe <- c("SPY", "MDY", "IWM",      # Large-Mid-Small Cap equities
              "EFA", "EEM",             # Intl.and emerging markets
              "AGG","TIP","TLT", "LQD", # Bonds
              "GSG",                    # Commodities
              "RWR", "RWX",  "MBB",     # Real Estate
              "VIX",                    # Market volatility (implied short-term volatility)
              "SHV")                    # Cash

# Get prices from Yahoo!
# prices <- getSymbols(universe, from = "1999-12-31")
# close_prices <- prices %>% map(~Cl(get(.))) %>% reduce(merge) %>% setNames(universe)
# adj_prices <- prices %>% map(~Ad(get(.))) %>% reduce(merge) %>% setNames(universe)
# saveRDS(close_prices, "data/VIX Strategy - close_prices.rds")
# saveRDS(adj_prices, "data/VIX Strategy - adj_prices.rds")

close_prices_raw <- read_rds("data/VIX Strategy - close_prices.rds")
adj_prices_raw <- read_rds("data/VIX Strategy - adj_prices.rds") 

# close_prices <- bind_cols(date = index(close_prices_raw), coredata(close_prices_raw))
# adj_prices <- bind_cols(date = index(adj_prices_raw), coredata(adj_prices_raw))

# close_prices_monthly <- close_prices_raw[endpoints(close_prices_raw, on = "months")] %>% na.omit()
adj_prices_monthly <- adj_prices_raw[endpoints(adj_prices_raw, on = "months")] %>% na.omit()

# data <- read.csv("U:/Blog/Post3/vixSP.csv", header=T, stringsAsFactors=FALSE)
# data$date<-as.Date(data$date, format="%d/%m/%Y")

returns_matrix <- ROC(adj_prices_monthly, type = "discrete") %>% na.omit()
    
decision <- NULL
for(i in 1:nrow(returns_matrix)) {
    decision[i] <- if_else(((exp(100*returns_matrix$VIX[i]/5)/(exp(100*returns_matrix$VIX[i]/5)+1))-(1/2))*2 > 0.5, 1, 0)
}





decision_matrix <- if_else(((exp(100*return_VIX/5)/(exp(100*return_VIX/5)+1))-(1/2))*2 > 0.5, 1, 0))

    
    decision = if_else(((exp(100*return_VIX/5)/(exp(100*return_VIX/5)+1))-(1/2))*2 > 0.5, 1, 0)) %>% 
    select(-c(SPY, VIX, SHV)) %>% 
    mutate(beg_port_value = 100) %>% View()
    mutate(gain_loss = lag(decision) * lag(beg_port_value) * return_SPY,
           gain_loss = replace_na(gain_loss, 0),
           cum_gain_loss = sum(100, cumsum(gain_loss))) %>% View()
    # mutate(end_port_value = beg_port_value + gain_loss) %>% 
    drop_na()
            

# for(i in 2:nrow(data)) {
#     data$beg_port_value[i] <- data$beg_port_value[i-1] + data$gain_loss[i]
# }

View(data)

data$beg_port_value    
data$end_port_value    
data$gain_loss    


    
    
    
#     drop_na() %>% 
#     mutate(gain_SPY = lag(decision) * lag(port_value) * return_SPY) %>% 
#     mutate(port_value = lag(port_value) + gain_SPY) %>% 
#     
# 
# 
# 
# 
# data %>% View()

# data$gain_SP[1] <- 0

# for(i in 2:nrow(data)) {
#     data$port_value[i] <- as.numeric(data$port_value[i-1]) + as.numeric(data$gain_SP[i])
#     }

# data$port_value <- stats::lag(data$port_value) + data$gain_SP
# data$port_value[1] <- 100
# 
# data$money <- stats::lag(data$money) * (1 + stats::lag(data$decision) * data$return_SP)
# 
# 
# 
# data %>% View()
# data$money = data$money/data$money[1]*100
# data$sp500 = data$sp500/data$sp500[1]*100
# 
# plot(data$money ~ data$date, type = ‘l’, col = ‘blue’,axes=TRUE, ann=FALSE, ylim = c(50, 250))
# lines(data$sp500~data$date, col = ‘red’)
# 
# title(xlab=”Years”, col.lab=rgb(0,0,0))
# title(ylab=”Index (US$)”, col.lab=rgb(0,0,0))
# legend(“topleft”,  c(“Portfolio”,”S&P 500″), cex=1,
#         col=c(“blue”,”red”), lty = 1, inset = 0.1);
