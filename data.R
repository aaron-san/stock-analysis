
# Load data

monthly_rets <- read_rds("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]
cash_rets <- read_rds("data/cash_returns.rds")
asset_rets <- read_rds("data/asset_returns.rds")

current_fundamentals_data_file <- list.files("data", pattern = "fundamentals_data", full.names = TRUE) %>% max()
fundamentals_raw <- read_csv(current_fundamentals_data_file,
                             col_types = "cDdddddddddddddddddddddddddddddcccccdddddddddddddddddddddddddddddddddddddddd")
# c = character, D = date, d = double, _ = skip, i = integer, n = number

fundamentals_data <- 
    fundamentals_raw %>%
    group_by(industry, date) %>%
    mutate(ind_rev = sum(total_revenue, na.rm = TRUE)) %>% 
    ungroup() %>%
    mutate(market_share = total_revenue / ind_rev) %>% 
    select(-sector, -industry_id, -form) %>% 
    pivot_longer(-c(ticker, date, industry, company_name), names_to = "field", values_to = "value")
               
                     
  
profile_data <- fundamentals_raw %>%
    select(ticker, sector, industry, form,
           industry_id, company_name)
field_choices <- fundamentals_data %>% 
    ungroup() %>% 
    distinct(field) %>% pull()

industry_choices <- fundamentals_data %>% 
    ungroup() %>% 
    drop_na(industry) %>% 
    distinct(industry) %>% pull()

                 
                 
current_econ_data_file <- list.files("data", pattern = "Data from FRED", full.names = TRUE) %>% max()
econ_data <- readr::read_csv(current_econ_data_file) %>% 
    dplyr::group_by(symbol) %>%
    dplyr::arrange(symbol, date) %>%
    dplyr::rename(level = price)

# current_yields_file <- list.files("data", pattern = "yields", full.names = TRUE) %>% max()
yields_monthly <- econ_data %>% 
    filter(symbol %in% c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")) %>% 
    dplyr::rename(yield = "level") %>%
    tibbletime::as_tbl_time(index = date) %>% 
    dplyr::group_by(symbol) %>%
    drop_na() %>%
    slice(endpoints(date, on = "months")) %>% 
    dplyr::mutate(symbol = factor(symbol, levels = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30"))) %>% 
    dplyr::mutate(symbol = dplyr::recode(symbol,
                                         "DGS1MO" = "1 month", 
                                         "DGS3MO" = "3 month",
                                         "DGS6MO" = "6 month", 
                                         "DGS1" = "1 year",
                                         "DGS2" = "2 year", 
                                         "DGS5" = "5 year", 
                                         "DGS7" = "7 year", 
                                         "DGS10" = "10 year", 
                                         "DGS20" = "20 year", 
                                         "DGS30" = "30 year"))

dates_yields <- yields_monthly %>% 
    dplyr::distinct(date) %>% 
    dplyr::pull(date) %>% 
    unique()



gdp <- econ_data %>%
    dplyr::filter(symbol == "GDP") %>%
    dplyr::mutate(change = TTR::ROC(level))

dates_gdp <- gdp %>%
    dplyr::pull(date) %>%
    unique()


inflation <- econ_data %>%
    dplyr::filter(symbol == "FPCPITOTLZGUSA")

dates_inflation <- inflation %>%
    dplyr::pull(date) %>%
    unique()

bond_yields <- econ_data %>%
    dplyr::filter(symbol %in% c("AAA", "BAA")) %>%
    dplyr::group_by(symbol)

dates_bond_yields <- bond_yields %>%
    dplyr::pull(date) %>%
    unique()





# Get FRED data ----------------------------------------------------------- #
# library(tidyverse)
# library(tidyquant)
# library(readxl)
# 
# file <- list.files("data/", pattern = "FRED Tickers") %>% max()
# symbols <- read_excel("data/FRED Tickers.xlsx", col_names = TRUE, sheet = 1, range = "A1:B100") %>%
#     drop_na()
# # 
# # 
# # symb <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS5",
# #   "DGS7", "DGS10", "DGS20", "DGS30", "GDP", "FPCPITOTLZGUSA", "AAA", "BAA")
# # 
# # 
# # 
# # # Get data for all tickers
# econ_data <- symbols %>%
#     pull(`FRED Symbol`) %>%
#     tq_get(get = "economic.data",
#            from = "1975-12-31")
#  
# # prices_to_load <- prices_to_load %>%
# #   filter(symbol %in% c("AAA", "BAA", "ICSA", "UNRATE", "DXY", "GLD"))
# 
# # Create CSV
# econ_data <- econ_data %>% full_join(symbols %>% rename(symbol = `FRED Symbol`))
# write_csv(econ_data, paste0("data/Data from FRED (", str_replace_all(Sys.Date(), "-", " "), ").csv"))



