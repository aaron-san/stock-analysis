
# Load data

monthly_rets <- read_rds("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]
cash_rets <- read_rds("data/cash_returns.rds")
asset_rets <- read_rds("data/asset_returns.rds")


# Load fundamentals data
# read_csv()
# c = character, D = date, d = double, _ = skip, i = integer, n = number

col_types <- list(ticker = "c", date = "D", form = "c", industry = "c", short_name = "c", long_business_summary = "c")
current_bs_1 <- list.files("data/cleaned", pattern = "balance_sheets_1", full.names = TRUE) %>% max()
bs_1 <- read_csv(current_bs_1, col_types = col_types)
current_bs_2 <- list.files("data/cleaned", pattern = "balance_sheets_2", full.names = TRUE) %>% max()
bs_2 <- read_csv(current_bs_2, col_types = col_types)
current_bs_3 <- list.files("data/cleaned", pattern = "balance_sheets_3", full.names = TRUE) %>% max()
bs_3 <- read_csv(current_bs_3, col_types = col_types)
current_bs_4 <- list.files("data/cleaned", pattern = "balance_sheets_4", full.names = TRUE) %>% max()
bs_4 <- read_csv(current_bs_4, col_types = col_types)

current_is_1 <- list.files("data/cleaned", pattern = "income_statements_1", full.names = TRUE) %>% max()
is_1 <- read_csv(current_is_1, col_types = col_types)
current_is_2 <- list.files("data/cleaned", pattern = "income_statements_2", full.names = TRUE) %>% max()
is_2 <- read_csv(current_is_2, col_types = col_types)
current_is_3 <- list.files("data/cleaned", pattern = "income_statements_3", full.names = TRUE) %>% max()
is_3 <- read_csv(current_is_3, col_types = col_types)
current_is_4 <- list.files("data/cleaned", pattern = "income_statements_4", full.names = TRUE) %>% max()
is_4 <- read_csv(current_is_4, col_types = col_types)

current_cf_1 <- list.files("data/cleaned", pattern = "cash_flow_statements_1", full.names = TRUE) %>% max()
cf_1 <- read_csv(current_cf_1, col_types = col_types)
current_cf_2 <- list.files("data/cleaned", pattern = "cash_flow_statements_2", full.names = TRUE) %>% max()
cf_2 <- read_csv(current_cf_2, col_types = col_types)
current_cf_3 <- list.files("data/cleaned", pattern = "cash_flow_statements_3", full.names = TRUE) %>% max()
cf_3 <- read_csv(current_cf_3, col_types = col_types)
current_cf_4 <- list.files("data/cleaned", pattern = "cash_flow_statements_4", full.names = TRUE) %>% max()
cf_4 <- read_csv(current_cf_4, col_types = col_types)



# fundamentals_data <- 
#   fundamentals_raw %>%
#   group_by(industry, date) %>%
#   # drop_na(total_revenue) %>%
#   # Calculate ratios  
#   mutate(market_share = total_revenue / sum(total_revenue, na.rm = TRUE)) %>%
#   mutate(gross_margin = (total_revenue - cost_of_revenue) / total_revenue) %>% 
#   #
#   select(-sector, -industry_id, -form) %>% 
#   ungroup() %>% 
#   pivot_longer(-c(ticker, date, industry, company_name), names_to = "field", values_to = "value")
#   

             
  
# profile_data <- fundamentals_raw %>%
#     select(ticker, sector, industry, form, company_name)




field_choices <- bs_1 %>% 
  full_join(is_1) %>% 
  full_join(cf_1) %>% 
  colnames() %>% 
  setdiff(c("ticker", "date", "form", "industry", "short_name", "long_business_summary"))

industry_choices <- bs_1 %>% 
  full_join(is_1) %>% 
  full_join(cf_1) %>% 
  drop_na(industry) %>% 
  distinct(industry) %>% 
  pull()

                 
                 
current_econ_data_file <- list.files("data", pattern = "Data from FRED", full.names = TRUE) %>% max()
col_types <- list(symbol = col_character(), date = col_date(), price = col_double())
econ_data <- read_csv(current_econ_data_file, col_types = col_types) %>% 
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



