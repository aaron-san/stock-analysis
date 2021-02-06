
mod_backtest_sortino_ui <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(
            bs4Card(
                width = 6,
                title = "Density Plot",
                status = "secondary",
                plotOutput(ns("density_plot"), height = "200px")
            ),
            bs4Card(
                width = 6,
                title = "Sortino Plot",
                status = "secondary",
                plotOutput(ns("perf_chart"), height = "200px")
                )
        )
        
    )
}


mod_backtest_sortino_server <- function(id, monthly_rets) {
    
    moduleServer(id,
                 function(input, output, server) {
             
                     symbols <- c("SPY", "MDY", "EFA", "IWM")        
                     wts_i <- 1/length(symbols)
                     mar <- 0.005
                     rolling_window <- 24 #months
                     
                     port_rets_xts <- xts(monthly_rets[colnames(monthly_rets) != "date"], order.by = monthly_rets$date)
                     port_rets_xts <- port_rets_xts[, colnames(port_rets_xts) %in% symbols]
                     wts_xts <- port_rets_xts
                     wts_xts[] <- matrix(wts_i, nrow = nrow(port_rets_xts), ncol = ncol(port_rets_xts))
                     
                     port_rets_xts_rebal_monthly <- Return.portfolio(port_rets_xts, weights = wts_xts, rebalance_on = "months")
                     
                     rolling_sortino <-
                         apply.rolling(port_rets_xts_rebal_monthly, width = rolling_window, 
                                       fun = 'SortinoRatio',  MAR = MAR) %>% 
                         setNames(paste(rolling_window, "-month rolling sortino", sep="")) 
                     
                     
                     
                     
                     output$perf_chart <- renderPlot(
                         
                         
                         rolling_sortino %>% 
                             as_tibble() %>% 
                             mutate(date = index(rolling_sortino)) %>% 
                             select(date, `24-month rolling sortino`) %>% 
                             drop_na() %>%
                             ggplot(aes(x = date, y = `24-month rolling sortino`, 
                                        fill = ifelse(`24-month rolling sortino` >= 0, "green", "red"))) + 
                             geom_col(alpha = 0.6) +
                             geom_hline(yintercept = 0, color = "red", alpha = 0.5) +
                             theme_minimal() + 
                             theme(legend.position = "") + 
                             labs(x = "",
                                  y = "",
                                  title = "24-month Rolling Sortino")
                             
                     )
                     
                     
                     # output$perf_chart <- renderHighchart(
                     #     highchart(type = "stock") %>%
                     #     hc_title(text = names(rolling_sortino)) %>%
                     #     # hc_title(text = names(rolling_sortino())) %>%
                     #     # hc_add_series(rolling_sortino(), name = names(rolling_sortino()), color = "cornflowerblue") %>%
                     #     hc_add_series(rolling_sortino, name = names(rolling_sortino), color = "cornflowerblue") %>%
                     #         hc_add_theme(hc_theme_economist()) %>% 
                     #         hc_navigator(enabled = FALSE) %>% 
                     #     hc_scrollbar(enabled = FALSE) 
                     # )
                     
                     
                     port_rets_xts_rebal_monthly %>% 
                         as_tibble() %>% 
                         mutate(date = index(port_rets_xts_rebal_monthly)) %>% 
                         mutate(returns_below_MAR = ifelse(portfolio.returns < mar, portfolio.returns, NA)) %>%
                         mutate(returns_above_MAR = ifelse(portfolio.returns > mar, portfolio.returns, NA)) %>% 
                         ggplot(aes(x = date)) +
                         geom_point(aes(y = returns_below_MAR), colour = "red") +
                         geom_point(aes(y = returns_above_MAR), colour = "green") + 
                         geom_vline(xintercept = as.numeric(as.Date("2016-11-30")), color = "blue") +
                         geom_hline(yintercept = mar, color = "purple", linetype = "dotted") +
                         annotate(geom="text", x=as.Date("2016-11-30"), 
                                  y = -.05, label = "Trump", fontface = "plain", 
                                  angle = 90, alpha = .5, vjust =  1.5) +
                         ylab("percent monthly returns")
                     
                     
                     port_rets_xts_rebal_monthly %>% 
                         as_tibble() %>% 
                         mutate(date = index(port_rets_xts_rebal_monthly)) %>% 
                         ggplot(aes(x = portfolio.returns)) +
                         geom_histogram(alpha = 0.25, binwidth = .01, fill = "cornflowerblue") +
                         geom_vline(xintercept = mar, color = "green") +
                         annotate(geom = "text", x = mar, 
                                  y = 10, label = "MAR", fontface = "plain", 
                                  angle = 90, alpha = .5, vjust =  1)
                     
                     
                     sortino_density_plot <- 
                         port_rets_xts_rebal_monthly %>% 
                         as_tibble() %>% 
                         mutate(date = index(port_rets_xts_rebal_monthly)) %>% 
                         ggplot(aes(x = portfolio.returns)) +
                         stat_density(geom = "line", size = 1, color = "cornflowerblue") +
                         theme_minimal()
                     
                     shaded_area_data <- ggplot_build(sortino_density_plot)$data[[1]] %>% 
                         filter(x < mar)
                     
                     
                     output$density_plot <- renderPlot(
                         
                         sortino_density_plot + 
                             geom_area(data = shaded_area_data, aes(x = x, y = y), fill="pink", alpha = 0.5) +
                             geom_segment(data = shaded_area_data, aes(x = mar, y = 0, xend = mar, yend = y), 
                                          color = "red", linetype = "dotted") +
                             annotate(geom = "text", x = mar, y = 5, label = paste("MAR =", mar, sep = ""), 
                                      fontface = "plain", angle = 90, alpha = .8, vjust =  -1) +
                             annotate(geom = "text", x = (mar - .02), y = .1, label = "Downside", 
                                      fontface = "plain", alpha = .8, vjust =  -1)    
                     )
                    
                     
                 })
    
}