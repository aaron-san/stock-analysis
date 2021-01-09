#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DT)
library(plotly)
library(quantmod)
library(PerformanceAnalytics)

# library(ggrepel)
# library(shinycssloaders)

monthly_rets <- readRDS("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]
cash_returns <- read_rds("data/cash_returns.rds")
asset_returns <- read_rds("data/asset_returns.rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    observe({
        # if input$all is TRUE (basically a SELECT ALL option), all choices will be selected
        # if input$all is FALSE (basically a NONE option), none of the choices will be selected

        updateSelectInput(session, 'tickers', choices = ticker_choices,
                          selected = if(input$all) ticker_choices
        )
    })
     
    output$returns_plot <- renderPlotly({
        
        # Take a dependency on input$showPlot
        input$showPlot
        
        # Show message if input tickers are not selected
        validate(need(isolate(input$tickers) != "", "Please select tickers and click 'Plot'"))
        # req(isolate(input$tickers))
        
        isolate(
            returns_plot <-
                monthly_rets %>% 
                filter(date >= min(input$daterange) &
                           date <= max(input$daterange)) %>%
                select(date, input$tickers) %>%
                pivot_longer(-date, names_to = "ticker", values_to = "return") %>%
                # mutate(return = scales::percent_format()(return)) %>% 
                group_by(ticker) %>% 
                arrange(date) %>% 
                mutate(idx = cumprod(1 + return)) %>%
                mutate(label = if_else(date == max(date), ticker, NA_character_)) %>% 
                ggplot(aes(date, idx, color = ticker)) +
                geom_line(size = .5) +
                theme_bw() +
                labs(x = NULL, y = "Index Level") +
                theme(legend.position = "top")
        )
        ggplotly(returns_plot) %>% 
            config(displayModeBar = FALSE)
    })
    
    output$returns_table = DT::renderDataTable({
        datatable(
            monthly_rets %>%
                filter(
                    date >= min(input$daterange) &
                        date <= max(input$daterange)
                ) %>%
                select(date, input$tickers) %>%
                mutate(across(
                    -date,
                    ~ scales::percent_format(accuracy = 0.1)(.x)
                )),
            rownames = F,
            extensions = c("Buttons"),
            escape = F,
            options = list(
                scrollX = T,
                pageLength = 5,
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel"),
                autoWidth = T,
                columnDefs = list(list(
                    width = '350px', targets = 6
                ))
            )
        )
    })
    
    
        
    fun <- eventReactive(input$bt_button, {
        
        # if_else(x < 0, 1, 0)
        eval(parse(text = paste('get_signal <- function(x) { return(',
                                input$signal_rule, ')}', collapse = '')))
    }) #, ignoreInit = TRUE)
        
    output$backtest_plot <- renderPlot({
        # Step 2: Create your indicator
        validate(
            need(isolate(input$signal_rule), "Enter signal rule.")
        )
        
        signals <-
            asset_returns %>%
            apply(., 2, fun()) %>%
            as.xts(order.by = index(asset_returns))
        
        get_weights <- function(x, data) {
            x / rowSums(data)
        }
        
        wts <-
            signals %>%
            apply(., 2, function(x) get_weights(x, data = signals)) %>%
            # round(3) %>%
            as.xts(order.by = index(signals))
        wts[is.nan(wts)] <- 0
        wts <- wts %>% na.omit()
        
        # rowSums(wts)
        wts$SHV <- 1 - rowSums(wts)
        
        
        # Signals are generated the monthly before performance is earned
        strat_returns <-
            (merge(asset_returns, cash_returns) * stats::lag(wts) - 0.0005) %>%
            rowSums() %>%
            xts(order.by = index(wts)) %>%
            na.omit()
        colnames(strat_returns) <- "Strategy"
    
        # Step 3: Use indicator to create equity curves
        rets <- merge(asset_returns$SPY, strat_returns) %>% na.omit()
        
        # Step 4: Evaluate strategy performance
        # table.DownsideRisk(rets)
        # table.Stats(rets)
        charts.PerformanceSummary(rets, wealth.index = TRUE)
        # chart.RelativePerformance(rets[ , 2], rets[ ,1])
        # chart.RiskReturnScatter(rets)
        
        # Return.annualized(rets)
        # SharpeRatio.annualized(rets)
        # Return.annualized(rets)/maxDrawdown(rets)
        # maxDrawdown(rets)
        
        
        
        # apply.yearly(rets, Return.cumulative) # geometric annual returns
        # apply.yearly(rets, maxDrawdown)
        # apply.yearly(rets, SharpeRatio.annualized)
        # 
        # cor(rets)
        # 
        # hist(rets$SPY, col="grey")
        # hist(rets$strat_returns, col="grey")
        # qqplot(rets$SPY, rets$strat_returns)
        # qqnorm(rets$SPY)
        # qqline(rets$strat_returns)
        })
    })
    




