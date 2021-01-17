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
library(lubridate)
library(sever)

# library(ggrepel)
# library(shinycssloaders)

monthly_rets <- readRDS("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]
cash_returns <- read_rds("data/cash_returns.rds")
asset_returns <- read_rds("data/asset_returns.rds")


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

    # sever()
    
    observe({
        updateSelectInput(session, 'tickers', choices = ticker_choices,
                          selected = if(input$all) ticker_choices
        )
    })
    
    min_date <- reactive({
        switch(input$date_lookback,
               `All` = min(monthly_rets$date),
               `10 yrs` = max(monthly_rets$date) %m-% years(10),
               `5 yrs` = max(monthly_rets$date) %m-% years(5),
               `1 yr` = max(monthly_rets$date) %m-% years(1),
               `3 mon` = as.Date(max(monthly_rets$date)) %m-% months(3))
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
                # filter(date >= min(input$daterange) &
                #            date <= max(input$daterange)) %>%
                filter(date >= min_date()) %>% 
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
                filter(date >= min_date()
                    # date >= min(input$daterange) &
                    #     date <= max(input$daterange)
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
    
    
    # Parse the decision rule    
    fun <- eventReactive(input$bt_button, {
        eval(parse(text = paste('get_signal <- function(x) { return(',
                                input$signal_rule, ')}', collapse = '')))
    }) #, ignoreInit = TRUE)
    
    
        
    output$backtest_plot <- renderPlot({
        # Step 2: Create your indicator
        validate(
            need(isolate(input$signal_rule), "Enter signal rule.")
        )
        
        asset_returns_filt <- reactive({
            asset_returns[paste(isolate(input$daterange_bt), collapse = "::")]
            # asset_returns[paste(c("2001-12-13", "2003-12-31"), collapse = "::")]
            
        })
        
        
        invested_asset_returns <- reactive({
            asset_returns_filt() %>% 
                .[, isolate(c(input$invested_ticker))]
        })
        
        
        
        signals <-
            asset_returns_filt() %>% 
            .[, isolate(input$ticker_decision_rule)] %>%
            # .[, "EEM"] %>% 
            apply(., 2, fun()) %>%
            # apply(., 2, function(x) if_else(x < -0.2, 1, 0)) %>%
            as.xts(order.by = index(asset_returns_filt()))
        
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
            (merge(invested_asset_returns(), cash_returns) * stats::lag(wts) - 0.0005) %>%
            rowSums() %>%
            xts(order.by = index(wts)) %>%
            na.omit()
        colnames(strat_returns) <- "Strategy"
    
        # Step 3: Use indicator to create equity curves
        rets <- merge(asset_returns_filt()[, isolate(input$invested_ticker)], strat_returns) %>% na.omit()
        
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
    




# library(gapminder) # for gapminder dataset
# library(plotly) # for plotly charts and %>% pipe operator
# 
# ## Size of the scatter data point will represent the population
# ## Color of the scatter data point will represent the continent
# 
# gapminder %>% 
#     plot_ly() %>%  
#     # add frame argument for animation and map to the variable needed on the timeline
#     add_markers(x=~gdpPercap, y=~lifeExp, 
#                 frame=~year, 
#                 size = ~pop,
#                 color=~continent,
#                 marker=list(sizemode="diameter"),
#                 text=~paste("Life Expectancy: ", round(lifeExp,1), 
#                             "<br>",
#                             "GDP Per Capita:", round(gdpPercap,1), 
#                             "<br>", 
#                             "Country:", country, 
#                             "<br>", 
#                             "Population:", pop), 
#                 hoverinfo= "text") %>% 
#     layout(title="Animated Plotly Bubble Plot",
#            xaxis=list(title="GDP Per Capita (log scale)", type="log"),
#            yaxis=list(title= "Life Expectancy"))




