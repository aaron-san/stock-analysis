
# Debugging
# library(shinyobjects)
# load_reactive_objects()


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
    
    updatePickerInput(session, "tickers", choices = ticker_choices, selected = ticker_choices)
    updateSelectInput(session, "ticker_decision_rule", choices = ticker_choices)
    updateSelectInput(session, "invested_ticker", choices = ticker_choices)
    updateSliderInput(session, "daterange_bt", min = monthly_rets %>% pull(date) %>% min(),
                      max = monthly_rets %>% pull(date) %>% max(),
                      value = c(as.Date("2014-01-01"), monthly_rets %>% pull(date) %>% max()))
    
    
    min_date <- reactive({
        switch(input$date_lookback,
               `All` = min(monthly_rets$date),
               `10 yrs` = max(monthly_rets$date) %m-% years(10),
               `5 yrs` = max(monthly_rets$date) %m-% years(5),
               `1 yr` = max(monthly_rets$date) %m-% years(1),
               `3 mon` = as.Date(max(monthly_rets$date)) %m-% months(3))
    })
    
    ###
    callModule(returnsReturnsModule, "returns")
    ###
    
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
    
    ###
    callModule(backtestBacktestModule, "backtest")
    ###
    
    
    
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




