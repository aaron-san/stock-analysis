
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
cash_rets <- read_rds("data/cash_returns.rds")
asset_rets <- read_rds("data/asset_returns.rds")


shinyServer(function(input, output, session) {


    callModule(returnsReturnsServer, 
               "returns", 
               monthly_rets = monthly_rets, 
               ticker_choices = ticker_choices)
    
    callModule(backtestBacktestServer,
               "backtest",
               asset_rets = asset_rets,
               cash_rets = cash_rets,
               monthly_rets = monthly_rets,
               ticker_choices = ticker_choices)
    
    callModule(returnsReturnsTableServer, 
               "returns_table",
               monthly_rets = monthly_rets, 
               ticker_choices = ticker_choices)
    
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




