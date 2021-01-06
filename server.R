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
library(shinycssloaders)

monthly_rets <- readRDS("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]


# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    observe({
        # if input$all is TRUE (basically a SELECT ALL option), all choices will be selected
        # if input$all is FALSE (basically a NONE option), none of the choices will be selected
        
        updateSelectInput(
            session, 'tickers', choices = ticker_choices,
            selected = if(input$all) ticker_choices
        )
    })
    

    # returns <- reactive({
    #     
    # })
    
    
    
    output$returnsPlot <- renderPlot({
        
        # Take a dependency on input$showPlot
        input$showPlot
        
        validate(
            need(isolate(input$tickers), "")
        )
        
        isolate(
            monthly_rets %>% 
            filter(date >= min(input$daterange) &
                       date <= max(input$daterange)) %>% 
            select(date, input$tickers) %>% 
            pivot_longer(-date, names_to = "ticker", values_to = "return") %>%
            # mutate(return = scales::percent_format()(return)) %>% 
            group_by(ticker) %>% 
            arrange(date) %>% 
            mutate(idx = cumprod(1 + return)) %>% 
            ggplot(aes(date, idx, color = ticker)) +
            geom_line(size = 1) +
            theme_bw() +
                theme(legend.position = "top")
        )
        
    })
    
    output$detailtable = DT::renderDataTable({
    
        datatable(
            monthly_rets %>% 
                filter(date >= min(input$daterange) &
                           date <= max(input$daterange)) %>% 
                select(date, input$tickers) %>% 
                mutate(across(-date, ~scales::percent_format(accuracy = 0.1)(.x))),
            rownames = F,
            extensions = c("Buttons"),
            escape = F,
            options = list(
                scrollX = T,
                pageLength = 5,
                dom = "Bfrtip",
                buttons = c("copy", "csv", "excel"),
                autoWidth = T,
                columnDefs = list(list(width = '350px', targets = 6))
            )
        )
        
    })
    
    
    
    
})
