
# returnsReturnsTableModule

returnsReturnsTableUI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        bs4Card(
            width = 12,
            fluidRow(
                span(
                    pickerInput(
                        width = 300,
                        inputId = ns("tickers_bt"),
                        label = "Select tickers:",
                        choices = "",
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                    )
                ),
                span(
                    shinyWidgets::radioGroupButtons(
                        width = 330,
                        inputId = ns("date_lookback_bt"),
                        label = "Period:",
                        choices = c("All", "10 yrs", "5 yrs", "1 yr", "3 mon"),
                        selected = "All"
                    )
                ),
                span(
                    actionButton(
                        width = 100,
                        inputId = ns("update"),
                        label = "Update"
                    )
                )
            ),
            br(),
            fluidRow(
                DT::dataTableOutput(ns("returns_table")),
                # footer = "Hover for state-by-state incident stats, scaled below",
                width = 12)
        )
    )
    
    
}


returnsReturnsTableServer <- function(input, output, session,
                                     monthly_rets, ticker_choices) {
    
    updatePickerInput(session,
                      "tickers_bt",
                      choices = ticker_choices,
                      selected = ticker_choices)
    
    min_date <- reactive({
        switch(input$date_lookback_bt,
               `All` = min(monthly_rets$date),
               `10 yrs` = max(monthly_rets$date) %m-% years(10),
               `5 yrs` = max(monthly_rets$date) %m-% years(5),
               `1 yr` = max(monthly_rets$date) %m-% years(1),
               `3 mon` = as.Date(max(monthly_rets$date)) %m-% months(3))
    })
    
    
    
    
    output$returns_table = DT::renderDataTable({
        
        # Take dependency on button
        input$update
        req(isolate(min_date()))
        
        
        datatable(
            monthly_rets %>%
                filter(date >= isolate(min_date())
                       # date >= min(input$daterange) &
                       #     date <= max(input$daterange)
                ) %>%
                select(date, isolate(input$tickers_bt)) %>%
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
    
}



