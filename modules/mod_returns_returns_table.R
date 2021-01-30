





# returnsReturnsTableModule

mod_returns_returns_table_ui <- function(id, ticker_choices) {
    ns <- NS(id)
    
    tagList(bs4Card(
        width = 12,
        fluidRow(
            pickerInput(
                width = 300,
                inputId = ns("tickers_bt"),
                label = "Select tickers:",
                choices = ticker_choices,
                selected = ticker_choices,
                options = list(`actions-box` = TRUE),
                multiple = TRUE
            ),
            
            shinyWidgets::radioGroupButtons(
                width = 330,
                inputId = ns("date_lookback_bt"),
                label = "Period:",
                choices = c("All", "10Y", "5Y", "1Y", "3M"),
                selected = "All"
                
            ),
            actionButton(
                width = 150,
                inputId = ns("update"),
                label = "Update table",
                icon("caret-right"),
                class = "btn-primary"
            )
        ),
        br(),
        fluidRow(DT::dataTableOutput(ns(
            "returns_table"
        )),
        # footer = "Hover for state-by-state incident stats, scaled below",
        width = 12)
    ))
    
    
}


mod_returns_returns_table_server <- function(id, monthly_rets) {
    moduleServer(id,
                 function(input,
                          output,
                          session) {
                     min_date <- reactive({
                         switch(
                             input$date_lookback_bt,
                             `All` = min(monthly_rets$date),
                             `10Y` = max(monthly_rets$date) %m-% years(10),
                             `5Y` = max(monthly_rets$date) %m-% years(5),
                             `1Y` = max(monthly_rets$date) %m-% years(1),
                             `3M` = as.Date(max(monthly_rets$date)) %m-% months(3)
                         )
                     })
                     
                     
                     
                     
                     output$returns_table = DT::renderDataTable({
                         # Take dependency on button
                         input$update
                         req(isolate(min_date()))
                         
                         
                         datatable(
                             monthly_rets %>%
                                 filter(date >= isolate(min_date())) %>%
                                 # date >= min(input$daterange) &
                                 #     date <= max(input$daterange)) %>%
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
                 })
}
