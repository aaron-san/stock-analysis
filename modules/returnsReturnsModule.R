

# paramsModule



returnsReturnsModuleUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        
        bs4Card(
            width = 12,
            headerBorder = FALSE,
            title = "Choose plot parameters",
            status = "primary",
            solidHeader = T,
            # 1st row
            fluidRow(
                span(
                    pickerInput(
                        inputId = ns("tickers"),
                        label = "Select Tickers:",
                        choices = "",
                        options = list(`actions-box` = TRUE),
                        multiple = TRUE
                    )
                ),
                span(
                    shinyWidgets::radioGroupButtons(
                        width = 330,
                        ns("date_lookback"),
                        "",
                        choices = c("All", "10 yrs", "5 yrs", "1 yr", "3 mon"),
                        selected = "All"
                    )
                ),
                span(
                    actionButton(
                        width = 100,
                        ns("showPlot"),
                        "Plot",
                        icon("caret-right"),
                        class = "btn-primary"
                    )
                )
            )
        ),
        bs4Card(
            width = 12,
            title = "Cumulative Returns Plot",
            status = "primary",
            solidHeader = T,
            fluidRow(plotlyOutput(ns("returns_plot")))
        )
    )
}


returnsReturnsModule <- function(input, output, session) {
    monthly_rets <- readRDS("data/monthly_rets_tbl.rds")
    ticker_choices <- colnames(monthly_rets)[-1]
    
    updatePickerInput(session,
                      "tickers",
                      choices = ticker_choices,
                      selected = ticker_choices)
    
    min_date <- reactive({
        switch(
            input$date_lookback,
            `All` = min(monthly_rets$date),
            `10 yrs` = max(monthly_rets$date) %m-% years(10),
            `5 yrs` = max(monthly_rets$date) %m-% years(5),
            `1 yr` = max(monthly_rets$date) %m-% years(1),
            `3 mon` = as.Date(max(monthly_rets$date)) %m-% months(3)
        )
    })
    
    output$returns_plot <- renderPlotly({
        # Take a dependency on input$showPlot
        input$showPlot
        req(isolate(min_date()))
        
        # Show message if input tickers are not selected
        validate(need(
            isolate(input$tickers) != "",
            "Please select tickers and click 'Plot'"
        ))
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
                mutate(idx = round(cumprod(1 + return), 2)) %>%
                mutate(label = if_else(
                    date == max(date), ticker, NA_character_
                )) %>%
                ggplot(aes(date, idx, color = ticker)) +
                geom_line(size = .5) +
                # theme_minimal() +
                labs(
                    x = NULL,
                    y = "Index Level",
                    title = "How much did assets grow over time?"
                ) +
                theme_update(
                    legend.position = "top",
                    panel.background = element_rect(fill = "#FFF5EE"),
                    plot.margin = unit(c(1, 1, 1, 1), "cm"),
                    plot.background = element_rect(fill = "#FFF5EE"),
                    # panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    legend.title = element_blank()
                )
        )
        ggplotly(returns_plot) %>%
            config(displayModeBar = FALSE)
    })
}

