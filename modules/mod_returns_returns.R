












# returnsReturnsModule


mod_returns_returns_ui <- function(id, ticker_choices) {
    ns <- NS(id)
    
    tagList(
            boxPlus(
            width = 12,
            title = "Tickers",
            # status = "morning",
            # labelText = 1,
            # status = "secondary",
            # dropdownIcon = "wrench",
            # labelStatus = "danger",
            # labelTooltip = "Hi bro",
            # dropdownMenu = dropdownItemList(
            #     dropdownItem(url = "", name = "google link"),
            #     dropdownItem(url = "", name = "item2"),
            #     dropdownDivider(),
            #     dropdownItem(url = "", name = "item3")
            # ),
            fluidRow(span(withTags(
                span(
                    b("Large-Mid-Small Cap equities"),
                    ul(li("SPY, MDY, IWM")),
                    b("Intl.and emerging markets"),
                    ul(li("EFA, EEM"))
                )
            )),
            span(withTags(
                span(b("Bonds"),
                     ul(li(
                         "AGG, TIP, TLT, LQD"
                     )),
                     b("Commodities"),
                     ul(li("GSG")))
            )),
            span(withTags(
                span(b("Real Estate"),
                     ul(li("RWR, RWX, MBB")),
                     b("Cash"),
                     ul(li("SHV")))
            )))
        ),
        fluidRow(
            boxPlus(
                width = 6,
                class = "input_card",
                title = "Inputs",
                # status = "secondary",
                pickerInput(
                    width = 240,
                    inputId = ns("tickers"),
                    label = "Select Tickers:",
                    choices = ticker_choices,
                    selected = ticker_choices[1:5],
                    options = list(`actions-box` = TRUE),
                    multiple = TRUE
                ),
                shinyWidgets::radioGroupButtons(
                    width = 330,
                    ns("date_lookback"),
                    label = "Period:",
                    choices = c("All", "10Y", "5Y", "1Y", "3M"),
                    selected = "All"
                ),
                actionButton(
                    width = 100,
                    ns("showPlot"),
                    "Plot",
                    icon("caret-right"),
                    class = "btn-primary"
                )
                
            ),
            
            boxPlus(
                width = 6,
                title = "Cumulative Returns Plot",
                    # status = "secondary",
                    # width = 8,
                    plotlyOutput(ns("returns_plot")))
        )
    )
}


mod_returns_returns_server <- function(id, monthly_rets) {
    moduleServer(id,
                 function(input, output, session) {
                     min_date <- reactive({
                         switch(
                             input$date_lookback,
                             `All` = min(monthly_rets$date),
                             `10Y` = max(monthly_rets$date) %m-% years(10),
                             `5Y` = max(monthly_rets$date) %m-% years(5),
                             `1Y` = max(monthly_rets$date) %m-% years(1),
                             `3M` = as.Date(max(monthly_rets$date)) %m-% months(3)
                         )
                     })
                     
                     
                     
                     
                     
                     output$returns_plot <- renderPlotly({
                         # Show message if input tickers are not selected
                         validate(
                             need(input$showPlot, "Please click 'Plot'."),
                             need(
                                 isolate(input$tickers) != "",
                                 "Please select tickers and click 'Plot'"
                             )
                         )
                         
                         # validate(need(input$w1 + input$w2 + input$w3 + input$w4 + input$w5 == 100, "The portfolio weights must sum to 100%!"))
                         
                         
                         
                         
                         
                         
                         isolate(
                             returns_plot <-
                                 monthly_rets %>%
                                 filter(date >= min_date()) %>%
                                 select(date, isolate(input$tickers)) %>%
                                 pivot_longer(-date,
                                              names_to = "ticker",
                                              values_to = "return") %>%
                                 group_by(ticker) %>%
                                 arrange(date) %>%
                                 mutate(idx = round(cumprod(
                                     1 + return
                                 ), 2)) %>%
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
                                 theme_solarized() +
                                 theme(legend.title = element_blank())
                             # theme_update(
                             #         legend.position = "top",
                             #         panel.background = element_rect(fill = "#FFF5EE"),
                             #         plot.margin = unit(c(1, 1, 1, 1), "cm"),
                             #         plot.background = element_rect(fill = "#FFF5EE"),
                             #         panel.grid.major = element_line(color = "gray80"),
                             #         panel.grid.minor = element_blank(),
                             #         panel.border = element_blank(),
                             #         legend.title = element_blank()
                             #     )
                         )
                         
                         
                         ggplotly(returns_plot) %>%
                             config(displayModeBar = FALSE)
                     })
                 })
}
