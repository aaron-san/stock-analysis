

mod_returns_returns_ui <- function(id, ticker_choices) {
    ns <- NS(id)
    
    tagList(
        
        fluidRow(    
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
            column(width = 4,
                h4("Large-Mid-Small Cap equities"),
                tags$ul(tags$li(h5("SPY - Large cap U.S. Equities, blend style")), 
                        tags$li(h5("MDY - S&P Midcap 400 ETF Trust, seeks to provide investment results that, before expenses, correspond to the S&P MidCap 400 Index")), 
                        tags$li(h5("IWM - iShares Russell 2000 ETF, seeks to track the investment results of an index composed of small-capitalization U.S. equities"))
                   ),
                h4("Intl.and emerging markets"),
                tags$ul(tags$li(h5("EFA - iShares EAFE (index of large- and mid-cap companies in developed countries ex/ U.S. and Canada)")), 
                        tags$li(h5("EEM - iShares MSCI Emerging Markets ETF, exposure to large and mid-sized companies in emerging markets"))
                   )
            ),
            column(width = 4,
                h4("Bonds"),
                tags$ul(tags$li(h5("AGG - U.S. Total bond market, all-term")), 
                        tags$li(h5("TIP")), 
                        tags$li(h5("TLT - Shares 20+ year Bonds")), 
                        tags$li(h5("LQD - iShares Investment Grade Corporate Bonds, U.S."))
                   ),
                h4("Commodities"),
                tags$ul(tags$li(h5("GSG - iShares S&P GSCI Commodity-Indexed, seeks to track the results of a fully collateralized investment in futures contracts on an index composed of a diversified group of commodities futures")))
            ), 
            column(width = 4,
              h4("Real Estate"),
              tags$ul(tags$li(h5("RWR - SPDR Dow Jones REIT ETF, seeks to provide investment results that, before fees and expenses, correspond to the return of the Dow Jones U.S. Select REIT Index, designed to serve as a prox for direct real estate investing")), 
                      tags$li(h5("RWX - SPDR Dow Jones International Real Estate ETF, seeks to provide investment results that, before fees and expenses, correspond to the toral return of the Dow Jones Global ex-U.S. Select Real Estate Securities Index, access to publicly traded real estate securities in non-U.S. developed and emerging markets")), 
                      tags$li(h5("MBB - iShares MBS ETF, seeks to track the investment results of an index composed of investment-grade mortgage-backed securities"))),
                 h4("Cash"),
              tags$ul(tags$li(h5("SHV - iShares Short Treasury Bond ETF, seeks to track the investment results of an index composed of U.S. Treasury bonds with remaining maturities one year or less")))
            )
        )
        
        ),
        fluidRow(
            boxPlus(
                width = 4,
                class = "input_card",
                title = "Inputs",
                # status = "secondary",
                pickerInput(
                    width = 240,
                    inputId = ns("tickers"),
                    label = "Select Tickers:",
                    choices = list(
                        Equities = c("SPY", "MDY", "IWM"),
                        `International & Emerging Markets` = c("EFA", "EEM"),
                        Bonds = c("AGG", "TIP", "TLT", "LQD"),
                        Commodities = "GSG",
                        `Real Estate` = c("RWR", "RWX", "MBB")),
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
                width = 8,
                title = "Cumulative Returns Plot",
                    # status = "secondary",
                    # width = 8,
                    plotOutput(ns("returns_plot"),
                               width = 200),
                footer = "footer"
                )
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
                     
                     
                     
                     
                     
                     output$returns_plot <- renderPlot({
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
                             # returns_plot <-
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
                                 geom_line(size = 1.5) +
                                 theme_minimal() +
                                 labs(
                                     x = NULL,
                                     y = "Index Level",
                                     title = "How much did assets grow over time?"
                                 ) +
                                 # theme_solarized() +
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
                         
                         
                         # ggplotly(returns_plot) %>%
                         #     config(displayModeBar = FALSE)
                     })
                 })
}
