

mod_notes_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        
        br(),
        br(),
        fluidRow( 
        tabBox(
            id = ns("tabcard"),
            width = 6,
            title = tags$small("Notes"),

            tabPanel(
                tabName = "General",
                active = TRUE,
                title = strong("General"),
                strong("Notes"),
                fluidRow(withTags(
                    ul(
                        li("General economic conditions, interest rates, investor perceptions, market liquidity"),
                        li("Unintended dilutive or accretive effect on the value of shareholders' investments in each portfolio"),
                        li("Each portfolio is considered an investment company"),
                        li("International Swap and Derivatives Association, Inc. Master Agreement")
                    )
                    )
                    ),
                strong("Industries"),
                fluidRow(withTags(
                    ul(
                        li("Consumer Discretionary"),
                        li("Consumer Staples"),
                        li("Energy"),
                        li("Financials"),
                        li("Health Care"),
                        li("Industrials"),
                        li("Information technology"),
                        li("Materials"),
                        li("etc.")
                    )
                )
                ),
                strong("ETFs"),
                fluidRow(withTags(
                    ul(
                        li("iShares 1-3 Year Treasury Bond ETF"),
                        li("iShares MSCI EAFE Index Fund"),
                        li("iShares MSCI Emerging Markets Fund"),
                        li("iShares MSCI Emerging Markets ETF"),
                        li("iShares Russell 1000 Value ETF"),
                        li("iShares S&P 500 Index Fund"),
                        li("SPDR Dow Jones International Real Estate ETF"),
                        li("Vanguard Real Estate Index ETF")
                    )
                )
                ),
                strong("Currencies"),
                fluidRow(withTags(
                    ul(
                        li("Currency rates may be affected by changes in market interest rates, intervention (of failure to intervene) by U.S. or foreign governments, centeral banks or supranational entities such as the International Monetary Fund, by the imposition of currency controls, or other political or economic developments domestically or abroad"),
                        li("Forward foreign currency contracts: currency purchased (sold)")
                    )
                    )
                    ),
                strong("Service Providers"),
                fluidRow(withTags(
                    ul(
                        li("Distributors - underwriters to the portfolios, etc.")
                    )
                )
                ),
                strong("Assets"),
                fluidRow(withTags(
                    ul(
                        li("Common stock"),
                        ul(
                            li("Consumer staples"),
                            li("Energy"),
                            li("Financials"),
                            li("Healthcare"),
                            li("Industrials"),
                            li("Information Technology"),
                            li("Materials"),
                            li("Real Estate"),
                            li("Telecommunications Services"),
                            li("Utilities")
                            ),
                        li("Exchange-Traded Funds"),
                        li("Mutual Funds"),
                        li("Preferred Stock"),
                        li("Rights"),
                        li("Corporate Bonds/Notes"),
                        li("Collateralized Mortgage Obligations"),
                        li("Commercial Mortgage-Backed Securities"),
                        li("U.S. Treasury Obligations"),
                        li("Foreign Government Bonds"),
                        li("U.S. Government Agency Obligations"),
                        li("Asset-Backed Securities"),
                        li("Derivatives that are often Assets and Liabilities"),
                            ul(
                                li("Centrally Cleared Swaps"),
                                li("Forward Foreign Currency Contracts"),
                                li("Forward Premium Swaptions"),
                                li("Futures")
                            )
                        )
                )
                )
                           
            ),
            tabPanel(tabName = "Watch List",
                     active = FALSE,
                     icon = icon("flag"),
                     title = strong("Watch List"),
                     strong("Companies"),
                     textInput(
                         inputId = ns("symbol_to_watch"),
                         label = "",
                         placeholder = "Enter a symbol, ..."
                     ),
                     textOutput(ns("symbols_to_watch")),
                     
                     fluidRow(withTags(
                         ul(
                             li("Mizuho Financial Group, Inc."),
                             li("Unintended dilutive or accretive effect on the value of shareholders' investments in each portfolio"),
                             li("Each portfolio is considered an investment company")
                         )
                     )
                     ),
                     strong("Asset Classes"),
                     fluidRow(withTags(
                         ul(
                             li("CMO - Collateralized Mortgage Obligations"),
                             li("CMBS - Commercial Mortgage Backed Securities"),
                             li("Each portfolio is considered an investment company")
                         )
                     )
                     )
        ),
        tabPanel(
            tabName = "Futures",
            active = FALSE,
            title = strong("Futures"),
            strong("Futures"),
            fluidRow(withTags(
                ul(
                    li("Upon entering into a futures contract, one is required to deposit either cash or securities (initial margin) in an amount equal to a certain percentage of the contract value. Subsequent payments (variation margin) are made or received each day. The variation margin payments are equal to the daily changes in the contract value and are recorded as unrealized gains and losses."),
                    li("Securites held in collateralized accounts are footnoted."),
                    li("Cash collateral held by the broker to cover initial margin requirements on open futurees contracts are noted on the balance sheet."),
                    li("The net change in unrealized appreciation and depreciation is reported in the income statement."),
                    li("Realized gains (losses) are reported on the income statement at the closing or expiration of futures contracts.")
                )
            )
            )
            )
    )
    )
    )
}
    

mod_notes_server <-
    function(id) {
        moduleServer(id,
                     function(input, output, session) {
                         
                         
                         
                         observe(
                             input$symbol_to_watch,
                             write_lines(input$symbol_to_watch, 
                                         file = "data/watchlist.txt")
                         )
                         
                         watchlist <- read_lines("data/watchlist.txt")
                         
                         output$symbols_to_watch <- renderText(
                             
                             c(watchlist, input$symbol_to_watch)
                         )
                         
                     })
    }

