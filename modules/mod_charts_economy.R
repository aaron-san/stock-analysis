
# mod_charts_economy

mod_charts_economy_ui <-
    function(id, dates_gdp, dates_yields, dates_inflation, dates_bond_yields) {
        ns <- NS(id)
        
        tagList(
            
            br(),
            br(),
            fluidRow(
                wellPanel(p("The economic growth regime changes in the '80s with Reagan to a debt-driven growth (growth prior to the '80s is irrelevant
00's (the oh-oh's"))),
            fluidRow(
                boxPlus(
                    width = 6,
                    plotOutput(ns("gdp"), height = "300px"),
                    footer = selectizeInput(
                        ns("lookback_gdp"),
                        label = "Lookback (years):",
                        choices = 1:(length(dates_gdp)/4 - 1),
                        selected = 2#,
                        # width = "40%"
                    )
                ),
                boxPlus(
                    width = 6,
                    plotOutput(ns("yields"), height = "300px"),
                    footer = selectizeInput(
                        ns("lookback_yields"),
                        label = "Lookback (months):",
                        selected = 12,
                        choices = c(3, 6, 9, seq(12, 12*floor(length(dates_yields)/12), by = 12))#,
                        # width = "40%"
                    )
                )
            ),
            
            fluidRow(
                boxPlus(
                    width = 6,
                    plotOutput(ns("inflation"), height = "300px"),
                    footer = selectizeInput(
                        ns("lookback_inflation"),
                        label = "Lookback (No. years):",
                        choices = 1:(length(dates_inflation) - 1),
                        selected = 5#,
                        # width = "50%"
                    )
                ),
                boxPlus(
                    width = 6,
                    plotOutput(ns("bond_yields"), height = "300px"),
                    footer = selectizeInput(
                        ns("lookback_bond_yields"),
                        label = "Lookback (No. months):",
                        choices = 1:(length(dates_bond_yields) - 1),
                        selected = 12#,
                        # width = "50%"
                    )
                ) 
            ),
            fluidRow(
                boxPlus(
                    width = 6,
                    title = "Notes",
                    # status = "primary",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h1("Economy"),
                    div("The dollar moves in long trends, generally"),
                    
                    div("There is a strong negative correlation between the USD and commodity returns"),
                    div("Negative interest rates are ultimately fatal to banks and their stocks"),
                    h2("Leading Indicators"),
                    h3("OECD CLI (Composite Leading Indicator)"),
                    div("The gold standard of leading indicators"),
                    h2("Yield Curve"),
                    div("Inverted about 10% of the time"),
                    h2("Stock markets"),
                    div("Stock markets used to be a leading indicator of economic recessions, but a look at the correlations between stock markets and GDP show a disconnection at certain times."),
                    br(),
                    h1("Technical Indicators"),
                    h2("RSI(2day) (overbought/oversold)"),
                    h2("MACD (Moving Average Convergence-Divergence)"),
                    p("Measures the momentum of price moveements. Made up of two exponential moving
            averages (26-day EMA, 12-day EMA, 9). The MACD is computed as the 12-day EMA less the
            26-day EMA and results in a barchart that oscillates about zero. Buy when above zero,
            and sell when below zero. Bearish when MACD is decreasing and below zero. Bullish when MACD is increasing and above zero."),
                    div("Buy when turns negative, contrary to convention)"),
                    h2("Bollinger Bands"),
                    div("Moving average price plus standard deviation band"),
                    div("Low volatility, the band contracts; high volatility, the band expands"),
                    div("Traders typically expect volatility to revert to some prior trend"),
                    h2("Any major stock gaps"),
                    h2("Print output for top 5 tickers ('Ideas')"),
                    h2("false signals"),
                    h1("Macroeconomic Indicators"),
                    div(
                        "The following indicators are leading indicators. It has been noted that economic recessions over the past several decades have been preceded by a fall in leading indicator levels. Falling levels of leading indicators doesn't always produce a recession, though. It is a necessary, but not sufficient event."
                    ),
                    h1("To Do:"),
                    div("Add company fundamentals"),
                    div("Create a technicals/fundamentals positive/negative map"),
                    div("Add stocks with current gaps"),
                    h1("Fundamental indicators"),
                    h2("QVAL Metrics"),
                    div("EBIT/TEV rank"),
                    div("ROC rank"),
                    div("10yr GP/TA rank"),
                    h3("Profitability"),
                    h3("Liquidity"),
                    h3("Solvency"),
                    h3("Activity (Operational Efficiency)"),
                    p(
                        "Show line plot of returns to asset classes including
                   Treasury bills, Inflation, LT government bonds,
                   Large-company stocks, Small-company stocks"
                    ),
                    
                    h1("Benjamain Graham Investing Rules"),
                    div(
                        "E/P yield >= 2x AAA bon yield. PE ratio < 40% of highest
            PE ratio the stock had over the trailing five years. Dividen yield
            >= 2/3 AAA bond yield. Stock price < 2/3 Net tangible assets per share.
            Stock price = 2/3 net current asset value. Total debt < book value.
            Current ratio > 2. Total debt < 2x Net current asset value (= CA - all liabilities).
            Earnings growth of previous ten years >= 7% annually. Stability of growth
            of earning (<= 2 declines of 5% or more in the prior 10 years."
                    ),
                    h1("Warren Buffett Investing Rules"),
                    div(
                        "ROE above 15% for prior five years. D/E ratio must be small. Profit
            margin >= 20% than industry average. Good profit margin over past five years
            consistently (PM = Net profit / Sales). >= 10 years publicly listed. Must
            have unique (not commodity) products. Intrinsic value >= 25% market capitalization."
                    )
                ),

            boxPlus(width = 6,
                title = "Correlation Chart",
                # status = "primary",
                solidHeader = TRUE,
                collapsible = TRUE,
                selectInput(width = 150,
                            ns("weak_ticker"), "Weak Ticker:",
                            choices = list(
                                Equities = c("SPY", "MDY", "IWM"),
                                `International & Emerging Markets` = c("EFA", "EEM"),
                                Bonds = c("AGG", "TIP", "TLT", "LQD"),
                                Commodities = "GSG",
                                `Real Estate` = c("RWR", "RWX", "MBB"),
                                Cash = "SHV"),
                            selected = "SPY"),

                plotOutput(ns("cor_plot")),
                footer = tagList(
                    h3("Findings"),
                    tags$ul(
                    tags$li("It seems that when SPY does poorly, MBB and TLT are the best alternatives."),
                    tags$li("It seems that when LQD does poorly, MBB and TLT are the best alternatives."),
                    tags$li("It seems that when GSG does poorly, AGG, MBB, and TLT are the best alternatives."),
                    tags$li("It seems that when MDY does poorly, MBB and TLT are the best alternatives."),
                    tags$li("It seems that when IWM does poorly, MBB and TLT are the best alternatives."),
                    tags$li("It seems that when EFA does poorly, TLT is the best alternative."),
                    tags$li("It seems that when EEM does poorly, MBB and TLT are the best alternatives."),
                    tags$li("It seems that when TLT does poorly, most of the other assets are good alternatives.")
                    )
                )
            )
        )
        )
    }



mod_charts_economy_server <-
    function(id, yields_monthly, gdp, inflation, bond_yields) {
        moduleServer(id,
                     function(input, output, session) {
                     
                         # Obtain Treasury yield data
                         symbols <- c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")
                         # DGS5: 5-Year Treasury Constant Maturity Rate, Percent, Not Seasonally Adjusted
                         
                         
                         
                         output$yields <- renderPlot({
                             
                             ####
                             # For testing
                             ####
                             # input <- list(ticker = "AMAT", lookback_yields = 10)
                             ####
                             
                             req(yields_monthly)
                             
                             withProgress(message = 'Fetching data', value = 0, {
                                 incProgress(1/3, detail = "Filtering data")
    
                                 start_date <- yields_monthly %>%
                                     dplyr::pull(date) %>%
                                     xts::last() %>%
                                     subtract(months(as.numeric(input$lookback_yields)))
                                 
                                 incProgress(2/3, detail = "Preparing data")
                                 # Sys.sleep(1)
                                 data_for_plot <- yields_monthly %>%
                                     dplyr::filter(date >= start_date) %>%
                                     dplyr::mutate(
                                         line_color = ifelse(
                                             date %in% xts::first(date),
                                             "yellow",
                                             ifelse(date %in% xts::last(date), "aqua", "blue")
                                         ),
                                         line_alpha = ifelse(date %in% c(xts::first(date), xts::last(date)), 1, 0.9)
                                     )
                                 
                                 
                                 
                                 incProgress(1, detail = "Plotting data")
                                 # Sys.sleep(1)
                                 data_for_plot %>%
                                     ggplot(aes(x = symbol, y = yield)) +
                                     geom_line(
                                         aes(
                                             group = date,
                                             color = line_color,
                                             alpha = line_alpha
                                         ),
                                         show.legend = FALSE,
                                         size = 1
                                     ) +
                                     scale_y_continuous(labels = scales::comma_format()) +
                                     scale_x_discrete(expand = expansion(mult = c(0, .2))) +
                                     geom_text_repel(
                                         data = data_for_plot  %>%
                                             dplyr::filter(symbol == "30 year") %>%
                                             dplyr::slice(c(1, dplyr::n())),
                                         aes(label = format(date, "%b %Y"),
                                             color = line_color),
                                         nudge_x = 45,
                                         size = 5,
                                         segment.color = NA,
                                         # fontface = "bold",
                                         show.legend = FALSE
                                     ) +
                                     labs(
                                         title = "Yield Curve",
                                         subtitle = "U.S. Treasury yields",
                                         y = "Yield",
                                         x = "Maturity"
                                     ) +
                                     theme_minimal() #+
                                 # theme(axis.text.x = element_text(angle = 90, hjust = 0.8,
                                 #                                  vjust = .5, color = "white", size = 12),
                                 #       axis.text.y = element_text(color = "white", size = 12),
                                 #       axis.ticks = element_blank(),
                                 #       axis.title.x = element_text(size = 15, color = "white"),
                                 #       axis.title.y = element_text(color = "white"),
                                 #
                                 #       panel.background = element_rect(fill = "#272b30", color = NA),
                                 #       panel.border = element_rect(fill = NA, color = "#272b30"),
                                 #       panel.grid.major = element_blank(),
                                 #       panel.grid.minor = element_blank(),
                                 #
                                 #       plot.background = element_rect(color = "#272b30", fill = "#272b30"),
                                 #       plot.title = element_text(color = "white"),
                                 #       plot.subtitle = element_text(color = "white"))
                            
                                 
                                 }) # END output
                             
                             
                         }) %>% bindCache(input$lookback_yields)
                         
                         
                         
                         
                         output$gdp <- renderPlot({
                             
                             ####
                             # For testing
                             ####
                             # input <- list(lookback_gdp = 4)
                             ####
                             
                             start_date <- gdp %>%
                                 dplyr::pull(date) %>% 
                                 xts::last() %>% 
                                 subtract(lubridate::years(input$lookback_gdp))
                             
                             data_for_plot <- gdp %>%
                                 dplyr::mutate(fill_color = ifelse(change > 0, "steelblue", "red"),
                                               sma = TTR::SMA(change, as.numeric(input$lookback_gdp)*4)) %>% # n-year MA
                                 dplyr::filter(date >= start_date)
                             
                             data_for_plot %>%
                                 ggplot(aes(x = date, y = change, fill = as.numeric(fill_color))) +
                                 geom_bar(stat = "identity", show.legend = FALSE) +
                                 geom_hline(aes(yintercept = xts::last(sma)), color = "black", size = 0.8,
                                            linetype = 1, alpha = 0.7) +
                                 # scale_x_date(date_breaks = "quarters") +
                                 scale_y_continuous(labels = scales::percent_format()) +
                                 labs(title = "% Change in GDP",
                                      subtitle = "Quarterly Percent change in Gross Domestic Product",
                                      y = "", x = "") +
                                 geom_label(aes(label = paste(round(100 * change, 1), "%")), 
                                           check_overlap = TRUE,
                                           fill = "white") + #, color = "white") #+ 
                                 theme_minimal()
                                 # theme(axis.text.x = element_text(angle = 90, hjust = 0.8,
                                 #                                  vjust = .5, color = "white", size = 12),
                                 #       axis.text.y = element_text(color = "white", size = 12),
                                 #       axis.ticks = element_blank(),
                                 #       axis.title.x = element_text(size = 15, color = "white"),
                                 #       axis.title.y = element_text(color = "white"),
                                 #       
                                 #       panel.background = element_rect(fill = "#272b30", color = NA),
                                 #       panel.border = element_rect(fill = NA, color = "#272b30"),
                                 #       panel.grid.major = element_blank(),
                                 #       panel.grid.minor = element_blank(),
                                 #       
                                 #       plot.background = element_rect(color = "#272b30", fill = "#272b30"),
                                 #       plot.title = element_text(color = "white"),
                                 #       plot.subtitle = element_text(color = "white"))
                             
                             # ggplotly(gdp_plot)
                         })
                         
                         
                         output$inflation <- renderPlot({
                             
                             ####
                             # For testing
                             ####
                             # input <- list(lookback_inflation = 4)
                             ####
                             
                             start_date <- inflation %>% 
                                 dplyr::pull(date) %>% 
                                 xts::last() %>% 
                                 subtract(lubridate::years(as.numeric(input$lookback_inflation)))
                             
                             data_for_plot <- inflation %>%
                                 dplyr::mutate(level = level / 100,
                                               sma = TTR::SMA(level, as.numeric(input$lookback_inflation))) %>% # n-year MA
                                 dplyr::filter(date >= start_date)
                             
                             data_for_plot %>%
                                 ggplot(aes(x = date, y = level)) +
                                 geom_bar(stat = "identity", show.legend = FALSE, fill = "#666666") +
                                 geom_hline(aes(yintercept = xts::last(sma)), color = "black", size = 0.8,
                                            linetype = 2, alpha = 0.7) +
                                 scale_x_date(date_breaks = "years", date_labels = "%Y") +
                                 scale_y_continuous(labels = scales::percent_format()) +
                                 labs(title = "Inflation rate",
                                      subtitle = "Yearly inflation rate",
                                      y = "", x = "") +
                                 geom_label_repel(aes(label = paste(round(100*level, 1), "%")),
                                                 size = 5,
                                                 segment.color = "white",
                                                 # color = "white",
                                                 nudge_y = .0025,
                                                 # fontface = "bold",
                                                 show.legend = FALSE) +
                                 theme_minimal()
                                 # theme(axis.text.x = element_text(angle = 90, hjust = 0.8,
                                 #                                  vjust = .5, color = "white", size = 12),
                                 #       axis.text.y = element_text(color = "white", size = 12),
                                 #       axis.ticks = element_blank(),
                                 #       axis.title.x = element_text(size = 15, color = "white"),
                                 #       axis.title.y = element_text(color = "white"),
                                 #       
                                 #       panel.background = element_rect(fill = "#272b30", color = NA),
                                 #       panel.border = element_rect(fill = NA, color = "#272b30"),
                                 #       panel.grid.major = element_blank(),
                                 #       panel.grid.minor = element_blank(),
                                 #       
                                 #       plot.background = element_rect(color = "#272b30", fill = "#272b30"),
                                 #       plot.title = element_text(color = "white"),
                                 #       plot.subtitle = element_text(color = "white"))
                         }) 
                         
                         
                         output$bond_yields <- renderPlot({
                             
                             ####
                             # For testing
                             ####
                             # input <- list(lookback_bond_yields = 12)
                             ####
                             
                             start_date <- bond_yields %>% 
                                 dplyr::pull(date) %>% 
                                 xts::last() %>% 
                                 subtract(months(as.numeric(input$lookback_bond_yields)))
                             
                             data_for_plot <- bond_yields %>%
                                 dplyr::mutate(sma = TTR::SMA(level, as.numeric(input$lookback_bond_yields))) %>% # n-year MA
                                 dplyr::filter(date >= start_date)
                             
                             data_for_plot %>%
                                 ggplot(aes(x = date, y = level/100)) +
                                 geom_line(aes(color = symbol)) + #show.legend = FALSE, ) +
                                 # geom_hline(aes(yintercept = data_for_plot %>% select(sma) %>% slice(n()), color = symbol), size = 0.8,
                                 #            linetype = 2, alpha = 0.7) +
                                 scale_x_date(date_breaks = "months", date_labels = "%b '%y") +
                                 scale_y_continuous(labels = scales::percent_format()) +
                                 labs(title = "Corporate Bond Yields",
                                      subtitle = "Moody's Seasoned Aaa and Baa Corporate Bond Yields",
                                      y = "", x = "") +
                                 theme(legend.position = "bottom") + 
                                 theme_minimal()
                                 # geom_text_repel(aes(label = symbol),
                                 #                 size = 5,
                                 #                 # segment.color = "white",
                                 #                 # color = "white",
                                 #                 nudge_y = .0025,
                                 #                 # fontface = "bold",
                                 #                 show.legend = FALSE) #+
                                 # theme(axis.text.x = element_text(angle = 90, hjust = 0.8,
                                 #                                  vjust = .5, color = "white", size = 12),
                                 #       axis.text.y = element_text(color = "white", size = 12),
                                 #       axis.ticks = element_blank(),
                                 #       axis.title.x = element_text(size = 15, color = "white"),
                                 #       axis.title.y = element_text(color = "white"),
                                 #       
                                 #       panel.background = element_rect(fill = "#272b30", color = NA),
                                 #       panel.border = element_rect(fill = NA, color = "#272b30"),
                                 #       panel.grid.major = element_blank(),
                                 #       panel.grid.minor = element_blank(),
                                 #       
                                 #       plot.background = element_rect(color = "#272b30", fill = "#272b30"),
                                 #       plot.title = element_text(color = "white"),
                                 #       plot.subtitle = element_text(color = "white"))
                         })
                         
                         # data(mtcars)
                         # library(corrplot)
                         # corr <- round(cor(mtcars), 1)
                         # 
                         # ggcorrplot::ggcorrplot(corr, hc.order = TRUE, type = "lower",
                         #                        colors = c("#6D9EC1", "white", "#E46726"),lab = TRUE)
                         # 
                         
                         output$cor_plot <- renderPlot(
                             ggcorrplot::ggcorrplot(
                                 cor(monthly_rets %>% select(-date) %>% 
                                     filter(!!sym(input$weak_ticker) < 0)),
                                 hc.order = TRUE,
                                 type = "lower",
                                 colors = c("#6D9EC1", "white", "#E46726"),
                                 lab = TRUE
                             )
                         )
                         
                         
                         # monthly_rets_xts <- monthly_rets %>% 
                         #     select(-date) %>% 
                         #     xts(order.by = monthly_rets$date)
                         
                         monthly_rets %>% 
                             filter(AGG < 0)
                         
                         
                         
                         
                         #############################
                         ### S&P500 Relative Value ###
                         #############################
                         
                         #############################
                         ####### The Fed Model #######
                         #############################
                         
                         ########################################################
                         ### Fed ratio = S&P earnings yield / LT T-bond yield ###
                         ########################################################
                         
                         
                         
                         #############################
                         ######## Yardeni Model#######
                         #############################
                         
                         # getSymbols('AAA','CPIAUCSL','Q13051USQ156NNBR',src="FRED")
                         # AAA 
                         # #CBY (AAA Corporate Bond Yield)
                         # 0.1 # Estimate of d
                         
                         
                         
                         ###########################@
                         ## P/10-year MA(Eearnings ##
                         ###########################@
                         }
        )
    }