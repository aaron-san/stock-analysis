
# mod_charts_economy



mod_charts_economy_ui <-
    function(id, dates_gdp, dates_yields, dates_inflation, dates_bond_yields) {
        ns <- NS(id)
        
        tagList(
            fluidRow(
                p("The economic growth regime changes in the '80s with Reagan to a debt-driven growth (growth prior to the '80s is irrelevant
00's (the oh-oh's")
                ),
            fluidRow(
                boxPlus(
                    width = 6,
                    plotOutput(ns("gdp"), height = "300px"),
                    footer = selectizeInput(
                        ns("lookback_gdp"),
                        label = "Lookback (No. years):",
                        choices = 1:(length(dates_gdp)/4 - 1),
                        selected = 3,
                        width = "50%"
                    )
                ),
                boxPlus(
                    width = 6,
                    plotOutput(ns("yields"), height = "300px"),
                    footer = sliderInput(
                        ns("lookback_yields"),
                        label = "Lookback (No. months):",
                        width = "80%",
                        min = 0,
                        max = length(dates_yields),
                        step = 3,
                        value = 6
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
                        selected = 5,
                        width = "50%"
                    )
                ),
                box(
                    width = 6,
                    plotOutput(ns("bond_yields"), height = "300px"),
                    footer = selectizeInput(
                        ns("lookback_bond_yields"),
                        label = "Lookback (No. months):",
                        choices = 1:(length(dates_bond_yields) - 1),
                        selected = 12,
                        width = "50%"
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
                             
                             start_date <- yields_monthly %>%
                                 dplyr::pull(date) %>%
                                 xts::last() %>% 
                                 subtract(months(as.numeric(input$lookback_yields)))
                             
                             data_for_plot <- yields_monthly %>%
                                 dplyr::filter(date >= start_date) %>%
                                 dplyr::mutate(line_color = ifelse(date %in% xts::first(date), "red",
                                                                   ifelse(date %in% xts::last(date), "steelblue", "white")),
                                               line_alpha = ifelse(date %in% c(xts::first(date), xts::last(date)), 1, 0.7))
                             
                             
                             
                             
                             data_for_plot %>%
                                 ggplot(aes(x = symbol, y = yield)) + 
                                 geom_line(aes(group = date, color = line_color, alpha = line_alpha),
                                           show.legend = FALSE, size = 1) +
                                 scale_y_continuous(labels = scales::comma_format()) +
                                 scale_x_discrete(expand = expansion(mult = c(0, .2))) +
                                 geom_text_repel(data = data_for_plot  %>%
                                                     dplyr::filter(symbol == "30 year") %>%
                                                     dplyr::slice(c(1, dplyr::n())),
                                                 aes(label = format(date, "%b %Y"),
                                                     color = line_color),
                                                 nudge_x = 45,
                                                 size = 5,
                                                 segment.color = NA,
                                                 # fontface = "bold",
                                                 show.legend = FALSE) +
                                 
                                 
                                 labs(title = "Yield Curve",
                                      subtitle = "U.S. Treasury yields",
                                      y = "Yield", x = "Maturity") #+
                                 # theme_dark() +
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
                                 geom_hline(aes(yintercept = xts::last(sma)), color = "white", size = 0.8,
                                            linetype = 2, alpha = 0.7) +
                                 # scale_x_date(date_breaks = "quarters") +
                                 scale_y_continuous(labels = scales::percent_format()) +
                                 labs(title = "% Change in GDP",
                                      subtitle = "Quarterly Percent change in Gross Domestic Product",
                                      y = "", x = "") +
                                 geom_label(aes(label = paste(round(100 * change, 1), "%")), 
                                           check_overlap = TRUE,
                                           fill = "white") #, color = "white") #+ 
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
                                 geom_hline(aes(yintercept = xts::last(sma)), color = "white", size = 0.8,
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
                                                 show.legend = FALSE) #+
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
                                 theme(legend.position = "bottom")
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
                         }
        )
    }