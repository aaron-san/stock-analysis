


mod_backtest_backtest_ui <-
    function(id, ticker_choices, monthly_rets) {
        ns <- NS(id)
        
        tagList(
            bs4Card(
                width = 12,
                fluidRow(
                    width = 12,
                    title = "Test a Decision Rule",
                    status = "primary",
                    solidHeader = T,
                    wellPanel(
                        p(
                            "Here you can explore custom decision rules directly below. Simply
                                     enter the condition in a format similar to these:"
                        ),
                        p("if_else(x < 0, 1, 0)"),
                        p("if_else(SMA(x, 3) > SMA(x, 6), 1, 0)"),
                        p(tags$b("if_else(x < -0.2, 1, 0)")),
                        p(
                            "if_else(SMA(x, 3) > 0.01 & stats::lag(x, 1) > 0 & runSD(x, 3) < 0.1, 1, 0)"
                        )
                    )
                )
            ),
            # 2nd row
            bs4Card(
                width = 12,
                title = "Decision Rule",
                solidHeader = T,
                fluidRow(
                    selectInput(
                        width = 200,
                        inputId = ns("ticker_decision_rule"),
                        label = "Ticker used in decision rule (x)",
                        selected = "EEM",
                        choices = ticker_choices
                        
                    ),
                    
                    selectInput(
                        width = 200,
                        inputId = ns("invested_ticker"),
                        label = "Ticker to invest in if rule is triggered",
                        selected = "EEM",
                        choices = ticker_choices
                        
                    ),
                    
                    textInput(
                        width = 300,
                        inputId = ns("signal_rule"),
                        "Enter a decision rule:",
                        value = "if_else(x < 0, 1, 0)"
                    )
                    
                    
                ),
                fluidRow(
                    sliderInput(
                        width = 400,
                        inputId = ns("daterange_bt"),
                        "Select a Date Range",
                        min = monthly_rets %>% pull(date) %>% min(),
                        max = monthly_rets %>% pull(date) %>% max(),
                        value = c(
                            as.Date("2014-01-01"),
                            monthly_rets %>% pull(date) %>% max()
                        ),
                        timeFormat = "%b %Y"#,
                        # animate = animationOptions(interval = 500)#,
                        # width = 400
                      
                    ),
                    span(),  
                    actionButton(
                        width = 70,
                        inputId = ns("bt_button"),
                        "Plot",
                        icon("caret-right"),
                        class = "btn-primary"
                    )
                ),
                fluidRow(
                    plotOutput(ns(
                    "backtest_plot"
                )))
            )
        )
    }


mod_backtest_backtest_server <-
    function(id, asset_rets, cash_rets) {
        moduleServer(id,
                     function(input, output, session) {
                         # Parse the decision rule
                         fun <- eventReactive(input$bt_button, {
                             eval(parse(
                                 text = paste(
                                     'get_signal <- function(x) { return(',
                                     input$signal_rule,
                                     ')}',
                                     collapse = ''
                                 )
                             ))
                         }) #, ignoreInit = TRUE)
                         
                         
                         
                         
                         
                         output$backtest_plot <- renderPlot({
                             # Step 2: Create your indicator
                             validate(need(isolate(input$signal_rule), "Enter signal rule."))
                             
                             asset_returns_filt <- reactive({
                                 asset_rets[paste(isolate(input$daterange_bt), collapse = "::")]
                                 # asset_rets[paste(c("2001-12-13", "2003-12-31"), collapse = "::")]
                                 
                             })
                             
                             
                             invested_asset_returns <- reactive({
                                 asset_returns_filt() %>%
                                     .[, isolate(c(input$invested_ticker))]
                             })
                             
                             
                             
                             signals <-
                                 asset_returns_filt() %>%
                                 .[, isolate(input$ticker_decision_rule)] %>%
                                 # .[, "EEM"] %>%
                                 apply(., 2, fun()) %>%
                                 # apply(., 2, function(x) if_else(x < -0.2, 1, 0)) %>%
                                 as.xts(order.by = index(asset_returns_filt()))
                             
                             get_weights <- function(x, data) {
                                 x / rowSums(data)
                             }
                             
                             wts <-
                                 signals %>%
                                 apply(., 2, function(x)
                                     get_weights(x, data = signals)) %>%
                                 # round(3) %>%
                                 as.xts(order.by = index(signals))
                             wts[is.nan(wts)] <- 0
                             wts <- wts %>% na.omit()
                             
                             # rowSums(wts)
                             wts$SHV <- 1 - rowSums(wts)
                             
                             
                             # Signals are generated the monthly before performance is earned
                             strat_returns <-
                                 (merge(invested_asset_returns(), cash_rets) * stats::lag(wts) - 0.0005) %>%
                                 rowSums() %>%
                                 xts(order.by = index(wts)) %>%
                                 na.omit()
                             colnames(strat_returns) <- "Strategy"
                             
                             # Step 3: Use indicator to create equity curves
                             rets <-
                                 merge(asset_returns_filt()[, isolate(input$invested_ticker)], strat_returns) %>% na.omit()
                             
                             # Step 4: Evaluate strategy performance
                             # table.DownsideRisk(rets)
                             # table.Stats(rets)
                             charts.PerformanceSummary(rets, wealth.index = TRUE)
                             # chart.RelativePerformance(rets[ , 2], rets[ ,1])
                             # chart.RiskReturnScatter(rets)
                             
                             # Return.annualized(rets)
                             # SharpeRatio.annualized(rets)
                             # Return.annualized(rets)/maxDrawdown(rets)
                             # maxDrawdown(rets)
                             
                             
                             
                             # apply.yearly(rets, Return.cumulative) # geometric annual returns
                             # apply.yearly(rets, maxDrawdown)
                             # apply.yearly(rets, SharpeRatio.annualized)
                             #
                             # cor(rets)
                             #
                             # hist(rets$SPY, col="grey")
                             # hist(rets$strat_returns, col="grey")
                             # qqplot(rets$SPY, rets$strat_returns)
                             # qqnorm(rets$SPY)
                             # qqline(rets$strat_returns)
                         })
                         
                     })
    }