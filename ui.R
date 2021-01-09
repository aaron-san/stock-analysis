#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# remove.packages("promises")
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyscreenshot)
library(plotly)
# library(semantic.dashboard)
library(dplyr)
library(DT)

monthly_rets <- readRDS("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]



# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    
    dashboardHeader(title = "Stock Analysis"),
    
    dashboardSidebar(
        
        ## * Initial Menu Tabs ####
        sidebarMenu(
            menuItem("A Shiny App by Theodore Cheek",
                     icon = NULL#,
                     # href = "https://www.linkedin.com/in/theodorecheek/"
                     ),
            menuItem('State Dashboard',
                     tabName = 'Breakdown',
                     icon = icon("map")),
            menuItem(
                'Returns',
                tabName = 'Returns',
                icon = icon('users')
            ),
            menuItem(
                'Backtest',
                tabName = "Backtest",
                icon = icon("newspaper")
            )
        ),
        sliderInput(
            "daterange",
            "Select a Date Range",
            min = monthly_rets %>% pull(date) %>% min(),
            max = monthly_rets %>% pull(date) %>% max(),
            value = c(as.Date("2014-01-01"), monthly_rets %>% pull(date) %>% max()),
            timeFormat = "%b %Y"#,
            # width = 400
        ),
        actionButton("showPlot", "Plot", icon("caret-right"), class = "btn-info"), # Bootstrap class
        
        
        selectizeInput("tickers",
                    label = "Select Tickers:",
                    choices = ticker_choices,
                    multiple = TRUE),
        checkboxInput('all', 'Select All/None', value = TRUE)#,
        # screenshotButton(selector = ".content", label = "Take a Screenshot")
    ),
    
    # Body Section ####
    dashboardBody(
        
        # Add custom css styles
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
        ),
        
        tabItems(
        tabItem(tabName = "Returns",
                tabsetPanel(
                    tabPanel(
                        "Returns",
                        br(),
                        h4("Tickers"),
                        fluidRow(
                            column(width = 4,
                            wellPanel(
                                withTags(
                                    span(
                                        b("Large-Mid-Small Cap equities"),
                                        ul(li("SPY, MDY, IWM")),
                                        b("Intl.and emerging markets"),
                                        ul(li("EFA, EEM"))
                                    )
                            )
                            )
                        ),
                        column(width = 4,    
                            wellPanel(
                                    withTags(
                                        span(
                                            b("Bonds"),
                                            ul(li("AGG, TIP, TLT, LQD")),
                                            b("Commodities"),
                                            ul(li("GSG"))
                                        )
                                    )
                                )
                            ),
                        column(width = 4,
                            wellPanel(
                                withTags(
                                    span(
                                        b("Real Estate"),
                                        ul(li("RWR, RWX, MBB")),
                                        b("Cash"),
                                        ul(li("SHV"))
                                    )
                                )
                            )
                        )
                        ),
                        br(),
                        box(width = 12, 
                            plotlyOutput("returns_plot")),
                        p("This is the bottom")
                    ),
                    tabPanel(## * State Dashboard ####
                             "Returns Table",
                             fluidRow(box(
                                     # title = "Incident Density Tracker by State",
                                     # solidHeader = T,
                                     # status = "primary",
                                     DT::dataTableOutput("returns_table"),
                                     # footer = "Hover for state-by-state incident stats, scaled below",
                                     width = 12
                                 ))
                             )
                )
                ),
        tabItem(tabName = "Backtest",
                tabsetPanel(
                    ## * Filtered, Searchable Data ####
                    tabPanel("Backtest",
                             fluidRow(
                                 box(
                                     title = "Test a Decision Rule",
                                     status = "primary",
                                     solidHeader = T,
                                     wellPanel(
                                         p("Here you can explore custom decision rules directly below. Simply 
                                     enter the condition in a format similar to these:"),
                                         p("if_else(x < 0, 1, 0)"),
                                         p("if_else(SMA(x, 20) > SMA(x, 50), 1, 0)"),
                                         p(tags$b("if_else(x < -0.2, 1, 0)")),
                                         p("if_else(SMA(x, 3) > 0.05/3 & stats::lag(x, 1) > 0 & runSD(x, 3) < 0.5/3, 1, 0)")
                                     ),
                                     textInput("signal_rule", 
                                                  "Enter a decision rule:",
                                                  value = "if_else(x < 0, 1, 0)"),
                                     actionButton("bt_button", "Plot"),
                                     # actionLink("infoLink", "Information Link", class = "btn-info"),
                                     plotOutput("backtest_plot"),
                                     width = 12
                                 )
                                 )
                             )
                    )
                )
    ))
))

    



