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
                     icon = NULL,
                     href = "https://www.linkedin.com/in/theodorecheek/"),
            menuItem('State Dashboard',
                     tabName = 'Breakdown',
                     icon = icon("map")),
            menuItem(
                'National Dashboard',
                tabName = 'Involvement',
                icon = icon('users')
            ),
            menuItem(
                'Data Reporting',
                tabName = "News",
                icon = icon("newspaper")
            )
        ),
        sliderInput(
            "daterange",
            "Select a Date Range",
            min = monthly_rets %>% pull(date) %>% min(),
            max = monthly_rets %>% pull(date) %>% max(),
            value = c(as.Date("2014-01-01"), as.Date("2018-03-01")),
            timeFormat = "%b %Y"#,
            # width = 400
        ),
        actionButton("showPlot", "Plot", icon("caret-right"), class = "btn-warning"), # Bootstrap class        
        selectizeInput("tickers",
                    label = "Select Tickers:",
                    choices = ticker_choices,
                    multiple = TRUE),
        checkboxInput('all', 'Select All/None', value = TRUE)
        
    ),
    
    # Body Section ####
    dashboardBody(tabItems(
        tabItem(tabName = "Breakdown",
                tabsetPanel(
                    tabPanel(
                        "Returns",
                        br(),
                        h4("Tickers"),
                        box(width = 4,
                               withTags(
                                    div(
                                        b("Large-Mid-Small Cap equities"),
                                        ul(li("SPY, MDY, IWM")),
                                        b("Intl.and emerging markets"),
                                        ul(li("EFA, EEM"))
                                    )
                                    )
                               ),
                        box(width = 4,
                               withTags(
                                   div(
                                       b("Bonds"),
                                       ul(li("AGG, TIP, TLT, LQD")),
                                       b("Commodities"),
                                       ul(li("GSG"))
                                   )
                               )
                        ),
                        box(width = 4,
                            withTags(
                                div(
                                    b("Real Estate"),
                                    ul(li("RWR, RWX, MBB")),
                                    b("Cash"),
                                    ul(li("SHV"))
                                )
                            )
                        ),
                        
                        br(),
                        plotOutput("returnsPlot")
                    ),
                    tabPanel(## * State Dashboard ####
                             "Geographical Overview",
                             fluidRow(
                                 box(
                                     title = "Incident Density Tracker by State",
                                     solidHeader = T,
                                     status = "primary",
                                     htmlOutput("gun.map"),
                                     footer = "Hover for state-by-state incident stats, scaled below",
                                     width = 12
                                 )
                             ))
                )
                ),
        tabItem(tabName = "News",
                tabsetPanel(
                    ## * Filtered, Searchable Data ####
                    tabPanel("Filtered, Searchable Data",
                             fluidRow(
                                 box(
                                     title = "Select Details from the Filtered Dataset",
                                     status = "primary",
                                     solidHeader = T,
                                     "Here you will find searchable, selected notes
                             from the dataset that has been filtered according
                             to your target characteristic. In order to save
                             this selection, use the buttons below to capture
                             the information",
                                     DT::dataTableOutput("detailtable"),
                                     width = 12
                                 )
                                 )
                             )
                    )
                )
    ))
))

    

