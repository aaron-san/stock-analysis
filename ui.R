


# bs4Card
# function (..., inputId = NULL, title = NULL, footer = NULL, status = NULL,
#           elevation = NULL, solidHeader = FALSE, headerBorder = TRUE,
#           gradientColor = NULL, width = 6, height = NULL, collapsible = TRUE,
#           collapsed = FALSE, closable = TRUE, maximizable = FALSE,
#           labelStatus = NULL, labelText = NULL, labelTooltip = NULL,
#           dropdownMenu = NULL, dropdownIcon = "wrench", overflow = FALSE,
#           enable_sidebar = FALSE, sidebar_content = NULL, sidebar_width = "25%",
#           sidebar_background = "#333a40", sidebar_start_open = FALSE,
#           sidebar_icon = "cogs")



# library(shinyobjects)
# load_reactive_objects()
#
# #   [ui-object] %>%
# #      view_ui(close_after = NULL)
#
# # First select code to convert to html,
# #  then in the console type:
# convert_selection()

library(shiny)
# library(shinydashboard)
library(bs4Dash)
library(shinyjs)
library(shinyscreenshot)
library(plotly)
library(shinyWidgets)
library(shinythemes)
# library(semantic.dashboard)
library(dplyr)
library(DT)
library(sass)
# library(sever) # Disconnection message
# library(bootstraplib) # for using bootstrap css variables in Shiny

# Convert Sass to CSS
sass <- readLines("www/custom.scss")
writeLines(sass, paste0("www/custom - ", gsub("-", " ", Sys.Date()), ".css"))

# sass::sass(sass_file("www/custom.scss"),
#            output = paste0("www/custom - ", gsub("-", " ", Sys.Date()), ".css"))

# Get current css file
current_css <-
    list.files("www", pattern = "\\d{4} \\d{2} \\d{2}\\.css") %>% max()



monthly_rets <- readRDS("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]

# bs4DashGallery()

# Define UI for application that draws a histogram
shinyUI(
    ui = bs4DashPage(
        # use_sever(),
        # sidebar_collapsed = TRUE,
        # controlbar_collapsed = TRUE,
        
        navbar = bs4DashNavbar(
            skin = "light", status = "white", sidebarIcon = "bars", 
            leftUi = actionButton("goButton1", "Go1"), 
            rightUi = actionButton("goButton2", "Go2"),
            fixed = TRUE,
            # compact = TRUE,
            p("Hi!")
        ),
        
        # Sidebar
        sidebar = bs4DashSidebar(
            inputId = "sidebar", 
            disable = FALSE, 
            title = "Stock Analysis", 
            skin = "dark", 
            status = "danger", 
            brandColor = "teal", # NULL
            url = "https://www.linkedin.com/in/aaron-hardy-651b2410/",
            src = "https://media-exp1.licdn.com/dms/image/C5603AQFS5xs6nUSEKg/profile-displayphoto-shrink_400_400/0/1517740988201?e=1616630400&v=beta&t=D3AghKSecL_waNSvQqdCyjTEKoDuMQZK16HRnUoQzro",
            expand_on_hover = TRUE,
            elevation = 4,
            opacity = 0.8,
            
            # Sidebar menu
            bs4SidebarMenu(
                id = "sidebarmenu",
                flat = FALSE,
                compact = FALSE,
                child_indent = TRUE,
                # bs4SidebarMenuItem("A Shiny App by Aaron Hardy",
                #                    icon = NULL,
                #                    href = "https://www.linkedin.com/in/aaron-hardy-651b2410/"),
                bs4SidebarMenuItem('Returns',
                                   tabName = 'Returns',
                                   icon = 'chart-line',
                                   bs4SidebarMenuSubItem(
                                       text = "Item 1",
                                       tabName = "item1",
                                       icon = "circle-thin"
                                   )),
                
                bs4SidebarMenuItem('Backtest',
                                   tabName = "Backtest",
                                   icon = "sliders-h")
            ),
            screenshotButton(selector = ".content", label = "Take a Screenshot")
        ),
        
        controlbar = bs4DashControlbar(skin = "light"),
        footer = bs4DashFooter(),
        title = "test",
        
        
        # Body
        body = bs4DashBody(# shinythemes::themeSelector(),
            
            # Add custom css style
            # tags$head(
            #     tags$link(rel = "stylesheet", type = "text/css", href = current_css)
            # ),
            
            bs4TabItems(
                # 1st tab item
                bs4TabItem(
                    tabName = "Returns",
                    # Tabset panel
                    bs4TabSetPanel(
                        id = "tabsetpanel1",
                        side = "left",
                        # Tab panel #1
                        bs4TabPanel(
                            tabName = "Returns",
                            br(),
                            # 1st fluid row
                            bs4Card(
                                width = 12,
                                title = "Tickers",
                                status = "morning",
                                solidHeader = T,
                                labelText = 1,
                                dropdownIcon = "wrench",
                                labelStatus = "danger",
                                labelTooltip = "Hi bro",
                                dropdownMenu = dropdownItemList(
                                    dropdownItem(url = "", name = "google link"),
                                    dropdownItem(url = "", name = "item2"),
                                    dropdownDivider(),
                                    dropdownItem(url = "", name = "item3")
                                ),
                                fluidRow(
                                    # wellPanel(
                                    column(width = 4,
                                           withTags(span(
                                               b("Large-Mid-Small Cap equities"),
                                               ul(li("SPY, MDY, IWM")),
                                               b("Intl.and emerging markets"),
                                               ul(li("EFA, EEM"))
                                           ))),
                                    column(width = 4,
                                           withTags(span(
                                               b("Bonds"),
                                               ul(li("AGG, TIP, TLT, LQD")),
                                               b("Commodities"),
                                               ul(li("GSG"))
                                           ))),
                                    column(width = 4,
                                           withTags(span(
                                               b("Real Estate"),
                                               ul(li("RWR, RWX, MBB")),
                                               b("Cash"),
                                               ul(li("SHV"))
                                           )))
                                    # )
                                )
                            ),
                            # 2nd row
                            bs4Card(
                                width = 12,
                                title = "Choose plot parameters",
                                status = "primary",
                                solidHeader = T,
                                fluidRow(column(
                                    10,
                                    # pickerInput(
                                    #     inputId = "tickers",
                                    #     label = "Select Tickers:",
                                    #     choices = ticker_choices,
                                    #     options = list(
                                    #         `actions-box` = TRUE),
                                    #     multiple = TRUE
                                    # )#,
                                    selectizeInput(
                                        "tickers",
                                        label = "Select Tickers:",
                                        choices = ticker_choices,
                                        multiple = TRUE
                                    )
                                ),
                                column(
                                    2,
                                    awesomeCheckbox('all',
                                                    'Select All/None',
                                                    value = TRUE)
                                )),
                                fluidRow(
                                    column(
                                        6,
                                        #  sliderInput(
                                        #     "daterange",
                                        #     "Select a Date Range",
                                        #     min = monthly_rets %>% pull(date) %>% min(),
                                        #     max = monthly_rets %>% pull(date) %>% max(),
                                        #     value = c(as.Date("2014-01-01"), monthly_rets %>% pull(date) %>% max()),
                                        #     timeFormat = "%b %Y"#,
                                        #     # width = 400
                                        # ),
                                        shinyWidgets::radioGroupButtons(
                                            "date_lookback",
                                            "",
                                            choices = c("All", "10 yrs", "5 yrs", "1 yr", "3 mon"),
                                            selected = "All"
                                        )
                                    ),
                                    column(3,
                                           actionButton(
                                               "showPlot", "Plot", icon("caret-right"), class = "btn-info"
                                           ))
                                )
                            ),
                            # 3rd row
                            bs4Card(
                                width = 12,
                                title = "Returns Index",
                                status = "primary",
                                solidHeader = T,
                                fluidRow(column(width = 12,
                                                plotlyOutput("returns_plot")))
                            ),
                            
                            p("This is the bottom")
                        ),
                        # Tab panel item #2
                        bs4TabPanel(tabName = "Returns Table",
                                    bs4Card(
                                        width = 12,
                                        fluidRow(# title = "Incident Density Tracker by State",
                                            # solidHeader = T,
                                            # status = "primary",
                                            DT::dataTableOutput("returns_table"),
                                            # footer = "Hover for state-by-state incident stats, scaled below",
                                            width = 12)
                                    ))
                    )
                ),
                bs4TabItem(
                    tabName = "Backtest",
                    bs4TabSetPanel(
                        id = "tabsetpanel2",
                        side = "left",
                        # Filtered, Searchable Data
                        bs4TabPanel(
                            tabName = "Backtest",
                            br(),
                            # 1st row
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
                                    column(
                                        width = 3,
                                        selectInput(
                                            inputId = "ticker_decision_rule",
                                            label = "Ticker used in decision rule (x)",
                                            selected = "EEM",
                                            choices = ticker_choices
                                        )
                                    ),
                                    column(
                                        width = 3,
                                        selectInput(
                                            inputId = "invested_ticker",
                                            label = "Ticker to invest in if rule is triggered",
                                            selected = "EEM",
                                            choices = ticker_choices
                                        )
                                    ),
                                    column(width = 6,
                                           textInput("signal_rule",
                                                     "Enter a decision rule:",
                                                     value = "if_else(x < 0, 1, 0)")
                                           )
                                    
                                ),
                                fluidRow(
                                    column(
                                        width = 6,
                                        sliderInput(
                                            "daterange_bt",
                                            "Select a Date Range",
                                            min = monthly_rets %>% pull(date) %>% min(),
                                            max = monthly_rets %>% pull(date) %>% max(),
                                            value = c(as.Date("2014-01-01"), monthly_rets %>% pull(date) %>% max()),
                                            timeFormat = "%b %Y"#,
                                            # width = 400
                                        )
                                    ),
                                    column(width = 3,
                                           actionButton("bt_button", "Plot"))), 
                                fluidRow(
                                    # actionLink("infoLink", "Information Link", class = "btn-info"),
                                    plotOutput("backtest_plot")
                                )
                            )
                        ),
                        bs4TabPanel(tabName = "Explanation",
                                    bs4Card(width = 12,
                                            p(
                                                "Here is the explanation..."
                                            )))
                    )
                )
            ))
    )
)
