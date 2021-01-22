




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

source("modules/returnsReturnsModule.R")
source("modules/backtestBacktestModule.R")

# Convert Sass to CSS
# sass <- readLines("www/custom.scss")
# writeLines(sass, paste0("www/custom - ", gsub("-", " ", Sys.Date()), ".css"))


# bs4DashGallery()


# Define UI for application that draws a histogram
shinyUI(
    ui = bs4DashPage(
        sidebar_collapsed = TRUE,
        controlbar_collapsed = TRUE,
        
        navbar = bs4DashNavbar(
            skin = "light",
            status = "white",
            sidebarIcon = "bars",
            # leftUi = actionButton("goButton1", "Go1"),
            rightUi = screenshotButton(selector = ".content", label = "Take a Screenshot"),
            fixed = TRUE,
            compact = TRUE,
            h4("Hi!")
        ),
        
        # Sidebar
        sidebar = bs4DashSidebar(
            inputId = "sidebar",
            disable = FALSE,
            title = "Stock Analysis",
            skin = "dark",
            status = "danger",
            brandColor = "teal",
            # NULL
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
                                   icon = 'chart-line'),
                # bs4SidebarMenuSubItem(
                #     text = "Returns",
                #     tabName = "Returns",
                #     icon = "circle-thin"
                # )),
                
                bs4SidebarMenuItem('Backtest',
                                   tabName = "Backtest",
                                   icon = "sliders-h")
            )
            
        ),
        
        controlbar = bs4DashControlbar(skin = "light"),
        footer = bs4DashFooter(),
        # title = "test",
        
        
        
        # Body
        body = bs4DashBody(# shinythemes::themeSelector(),
            
            bs4TabItems(
                bs4TabItem(
                    tabName = "Returns",
                    
                    bs4TabSetPanel(
                        id = "tabsetpanel1",
                        side = "left",
                        
                        bs4TabPanel(
                            tabName = "Returns",
                            br(),
                            
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
                                    span(withTags(span(
                                        b("Large-Mid-Small Cap equities"),
                                        ul(li("SPY, MDY, IWM")),
                                        b("Intl.and emerging markets"),
                                        ul(li("EFA, EEM"))
                                    ))),
                                    span(withTags(span(
                                        b("Bonds"),
                                        ul(li("AGG, TIP, TLT, LQD")),
                                        b("Commodities"),
                                        ul(li("GSG"))
                                    ))),
                                    span(withTags(span(
                                        b("Real Estate"),
                                        ul(li("RWR, RWX, MBB")),
                                        b("Cash"),
                                        ul(li("SHV"))
                                    ))),
                                    
                                    
                                    returnsReturnsModuleUI("returns")
                                    #... .module
                                    
                                    # 3rd row
                                    # ,
                                    
                                    
                                )
                                
                            ),
                            p("This is the bottom")
                        ),
                        
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
                    
                                backtestBacktestModuleUI("backtest")
                            ),
                    
                    bs4TabPanel(tabName = "Explanation",
                                bs4Card(width = 12,
                                        p(
                                            "Here is the explanation..."
                                        ))),
                    bs4TabPanel(tabName = "Ideas",
                                bs4Card(
                                    width = 12,
                                    p(
                                        span("Feb: +XLE (Energy Select ETF)"),
                                        span("May: -XLE,"),
                                        span("+Bonds (e.g,. TLT - 20yr Treasury ETF)"),
                                        span("Aug: -TLT,"),
                                        span("+TVIX"),
                                        span("Sep: -TVIX,"),
                                        span("+XLK (Technology Select ETF)"),
                                        span("(Using stop-losses and take-profits (can't be 100% works)).")
                                    )
                                ))
                )
            ))
            ),
        # Add custom css style
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        )
    )
    )

