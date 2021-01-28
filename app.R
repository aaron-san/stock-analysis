

# Debugging
# library(shinyobjects)
# load_reactive_objects()



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
# library(dplyr)
library(DT)
library(sass)
library(tidyverse)
library(quantmod)
library(PerformanceAnalytics)
library(lubridate)
# library(ggrepel)
# library(shinycssloaders)
# library(sever) # Disconnection message
# library(bootstraplib) # for using bootstrap css variables in Shiny

source("modules/returnsReturnsModule.R")
source("modules/backtestBacktestModule.R")
source("modules/returnsReturnsTableModule.R")
source("modules/dashboardAboutModule.R")


monthly_rets <- read_rds("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]
cash_rets <- read_rds("data/cash_returns.rds")
asset_rets <- read_rds("data/asset_returns.rds")





# Convert Sass to CSS
sass(sass_file("www/custom.scss"),
     output = "www/custom.css",
     cache = FALSE)

# bs4DashGallery()


###########
### UI ####
###########

ui <- bs4DashPage(
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
        p("Hi!")
    ),
    
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
            bs4SidebarMenuItem('Dashboard',
                               tabName = 'Dashboard',
                               icon = 'dashboard'),
            bs4SidebarMenuItem('Returns',
                               tabName = 'Returns',
                               icon = 'chart-line'),
            
            bs4SidebarMenuItem('Backtest',
                               tabName = "Backtest",
                               icon = "sliders-h")
        )
        
    ),
    
    controlbar = bs4DashControlbar(skin = "dark"),
    footer = bs4DashFooter(),
    # title = "test",
    
    
    
    
    # Body
    body = bs4DashBody(
        # Add custom css style
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        bs4TabItems(
            bs4TabItem(tabName = "Dashboard",
                       
                       dashboardAboutUI("dashboard_about")),
            
            
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
                                
                                
                                returnsReturnsUI("returns", ticker_choices = ticker_choices)
                            )
                            
                        )
                    ),
                    
                    bs4TabPanel(
                        tabName = "Returns Table",
                        returnsReturnsTableUI("returns_table", ticker_choices = ticker_choices)
                    )
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
                        
                        backtestBacktestUI(
                            "backtest",
                            ticker_choices = ticker_choices,
                            monthly_rets = monthly_rets
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
                )
            )
        )
    )
)


##############
### Server ###
##############

server <- function(input, output, session) {
    returnsReturnsServer(id = "returns",
                         monthly_rets = monthly_rets)
    
    backtestBacktestServer(id = "backtest",
                           asset_rets = asset_rets,
                           cash_rets = cash_rets)
    
    returnsReturnsTableServer(id = "returns_table",
                              monthly_rets = monthly_rets)
    
}





# library(gapminder) # for gapminder dataset
# library(plotly) # for plotly charts and %>% pipe operator
#
# ## Size of the scatter data point will represent the population
# ## Color of the scatter data point will represent the continent
#
# gapminder %>%
#     plot_ly() %>%
#     # add frame argument for animation and map to the variable needed on the timeline
#     add_markers(x=~gdpPercap, y=~lifeExp,
#                 frame=~year,
#                 size = ~pop,
#                 color=~continent,
#                 marker=list(sizemode="diameter"),
#                 text=~paste("Life Expectancy: ", round(lifeExp,1),
#                             "<br>",
#                             "GDP Per Capita:", round(gdpPercap,1),
#                             "<br>",
#                             "Country:", country,
#                             "<br>",
#                             "Population:", pop),
#                 hoverinfo= "text") %>%
#     layout(title="Animated Plotly Bubble Plot",
#            xaxis=list(title="GDP Per Capita (log scale)", type="log"),
#            yaxis=list(title= "Life Expectancy"))








shinyApp(ui = ui, server = server)
