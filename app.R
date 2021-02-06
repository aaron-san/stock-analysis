



# theme_pander() +
# scale_fill_pander()



# htmlOutput("gun.map")
#
# output$gun.map = renderGvis({
#     dash.data = dashdata.df()
#
#     # Produces the gvis output, using the counted incidents
#     gvisGeoChart(
#         dash.data,
#         locationvar = "state",
#         colorvar = "Incidents",
#         options = list(
#             region = "US",
#             displayMode = "auto",
#             resolution = "provinces",
#             datalessRegionColor = "grey"
#         )
#     )
#
# })




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
library(shinydashboardPlus)
# library(bs4Dash)
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
library(highcharter)
library(ggthemes)
# library(tidyquant)
# library(timetk)
# library(ggrepel)
# library(shinycssloaders)
# library(sever) # Disconnection message

# Source all modules
lapply(
    list.files("modules", pattern = ".R", full.names = TRUE),
    FUN = function(x)
        source(x)
)

# Load data
monthly_rets <- read_rds("data/monthly_rets_tbl.rds")
ticker_choices <- colnames(monthly_rets)[-1]
cash_rets <- read_rds("data/cash_returns.rds")
asset_rets <- read_rds("data/asset_returns.rds")

# Convert Sass to CSS
sass(sass_file("www/custom.scss"),
     output = "www/custom.css",
     cache = FALSE)


###########
### UI ####
###########

ui <- dashboardPagePlus(
    collapse_sidebar = FALSE,
    sidebar_fullCollapse = TRUE,
    title = "Stock Analysis",
    skin = c("red-light"), #"midnight", "black-light"
    
    # sidebar_background = NULL,
    
    
    header = dashboardHeaderPlus(enable_rightsidebar = TRUE,
                                 rightSidebarIcon = "gears"),
    
    # navbar = bs4DashNavbar(
    #     skin = "light",
    #     status = "white",
    #     sidebarIcon = "bars",
    #     # leftUi = actionButton("goButton1", "Go1"),
    #     rightUi = screenshotButton(selector = ".content", label = "Take a Screenshot"),
    #     fixed = TRUE,
    #     compact = TRUE,
    #     p("Tools for investing!", class = "navbar-text")
    # ),
    
    sidebar = dashboardSidebar(
        sidebarUserPanel(
            "Aaron Hardy",
            subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
            # Image file should be in www/ subdir
            image = "https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ"
        ),
        sidebarSearchForm(label = "Enter a number", "searchText", "searchButton"),
        sidebarMenu(
            # Setting id makes input$tabs give the tabName of currently-selected tab
            id = "tabs",
            menuItem(
                "Dashboard",
                tabName = "dashboard",
                icon = icon("dashboard")
            ),
            menuItem(
                "Returns",
                icon = icon("th"), # chart-line
                tabName = "returns",
                badgeLabel = "new",
                badgeColor = "green"
            ),
            menuItem(
                "Widgets",
                icon = icon("th"),
                tabName = "widgets",
                badgeLabel = "new",
                badgeColor = "green"
            ),
            menuItem(
                "Charts",
                icon = icon("bar-chart-o"),
                menuSubItem("Sub-item 1", tabName = "subitem1"),
                menuSubItem("Sub-item 2", tabName = "subitem2")
            ),
            menuItem('Backtest',
                               tabName = "Backtest",
                               icon = icon("sliders-h")
                     ),
            menuItem('Research',
                               tabName = "Research",
                               icon = icon("lightbulb")
                     ) #wrench, lock (ionicon.com)
        )
    ),
    
    
  
    # url = "https://www.linkedin.com/in/aaron-hardy-651b2410/",
    # src = "https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ",
    
    


    # controlbar = bs4DashControlbar(skin = "dark"),
    footer = dashboardFooter(
        left_text = "By Aaron Hardy",
        right_text = format(Sys.Date(), "%Y")#,
        # copyrights = a(href = "https://www.investwithr.com", target = "https://www.investwithr.com",
        #                "Invest with R"),
    ),
    
    
    body = dashboardBody(
        # Add custom css style
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
        ),
        
        setShadow(class = "dropdown-menu"),
        
        tabItems(
            tabItem(tabName = "dashboard",
                    
                    mod_dashboard_about_ui("dashboard_about")),
            tabItem(
                tabName = "returns",
                tabsetPanel(
                    id = "returns",
                    side = "left",
                    tabPanel(
                        tabName = "Returns",
                        br(),
                        mod_returns_returns_ui("returns", ticker_choices = ticker_choices)
                    ),
                    tabPanel(
                        tabName = "Returns Table",
                        mod_returns_returns_table_ui("returns_table", ticker_choices = ticker_choices)
                    )
                )
            ),
            tabItem(
                tabName = "Backtest",
                tabsetPanel(
                    id = "backtest",
                    side = "left",
                    
                    tabPanel(
                        tabName = "Backtest",
                        br(),
                        mod_backtest_backtest_ui(
                            "backtest",
                            ticker_choices = ticker_choices,
                            monthly_rets = monthly_rets
                        )
                        
                    ),
                    tabPanel(tabName = "Sortino",
                             br(),
                             mod_backtest_sortino_ui("density")),
                    tabPanel(tabName = "Explanation",
                                boxPlus(width = 12,
                                        p(
                                            "Here is the explanation..."
                                        ))),
                    tabPanel(tabName = "Ideas",
                         boxPlus(
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
            ),
            tabItem(
                tabName = "Research",
                tabsetPanel(
                    id = "research",
                    side = "left",
                    tabPanel(tabName = "Models",

                                ),
                    tabPanel(tabName = "Learning",
                                mod_research_learning_ui("learning"))
                )
            )
        )
    )
)


##############
### Server ###
##############

server <- function(input, output, session) {
    mod_returns_returns_server(id = "returns",
                               monthly_rets = monthly_rets)
    
    mod_backtest_backtest_server(id = "backtest",
                                 asset_rets = asset_rets,
                                 cash_rets = cash_rets)
    
    mod_returns_returns_table_server(id = "returns_table",
                                     monthly_rets = monthly_rets)
    
    mod_backtest_sortino_server(id = "density",
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
