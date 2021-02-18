
# shinyWidgetsGallery()

# icons: desktop, gears, paint-brush,  "fa fa-users"

# widgetUserBox(
#   title = "Elizabeth Pierce",
#   subtitle = "Web Designer",
#   type = NULL,
#   width = 12,
#   src = "https://adminlte.io/themes/AdminLTE/dist/img/user3-128x128.jpg",
#   background = TRUE,
#   backgroundUrl = "https://images.pexels.com/photos/531880/pexels-photo-531880.jpeg?auto=compress&cs=tinysrgb&h=350",
#   closable = TRUE,
#   "Some text here!",
#   footer = "The footer here!"
# )


# box(
#   width = NULL,
#   title = "App Buttons",
#   status = NULL,
#   appButton(
#     url = "http://google.com",
#     label = "Users", 
#     icon = "fa fa-users", 
#     enable_badge = TRUE, 
#     badgeColor = "purple", 
#     badgeLabel = 891
#   ),
#   appButton(
#     label = "Edit", 
#     icon = "fa fa-edit", 
#     enable_badge = FALSE, 
#     badgeColor = NULL, 
#     badgeLabel = NULL
#   ),
#   appButton(
#     label = "Likes", 
#     icon = "fa fa-heart-o", 
#     enable_badge = TRUE, 
#     badgeColor = "red", 
#     badgeLabel = 3
#   )
# )


# box(
#   solidHeader = FALSE,
#   title = "Status summary",
#   background = NULL,
#   width = 12,
#   status = "danger",
#   footer = fluidRow(
#     column(
#       width = 6,
#       descriptionBlock(
#         number = "17%", 
#         number_color = "green", 
#         number_icon = "fa fa-caret-up",
#         header = "$35,210.43", 
#         text = "TOTAL REVENUE", 
#         right_border = TRUE,
#         margin_bottom = FALSE
#       )
#     ),
#     column(
#       width = 6,
#       descriptionBlock(
#         number = "18%", 
#         number_color = "red", 
#         number_icon = "fa fa-caret-down",
#         header = "1200", 
#         text = "GOAL COMPLETION", 
#         right_border = FALSE,
#         margin_bottom = FALSE
#       )
#     )
#   )
# )


# boxPlus(
#   width = 12,
#   title = "boxPlus with sidebar", 
#   closable = TRUE, 
#   status = "warning", 
#   solidHeader = FALSE, 
#   collapsible = TRUE,
#   enable_sidebar = TRUE,
#   sidebar_width = 25,
#   sidebar_start_open = TRUE,
#   sidebar_content = tagList(
#     checkboxInput("somevalue", "Some value", FALSE),
#     verbatimTextOutput("value"),
#     sliderInput(
#       "slider_boxsidebar", 
#       "Number of observations:",
#       min = 0, 
#       max = 1000, 
#       value = 500
#     )
#   ),
#   plotOutput("boxSidebarPlot")
# )

# box(
#   title = "Box with a green boxPad",
#   status = "warning",
#   width = NULL,
#   fluidRow(
#     column(width = 6),
#     column(
#       width = 6,
#       boxPad(
#         color = "green",
#         descriptionBlock(
#           header = "8390", 
#           text = "VISITS", 
#           right_border = FALSE,
#           margin_bottom = TRUE
#         ),
#         descriptionBlock(
#           header = "30%", 
#           text = "REFERRALS", 
#           right_border = FALSE,
#           margin_bottom = TRUE
#         ),
#         descriptionBlock(
#           header = "70%", 
#           text = "ORGANIC", 
#           right_border = FALSE,
#           margin_bottom = FALSE
#         )
#       )
#     )
#   )
# )

# box(
#   title = "Inputs", background = "black",
#   "Box content here", br(), "More box content",
#   sliderInput("slider", "Slider input:", 1, 100, 50),
#   textInput("text", "Text input:")
# )

# ?validStatuses
# ?validColors

# dropdownMenu(type = "tasks", badgeStatus = "success",
#              taskItem(value = 90, color = "green",
#                       "Documentation"
#              ),
#              taskItem(value = 17, color = "aqua",
#                       "Project X"
#              ),
#              taskItem(value = 75, color = "yellow",
#                       "Server deployment"
#              ),
#              taskItem(value = 80, color = "red",
#                       "Overall project"
#              )
# )



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
library(shinydashboard)
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
# library(highcharter)
library(ggthemes)
library(htmlwidgets) # 0.4 MB
library(ggrepel) # 0.1 MB
library(magrittr)
# library(shinycssloaders)
# library(sever) # Disconnection message

# Source all modules
lapply(
    list.files("modules", pattern = ".R", full.names = TRUE),
    FUN = function(x)
        source(x)
)

# Load data
source("data.R")


# Convert Sass to CSS
sass(sass_file("www/custom.scss"),
     output = "www/custom.css",
     cache = FALSE)


###########
### UI ####
###########

ui <- dashboardPagePlus(
    useShinyjs(),
    skin = "red-light", #c("deeppink2", "mistyrose3", "steelblue1")red-light"), #"midnight", "black-light"
    
    # sidebar_background = NULL,
    
    
    header = dashboardHeaderPlus(
      title = "Stock Analysis",
      enable_rightsidebar = TRUE,
      rightSidebarIcon = "gears",
      fixed = TRUE,
      left_menu = tagList(
        dropdownBlock(
          id = "mydropdown",
          title = "Dropdown 1",
          icon = "sliders",
          screenshotButton(selector = ".content", label = "Take a Screenshot"),
          sliderInput(
            inputId = "n",
            label = "Number of observations",
            min = 10, max = 100, value = 30
          ),
          prettyToggle(
            inputId = "na",
            label_on = "NAs kept",
            label_off = "NAs removed",
            icon_on = icon("check"),
            icon_off = icon("remove")
          )
        )
        )
      ),
    
    # navbar = bs4Dash::bs4DashNavbar(
        # skin = "light",
        # status = "white",
        # sidebarIcon = "bars",
        # leftUi = actionButton("goButton1", "Go1"),
        # rightUi = ,
        # fixed = TRUE,
        # compact = TRUE,
        # p("Tools for investing!", class = "navbar-text")
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
                "Notes",
                icon = icon("pen"),
                tabName = "notes",
                badgeLabel = "new",
                badgeColor = "green"
            ),
            menuItem(
                "Charts",
                icon = icon("bar-chart-o"),
                menuSubItem("Economy", tabName = "economy"),
                menuSubItem("Securities", tabName = "securities")
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
    
  
    rightsidebar = rightSidebar(
      # background = "dark",
      rightSidebarTabContent(
        id = 1,
        title = "Tab 1",
        icon = "desktop",
        active = TRUE,
        sliderInput(
          "obs",
          "Number of observations:",
          min = 0, max = 1000, value = 500
        )
      ),
      rightSidebarTabContent(
        id = 2,
        title = "Tab 2",
        textInput("caption", "Caption", "Data Summary")
      ),
      rightSidebarTabContent(
        id = 3,
        icon = "paint-brush",
        title = "Tab 3",
        numericInput("obs", "Observations:", 10, min = 1, max = 100)
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
      includeCSS("www/custom.css"),
      
        # setShadow(class = "box"),
        tabItems(
            tabItem(tabName = "dashboard",
                    mod_dashboard_about_ui("dashboard_about", 
                                           inflation = inflation)
                    ),
            tabItem(
                tabName = "returns",
                
                br(),
                br(),
                tabsetPanel(
                    id = "Returns",
                    tabPanel(
                        title = "returns",
                        br(),
                        mod_returns_returns_ui("returns", ticker_choices = ticker_choices)
                    ),
                    tabPanel(
                        title = "Returns Table",
                        mod_returns_returns_table_ui("returns_table", ticker_choices = ticker_choices)
                    )
                )
            ),
            tabItem(
                tabName = "Backtest",
                tabsetPanel(
                    id = "backtest",
                    tabPanel(
                        title = "Backtest",
                        br(),
                        mod_backtest_backtest_ui(
                            "backtest",
                            ticker_choices = ticker_choices,
                            monthly_rets = monthly_rets
                        )
                        
                    ),
                    tabPanel(
                      title = "Sortino",
                             br(),
                             br(),
                             br(),
                             mod_backtest_sortino_ui("density")),
                    tabPanel(
                      title = "Explanation",
                      br(),
                      gradientBox(width = 12,
                                        title = "gradient box",
                                            # status = "warning",
                                        # icon = "fa fa-heart",
                                        collapsible = TRUE,
                                        gradientColor = "green", 
                                        p(
                                            "Here is the explanation..."
                                        ),
                                        footer = dateInput("ids", "date"))),
                    tabPanel(
                      title = "Ideas",
                      boxPlus(
                           title = "trading idea",  
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
                             ),
                             footer = "footer",
                             collapsible = TRUE,
                             solidHeader = FALSE,

                             enable_label = TRUE,
                             label_text = "label text",
                             label_status = "primary",
                             enable_dropdown = TRUE,
                             dropdown_icon = "wrench",
                             dropdown_menu = NULL,
                             enable_sidebar = TRUE,
                             sidebar_content = NULL,
                             sidebar_title = "sidebar title",
                             sidebar_width = 25,
                             sidebar_background = "#222d32",

                             sidebar_icon = "cogs"
                         ))
                    
                )
            ),
            tabItem(
              tabName = "notes",
              mod_notes_ui("notes")
            ),
            
            
            tabItem(
              tabName = "economy",
              mod_charts_economy_ui("economy",
                                    dates_gdp = dates_gdp, 
                                    dates_yields = dates_yields, 
                                    dates_inflation = dates_inflation, 
                                    dates_bond_yields = dates_bond_yields)
            ),
            tabItem(
              tabName = "securities",
              mod_charts_securities_ui(id = "securities",
                                       industry_choices = industry_choices,
                                       field_choices = field_choices)
              
            ),
            tabItem(
                tabName = "Research",
                tabsetPanel(
                    id = "research",
                    # side = "left",
                    tabPanel(
                      # title = "Models",
                      title = "Models"

                                ),
                    tabPanel(
                      # tabName = "Learning",
                      title = "Learning",
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
    mod_charts_economy_server(id = "economy",
                              yields_monthly = yields_monthly, 
                              gdp = gdp, 
                              inflation = inflation, 
                              bond_yields = bond_yields)
    mod_dashboard_about_server(id = "dashboard_about")
    mod_charts_securities_server(id = "securities",
                                 fundamentals_data = fundamentals_data)
    
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
