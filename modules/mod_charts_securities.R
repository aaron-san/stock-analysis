
mod_charts_securities_ui <-
    function(id, industry_choices, field_choices) {
        ns <- NS(id)
        
        tagList(
            
            br(),
            br(),
            fluidRow(
                boxPlus(width = 7,
                    plotlyOutput(ns("roe"),
                               height = "300px"),
                               footer = inputPanel(selectInput(ns("industry"), 
                                                    "Industry:",
                                                    choices = industry_choices,
                                                    selected = industry_choices[1]),
                                                selectInput(ns("field"), 
                                                            "Field:",
                                                            choices = field_choices,
                                                            selected = field_choices[1]))
                    ),
                boxPlus(width = 5,
                    tableOutput(ns("names"))
                )
            )
            
        )
    }


mod_charts_securities_server <-
    function(id, fundamentals_data) {
        moduleServer(id,
                     function(input, output, session) {
     
                         output$roe <- renderPlotly({
                         
                             funds_plot <- fundamentals_data %>%
                                drop_na(value) %>%
                                filter(industry == input$industry) %>%
                                 # filter(industry == "Retail - Apparel & Specialty") %>%
                                filter(field == input$field) %>%
                                # filter(field == "cash") %>%
                                 # mutate(ticker = fct_reorder(ticker, value)) %>%
                                 ggplot(aes(x = date, y = value, fill = ticker)) +
                                geom_col(position = if_else(input$field == "market_share", "stack", "dodge")) +
                                 scale_y_continuous(label = scales::dollar_format(scale = 1e-6, unit = "M")) + 
                                 # scale_y_log10()
                                labs(title = snakecase::to_title_case(as.character(input$field)),
                                    subtitle = "",
                                    x = "", y = "in millions") +
                             theme_minimal() +
                                 theme(axis.title.y = element_text(color = "grey"))
                             
                             ggplotly(funds_plot) %>% config(displayModeBar = FALSE)
                         })
                         
                         output$names <- renderTable({
                             fundamentals_data %>% 
                                 filter(industry == input$industry) %>% 
                                 # select(ticker, company_name) %>% 
                                 distinct(ticker, company_name) %>% 
                                 mutate(company_name = snakecase::to_title_case(company_name))
                         })
                         
                         
                                             
                     }
        )
    }