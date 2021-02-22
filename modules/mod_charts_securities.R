
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
                ),
                boxPlus(width = 8,
                        # selectInput(ns("long_business_name_input"),
                        #             "Select ticker:",
                        #             choices = )
                        tableOutput(ns("long_business_summary"))
                        )
            )
            
        )
    }


mod_charts_securities_server <-
    function(id, is_1, is_2, is_3, is_4, bs_1, bs_2, bs_3, bs_4,
             cf_1, cf_2, cf_3, cf_4) {
        moduleServer(id,
                     function(input, output, session) {
     
                         # Filter by industry and join data
                         industry <- reactive({input$industry})
                         fundamentals_data <- reactive({
                             bs_1 <- bs_1 %>% filter(industry == industry()) 
                             bs_2 <- bs_2 %>% filter(industry == industry())
                             bs_3 <- bs_3 %>% filter(industry == industry())
                             bs_4 <- bs_4 %>% filter(industry == industry())
                             is_1 <- is_1 %>% filter(industry == industry())
                             is_2 <- is_2 %>% filter(industry == industry())
                             is_3 <- is_3 %>% filter(industry == industry())
                             is_4 <- is_4 %>% filter(industry == industry())
                             cf_1 <- cf_1 %>% filter(industry == industry())
                             cf_2 <- cf_2 %>% filter(industry == industry())
                             cf_3 <- cf_3 %>% filter(industry == industry())
                             cf_4 <- cf_4 %>% filter(industry == industry())
                             
                             joined_data <- bs_1 %>% 
                                 full_join(bs_2) %>% 
                                 full_join(bs_3) %>% 
                                 full_join(bs_4) %>%
                                 full_join(is_1) %>% 
                                 full_join(is_2) %>% 
                                 full_join(is_3) %>%
                                 full_join(is_4) %>% 
                                 full_join(cf_1) %>% 
                                 full_join(cf_2) %>%
                                 full_join(cf_3) %>% 
                                 full_join(cf_4)
                             
                             pivoted_data <-
                                 joined_data %>% 
                                 pivot_longer(-c(ticker, date, industry, form, short_name, long_business_summary), 
                                              names_to = "field", values_to = "value")
                             return(pivoted_data)
                         })
                         
                         output$long_business_summary <- renderTable({
                             fundamentals_data() %>% 
                                 select(ticker, long_business_summary)
                         })
                         
                         output$roe <- renderPlotly({
                         
                             funds_plot <- 
                                 fundamentals_data() %>%
                                filter(field == input$field) %>%
                                # filter(field == "intangible_assets") %>%
                                 # mutate(ticker = fct_reorder(ticker, value)) %>%
                                 ggplot(aes(x = date, y = value, fill = ticker)) +
                                 # geom_col(position = if_else(input$field == "market_share", "stack", "dodge")) +
                                 geom_point(aes(size = value, color = ticker)) +
                                 scale_y_continuous(label = scales::dollar_format(scale = 1e-6, unit = "M")) + 
                                 # scale_y_log10()
                                labs(title = snakecase::to_title_case(as.character(input$field)),
                                    subtitle = "",
                                    x = "", y = "in millions") +
                             theme_minimal() +
                                 theme(axis.title.y = element_text(color = "grey"))
                             
                             ggplotly(funds_plot) %>% config(displayModeBar = FALSE)
                         })
                         
                         output$names <- renderTable(
                             fundamentals_data() %>% 
                                 distinct(ticker, short_name) %>% 
                                 mutate(short_name = snakecase::to_title_case(short_name))
                             # options = list(pageLength = 3)
                         )
                         
                         
                                             
                     }
        )
    }