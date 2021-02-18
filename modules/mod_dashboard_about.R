

mod_dashboard_about_ui <- function(id, inflation) {
    
    ns <- NS(id)
    

    tagList(
        
        br(),
        br(),
        fluidRow(
            valueBox(
                width = 4,
                value = paste0(inflation %>% pull(level) %>% last() %>% round(1), "%"),
                subtitle = paste0("Inflation \n YE: ", 
                inflation %>% pull(date) %>% last(), " (YEFPCPITOTLZGUSA)"),
                color = "aqua",
                icon = icon("money")#,
                # href = ""
            ),
            
            valueBox(
                width = 4,
                value = yields_monthly %>% slice(n()) %>% filter(symbol == '20 year') %>% pull(yield) - 
                    yields_monthly %>% slice(n()) %>% filter(symbol == '1 year') %>% pull(yield),
                subtitle = "Yield Curve \n (Treasury 20-year less 1-year)",
                color = "blue",
                icon = icon("cogs")
            ),
            valueBox(
                width = 4,
                value = paste0(100*round(monthly_rets %>% arrange(date) %>% select("SPY") %>% pull(SPY) %>% last(), 3), "%"),
                subtitle = paste0("SPY Return \n Month ended: ", monthly_rets %>% arrange(date) %>% select(date, SPY) %>% pull(date) %>% last()),
                color = "green", #navy, fuchsia, purple, maroon, aqua, yellow
                icon = icon("line-chart")
            
            )
        ),
        fluidRow(
        boxPlus(
            id = ns("tabcard"),
            width = 8,
            title = "A card with tabs",
            tabsetPanel(
                id = ns("tabsetpanel"),
                tabPanel(
                    tabName = "Quotes",
                    title = "Quotes",
                    active = TRUE,
                    blockQuote(
                        HTML(
                            "I can calculate the motions of the heavenly bodies, but not the madness of the people. - <b class='quote_author'>Isaac Newton</b>"
                        ),
                        status = "indigo"
                    ),
                    blockQuote(
                        HTML(
                            "Be fearful when others are greedy and greedy when others are fearful. - <b class='quote_author'>Warren Buffett</b>"
                        ),
                        status = "danger"
                    ),
                    blockQuote(
                        HTML(
                            "Only buy something that you'd be perfectly willing to hold if the market shut down for 10 years. - <b class='quote_author'>Warren Buffett</b>"
                        ),
                        status = "teal"
                    ),
                    blockQuote(
                        HTML(
                            "Stop trying to predict the direction of the stock market, the economy, interest rates, or elections. - <b class='quote_author'>Warren Buffett</b>"
                        ),
                        status = "orange"
                    )#,
                    # bs4Quote("Blablabla", status = "warning"),
                    # bs4Quote("Blablabla", status = "fuchsia")
                    
                    
                    
                ),
                tabPanel(
                    tabName = "Tab 2",
                    active = FALSE,
                    title = "Tab 2",
                    "Content 2"
                ),
                tabPanel(
                    tabName = "About",
                    title = "About",
                    active = FALSE,
                    # create manually
                    # create in bs4dash and copy html and css
                    # bs4Dash::bs4UserCard(
                    # fluidRow(
                        # div(
                        div(style="display: flex; align-items: center;",
                            div(
                            div(tags$img(src="https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ", 
                                        alt="Aaron Hardy", 
                                        style="width: 180px; border-radius: 5px;
                                        box-shadow: 3px 5px 10px grey; margin-top: 25px;")
                                ),
                            div(actionButton(inputId = ns("toggleAdvanced"),
                                             label = "Contact",
                                             class="btn",
                                             style="width: 180px;
                                             background-color: #707070; color: white;
                                             border: none; margin: 6px 0 6px 0;
                                             box-shadow: 1px 3px 5px grey; margin-top: 25px;"),
                                shinyjs::hidden(
                                    div(id = ns("advanced"), a("aaronhardy6@gmail.com", href="aaronhardy6@gmail.com"))
                                )
                                )
                            
                         
                            ),
                                    # style="float: right;"),
                            div(style="margin: 15px;",
                                h2("Aaron Hardy"),
                                h4("App Creator"),
                                p("Thank you for using this app. I designed it to make investing more fun and more accessible to serious investors."),
                                p(HTML("If you would like to see more of my work, please visit my <a href = 'https://www.linkedin.com/in/aaron-hardy-651b2410/'>LinkedIn</a> profile.")),
                                p(HTML("I also maintain a blog at <a href = 'https://www.freeanalystnotes.com/'>FreeAnalystNotes.com</a>. and
                    <a href = 'https://www.investwithr.com'>InvestWithR.com</a>"))
                            )
                        )
                )
                        
                        # boxPlus(
                        #     width = 12,
                        #     # src = "https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ",
                        #     status = "info",
                        #     h2("App Creator"),
                        #     h3("Aaron Hardy"),
                        #     # elevation = 2,
                        #     tags$img(src="https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ",
                        #              style="width:100%px; height: 120px;
                        #              border-radius: 5px;
                        #              box-shadow: 3px 5px 10px grey;"),
                        #     p("Thank you for using this app. I designed it to make investing more fun and more accessible to serious investors."),
                        #     p(HTML("If you would like to see more of my work, please visit my <a href = 'https://www.linkedin.com/in/aaron-hardy-651b2410/'>LinkedIn</a> profile.")),
                        #     p(HTML("I also maintain a blog at <a href = 'https://www.freeanalystnotes.com/'>FreeAnalystNotes.com</a>. and
                        #     <a href = 'https://www.investwithr.com'>InvestWithR.com</a>"))
                        # 
                        # )
                    
                )
            
        ),
        
            boxPlus(
                solidHeader = FALSE,
                title = "Status summary",
                background = NULL,
                width = 4,
                status = "danger",
                footer = fluidRow(column(width = 6,
                                         descriptionBlock(
                                             number = "17%",
                                             numberColor = "success", #"green",
                                             numberIcon = "fa fa-caret-up",
                                             header = "$35,210.43",
                                             text = "TOTAL REVENUE",
                                             rightBorder = TRUE,
                                             marginBottom = FALSE
                                         )
                                         ),
                                         column(width = 6,
                                                descriptionBlock(
                                                    number = "18%",
                                                    numberColor = "warning", #"red",
                                                    numberIcon = "fa fa-caret-down",
                                                    header = "1200",
                                                    text = "GOAL COMPLETION",
                                                    rightBorder = FALSE,
                                                    marginBottom = FALSE

                                                )))
            
            )
        )
    )
    
    
}



mod_dashboard_about_server <- function(id) {
    moduleServer(id,
                 function(input, output, session) {
                     
                     shinyjs::onclick("toggleAdvanced",
                                      shinyjs::toggle(id = "advanced", 
                                                      anim = TRUE, animType = "slide", time = 0.5))
                     
                 })
}
                     