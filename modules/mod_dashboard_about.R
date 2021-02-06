

mod_dashboard_about_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        
        fluidRow(
            boxPlus(
                id = ns("tabcard"),
                width = 8,
                title = "A card with tabs",
                tabPanel(tabName = "Quotes",
                            active = TRUE,
                            blockQuote(HTML("I can calculate the motions of the heavenly bodies, but not the madness of the people. - <b class='quote_author'>Isaac Newton</b>"), status = "indigo"),
                         blockQuote(HTML("Be fearful when others are greedy and greedy when others are fearful. - <b class='quote_author'>Warren Buffett</b>"), status = "danger"),
                         blockQuote(HTML("Only buy something that you'd be perfectly willing to hold if the market shut down for 10 years. - <b class='quote_author'>Warren Buffett</b>"), status = "teal"),
                         blockQuote(HTML("Stop trying to predict the direction of the stock market, the economy, interest rates, or elections. - <b class='quote_author'>Warren Buffett</b>"), status = "orange")#,
                            # bs4Quote("Blablabla", status = "warning"),
                            # bs4Quote("Blablabla", status = "fuchsia")
                            
                            
                            
                            ),
                tabPanel(tabName = "Tab 2",
                            active = FALSE,
                            "Content 2"),
                tabPanel(tabName = "Tab 3",
                            active = FALSE,
                            "Content 3")
            )#,
            
            # bs4UserCard(
            #     width = 4,
            #     src = "https://media-exp1.licdn.com/dms/image/C4E03AQG4F7-HObv3PA/profile-displayphoto-shrink_400_400/0/1611816873957?e=1617235200&v=beta&t=dsZLZ_3ID9EOu5NFE3yU8vpmZkxnkdrfllN7uO31guQ",
            #     status = "info",
            #     title = "App Creator",
            #     subtitle = "Aaron Hardy",
            #     elevation = 2,
            #     p("Thank you for using this app. I designed it to make investing more fun and more accessible to serious investors."),
            #     p(HTML("If you would like to see more of my work, please visit my <a href = 'https://www.linkedin.com/in/aaron-hardy-651b2410/'>LinkedIn</a> profile.")),
            #     p(HTML("I also maintain a blog at <a href = 'https://www.freeanalystnotes.com/'>FreeAnalystNotes.com</a>. and 
            #     <a href = 'https://www.investwithr.com'>InvestWithR.com</a>"))
            #     
            # )
        ), 
        
        
        boxPlus(
            solidHeader = FALSE,
            title = "Status summary",
            background = NULL,
            width = 4,
            status = "danger",
            footer = fluidRow(
                column(
                    width = 6#,
                    # descriptionBlock(
                    #     number = "17%",
                    #     number_color = "success",
                    #     number_icon = "fa fa-caret-up",
                    #     header = "$35,210.43",
                    #     text = "TOTAL REVENUE",
                    #     right_border = TRUE,
                    #     margin_bottom = FALSE
                    # )
                ),
                column(
                    width = 6#,
                    # descriptionBlock(
                    #     number = "18%",
                    #     number_color = "danger",
                    #     number_icon = "fa fa-caret-down",
                    #     header = "1200",
                    #     text = "GOAL COMPLETION",
                    #     right_border = FALSE,
                    #     margin_bottom = FALSE
                    # )
                )
            )
        ),
        fluidRow(
            valueBox(
                value = 150,
                subtitle = "New orders",
                # status = "primary",
                icon = icon("shopping-cart"),
                href = "#"
            ),
            valueBox(
                value = "53%",
                subtitle = "New orders",
                # status = "danger",
                icon = icon("cogs")#,
                # footer = div("Hello World")
            ),
            valueBox(
                value = "44",
                subtitle = "User Registrations",
                # status = "warning",
                icon = icon("sliders")
            )
        )
    )
    
    
}