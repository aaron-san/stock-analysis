

mod_research_learning_ui <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        tabBox(
            id = ns("tabcard"),
            width = 8,
            title = tags$small("Study!"),
                tabPanel(
                    tabName = "Quotes",
                    active = TRUE,
                    strong("Quotes"),
                    blockQuote(HTML("I can calculate the motions of the heavenly bodies, but not the madness of the people. - <b class='quote_author'>Isaac Newton</b>"), status = "indigo"),
                    blockQuote(HTML("Be fearful when others are greedy and greedy when others are fearful. - <b class='quote_author'>Warren Buffett</b>"), status = "danger"),
                    blockQuote(HTML("Only buy something that you'd be perfectly willing to hold if the market shut down for 10 years. - <b class='quote_author'>Warren Buffett</b>"), status = "teal"),
                    blockQuote(HTML("Stop trying to predict the direction of the stock market, the economy, interest rates, or elections. - <b class='quote_author'>Warren Buffett</b>"), status = "orange")
            ),
            tabPanel(tabName = "Data Preprocessing",
                        active = FALSE,
                        
                        strong("Data preprocessing"),
                        p(HTML('<ul>'),
                          HTML('<li>'), "Range - Normalize values so they range between 0 and 1", HTML('</li>'),
                          HTML('<li>'), "Center - Subtract the mean", HTML('</li>'),
                          HTML('<li>'), "Scale - Divide by the standard deviation", HTML('</li>'), 
                          HTML('<li>'), "BoxCox - Remove skewness leading to normality. Values must be > 0", HTML('</li>'),
                          HTML('<li>'), "YeoJohnson - Like BoxCox, but works for negative values", HTML('</li>'),
                          HTML('<li>'), "expoTrans - Exponential transformation, works for negative values", HTML('</li>'),
                          HTML('<li>'), "pca - Replace with principal components", HTML('</li>'),
                          HTML('<li>'), "ica - Replace with independent components", HTML('</li>')
                        )
                        ),
            tabPanel(tabName = "Tab 3",
                     active = FALSE,
                     strong("Content 3")
                     )
        )
    )
}