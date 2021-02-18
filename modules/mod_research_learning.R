

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
            tabPanel(tabName = "SPACs",
                     active = FALSE,
                     strong("SPACs"),
                     fluidRow(withTags(
                         ul(
                             li("What is a SPAC?"),
                             li("SPAC is an acronym for a Special-Purpose Acquisition Company."), 
                             li("You can think of a SPAC as an empty 'shell company' without a business of its own, that exists only to:"),
                                ol(
                                    li("Raise money through an Initial Public Offering (IPO)"),
                                    li("Find a private, target company"),
                                    li("Acquire or merge with the target company to provide them a means to enter the public equity market without the target company going through a traditional IPO themselves")
                                 )
                         )
                         )
                         ),
                     
                     div("Most Special-Purpose Acquisition Companies IPO with common shares offered at $10 a share. Once the deal is completed with the target company and the acquisition is successful, SPAC shareholders become shareholders of the acquired target company."),
                     span(),    
                     div("This provides a golden opportunity for retail investors, as this early opportunity is almost always reserved exclusively for larger, private investors and institutions. For comparison, recent traditional IPOs for Snowflake and Airbnb were priced at $120 and $68 respectively, but began trading at $245 and $146 respectively for the mass market."),
                     span(),    
                     div("While the opportunity to be an early investor is a benefit, it also comes with risks for the investors. The target company is unknown, leaving investors at the mercy of the SPAC managers and their ability to find and successfully negotiate with a favorable target company."),
                     div("Time is a factor to consider as well, as most SPACs have a 2-year window to complete its acquisition before being forced to dissolve and return the funds back to its shareholders, or voting to extend the deadline to complete an acquisition."),
                     span(),    
                     div("For the target companies, SPACs provide a means to access public equity markets without having to go through an expensive and tedious IPO process. They are also guaranteed the full fund raised by SPAC investors-hundreds of millions, if not billions of dollars - upon the merger, thus eliminating the concern of being unable to raise money from a lackluster IPO."),
                     span(),    
                     div("I analyzed 71 SPACs and their target companies that completed mergers between January 2020 and January 2021. The list was sourced from spactrack.net, and daily closing prices for each company as well as the market indices used in my comparative analysis were available through Yahoo Finance."),
                     span(),
                     div("The IPO date of the SPACs date range between June 2017 and June 2020."),
                     span(),
                     div("Historically, SPACs had a stigma as a scam 'backdoor IPO.' However, notable companies such as DraftKings and Virgin Galactic have used it as a means to go public in 2020. In analyzing the enterprise valuation of target companies, the average valuation was $2.6 billion dollars, with MultiPlan Corporation receiving the highest valuation at $11 billion. The data disproves the notion that only small companies worthy of investor skepticism need SPAC acquisitions, but that larger, reputable companies are using SPACs to go public as well."),
                     span(),
                     div("SPACs can vary in size, to match the vastly different valuations of their target companies"),
                     span(),
                     div("During its pre-merger stage, SPACs have limited risk to fall below its initial IPO price of $10"),
                     span(),
                     div("Time is a major factor to consider - SPAC companies can take an extended amount of time to complete the merger, sometimes without much news. Investors will need to consider the possibility of their share prices remaining stagnant in these instances for months, possibly even years if merger deadlines are extended"),
                     span(),
                     div("While many companies maintain a post-merger price around its IPO price,  price action can vary drastically depending on the target company and market reaction. Investors may expect high volatility post-merger, resulting in very significant gains or losses in a matter of weeks, if not months)")
                     
                     
                ),
            tabPanel(tabName = "Shiny notes",
                     active = FALSE,
                     strong("Shiny"),
                     fluidRow(withTags(
                         ul(
                             li("reactive({}) %>% bindCache(input$city)"),
                             li(""), 
                             li(""),
                             ol(
                                 li("")
                             )
                         )
                     ))
            )
        )
    )
}