source('global.R')
ui <- fluidPage(
    useShinyjs(),
    theme = "bootstrap.css",

    navbarPage("W O L F",
               tabPanel("Home"),
               tabPanel("Google AdWords",
                        actionButton("adwordsFullScreenToggle", "", icon=icon("compress")),
                        sidebarLayout(
                            sidebarPanel(id="adwordsSidebar",
                                         textInput("adwordsAccountId", "Account ID",
                                                   placeholder = "XXX-XXX-XXXX",
                                                   width = "35%"),
                                         dateRangeInput("adwordsDateRange","Date Range"),
                                         actionButton("getAdWordsData", "Get Data")

                                         ),
                            mainPanel(
                                img(src="exampleimg.jpg")
                            )
                        )),
               tabPanel("Google Analytics",
                        actionButton("gaFullScreenToggle", "", icon=icon("compress")),
                        sidebarLayout(
                            sidebarPanel(id="gaSidebar",
                                         selectInput("gaViewId", "Choose GA View",
                                                     choices = c("Client 1","Client 2",
                                                                 "Client 3","Client 4")),
                                         dateRangeInput("adwordsDateRange","Date Range"),
                                         actionButton("getAdWordsData", "Get Data")

                                         ),
                            mainPanel(
                                img(src="exampleimg.jpg")
                            )
                        )),
               tabPanel("Facebook Ads"),
               tabPanel("Twitter Ads"),
               tabPanel("LinkedIn Ads"),
               tabPanel("Moz")
               )

)
