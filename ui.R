source('global.R')
ui <- fluidPage(
    useShinyjs(),
    theme = "bootstrap.css",

    navbarPage("W O L F",
               tabPanel("Settings",
                        googleAuthUI("gaLogin"),
                        uiOutput("globalSettings")
                        ),
               navbarMenu("Google AdWords",
                          tabPanel("Scatterplot",
                                   actionButton("adwordsFullScreenToggle", "", icon=icon("compress")),
                                   sidebarLayout(
                                       sidebarPanel(id="adwordsSidebar",
                                                    textInput("adwordsAccountId", "Account ID",
                                                              placeholder = "XXX-XXX-XXXX",
                                                              width = "35%"),
                                                    dateRangeInput("adwordsDateRange","Date Range"),
                                                    selectInput("gaAnalysis", "Choose Analysis",
                                                                choices=c("Scatterplot", "Corplot")),
                                                    actionButton("getAdWordsData", "Plot Data")
                                                    ),
                                       mainPanel(
                                           img(src="exampleimg.jpg")
                                       )
                                   )
                                   )),
               tabPanel("Google Analytics",
                        actionButton("gaFullScreenToggle", "", icon=icon("compress")),
                        sidebarLayout(
                            sidebarPanel(id="gaSidebar",
                                         dateRangeInput("adwordsDateRange","Date Range"),
                                         actionButton("getAdWordsData", "Get Data")
                                         ),
                            mainPanel(
                                img(src="exampleimg.jpg")
                            )
                        )
                        )
               )
)
