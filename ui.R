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
                          tabPanel("Campaign Performance",
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
                                   ),
                          tabPanel("Correlation Plot"),
                          tabPanel("Ad Scheduling"),
                          tabPanel("N-Grams"),
                          tabPanel("Impression Share"),
                          tabPanel("Pacing vs Targets"),
                          tabPanel("Device Performance"),
                          tabPanel("Quality Score"),
                          tabPanel("Match Types"),
                          tabPanel("Ad-Keyword Combinations"),
                          tabPanel("Performance Segments"),
                          tabPanel("Anomaly Detection"),
                          tabPanel("Google Trends"),
                          tabPanel("Treemaps"),
                          tabPanel("Geoperformance"),
                          tabPanel("")),
               navbarMenu("Google Analytics",
                          tabPanel("Yesterday's Overview",
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
                                   ),
                          tabPanel("New vs Returning"),
                          tabPanel("Hostnames"),
                          tabPanel("Device Categories"),
                          tabPanel("Channel Performance"),
                          tabPanel("Pages Performance"),
                          tabPanel("Landing Pages Performance"),
                          tabPanel("e-Commerce Performance"),
                          tabPanel("Session Count"),
                          tabPanel("Days Since Last Session"),
                          tabPanel("Session Duration"),
                          tabPanel("Page Depth"),
                          tabPanel("Correlation Plot"),
                          tabPanel("City Performance"),
                          tabPanel("Events"),
                          tabPanel("Goal Performance"),
                          tabPanel("Product Performance"),
                          tabPanel("Search Terms")
               ))
)
