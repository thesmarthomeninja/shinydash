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
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotCampaignPerformance", "Plot Data")),
                                       mainPanel(plotlyOutput("campaignPerformancePlot"))
                                   )
                                   ),
                          tabPanel("Correlation Plot",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotCorrelationMatrix", "Plot Data")),
                                       mainPanel(plotOutput("corrplot"))
                                   )
                                   ),
                          tabPanel("Ad Scheduling",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotAdScheduling", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Impressions", plotOutput("impressionHeatMap")),
                                               tabPanel("Impression Share", plotOutput("isHeatMap")),
                                               tabPanel("Conversions", plotOutput("conversionHeatMap")),
                                               tabPanel("CPA", plotOutput("cpaHeatMap"))
                                           ))
                                   )
                                   ),
                          tabPanel("N-Grams",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotnGrams", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Unigrams", tableOutput("unigrams")),
                                               tabPanel("Bigrams", tableOutput("bigrams")),
                                               tabPanel("Trigrams", tableOutput("trigrams"))
                                           ))
                                   )
                                   ),
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
