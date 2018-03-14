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
                                       sidebarPanel(
                                         actionButton("plotAdScheduling", "Plot Data"),
                                         downloadButton("downloadAdScheduling", "Save Plot")),
                                       mainPanel(
                                           tabsetPanel(id = "adSchedulingTabs",
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
                                               tabPanel("Unigrams", plotOutput("unigrams")),
                                               tabPanel("Bigrams", plotOutput("bigrams")),
                                               tabPanel("Trigrams", plotOutput("trigrams"))
                                           ))
                                   )
                                   ),
                          tabPanel("Impression Share",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotImpressionShare", "Plot Data")),
                                       mainPanel(
                                           plotOutput("isPlot"),
                                           plotOutput("isOtherPlot"))
                                   )
                                   ),
                          tabPanel("Device Performance",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotDevicePerformance", "Plot Data")),
                                       mainPanel(plotOutput("devicePlot"))
                                   )
                                   ),
                          tabPanel("Quality Score",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotQs", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Ad Relevance", tableOutput("adRelevance")),
                                               tabPanel("LP Experience", tableOutput("lpExperience")),
                                               tabPanel("Expected CTR", tableOutput("expectedCtr")),
                                               tabPanel("Quality Score", tableOutput("qualityScore")),
                                               tabPanel("Weighted QS", plotlyOutput("weightedQs"))
                                           ))
                                   )
                                   ),
                          tabPanel("Match Types",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotMatchTypes", "Plot Data")),
                                       mainPanel(plotOutput("matchTypesPlot"))
                                   )
                                   ),
                          tabPanel("Ad-Keyword Combinations",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotAdKeywords", "Plot Data")),
                                       mainPanel(plotlyOutput("adKeywordsPlot"))
                                   )
                                   ),
                          tabPanel("Performance Segments",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotPerformanceSegments", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Converters", plotlyOutput("convertersPlot")),
                                               tabPanel("Non-Converters", plotlyOutput("nonConvertersPlot")),
                                               tabPanel("Clickless", plotlyOutput("clicklessPlot"))
                                           ))
                                   )
                                   ),
                          tabPanel("Anomaly Detection",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotAnomalyDetection", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Clicks", plotOutput("clickAnomalies")),
                                               tabPanel("Conversions", plotOutput("conversionAnomalies")),
                                               tabPanel("CPA", plotOutput("cpaAnomalies")),
                                               tabPanel("CPC", plotOutput("cpcAnomalies")),
                                               tabPanel("IS", plotOutput("isAnomalies")),
                                               tabPanel("Position", plotOutput("positionAnomalies"))
                                           ))
                                   )
                                   ),
                          tabPanel("Treemaps",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotTreemaps", "Plot Data")),
                                       mainPanel(plotOutput("treemapsPlot"))
                                   )
                                   ),
                          tabPanel("Audiences",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotAudiencePerformance", "Plot Data")),
                                       mainPanel(tableOutput("audiencePerformanceTable"))
                                   )
                                   ),
                          tabPanel("Device-Schedule",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotDeviceSchedulePerformance", "Plot Data")),
                                       mainPanel(plotOutput("deviceSchedulePerformancePlot"))
                                   )
                                   )),
               navbarMenu("Google Analytics",
                          tabPanel("Performance Overview",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotPerformanceOverview", "Plot Data")),
                                       mainPanel(plotOutput("performanceOverviewPlot"))
                                   )
                                   ),
                          tabPanel("New vs Returning",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotNewVsReturning", "Plot Data")),
                                       mainPanel(plotOutput("newVsReturningPlot"))
                                   )
                                   ),
                          tabPanel("Hostnames",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotHostnames", "Plot Data")),
                                       mainPanel(tableOutput("hostnamesPlot"))
                                   )
                                   ),
                          tabPanel("Device Categories",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotGaDeviceCategory", "Plot Data")),
                                       mainPanel(plotOutput("gaDeviceCategoryPlot"))
                                   )
                                   ),
                          tabPanel("Channel Performance",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotChannelPerformance", "Plot Data")),
                                       mainPanel(plotlyOutput("channelPerformancePlot"))
                                   )
                                   ),
                          tabPanel("Pages Performance",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotPagePerformance", "Plot Data")),
                                       mainPanel(plotlyOutput("pagePerformancePlot"))
                                   )
                                   ),
                          tabPanel("Landing Pages Performance",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotLpPerformance", "Plot Data")),
                                       mainPanel(plotlyOutput("lpPerformancePlot"))
                                   )
                                   ),
                          tabPanel("e-Commerce Performance",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotEcomPerformance", "Plot Data")),
                                       mainPanel(plotOutput("ecomPerformancePlot"))
                                   )
                                   ),
                          tabPanel("Session Count",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotSessionCount", "Plot Data")),
                                       mainPanel(plotOutput("sessionCountPlot"))
                                   )
                                   ),
                          tabPanel("Days Since Last Session",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotDaysSinceLastSession", "Plot Data")),
                                       mainPanel(plotOutput("daysSinceLastSessionPlot"))
                                   )
                                   ),
                          tabPanel("Session Duration",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotSessionDuration", "Plot Data")),
                                       mainPanel(plotOutput("sessionDurationPlot"))
                                   )
                                   ),
                          tabPanel("Page Depth",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotPageDepth", "Plot Data")),
                                       mainPanel(plotOutput("pageDepthPlot"))
                                   )
                                   ),
                          tabPanel("Correlation Plot",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotGaCorplot", "Plot Data")),
                                       mainPanel(plotOutput("gaCorPlot"))
                                   )
                                   ),
                          tabPanel("City Performance",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotCityPerformance", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Pageviews", plotOutput("pageviewsGgMap")),
                                               tabPanel("Sessions", plotOutput("sessionsGgMap")),
                                               tabPanel("Bounce Rate", plotOutput("bounceRateGgMap")),
                                               tabPanel("Pages/Session", plotOutput("pagesPerSessionGgMap")),
                                               tabPanel("Revenue", plotOutput("revenueGgMap")),
                                               tabPanel("Conversion Rate", plotOutput("crGgMap"))
                                           ))
                                   )
                                   ),
                          tabPanel("Events",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotEventPerformance", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Total Events", plotOutput("totalEventsPlot")),
                                               tabPanel("Sessions With Events", plotOutput("sessionsWithEventsPlot"))
                                           ))
                                   )
                                   ),
                          tabPanel("Goal Performance",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotGoalPerformance", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Goal Completions", plotOutput("goalCompletionsPlot")),
                                               tabPanel("Conversion Rates", plotOutput("goalConversionRatesPlot"))
                                           ))
                                   )
                                   ),
                          tabPanel("Product Performance",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotProductPerformance", "Plot Data")),
                                       mainPanel(plotlyOutput("productPerformancePlot"))
                                   )
                                   ),
                          tabPanel("Anomaly Detector",
                                   sidebarPanel(actionButton("plotGaAnomalies", "Plot Data")),
                                   mainPanel(
                                       tabsetPanel(
                                           tabPanel("Acquisition",
                                                        tabsetPanel(
                                                            tabPanel("Sessions", plotOutput("sessionAnomalies")),
                                                            tabPanel("Users", plotOutput("userAnomalies")),
                                                            tabPanel("Pageviews", plotOutput("pageviewAnomalies"))
                                                        )),
                                           tabPanel("Behaviour",
                                                        tabsetPanel(
                                                            tabPanel("Bounce Rate", plotOutput("bounceRateAnomalies")),
                                                            tabPanel("Avg Session Duration", plotOutput("avgSessionDurationAnomalies")),
                                                            tabPanel("Pages/Session", plotOutput("pagesPerSessionAnomalies"))
                                                        )),
                                           tabPanel("Conversions",
                                                    tabsetPanel(
                                                        tabPanel("Transactions", plotOutput("transactionAnomalies")),
                                                        tabPanel("Revenue", plotOutput("revenueAnomalies")),
                                                        tabPanel("Conversion Rate", plotOutput("ecomConversionRateAnomalies")),
                                                        tabPanel("AOV", plotOutput("avgOrderValueAnomalies"))
                                                    ))
                                           ))
                                   ),
                          tabPanel("Search Terms",
                                   sidebarLayout(
                                       sidebarPanel(actionButton("plotSearchTerms", "Plot Data")),
                                       mainPanel(
                                           tabsetPanel(
                                               tabPanel("Unique Searches",
                                                        plotOutput("uniqueSearches"),
                                                        plotOutput("uniqueSearchesRefinement")),
                                               tabPanel("Avg Search Depth",
                                                        plotOutput("avgSearchDepth"),
                                                        plotOutput("avgSearchDepthRefinement")),
                                               tabPanel("Avg Time After Search",
                                                        plotOutput("avgSearchDuration"),
                                                        plotOutput("avgSearchDurationRefinement"))
                                           ))
                                   )
                                   )
                          ))
)
