source('global.R')
ui <- fluidPage(
  useShinyjs(),
  includeCSS("www/custom.css"),
  
  navbarPage(title = div(img(src="wg-logo.png", height = "50"), id = "logo"), windowTitle = "WG Analysis Tool",theme = shinytheme("flatly"),
             tabPanel("Settings",
                      sidebarLayout(
                        sidebarPanel(
                          googleAuthUI("gaLogin"),
                          uiOutput("globalSettings")),
                        mainPanel()
                      )
             ),
             navbarMenu("Google AdWords",
                        tabPanel("Campaign Performance",
                                 sidebarLayout(
                                   sidebarPanel(width = 3, 
                                                actionButton("plotCampaignPerformance", "Plot Data")),
                                   mainPanel(
                                     plotOutput("campaignPerformancePlot", brush = "cpBrush"),
                                     DTOutput("cpSelectedPoints"))
                                 )
                        ),
                        tabPanel("Correlation Plot",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotCorrelationMatrix", "Plot Data")),
                                   mainPanel(plotOutput("corrplot"))
                                 )
                        ),
                        tabPanel("Ad Scheduling",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
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
                        tabPanel("Search Term Performance Lookup",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                textInput("lookupTerms", "Enter search terms separated by comma:"),
                                                actionButton("plotSearchTermPerformance", "Plot Data")),
                                   mainPanel(
                                     tabsetPanel(id = "searchTermsTabs",
                                                 tabPanel("Totals", DTOutput("searchTermTotals")),
                                                 tabPanel("Impressions", plotOutput("searchTermImpressions")),
                                                 tabPanel("Clicks", plotOutput("searchTermClicks")),
                                                 tabPanel("Cost", plotOutput("searchTermCost")),
                                                 tabPanel("CPC", plotOutput("searchTermCpc")),
                                                 tabPanel("ConversionRate", plotOutput("searchTermCr")),
                                                 tabPanel("Conversions", plotOutput("searchTermConversions")),
                                                 tabPanel("CPA", plotOutput("searchTermCpa"))
                                     ))
                                 )
                        ),
                        tabPanel("N-Grams",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotnGrams", "Plot Data")),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Unigrams", plotOutput("unigrams", height = "800px")),
                                       tabPanel("Bigrams", plotOutput("bigrams", height = "800px")),
                                       tabPanel("Trigrams", plotOutput("trigrams", height = "800px"))
                                     ))
                                 )
                        ),
                        tabPanel("Impression Share",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotImpressionShare", "Plot Data")),
                                   mainPanel(
                                     plotOutput("isPlot"),
                                     plotOutput("isOtherPlot"))
                                 )
                        ),
                        tabPanel("Device Performance",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotDevicePerformance", "Plot Data")),
                                   mainPanel(plotOutput("devicePlot", height = "800px"))
                                 )
                        ),
                        tabPanel("Quality Score",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotQs", "Plot Data")),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Ad Relevance", tableOutput("adRelevance")),
                                       tabPanel("LP Experience", tableOutput("lpExperience")),
                                       tabPanel("Expected CTR", tableOutput("expectedCtr")),
                                       tabPanel("Quality Score", tableOutput("qualityScore")),
                                       tabPanel("Weighted QS", plotlyOutput("weightedQs", height = "800px"))
                                     ))
                                 )
                        ),
                        tabPanel("Match Types",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotMatchTypes", "Plot Data")),
                                   mainPanel(plotOutput("matchTypesPlot", height = "800px"))
                                 )
                        ),
                        tabPanel("Ad-Keyword Combinations",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotAdKeywords", "Plot Data")),
                                   mainPanel(plotOutput("adKeywordsPlot", brush = "adKeyBrush"),
                                             DTOutput("selectedKeyword"))
                                 )
                        ),
                        tabPanel("Performance Segments",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotPerformanceSegments", "Plot Data")),
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
                                   sidebarPanel(width = 3,
                                                actionButton("plotAnomalyDetection", "Plot Data")),
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
                                   sidebarPanel(width = 3,
                                                actionButton("plotTreemaps", "Plot Data")),
                                   mainPanel(plotOutput("treemapsPlot", height = "600px"))
                                 )
                        ),
                        tabPanel("Audiences",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotAudiencePerformance", "Plot Data")),
                                   mainPanel(DTOutput("audiencePerformanceTable"))
                                 )
                        ),
                        tabPanel("Device-Schedule",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotDeviceSchedulePerformance", "Plot Data")),
                                   mainPanel(plotOutput("deviceSchedulePerformancePlot", height = "1200px"))
                                 )
                        )),
             navbarMenu("Google Analytics",
                        tabPanel("Performance Overview",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotPerformanceOverview", "Plot Data")),
                                   mainPanel(plotOutput("performanceOverviewPlot", height = "800px"))
                                 )
                        ),
                        tabPanel("New vs Returning",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotNewVsReturning", "Plot Data")),
                                   mainPanel(plotOutput("newVsReturningPlot"))
                                 )
                        ),
                        tabPanel("Hostnames",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotHostnames", "Plot Data")),
                                   mainPanel(DTOutput("hostnamesPlot"))
                                 )
                        ),
                        tabPanel("Device Categories",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                     actionButton("plotGaDeviceCategory", "Plot Data")),
                                   mainPanel(
                                     uiOutput("dateSlider"),
                                     plotOutput("gaDeviceCategoryPlot"))
                                 )
                        ),
                        tabPanel("Channel Performance",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotChannelPerformance", "Plot Data")),
                                   mainPanel(plotlyOutput("channelPerformancePlot"))
                                 )
                        ),
                        tabPanel("Pages Performance",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotPagePerformance", "Plot Data")),
                                   mainPanel(plotlyOutput("pagePerformancePlot"))
                                 )
                        ),
                        tabPanel("Landing Pages Performance",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotLpPerformance", "Plot Data")),
                                   mainPanel(plotlyOutput("lpPerformancePlot"))
                                 )
                        ),
                        tabPanel("e-Commerce Performance",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotEcomPerformance", "Plot Data")),
                                   mainPanel(plotOutput("ecomPerformancePlot", height = "600px"))
                                 )
                        ),
                        tabPanel("Session Count",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotSessionCount", "Plot Data")),
                                   mainPanel(plotOutput("sessionCountPlot", height = "500px"))
                                 )
                        ),
                        tabPanel("Days Since Last Session",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotDaysSinceLastSession", "Plot Data")),
                                   mainPanel(plotOutput("daysSinceLastSessionPlot", height = "500px"))
                                 )
                        ),
                        tabPanel("Session Duration",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotSessionDuration", "Plot Data")),
                                   mainPanel(plotOutput("sessionDurationPlot", height = "500px"))
                                 )
                        ),
                        tabPanel("Page Depth",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotPageDepth", "Plot Data")),
                                   mainPanel(plotOutput("pageDepthPlot", height = "500px"))
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
                                   sidebarPanel(width = 3,
                                                actionButton("plotEventPerformance", "Plot Data")),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Total Events", plotOutput("totalEventsPlot")),
                                       tabPanel("Sessions With Events", plotOutput("sessionsWithEventsPlot"))
                                     ))
                                 )
                        ),
                        tabPanel("Goal Performance",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotGoalPerformance", "Plot Data")),
                                   mainPanel(
                                     tabsetPanel(
                                       tabPanel("Goal Completions", plotOutput("goalCompletionsPlot")),
                                       tabPanel("Conversion Rates", plotOutput("goalConversionRatesPlot"))
                                     ))
                                 )
                        ),
                        tabPanel("Product Performance",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                     actionButton(
                                                  "plotProductPerformance", "Plot Data"),
                                     selectInput("sizeMetric", "Dot Size Metric:",
                                                 c("Quantity" = "itemQuantity",
                                                   "Revenue" = "itemRevenue"))),
                                   mainPanel(plotlyOutput("productPerformancePlot"))
                                 )
                        ),
                        tabPanel("Anomaly Detector",
                                 sidebarPanel(width = 3,
                                              actionButton("plotGaAnomalies", "Plot Data")),
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
                                   sidebarPanel(width = 3,
                                                actionButton("plotSearchTerms", "Plot Data")),
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
                        ),
                        tabPanel("Query Parameter Extractor",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotUniqeParams", "Extract Parameters")),
                                   mainPanel(verbatimTextOutput("uniqueParamsPlot"))
                                 )
                        ),
                        tabPanel("Conversion Quality Index",
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                                actionButton("plotConversionQualityIndex", "Plot Data")),
                                   mainPanel(plotlyOutput("conversionQualityIndexPlot", height = "500px"))
                                 )
                        )
             ),
             navbarMenu("Other",
                        tabPanel("Scatterplot",
                                 sidebarLayout(
                                   sidebarPanel(
                                     fileInput("file1", "Choose CSV File",
                                               multiple = FALSE,
                                               accept = c("text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv")),
                                     actionButton("plotExampleScatterplot", "Plot Data"),
                                     checkboxInput("addSmooth", "Add Smooth")),
                                   mainPanel(plotOutput("exampleScatterplot"))
                                 )
                        )
             )
  )
)
