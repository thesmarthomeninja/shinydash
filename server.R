source('global.R')
server <- function(input, output, session) {

    analyticsAccessToken  <- callModule(googleAuth, "gaLogin")
    adwordsAccessToken <- doAuth()

    account_list <- reactive({
        validate(
            need(analyticsAccessToken(), "Please log in to see settings")
        )
        with_shiny(ga_account_list,
                   shiny_access_token = analyticsAccessToken())
    })

    output$globalSettings <- renderUI({
        accList <- account_list()
        sidebarPanel("",
                     selectInput("gaViewId", "Choose GA View",
                                 choices=unique(accList$accountName)),
                     textInput("adwordsAccountId", "Account ID",
                               placeholder = "XXX-XXX-XXXX",
                               width = "35%"),
                     dateRangeInput("dateRange","Date Range")
                     )
    })


    observeEvent(input$plotCampaignPerformance, {
        campaignPerformanceQuery <- statement(select=c("Date","CampaignName","Conversions","Cost","Clicks"),
                                              report="CAMPAIGN_PERFORMANCE_REPORT",
                                              start=input$dateRange[1],
                                              end=input$dateRange[2])

        campaignPerformance <- getData(clientCustomerId=input$adwordsAccountId,
                                       google_auth=adwordsAccessToken, statement=campaignPerformanceQuery)
        names(campaignPerformance) <- c("Date","Campaign", "Conversions", "Cost", "Clicks")

        tidyCampaignPerformance <- campaignPerformance %>% group_by(Campaign) %>%
            summarize(Cost = sum(Cost), Conversions = sum(Conversions), CPA = sum(Cost)/sum(Conversions),
                      Clicks = sum(Clicks), ConversionRate = sum(Conversions)/sum(Clicks))

        output$campaignPerformancePlot <- renderPlotly({
            plot_ly(data = tidyCampaignPerformance, x = ~ConversionRate, y = ~CPA, size = ~Conversions,
                    text = ~paste("Campaign: ", Campaign, "<br>Cost: ", Cost, "<br>Conversions: ",
                              Conversions, "<br>CPA: ", CPA, "<br>ConversionRate", ConversionRate),
                type = "scatter", mode = "markers")
        })
    })

    observeEvent(input$plotCorrelationMatrix, {
        accountPerformanceQuery <- statement(select=c("Date","DayOfWeek","HourOfDay","Device","Clicks","Impressions",
                                                      "Conversions","Cost", "SearchImpressionShare", "AveragePosition",
                                                      "SearchRankLostImpressionShare","SearchBudgetLostImpressionShare",
                                                      "SearchExactMatchImpressionShare"),
                                             report="ACCOUNT_PERFORMANCE_REPORT",
                                             start=input$dateRange[1],
                                             end=input$dateRange[2])

        accountPerformance <- getData(clientCustomerId=input$adwordsAccountId, google_auth=adwordsAccessToken, statement=accountPerformanceQuery)
        names(accountPerformance) <- c("Date","DayOfWeek","HourOfDay","Device","Clicks","Impressions","Conversions","Cost","SearchIS",
                                       "AveragePosition","RankLostIS","BudgetLostIS","ExactMatchIS")

        correlationData <- accountPerformance %>% group_by(Date) %>%
            summarise(Clicks = sum(Clicks), Conversions = sum(Conversions),
                      CPA = sum(Cost)/sum(Conversions), Cost = sum(Cost),
                      ConversionRate = sum(Conversions)/sum(Clicks),
                      CPC = sum(Cost)/sum(Clicks), Impressions = sum(Impressions),
                      CTR = sum(Clicks)/sum(Impressions)) %>%
            select(Clicks, Conversions, CPA, Cost, ConversionRate, CPC, Impressions, CTR)

        correlationData[!is.finite(correlationData$CPA),"CPA"] <- 0

        corr <- cor(correlationData)

        output$corrplot <- renderPlot(corrplot(corr, method = "pie", type = "upper"))
    })

    observeEvent(input$plotAdScheduling,{
        accountPerformanceQuery <- statement(select=c("Date","DayOfWeek","HourOfDay","Device","Clicks","Impressions",
                                                      "Conversions","Cost", "SearchImpressionShare", "AveragePosition",
                                                      "SearchRankLostImpressionShare","SearchBudgetLostImpressionShare",
                                                      "SearchExactMatchImpressionShare"),
                                             report="ACCOUNT_PERFORMANCE_REPORT",
                                             start=input$dateRange[1],
                                             end=input$dateRange[2])

        accountPerformance <- getData(clientCustomerId=input$adwordsAccountId, google_auth=adwordsAccessToken, statement=accountPerformanceQuery)
        names(accountPerformance) <- c("Date","DayOfWeek","HourOfDay","Device","Clicks","Impressions","Conversions","Cost","SearchIS",
                                           "AveragePosition","RankLostIS","BudgetLostIS","ExactMatchIS")

        accountPerformanceHeatmaps <- accountPerformance %>% mutate(ImpIS = Impressions * SearchIS) %>%
            group_by(DayOfWeek,HourOfDay) %>%
            summarize(Impressions = sum(Impressions), Conversions = sum(Conversions), CPA = sum(Cost)/sum(Conversions),
              ImpIS = sum(ImpIS, na.rm=TRUE)) %>% mutate(WeightedIS = ImpIS / Impressions)

        accountPerformanceHeatmaps$HourOfDay <- factor(accountPerformanceHeatmaps$HourOfDay, levels=c(0:23))
        accountPerformanceHeatmaps$DayOfWeek <- factor(accountPerformanceHeatmaps$DayOfWeek,
                                                       levels=c("Sunday","Saturday","Friday","Thursday","Wednesday","Tuesday","Monday"))

        output$impressionHeatMap <- renderPlot({
            ggplot(accountPerformanceHeatmaps, aes(x=HourOfDay, y=DayOfWeek, z=Impressions)) +
                geom_tile(aes(fill = Impressions)) + scale_fill_gradient(low="white", high="blue", labels=comma)
            })

        output$isHeatMap <- renderPlot({
            ggplot(accountPerformanceHeatmaps, aes(x=HourOfDay, y=DayOfWeek, z=WeightedIS)) +
                geom_tile(aes(fill = WeightedIS)) + scale_fill_gradient(low="white", high="blue", labels=comma)
            })

        output$conversionHeatMap <- renderPlot({
            ggplot(accountPerformanceHeatmaps, aes(x=HourOfDay, y=DayOfWeek, z=Conversions)) +
                geom_tile(aes(fill = Conversions)) + scale_fill_gradient(low="white", high="blue", labels=comma)
            })

        output$cpaHeatMap <- renderPlot({
            ggplot(accountPerformanceHeatmaps, aes(x=HourOfDay, y=DayOfWeek, z=CPA)) +
                geom_tile(aes(fill = CPA)) + scale_fill_gradient(low="white", high="blue", labels=comma)
            })

    })

    observeEvent(input$plotnGrams,{
        data(stop_words)
        searchQueryPerformanceQuery <- statement(select=c("Query","Clicks","Impressions","Conversions","Cost"),
                                                 report="SEARCH_QUERY_PERFORMANCE_REPORT",
                                                 start=input$dateRange[1],
                                                 end=input$dateRange[2])

        searchQueryPerformance <- getData(clientCustomerId=input$adwordsAccountId,
                                          google_auth=adwordsAccessToken, statement=searchQueryPerformanceQuery)

        unigrams <- searchQueryPerformance %>% unnest_tokens(word, Searchterm) %>% anti_join(stop_words) %>%
            group_by(word) %>%
            summarize(Clicks=sum(Clicks), Impressions=sum(Impressions), Conversions=sum(Conversions), Cost=sum(Cost),
                      Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks), Cpa=sum(Cost)/sum(Conversions),
                      AvgCpc = sum(Cost)/sum(Clicks))
        output$unigrams <- renderTable(unigrams)

        bigrams <- searchQueryPerformance %>% unnest_tokens(bigram, Searchterm, token="ngrams", n=2) %>% group_by(bigram) %>%
            summarize(Clicks=sum(Clicks), Impressions=sum(Impressions), Conversions=sum(Conversions),
                      Cost=sum(Cost), Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks),
                      Cpa=sum(Cost)/sum(Conversions), AvgCpc = sum(Cost)/sum(Clicks))
        output$bigrams <- renderTable(bigrams)

        trigrams <- searchQueryPerformance %>% unnest_tokens(trigram, Searchterm, token="ngrams", n=3) %>% group_by(trigram) %>%
            summarize(Clicks=sum(Clicks), Impressions=sum(Impressions), Conversions=sum(Conversions),
                      Cost=sum(Cost), Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks),
                      Cpa=sum(Cost)/sum(Conversions), AvgCpc = sum(Cost)/sum(Clicks))
        output$trigrams <- renderTable(trigrams)

    })

}
