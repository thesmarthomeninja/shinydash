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

        pu_imp <- unigrams %>% top_n(10, Impressions) %>% ggplot(aes(y=Impressions, x=reorder(word, Impressions))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("word") +
            theme(axis.text.y = element_text(size=8))

        pu_clicks <- unigrams %>% top_n(10, Clicks) %>% ggplot(aes(y=Clicks, x=reorder(word, Clicks))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("word") +
            theme(axis.text.y = element_text(size=8))

        pu_ctr <- unigrams %>% filter(Clicks > 100) %>% top_n(10, Ctr) %>% ggplot(aes(y=Ctr, x=reorder(word, Ctr))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("word") +
            theme(axis.text.y = element_text(size=8))

        pu_cpc <- unigrams %>% filter(Clicks > 100) %>% top_n(10, AvgCpc) %>% ggplot(aes(y=AvgCpc, x=reorder(word, AvgCpc))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("word") +
            theme(axis.text.y = element_text(size=8))

        pu_cost <- unigrams %>% top_n(10, Cost) %>% ggplot(aes(y=Cost, x=reorder(word, Cost))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("word") +
            theme(axis.text.y = element_text(size=8))

        pu_conv <- unigrams %>% top_n(10, Conversions) %>% ggplot(aes(y=Conversions, x=reorder(word, Conversions))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("word") +
            theme(axis.text.y = element_text(size=8))

        pu_cpa <- unigrams %>% filter(Conversions > 1) %>% top_n(10, Cpa) %>% ggplot(aes(y=Cpa, x=reorder(word, Cpa))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("word") +
            theme(axis.text.y = element_text(size=8))

        pu_cr <- unigrams %>% filter(Conversions > 1) %>% top_n(10, ConversionRate) %>% ggplot(aes(y=ConversionRate, x=reorder(word, ConversionRate))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("word") +
            theme(axis.text.y = element_text(size=8))

        output$unigrams <- renderPlot(plot_grid(pu_imp, pu_clicks, pu_ctr, pu_cpc, pu_cost, pu_conv, pu_cpa, pu_cr, ncol=2))

        bigrams <- searchQueryPerformance %>% unnest_tokens(bigram, Searchterm, token="ngrams", n=2) %>% group_by(bigram) %>%
            summarize(Clicks=sum(Clicks), Impressions=sum(Impressions), Conversions=sum(Conversions),
                      Cost=sum(Cost), Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks),
                      Cpa=sum(Cost)/sum(Conversions), AvgCpc = sum(Cost)/sum(Clicks))

        pb_imp <- bigrams %>% top_n(10, Impressions) %>% ggplot(aes(y=Impressions, x=reorder(bigram, Impressions))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pb_clicks <- bigrams %>% top_n(10, Clicks) %>% ggplot(aes(y=Clicks, x=reorder(bigram, Clicks))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pb_ctr <- bigrams %>% filter(Clicks > 100) %>% top_n(10, Ctr) %>% ggplot(aes(y=Ctr, x=reorder(bigram, Ctr))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pb_cpc <- bigrams %>% filter(Clicks > 100) %>% top_n(10, AvgCpc) %>% ggplot(aes(y=AvgCpc, x=reorder(bigram, AvgCpc))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pb_cost <- bigrams %>% top_n(10, Cost) %>% ggplot(aes(y=Cost, x=reorder(bigram, Cost))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pb_conv <- bigrams %>% top_n(10, Conversions) %>% ggplot(aes(y=Conversions, x=reorder(bigram, Conversions))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pb_cpa <- bigrams %>% filter(Conversions > 1) %>% top_n(10, Cpa) %>% ggplot(aes(y=Cpa, x=reorder(bigram, Cpa))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pb_cr <- bigrams %>% filter(Conversions > 1) %>% top_n(10, ConversionRate) %>%
            ggplot(aes(y=ConversionRate, x=reorder(bigram, ConversionRate))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        output$bigrams <- renderPlot(plot_grid(pb_imp, pb_clicks, pb_ctr, pb_cpc, pb_cost, pb_conv, pb_cpa, pb_cr, ncol=2))

        trigrams <- searchQueryPerformance %>% unnest_tokens(trigram, Searchterm, token="ngrams", n=3) %>% group_by(trigram) %>%
            summarize(Clicks=sum(Clicks), Impressions=sum(Impressions), Conversions=sum(Conversions),
                      Cost=sum(Cost), Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks),
                      Cpa=sum(Cost)/sum(Conversions), AvgCpc = sum(Cost)/sum(Clicks))

        pt_imp <- trigrams %>% top_n(10, Impressions) %>% ggplot(aes(y=Impressions, x=reorder(trigram, Impressions))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pt_clicks <- trigrams %>% top_n(10, Clicks) %>% ggplot(aes(y=Clicks, x=reorder(trigram, Clicks))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pt_ctr <- trigrams %>% filter(Clicks > 100) %>% top_n(10, Ctr) %>% ggplot(aes(y=Ctr, x=reorder(trigram, Ctr))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pt_cpc <- trigrams %>% filter(Clicks > 100) %>% top_n(10, AvgCpc) %>% ggplot(aes(y=AvgCpc, x=reorder(trigram, AvgCpc))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pt_cost <- trigrams %>% top_n(10, Cost) %>% ggplot(aes(y=Cost, x=reorder(trigram, Cost))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pt_conv <- trigrams %>% top_n(10, Conversions) %>% ggplot(aes(y=Conversions, x=reorder(trigram, Conversions))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pt_cpa <- trigrams %>% filter(Conversions > 1) %>% top_n(10, Cpa) %>% ggplot(aes(y=Cpa, x=reorder(trigram, Cpa))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        pt_cr <- trigrams %>% filter(Conversions > 1) %>% top_n(10, ConversionRate) %>%
            ggplot(aes(y=ConversionRate, x=reorder(trigram, ConversionRate))) +
            geom_bar(stat = "identity") +
            coord_flip() +
            xlab("phrase") +
            theme(axis.text.y = element_text(size=8))

        output$trigrams <- renderPlot(plot_grid(pt_imp, pt_clicks, pt_ctr, pt_cpc, pt_cost, pt_conv, pt_cpa, pt_cr, ncol=2))

    })

    observeEvent(input$plotImpressionShare,{
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

        accountPerformanceIS <- accountPerformance %>%
            mutate(ImpIS = Impressions * SearchIS, ImpBudgetLostIS = Impressions * BudgetLostIS,
                   ImpRankLostIS = Impressions * RankLostIS,
                   ImpExactMatchIS = Impressions * ExactMatchIS) %>%
            group_by(Date) %>%
            summarize(CPC = sum(Cost, na.rm = TRUE)/sum(Clicks, na.rm = TRUE), ImpRankLostIS = sum(ImpRankLostIS, na.rm = TRUE),
                      ImpBudgetLostIS = sum(ImpBudgetLostIS, na.rm = TRUE), ImpIS = sum(ImpIS, na.rm=TRUE),
                      ImpExactMatchIS = sum(ImpExactMatchIS, na.rm = TRUE), Impressions = sum(Impressions, na.rm = TRUE)) %>%
            mutate(IS = ImpIS / Impressions, ISLostToRank = ImpRankLostIS / Impressions,
                   ISLostToBudget = ImpBudgetLostIS / Impressions,
                   ExactMatchIS = ImpExactMatchIS / Impressions)

        longAccountPerformanceIS <- gather(accountPerformanceIS, metric, value, CPC:ExactMatchIS)

        long_IS <- longAccountPerformanceIS %>% filter(metric %in% c("IS","ISLostToBudget","ISLostToRank"))
        long_other <- longAccountPerformanceIS %>% filter(metric %in% c("CPC","ExactMatchIS"))

        output$isPlot <- renderPlot({
            ggplot(long_IS) +
            geom_area(aes(x = Date, y = value, fill = metric), position = "stack") +
                theme(legend.position = "bottom")
            })

        output$isOtherPlot <- renderPlot({
            ggplot(long_other, aes(x = Date, y = value)) +
            geom_line() +
                facet_wrap(~metric, ncol=1)
            })
    })

    observeEvent(input$plotDevicePerformance,{
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

        accountPerformanceDevice <- accountPerformance %>% group_by(Date, Device) %>%
            summarize(Conversions = sum(Conversions, na.rm = TRUE), Cost = sum(Cost, na.rm = TRUE),
                      Clicks = sum(Clicks, na.rm = TRUE),
                      Impressions = sum(Impressions, na.rm = TRUE)) %>%
            mutate(CPA = Cost / Conversions, ConversionRate = Conversions / Clicks,
                   CPC = Cost / Clicks, CTR = Clicks / Impressions)

        longAccountPerformanceDevice <- accountPerformanceDevice %>% gather(metric, value, Conversions:CTR)

        output$devicePlot <- renderPlot({
            longAccountPerformanceDevice %>% ggplot(aes(x = Date, y = value, color = Device)) +
            geom_line() +
            facet_grid(metric ~ ., scales = "free_y") +
                theme(legend.position = "bottom")
            })

    })

    observeEvent(input$plotQs,{
        keywordPerformanceQuery <- statement(select=c("Date","CampaignName","AdGroupName","Criteria","KeywordMatchType",
                                                      "Impressions","Clicks","Cost","Conversions","HasQualityScore",
                                                      "CreativeQualityScore","PostClickQualityScore","SearchPredictedCtr",
                                                      "AveragePosition","QualityScore","SearchImpressionShare"),
                                             report="KEYWORDS_PERFORMANCE_REPORT",
                                             start=input$dateRange[1],
                                             end=input$dateRange[2])

        keywordPerformance <- getData(clientCustomerId=input$adwordsAccountId, google_auth=adwordsAccessToken, statement=keywordPerformanceQuery)
        names(keywordPerformance) <- c("Date","Campaign","AdGroup","Keyword","MatchType","Impressions","Clicks","Cost","Conversions",
                                       "HasQualityScore","AdRelevance","LPExperience","ExpectedCTR","Position","QS","SearchIS")

        qualityScoreAnalysis <- keywordPerformance %>% filter(Date == input$dateRange[2], HasQualityScore == "true")

        output$adRelevance <- renderTable({
            qualityScoreAnalysis %>% group_by(AdRelevance) %>% summarize(n = n()) %>%
                mutate(freq = n/sum(n)) %>% arrange(desc(n))
        })

        output$lpExperience <- renderTable({
            qualityScoreAnalysis %>% group_by(LPExperience) %>% summarize(n = n()) %>%
                mutate(freq = n/sum(n)) %>% arrange(desc(n))
        })

        output$expectedCtr <- renderTable({
            qualityScoreAnalysis %>% group_by(ExpectedCTR) %>% summarize(n = n()) %>%
                mutate(freq = n/sum(n)) %>% arrange(desc(n))
        })

        output$qualityScore <- renderTable({
            qualityScoreAnalysis %>% group_by(QS) %>% summarize(n = n()) %>%
                mutate(freq = n/sum(n)) %>% arrange(desc(n))
        })

        plotData <- qualityScoreAnalysis %>% mutate(QSImpr = as.numeric(QS) * Impressions) %>% group_by(Campaign, AdGroup) %>%
            summarise(QSImpr = sum(QSImpr), Impressions = sum(Impressions)) %>% ungroup() %>% mutate(weightedQS = QSImpr/Impressions)

        plot <- plotData %>% ggplot(aes(x = weightedQS, y = 1)) + geom_text(aes(label = AdGroup),position = position_jitter(0.7), size = 1.5)
        output$weightedQs <- renderPlotly(ggplotly(plot))
    })
}
