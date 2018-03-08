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
            summarise(QSImpr = sum(QSImpr), Impressions = sum(Impressions)) %>% ungroup() %>% mutate(weightedQS = QSImpr/Impressions) %>%
            filter(Impressions > 10)

        plot <- plotData %>% ggplot(aes(x = weightedQS, y = 1)) + geom_text(aes(label = AdGroup),position = position_jitter(0.7), size = 1.5)
        output$weightedQs <- renderPlotly(plot)
    })

    observeEvent(input$plotMatchTypes,{
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

        matchTypePerformance <- keywordPerformance %>% group_by(Date, MatchType) %>%
            summarize(Impressions = sum(Impressions), Clicks = sum(Clicks), Cost = sum(Cost),Conversions = sum(Conversions)) %>%
            mutate(Ctr = Clicks/Impressions, CPC = Cost/Clicks, CPA = Cost/Conversions, ConversionRate = Conversions/Clicks)

        matchTypePerformanceLong <- gather(matchTypePerformance, metric, value, Impressions:ConversionRate)

        output$matchTypesPlot <- renderPlot({
            ggplot(matchTypePerformanceLong, aes(y = value, x = Date, color = MatchType)) +
            geom_line() +
            facet_grid(metric ~ ., scales = "free_y") +
                theme(legend.position = "bottom")
        })
    })

    observeEvent(input$plotAdKeywords,{
        adPerformanceQuery <- statement(select=c('CampaignName','AdGroupName','Id','Description','HeadlinePart2','CriterionId',
                                                 'Cost','Conversions','CostPerConversion','Clicks'),
                                        report="AD_PERFORMANCE_REPORT",
                                        where="Cost > 100000000 AND AdNetworkType1 = SEARCH",
                                        start=input$dateRange[1],
                                        end=input$dateRange[2])

        adPerformance <- getData(clientCustomerId=input$adwordsAccountId, google_auth=adwordsAccessToken, statement=adPerformanceQuery)
        names(adPerformance) <- c("Campaign", "AdGroup", "AdId", "Description", "Headline2", "KeywordId",
                                  "Cost", "Conversions", "CPA","Clicks")

        adPerformance <- adPerformance %>% mutate(ConversionRate = Conversions/Clicks)

        output$adKeywordsPlot <- renderPlotly({
            plot_ly(data = adPerformance, x = ~ConversionRate, y = ~CPA, size = ~Conversions,
                text = ~paste("Campaign: ", Campaign, "<br>AdGroup: ", AdGroup, "<br>Headline2: ",
                              Headline2, "<br>Description: ", Description ,"<br>KeywordId: ", KeywordId))
        })
    })

    observeEvent(input$plotPerformanceSegments,{
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

        performanceSegments <- keywordPerformance %>% mutate(ImpQS = Impressions * as.numeric(QS), ImpPos = Impressions * Position) %>%
            group_by(Keyword) %>% summarise(CPA = sum(Cost)/sum(Conversions), Impressions = sum(Impressions), ImpQS = sum(ImpQS),
                                            Cost = sum(Cost), Conversions = sum(Conversions), ImpPos = sum(ImpPos), CPC = sum(Cost)/sum(Clicks)) %>%
            mutate(Position = ImpPos/Impressions, QS = ImpQS/Impressions)

        hasConversions <- performanceSegments %>% filter(Conversions > 0)
        output$convertersPlot <- renderPlotly({
            plot_ly(data = hasConversions, x = ~Position, y = ~CPA, size = ~Conversions,
                text = ~paste("Keyword: ", Keyword, "<br>Conversions: ", Conversions,
                              "<br>CPA: ", CPA, "<br>Position: ", Position))
        })

        notConverting <- performanceSegments %>% filter(Conversions == 0, Cost > 0)
        output$nonConvertersPlot <- renderPlotly({
            plot_ly(data = notConverting, x = ~Position, y = ~CPC, size = ~Cost,
                text = ~paste("Keyword: ", Keyword, "<br>Cost: ", Cost,
                              "<br>CPC: ", CPC, "<br>Position: ", Position))
        })

        clickless <- performanceSegments %>% filter(Cost == 0, Impressions > 0)
        output$clicklessPlot <- renderPlotly({
            plot_ly(data = clickless, x = ~Position, y = ~QS, size = ~Impressions, type = 'scatter',
                text = ~paste("Keyword: ", Keyword, "<br>Impressions: ", Impressions,
                              "<br>QS: ", QS, "<br>Position: ", Position))
        })
    })

    observeEvent(input$plotAnomalyDetection,{
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

        anomalyData <- accountPerformance %>%
            mutate(DateTime = ymd_hms(paste0(Date," ",HourOfDay,":00:00")),ImpIS = Impressions * SearchIS,
                   ImpPosition = Impressions * AveragePosition) %>%
            group_by(DateTime, HourOfDay) %>%
            summarise(Clicks = sum(Clicks, na.rm=TRUE), Conversions = sum(Conversions, na.rm=TRUE),
                      CPA = sum(Cost, na.rm=TRUE)/sum(Conversions, na.rm=TRUE),
                      CPC = sum(Cost, na.rm=TRUE)/sum(Clicks, na.rm=TRUE),
                      ImpIS = sum(ImpIS, na.rm=TRUE), ImpPosition = sum(ImpPosition, na.rm=TRUE),
                      Impressions = sum(Impressions, na.rm=TRUE)) %>%
            mutate(SearchIS = ImpIS / Impressions, Position = ImpPosition / Impressions)

        anomalyData <- anomalyData %>% arrange(DateTime)
        anomalyData[!is.finite(anomalyData$CPA),"CPA"] <- 0
        anomalyData[is.na(anomalyData$SearchIS),"SearchIS"] <- 0
        anomalyData[is.na(anomalyData$Clicks),"Clicks"] <- 0
        anomalyData[is.na(anomalyData$Conversions),"Conversions"] <- 0
        anomalyData[is.na(anomalyData$CPC),"CPC"] <- 0
        anomalyData[is.na(anomalyData$Position),"Position"] <- 0

        anomClicks <- AnomalyDetectionTs(anomalyData[,c("DateTime","Clicks")], direction='both',
                                         plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Clicks")
        output$clickAnomalies <- renderPlot(anomClicks$plot)

        anomConversions <- AnomalyDetectionTs(anomalyData[,c("DateTime","Conversions")], direction='both',
                                              plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Conversions")
        output$conversionAnomalies <- renderPlot(anomConversions$plot)

        anomCPA <- AnomalyDetectionTs(anomalyData[,c("DateTime","CPA")], direction='both',
                                      plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "CPA")
        output$cpaAnomalies <- renderPlot(anomCPA$plot)

        anomCPC <- AnomalyDetectionTs(anomalyData[,c("DateTime","CPC")], direction='both',
                                      plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "CPC")
        output$cpcAnomalies <- renderPlot(anomCPC$plot)

        anomSearchIS <- AnomalyDetectionTs(anomalyData[,c("DateTime","SearchIS")], direction='both',
                                           plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "SearchIS")
        output$isAnomalies <- renderPlot(anomSearchIS$plot)

        anomPosition <- AnomalyDetectionTs(anomalyData[,c("DateTime","Position")], direction='both',
                                           plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Position")
        output$positionAnomalies <- renderPlot(anomPosition$plot)
    })

    observeEvent(input$plotTreemaps,{
        campaignPerformanceQuery <- statement(select=c("Date","CampaignName","Conversions","Cost","Clicks"),
                                              report="CAMPAIGN_PERFORMANCE_REPORT",
                                              start=input$dateRange[1],
                                              end=input$dateRange[2])

        campaignPerformance <- getData(clientCustomerId=input$adwordsAccountId,
                                       google_auth=adwordsAccessToken, statement=campaignPerformanceQuery)
        names(campaignPerformance) <- c("Date","Campaign", "Conversions", "Cost", "Clicks")

        campaignPerformanceTidy <- campaignPerformance %>% group_by(Campaign) %>%
            summarize(Cost = sum(Cost), Conversions = sum(Conversions), CPA = sum(Cost)/sum(Conversions),
                      Clicks = sum(Clicks), ConversionRate = sum(Conversions)/sum(Clicks))

        campaignPerformanceTidy[!is.finite(campaignPerformanceTidy$CPA),"CPA"] <- 0

        output$treemapsPlot <- renderPlot({
            treemap(campaignPerformanceTidy,
                index="Campaign",
                vSize = "Cost",
                vColor = "CPA",
                palette = "RdYlBu",
                type="value")
        })
    })

    observeEvent(input$plotAudiencePerformance,{
        audiencePerformanceQuery <- statement(select = c( "UserListName", "Criteria", "AveragePosition", "Clicks",
                                                    "Conversions", "Cost", "Impressions" ),
                          report = "AUDIENCE_PERFORMANCE_REPORT",
                          start=input$dateRange[1],
                          end=input$dateRange[2])

        audiencePerformance <- getData(clientCustomerId=input$adwordsAccountId, google_auth=adwordsAccessToken, statement=audiencePerformanceQuery)

        tidyAudiencePerformance <- audiencePerformance %>% mutate(impPosition = Impressions * Position) %>%
            group_by(Userlistname, Audience) %>%
            summarise(Clicks = sum(Clicks), Impressions=sum(Impressions), Cost = sum(Cost),
                      Conversions = sum(Conversions), impPosition = sum(impPosition)) %>%
            mutate(CTR = Clicks/Impressions, AvgCpc = Cost/Clicks, AvgPosition = impPosition/Impressions,
                   CPA = Cost/Conversions, ConversionRate = Conversions/Clicks) %>%
            select(-impPosition)

        output$audiencePerformanceTable <- renderTable(tidyAudiencePerformance)
    })
}
