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
  
  selectedId <- callModule(authDropdown, "auth_menu", ga.table = account_list)
  
  output$globalSettings <- renderUI({
    accList <- account_list()
    tagList(
      authDropdownUI("auth_menu"),
      textInput("adwordsAccountId", "Account ID",
                placeholder = "XXX-XXX-XXXX",
                width = "35%", value = "587-337-7079"),
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
    
    output$campaignPerformancePlot <- renderPlot({
      ggplot(tidyCampaignPerformance, aes(x = ConversionRate, y = CPA, size = Conversions)) + 
        geom_point(alpha = 0.5, color = "blue") +
        theme_minimal()
      
    })
    
    output$cpSelectedPoints <- renderDT({
      datatable(brushedPoints(tidyCampaignPerformance, input$cpBrush, xvar = "ConversionRate", yvar = "CPA"), 
                options = list(scrollX = T)) %>% 
        formatCurrency(c("Cost", "CPA")) %>% 
        formatPercentage(c("ConversionRate"), 2) 
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
    significance <- cor.mtest(correlationData, conf.level = 0.95)
    
    output$corrplot <- renderPlot(corrplot(corr, type = "upper", p.mat = significance$p, insig = "label_sig",
                                           sig.level = c(.001, .01, .05), pch.cex = .9, pch.col = "white"))
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
    
    currentAdSchedulingTab <- reactive(input$adSchedulingTabs)
    
    impressionHeatMap <- reactive({
      ggplot(accountPerformanceHeatmaps, aes(x=HourOfDay, y=DayOfWeek, z=Impressions)) +
        geom_tile(aes(fill = Impressions)) + scale_fill_gradientn(colours = c("green", "yellow", "red"), labels=comma) +
        theme_minimal()
    })
    
    output$impressionHeatMap <- renderPlot({
      impressionHeatMap()
    })
    
    isHeatMap <- reactive({
      ggplot(accountPerformanceHeatmaps, aes(x=HourOfDay, y=DayOfWeek, z=WeightedIS)) +
        geom_tile(aes(fill = WeightedIS)) + scale_fill_gradientn(colours = c("green", "yellow", "red"), labels=comma) +
        theme_minimal()
    })
    
    output$isHeatMap <- renderPlot({
      isHeatMap()
    })
    
    conversionHeatMap <- reactive({
      ggplot(accountPerformanceHeatmaps, aes(x=HourOfDay, y=DayOfWeek, z=Conversions)) +
        geom_tile(aes(fill = Conversions)) + scale_fill_gradientn(colours = c("green", "yellow", "red"), labels=comma) +
        theme_minimal()
    })
    
    output$conversionHeatMap <- renderPlot({
      conversionHeatMap()
    })
    
    cpaHeatMap <- reactive({
      ggplot(accountPerformanceHeatmaps, aes(x=HourOfDay, y=DayOfWeek, z=CPA)) +
        geom_tile(aes(fill = CPA)) + scale_fill_gradientn(colours = c("green", "yellow", "red"), labels=comma) +
        theme_minimal()
    })
    
    output$cpaHeatMap <- renderPlot({
      cpaHeatMap()
    })
    
    output$downloadAdScheduling <- downloadHandler(
      filename = "testplot.png",
      content = function(file){
        png(file, width = 720)
        
        switch(currentAdSchedulingTab(),
               "Impressions" = print(impressionHeatMap()),
               "Impression Share" = print(isHeatMap()),
               "Conversions" = print(conversionHeatMap()),
               "CPA" = print(cpaHeatMap())
        )
        
        dev.off()
      }
    )
    
  })
  
  lastSearchTermPerformanceQuery <- NULL
  lastSearchTermPerformance <- NULL 
  
  observeEvent(input$plotSearchTermPerformance,{
    
    lookupWords <- reactive({
      str_replace_all(input$lookupTerms, ",", "|")
    })
    
    
    searchTermPerformanceQuery <- statement(select=c("Query","MonthOfYear","Clicks","Impressions","Conversions","Cost"),
                                            report="SEARCH_QUERY_PERFORMANCE_REPORT",
                                            start=input$dateRange[1],
                                            end=input$dateRange[2])
    
    if(is.null(lastSearchTermPerformanceQuery) || !identical(lastSearchTermPerformanceQuery, searchTermPerformanceQuery)){
      
      searchTermPerformance <- getData(clientCustomerId=input$adwordsAccountId,
                                       google_auth=adwordsAccessToken, statement=searchTermPerformanceQuery) 
      
      lastSearchTermPerformanceQuery <<- searchTermPerformanceQuery
      lastSearchTermPerformance <<- searchTermPerformance
    }
    
    filteredSearchTerms <- lastSearchTermPerformance %>%
      filter(str_detect(lastSearchTermPerformance$Searchterm, lookupWords())) %>%
      mutate(Term = str_extract(Searchterm, lookupWords()))
    
    groupedSearchTerms <- filteredSearchTerms %>% group_by(Term) %>% 
      summarise(Clicks = sum(Clicks), Impressions = sum(Impressions), Conversions = sum(Conversions), Cost = sum(Cost)) %>% 
      mutate(Ctr = Clicks/Impressions, Cpc = Cost/Clicks, Cpa = Cost/Conversions, Cr = Conversions/Clicks)
    
    output$searchTermTotals <- renderDT({
      datatable(groupedSearchTerms, options = list(scrollX = T)) %>% 
        formatCurrency(c("Cost", "Cpc","Cpa")) %>% 
        formatPercentage(c("Ctr", "Cr"), 2)
    })
    
    groupedSearchTermsByMonth <- filteredSearchTerms %>% group_by(Term, MonthofYear) %>% summarise(Clicks = sum(Clicks),
                                                                                                   Impressions = sum(Impressions), Conversions = sum(Conversions), Cost = sum(Cost)) %>% 
      mutate(Ctr = Clicks/Impressions, Cpc = Cost/Clicks, Cpa = Cost/Conversions, Cr = Conversions/Clicks)
    
    groupedSearchTermsByMonth$MonthofYear <- factor(groupedSearchTermsByMonth$MonthofYear, 
                                                    levels=c("January","February","March","April","May","June","July","August",
                                                             "September","October","Novemer","December"))
    
    
    searchTermClicks <- reactive({
      ggplot(groupedSearchTermsByMonth, aes(x = MonthofYear, y = Clicks, fill = Term)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal()
    })
    
    output$searchTermClicks <- renderPlot({
      searchTermClicks()
    })
    
    searchTermImpressions <- reactive({
      ggplot(groupedSearchTermsByMonth, aes(x = MonthofYear, y = Impressions, fill = Term)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal()
    })
    
    output$searchTermImpressions <- renderPlot({
      searchTermImpressions()
    })
    
    searchTermCost <- reactive({
      ggplot(groupedSearchTermsByMonth, aes(x = MonthofYear, y = Cost, fill = Term)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal()
    })
    
    output$searchTermCost <- renderPlot({
      searchTermCost()
    })
    
    searchTermCpc <- reactive({
      ggplot(groupedSearchTermsByMonth, aes(x = MonthofYear, y = Cpc, fill = Term)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal()
    })
    
    output$searchTermCpc <- renderPlot({
      searchTermCpc()
    })
    
    searchTermConversions <- reactive({
      ggplot(groupedSearchTermsByMonth, aes(x = MonthofYear, y = Conversions, fill = Term)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal()
    })
    
    output$searchTermConversions <- renderPlot({
      searchTermConversions()
    })
    
    searchTermCpa <- reactive({
      ggplot(groupedSearchTermsByMonth, aes(x = MonthofYear, y = Cpa, fill = Term)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal()
    })
    
    output$searchTermCpa <- renderPlot({
      searchTermCpa()
    })
    
    searchTermCr <- reactive({
      ggplot(groupedSearchTermsByMonth, aes(x = MonthofYear, y = Cr, fill = Term)) +
        geom_bar(position = "dodge", stat = "identity") +
        theme_minimal()
    })
    
    output$searchTermCr <- renderPlot({
      searchTermCr()
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
      theme(axis.text.y = element_text(size=16))
    
    pu_clicks <- unigrams %>% top_n(10, Clicks) %>% ggplot(aes(y=Clicks, x=reorder(word, Clicks))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("word") +
      theme(axis.text.y = element_text(size=16))
    
    pu_ctr <- unigrams %>% filter(Clicks > 100) %>% top_n(10, Ctr) %>% ggplot(aes(y=Ctr, x=reorder(word, Ctr))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("word") +
      theme(axis.text.y = element_text(size=16))
    
    pu_cpc <- unigrams %>% filter(Clicks > 100) %>% top_n(10, AvgCpc) %>% ggplot(aes(y=AvgCpc, x=reorder(word, AvgCpc))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("word") +
      theme(axis.text.y = element_text(size=16))
    
    pu_cost <- unigrams %>% top_n(10, Cost) %>% ggplot(aes(y=Cost, x=reorder(word, Cost))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("word") +
      theme(axis.text.y = element_text(size=16))
    
    pu_conv <- unigrams %>% top_n(10, Conversions) %>% ggplot(aes(y=Conversions, x=reorder(word, Conversions))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("word") +
      theme(axis.text.y = element_text(size=16))
    
    pu_cpa <- unigrams %>% filter(Conversions > 1) %>% top_n(10, Cpa) %>% ggplot(aes(y=Cpa, x=reorder(word, Cpa))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("word") +
      theme(axis.text.y = element_text(size=16))
    
    pu_cr <- unigrams %>% filter(Conversions > 1) %>% top_n(10, ConversionRate) %>% ggplot(aes(y=ConversionRate, x=reorder(word, ConversionRate))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("word") +
      theme(axis.text.y = element_text(size=16))
    
    output$unigrams <- renderPlot(plot_grid(pu_imp, pu_clicks, pu_ctr, pu_cpc, pu_cost, pu_conv, pu_cpa, pu_cr, ncol=2))
    
    bigrams <- searchQueryPerformance %>% unnest_tokens(bigram, Searchterm, token="ngrams", n=2) %>% group_by(bigram) %>%
      summarize(Clicks=sum(Clicks), Impressions=sum(Impressions), Conversions=sum(Conversions),
                Cost=sum(Cost), Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks),
                Cpa=sum(Cost)/sum(Conversions), AvgCpc = sum(Cost)/sum(Clicks))
    
    pb_imp <- bigrams %>% top_n(10, Impressions) %>% ggplot(aes(y=Impressions, x=reorder(bigram, Impressions))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pb_clicks <- bigrams %>% top_n(10, Clicks) %>% ggplot(aes(y=Clicks, x=reorder(bigram, Clicks))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pb_ctr <- bigrams %>% filter(Clicks > 100) %>% top_n(10, Ctr) %>% ggplot(aes(y=Ctr, x=reorder(bigram, Ctr))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pb_cpc <- bigrams %>% filter(Clicks > 100) %>% top_n(10, AvgCpc) %>% ggplot(aes(y=AvgCpc, x=reorder(bigram, AvgCpc))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pb_cost <- bigrams %>% top_n(10, Cost) %>% ggplot(aes(y=Cost, x=reorder(bigram, Cost))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pb_conv <- bigrams %>% top_n(10, Conversions) %>% ggplot(aes(y=Conversions, x=reorder(bigram, Conversions))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pb_cpa <- bigrams %>% filter(Conversions > 1) %>% top_n(10, Cpa) %>% ggplot(aes(y=Cpa, x=reorder(bigram, Cpa))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pb_cr <- bigrams %>% filter(Conversions > 1) %>% top_n(10, ConversionRate) %>%
      ggplot(aes(y=ConversionRate, x=reorder(bigram, ConversionRate))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    output$bigrams <- renderPlot(plot_grid(pb_imp, pb_clicks, pb_ctr, pb_cpc, pb_cost, pb_conv, pb_cpa, pb_cr, ncol=2))
    
    trigrams <- searchQueryPerformance %>% unnest_tokens(trigram, Searchterm, token="ngrams", n=3) %>% group_by(trigram) %>%
      summarize(Clicks=sum(Clicks), Impressions=sum(Impressions), Conversions=sum(Conversions),
                Cost=sum(Cost), Ctr=sum(Clicks)/sum(Impressions), ConversionRate=sum(Conversions)/sum(Clicks),
                Cpa=sum(Cost)/sum(Conversions), AvgCpc = sum(Cost)/sum(Clicks))
    
    pt_imp <- trigrams %>% top_n(10, Impressions) %>% ggplot(aes(y=Impressions, x=reorder(trigram, Impressions))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pt_clicks <- trigrams %>% top_n(10, Clicks) %>% ggplot(aes(y=Clicks, x=reorder(trigram, Clicks))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pt_ctr <- trigrams %>% filter(Clicks > 100) %>% top_n(10, Ctr) %>% ggplot(aes(y=Ctr, x=reorder(trigram, Ctr))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pt_cpc <- trigrams %>% filter(Clicks > 100) %>% top_n(10, AvgCpc) %>% ggplot(aes(y=AvgCpc, x=reorder(trigram, AvgCpc))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pt_cost <- trigrams %>% top_n(10, Cost) %>% ggplot(aes(y=Cost, x=reorder(trigram, Cost))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pt_conv <- trigrams %>% top_n(10, Conversions) %>% ggplot(aes(y=Conversions, x=reorder(trigram, Conversions))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pt_cpa <- trigrams %>% filter(Conversions > 1) %>% top_n(10, Cpa) %>% ggplot(aes(y=Cpa, x=reorder(trigram, Cpa))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
    pt_cr <- trigrams %>% filter(Conversions > 1) %>% top_n(10, ConversionRate) %>%
      ggplot(aes(y=ConversionRate, x=reorder(trigram, ConversionRate))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("phrase") +
      theme(axis.text.y = element_text(size=16))
    
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
        theme_minimal() +
        theme(legend.position = "bottom")
    })
    
    output$isOtherPlot <- renderPlot({
      ggplot(long_other, aes(x = Date, y = value)) +
        geom_line() +
        theme_minimal() +
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
        theme_minimal() +
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
      qualityScoreAnalysis %>% group_by(AdRelevance) %>% summarize(n = n(), cost = sum(Cost)) %>%
        mutate(perc = n/sum(n), perc_cost = cost/sum(cost)) %>% select(AdRelevance, n, perc, cost, perc_cost) %>% arrange(desc(n))
    })
    
    output$lpExperience <- renderTable({
      qualityScoreAnalysis %>% group_by(LPExperience) %>% summarize(n = n(), cost = sum(Cost)) %>%
        mutate(perc = n/sum(n), perc_cost = cost/sum(cost)) %>% select(LPExperience, n, perc, cost, perc_cost) %>% arrange(desc(n))
    })
    
    output$expectedCtr <- renderTable({
      qualityScoreAnalysis %>% group_by(ExpectedCTR) %>% summarize(n = n(), cost = sum(Cost)) %>%
        mutate(perc = n/sum(n), perc_cost = cost/sum(cost)) %>% select(ExpectedCTR, n, perc, cost, perc_cost) %>% arrange(desc(n))
    })
    
    output$qualityScore <- renderTable({
      qualityScoreAnalysis %>% group_by(QS) %>% summarize(n = n(), cost = sum(Cost)) %>%
        mutate(perc = n/sum(n), perc_cost = cost/sum(cost)) %>% select(QS, n, perc, cost, perc_cost) %>% arrange(desc(as.numeric(QS)))
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
        theme_minimal() +
        theme(legend.position = "bottom")
    })
  })
  
  observeEvent(input$plotAdKeywords,{
    
    adPerformanceQuery <- statement(select=c('Id','HeadlinePart1','HeadlinePart2','Description'),
                                    report="AD_PERFORMANCE_REPORT",
                                    where = "Clicks > 100",
                                    start=input$dateRange[1],
                                    end=input$dateRange[2])
    
    adPerformance <- getData(clientCustomerId=input$adwordsAccountId, google_auth=adwordsAccessToken, statement=adPerformanceQuery)
    
    names(adPerformance) <- c("AdId", "Headline1", "Headline2", "Description")
    
    keywordPerformanceQuery <- statement(select=c('Query','CreativeId','Cost','Conversions','Clicks'),
                                         report="SEARCH_QUERY_PERFORMANCE_REPORT",
                                         where="Clicks > 9",
                                         start=input$dateRange[1],
                                         end=input$dateRange[2])
    
    keywordPerformance <- getData(clientCustomerId=input$adwordsAccountId, google_auth=adwordsAccessToken, statement=keywordPerformanceQuery)
    names(keywordPerformance) <- c("Query","AdId","Cost","Conversions","Clicks")
    
    keywordPerformance <- keywordPerformance %>% mutate(ConversionRate = Conversions/Clicks, CPA = Cost/Conversions)
    
    mergedPerformance <- inner_join(keywordPerformance, adPerformance, by = "AdId")
    
    output$adKeywordsPlot <- renderPlot({
      ggplot(data = mergedPerformance, aes(x = ConversionRate, y = CPA, size = Conversions)) +
        geom_point() +
        theme_minimal()
    })
    
    output$selectedKeyword <- renderDT({
      datatable(brushedPoints(mergedPerformance, input$adKeyBrush, xvar = "ConversionRate", yvar = "CPA"), 
                options = list(scrollX = T)) %>% 
        formatCurrency(c("Cost", "CPA")) %>% 
        formatPercentage(c("ConversionRate"), 2) 
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
    
    output$audiencePerformanceTable <- renderDT({
      datatable(tidyAudiencePerformance, options = list(scrollX = T)) %>% 
        formatCurrency(c("Cost", "AvgCpc","CPA")) %>% 
        formatPercentage(c("CTR", "ConversionRate"), 2) %>% 
        formatRound("AvgPosition", 2)
    }) 
  })
  
  observeEvent(input$plotDeviceSchedulePerformance,{
    deviceScheduleQuery <- statement(select = c("Device", "DayOfWeek", "HourOfDay", "AveragePosition", "Clicks", "Conversions",
                                                "Cost", "Impressions", "Ctr", "AverageCpc", "CostPerConversion", "ConversionRate",
                                                "SearchImpressionShare"),
                                     report = "ACCOUNT_PERFORMANCE_REPORT",
                                     start=input$dateRange[1],
                                     end=input$dateRange[2])
    
    deviceSchedulePerformance <- getData(clientCustomerId=input$adwordsAccountId, google_auth=adwordsAccessToken,
                                         statement=deviceScheduleQuery)
    names(deviceSchedulePerformance) <- c("Device","DayOfWeek","HourOfDay","Position","Clicks","Conversions",
                                          "Cost","Impressions","Ctr","CPC","CPA","CR","IS")
    
    tidyDeviceSchedule <- gather(deviceSchedulePerformance, metric, value, Position:IS)
    
    tidyDeviceSchedule$DayOfWeek<- factor(tidyDeviceSchedule$DayOfWeek,c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
    tidyDeviceSchedule$metric<- factor(tidyDeviceSchedule$metric,c("Clicks","Impressions","Ctr","CPC","Cost","Position","Conversions",
                                                                   "CPA","CR","IS"))
    
    output$deviceSchedulePerformancePlot <- renderPlot({
      ggplot(tidyDeviceSchedule, aes(x=as.numeric(HourOfDay), y=value, color=Device)) +
        geom_line() +
        theme_minimal() +
        theme(legend.position = "top") +
        facet_grid(metric~DayOfWeek, scales = "free_y")
    })
  })
  
  observeEvent(input$plotPerformanceOverview,{
    gaPerformanceOverview <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                              metrics = c('sessions','users','sessionsPerUser','pageviewsPerSession',
                                                          'avgSessionDuration','bounceRate','transactions','transactionRevenue',
                                                          'revenuePerTransaction','transactionsPerSession'),
                                              dimensions = 'date',
                                              anti_sample = TRUE)
    
    names(gaPerformanceOverview) <- c('Date','Sessions','Users','SessionsPerUser','PagesPerSession',
                                      'AvgSessionDuratio','BounceRate','Transactions','Revenue','AvgOrderValue','ConversionRate')
    
    longGaPerformanceOverview <- gaPerformanceOverview %>% gather(metric, value, Sessions:ConversionRate)
    
    output$performanceOverviewPlot <- renderPlot({
      ggplot(longGaPerformanceOverview, aes(x = Date, y = value)) +
        geom_line() +
        theme_minimal() +
        facet_grid(metric~., scales = "free_y")
    })
  })
  
  observeEvent(input$plotNewVsReturning,{
    newVsReturning <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                       metrics = 'users',
                                       dimensions = c('date','userType'),
                                       anti_sample = TRUE)
    
    names(newVsReturning) <- c('Date','userType','Users')
    
    output$newVsReturningPlot <- renderPlot({
      ggplot(newVsReturning) +
        geom_area(aes(x = Date, y = Users, fill = userType), position = "stack") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
  })
  
  observeEvent(input$plotHostnames,{
    hostnames <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                  metrics = 'sessions',
                                  dimensions = 'hostname',
                                  anti_sample = TRUE)
    
    output$hostnamesPlot <- renderDT(hostnames %>% arrange(desc(sessions)))
  })
  
  output$dateSlider <- renderUI({
    sliderInput("dateRangeSlider", "Date Range:",
                min = as.Date(input$dateRange[1]),
                max = as.Date(input$dateRange[2]),
                value=c(as.Date(input$dateRange[1]), as.Date(input$dateRange[2])))
  })
  
  observeEvent(input$plotGaDeviceCategory,{
    gaDeviceCategory <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                         metrics = 'sessions',
                                         dimensions = c('date','deviceCategory'),
                                         anti_sample = TRUE)
    
    #    gaDeviceCategory <- google_analytics("104371403", date_range = c("2018-03-01", "2018-03-15"),
    #                                         metrics = 'sessions',
    #                                         dimensions = c('date','deviceCategory'),
    #                                         anti_sample = TRUE)
    
    names(gaDeviceCategory) <- c('Date','deviceCategory','Sessions')
    
    filteredGaDeviceCategory <- reactive({
      gaDeviceCategory %>% 
        filter(Date >= input$dateRangeSlider[1] & Date <= input$dateRangeSlider[2])
    })
    
    #    filteredGaDeviceCategory <- gaDeviceCategory %>% 
    #      filter(Date >= "2018-03-07" & Date <= "2018-03-15")
    
    output$gaDeviceCategoryPlot <- renderPlot({
      ggplot(filteredGaDeviceCategory()) +
        geom_area(aes(x = Date, y = Sessions, fill = deviceCategory), position = "stack") +
        theme_minimal() +
        theme(legend.position = "bottom")
    })
  })
  
  observeEvent(input$plotChannelPerformance,{
    channelPerformance <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                           metrics = c('transactionRevenue','revenuePerTransaction','transactionsPerSession'),
                                           dimensions = 'channelGrouping',
                                           anti_sample = TRUE)
    
    output$channelPerformancePlot <- renderPlotly({
      plot_ly(data = channelPerformance, x = ~transactionsPerSession, y = ~revenuePerTransaction, size = ~transactionRevenue,
              text = ~paste("Channel: ", channelGrouping, "<br>Revenue: ", transactionRevenue,
                            "<br>AvgOrderValue: ", revenuePerTransaction, "<br>ConversionRate: ", transactionsPerSession))
    })
  })
  
  observeEvent(input$plotPagePerformance,{
    pagesPerformance <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                         metrics = c('pageviews','avgTimeOnPage','pageValue'),
                                         dimensions = 'pagePath',
                                         anti_sample = TRUE)
    
    output$pagePerformancePlot <- renderPlotly({
      plot_ly(data = pagesPerformance, x = ~log(avgTimeOnPage), y = ~log(pageValue), size = ~pageviews,
              text = ~paste("Page: ", pagePath, "<br>Pageviews: ", pageviews,
                            "<br>AvgTimeOnPage: ", avgTimeOnPage, "<br>PageValue: ", pageValue))
    })
  })
  
  observeEvent(input$plotLpPerformance,{
    landingPagesPerformance <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                                metrics = c('sessions','avgSessionDuration','transactionsPerSession'),
                                                dimensions = 'landingPagePath',
                                                anti_sample = TRUE)
    
    output$lpPerformancePlot <- renderPlotly({
      plot_ly(data = landingPagesPerformance, x = ~log(avgSessionDuration), y = ~log(transactionsPerSession), size = ~sessions,
              text = ~paste("LandingPage: ", landingPagePath, "<br>Sessions: ", sessions,
                            "<br>AvgSessionDuration: ", avgSessionDuration, "<br>ConversionRate: ", transactionsPerSession))
    })
  })
  
  observeEvent(input$plotEcomPerformance,{
    ecommerceOverview <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                          metrics = c('transactions','transactionsPerSession','transactionRevenue',
                                                      'revenuePerTransaction'),
                                          dimensions = 'date',
                                          anti_sample = TRUE)
    
    names(ecommerceOverview) <- c('Date','Transactions','ConversionRate','Revenue','AvgOrderValue')
    
    longEcommerceOverview <- ecommerceOverview %>% gather(metric, value, Transactions:AvgOrderValue)
    
    output$ecomPerformancePlot <- renderPlot({
      ggplot(longEcommerceOverview, aes(x = Date, y = value)) +
        geom_line() +
        theme_minimal() +
        facet_grid(metric~., scales = "free_y")
    })
  })
  
  observeEvent(input$plotSessionCount,{
    sessionCount <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                     metrics = c('users','pageviews'),
                                     dimensions = 'sessionCount',
                                     anti_sample = TRUE)
    
    sessionCount$sessionCount <- as.numeric(sessionCount$sessionCount)
    sessionCount$bucket <- NA
    
    sessionCount[sessionCount$sessionCount == 1,"bucket"] <- '1'
    sessionCount[sessionCount$sessionCount == 2,"bucket"] <- '2'
    sessionCount[sessionCount$sessionCount == 3,"bucket"] <- '3'
    sessionCount[sessionCount$sessionCount == 4,"bucket"] <- '4'
    sessionCount[sessionCount$sessionCount == 5,"bucket"] <- '5'
    sessionCount[sessionCount$sessionCount == 6,"bucket"] <- '6'
    sessionCount[sessionCount$sessionCount == 7,"bucket"] <- '7'
    sessionCount[sessionCount$sessionCount == 8,"bucket"] <- '8'
    sessionCount[sessionCount$sessionCount == 9,"bucket"] <- '9'
    sessionCount[sessionCount$sessionCount > 9 & sessionCount$sessionCount < 26,"bucket"] <- '10 - 25'
    sessionCount[sessionCount$sessionCount > 25 & sessionCount$sessionCount < 51,"bucket"] <- '26 - 50'
    sessionCount[sessionCount$sessionCount > 50 & sessionCount$sessionCount < 101,"bucket"] <- '51 - 100'
    sessionCount[sessionCount$sessionCount > 100,"bucket"] <- '> 100'
    
    sessionCountGrouped <- sessionCount %>% group_by(bucket) %>% summarize(Users = sum(users), Pageviews = sum(pageviews)) %>%
      gather(metric, value, Users:Pageviews)
    
    sessionCountGrouped$bucket <- factor(sessionCountGrouped$bucket, levels = c('1','2','3','4','5','6','7','8','9',
                                                                                '10 - 25','26 - 50','51 - 100','> 100'))
    
    output$sessionCountPlot <- renderPlot({
      ggplot(sessionCountGrouped, aes(x = bucket, y = value, fill = metric)) +
        geom_col(position='dodge') +
        scale_y_continuous(labels=comma) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        coord_flip()
    })
  })
  
  observeEvent(input$plotDaysSinceLastSession,{
    daysSinceLastSession <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                             metrics = c('users','pageviews','sessions'),
                                             dimensions = 'daysSinceLastSession',
                                             anti_sample = TRUE)
    
    daysSinceLastSession$daysSinceLastSession <- as.numeric(daysSinceLastSession$daysSinceLastSession)
    daysSinceLastSession$bucket <- NA
    
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 0,"bucket"] <- '0'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 1,"bucket"] <- '1'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 2,"bucket"] <- '2'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 3,"bucket"] <- '3'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 4,"bucket"] <- '4'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 5,"bucket"] <- '5'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 6,"bucket"] <- '6'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 7,"bucket"] <- '7'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 8,"bucket"] <- '8'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession == 9,"bucket"] <- '9'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession > 9 & daysSinceLastSession$daysSinceLastSession < 26,"bucket"] <- '10 - 25'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession > 25 & daysSinceLastSession$daysSinceLastSession < 51,"bucket"] <- '26 - 50'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession > 50 & daysSinceLastSession$daysSinceLastSession < 101,"bucket"] <- '51 - 100'
    daysSinceLastSession[daysSinceLastSession$daysSinceLastSession > 100,"bucket"] <- '> 100'
    
    daysSinceLastSessionGrouped <- daysSinceLastSession %>% group_by(bucket) %>%
      summarize(Users = sum(users), Pageviews = sum(pageviews), Sessions = sum(sessions)) %>%
      gather(metric, value, Users:Sessions)
    
    daysSinceLastSessionGrouped$bucket <- factor(daysSinceLastSessionGrouped$bucket,
                                                 levels = c('0','1','2','3','4','5','6','7','8','9','10 - 25','26 - 50','51 - 100','> 100'))
    
    output$daysSinceLastSessionPlot <- renderPlot({
      ggplot(daysSinceLastSessionGrouped, aes(x = bucket, y = value, fill = metric)) +
        geom_col(position='dodge') +
        scale_y_continuous(labels=comma) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        coord_flip()
    })
  })
  
  observeEvent(input$plotSessionDuration,{
    sessionDuration <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                        metrics = c('users','pageviews','sessions'),
                                        dimensions = 'sessionDurationBucket',
                                        anti_sample = TRUE)
    
    sessionDuration$sessionDurationBucket <- as.numeric(sessionDuration$sessionDurationBucket)
    sessionDuration$bucket <- NA
    
    sessionDuration[sessionDuration$sessionDurationBucket < 11,"bucket"] <- '0 - 10'
    sessionDuration[sessionDuration$sessionDurationBucket > 10 & sessionDuration$sessionDurationBucket < 31,"bucket"] <- '11 - 30'
    sessionDuration[sessionDuration$sessionDurationBucket > 30 & sessionDuration$sessionDurationBucket < 61,"bucket"] <- '31 - 60'
    sessionDuration[sessionDuration$sessionDurationBucket > 60 & sessionDuration$sessionDurationBucket < 181,"bucket"] <- '61 - 180'
    sessionDuration[sessionDuration$sessionDurationBucket > 180 & sessionDuration$sessionDurationBucket < 601,"bucket"] <- '181 - 600'
    sessionDuration[sessionDuration$sessionDurationBucket > 600 & sessionDuration$sessionDurationBucket < 1801,"bucket"] <- '601 - 1800'
    sessionDuration[sessionDuration$sessionDurationBucket > 1800,"bucket"] <- '> 1800'
    
    sessionDurationGrouped <- sessionDuration %>% group_by(bucket) %>%
      summarize(Users = sum(users), Pageviews = sum(pageviews), Sessions = sum(sessions)) %>%
      gather(metric, value, Users:Sessions)
    
    sessionDurationGrouped$bucket <- factor(sessionDurationGrouped$bucket,
                                            levels = c('0 - 10','11 - 30','31 - 60','61 - 180','181 - 600','601 - 1800','> 1800'))
    
    output$sessionDurationPlot <- renderPlot({
      ggplot(sessionDurationGrouped, aes(x = bucket, y = value, fill = metric)) +
        geom_col(position='dodge') +
        scale_y_continuous(labels=comma) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        coord_flip()
    })
  })
  
  observeEvent(input$plotPageDepth,{
    pageDepth <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                  metrics = c('users','pageviews','sessions'),
                                  dimensions = 'pageDepth',
                                  anti_sample = TRUE)
    
    pageDepth$pageDepth <- as.numeric(pageDepth$pageDepth)
    pageDepth$bucket <- NA
    
    pageDepth[pageDepth$pageDepth == 0,"bucket"] <- '0'
    pageDepth[pageDepth$pageDepth == 1,"bucket"] <- '1'
    pageDepth[pageDepth$pageDepth == 2,"bucket"] <- '2'
    pageDepth[pageDepth$pageDepth == 3,"bucket"] <- '3'
    pageDepth[pageDepth$pageDepth == 4,"bucket"] <- '4'
    pageDepth[pageDepth$pageDepth == 5,"bucket"] <- '5'
    pageDepth[pageDepth$pageDepth == 6,"bucket"] <- '6'
    pageDepth[pageDepth$pageDepth == 7,"bucket"] <- '7'
    pageDepth[pageDepth$pageDepth == 8,"bucket"] <- '8'
    pageDepth[pageDepth$pageDepth == 9,"bucket"] <- '9'
    pageDepth[pageDepth$pageDepth == 10,"bucket"] <- '10'
    pageDepth[pageDepth$pageDepth == 11,"bucket"] <- '11'
    pageDepth[pageDepth$pageDepth == 12,"bucket"] <- '12'
    pageDepth[pageDepth$pageDepth == 13,"bucket"] <- '13'
    pageDepth[pageDepth$pageDepth == 14,"bucket"] <- '14'
    pageDepth[pageDepth$pageDepth == 15,"bucket"] <- '15'
    pageDepth[pageDepth$pageDepth == 16,"bucket"] <- '16'
    pageDepth[pageDepth$pageDepth == 17,"bucket"] <- '17'
    pageDepth[pageDepth$pageDepth == 18,"bucket"] <- '18'
    pageDepth[pageDepth$pageDepth == 19,"bucket"] <- '19'
    pageDepth[pageDepth$pageDepth == 20,"bucket"] <- '20'
    pageDepth[pageDepth$pageDepth > 20,"bucket"] <- '> 20'
    
    pageDepthGrouped <- pageDepth %>% group_by(bucket) %>%
      summarize(Users = sum(users), Pageviews = sum(pageviews), Sessions = sum(sessions)) %>%
      gather(metric, value, Users:Sessions)
    
    pageDepthGrouped$bucket <- factor(pageDepthGrouped$bucket,
                                      levels = c('0','1', '2', '3', '4', '5', '6', '7', '8', '9', '10',
                                                 '11', '12', '13', '14', '15', '16', '17', '18', '19', '20','> 20'))
    
    output$pageDepthPlot <- renderPlot({
      ggplot(pageDepthGrouped, aes(x = bucket, y = value, fill = metric)) +
        geom_col(position='dodge') +
        scale_y_continuous(labels=comma) +
        theme_minimal() +
        theme(legend.position = "bottom") +
        coord_flip()
    })
  })
  
  observeEvent(input$plotGaCorplot,{
    gaCorrelationData <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                          metrics = c('sessions','users','sessionsPerUser','pageviewsPerSession', 'avgSessionDuration',
                                                      'bounceRate','transactions', 'transactionRevenue',
                                                      'revenuePerTransaction','transactionsPerSession'),
                                          dimensions = 'date',
                                          anti_sample = TRUE)
    
    names(gaCorrelationData) <- c('Date','Sessions','Users','SessionsPerUser','PagesPerSession',
                                  'AvgSessionDuration','BounceRate','Transactions','Revenue','AvgOrderValue','ConversionRate')
    
    corr <- cor(gaCorrelationData[,-1])
    
    output$gaCorPlot <- renderPlot(corrplot(corr, method = "pie", type = "upper"))
  })
  
  observeEvent(input$plotCityPerformance,{
    cityPerformance <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                        metrics = c('pageviews','sessions','bounceRate','pageviewsPerSession',
                                                    'transactionRevenue','transactionsPerSession'),
                                        dimensions = c('city','longitude','latitude'),
                                        anti_sample = TRUE)
    
    cityPerformance$latitude <- as.numeric(cityPerformance$latitude)
    cityPerformance$longitude <- as.numeric(cityPerformance$longitude)
    
    ireland <- get_map('Athlone', zoom=7)
    
    output$pageviewsGgMap <- renderPlot({
      ggmap(ireland) + geom_point(data = cityPerformance, aes(x = longitude, y = latitude, size = pageviews),
                                  color = 'blue', alpha = 0.5) +
        theme(legend.position = "bottom") +
        scale_size_continuous(labels=comma)
    })
    
    output$sessionsGgMap <- renderPlot({
      ggmap(ireland) + geom_point(data = cityPerformance, aes(x = longitude, y = latitude, size = sessions),
                                  color = 'blue', alpha = 0.5) +
        theme(legend.position = "bottom") +
        scale_size_continuous(labels=comma)
    })
    
    output$bounceRateGgMap <- renderPlot({
      ggmap(ireland) + geom_point(data = cityPerformance, aes(x = longitude, y = latitude, size = bounceRate),
                                  color = 'blue', alpha = 0.5) +
        theme(legend.position = "bottom") +
        scale_size_continuous(labels=comma)
    })
    
    output$pagesPerSessionGgMap <- renderPlot({
      ggmap(ireland) + geom_point(data = cityPerformance, aes(x = longitude, y = latitude, size = pageviewsPerSession),
                                  color = 'blue', alpha = 0.5) +
        theme(legend.position = "bottom") +
        scale_size_continuous(labels=comma)
    })
    
    output$revenueGgMap <- renderPlot({
      ggmap(ireland) + geom_point(data = cityPerformance, aes(x = longitude, y = latitude, size = transactionRevenue),
                                  color = 'blue', alpha = 0.5) +
        theme(legend.position = "bottom") +
        scale_size_continuous(labels=comma)
    })
    
    output$crGgMap <- renderPlot({
      ggmap(ireland) + geom_point(data = cityPerformance, aes(x = longitude, y = latitude, size = transactionsPerSession),
                                  color = 'blue', alpha = 0.5) +
        theme(legend.position = "bottom") +
        scale_size_continuous(labels=comma)
    })
  })
  
  observeEvent(input$plotEventPerformance,{
    eventPerformance <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                         metrics = c('totalEvents','sessionsWithEvent'),
                                         dimensions = c('date','eventCategory'),
                                         anti_sample = TRUE)
    
    output$totalEventsPlot <- renderPlot({
      ggplot(eventPerformance, aes(x = date, y = totalEvents, color = eventCategory)) + geom_line() +
        theme_minimal() +
        theme(legend.position = "bottom") 
    })
    
    output$sessionsWithEventsPlot <- renderPlot({
      ggplot(eventPerformance, aes(x = date, y = sessionsWithEvent, color = eventCategory)) + geom_line() +
        theme_minimal() +
        theme(legend.position = "bottom") 
    })
  })
  
  observeEvent(input$plotGoalPerformance,{
    goalPerformance <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                        metrics = c('goal1Completions','goal2Completions','goal1ConversionRate','goal2ConversionRate'),
                                        dimensions = 'date',
                                        anti_sample = TRUE)
    
    goalCompletions <- goalPerformance %>% gather(goal, completions, 2:3)
    goalConversionRate <- goalPerformance %>% gather(goal, conversionRates, 4:5)
    
    output$goalCompletionsPlot <- renderPlot({
      ggplot(goalCompletions, aes(x = date, y = completions, color = goal)) + geom_line() +
        theme_minimal() +
        theme(legend.position = "bottom") 
    })
    
    output$goalConversionRatesPlot <- renderPlot({
      ggplot(goalConversionRate, aes(x = date, y = conversionRates, color = goal)) + geom_line() +
        theme_minimal() +
        theme(legend.position = "bottom") 
    })
  })
  
  observeEvent(input$plotProductPerformance,{
    productPerformance <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                           metrics = c('itemRevenue','revenuePerItem','itemsPerPurchase','itemQuantity'),
                                           dimensions = c('productSku','productName'),
                                           anti_sample = TRUE)
    
    output$productPerformancePlot <- renderPlotly({
      plot_ly(data = productPerformance, x = ~log(revenuePerItem), y = ~log(itemsPerPurchase), size = ~get(input$sizeMetric),
              text = ~paste("Product: ", productName, "<br>SKU: ", productSku, "<br>Revenue: ", itemRevenue,
                            "<br>Avg. Price: ", revenuePerItem, "<br>Avg. Quantity: ", itemsPerPurchase))
    })
  })
  
  observeEvent(input$plotGaAnomalies,{
    anomalyGaData <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                      metrics = c('sessions','pageviews','users','transactions', 'bounceRate','avgSessionDuration',
                                                  'pageviewsPerSession','transactionsPerSession','revenuePerTransaction', 'transactionRevenue'),
                                      dimensions = 'dateHour',
                                      anti_sample = TRUE)
    
    anomalyGaData <- anomalyGaData %>% mutate(DateHour = ymd_hms(paste0(anomalyGaData$dateHour,"0000")))
    
    anomalyGaData <- anomalyGaData %>% arrange(DateHour)
    
    anomSessions <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","sessions")], direction='both',
                                       plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Sessions")
    output$sessionAnomalies <- renderPlot(anomSessions$plot)
    
    anomPageviews <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","pageviews")], direction='both',
                                        plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Pageviews")
    output$pageviewAnomalies <- renderPlot(anomPageviews$plot)
    
    anomUsers <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","users")], direction='both',
                                    plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Users")
    output$userAnomalies <- renderPlot(anomUsers$plot)
    
    anomBounceRate <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","bounceRate")], direction='both',
                                         plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Bounce Rate")
    output$bounceRateAnomalies <- renderPlot(anomBounceRate$plot)
    
    anomAvgSessionDuration <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","avgSessionDuration")], direction='both',
                                                 plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Avg Session Duration")
    output$avgSessionDurationAnomalies <- renderPlot(anomAvgSessionDuration$plot)
    
    anomPagesPerSession <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","pageviewsPerSession")], direction='both',
                                              plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Pages Per Session")
    output$pagesPerSessionAnomalies <- renderPlot(anomPagesPerSession$plot)
    
    anomTransactions <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","transactions")], direction='both',
                                           plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Transactions")
    output$transactionAnomalies <- renderPlot(anomTransactions$plot)
    
    anomEcomConversionRate <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","transactionsPerSession")], direction='both',
                                                 plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Conversion Rate")
    output$ecomConversionRateAnomalies <- renderPlot(anomEcomConversionRate$plot)
    
    anomRevenue <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","transactionRevenue")], direction='both',
                                      plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "Revenue")
    output$revenueAnomalies <- renderPlot(anomRevenue$plot)
    
    anomAvgOrderValue <- AnomalyDetectionTs(anomalyGaData[,c("DateHour","revenuePerTransaction")], direction='both',
                                            plot=TRUE, e_value=TRUE, max_anoms=0.01, ylabel = "AOV")
    output$avgOrderValueAnomalies <- renderPlot(anomAvgOrderValue$plot)
  })
  
  observeEvent(input$plotSearchTerms,{
    searchTerms <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                    metrics = c('searchUniques','avgSearchDuration','avgSearchDepth'),
                                    dimensions = c('searchKeyword','searchKeywordRefinement'),
                                    anti_sample = TRUE)
    
    set.seed(1234)
    
    output$uniqueSearches <- renderPlot({
      wordcloud(words = searchTerms$searchKeyword, freq = searchTerms$searchUniques, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$uniqueSearchesRefinement <- renderPlot({
      wordcloud(words = searchTerms$searchKeywordRefinement, freq = searchTerms$searchUniques, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$avgSearchDepth <- renderPlot({
      wordcloud(words = searchTerms$searchKeyword, freq = searchTerms$avgSearchDepth, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$avgSearchDepthRefinement <- renderPlot({
      wordcloud(words = searchTerms$searchKeywordRefinement, freq = searchTerms$avgSearchDepth, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$avgSearchDuration <- renderPlot({
      wordcloud(words = searchTerms$searchKeyword, freq = searchTerms$avgSearchDuration, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
    
    output$avgSearchDurationRefinement <- renderPlot({
      wordcloud(words = searchTerms$searchKeywordRefinement, freq = searchTerms$avgSearchDuration, min.freq = 1,
                max.words=200, random.order=FALSE, rot.per=0.35,
                colors=brewer.pal(8, "Dark2"))
    })
  })
  
  observeEvent(input$plotUniqeParams,{
    pagesPerformance <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                         metrics = c('pageviews','avgTimeOnPage','pageValue'),
                                         dimensions = 'pagePath',
                                         anti_sample = TRUE)

    uniqueParams <- pagesPerformance[str_detect(pagesPerformance$pagePath, "\\?"),]$pagePath %>% 
      str_replace_all(".*\\?", "") %>% str_split("&") %>% unlist() %>% str_replace_all("=.*", "") %>% unique()
    
    output$uniqueParamsPlot <- renderPrint({
      print(uniqueParams)
    })
  })
  
  observeEvent(input$plotConversionQualityIndex,{
    conversionQuality <- google_analytics(selectedId(), date_range = c(input$dateRange[1],input$dateRange[2]),
                                          metrics = c("sessions", "transactions"),
                                          dimensions = c("date","medium"),
                                          anti_sample = TRUE)
    
    conversionQuality <- conversionQuality %>% 
      mutate(conversionQualityIndex = ((transactions/sum(transactions))*100)/((sessions/sum(sessions))*100))
    
    output$conversionQualityIndexPlot <- renderPlotly({
      p <- ggplot(conversionQuality, aes(x = date, y = conversionQualityIndex, color = medium)) +
        geom_line() +
        theme_minimal()
      
      ggplotly(p)
      
  })
})
  
  observeEvent(input$plotExampleScatterplot,{
    req(input$file1)
    
    df <- read.csv2(input$file1$datapath, header = TRUE, sep = ",") 
    
    output$exampleScatterplot <- renderPlot({
      p <- ggplot(df, aes(as.numeric(x),as.numeric(y))) + theme_minimal()
      
      p <- p + geom_point()
      
      if(input$addSmooth){
        p <- p + geom_smooth()
      }
      
      print(p)
      
    })
  })
}
