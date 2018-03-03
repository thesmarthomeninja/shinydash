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

    observeEvent(input$adwordsFullScreenToggle, {
        toggle("adwordsSidebar")
    })

    observeEvent(input$gaFullScreenToggle, {
        toggle("gaSidebar")
    })

    output$table <- renderTable({
        req(account_list())
        account_list()
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
}
