source('global.R')
server <- function(input, output, session) {

    analyticsAccessToken  <- callModule(googleAuth, "gaLogin")
    adwordsAccessToken <- doAuth()

    account_list <- reactive({
        with_shiny(ga_account_list,
                   shiny_access_token = analyticsAccessToken())
    })

    observeEvent(input$fullScreenToggle, {
        toggle("sidebar")
    })

}
