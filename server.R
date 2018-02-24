server <- function(input, output, session) {

    access_token  <- callModule(googleAuth, "auth1")

    gadata <- reactive({

        with_shiny(google_analytics,
                   "62546110",
                   date_range=c("2018-01-01", "2018-01-22"),
                   metrics = c("sessions", "bounceRate"),
                   dimensions = c("source", "medium"),
                   shiny_access_token = access_token())
    })


    output$something <- renderTable({

        req(access_token())

        gadata <- gadata()

        gadata

    })

}
