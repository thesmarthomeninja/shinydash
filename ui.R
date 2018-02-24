source('global.R')
ui <- fluidPage(

    titlePanel("Hello Shiny!"),

    sidebarLayout(

        sidebarPanel(

            googleAuthUI("auth1")

        ),

        mainPanel(
            tableOutput("something")
        )
    )
)
