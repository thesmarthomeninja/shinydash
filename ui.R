source('global.R')
ui <- fluidPage(
    useShinyjs(),
    theme = "bootstrap.css",

    navbarPage("W O L F",
               tabPanel("Home"),
               tabPanel("Google AdWords",
                        actionButton("fullScreenToggle", "", icon=icon("compress")),
                        sidebarLayout(
                            sidebarPanel(id="sidebar"),
                            mainPanel(
                                img(src="exampleimg.jpg")
                            )
                        )),
               tabPanel("Google Analytics"),
               tabPanel("Facebook Ads"),
               tabPanel("Twitter Ads"),
               tabPanel("LinkedIn Ads"),
               tabPanel("Moz")
               )

)
