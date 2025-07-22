
# global settings -->
library(shiny)
library(rvest)
library(httr)

# end of global settings

fluidPage(

    titlePanel(" "),

    sidebarLayout(
        sidebarPanel(
            shiny::textInput("url",
                             label = "Website URL",
                             value = "https://",
                             placeholder = "https://247sports.com/college/arizona/Season/2026-Football/Commits/"),
            actionButton("go_button",
                         label = "Evaluate URL")
        ),
        

        mainPanel(
            textOutput("url"),
            textOutput('url_valid')
        )
    )
)
