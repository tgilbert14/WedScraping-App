
# global settings -->
library(shiny)

# end of global settings

fluidPage(

    titlePanel(" "),

    sidebarLayout(
        sidebarPanel(
            shiny::textInput("url",
                             label = "Website URL",
                             value = "https://www.",
                             placeholder = "https://247sports.com/college/arizona/Season/2026-Football/Commits/"),
            actionButton("go_button",
                         label = "Evaluate URL")
        ),
        

        mainPanel(
            textOutput("url")
        )
    )
)
