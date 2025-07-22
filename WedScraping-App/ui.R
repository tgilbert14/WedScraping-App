
# global settings
library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel(" "),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            shiny::textInput("url",
                             label = "Website URL",
                             value = "https://www.",
                             placeholder = "https://247sports.com/college/arizona/Season/2026-Football/Commits/"),
            actionButton("go_button",
                         label = "Evaluate URL")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            textOutput("url")
        )
    )
)
