library(shiny)

ui <- fluidPage(
  titlePanel("Embedded Webpage Example"),
  mainPanel(
    tags$iframe(src = "http://www.247sports.com", 
                width = "800px", 
                height = "600px")
  )
)

server <- function(input, output, session) {
  # No server-side logic needed for simple iframe embedding
}

shinyApp(ui = ui, server = server)