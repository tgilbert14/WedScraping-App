
# global settings -->
library(shiny)
library(rvest)
library(httr)
library(shinyjs)

# end of global settings

fluidPage(
  includeCSS("www/styles.css"),
  useShinyjs(),

    titlePanel("Interactive Web Scraper"),
  
    sidebarLayout(
      div(id="ui_initial",
        sidebarPanel(width = "100%",
          tags$div(class = "sidebar-panel",
            textInput("url",
                             label = "⬇️ Enter Website Below ⬇️",
                             value = "https://247sports.com/college/arizona/Season/2026-Football/Commits/",
                             placeholder = "https://247sports.com/college/arizona/Season/2026-Football/Commits/"),
            actionButton("go_button",
                         label = "Evaluate URL")
          ))
        ), # end of div for UI
        
      
      
        # main pane view -->
        mainPanel(

          tags$div(class = "flex-box",

            div(id="box-1",
            tags$div(class = "output-box-1",
              h4("⬅️Check if URL is Valid..."),
              textOutput("url")
              )),

            # div(id="box-2",
            # tags$div(class = "output-box-2",
            #   h4("HTML"),
            #   verbatimTextOutput("raw_html")
            #   ))
            
            ) # end of tags$div
        ) # end of main panel view
      
    )
)
