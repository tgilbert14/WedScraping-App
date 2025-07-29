function(input, output, session) {

  res_status <- reactiveVal("Waiting for input...")
  res_data <- reactiveVal(NULL)
  
  observeEvent(input$go_button, {
    res_status("Checking URL...")
    
    # Insert new UI after the original
    insertUI(
      selector = "#ui_initial",
      where = "afterBegin",
      ui = tags$div(id = "new_ui", class = "new-ui",
                    textInput("parse_key", label = "What node do you want to parse?", placeholder = "href, text, node"),
                    actionButton("go_button2", label = "Evaluate New URL"),
      )
    )
    
    # Hide the original UI
    hide("ui_initial")

    url_input <- input$url
    go <- TRUE
    message <- ""
    
    if (substr(url_input, 1, 8) != "https://") {
      go <- FALSE
      message <- "URL must start with 'https://'"
    }
    
    if (go && nchar(url_input) <= 8) {
      go <- FALSE
      message <- "URL has no content after 'https://'"
    }
    
    if (go) {
      tryCatch({
        res <- RETRY(
          "GET", url_input,
          timeout(10),
          times = 3,
          pause_base = 3,
          pause_cap = 5,
          terminate_on = c(404)
        )
        res_data(res)  # save for downstream use
        res_status(paste("Status Code:", status_code(res)))
      }, error = function(e) {
        res_status(paste("Request failed:", e$message))
        res_data(NULL)
      })
      
      # if valid - make box green -->
      if (res_status() == "Status Code: 200") {
        # Insert new UI after the original
        insertUI(
          selector = "#box-1",
          where = "afterEnd",
          ui = tags$div(
            br(),
            div(id="box-1-goButtonPush-TRUE",
                tags$div(class = "output-box-1-goButtonPush-TRUE",
                         h4("URL Valid!"),
                         textOutput("url_2"),
                         br(),
                         actionButton("ok", "  Get HTML", width = "100%"))),
          )
        )
        # Hide the original UI
        hide("box-1")
      } else {
        # if not valid - make box red -->
        insertUI(
          selector = "#box-1",
          where = "afterEnd",
          ui = tags$div(
            br(),
            div(id="box-1-goButtonPush-FALSE",
                tags$div(class = "output-box-1-goButtonPush-FALSE",
                         h4("URL Not Valid!"),
                         textOutput("url_2"),
                         br(),
                         actionButton("reset", "  Reset", width = "100%"))),
          )
        )
        # Hide the original UI
        hide("box-1")
      } # end of else statement
      
      res_status()
    }
    
    # for UI status output -->
    output$url <- renderText({
      res_status()
    })
    output$url_2 <- renderText({
      res_status()
    })
    
    # output$raw_html <- renderText({
    #   req(res_data())  # ensures it's non-NULL
    #   page <- read_html(res_data())
    #   as.character(page)
    # })
    
  }) # end of go button
  
  observeEvent(input$go_button2, {
    req(res_data())
    page <- read_html(res_data())
    
    # Find all player name nodes
    names <- page %>%
      html_nodes(".name") %>%
      html_text(trim = TRUE)
    
    output$parsed_names <- renderPrint({
      names
    })
  })
  
}