function(input, output, session) {
  
  res_status <- reactiveVal("Waiting for input...")
  
  observeEvent(input$go_button, {
    
    res_status("Checking URL...")
    
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
        res_status(paste("Status Code:", status_code(res)))
      }, error = function(e) {
        res_status(paste("Request failed:", e$message))
      })
    }
    
  })
  
  output$url <- renderText({
    res_status()
  })
}