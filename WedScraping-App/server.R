

# Define server logic required to draw a histogram
function(input, output, session) {

  
  observeEvent(input$go_button, {
    
    output$url <- renderText({
      go = 1 # start process->
      message = "" # no message to start
      
      url_input <- input$url
      # check if valid url start...
      if (substr(url_input,1,12) != "https://www.") {
        go <- 0 # stop process
        message <- paste0("URL must start with 'https://www.' to be valid")
      }
      
      # if url valid - check length
      if (go == 1 && nchar(url_input) <= 12) {
        go <- 0 # stop process
        message <- paste0("nothing after 'https://www.'")
      }
      
      # if still good - update output, else add message ->
      if (go == 1 && nchar(url_input) > 12) {
        url_check <- url_input
      } else {
        url_check <- paste0("URL not valid: ",message)
      }
      
      url_check
    })
    
  })
    

}
