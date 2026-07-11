##testing
library(httr)
library(rvest)
library(stringr)
library(dplyr)

# https://snre.arizona.edu/research
# https://envs.arizona.edu/research

url_input <- "https://infosci.arizona.edu/research-faculty/faculty"

res <- RETRY(
  "GET", url_input,
  timeout(10),
  times = 3,
  pause_base = 3,
  pause_cap = 5,
  terminate_on = c(404)
)

status_code(res)

# Parse response as HTML
page <- read_html(res)

# Extract the state value
card <- page %>% 
  html_nodes(".card-body") %>% 
  html_text(trim = TRUE)

# Extract meaningful parts
extract_profile <- function(text) {
  lines <- str_split(text, "\n")[[1]] %>% str_squish()
  lines <- lines[lines != ""]  # remove empty lines
  
  # Heuristic extraction
  name <- lines[1]
  email <- tail(lines[str_detect(lines, "@")], 1)
  title <- lines[2]
  interests <- lines[str_detect(lines, "Learning|climate|data|Evolutionary|biology|Bioinformatics|natural|resources|managment|fauna|flora|analytics|statistics|visualization|web|scraping|mining")]

  list(
    name = name,
    title = title,
    interests = paste(interests, collapse = ", "),
    email = email
  )
}

profiles <- lapply(card, extract_profile)

profiles_df <- bind_rows(profiles)
View(profiles_df)



## would you be open to chatting about programming and/or data visualizations you are doing in your lab?
## make interactive shiny apps for your work.. allow me to use some of the code on my portfolio...
