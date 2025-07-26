##testing
library(httr)
library(rvest)
library(stringr)

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

card[1]




# subject <- page %>% 
#   html_nodes("#views-bootstrap-az-person-grid-block-3cols .field--label-hidden") %>% 
#   html_text(trim = TRUE)
# subject

card %>% 
  str_replace_all("\n\n", "\n") %>%    # Normalize line breaks
  str_squish() %>%                   # Remove excessive whitespace
  str_split_fixed(" ", n = 200)

# Clean text and split into components
cleaned <- card %>% 
  str_replace_all("\n", "\n") %>%    # Normalize line breaks
  str_squish() %>%                   # Remove excessive whitespace
  str_split_fixed(" ", n = 200)      # Break into chunks

# Extract meaningful parts
extract_profile <- function(text) {
  lines <- str_split(text, "\n")[[1]] %>% str_squish()
  lines <- lines[lines != ""]  # remove empty lines
  
  # Heuristic extraction
  name <- lines[1]
  email <- tail(lines[str_detect(lines, "@")], 1)
  title <- lines[2]
  interests <- lines[str_detect(lines, "Learning|intelligence|Cybersecurity")]
  
  list(
    name = name,
    title = title,
    interests = paste(interests, collapse = ", "),
    email = email
  )
}

profiles <- lapply(raw, extract_profile)
profiles_df <- bind_rows(lapply(profiles, as.data.frame))

