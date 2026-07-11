# ============================================================================
# global.R - Web Scraper Studio
# Libraries, constants, theme. Engine functions live in R/ (auto-sourced).
# ============================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(shinyjs)
  library(rvest)
  library(xml2)
  library(httr)
  library(dplyr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(DT)
  # NOTE: jsonlite is used fully-qualified (jsonlite::) and deliberately NOT
  # attached - library(jsonlite) would mask shiny::validate().
  library(htmltools)
  library(shinycssloaders)
})

HAS_WRITEXL  <- requireNamespace("writexl",  quietly = TRUE)
HAS_CHROMOTE <- requireNamespace("chromote", quietly = TRUE)

# Engine functions. Shiny can auto-source R/, but not in every launch mode -
# source explicitly so the app works everywhere (double-sourcing is harmless).
source("R/scraper_engine.R", local = FALSE)
source("R/recipes.R",        local = FALSE)

APP_TITLE <- "Web Scraper Studio"

PRESETS <- c(
  "Try an example page..." = "",
  "247Sports - Arizona 2026 commits (recipe + cards)" =
    "https://247sports.com/college/arizona/Season/2026-Football/Commits/",
  "247Sports - 2026 team class rankings (recipe)" =
    "https://247sports.com/Season/2026-Football/CompositeTeamRankings/",
  "247Sports - 2026 top recruits (recipe)" =
    "https://247sports.com/Season/2026-Football/CompositeRecruitRankings/",
  "247Sports - 2026 transfer portal (recipe)" =
    "https://247sports.com/Season/2026-Football/TransferPortal/",
  "On3 - Arizona 2026 commits (embedded JSON)" =
    "https://www.on3.com/college/arizona-wildcats/football/2026/commits/",
  "Wikipedia - US state populations (tables)" =
    "https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population",
  "Books to Scrape - demo store (cards)" =
    "https://books.toscrape.com/",
  "Quotes to Scrape - demo site (cards)" =
    "https://quotes.toscrape.com/",
  "UA InfoSci - faculty directory (cards)" =
    "https://infosci.arizona.edu/research-faculty/faculty"
)

APP_THEME <- bslib::bs_theme(
  version    = 5,
  primary    = "#0f766e",
  secondary  = "#475569",
  success    = "#15803d",
  warning    = "#b45309",
  danger     = "#b91c1c",
  "border-radius" = "0.5rem"
)

# ---------------------------------------------------------------------------
# Discovery bundle (composes the engine's discover_* functions)
# ---------------------------------------------------------------------------

discover_all <- function(page, base_url) {
  list(
    tables   = discover_tables(page),
    cards    = discover_cards(page),
    lists    = discover_lists(page),
    links    = scrape_links(page, base_url),
    images   = scrape_images(page, base_url),
    headings = scrape_headings(page),
    json     = discover_json(page)
  )
}

discover_all_empty <- function() {
  list(
    tables   = empty_tables_tbl(),
    cards    = empty_cards_tbl(),
    lists    = tibble::tibble(index = integer(), tag = character(),
                              items = integer(), context = character(),
                              sample = character()),
    links    = tibble::tibble(text = character(), href = character(),
                              url = character(), internal = logical()),
    images   = tibble::tibble(alt = character(), src = character(),
                              url = character()),
    headings = tibble::tibble(level = character(), text = character()),
    json     = list()
  )
}

# ---------------------------------------------------------------------------
# Small display helpers shared by server outputs
# ---------------------------------------------------------------------------

dt_table <- function(df, page_len = 10) {
  DT::datatable(
    df,
    rownames = FALSE,
    class    = "compact stripe hover",
    options  = list(
      pageLength   = page_len,
      scrollX      = TRUE,
      lengthChange = FALSE,
      searchHighlight = TRUE
    )
  )
}

# Cap huge frames for responsive previews (full data is kept for download)
cap_preview <- function(df, n = 1500) {
  if (nrow(df) > n) utils::head(df, n) else df
}

# Shorten a long "a > b > c" JSON path label from the left
shorten_path <- function(x, width = 70) {
  ifelse(nchar(x) > width, paste0("...", substr(x, nchar(x) - width + 4, nchar(x))), x)
}

spinner <- function(ui_element) {
  shinycssloaders::withSpinner(ui_element, type = 7, color = "#0f766e",
                               proxy.height = "220px")
}
