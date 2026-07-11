# ============================================================================
# ui.R - Web Scraper Studio
# Point -> Analyze -> Pick -> Data frame -> Download
# ============================================================================

# ---- tab bodies -------------------------------------------------------------

tables_tab <- tagList(
  layout_columns(
    col_widths = c(4, 8),
    card(
      class = "control-card",
      card_header(tagList(icon("table"), " Choose a table")),
      card_body(
        selectInput("table_pick", NULL, choices = c("No tables found yet" = ""),
                    selectize = FALSE, width = "100%"),
        checkboxInput("table_promote", "Use first row as column names", FALSE),
        uiOutput("table_meta"),
        actionButton("table_add", tagList(icon("plus"), " Add to My Data"),
                     class = "btn-success w-100 mt-2")
      )
    ),
    card(
      card_header("Preview"),
      card_body(spinner(DT::DTOutput("table_dt")))
    )
  ),
  accordion(
    open = FALSE, class = "code-accordion",
    accordion_panel(
      title = tagList(icon("code"), " R code for this extraction"),
      value = "acc_table_code",
      verbatimTextOutput("table_code")
    )
  )
)

cards_tab <- tagList(
  layout_columns(
    col_widths = c(4, 8),
    card(
      class = "control-card",
      card_header(tagList(icon("layer-group"), " Repeated element groups")),
      card_body(
        p(class = "hint-text",
          "Groups of elements that repeat with the same structure - player rows, product cards, people listings. Each repeated child class becomes a column."),
        selectInput("card_pick", NULL, choices = c("Analyze a site first" = ""),
                    selectize = FALSE, width = "100%"),
        uiOutput("card_meta"),
        checkboxGroupInput("card_fields", "Columns to extract", choices = character(0)),
        div(class = "extras-row",
            checkboxInput("card_link",     "Include first link in each item", TRUE),
            checkboxInput("card_image",    "Include first image in each item", FALSE),
            checkboxInput("card_fulltext", "Include full item text", FALSE)
        ),
        uiOutput("card_pages_hint"),
        actionButton("card_add", tagList(icon("plus"), " Add to My Data"),
                     class = "btn-success w-100 mt-2")
      )
    ),
    card(
      card_header("Preview"),
      card_body(spinner(DT::DTOutput("card_dt")))
    )
  ),
  accordion(
    open = FALSE, class = "code-accordion",
    accordion_panel(
      title = tagList(icon("code"), " R code for this extraction"),
      value = "acc_card_code",
      verbatimTextOutput("card_code")
    )
  )
)

links_tab <- navset_pill(
  id = "links_subtabs",
  header = tags$div(class = "pt-3"),
  nav_panel(
    "Links",
    layout_columns(
      col_widths = c(4, 8),
      card(
        class = "control-card",
        card_body(
          textInput("link_filter", "Filter (text or URL contains)", "",
                    placeholder = "e.g. roster, .pdf, /player/"),
          checkboxInput("link_internal", "Internal links only", FALSE),
          uiOutput("links_meta"),
          actionButton("links_add", tagList(icon("plus"), " Add to My Data"),
                       class = "btn-success w-100 mt-2")
        )
      ),
      card(card_body(spinner(DT::DTOutput("links_dt"))))
    )
  ),
  nav_panel(
    "Images",
    layout_columns(
      col_widths = c(4, 8),
      card(class = "control-card", card_body(
        p(class = "hint-text", "Every image on the page with its alt text and absolute URL."),
        actionButton("images_add", tagList(icon("plus"), " Add to My Data"),
                     class = "btn-success w-100")
      )),
      card(card_body(spinner(DT::DTOutput("images_dt"))))
    )
  ),
  nav_panel(
    "Headings",
    layout_columns(
      col_widths = c(4, 8),
      card(class = "control-card", card_body(
        p(class = "hint-text", "The page outline (h1-h5) - useful for section titles."),
        actionButton("headings_add", tagList(icon("plus"), " Add to My Data"),
                     class = "btn-success w-100")
      )),
      card(card_body(spinner(DT::DTOutput("headings_dt"))))
    )
  ),
  nav_panel(
    "Lists",
    layout_columns(
      col_widths = c(4, 8),
      card(class = "control-card", card_body(
        selectInput("list_pick", "Bullet / numbered lists", choices = c("Analyze a site first" = ""),
                    selectize = FALSE, width = "100%"),
        actionButton("list_add", tagList(icon("plus"), " Add to My Data"),
                     class = "btn-success w-100 mt-2")
      )),
      card(card_body(spinner(DT::DTOutput("list_dt"))))
    )
  )
)

json_tab <- tagList(
  layout_columns(
    col_widths = c(4, 8),
    card(
      class = "control-card",
      card_header(tagList(icon("file-code"), " Embedded page data")),
      card_body(
        p(class = "hint-text",
          "Modern JS sites (Next.js, Nuxt) ship their real data as JSON inside the page - often cleaner than the visible HTML. Structured data (ld+json) also lands here."),
        selectInput("json_pick", "JSON payloads found", choices = c("Analyze a site first" = ""),
                    selectize = FALSE, width = "100%"),
        selectInput("json_table", "Record sets inside this payload",
                    choices = c("Pick a payload first" = ""),
                    selectize = FALSE, width = "100%"),
        actionButton("json_add", tagList(icon("plus"), " Add to My Data"),
                     class = "btn-success w-100 mt-2")
      )
    ),
    card(
      card_header("Preview"),
      card_body(spinner(DT::DTOutput("json_dt")))
    )
  ),
  accordion(
    open = FALSE, class = "code-accordion",
    accordion_panel(
      title = tagList(icon("code"), " R code for this extraction"),
      value = "acc_json_code",
      verbatimTextOutput("json_code")
    )
  )
)

custom_tab <- tagList(
  layout_columns(
    col_widths = c(4, 8),
    card(
      class = "control-card",
      card_header(tagList(icon("crosshairs"), " Selector playground")),
      card_body(
        textInput("custom_selector", "CSS selector", "",
                  placeholder = '.price_color   or   table.wikitable   or   a[href]'),
        uiOutput("custom_count"),
        radioButtons("custom_mode", "What to extract",
                     c("Text" = "text",
                       "Link URLs (href)" = "href",
                       "Image sources (src)" = "src",
                       "Any attribute" = "attr",
                       "Raw HTML" = "html")),
        conditionalPanel(
          "input.custom_mode == 'attr'",
          textInput("custom_attr", "Attribute name", "", placeholder = "data-id")
        ),
        actionButton("custom_add", tagList(icon("plus"), " Add to My Data"),
                     class = "btn-success w-100 mt-2"),
        tags$hr(),
        p(class = "hint-text",
          strong("Cheat sheet: "), tags$code(".class"), " by class, ",
          tags$code("#id"), " by id, ", tags$code("tag"), " by tag, ",
          tags$code("a[href*='player']"), " attribute contains, ",
          tags$code(".row .name"), " nested.")
      )
    ),
    card(
      card_header("Live preview"),
      card_body(spinner(DT::DTOutput("custom_dt")))
    )
  ),
  accordion(
    open = FALSE, class = "code-accordion",
    accordion_panel(
      title = tagList(icon("code"), " R code for this extraction"),
      value = "acc_custom_code",
      verbatimTextOutput("custom_code")
    )
  )
)

data_tab <- tagList(
  layout_columns(
    col_widths = c(4, 8),
    card(
      class = "control-card",
      card_header(tagList(icon("database"), " Datasets")),
      card_body(
        selectInput("data_pick", NULL, choices = c("No datasets yet" = ""),
                    selectize = FALSE, width = "100%"),
        uiOutput("data_meta"),
        tags$label(class = "form-label fw-semibold mt-2", "Clean"),
        div(class = "btn-stack",
            actionButton("data_types", tagList(icon("wand-magic-sparkles"), " Auto-detect column types"),
                         class = "btn-outline-primary btn-sm w-100"),
            actionButton("data_dedupe",   "Remove duplicate rows", class = "btn-outline-secondary btn-sm w-100"),
            actionButton("data_dropcols", "Drop empty columns",    class = "btn-outline-secondary btn-sm w-100"),
            actionButton("data_droprows", "Drop empty rows",       class = "btn-outline-secondary btn-sm w-100")
        ),
        tags$label(class = "form-label fw-semibold mt-3", "Columns (untick to drop, drag to reorder)"),
        div(class = "cols-row",
            selectizeInput("data_cols", NULL, choices = NULL, multiple = TRUE,
                           width = "100%",
                           options = list(plugins = list("remove_button"),
                                          placeholder = "all columns")),
            actionButton("data_cols_apply", "Apply", class = "btn-outline-secondary btn-sm")
        ),
        tags$label(class = "form-label fw-semibold mt-2", "Rename"),
        div(class = "rename-row",
            textInput("data_rename", NULL, "", placeholder = "new_name", width = "100%"),
            actionButton("data_rename_btn", "Rename", class = "btn-outline-secondary btn-sm")
        ),
        tags$hr(),
        radioButtons("dl_format", "Download format",
                     choices = if (HAS_WRITEXL) {
                       c("CSV" = "csv", "Excel" = "xlsx", "TSV" = "tsv",
                         "JSON" = "json", "RDS" = "rds")
                     } else {
                       c("CSV" = "csv", "TSV" = "tsv", "JSON" = "json", "RDS" = "rds")
                     },
                     inline = TRUE),
        downloadButton("dl_dataset", "Download dataset", class = "btn-primary w-100"),
        actionButton("data_copy", tagList(icon("clipboard"), " Copy data (TSV)"),
                     class = "btn-outline-primary btn-sm w-100 mt-2"),
        if (HAS_WRITEXL)
          downloadButton("dl_all", "All datasets (Excel workbook)",
                         class = "btn-outline-primary w-100 mt-2"),
        tags$hr(),
        actionButton("data_delete", tagList(icon("trash"), " Delete dataset"),
                     class = "btn-outline-danger w-100")
      )
    ),
    card(
      card_header(uiOutput("data_title", inline = TRUE)),
      card_body(spinner(DT::DTOutput("data_dt")))
    )
  ),
  accordion(
    open = FALSE, class = "code-accordion",
    accordion_panel(
      title = tagList(icon("code"), " R script to reproduce this dataset"),
      value = "acc_data_code",
      verbatimTextOutput("data_code")
    )
  )
)

history_tab <- card(
  card_header(tagList(icon("clock-rotate-left"), " Session history")),
  card_body(
    DT::DTOutput("history_dt"),
    div(class = "mt-3 d-flex gap-2",
        downloadButton("dl_history", "Download log (CSV)", class = "btn-outline-primary btn-sm"),
        actionButton("history_clear", "Clear log", class = "btn-outline-secondary btn-sm"))
  )
)

# ---- page -------------------------------------------------------------------

page_sidebar(
  title = tags$span(class = "app-title",
                    icon("spider"), tags$span(APP_TITLE),
                    tags$small(class = "app-subtitle",
                               "point - analyze - pick - data frame")),
  window_title = APP_TITLE,
  theme = APP_THEME,
  fillable = FALSE,

  sidebar = sidebar(
    width = 380,
    class = "app-sidebar",
    tags$div(class = "side-step",
             tags$span(class = "step-num", "1"),
             tags$span(class = "step-label", "Point it at a page")),
    selectInput("preset", NULL, choices = PRESETS, selectize = FALSE, width = "100%"),
    textInput("url", NULL,
              value = "https://247sports.com/college/arizona/Season/2026-Football/Commits/",
              placeholder = "https://example.com/page  (press Enter to analyze)",
              width = "100%"),
    uiOutput("recent_ui"),
    checkboxInput("render_js",
                  tags$span("Render with headless Chrome",
                            tags$small(class = "text-muted", "for JS-heavy sites")),
                  value = FALSE),
    div(class = "pages-row",
        numericInput("pages", NULL, value = 1, min = 1, max = 25, step = 1,
                     width = "72px"),
        tags$span(class = "pages-label",
                  tags$b("Pages to scrape"),
                  tags$small(class = "text-muted",
                             "follows “next page” links for lists & cards"))),
    actionButton("analyze", tagList(icon("wand-magic-sparkles"), " Analyze site"),
                 class = "btn-primary w-100 analyze-btn"),
    uiOutput("status_ui"),
    uiOutput("found_ui")
  ),

  tags$head(
    # bump ?v= whenever styles.css / app.js changes so returning browsers
    # don't serve a stale cached copy
    tags$link(rel = "stylesheet", href = "styles.css?v=8"),
    tags$script(src = "app.js?v=4")
  ),
  useShinyjs(),

  navset_card_underline(
    id = "main_tabs",
    nav_panel(tagList(icon("compass"), " Overview"),        value = "tab_overview", uiOutput("overview_ui")),
    nav_panel(tagList(icon("table"), " Tables"),            value = "tab_tables",   tables_tab),
    nav_panel(tagList(icon("layer-group"), " Cards"),       value = "tab_cards",    cards_tab),
    nav_panel(tagList(icon("link"), " Links & Media"),      value = "tab_links",    links_tab),
    nav_panel(tagList(icon("code"), " Page data (JSON)"),   value = "tab_json",     json_tab),
    nav_panel(tagList(icon("crosshairs"), " Custom"),       value = "tab_custom",   custom_tab),
    nav_panel(tagList(icon("database"), " My Data"),        value = "tab_data",     data_tab),
    nav_panel(tagList(icon("clock-rotate-left"), " History"), value = "tab_history", history_tab)
  )
)
