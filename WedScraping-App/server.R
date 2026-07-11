# ============================================================================
# server.R - Web Scraper Studio
# ============================================================================

function(input, output, session) {

  # --------------------------------------------------------------------------
  # State
  # --------------------------------------------------------------------------
  rv <- reactiveValues(
    fetch       = NULL,   # result of fetch_page()
    analysis    = NULL,   # result of analyze_page()
    disc        = NULL,   # result of discover_all()
    json_blobs  = list(), # list(id, label, text)
    json_tables = list(), # named list of tibbles from the selected blob
    datasets    = list(), # name -> list(df, code, source, time)
    pending     = NULL,   # payload awaiting the "Add to My Data" modal
    recents     = character(),  # recently analyzed URLs (this session)
    history     = tibble::tibble(time = character(), action = character(),
                                 detail = character(), rows = integer())
  )

  notify <- function(msg, type = "message") {
    showNotification(msg, type = type, duration = 5)
  }

  add_history <- function(action, detail, rows = NA_integer_) {
    rv$history <- dplyr::bind_rows(
      rv$history,
      tibble::tibble(time = format(Sys.time(), "%H:%M:%S"),
                     action = action,
                     detail = substr(detail, 1, 200),
                     rows = as.integer(rows))
    )
  }

  used_js  <- reactive(identical(rv$fetch$method, "chromote"))
  cur_url  <- reactive({ req(rv$fetch); rv$fetch$final_url %||% rv$fetch$url })
  cur_page <- reactive({ req(rv$fetch$page); rv$fetch$page })

  # how many pages to scrape (the sidebar control), clamped to a sane range
  pages_n <- reactive({
    p <- suppressWarnings(as.integer(input$pages %||% 1))
    if (is.na(p) || p < 1) 1L else min(p, 25L)
  })

  # --------------------------------------------------------------------------
  # Sidebar: presets + analyze
  # --------------------------------------------------------------------------
  observeEvent(input$preset, {
    if (nzchar(input$preset)) updateTextInput(session, "url", value = input$preset)
  })

  do_analyze <- function(url_raw, render_js) {
    v <- validate_url(url_raw)
    if (!v$ok) { notify(v$message, "error"); return(invisible()) }
    updateTextInput(session, "url", value = v$url)

    rv$analysis <- NULL; rv$disc <- NULL
    rv$json_blobs <- list(); rv$json_tables <- list()

    withProgress(message = "Scraper Studio", value = 0, {
      incProgress(0.1, detail = if (render_js) "Rendering with headless Chrome..."
                  else "Fetching page...")
      message("[studio] fetching ", v$url)
      f <- fetch_page(v$url, render_js = render_js)
      message("[studio] fetch done: status=", f$status, " ok=", f$ok)
      rv$fetch <- f
      if (!isTRUE(f$ok)) {
        add_history("fetch failed", paste(v$url, "-", f$error %||% "unknown error"))
        notify(f$error %||% "Fetch failed.", "error")
        return(invisible())
      }
      if (isTRUE(f$is_json)) {
        incProgress(0.6, detail = "Parsing JSON response...")
        rv$analysis   <- analyze_json_endpoint(f)
        rv$disc       <- discover_all_empty()
        rv$json_blobs <- list(list(id = "body", label = "Response body (JSON)",
                                   text = f$raw_text))
      } else {
        incProgress(0.4, detail = "Discovering scrapable structures...")
        message("[studio] discovering structures")
        d <- discover_all(f$page, f$final_url %||% f$url)
        message("[studio] discovery done: tables=", nrow(d$tables),
                " cards=", nrow(d$cards))
        rv$disc       <- d
        rv$json_blobs <- d$json
        incProgress(0.8, detail = "Fingerprinting the site...")
        rv$analysis   <- analyze_page(f, d)
        message("[studio] analysis done: ", rv$analysis$site_type)
      }
    })
    message("[studio] refreshing pickers")

    refresh_pickers()
    bslib::nav_select("main_tabs", "tab_overview", session)
    if (!is.null(rv$analysis)) {
      add_history("analyze", sprintf("%s  [%s]", v$url, rv$analysis$site_type))
      notify(sprintf("Detected: %s", rv$analysis$site_type))
      # remember this URL for the "Recent" picker (newest first, capped at 8)
      rv$recents <- utils::head(unique(c(v$url, rv$recents)), 8)
    }
    invisible()
  }

  output$recent_ui <- renderUI({
    req(length(rv$recents) > 0)
    selectInput("recent_url", NULL,
                choices = c("Recent URLs (this session)..." = "", rv$recents),
                selectize = FALSE, width = "100%")
  })

  observeEvent(input$recent_url, {
    if (nzchar(input$recent_url %||% "")) {
      updateTextInput(session, "url", value = input$recent_url)
    }
  })

  # Re-detect the field checkboxes for a card group. Called from BOTH the
  # card_pick observer and refresh_pickers: after re-analyzing a new site the
  # selected id (e.g. "card_1") often doesn't change, so the observer alone
  # would leave the previous site's fields in place.
  set_card_fields <- function(card_id) {
    if (is.null(rv$disc) || nrow(rv$disc$cards) == 0 ||
        is.null(rv$fetch$page) || !nzchar(card_id %||% "")) {
      updateCheckboxGroupInput(session, "card_fields", choices = character(0))
      return(invisible())
    }
    g <- rv$disc$cards[rv$disc$cards$id == card_id, ]
    if (nrow(g) != 1) return(invisible())
    toks <- tryCatch(detect_card_fields(rv$fetch$page, g$xpath),
                     error = function(e) character())
    if (length(toks)) {
      updateCheckboxGroupInput(
        session, "card_fields",
        choiceNames  = lapply(toks, function(t)
          tags$span(pretty_field_name(t),
                    tags$code(class = "field-code", paste0(".", t)))),
        choiceValues = as.list(toks),
        selected = utils::head(toks, 8)
      )
    } else {
      updateCheckboxGroupInput(session, "card_fields", choices = character(0))
    }
    invisible()
  }

  # Parse a JSON payload into record sets + refresh the record-set picker.
  # Shared by the json_pick observer and refresh_pickers (same reason as above).
  set_json_blob <- function(i) {
    if (is.na(i) || i < 1 || i > length(rv$json_blobs)) return(invisible())
    tabs <- tryCatch(json_to_tables(rv$json_blobs[[i]]$text),
                     error = function(e) list())
    # most-relevant-looking record sets first (players over nav menus)
    if (length(tabs) > 1) tabs <- tabs[order(-score_json_records(tabs))]
    rv$json_tables <- tabs
    if (length(rv$json_tables)) {
      labs <- sprintf("%s   (%d x %d)",
                      shorten_path(names(rv$json_tables)),
                      vapply(rv$json_tables, nrow, integer(1)),
                      vapply(rv$json_tables, ncol, integer(1)))
      updateSelectInput(session, "json_table",
                        choices = stats::setNames(seq_along(rv$json_tables), labs))
    } else {
      updateSelectInput(session, "json_table",
                        choices = c("No record sets found in this payload" = ""))
    }
    invisible()
  }

  refresh_pickers <- function() {
    d <- rv$disc
    # tables
    if (!is.null(d) && nrow(d$tables) > 0) {
      extra <- ifelse(nzchar(d$tables$caption),
                      paste0(" - ", substr(d$tables$caption, 1, 40)),
                      ifelse(nzchar(d$tables$columns),
                             paste0(" - ", substr(d$tables$columns, 1, 40)), ""))
      labs <- sprintf("Table %d  (%d x %d)%s",
                      d$tables$index, d$tables$rows, d$tables$cols, extra)
      updateSelectInput(session, "table_pick",
                        choices = stats::setNames(d$tables$index, labs))
    } else {
      updateSelectInput(session, "table_pick", choices = c("No tables found" = ""))
    }
    # cards
    if (!is.null(d) && nrow(d$cards) > 0) {
      updateSelectInput(session, "card_pick",
                        choices = stats::setNames(d$cards$id, d$cards$label))
      set_card_fields(d$cards$id[1])
    } else {
      updateSelectInput(session, "card_pick",
                        choices = c("No repeated groups found" = ""))
      updateCheckboxGroupInput(session, "card_fields", choices = character(0))
    }
    # lists
    if (!is.null(d) && nrow(d$lists) > 0) {
      labs <- sprintf("List %d  (%d items, %s)  %s",
                      d$lists$index, d$lists$items, d$lists$context,
                      substr(d$lists$sample, 1, 40))
      updateSelectInput(session, "list_pick",
                        choices = stats::setNames(d$lists$index, labs))
    } else {
      updateSelectInput(session, "list_pick", choices = c("No lists found" = ""))
    }
    # json payloads
    if (length(rv$json_blobs)) {
      labs <- vapply(rv$json_blobs, function(b) b$label, character(1))
      updateSelectInput(session, "json_pick",
                        choices = stats::setNames(seq_along(rv$json_blobs), labs))
      set_json_blob(1L)
    } else {
      updateSelectInput(session, "json_pick",
                        choices = c("No embedded JSON found" = ""))
      updateSelectInput(session, "json_table",
                        choices = c("Pick a payload first" = ""))
    }
  }

  # A raw error inside an observer kills the whole Shiny session - never let
  # an analysis problem take the app down.
  safe_analyze <- function(url_raw, render_js) {
    tryCatch(
      do_analyze(url_raw, render_js),
      error = function(e) {
        message("[studio] analyze ERROR: ", conditionMessage(e))
        notify(paste("Something went wrong while analyzing:",
                     conditionMessage(e)), "error")
      }
    )
  }

  observeEvent(input$analyze, {
    safe_analyze(input$url, isTRUE(input$render_js))
  })

  observeEvent(input$reanalyze_js, {
    req(input$reanalyze_js > 0)
    updateCheckboxInput(session, "render_js", value = TRUE)
    safe_analyze(input$url, TRUE)
  })

  # --------------------------------------------------------------------------
  # Sidebar status + found chips
  # --------------------------------------------------------------------------
  output$status_ui <- renderUI({
    f <- rv$fetch
    if (is.null(f)) {
      return(div(class = "status-card status-idle",
                 icon("circle-info"),
                 " Pick an example or paste a URL, then hit Analyze."))
    }
    if (!isTRUE(f$ok)) {
      return(div(class = "status-card status-bad",
                 div(icon("triangle-exclamation"), strong(" Fetch failed")),
                 div(class = "status-line", f$error %||% "Unknown error")))
    }
    div(class = "status-card status-ok",
        div(icon("circle-check"), strong(" Fetched OK "),
            span(class = "mono", sprintf("HTTP %s", f$status))),
        div(class = "status-line",
            if (identical(f$method, "chromote")) "Rendered by headless Chrome (JS)"
            else "Direct request"),
        div(class = "status-line",
            if (isTRUE(f$is_json)) "Content: JSON" else "Content: HTML"),
        div(class = "status-line mono", format(f$fetched_at, "%H:%M:%S"))
    )
  })

  output$found_ui <- renderUI({
    req(rv$analysis, rv$disc)
    d <- rv$disc
    chip <- function(n, label) {
      div(class = paste("found-chip", if (n > 0) "found-yes" else "found-no"),
          span(class = "found-n", n), span(class = "found-label", label))
    }
    tagList(
      tags$div(class = "side-step mt-3",
               tags$span(class = "step-num", "2"),
               tags$span(class = "step-label", "Pick what to extract")),
      div(class = "found-chips",
          chip(nrow(d$tables), "tables"),
          chip(nrow(d$cards),  "card groups"),
          chip(nrow(d$lists),  "lists"),
          chip(length(rv$json_blobs), "JSON payloads"),
          chip(nrow(d$links),  "links")
      )
    )
  })

  # --------------------------------------------------------------------------
  # Overview tab
  # --------------------------------------------------------------------------
  output$overview_ui <- renderUI({
    a <- rv$analysis
    if (is.null(a)) {
      step_card <- function(num, icon_name, title, text) {
        div(class = "step-card",
            div(class = "step-card-top",
                span(class = "step-card-icon", icon(icon_name)),
                span(class = "step-card-num", paste0("STEP ", num))),
            h5(title),
            p(text))
      }
      return(div(class = "welcome-wrap",
        div(class = "hero",
            div(class = "hero-badge", icon("spider"), "Web Scraper Studio"),
            h1(class = "hero-title",
               "Turn any web page into a ", span(class = "hero-accent", "data frame")),
            p(class = "hero-sub",
              "Point it at a page and the studio figures out what kind of site it is, then offers you every structure worth scraping - no code required (but it writes the R code for you anyway).")),
        div(class = "step-grid",
            step_card(1, "location-crosshairs", "Point",
                      "Paste a URL in the sidebar, or pick a bundled example."),
            step_card(2, "wand-magic-sparkles", "Analyze",
                      "Fingerprints the site and discovers tables, cards, lists, links, and embedded JSON."),
            step_card(3, "hand-pointer", "Pick",
                      "Preview any structure as a data frame and add it to My Data."),
            step_card(4, "download", "Use",
                      "Clean, rename, download - and grab the R script that reproduces it.")),
        div(class = "hero-tip",
            icon("lightbulb"),
            span(HTML("JS-heavy sites look empty on a direct request &mdash; flip on <b>Render with headless Chrome</b> and re-analyze.")))
      ))
    }

    d <- rv$disc
    found_row <- function(label, n, btn_id, icon_name) {
      div(class = "found-row",
          div(class = "found-row-left",
              span(class = "found-row-icon", icon(icon_name)),
              span(class = paste("count-badge", if (n > 0) "count-yes" else "count-zero"), n),
              span(class = "found-row-label", label)),
          if (n > 0) actionButton(btn_id, "Open", class = "btn-sm btn-outline-primary")
      )
    }

    tagList(
      layout_columns(
        col_widths = c(7, 5),
        card(
          class = "fit-card",
          card_header(tagList(icon("fingerprint"), " Site fingerprint")),
          card_body(
            div(class = "fp-title", a$title %||% ""),
            div(class = "fp-row", span(class = "fp-key", "URL"),
                tags$a(href = a$url, target = "_blank", class = "fp-url", a$url)),
            div(class = "fp-row", span(class = "fp-key", "Domain"), a$domain),
            div(class = "fp-row", span(class = "fp-key", "Detected type"),
                span(class = "type-badge", a$site_type)),
            if (!is.na(a$description %||% NA))
              div(class = "fp-desc", substr(a$description, 1, 300)),
            if (length(a$tech))
              div(class = "fp-row", span(class = "fp-key", "Built with"),
                  lapply(a$tech, function(t) span(class = "tech-chip", t))),
            if (length(a$schema_types))
              div(class = "fp-row", span(class = "fp-key", "Structured data"),
                  lapply(utils::head(a$schema_types, 6),
                         function(t) span(class = "tech-chip schema-chip", t))),
            if (isTRUE(a$js_shell) && !used_js())
              div(class = "alert alert-warning js-alert",
                  icon("triangle-exclamation"),
                  " This page looks like a JavaScript shell - most content probably loads in the browser. ",
                  actionButton("reanalyze_js", "Re-analyze with headless Chrome",
                               class = "btn-sm btn-warning"))
          )
        ),
        card(
          card_header(tagList(icon("binoculars"), " What we found")),
          card_body(
            found_row("HTML tables",          nrow(d$tables), "goto_tables", "table"),
            found_row("Repeated card groups", nrow(d$cards),  "goto_cards",  "layer-group"),
            found_row("Lists",                nrow(d$lists),  "goto_lists",  "list-ul"),
            found_row("Embedded JSON payloads", length(rv$json_blobs), "goto_json", "file-code"),
            found_row("Links",                nrow(d$links),  "goto_links",  "link"),
            found_row("Images",               nrow(d$images), "goto_images", "image"),
            tags$hr(),
            actionButton("quick_scrape",
                         tagList(icon("bolt"), " Quick scrape - best guess to My Data"),
                         class = "btn-primary w-100")
          )
        )
      ),
      if (!is.null(a$recipe)) {
        card(
          class = "recipe-card",
          card_header(tagList(icon("wand-magic-sparkles"),
                              " Site recipe matched: ", strong(a$recipe$label))),
          card_body(
            p(a$recipe$description),
            actionButton("run_recipe_btn",
                         tagList(icon("play"), " Run recipe → My Data"),
                         class = "btn-success")
          )
        )
      },
      accordion(
        open = FALSE, class = "code-accordion mt-3",
        accordion_panel(
          title = tagList(icon("file-code"), " Page source (first 8,000 characters)"),
          value = "acc_source",
          verbatimTextOutput("source_html")
        )
      )
    )
  })

  output$source_html <- renderText({
    req(rv$fetch$raw_text)
    substr(rv$fetch$raw_text, 1, 8000)
  })

  # Overview navigation buttons (guard > 0: dynamic buttons reset on re-render)
  observeEvent(input$goto_tables, { req(input$goto_tables > 0); bslib::nav_select("main_tabs", "tab_tables", session) })
  observeEvent(input$goto_cards,  { req(input$goto_cards  > 0); bslib::nav_select("main_tabs", "tab_cards",  session) })
  observeEvent(input$goto_json,   { req(input$goto_json   > 0); bslib::nav_select("main_tabs", "tab_json",   session) })
  observeEvent(input$goto_links, {
    req(input$goto_links > 0)
    bslib::nav_select("main_tabs", "tab_links", session)
    bslib::nav_select("links_subtabs", "Links", session)
  })
  observeEvent(input$goto_lists, {
    req(input$goto_lists > 0)
    bslib::nav_select("main_tabs", "tab_links", session)
    bslib::nav_select("links_subtabs", "Lists", session)
  })
  observeEvent(input$goto_images, {
    req(input$goto_images > 0)
    bslib::nav_select("main_tabs", "tab_links", session)
    bslib::nav_select("links_subtabs", "Images", session)
  })

  # --------------------------------------------------------------------------
  # Add-to-data modal (shared by every tab)
  # --------------------------------------------------------------------------
  open_add_modal <- function(default_name, df, code, source) {
    rv$pending <- list(df = df, code = code, source = source)
    showModal(modalDialog(
      title = "Add to My Data",
      p(class = "text-muted",
        sprintf("%s rows × %s columns", format(nrow(df), big.mark = ","), ncol(df))),
      textInput("ds_name", "Dataset name", value = default_name, width = "100%"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add", "Add dataset", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  }

  update_data_select <- function(selected = NULL) {
    ch <- names(rv$datasets)
    if (!length(ch)) {
      updateSelectInput(session, "data_pick", choices = c("No datasets yet" = ""))
    } else {
      updateSelectInput(session, "data_pick", choices = ch,
                        selected = selected %||% ch[length(ch)])
    }
  }

  store_dataset <- function(name, df, code, source) {
    nm <- slugify(name)
    if (nm %in% names(rv$datasets)) {
      nm <- make.unique(c(names(rv$datasets), nm), sep = "_")[length(rv$datasets) + 1]
    }
    rv$datasets[[nm]] <- list(df = df, code = code, source = source, time = Sys.time())
    add_history("extract", paste0(nm, "  ←  ", source), nrow(df))
    update_data_select(selected = nm)
    nm
  }

  observeEvent(input$confirm_add, {
    req(rv$pending)
    nm <- store_dataset(input$ds_name, rv$pending$df, rv$pending$code, rv$pending$source)
    rv$pending <- NULL
    removeModal()
    notify(sprintf("Added '%s' - open the My Data tab to clean & download.", nm))
  })

  # --------------------------------------------------------------------------
  # Quick scrape + recipe runner
  # --------------------------------------------------------------------------
  # a runnable, standalone reproduce-script for a curated recipe
  recipe_code <- function(recipe, np) {
    paste0("# Curated recipe: ", recipe$label,
           if (np > 1) sprintf("  (scraped up to %d pages)\n", np) else "\n",
           gen_recipe_code(recipe, cur_url(), used_js(), pages = np))
  }

  best_guess_scrape <- function() {
    a <- rv$analysis; d <- rv$disc
    page <- rv$fetch$page
    np <- pages_n()
    # 1. curated recipe
    if (!is.null(a$recipe) && !is.null(page)) {
      df <- if (np > 1 && !used_js()) {
        scrape_paginated(page, cur_url(),
                         function(pg, u) run_recipe(a$recipe, pg, u),
                         max_pages = np)
      } else run_recipe(a$recipe, page, cur_url())
      if (nrow(df) > 0) {
        ps <- attr(df, "pages_scraped") %||% 1L
        return(list(df = df, name = a$recipe$id,
                    source = paste0("Recipe: ", a$recipe$label,
                                    if (ps > 1) sprintf(" (%s pages)", ps) else ""),
                    code = recipe_code(a$recipe, np)))
      }
    }
    # 2. biggest table (single page - table pagination is rare, keeps code exact)
    if (!is.null(d) && nrow(d$tables) > 0 && !is.null(page)) {
      best <- d$tables[which.max(d$tables$rows * d$tables$cols), ]
      df <- sanitize_df(scrape_table(page, best$index))
      if (nrow(df) > 0) {
        return(list(df = df, name = paste0("table_", best$index),
                    source = paste0("Table ", best$index, " on ", cur_url()),
                    code = gen_table_code(cur_url(), best$index, used_js())))
      }
    }
    # 3. best repeated card group
    if (!is.null(d) && nrow(d$cards) > 0 && !is.null(page)) {
      g <- d$cards[1, ]
      toks <- detect_card_fields(page, g$xpath)
      df <- if (np > 1 && !used_js()) {
        scrape_paginated(page, cur_url(),
                         function(pg, u) scrape_cards(pg, g$xpath, toks, base_url = u),
                         max_pages = np)
      } else scrape_cards(page, g$xpath, toks, base_url = cur_url())
      if (nrow(df) > 0) {
        ps <- attr(df, "pages_scraped") %||% 1L
        return(list(df = df, name = "cards",
                    source = paste0(g$label, " on ", cur_url(),
                                    if (ps > 1) sprintf(" (%s pages)", ps) else ""),
                    code = gen_cards_code(cur_url(), g$xpath, toks, used_js(), pages = np)))
      }
    }
    # 4. embedded JSON
    if (length(rv$json_blobs)) {
      tabs <- json_to_tables(rv$json_blobs[[1]]$text)
      if (length(tabs)) {
        return(list(df = tabs[[1]], name = "json_data",
                    source = paste0(rv$json_blobs[[1]]$label, " on ", cur_url()),
                    code = gen_json_code(cur_url(), rv$json_blobs[[1]]$id,
                                         names(tabs)[1], used_js())))
      }
    }
    NULL
  }

  observeEvent(input$quick_scrape, {
    req(input$quick_scrape > 0, rv$analysis)
    if (pages_n() > 1 && used_js()) {
      notify("Pagination isn't available with headless-Chrome rendering - only this page was scraped.", "warning")
    }
    res <- withProgress(message = "Quick scraping...",
                        tryCatch(best_guess_scrape(), error = function(e) NULL))
    if (is.null(res)) {
      notify("Couldn't auto-pick a dataset here - browse the tabs to choose one manually.",
             "warning")
      return()
    }
    nm <- store_dataset(paste0(res$name, "_", rv$analysis$domain),
                        res$df, res$code, res$source)
    bslib::nav_select("main_tabs", "tab_data", session)
    notify(sprintf("Quick scrape added '%s' (%d rows).", nm, nrow(res$df)))
  })

  observeEvent(input$run_recipe_btn, {
    req(input$run_recipe_btn > 0, rv$analysis$recipe, cur_page())
    r <- rv$analysis$recipe
    np <- pages_n()
    if (np > 1 && used_js()) {
      notify("Pagination isn't available with headless-Chrome rendering - only this page was scraped.", "warning")
    }
    df <- withProgress(
      message = paste("Running recipe:", r$label),
      detail = if (np > 1) sprintf("across up to %d pages...", np) else NULL,
      tryCatch(
        if (np > 1 && !used_js()) {
          scrape_paginated(cur_page(), cur_url(),
                           function(pg, u) run_recipe(r, pg, u), max_pages = np)
        } else run_recipe(r, cur_page(), cur_url()),
        error = function(e) tibble::tibble()))
    if (nrow(df) == 0) {
      notify("The recipe matched this site but returned no rows - the page layout may have changed. Try the Cards or JSON tabs.",
             "warning")
      return()
    }
    pg_scraped <- attr(df, "pages_scraped") %||% 1L
    nm <- store_dataset(paste0(r$id, "_", rv$analysis$domain), df,
                        recipe_code(r, np),
                        paste0("Recipe: ", r$label,
                               if (pg_scraped > 1) sprintf(" (%s pages)", pg_scraped) else ""))
    bslib::nav_select("main_tabs", "tab_data", session)
    notify(sprintf("Recipe added '%s' (%d rows%s).", nm, nrow(df),
                   if (pg_scraped > 1) sprintf(" from %s pages", pg_scraped) else ""))
  })

  # --------------------------------------------------------------------------
  # Tables tab
  # --------------------------------------------------------------------------
  table_prev <- reactive({
    req(cur_page(), nzchar(input$table_pick %||% ""))
    scrape_table(cur_page(), as.integer(input$table_pick),
                 isTRUE(input$table_promote))
  })

  output$table_meta <- renderUI({
    req(rv$disc, nzchar(input$table_pick %||% ""))
    row <- rv$disc$tables[rv$disc$tables$index == as.integer(input$table_pick), ]
    req(nrow(row) == 1)
    div(class = "meta-block",
        if (nzchar(row$caption)) div(class = "meta-line", strong("Caption: "), row$caption),
        div(class = "meta-line", strong("Columns: "), row$columns))
  })

  output$table_dt <- DT::renderDT({
    validate(need(!is.null(rv$fetch) && isTRUE(rv$fetch$ok),
                  "Analyze a site first (sidebar, step 1)."))
    validate(need(!is.null(rv$disc) && nrow(rv$disc$tables) > 0,
                  "No HTML tables on this page. Try the Cards tab or Page data (JSON)."))
    df <- table_prev()
    validate(need(nrow(df) > 0, "This table parsed to zero rows."))
    dt_table(cap_preview(sanitize_df(df)))
  })

  output$table_code <- renderText({
    req(rv$fetch, nzchar(input$table_pick %||% ""))
    gen_table_code(cur_url(), as.integer(input$table_pick), used_js(),
                   isTRUE(input$table_promote))
  })

  observeEvent(input$table_add, {
    df <- tryCatch(sanitize_df(table_prev()), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      notify("Nothing to add - pick a table with rows first.", "warning"); return()
    }
    open_add_modal(
      default_name = paste0("table", input$table_pick, "_", slugify(rv$analysis$domain)),
      df = df,
      code = gen_table_code(cur_url(), as.integer(input$table_pick), used_js(),
                            isTRUE(input$table_promote)),
      source = paste0("Table ", input$table_pick, " on ", cur_url())
    )
  })

  # --------------------------------------------------------------------------
  # Cards tab
  # --------------------------------------------------------------------------
  card_group <- reactive({
    req(rv$disc, nzchar(input$card_pick %||% ""))
    g <- rv$disc$cards[rv$disc$cards$id == input$card_pick, ]
    req(nrow(g) == 1)
    g
  })

  observeEvent(input$card_pick, {
    req(nzchar(input$card_pick %||% ""), rv$fetch$page)
    set_card_fields(input$card_pick)
  }, ignoreInit = TRUE)

  card_prev <- reactive({
    req(cur_page())
    g <- card_group()
    scrape_cards(cur_page(), g$xpath,
                 tokens = input$card_fields %||% character(0),
                 base_url = cur_url(),
                 include_link = isTRUE(input$card_link),
                 include_image = isTRUE(input$card_image),
                 include_full_text = isTRUE(input$card_fulltext))
  })

  output$card_meta <- renderUI({
    g <- tryCatch(card_group(), error = function(e) NULL)
    req(g)
    div(class = "meta-block",
        div(class = "meta-line", strong(g$n), " matching items"),
        div(class = "meta-line sample-line", em(substr(g$sample, 1, 150))))
  })

  output$card_dt <- DT::renderDT({
    validate(need(!is.null(rv$fetch) && isTRUE(rv$fetch$ok),
                  "Analyze a site first (sidebar, step 1)."))
    validate(need(!is.null(rv$disc) && nrow(rv$disc$cards) > 0,
                  "No repeated element groups found. If this site loads content with JavaScript, enable headless Chrome and re-analyze."))
    df <- card_prev()
    validate(need(nrow(df) > 0, "This group produced zero rows."))
    dt_table(cap_preview(df))
  })

  card_codegen <- function(g) {
    gen_cards_code(cur_url(), g$xpath, input$card_fields %||% character(0),
                   used_js(),
                   include_link      = isTRUE(input$card_link),
                   include_image     = isTRUE(input$card_image),
                   include_full_text = isTRUE(input$card_fulltext),
                   pages             = pages_n())
  }

  output$card_code <- renderText({
    g <- tryCatch(card_group(), error = function(e) NULL)
    req(g, rv$fetch)
    card_codegen(g)
  })

  output$card_pages_hint <- renderUI({
    np <- pages_n()
    if (np <= 1) return(NULL)
    if (used_js()) {
      return(div(class = "pages-hint warn",
                 "Pagination isn't available with headless-Chrome rendering - only this page will be added."))
    }
    div(class = "pages-hint",
        sprintf("Preview shows page 1; Add will follow the 'next page' link across up to %d pages.", np))
  })

  observeEvent(input$card_add, {
    g <- tryCatch(card_group(), error = function(e) NULL)
    if (is.null(g)) { notify("Nothing to add - pick a card group first.", "warning"); return() }
    np <- pages_n()
    df <- if (np > 1 && !used_js()) {
      withProgress(
        message = "Scraping pages...",
        detail = sprintf("up to %d, following 'next' links", np),
        tryCatch(
          scrape_paginated(cur_page(), cur_url(),
                           function(pg, u) scrape_cards(pg, g$xpath,
                                                        tokens = input$card_fields %||% character(0),
                                                        base_url = u,
                                                        include_link = isTRUE(input$card_link),
                                                        include_image = isTRUE(input$card_image),
                                                        include_full_text = isTRUE(input$card_fulltext)),
                           max_pages = np),
          error = function(e) NULL))
    } else {
      tryCatch(card_prev(), error = function(e) NULL)
    }
    if (is.null(df) || nrow(df) == 0) {
      notify("Nothing to add - pick a card group first.", "warning"); return()
    }
    pg_scraped <- attr(df, "pages_scraped") %||% 1L
    open_add_modal(
      default_name = paste0("cards_", slugify(rv$analysis$domain)),
      df = df,
      code = card_codegen(g),
      source = paste0(g$label, " on ", cur_url(),
                      if (pg_scraped > 1) sprintf(" (%s pages)", pg_scraped) else "")
    )
  })

  # --------------------------------------------------------------------------
  # Links & media tab
  # --------------------------------------------------------------------------
  links_filtered <- reactive({
    req(rv$disc)
    df <- rv$disc$links
    if (isTRUE(input$link_internal) && "internal" %in% names(df)) {
      df <- dplyr::filter(df, internal %in% TRUE)
    }
    flt <- stringr::str_squish(input$link_filter %||% "")
    if (nzchar(flt) && nrow(df) > 0) {
      df <- dplyr::filter(
        df,
        stringr::str_detect(paste(text, url),
                            stringr::fixed(flt, ignore_case = TRUE)))
    }
    df
  })

  output$links_meta <- renderUI({
    req(rv$disc)
    div(class = "meta-block",
        div(class = "meta-line",
            strong(nrow(links_filtered())), " of ", nrow(rv$disc$links), " links"))
  })

  output$links_dt <- DT::renderDT({
    validate(need(!is.null(rv$disc), "Analyze a site first (sidebar, step 1)."))
    validate(need(nrow(rv$disc$links) > 0, "No links found on this page."))
    dt_table(cap_preview(links_filtered()))
  })

  observeEvent(input$links_add, {
    df <- links_filtered()
    if (nrow(df) == 0) { notify("No links to add.", "warning"); return() }
    open_add_modal(paste0("links_", slugify(rv$analysis$domain)), df,
                   gen_links_code(cur_url(), used_js(),
                                  filter_text = stringr::str_squish(input$link_filter %||% ""),
                                  internal_only = isTRUE(input$link_internal)),
                   paste0("Links on ", cur_url()))
  })

  output$images_dt <- DT::renderDT({
    validate(need(!is.null(rv$disc), "Analyze a site first (sidebar, step 1)."))
    validate(need(nrow(rv$disc$images) > 0, "No images found on this page."))
    dt_table(cap_preview(rv$disc$images))
  })

  observeEvent(input$images_add, {
    req(rv$disc); df <- rv$disc$images
    if (nrow(df) == 0) { notify("No images to add.", "warning"); return() }
    open_add_modal(paste0("images_", slugify(rv$analysis$domain)), df,
                   gen_custom_code(cur_url(), "img[src]", "src", render_js = used_js()),
                   paste0("Images on ", cur_url()))
  })

  output$headings_dt <- DT::renderDT({
    validate(need(!is.null(rv$disc), "Analyze a site first (sidebar, step 1)."))
    validate(need(nrow(rv$disc$headings) > 0, "No headings found on this page."))
    dt_table(cap_preview(rv$disc$headings))
  })

  observeEvent(input$headings_add, {
    req(rv$disc); df <- rv$disc$headings
    if (nrow(df) == 0) { notify("No headings to add.", "warning"); return() }
    open_add_modal(paste0("headings_", slugify(rv$analysis$domain)), df,
                   gen_custom_code(cur_url(), "h1, h2, h3, h4, h5", "text",
                                   render_js = used_js()),
                   paste0("Headings on ", cur_url()))
  })

  list_prev <- reactive({
    req(cur_page(), nzchar(input$list_pick %||% ""))
    scrape_list(cur_page(), as.integer(input$list_pick), cur_url())
  })

  output$list_dt <- DT::renderDT({
    validate(need(!is.null(rv$disc), "Analyze a site first (sidebar, step 1)."))
    validate(need(nrow(rv$disc$lists) > 0, "No sizable lists found on this page."))
    df <- list_prev()
    validate(need(nrow(df) > 0, "This list parsed to zero items."))
    dt_table(cap_preview(df))
  })

  observeEvent(input$list_add, {
    df <- tryCatch(list_prev(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) { notify("Pick a list first.", "warning"); return() }
    open_add_modal(paste0("list", input$list_pick, "_", slugify(rv$analysis$domain)),
                   df, gen_list_code(cur_url(), as.integer(input$list_pick), used_js()),
                   paste0("List ", input$list_pick, " on ", cur_url()))
  })

  # --------------------------------------------------------------------------
  # Page data (JSON) tab
  # --------------------------------------------------------------------------
  observeEvent(input$json_pick, {
    req(nzchar(input$json_pick %||% ""))
    withProgress(message = "Parsing JSON payload...", {
      set_json_blob(as.integer(input$json_pick))
    })
  }, ignoreInit = TRUE)

  json_prev <- reactive({
    req(nzchar(input$json_table %||% ""))
    i <- as.integer(input$json_table)
    req(i >= 1, i <= length(rv$json_tables))
    rv$json_tables[[i]]
  })

  output$json_dt <- DT::renderDT({
    validate(need(!is.null(rv$fetch) && isTRUE(rv$fetch$ok),
                  "Analyze a site first (sidebar, step 1)."))
    validate(need(length(rv$json_blobs) > 0,
                  "No embedded JSON payloads found on this page."))
    validate(need(length(rv$json_tables) > 0,
                  "Pick a payload on the left - record sets inside it will appear."))
    df <- json_prev()
    validate(need(nrow(df) > 0, "This record set is empty."))
    dt_table(cap_preview(df))
  })

  output$json_code <- renderText({
    req(length(rv$json_blobs) > 0, nzchar(input$json_pick %||% ""),
        nzchar(input$json_table %||% ""))
    i <- as.integer(input$json_pick)
    j <- as.integer(input$json_table)
    # bounds guards: inputs are stale for one round-trip after re-analysis
    req(!is.na(i), i >= 1, i <= length(rv$json_blobs),
        !is.na(j), j >= 1, j <= length(rv$json_tables))
    gen_json_code(cur_url(), rv$json_blobs[[i]]$id,
                  names(rv$json_tables)[j], used_js())
  })

  observeEvent(input$json_add, {
    df <- tryCatch(json_prev(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) {
      notify("Pick a payload and a record set first.", "warning"); return()
    }
    i <- as.integer(input$json_pick)
    req(!is.na(i), i >= 1, i <= length(rv$json_blobs))
    open_add_modal(paste0("json_", slugify(rv$analysis$domain)), df,
                   gen_json_code(cur_url(), rv$json_blobs[[i]]$id,
                                 names(rv$json_tables)[as.integer(input$json_table)],
                                 used_js()),
                   paste0(rv$json_blobs[[i]]$label, " on ", cur_url()))
  })

  # --------------------------------------------------------------------------
  # Custom selector tab
  # --------------------------------------------------------------------------
  custom_sel_raw <- reactive(input$custom_selector)
  custom_sel <- debounce(custom_sel_raw, 500)

  custom_res <- reactive({
    req(rv$fetch$page)
    scrape_custom(cur_page(), custom_sel(),
                  mode = input$custom_mode %||% "text",
                  attr_name = input$custom_attr,
                  base_url = cur_url())
  })

  output$custom_count <- renderUI({
    if (is.null(rv$fetch) || is.null(rv$fetch$page)) {
      return(div(class = "sel-count sel-idle", "Analyze a site first."))
    }
    # blank selector = neutral idle state, not an error
    if (!nzchar(stringr::str_squish(custom_sel() %||% ""))) {
      return(div(class = "sel-count sel-idle", "Type a selector to test it live."))
    }
    r <- custom_res()
    if (!isTRUE(r$ok)) return(div(class = "sel-count sel-bad", r$error))
    if (r$n == 0) {
      return(div(class = "sel-count sel-warn", "0 matches - tweak the selector."))
    }
    div(class = "sel-count sel-good",
        sprintf("%s matching element%s", format(r$n, big.mark = ","),
                if (r$n == 1) "" else "s"))
  })

  output$custom_dt <- DT::renderDT({
    validate(need(!is.null(rv$fetch) && !is.null(rv$fetch$page),
                  "Analyze a site first (sidebar, step 1)."))
    r <- custom_res()
    validate(need(isTRUE(r$ok), r$error %||% "Invalid selector."))
    validate(need(r$n > 0, "No matches yet - try a selector from the cheat sheet."))
    dt_table(cap_preview(r$data))
  })

  output$custom_code <- renderText({
    req(rv$fetch, nzchar(stringr::str_squish(custom_sel() %||% "")))
    gen_custom_code(cur_url(), custom_sel(), input$custom_mode %||% "text",
                    input$custom_attr, used_js())
  })

  observeEvent(input$custom_add, {
    r <- tryCatch(custom_res(), error = function(e) NULL)
    if (is.null(r) || !isTRUE(r$ok) || r$n == 0 || nrow(r$data) == 0) {
      notify("Get some matches first, then add.", "warning"); return()
    }
    open_add_modal(paste0("custom_", slugify(rv$analysis$domain)), r$data,
                   gen_custom_code(cur_url(), custom_sel(),
                                   input$custom_mode %||% "text",
                                   input$custom_attr, used_js()),
                   paste0("Selector '", custom_sel(), "' on ", cur_url()))
  })

  # --------------------------------------------------------------------------
  # My Data tab
  # --------------------------------------------------------------------------
  cur_ds <- reactive({
    req(nzchar(input$data_pick %||% ""), input$data_pick %in% names(rv$datasets))
    rv$datasets[[input$data_pick]]
  })

  output$data_title <- renderUI({
    if (is.null(tryCatch(cur_ds(), error = function(e) NULL))) return("Preview")
    tagList(icon("table-list"), " ", input$data_pick)
  })

  output$data_meta <- renderUI({
    ds <- tryCatch(cur_ds(), error = function(e) NULL)
    req(ds)
    div(class = "meta-block",
        div(class = "meta-line",
            strong(format(nrow(ds$df), big.mark = ",")), " rows × ",
            strong(ncol(ds$df)), " columns"),
        div(class = "meta-line sample-line", ds$source),
        div(class = "meta-line mono", format(ds$time, "%H:%M:%S")))
  })

  output$data_dt <- DT::renderDT({
    validate(need(length(rv$datasets) > 0,
                  "Nothing here yet - extract something from the Tables / Cards / JSON / Custom tabs, or use Quick scrape on the Overview."))
    ds <- cur_ds()
    dt_table(cap_preview(ds$df))
  })

  output$data_code <- renderText({
    ds <- tryCatch(cur_ds(), error = function(e) NULL)
    req(ds)
    ds$code %||% "# No code was recorded for this dataset."
  })

  # keep the stored "reproduce this dataset" script in sync with cleaning
  append_clean_code <- function(name, code_lines) {
    old <- rv$datasets[[name]]$code %||% ""
    rv$datasets[[name]]$code <- paste0(old, "\n", code_lines)
  }

  observeEvent(input$data_dedupe, {
    ds <- tryCatch(cur_ds(), error = function(e) NULL); req(ds)
    before <- nrow(ds$df)
    rv$datasets[[input$data_pick]]$df <- clean_dataset(ds$df, dedupe = TRUE)
    removed <- before - nrow(rv$datasets[[input$data_pick]]$df)
    if (removed > 0) {
      append_clean_code(input$data_pick,
                        "\n# cleaning applied in the app\ndf <- dplyr::distinct(df)")
    }
    notify(sprintf("Removed %d duplicate row%s.", removed, if (removed == 1) "" else "s"))
  })

  observeEvent(input$data_dropcols, {
    ds <- tryCatch(cur_ds(), error = function(e) NULL); req(ds)
    before <- ncol(ds$df)
    rv$datasets[[input$data_pick]]$df <- clean_dataset(ds$df, drop_empty_cols = TRUE)
    removed <- before - ncol(rv$datasets[[input$data_pick]]$df)
    if (removed > 0) {
      append_clean_code(input$data_pick, paste0(
        "\n# cleaning applied in the app\n",
        "df <- df[, vapply(df, function(col) {\n",
        "  v <- as.character(col)\n",
        "  any(!is.na(v) & nzchar(trimws(v)))\n",
        "}, logical(1)), drop = FALSE]"))
    }
    notify(sprintf("Dropped %d empty column%s.", removed, if (removed == 1) "" else "s"))
  })

  observeEvent(input$data_droprows, {
    ds <- tryCatch(cur_ds(), error = function(e) NULL); req(ds)
    before <- nrow(ds$df)
    rv$datasets[[input$data_pick]]$df <- clean_dataset(ds$df, drop_empty_rows = TRUE)
    removed <- before - nrow(rv$datasets[[input$data_pick]]$df)
    if (removed > 0) {
      append_clean_code(input$data_pick, paste0(
        "\n# cleaning applied in the app\n",
        "df <- df[apply(df, 1, function(r) {\n",
        "  any(!is.na(r) & nzchar(trimws(as.character(r))))\n",
        "}), , drop = FALSE]"))
    }
    notify(sprintf("Dropped %d empty row%s.", removed, if (removed == 1) "" else "s"))
  })

  # auto-detect + coerce column types (text -> number / integer / logical)
  observeEvent(input$data_types, {
    ds <- tryCatch(cur_ds(), error = function(e) NULL); req(ds)
    res <- infer_types(ds$df)
    if (!length(res$changed)) {
      notify("No columns needed a type change - they're already numeric/typed.", "message")
      return()
    }
    rv$datasets[[input$data_pick]]$df <- res$df
    append_clean_code(input$data_pick, gen_infer_code(res$changed))
    notify(sprintf("Converted %d column%s: %s", length(res$changed),
                   if (length(res$changed) == 1) "" else "s",
                   paste(res$changed, collapse = ", ")))
  })

  # keep the column picker in sync — but only reset the selection when the
  # column SET actually changes (new dataset / drop / rename), so a pure row
  # cleanup doesn't discard a user's in-progress un-tick or reorder.
  prev_cols <- reactiveVal(NULL)
  observe({
    ds <- tryCatch(cur_ds(), error = function(e) NULL)
    cols <- if (is.null(ds)) character() else names(ds$df)
    if (!identical(cols, isolate(prev_cols()))) {
      updateSelectizeInput(session, "data_cols", choices = cols, selected = cols,
                           server = FALSE)
      prev_cols(cols)
    }
  })

  observeEvent(input$data_cols_apply, {
    ds <- tryCatch(cur_ds(), error = function(e) NULL); req(ds)
    sel <- input$data_cols
    # guard against a stale selection from the previously-selected dataset
    # (updateSelectizeInput lags one client round-trip)
    if (!all(sel %in% names(ds$df))) {
      notify("Column list is still updating - click Apply again.", "warning"); return()
    }
    sel <- sel[sel %in% names(ds$df)]
    if (!length(sel)) { notify("Keep at least one column.", "warning"); return() }
    if (identical(sel, names(ds$df))) { notify("Columns unchanged."); return() }
    rv$datasets[[input$data_pick]]$df <- ds$df[, sel, drop = FALSE]
    append_clean_code(input$data_pick, sprintf(
      "\n# keep / reorder columns (applied in the app)\ndf <- df[, c(%s)]",
      paste(vapply(sel, function(s) deparse1(s), character(1)), collapse = ", ")))
    notify(sprintf("Kept %d column%s.", length(sel), if (length(sel) == 1) "" else "s"))
  })

  # copy the current dataset to the clipboard as TSV (spreadsheet-pasteable)
  observeEvent(input$data_copy, {
    ds <- tryCatch(cur_ds(), error = function(e) NULL)
    if (is.null(ds) || nrow(ds$df) == 0) { notify("No dataset to copy.", "warning"); return() }
    m <- as.data.frame(lapply(ds$df, function(col)
      gsub("[\t\r\n]+", " ", as.character(col))), stringsAsFactors = FALSE)
    header <- paste(names(ds$df), collapse = "\t")
    body   <- if (nrow(m)) apply(m, 1, function(r) paste(r, collapse = "\t")) else character()
    tsv    <- paste(c(header, body), collapse = "\n")
    session$sendCustomMessage("copy_text",
                              list(text = tsv,
                                   label = sprintf("%d rows x %d cols", nrow(ds$df), ncol(ds$df))))
  })

  observeEvent(input$data_rename_btn, {
    ds <- tryCatch(cur_ds(), error = function(e) NULL); req(ds)
    nm <- slugify(input$data_rename)
    if (!nzchar(input$data_rename)) { notify("Type a new name first.", "warning"); return() }
    if (nm %in% names(rv$datasets)) { notify("A dataset with that name already exists.", "warning"); return() }
    old <- input$data_pick
    rv$datasets[[nm]] <- rv$datasets[[old]]
    rv$datasets[[old]] <- NULL
    update_data_select(selected = nm)
    updateTextInput(session, "data_rename", value = "")
    notify(sprintf("Renamed '%s' to '%s'.", old, nm))
  })

  observeEvent(input$data_delete, {
    ds <- tryCatch(cur_ds(), error = function(e) NULL); req(ds)
    old <- input$data_pick
    rv$datasets[[old]] <- NULL
    update_data_select()
    notify(sprintf("Deleted '%s'.", old))
  })

  # keep download buttons enabled only when they can work
  observe({
    has_ds <- nzchar(input$data_pick %||% "") &&
      input$data_pick %in% names(rv$datasets)
    shinyjs::toggleState("dl_dataset", condition = has_ds)
    shinyjs::toggleState("data_delete", condition = has_ds)
    shinyjs::toggleState("data_rename_btn", condition = has_ds)
    shinyjs::toggleState("data_dedupe", condition = has_ds)
    shinyjs::toggleState("data_dropcols", condition = has_ds)
    shinyjs::toggleState("data_droprows", condition = has_ds)
    shinyjs::toggleState("data_types", condition = has_ds)
    shinyjs::toggleState("data_cols_apply", condition = has_ds)
    shinyjs::toggleState("data_copy", condition = has_ds)
    if (HAS_WRITEXL) shinyjs::toggleState("dl_all", condition = length(rv$datasets) > 0)
  })

  output$dl_dataset <- downloadHandler(
    filename = function() {
      ext <- switch(input$dl_format, csv = "csv", tsv = "tsv", xlsx = "xlsx",
                    json = "json", rds = "rds", "csv")
      paste0(input$data_pick, "_", format(Sys.time(), "%Y%m%d_%H%M"), ".", ext)
    },
    content = function(file) {
      ds <- rv$datasets[[input$data_pick]]
      df <- ds$df
      switch(input$dl_format,
        csv  = utils::write.csv(df, file, row.names = FALSE, na = ""),
        tsv  = utils::write.table(df, file, sep = "\t", row.names = FALSE,
                                  na = "", quote = TRUE),
        xlsx = writexl::write_xlsx(stats::setNames(list(df),
                                                   substr(input$data_pick, 1, 31)), file),
        json = jsonlite::write_json(df, file, dataframe = "rows",
                                    auto_unbox = TRUE, pretty = TRUE, na = "null"),
        rds  = saveRDS(df, file),
        utils::write.csv(df, file, row.names = FALSE, na = "")
      )
    }
  )

  output$dl_all <- downloadHandler(
    filename = function() {
      paste0("scraped_datasets_", format(Sys.time(), "%Y%m%d_%H%M"), ".xlsx")
    },
    content = function(file) {
      dfs <- lapply(rv$datasets, function(d) d$df)
      sheet_names <- make.unique(substr(gsub("[\\[\\]:*?/\\\\]", "_",
                                             names(dfs)), 1, 28), sep = "_")
      writexl::write_xlsx(stats::setNames(dfs, sheet_names), file)
    }
  )

  # --------------------------------------------------------------------------
  # History tab
  # --------------------------------------------------------------------------
  output$history_dt <- DT::renderDT({
    validate(need(nrow(rv$history) > 0,
                  "No activity yet - everything you analyze and extract gets logged here."))
    dt_table(rv$history[rev(seq_len(nrow(rv$history))), ], page_len = 15)
  })

  output$dl_history <- downloadHandler(
    filename = function() paste0("scrape_log_", format(Sys.time(), "%Y%m%d_%H%M"), ".csv"),
    content = function(file) utils::write.csv(rv$history, file, row.names = FALSE, na = "")
  )

  observeEvent(input$history_clear, {
    rv$history <- rv$history[0, ]
    notify("History cleared.")
  })
}
