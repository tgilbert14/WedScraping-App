# ============================================================================
# scraper_engine.R
# Core engine for the Interactive Web Scraper:
#   fetch  -> analyze -> discover -> extract -> generate reproducible R code
# All functions return plain R objects (lists / tibbles) so they can be used
# from the Shiny app OR sourced directly in a script.
# ============================================================================

BROWSER_UA <- paste0(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 ",
  "(KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36"
)

# Tags that never contain scrape-worthy repeated content
BORING_TAGS <- c(
  "path", "svg", "use", "circle", "rect", "g", "line", "polygon", "polyline",
  "defs", "clippath", "br", "hr", "source", "meta", "link", "style", "script",
  "noscript", "template", "symbol", "stop", "ellipse", "mask", "filter"
)

`%||%` <- function(x, y) if (is.null(x) || (length(x) == 1 && is.na(x))) y else x

# Render x as a syntactically-valid double-quoted R string literal (escapes
# quotes, backslashes, etc.). EVERY user/site-derived value interpolated into
# generated code must go through this.
r_str <- function(x) deparse1(as.character(x %||% ""))

# ---------------------------------------------------------------------------
# URL helpers
# ---------------------------------------------------------------------------

clean_url <- function(url) {
  url <- stringr::str_squish(url %||% "")
  if (url == "") return(url)
  if (!stringr::str_detect(url, "^[A-Za-z][A-Za-z0-9+.-]*://")) {
    url <- paste0("https://", url)
  }
  url
}

validate_url <- function(url) {
  url <- clean_url(url)
  if (url == "") {
    return(list(ok = FALSE, url = url, message = "Please enter a URL."))
  }
  if (!stringr::str_detect(url, "^https?://")) {
    return(list(ok = FALSE, url = url,
                message = "Only http:// and https:// URLs are supported."))
  }
  if (!stringr::str_detect(url, "^https?://[^\\s/]+\\.[^\\s/]{2,}")) {
    return(list(ok = FALSE, url = url,
                message = "That doesn't look like a full web address (e.g. https://example.com)."))
  }
  list(ok = TRUE, url = url, message = "")
}

url_domain <- function(url) {
  m <- stringr::str_match(url %||% "", "^https?://(?:www\\.)?([^/:]+)")[, 2]
  ifelse(is.na(m), "", m)
}

# ---------------------------------------------------------------------------
# PAGINATION - find the "next page" link so a list can be scraped in full
# ---------------------------------------------------------------------------

# Extract the page number encoded in a URL (?page=N / ?Page=N / /page/N /
# /page-N), or NA if none. Case-insensitive.
url_page_num <- function(u) {
  u <- u %||% ""
  m <- stringr::str_match(u, "(?i)[?&]page=(\\d+)")[, 2]
  if (is.na(m)) m <- stringr::str_match(u, "(?i)/page[-/](\\d+)")[, 2]
  if (is.na(m)) NA_integer_ else as.integer(m)
}

# Return the absolute URL of the next page, or NA if there isn't one.
# Tries, in order: <link rel=next>, <a rel=next>, common "next" controls, a
# next-glyph anchor, and finally a numbered-pagination link to page (current+1).
next_page_url <- function(page, current_url) {
  pick <- function(node) {
    if (inherits(node, "xml_missing") || length(node) == 0) return(NA_character_)
    href <- rvest::html_attr(node[[1]], "href")
    if (is.na(href) || !nzchar(href) || stringr::str_detect(href, "^(#|javascript:)")) {
      return(NA_character_)
    }
    abs <- xml2::url_absolute(href, current_url)
    if (identical(abs, current_url)) NA_character_ else abs
  }

  # 1. semantic rel=next
  for (sel in c("link[rel='next']", "a[rel='next']")) {
    u <- pick(rvest::html_elements(page, sel))
    if (!is.na(u)) return(u)
  }
  # 2. common "next" controls (class / aria). NB: rvest's selectr CSS parser
  # rejects the case-insensitive `i` attribute flag, so match both casings.
  for (sel in c("a.next[href]", "li.next a[href]", ".next > a[href]",
                ".pagination-next a[href]", "a.pagination__next[href]",
                "a[aria-label*='Next'][href]", "a[aria-label*='next'][href]",
                "a[title*='Next'][href]", "a[rel~='next'][href]")) {
    u <- pick(rvest::html_elements(page, sel))
    if (!is.na(u)) return(u)
  }
  # 3. an anchor whose visible text is a next-glyph / "next"
  anchors <- rvest::html_elements(page, "a[href]")
  if (length(anchors)) {
    txt <- stringr::str_squish(tolower(rvest::html_text(anchors)))
    is_next <- txt %in% c("next", "next page", "next »", "next ›", "»", "›", ">",
                          "older", "older posts", "next ›", "next »")
    if (any(is_next)) {
      u <- pick(anchors[which(is_next)[1]])
      if (!is.na(u)) return(u)
    }
  }
  # 4. numbered pagination: a link to page (current + 1). Catches ?Page=N
  # sites (e.g. 247Sports) that have no rel=next / .next control.
  if (length(anchors)) {
    hrefs <- rvest::html_attr(anchors, "href")
    want  <- (url_page_num(current_url) %||% 1L)
    want  <- (if (is.na(want)) 1L else want) + 1L
    nums  <- vapply(hrefs, function(h) {
      abs <- if (is.na(h)) NA_character_ else xml2::url_absolute(h, current_url)
      url_page_num(abs)
    }, integer(1))
    hit <- which(!is.na(nums) & nums == want)
    if (length(hit)) {
      u <- pick(anchors[hit[1]])
      if (!is.na(u)) return(u)
    }
  }
  NA_character_
}

# Run `extract_fn(page, url)` across up to `max_pages` pages, following the
# next-page link each time, and stack the rows. `extract_fn` returns a tibble.
# `on_progress(i, n_rows)` is an optional callback for UI feedback.
scrape_paginated <- function(first_page, first_url, extract_fn,
                             max_pages = 5, render_js = FALSE, delay_s = 0.6,
                             on_progress = NULL, tag_page = TRUE) {
  max_pages <- max(1, min(as.integer(max_pages %||% 1), 50))
  visited <- character()
  frames <- list()
  page <- first_page
  url  <- first_url
  i <- 0L
  repeat {
    i <- i + 1L
    df <- tryCatch(extract_fn(page, url), error = function(e) tibble::tibble())
    if (is.data.frame(df) && nrow(df) > 0) {
      if (isTRUE(tag_page)) df$page <- i
      frames[[length(frames) + 1]] <- df
    }
    if (is.function(on_progress)) on_progress(i, sum(vapply(frames, nrow, integer(1))))
    visited <- c(visited, url)
    if (i >= max_pages) break

    nxt <- tryCatch(next_page_url(page, url), error = function(e) NA_character_)
    if (is.na(nxt) || nxt %in% visited) break

    Sys.sleep(delay_s)  # be polite between page fetches
    f <- fetch_page(nxt, render_js = render_js)
    if (!isTRUE(f$ok) || is.null(f$page)) break
    page <- f$page
    url  <- f$final_url %||% nxt
  }

  out <- dplyr::bind_rows(frames)
  # a single scraped page shouldn't carry a constant page=1 column
  if (isTRUE(tag_page) && "page" %in% names(out) &&
      length(unique(out$page)) <= 1) {
    out$page <- NULL
  }
  attr(out, "pages_scraped") <- i
  out
}

# ---------------------------------------------------------------------------
# FETCH
# ---------------------------------------------------------------------------

friendly_http_message <- function(status) {
  msg <- dplyr::case_when(
    status == 200 ~ "OK - page fetched successfully.",
    status == 403 ~ "403 Forbidden - the site refused the request. Try the 'Render with headless Chrome' option.",
    status == 404 ~ "404 Not Found - check the URL for typos.",
    status == 429 ~ "429 Too Many Requests - the site is rate-limiting you. Wait a bit and retry.",
    status >= 500 ~ paste0(status, " Server error - the problem is on their end. Retry later."),
    status >= 400 ~ paste0(status, " Client error - the site rejected the request."),
    status >= 300 ~ paste0(status, " Redirect that could not be followed."),
    TRUE          ~ paste0("HTTP status ", status, ".")
  )
  msg
}

fetch_page <- function(url, render_js = FALSE, timeout_s = 20) {
  if (isTRUE(render_js)) return(fetch_page_chromote(url, timeout_s = max(timeout_s, 30)))

  base <- list(url = url, final_url = url, status = NA_integer_,
               content_type = "", method = "httr", fetched_at = Sys.time(),
               ok = FALSE, is_json = FALSE, page = NULL, raw_text = NULL,
               error = NULL)

  res <- tryCatch(
    httr::RETRY(
      "GET", url,
      httr::user_agent(BROWSER_UA),
      httr::add_headers(
        Accept = "text/html,application/xhtml+xml,application/xml;q=0.9,application/json;q=0.8,*/*;q=0.7",
        `Accept-Language` = "en-US,en;q=0.9"
      ),
      httr::timeout(timeout_s),
      times = 3, pause_base = 2, pause_cap = 6,
      terminate_on = c(400, 401, 403, 404, 410),
      quiet = TRUE
    ),
    error = function(e) e
  )

  if (inherits(res, "error")) {
    base$error <- paste0("Request failed: ", conditionMessage(res))
    return(base)
  }

  base$status       <- httr::status_code(res)
  base$final_url    <- res$url %||% url
  base$content_type <- httr::headers(res)[["content-type"]] %||% ""
  base$ok           <- base$status >= 200 && base$status < 300

  if (!base$ok) {
    base$error <- friendly_http_message(base$status)
    return(base)
  }

  txt <- tryCatch(
    suppressMessages(suppressWarnings(
      httr::content(res, as = "text", encoding = "UTF-8")
    )),
    error = function(e) NULL
  )
  if (is.null(txt) || !nzchar(txt)) {
    base$ok <- FALSE
    base$error <- "The response body was empty or could not be decoded as text."
    return(base)
  }
  base$raw_text <- txt

  if (grepl("json", base$content_type, ignore.case = TRUE) ||
      (grepl("^\\s*[\\[{]", txt) && !grepl("<html", txt, ignore.case = TRUE))) {
    base$is_json <- TRUE
    return(base)
  }

  page <- tryCatch(xml2::read_html(txt), error = function(e) NULL)
  if (is.null(page)) {
    base$ok <- FALSE
    base$error <- "Fetched the URL but could not parse the content as HTML."
    return(base)
  }
  base$page <- page
  base
}

fetch_page_chromote <- function(url, timeout_s = 30, settle_s = 2) {
  base <- list(url = url, final_url = url, status = NA_integer_,
               content_type = "text/html (rendered by headless Chrome)",
               method = "chromote", fetched_at = Sys.time(),
               ok = FALSE, is_json = FALSE, page = NULL, raw_text = NULL,
               error = NULL)

  if (!requireNamespace("chromote", quietly = TRUE)) {
    base$error <- "The 'chromote' package is not installed - install it (and Chrome) to render JS-heavy sites."
    return(base)
  }

  b <- tryCatch(chromote::ChromoteSession$new(), error = function(e) e)
  if (inherits(b, "error")) {
    base$error <- paste0("Could not start headless Chrome: ", conditionMessage(b))
    return(base)
  }
  on.exit(try(b$close(), silent = TRUE), add = TRUE)

  out <- tryCatch({
    b$Page$enable()
    b$Runtime$enable()
    b$Page$navigate(url)

    start <- Sys.time()
    repeat {
      state <- tryCatch(
        b$Runtime$evaluate("document.readyState")$result$value,
        error = function(e) ""
      )
      if (identical(state, "complete")) break
      if (as.numeric(difftime(Sys.time(), start, units = "secs")) > timeout_s) break
      Sys.sleep(0.4)
    }
    Sys.sleep(settle_s)  # give client-side rendering a moment to settle

    html <- b$Runtime$evaluate(
      "document.documentElement.outerHTML", returnByValue = TRUE
    )$result$value
    final <- tryCatch(
      b$Runtime$evaluate("location.href")$result$value,
      error = function(e) url
    )
    list(html = html, final = final)
  }, error = function(e) e)

  if (inherits(out, "error")) {
    base$error <- paste0("Headless Chrome failed: ", conditionMessage(out))
    return(base)
  }
  if (is.null(out$html) || !nzchar(out$html)) {
    base$error <- "Headless Chrome returned an empty page."
    return(base)
  }

  page <- tryCatch(xml2::read_html(out$html), error = function(e) NULL)
  if (is.null(page)) {
    base$error <- "Could not parse the rendered page as HTML."
    return(base)
  }

  base$ok        <- TRUE
  base$status    <- 200L
  base$final_url <- out$final %||% url
  base$raw_text  <- out$html
  base$page      <- page
  base
}

# ---------------------------------------------------------------------------
# ANALYZE - site fingerprinting
# ---------------------------------------------------------------------------

meta_content <- function(page, css) {
  v <- rvest::html_attr(rvest::html_element(page, css), "content")
  if (length(v) == 0 || is.na(v)) return(NA_character_)
  v <- stringr::str_squish(v)
  if (!nzchar(v)) NA_character_ else v
}

detect_tech <- function(page) {
  tech <- character()
  if (length(rvest::html_elements(page, "script#__NEXT_DATA__")) > 0)
    tech <- c(tech, "Next.js (React)")
  if (length(rvest::html_elements(page, "script[src*='wp-content'], link[href*='wp-content']")) > 0)
    tech <- c(tech, "WordPress")
  if (length(rvest::html_elements(page, "[data-drupal-selector], script[src*='drupal']")) > 0)
    tech <- c(tech, "Drupal")
  if (length(rvest::html_elements(page, "[data-reactroot], #___gatsby")) > 0)
    tech <- c(tech, "React")
  if (length(rvest::html_elements(page, "[data-v-app], #__nuxt")) > 0)
    tech <- c(tech, "Vue / Nuxt")
  if (length(rvest::html_elements(page, "[ng-version]")) > 0)
    tech <- c(tech, "Angular")
  if (length(rvest::html_elements(page, "script[src*='shopify'], link[href*='cdn.shopify']")) > 0)
    tech <- c(tech, "Shopify")
  if (length(rvest::html_elements(page, "script[src*='squarespace']")) > 0)
    tech <- c(tech, "Squarespace")
  if (length(rvest::html_elements(page, "meta[name='generator']")) > 0) {
    gen <- meta_content(page, "meta[name='generator']")
    if (!is.na(gen)) tech <- c(tech, gen)
  }
  unique(tech)
}

detect_schema_types <- function(page) {
  types <- character()
  it <- rvest::html_attr(rvest::html_elements(page, "[itemtype]"), "itemtype")
  if (length(it)) types <- c(types, basename(unique(it)))
  ld <- rvest::html_elements(page, "script[type='application/ld+json']")
  for (node in ld) {
    x <- tryCatch(jsonlite::fromJSON(rvest::html_text(node), simplifyVector = FALSE),
                  error = function(e) NULL)
    grab_type <- function(o) {
      if (is.list(o)) {
        t <- o[["@type"]]
        if (!is.null(t)) types <<- c(types, unlist(t))
        for (child in o) if (is.list(child)) grab_type(child)
      }
    }
    if (!is.null(x)) grab_type(x)
  }
  unique(types[!is.na(types) & nzchar(types)])
}

classify_site <- function(page, url, counts, recipe, schema_types, tech) {
  domain <- url_domain(url)
  if (!is.null(recipe)) return(recipe$site_type %||% recipe$label)
  if (grepl("wikipedia\\.org$", domain)) return("Wikipedia article")
  if (any(grepl("Product|Offer", schema_types, ignore.case = TRUE)) ||
      any(grepl("Shopify", tech)))
    return("E-commerce / product page")
  if (any(grepl("NewsArticle|Article|BlogPosting", schema_types)))
    return("News / article page")
  og_type <- meta_content(page, "meta[property='og:type']")
  if (!is.na(og_type) && grepl("article", og_type, ignore.case = TRUE))
    return("News / article page")
  if (counts$tables >= 2) return("Table-based data page")
  if (counts$tables == 1) return("Page with a data table")
  if (counts$cards   >= 1) return("Card / list directory page")
  if (counts$forms   >= 2) return("Form-heavy page")
  "General web page"
}

analyze_page <- function(fetch, disc = NULL) {
  page <- fetch$page
  url  <- fetch$final_url %||% fetch$url

  body_text <- tryCatch(
    stringr::str_squish(rvest::html_text(rvest::html_element(page, "body"))),
    error = function(e) ""
  )
  body_text <- body_text %||% ""

  counts <- list(
    tables   = length(rvest::html_elements(page, "table")),
    links    = length(rvest::html_elements(page, "a[href]")),
    images   = length(rvest::html_elements(page, "img")),
    forms    = length(rvest::html_elements(page, "form")),
    lists    = length(rvest::html_elements(page, "ul, ol")),
    scripts  = length(rvest::html_elements(page, "script")),
    iframes  = length(rvest::html_elements(page, "iframe")),
    headings = length(rvest::html_elements(page, "h1, h2, h3, h4")),
    cards    = if (!is.null(disc)) nrow(disc$cards) else 0L,
    json     = if (!is.null(disc)) length(disc$json) else 0L
  )

  tech         <- detect_tech(page)
  schema_types <- detect_schema_types(page)
  recipe       <- find_recipe(url, page)

  js_shell <- nchar(body_text) < 400 && counts$scripts > 5

  list(
    url          = url,
    domain       = url_domain(url),
    title        = stringr::str_squish(rvest::html_text(rvest::html_element(page, "title")) %||% ""),
    description  = meta_content(page, "meta[name='description'], meta[property='og:description']"),
    og_site      = meta_content(page, "meta[property='og:site_name']"),
    tech         = tech,
    schema_types = schema_types,
    counts       = counts,
    js_shell     = js_shell,
    recipe       = recipe,
    site_type    = classify_site(page, url, counts, recipe, schema_types, tech),
    text_chars   = nchar(body_text)
  )
}

analyze_json_endpoint <- function(fetch) {
  list(
    url          = fetch$final_url %||% fetch$url,
    domain       = url_domain(fetch$final_url %||% fetch$url),
    title        = "JSON response",
    description  = NA_character_,
    og_site      = NA_character_,
    tech         = character(),
    schema_types = character(),
    counts       = list(tables = 0, links = 0, images = 0, forms = 0, lists = 0,
                        scripts = 0, iframes = 0, headings = 0, cards = 0, json = 1),
    js_shell     = FALSE,
    recipe       = NULL,
    site_type    = "JSON API endpoint",
    text_chars   = nchar(fetch$raw_text %||% "")
  )
}

# ---------------------------------------------------------------------------
# DISCOVER - find scrapable structures
# ---------------------------------------------------------------------------

empty_tables_tbl <- function() {
  tibble::tibble(index = integer(), rows = integer(), cols = integer(),
                 caption = character(), columns = character())
}

discover_tables <- function(page) {
  tabs <- rvest::html_elements(page, "table")
  if (!length(tabs)) return(empty_tables_tbl())
  out <- purrr::map(seq_along(tabs), function(i) {
    df <- tryCatch(rvest::html_table(tabs[[i]], fill = TRUE),
                   error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(NULL)
    cap <- rvest::html_text(rvest::html_element(tabs[[i]], "caption"))
    cap <- if (length(cap) == 0 || is.na(cap)) "" else stringr::str_squish(cap)
    cols <- names(df)
    cols <- cols[nzchar(cols)]
    tibble::tibble(
      index   = i,
      rows    = nrow(df),
      cols    = ncol(df),
      caption = cap,
      columns = paste(utils::head(cols, 6), collapse = ", ")
    )
  })
  out <- dplyr::bind_rows(out)
  if (nrow(out) == 0) empty_tables_tbl() else out
}

scrape_table <- function(page, index, promote_header = FALSE) {
  tabs <- rvest::html_elements(page, "table")
  if (index < 1 || index > length(tabs)) return(tibble::tibble())
  df <- tryCatch(rvest::html_table(tabs[[index]], fill = TRUE),
                 error = function(e) tibble::tibble())
  if (isTRUE(promote_header) && nrow(df) >= 2) {
    new_names <- as.character(unlist(df[1, ], use.names = FALSE))
    new_names[!nzchar(new_names) | is.na(new_names)] <- paste0("col_", which(!nzchar(new_names) | is.na(new_names)))
    df <- df[-1, , drop = FALSE]
    names(df) <- make.unique(new_names)
  }
  names(df) <- make.unique(ifelse(nzchar(names(df)), names(df),
                                  paste0("col_", seq_along(names(df)))))
  tibble::as_tibble(df, .name_repair = "minimal")
}

# --- repeated structures ("cards") -----------------------------------------

xpath_for_signature <- function(tag, class) {
  sprintf("//%s[@class='%s']", tag, class)
}

xpath_for_class_token <- function(token) {
  sprintf(".//*[contains(concat(' ', normalize-space(@class), ' '), ' %s ')]", token)
}

empty_cards_tbl <- function() {
  tibble::tibble(id = character(), label = character(), tag = character(),
                 class = character(), xpath = character(), n = integer(),
                 sample = character(), score = numeric())
}

discover_cards <- function(page, min_count = 3, max_nodes = 8000,
                           max_candidates = 80, max_groups = 40) {
  nodes <- rvest::html_elements(page, "*[class]")
  if (!length(nodes)) return(empty_cards_tbl())
  if (length(nodes) > max_nodes) nodes <- nodes[seq_len(max_nodes)]

  tags <- rvest::html_name(nodes)
  cls  <- stringr::str_squish(rvest::html_attr(nodes, "class"))
  # quotes break the xpath we build; backslashes break the generated R code
  keep <- !is.na(cls) & nzchar(cls) &
    !stringr::str_detect(cls, "[\"'\\\\]") &
    !(tags %in% BORING_TAGS)
  if (!any(keep)) return(empty_cards_tbl())

  sig    <- paste0(tags[keep], "|", cls[keep])
  counts <- sort(table(sig), decreasing = TRUE)
  counts <- counts[counts >= min_count]
  if (!length(counts)) return(empty_cards_tbl())
  counts <- utils::head(counts, max_candidates)

  parts <- stringr::str_split_fixed(names(counts), stringr::fixed("|"), 2)
  cand  <- tibble::tibble(tag = parts[, 1], class = parts[, 2],
                          n = as.integer(counts))

  stats <- purrr::pmap(cand, function(tag, class, n) {
    xp    <- xpath_for_signature(tag, class)
    items <- tryCatch(rvest::html_elements(page, xpath = xp),
                      error = function(e) NULL)
    if (is.null(items) || !length(items)) return(NULL)
    smp   <- utils::head(items, 10)
    texts <- stringr::str_squish(rvest::html_text(smp))
    med   <- stats::median(nchar(texts))
    kids  <- length(rvest::html_elements(smp[[1]], xpath = ".//*"))
    tibble::tibble(
      tag = tag, class = class, xpath = xp, n = n,
      med_len = med, kids = kids,
      sample = substr(paste(utils::head(texts[nzchar(texts)], 2), collapse = "  |  "), 1, 160)
    )
  })
  stats <- dplyr::bind_rows(stats)
  if (nrow(stats) == 0) return(empty_cards_tbl())

  stats <- stats %>%
    dplyr::filter(med_len >= 5) %>%
    dplyr::mutate(score = n * log1p(med_len) * log1p(kids + 1)) %>%
    dplyr::arrange(dplyr::desc(score)) %>%
    utils::head(max_groups)
  if (nrow(stats) == 0) return(empty_cards_tbl())

  first_token <- stringr::str_extract(stats$class, "^[^ ]+")
  stats %>%
    dplyr::mutate(
      id    = paste0("card_", dplyr::row_number()),
      label = sprintf("<%s class=\"%s\">  -  %d items", tag, first_token, n)
    ) %>%
    dplyr::select(id, label, tag, class, xpath, n, sample, score)
}

# Detect candidate "fields" (columns) inside a repeated card group.
# Regular fields are class tokens; "@heading" is a pseudo-field for the first
# h1-h6 inside each item (titles often carry no class at all).
detect_card_fields <- function(page, xpath, sample_n = 12, max_fields = 12) {
  items <- tryCatch(rvest::html_elements(page, xpath = xpath),
                    error = function(e) NULL)
  if (is.null(items) || !length(items)) return(character())
  smp <- utils::head(items, sample_n)

  per_item <- purrr::imap(smp, function(it, i) {
    kids <- rvest::html_elements(it, xpath = ".//*[@class]")
    if (!length(kids)) return(NULL)
    toks <- purrr::map_chr(
      stringr::str_split(stringr::str_squish(rvest::html_attr(kids, "class")), " "),
      1
    )
    txts <- stringr::str_squish(rvest::html_text(kids))
    ok <- nzchar(toks) & !stringr::str_detect(toks, "['\"\\\\]")
    if (!any(ok)) return(NULL)
    tibble::tibble(item = i, token = toks[ok], text = txts[ok]) %>%
      dplyr::distinct(token, .keep_all = TRUE)
  })
  stats <- dplyr::bind_rows(per_item)

  n_items <- length(smp)
  tokens <- character()
  if (nrow(stats) > 0) {
    tokens <- stats %>%
      dplyr::group_by(token) %>%
      dplyr::summarise(
        coverage = dplyr::n() / n_items,
        nonempty = mean(nzchar(text)),
        avg_len  = mean(nchar(text)),
        distinctness = dplyr::n_distinct(text) / dplyr::n(),
        .groups = "drop"
      ) %>%
      dplyr::filter(coverage >= 0.5, nonempty >= 0.5, avg_len >= 1, avg_len <= 400) %>%
      dplyr::arrange(dplyr::desc(coverage * nonempty * (0.25 + 0.75 * distinctness))) %>%
      utils::head(max_fields) %>%
      dplyr::pull(token)
  }

  # heading pseudo-field: many card titles are unclassed <h3><a>...</a></h3>
  heading_cov <- mean(vapply(smp, function(it) {
    h <- rvest::html_element(it, xpath = ".//h1|.//h2|.//h3|.//h4|.//h5|.//h6")
    !inherits(h, "xml_missing") &&
      nzchar(stringr::str_squish(rvest::html_text(h)))
  }, logical(1)))
  if (heading_cov >= 0.5) tokens <- c("@heading", tokens)

  tokens
}

# Turn a raw class token into a readable column name
pretty_field_name <- function(tok) {
  x <- stringr::str_remove(tok, "__[A-Za-z0-9]{4,10}$")  # CSS-module hash
  x <- stringr::str_replace_all(x, "__", "_")
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9]+", "_")
  x <- stringr::str_remove_all(x, "^_+|_+$")
  if (!nzchar(x)) x <- tok
  x
}

scrape_cards <- function(page, xpath, tokens = NULL, base_url = NULL,
                         include_link = TRUE, include_image = FALSE,
                         include_full_text = FALSE) {
  items <- tryCatch(rvest::html_elements(page, xpath = xpath),
                    error = function(e) NULL)
  if (is.null(items) || !length(items)) return(tibble::tibble())
  if (is.null(tokens)) tokens <- detect_card_fields(page, xpath)
  tokens <- tokens[nzchar(tokens) & !stringr::str_detect(tokens, "['\"\\\\]")]

  rows <- purrr::map(items, function(it) {
    vals <- list()
    for (t in tokens) {
      if (identical(t, "@heading")) {
        h <- rvest::html_element(it, xpath = ".//h1|.//h2|.//h3|.//h4|.//h5|.//h6")
        # prefer the title attribute of a link inside the heading (often the
        # full, untruncated text), else the heading text itself
        val <- NA_character_
        if (!inherits(h, "xml_missing")) {
          a <- rvest::html_element(h, xpath = ".//a[@title]")
          val <- if (!inherits(a, "xml_missing")) {
            stringr::str_squish(rvest::html_attr(a, "title"))
          } else {
            stringr::str_squish(rvest::html_text(h))
          }
        }
        vals[["@heading"]] <- val
        next
      }
      node <- rvest::html_element(it, xpath = xpath_for_class_token(t))
      vals[[t]] <- if (inherits(node, "xml_missing")) NA_character_ else
        stringr::str_squish(rvest::html_text(node))
    }
    if (include_link) {
      a <- rvest::html_element(it, xpath = ".//a[@href]")
      if (inherits(a, "xml_missing")) {
        vals[[".link_text"]] <- NA_character_
        vals[[".link"]]      <- NA_character_
      } else {
        vals[[".link_text"]] <- stringr::str_squish(rvest::html_text(a))
        vals[[".link"]]      <- rvest::html_attr(a, "href")
      }
    }
    if (include_image) {
      img <- rvest::html_element(it, xpath = ".//img[@src]")
      vals[[".image"]] <- if (inherits(img, "xml_missing")) NA_character_ else
        rvest::html_attr(img, "src")
    }
    if (include_full_text || length(tokens) == 0) {
      vals[[".full_text"]] <- stringr::str_squish(rvest::html_text(it))
    }
    tibble::as_tibble(vals)
  })
  out <- dplyr::bind_rows(rows)
  if (nrow(out) == 0) return(out)

  if (!is.null(base_url)) {
    for (col in intersect(c(".link", ".image"), names(out))) {
      out[[col]] <- ifelse(is.na(out[[col]]), NA_character_,
                           xml2::url_absolute(out[[col]], base_url))
    }
  }

  new_names <- names(out)
  pretty <- vapply(new_names, function(nm) {
    if (startsWith(nm, "."))      sub("^\\.", "", nm)
    else if (startsWith(nm, "@")) sub("^@", "", nm)
    else                          pretty_field_name(nm)
  }, character(1))
  names(out) <- make.unique(pretty, sep = "_")
  out
}

# --- lists ------------------------------------------------------------------

discover_lists <- function(page, min_items = 4, max_groups = 30) {
  ls <- rvest::html_elements(page, "ul, ol")
  if (!length(ls)) {
    return(tibble::tibble(index = integer(), tag = character(), items = integer(),
                          context = character(), sample = character()))
  }
  out <- purrr::map(seq_along(ls), function(i) {
    l  <- ls[[i]]
    li <- rvest::html_elements(l, xpath = "./li")
    if (length(li) < min_items) return(NULL)
    in_nav <- length(rvest::html_elements(
      l, xpath = "ancestor::nav | ancestor::footer | ancestor::header")) > 0
    texts <- stringr::str_squish(rvest::html_text(utils::head(li, 3)))
    tibble::tibble(
      index   = i,
      tag     = rvest::html_name(l),
      items   = length(li),
      context = if (in_nav) "navigation / chrome" else "page content",
      sample  = substr(paste(texts[nzchar(texts)], collapse = "  |  "), 1, 140)
    )
  })
  out <- dplyr::bind_rows(out)
  if (nrow(out) == 0) return(tibble::tibble(index = integer(), tag = character(),
                                            items = integer(), context = character(),
                                            sample = character()))
  dplyr::arrange(out, dplyr::desc(context == "page content"), dplyr::desc(items)) %>%
    utils::head(max_groups)
}

scrape_list <- function(page, index, base_url = NULL) {
  ls <- rvest::html_elements(page, "ul, ol")
  if (index < 1 || index > length(ls)) return(tibble::tibble())
  li <- rvest::html_elements(ls[[index]], xpath = "./li")
  hrefs <- purrr::map_chr(li, function(x) {
    a <- rvest::html_element(x, xpath = ".//a[@href]")
    if (inherits(a, "xml_missing")) NA_character_ else rvest::html_attr(a, "href")
  })
  if (!is.null(base_url)) {
    hrefs <- ifelse(is.na(hrefs), NA_character_, xml2::url_absolute(hrefs, base_url))
  }
  tibble::tibble(
    item = stringr::str_squish(rvest::html_text(li)),
    link = hrefs
  )
}

# --- links / images / headings ----------------------------------------------

scrape_links <- function(page, base_url = NULL, max_links = 3000) {
  a <- rvest::html_elements(page, "a[href]")
  if (!length(a)) return(tibble::tibble(text = character(), href = character(),
                                        url = character(), internal = logical()))
  a <- utils::head(a, max_links)
  href <- rvest::html_attr(a, "href")
  txt  <- stringr::str_squish(rvest::html_text(a))
  url  <- if (!is.null(base_url)) xml2::url_absolute(href, base_url) else href
  dom  <- url_domain(base_url %||% "")
  # compare the link's own domain (suffix match covers subdomains) - a mere
  # "contains" test would flag other-site links carrying ?ref=<our-domain>
  link_dom <- url_domain(url %||% "")
  tibble::tibble(
    text = txt,
    href = href,
    url  = url,
    internal = if (nzchar(dom)) {
      !is.na(link_dom) & endsWith(link_dom, dom)
    } else NA
  ) %>%
    dplyr::filter(!stringr::str_detect(href, "^(javascript:|#$)")) %>%
    dplyr::distinct(url, text, .keep_all = TRUE)
}

scrape_images <- function(page, base_url = NULL, max_imgs = 1000) {
  imgs <- rvest::html_elements(page, "img[src]")
  if (!length(imgs)) return(tibble::tibble(alt = character(), src = character(),
                                           url = character()))
  imgs <- utils::head(imgs, max_imgs)
  src <- rvest::html_attr(imgs, "src")
  tibble::tibble(
    alt = stringr::str_squish(rvest::html_attr(imgs, "alt") %||% ""),
    src = src,
    url = if (!is.null(base_url)) xml2::url_absolute(src, base_url) else src
  ) %>% dplyr::distinct(url, .keep_all = TRUE)
}

scrape_headings <- function(page) {
  hs <- rvest::html_elements(page, "h1, h2, h3, h4, h5")
  if (!length(hs)) return(tibble::tibble(level = character(), text = character()))
  tibble::tibble(
    level = rvest::html_name(hs),
    text  = stringr::str_squish(rvest::html_text(hs))
  ) %>% dplyr::filter(nzchar(text))
}

# --- embedded JSON (Next.js payloads, ld+json, plain <script> JSON) ----------

discover_json <- function(page) {
  blobs <- list()

  nd <- rvest::html_element(page, "script#__NEXT_DATA__")
  if (!inherits(nd, "xml_missing")) {
    txt <- rvest::html_text(nd)
    if (nzchar(txt)) {
      blobs[[length(blobs) + 1]] <- list(
        id = "next_data", label = sprintf("Next.js page data (__NEXT_DATA__, %s chars)",
                                          format(nchar(txt), big.mark = ",")),
        text = txt
      )
    }
  }

  ld <- rvest::html_elements(page, "script[type='application/ld+json']")
  for (i in seq_along(ld)) {
    txt <- rvest::html_text(ld[[i]])
    if (!nzchar(txt)) next
    blobs[[length(blobs) + 1]] <- list(
      id = paste0("ldjson_", i),
      label = sprintf("Structured data #%d (ld+json, %s chars)", i,
                      format(nchar(txt), big.mark = ",")),
      text = txt
    )
  }

  nuxt <- rvest::html_element(page, "script#__NUXT_DATA__")
  if (!inherits(nuxt, "xml_missing")) {
    txt <- rvest::html_text(nuxt)
    if (nzchar(txt)) {
      blobs[[length(blobs) + 1]] <- list(
        id = "nuxt_data", label = sprintf("Nuxt page data (%s chars)",
                                          format(nchar(txt), big.mark = ",")),
        text = txt
      )
    }
  }
  blobs
}

# Walk a parsed JSON structure and harvest every data-frame-like array
json_to_tables <- function(txt, max_tables = 80, max_depth = 10) {
  x <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = TRUE),
                error = function(e) NULL)
  if (is.null(x)) return(list())

  found <- list()
  walk <- function(node, path, depth) {
    if (depth > max_depth || length(found) >= max_tables) return(invisible())
    if (is.data.frame(node)) {
      if (nrow(node) >= 1 && ncol(node) >= 1) {
        label <- paste(path, collapse = " > ")
        if (!nzchar(label)) label <- "(root)"
        found[[label]] <<- node
      }
      return(invisible())
    }
    if (is.list(node)) {
      nms <- names(node)
      for (i in seq_along(node)) {
        nm <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("[[", i, "]]")
        walk(node[[i]], c(path, nm), depth + 1)
      }
    }
    invisible()
  }
  walk(x, character(), 0)

  # prefer bigger tables first
  if (length(found) > 1) {
    found <- found[order(-vapply(found, function(d) nrow(d) * ncol(d), numeric(1)))]
  }
  lapply(found, sanitize_df)
}

# Score JSON record sets for "is this the page's real subject?" - used by
# recipes to pick e.g. the commits list over a site-navigation directory.
score_json_records <- function(tables) {
  if (!length(tables)) return(numeric())
  paths <- names(tables) %||% rep("", length(tables))
  vapply(seq_along(tables), function(i) {
    d <- tables[[i]]
    path <- paths[i]
    s <- nrow(d) * ncol(d)
    cols <- paste(names(d), collapse = " ")
    person_hits <- sum(vapply(
      c("position", "rating", "stars", "height", "weight", "hometown",
        "highschool", "school", "commit", "status", "jersey", "classyear",
        "ranking"),
      function(k) grepl(k, cols, ignore.case = TRUE), logical(1)))
    if (person_hits >= 2) s <- s * 50
    if (grepl("name|title", cols, ignore.case = TRUE)) s <- s * 2
    if (grepl("commit|player|recruit|roster|rank|athlete|item|result|entr",
              path, ignore.case = TRUE)) s <- s * 20
    if (grepl("menu|nav|footer|header|siteurl|sportsdata|menudata|config|locale",
              path, ignore.case = TRUE)) s <- s / 50
    s
  }, numeric(1))
}

# ---------------------------------------------------------------------------
# Custom CSS selector extraction
# ---------------------------------------------------------------------------

scrape_custom <- function(page, selector, mode = "text", attr_name = NULL,
                          base_url = NULL) {
  selector <- stringr::str_squish(selector %||% "")
  if (!nzchar(selector)) {
    return(list(ok = FALSE, error = "Enter a CSS selector to test.", n = 0,
                data = tibble::tibble()))
  }
  nodes <- tryCatch(rvest::html_elements(page, selector), error = function(e) NULL)
  if (is.null(nodes)) {
    return(list(ok = FALSE, error = "That is not a valid CSS selector.", n = 0,
                data = tibble::tibble()))
  }
  if (!length(nodes)) {
    return(list(ok = TRUE, error = NULL, n = 0, data = tibble::tibble()))
  }

  df <- switch(
    mode,
    text = tibble::tibble(text = stringr::str_squish(rvest::html_text(nodes))),
    href = {
      v <- rvest::html_attr(nodes, "href")
      if (!is.null(base_url)) v <- ifelse(is.na(v), v, xml2::url_absolute(v, base_url))
      tibble::tibble(text = stringr::str_squish(rvest::html_text(nodes)), href = v)
    },
    src = {
      v <- rvest::html_attr(nodes, "src")
      if (!is.null(base_url)) v <- ifelse(is.na(v), v, xml2::url_absolute(v, base_url))
      tibble::tibble(src = v)
    },
    html = tibble::tibble(html = vapply(nodes, function(n)
      as.character(n), character(1))),
    attr = {
      an <- stringr::str_squish(attr_name %||% "")
      if (!nzchar(an)) {
        return(list(ok = FALSE, error = "Enter an attribute name (e.g. href, data-id).",
                    n = length(nodes), data = tibble::tibble()))
      }
      tibble::tibble(
        text = stringr::str_squish(rvest::html_text(nodes)),
        value = rvest::html_attr(nodes, an)
      )
    },
    tibble::tibble(text = stringr::str_squish(rvest::html_text(nodes)))
  )
  list(ok = TRUE, error = NULL, n = length(nodes), data = df)
}

# ---------------------------------------------------------------------------
# Data-frame utilities
# ---------------------------------------------------------------------------

# Make any data frame safe for display / CSV: flatten nested frames,
# stringify list-columns.
sanitize_df <- function(df) {
  if (!is.data.frame(df)) return(tibble::tibble())
  df <- tryCatch(jsonlite::flatten(as.data.frame(df, stringsAsFactors = FALSE)),
                 error = function(e) as.data.frame(df, stringsAsFactors = FALSE))
  for (col in names(df)) {
    if (is.list(df[[col]])) {
      df[[col]] <- vapply(df[[col]], function(v) {
        if (is.null(v) || length(v) == 0) return(NA_character_)
        if (is.atomic(v) && length(v) == 1) return(as.character(v))
        out <- tryCatch(
          as.character(jsonlite::toJSON(v, auto_unbox = TRUE, null = "null")),
          error = function(e) paste(as.character(unlist(v)), collapse = "; ")
        )
        substr(out, 1, 2000)
      }, character(1))
    }
  }
  names(df) <- make.unique(ifelse(nzchar(names(df)), names(df),
                                  paste0("col_", seq_along(names(df)))))
  tibble::as_tibble(df, .name_repair = "minimal")
}

# Guess a better type for a character column and coerce it. Only commits when
# the whole non-empty column parses cleanly (no silent NA-coercion). Handles
# numbers with thousands separators / %, integers, and logicals. Returns the
# (possibly unchanged) column.
coerce_column <- function(x) {
  if (!is.character(x)) return(x)
  v <- stringr::str_squish(x)
  v[v %in% c("", "NA", "N/A", "na", "n/a", "-", "--", "null", "NULL")] <- NA
  nonempty <- v[!is.na(v)]
  if (!length(nonempty)) return(x)

  # logical
  low <- tolower(nonempty)
  if (all(low %in% c("true", "false", "yes", "no", "t", "f", "y", "n"))) {
    out <- rep(NA, length(v))
    out[!is.na(v)] <- tolower(stringr::str_squish(v[!is.na(v)])) %in%
      c("true", "yes", "t", "y")
    return(out)
  }

  # numeric: strip thousands separators, currency, trailing %
  stripped <- stringr::str_remove_all(nonempty, "[,$£€\\s]")
  had_pct <- grepl("%$", stripped)
  stripped <- stringr::str_remove(stripped, "%$")
  if (all(grepl("^[+-]?(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?$", stripped))) {
    num_all <- suppressWarnings(as.numeric(
      stringr::str_remove(stringr::str_remove_all(v, "[,$£€\\s]"), "%$")))
    if (any(had_pct)) num_all <- num_all / 100
    # integer if every value is whole
    if (all(is.na(num_all) | num_all == round(num_all))) {
      return(as.integer(round(num_all)))
    }
    return(num_all)
  }
  x
}

# Auto-detect + coerce every column; report which changed type.
infer_types <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) {
    return(list(df = df, changed = character()))
  }
  changed <- character()
  for (nm in names(df)) {
    before <- class(df[[nm]])[1]
    new <- coerce_column(df[[nm]])
    after <- class(new)[1]
    if (!identical(before, after)) {
      df[[nm]] <- new
      changed <- c(changed, sprintf("%s (%s->%s)", nm, before, after))
    }
  }
  list(df = tibble::as_tibble(df), changed = changed)
}

clean_dataset <- function(df, dedupe = FALSE, drop_empty_cols = FALSE,
                          drop_empty_rows = FALSE) {
  if (isTRUE(drop_empty_cols) && ncol(df) > 0) {
    keep <- vapply(df, function(col) {
      v <- as.character(col)
      any(!is.na(v) & nzchar(stringr::str_squish(v)))
    }, logical(1))
    df <- df[, keep, drop = FALSE]
  }
  if (isTRUE(drop_empty_rows) && nrow(df) > 0) {
    keep <- apply(df, 1, function(r) {
      any(!is.na(r) & nzchar(stringr::str_squish(as.character(r))))
    })
    df <- df[keep, , drop = FALSE]
  }
  if (isTRUE(dedupe)) df <- dplyr::distinct(df)
  tibble::as_tibble(df)
}

slugify <- function(x) {
  x <- stringr::str_replace_all(tolower(x %||% "data"), "[^a-z0-9]+", "_")
  x <- stringr::str_remove_all(x, "^_+|_+$")
  if (!nzchar(x)) x <- "data"
  substr(x, 1, 60)
}

# ---------------------------------------------------------------------------
# Reproducible-code generation
# Every generator mirrors its scrape_* counterpart's semantics (columns,
# name repair, absolutization) and escapes ALL interpolated values via r_str()
# so the emitted script always parses, whatever the selector/url contains.
# ---------------------------------------------------------------------------

gen_fetch_code <- function(url, render_js = FALSE) {
  if (isTRUE(render_js)) {
    sprintf(
'library(rvest)
library(chromote)

url <- %s

# This page needed JavaScript rendering (headless Chrome)
b <- ChromoteSession$new()
b$Page$navigate(url)
Sys.sleep(5)  # wait for the page to render
html <- b$Runtime$evaluate("document.documentElement.outerHTML",
                           returnByValue = TRUE)$result$value
b$close()
page <- read_html(html)', r_str(url))
  } else {
    sprintf(
'library(httr)
library(rvest)

url <- %s

resp <- RETRY("GET", url,
              user_agent(%s),
              timeout(20), times = 3, pause_base = 2)
stop_for_status(resp)
page <- read_html(content(resp, as = "text", encoding = "UTF-8"))',
      r_str(url), r_str(BROWSER_UA))
  }
}

gen_table_code <- function(url, index, render_js = FALSE, promote_header = FALSE) {
  promote <- if (isTRUE(promote_header)) '

# promote first row to column names (blank/NA cells become col_N)
new_names <- as.character(unlist(df[1, ], use.names = FALSE))
blank <- !nzchar(new_names) | is.na(new_names)
new_names[blank] <- paste0("col_", which(blank))
df <- df[-1, , drop = FALSE]
names(df) <- make.unique(new_names)' else ""
  paste0(
    gen_fetch_code(url, render_js), sprintf('

# Extract table %d on the page
df <- html_table(html_elements(page, "table")[[%d]], fill = TRUE)%s

# repair blank/duplicate column names (same as the app)
names(df) <- make.unique(ifelse(nzchar(names(df)), names(df),
                                paste0("col_", seq_along(names(df)))))

df', index, index, promote))
}

# the next-page follow helper, emitted into paginated scripts
NEXT_URL_FN <- 'url_page_num <- function(u) {
  u <- if (is.null(u) || is.na(u)) "" else u
  m <- stringr::str_match(u, "(?i)[?&]page=(\\\\d+)")[, 2]
  if (is.na(m)) m <- stringr::str_match(u, "(?i)/page[-/](\\\\d+)")[, 2]
  if (is.na(m)) NA_integer_ else as.integer(m)
}
next_url <- function(page, url) {
  for (sel in c("link[rel=\'next\']", "a[rel=\'next\']", "a.next[href]",
                "li.next a[href]", "a[aria-label*=\'Next\'][href]")) {
    nx <- html_element(page, sel)
    if (!inherits(nx, "xml_missing")) {
      href <- html_attr(nx, "href")
      if (!is.na(href) && nzchar(href)) return(xml2::url_absolute(href, url))
    }
  }
  # numbered pagination: a link to page (current + 1)
  anchors <- html_elements(page, "a[href]")
  if (length(anchors)) {
    want <- (url_page_num(url)); want <- (if (is.na(want)) 1L else want) + 1L
    hrefs <- html_attr(anchors, "href")
    for (h in hrefs) {
      abs <- if (is.na(h)) NA_character_ else xml2::url_absolute(h, url)
      n <- url_page_num(abs)
      if (!is.na(n) && n == want) return(abs)
    }
  }
  NA_character_
}'

gen_cards_code <- function(url, xpath, tokens, render_js = FALSE,
                           include_link = TRUE, include_image = FALSE,
                           include_full_text = FALSE, pages = 1) {
  has_heading <- "@heading" %in% tokens
  cls_tokens  <- setdiff(tokens, "@heading")
  tok_vec <- if (length(cls_tokens)) {
    paste0("c(", paste(vapply(cls_tokens, r_str, character(1)), collapse = ", "), ")")
  } else "character(0)"

  heading_block <- if (has_heading) '
  h <- html_element(it, xpath = ".//h1|.//h2|.//h3|.//h4|.//h5|.//h6")
  vals$heading <- NA_character_
  if (!inherits(h, "xml_missing")) {
    ha <- html_element(h, xpath = ".//a[@title]")   # full, untruncated title
    vals$heading <- if (!inherits(ha, "xml_missing")) {
      str_squish(html_attr(ha, "title"))
    } else str_squish(html_text(h))
  }' else ""

  link_block <- if (isTRUE(include_link)) '
  a <- html_element(it, xpath = ".//a[@href]")
  vals$link_text <- if (inherits(a, "xml_missing")) NA_character_ else str_squish(html_text(a))
  vals$link      <- if (inherits(a, "xml_missing")) NA_character_ else
    xml2::url_absolute(html_attr(a, "href"), url)' else ""

  image_block <- if (isTRUE(include_image)) '
  img <- html_element(it, xpath = ".//img[@src]")
  vals$image <- if (inherits(img, "xml_missing")) NA_character_ else
    xml2::url_absolute(html_attr(img, "src"), url)' else ""

  fulltext_block <- if (isTRUE(include_full_text) ||
                        (length(cls_tokens) == 0 && !has_heading)) '
  vals$full_text <- str_squish(html_text(it))' else ""

  # the per-item -> row extraction, shared by single-page and paginated forms
  extract_body <- sprintf('  items <- html_elements(page, xpath = %s)
  map_dfr(items, function(it) {
    vals <- map(fields, function(f) {
      xp <- sprintf(".//*[contains(concat(\' \', normalize-space(@class), \' \'), \' %%s \')]", f)
      node <- html_element(it, xpath = xp)
      if (inherits(node, "xml_missing")) NA_character_ else str_squish(html_text(node))
    })
    names(vals) <- fields%s%s%s%s
    as_tibble(vals)
  })', r_str(xpath), heading_block, link_block, image_block, fulltext_block)

  pages <- max(1, as.integer(pages %||% 1))

  # paginated form (non-JS only — server-rendered lists are the paginated case)
  if (pages > 1 && !isTRUE(render_js)) {
    return(sprintf(
'library(httr)
library(rvest)
library(dplyr)
library(purrr)
library(stringr)

first_url <- %s
max_pages  <- %d
fields <- %s

extract_page <- function(page, url) {
%s
}

%s

frames <- list(); url <- first_url; visited <- character()
for (i in seq_len(max_pages)) {
  resp <- RETRY("GET", url, user_agent(%s), timeout(20), times = 3, pause_base = 2)
  if (http_error(resp)) break
  page <- read_html(content(resp, as = "text", encoding = "UTF-8"))
  df_i <- extract_page(page, url); df_i$page <- i
  frames[[length(frames) + 1]] <- df_i
  visited <- c(visited, url)
  nx <- next_url(page, url)
  if (is.na(nx) || nx %%in%% visited) break
  url <- nx; Sys.sleep(0.6)  # be polite between page fetches
}
df <- bind_rows(frames)

df', r_str(url), pages, tok_vec, extract_body, NEXT_URL_FN, r_str(BROWSER_UA)))
  }

  # single-page form
  paste0(
    gen_fetch_code(url, render_js), sprintf('

library(dplyr)
library(purrr)
library(stringr)

# One column per repeated child class
fields <- %s

df <- {
%s
}

df', tok_vec, extract_body))
}

gen_custom_code <- function(url, selector, mode = "text", attr_name = NULL,
                            render_js = FALSE) {
  sel <- r_str(selector)
  body <- switch(
    mode,
    text = sprintf('df <- tibble::tibble(
  text = stringr::str_squish(html_text(html_elements(page, %s)))
)', sel),
    href = sprintf('nodes <- html_elements(page, %s)
href <- html_attr(nodes, "href")
df <- tibble::tibble(
  text = stringr::str_squish(html_text(nodes)),
  href = ifelse(is.na(href), NA_character_, xml2::url_absolute(href, url))
)', sel),
    src = sprintf('nodes <- html_elements(page, %s)
src <- html_attr(nodes, "src")
df <- tibble::tibble(
  src = ifelse(is.na(src), NA_character_, xml2::url_absolute(src, url))
)', sel),
    html = sprintf('df <- tibble::tibble(
  html = as.character(html_elements(page, %s))
)', sel),
    attr = sprintf('nodes <- html_elements(page, %s)
df <- tibble::tibble(
  text  = stringr::str_squish(html_text(nodes)),
  value = html_attr(nodes, %s)
)', sel, r_str(attr_name %||% "href")),
    sprintf('df <- tibble::tibble(
  text = stringr::str_squish(html_text(html_elements(page, %s)))
)', sel)
  )
  paste0(gen_fetch_code(url, render_js), "\n\n", body, "\n\ndf")
}

gen_json_code <- function(url, blob_id, table_label, render_js = FALSE) {
  # JSON API endpoint: the body IS the JSON - no HTML parsing at all
  if (identical(blob_id, "body")) {
    return(sprintf(
'library(httr)
library(jsonlite)

url <- %s

resp <- RETRY("GET", url,
              user_agent(%s),
              timeout(20), times = 3, pause_base = 2)
stop_for_status(resp)
data <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))

# The record set used in the app lives at:
#   %s
# Drill into `data` with $ / [[ ]] to reach it.

str(data, max.level = 2)', r_str(url), r_str(BROWSER_UA), table_label))
  }

  extract_line <- if (identical(blob_id, "next_data")) {
    'json_txt <- html_text(html_element(page, "script#__NEXT_DATA__"))'
  } else if (identical(blob_id, "nuxt_data")) {
    'json_txt <- html_text(html_element(page, "script#__NUXT_DATA__"))'
  } else {
    # ldjson_N -> pick the N-th ld+json block (html_element alone would
    # always return the first one)
    idx <- suppressWarnings(as.integer(sub("^ldjson_", "", blob_id)))
    if (is.na(idx)) idx <- 1L
    sprintf(
      'json_txt <- html_text(html_elements(page, "script[type=\'application/ld+json\']")[[%d]])',
      idx)
  }

  paste0(
    gen_fetch_code(url, render_js), sprintf('

library(jsonlite)

# Pull the embedded JSON payload out of the page
%s
data <- fromJSON(json_txt)

# The record set used in the app lives at:
#   %s
# Drill into `data` with $ / [[ ]] to reach it, e.g.:
# df <- data$props$pageProps$...

str(data, max.level = 2)', extract_line, table_label))
}

gen_list_code <- function(url, index, render_js = FALSE) {
  paste0(
    gen_fetch_code(url, render_js), sprintf('

lst <- html_elements(page, "ul, ol")[[%d]]
li  <- html_elements(lst, xpath = "./li")
hrefs <- vapply(li, function(x) {
  a <- html_element(x, xpath = ".//a[@href]")
  if (inherits(a, "xml_missing")) NA_character_ else html_attr(a, "href")
}, character(1))
df <- tibble::tibble(
  item = stringr::str_squish(html_text(li)),
  link = ifelse(is.na(hrefs), NA_character_, xml2::url_absolute(hrefs, url))
)

df', index))
}

gen_links_code <- function(url, render_js = FALSE, filter_text = "",
                           internal_only = FALSE) {
  filt <- ""
  if (isTRUE(internal_only)) {
    filt <- paste0(filt, sprintf('

# keep only links on this site (the "Internal links only" toggle)
link_dom <- stringr::str_match(df$url, "^https?://(?:www\\\\.)?([^/:]+)")[, 2]
df <- df[!is.na(link_dom) & endsWith(link_dom, %s), ]', r_str(url_domain(url))))
  }
  if (nzchar(filter_text %||% "")) {
    filt <- paste0(filt, sprintf('

# the filter typed in the app
df <- dplyr::filter(df, stringr::str_detect(
  paste(text, url), stringr::fixed(%s, ignore_case = TRUE)))',
      r_str(filter_text)))
  }
  paste0(
    gen_fetch_code(url, render_js), '

a <- html_elements(page, "a[href]")
df <- tibble::tibble(
  text = stringr::str_squish(html_text(a)),
  href = html_attr(a, "href"),
  url  = xml2::url_absolute(html_attr(a, "href"), url)
)

# drop javascript:/bare-# links and duplicates (same as the app)
df <- dplyr::filter(df, !stringr::str_detect(href, "^(javascript:|#$)"))
df <- dplyr::distinct(df, url, text, .keep_all = TRUE)', filt, '

df')
}

# Appended to a dataset's script when the user runs "Auto-detect column types".
# readr::type_convert reproduces the same text->number/logical coercion.
gen_infer_code <- function(changed) {
  cols <- if (length(changed)) paste0("#   ", changed, collapse = "\n") else "#   (none)"
  paste0('

# --- auto-detected column types (applied in the app) ---
# readr guesses numeric / integer / logical from the text and coerces cleanly.
# Columns whose type changed:
', cols, '
df <- readr::type_convert(df, guess_integer = TRUE)')
}
