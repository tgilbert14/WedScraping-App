# ============================================================================
# recipes.R
# Curated "recipes" for known site families. A recipe is matched by domain
# (and optionally page content); its scrape() returns a ready-to-use tibble.
# If a recipe returns 0 rows the app falls back to generic discovery, so
# recipes are always best-effort sugar, never a requirement.
# ============================================================================

.txt <- function(node, css) {
  x <- rvest::html_element(node, css)
  if (inherits(x, "xml_missing")) return(NA_character_)
  stringr::str_squish(rvest::html_text(x))
}

RECIPES <- list(

  # --- 247Sports (multi-page-type) -------------------------------------------
  # One recipe, four page kinds, auto-detected from the markup:
  #   * team commits / roster lists   (li.ri-page__list-item)
  #   * player rankings / top recruits (li.rankings-page__list-item + .recruit)
  #   * team class rankings            (li.rankings-page__list-item + .team)
  #   * transfer portal                (li.transfer-player)
  list(
    id          = "sports247",
    label       = "247Sports recruiting page",
    site_type   = "247Sports - recruiting page",
    domain      = "247sports\\.com",
    description = paste(
      "Works on team commit lists, player rankings (top recruits), team class",
      "rankings, and the transfer portal - the page kind is detected",
      "automatically and the right columns are extracted."
    ),
    applies = function(page, url) {
      length(rvest::html_elements(page, paste(
        "li.ri-page__list-item a.ri-page__name-link",
        "li.rankings-page__list-item a.rankings-page__name-link",
        "li.transfer-player",
        sep = ", "))) > 0
    },
    scrape = function(page, url) {
      # ---- transfer portal ------------------------------------------------
      portal <- rvest::html_elements(page, "li.transfer-player")
      if (length(portal)) {
        img_alts <- function(it, css) {
          imgs <- rvest::html_elements(it, css)
          alts <- stringr::str_squish(rvest::html_attr(imgs, "alt"))
          alts <- alts[!is.na(alts) & nzchar(alts)]
          if (!length(alts)) NA_character_ else paste(unique(alts), collapse = ", ")
        }
        rows <- purrr::map(portal, function(it) {
          a <- rvest::html_element(it, "h3 a, .player-avatar a[title]")
          nm <- if (inherits(a, "xml_missing")) NA_character_ else
            stringr::str_squish(rvest::html_text(a))
          if (is.na(nm) || !nzchar(nm)) {
            # avatar-only fallback: title = "View player information for <name>"
            ttl <- if (inherits(a, "xml_missing")) NA_character_ else
              rvest::html_attr(a, "title")
            nm <- stringr::str_squish(stringr::str_remove(
              ttl %||% "", "^View player information for "))
          }
          if (is.na(nm) || !nzchar(nm)) return(NULL)
          tibble::tibble(
            name          = nm,
            profile_url   = xml2::url_absolute(rvest::html_attr(a, "href"), url),
            position      = .txt(it, ".position"),
            rating        = .txt(it, ".rating"),
            height_weight = .txt(it, ".bio"),
            origin        = .txt(it, ".trend"),
            status        = .txt(it, ".status"),
            status_date   = .txt(it, ".statusDate"),
            destination   = img_alts(it, ".destination img[alt]"),
            predictions   = img_alts(it, ".transfer-prediction img[alt], .predictions img[alt]")
          )
        })
        return(dplyr::bind_rows(rows))
      }

      # ---- rankings pages (players OR teams) --------------------------------
      rk <- rvest::html_elements(page, "li.rankings-page__list-item")
      if (length(rk)) {
        is_team_page <- length(rvest::html_elements(
          page, "li.rankings-page__list-item div.team")) > 0
        rows <- purrr::map(rk, function(it) {
          name_node <- rvest::html_element(it, "a.rankings-page__name-link")
          if (inherits(name_node, "xml_missing")) return(NULL)  # header rows
          nm <- stringr::str_squish(rvest::html_text(name_node))
          if (!nzchar(nm)) return(NULL)
          if (is_team_page) {
            tibble::tibble(
              rank          = .txt(it, ".rank-column .primary"),
              prev_rank     = .txt(it, ".rank-column .other"),
              team          = nm,
              team_url      = xml2::url_absolute(rvest::html_attr(name_node, "href"), url),
              commits       = .txt(it, ".total"),
              avg_rating    = .txt(it, ".avg"),
              star_commits  = .txt(it, ".star-commits-list"),
              points        = .txt(it, ".points")
            )
          } else {
            tibble::tibble(
              rank            = .txt(it, ".rank-column .primary"),
              name            = nm,
              profile_url     = xml2::url_absolute(rvest::html_attr(name_node, "href"), url),
              position        = .txt(it, ".position"),
              height_weight   = .txt(it, ".metrics"),
              hometown_school = .txt(it, ".meta"),
              rating          = .txt(it, ".score, .rankings-page__star-and-score .score"),
              national_rank   = .txt(it, "a.natrank"),
              position_rank   = .txt(it, "a.posrank"),
              state_rank      = .txt(it, "a.sttrank"),
              status          = .txt(it, ".status, .commit-date, .predictions")
            )
          }
        })
        return(dplyr::bind_rows(rows))
      }

      # ---- team commits / roster lists --------------------------------------
      items <- rvest::html_elements(page, "li.ri-page__list-item")
      rows <- purrr::map(items, function(it) {
        name_node <- rvest::html_element(it, "a.ri-page__name-link")
        if (inherits(name_node, "xml_missing")) return(NULL)  # header rows
        nm <- stringr::str_squish(rvest::html_text(name_node))
        if (!nzchar(nm)) return(NULL)
        tibble::tibble(
          name            = nm,
          profile_url     = xml2::url_absolute(rvest::html_attr(name_node, "href"), url),
          position        = .txt(it, ".position"),
          height_weight   = .txt(it, ".metrics"),
          hometown_school = .txt(it, ".meta"),
          rating          = .txt(it, ".score, .rankings-page__star-and-score .score"),
          status          = .txt(it, ".status, .commit-date, .predictions")
        )
      })
      dplyr::bind_rows(rows)
    }
  ),

  # --- On3 (Next.js): pull the record set out of __NEXT_DATA__ --------------
  list(
    id          = "on3_nextdata",
    label       = "On3 page data (Next.js)",
    site_type   = "On3 - recruiting page (JS-rendered, data embedded as JSON)",
    domain      = "on3\\.com",
    description = paste(
      "On3 is a Next.js site: the real data ships inside a JSON blob",
      "(__NEXT_DATA__). This recipe finds the largest record set in that blob.",
      "Use the 'Page data (JSON)' tab to browse every other record set."
    ),
    applies = function(page, url) {
      !inherits(rvest::html_element(page, "script#__NEXT_DATA__"), "xml_missing")
    },
    scrape = function(page, url) {
      nd <- rvest::html_element(page, "script#__NEXT_DATA__")
      if (inherits(nd, "xml_missing")) return(tibble::tibble())
      tables <- json_to_tables(rvest::html_text(nd))
      if (!length(tables)) return(tibble::tibble())
      tables[[which.max(score_json_records(tables))]]
    }
  ),

  # --- Rivals recruit lists ---------------------------------------------------
  list(
    id          = "rivals_recruits",
    label       = "Rivals recruit list",
    site_type   = "Rivals - recruiting page",
    domain      = "rivals\\.com",
    description = paste(
      "Rivals team/commitments pages. Tries the embedded JSON payload first,",
      "then repeated card structures."
    ),
    applies = function(page, url) TRUE,
    scrape = function(page, url) {
      # rivals ships data in ld+json / inline JSON on many pages
      blobs <- discover_json(page)
      for (b in blobs) {
        tabs <- json_to_tables(b$text)
        if (length(tabs)) {
          best <- tabs[[which.max(score_json_records(tabs))]]
          if (nrow(best) >= 3) return(best)
        }
      }
      tibble::tibble()
    }
  ),

  # --- Wikipedia: articles are table goldmines --------------------------------
  list(
    id          = "wikipedia_tables",
    label       = "Wikipedia article tables",
    site_type   = "Wikipedia article",
    domain      = "wikipedia\\.org",
    description = paste(
      "Wikipedia articles keep their data in wikitables. This recipe grabs the",
      "largest wikitable; the Tables tab lists all of them."
    ),
    applies = function(page, url) {
      length(rvest::html_elements(page, "table.wikitable")) > 0
    },
    scrape = function(page, url) {
      tabs <- rvest::html_elements(page, "table.wikitable")
      dfs <- purrr::map(tabs, function(t) {
        tryCatch(rvest::html_table(t, fill = TRUE), error = function(e) NULL)
      })
      dfs <- purrr::compact(dfs)
      if (!length(dfs)) return(tibble::tibble())
      sizes <- vapply(dfs, function(d) nrow(d) * ncol(d), numeric(1))
      tibble::as_tibble(dfs[[which.max(sizes)]], .name_repair = "unique_quiet")
    }
  ),

  # --- ESPN roster/stat pages --------------------------------------------------
  list(
    id          = "espn_tables",
    label       = "ESPN roster / stats tables",
    site_type   = "ESPN - roster or stats page",
    domain      = "espn\\.com",
    description = "ESPN roster and stat pages render as HTML tables; grabs the largest one.",
    applies = function(page, url) {
      length(rvest::html_elements(page, "table")) > 0
    },
    scrape = function(page, url) {
      tabs <- rvest::html_elements(page, "table")
      dfs <- purrr::compact(purrr::map(tabs, function(t) {
        tryCatch(rvest::html_table(t, fill = TRUE), error = function(e) NULL)
      }))
      if (!length(dfs)) return(tibble::tibble())
      sizes <- vapply(dfs, function(d) nrow(d) * ncol(d), numeric(1))
      tibble::as_tibble(dfs[[which.max(sizes)]], .name_repair = "unique_quiet")
    }
  ),

  # --- Faculty / staff directory cards (universities) --------------------------
  list(
    id          = "directory_cards",
    label       = "Directory cards (.card-body)",
    site_type   = "Directory / people-listing page",
    domain      = "\\.edu",
    description = paste(
      "University directory pages built on Bootstrap cards. Extracts each",
      "card's text block plus the first link."
    ),
    applies = function(page, url) {
      length(rvest::html_elements(page, ".card-body")) >= 4
    },
    scrape = function(page, url) {
      cards <- rvest::html_elements(page, ".card-body")
      rows <- purrr::map(cards, function(cd) {
        lines <- stringr::str_split(rvest::html_text(cd), "\n")[[1]]
        lines <- stringr::str_squish(lines)
        lines <- lines[nzchar(lines)]
        if (!length(lines)) return(NULL)
        a <- rvest::html_element(cd, "a[href]")
        tibble::tibble(
          name   = lines[1],
          title  = if (length(lines) >= 2) lines[2] else NA_character_,
          email  = {
            em <- lines[stringr::str_detect(lines, "@")]
            if (length(em)) utils::tail(em, 1) else NA_character_
          },
          detail = paste(lines[-1], collapse = " | "),
          link   = if (inherits(a, "xml_missing")) NA_character_ else
            xml2::url_absolute(rvest::html_attr(a, "href"), url)
        )
      })
      dplyr::bind_rows(rows)
    }
  )
)

# Return the first recipe whose domain matches AND whose applies() passes.
# Matched against the URL's DOMAIN only - a path like /article/about.edu.html
# must not trigger the .edu recipe.
find_recipe <- function(url, page = NULL) {
  dom <- url_domain(url %||% "")
  for (r in RECIPES) {
    if (!stringr::str_detect(dom, stringr::regex(r$domain, ignore_case = TRUE))) next
    if (!is.null(page) && is.function(r$applies)) {
      ok <- tryCatch(isTRUE(r$applies(page, url)), error = function(e) FALSE)
      if (!ok) next
    }
    return(r)
  }
  NULL
}

run_recipe <- function(recipe, page, url) {
  df <- tryCatch(recipe$scrape(page, url), error = function(e) tibble::tibble())
  if (!is.data.frame(df)) df <- tibble::tibble()
  tryCatch(sanitize_df(df), error = function(e) tibble::tibble())
}
