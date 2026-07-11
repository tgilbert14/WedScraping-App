# Web Scraper Studio

An interactive R Shiny app that turns any web page into a data frame:
**point → analyze → pick → data frame → download**.

Paste a URL and the studio fetches the page, figures out what kind of site it
is, and offers you every scrapable structure it can find — no code required
(but it writes the reproducible R code for you anyway).

## Run it

```r
shiny::runApp("WedScraping-App")
```

Requires R ≥ 4.3 with: `shiny`, `bslib`, `shinyjs`, `shinycssloaders`, `rvest`,
`xml2`, `httr`, `dplyr`, `purrr`, `stringr`, `tibble`, `DT`, `jsonlite`.
Optional: `writexl` (Excel export), `chromote` + Chrome (JavaScript-rendered
sites).

## What it does

1. **Point** — paste a URL (or pick a bundled example: 247Sports commits, On3
   commits, Wikipedia tables, Books to Scrape, a faculty directory).
2. **Analyze** — the app fetches the page with a real-browser User-Agent
   (httr `RETRY`, friendly error messages) and fingerprints the site: domain,
   detected type, tech stack (Next.js / WordPress / React / …), structured
   data, and whether the page is a JavaScript shell that needs headless
   Chrome. A **Render with headless Chrome** toggle (chromote) handles
   JS-heavy sites.
3. **Pick** — discovered structures land in dedicated tabs:
   - **Tables** — every HTML table with dimensions, captions and column names.
   - **Cards** — repeated element groups (player rows, product cards, people
     listings) detected by class-signature frequency; each repeated child
     class becomes a column you can toggle on/off, plus heading / link /
     image / full-text pseudo-fields.
   - **Links & Media** — links (filterable, internal-only toggle), images,
     headings, bullet lists.
   - **Page data (JSON)** — embedded JSON payloads (`__NEXT_DATA__`, Nuxt,
     `ld+json`) walked for every record set inside, ranked by relevance —
     this is how you scrape modern JS sites like On3 *without* rendering.
   - **Custom** — a live CSS-selector playground with match counts (text /
     href / src / any attribute / raw HTML).
   - **Site recipes** — curated extractors for known site families with
     graceful fallback to generic discovery. The 247Sports recipe
     auto-detects the page kind and extracts the right columns for
     **team commit lists**, **player rankings / top recruits**,
     **team class rankings**, and the **transfer portal** (with predicted
     destinations). Also: On3 via `__NEXT_DATA__`, Rivals, Wikipedia, ESPN,
     and university directory cards. **Quick scrape** picks the best guess
     in one click.
4. **Use** — datasets collect in **My Data**: clean (dedupe, drop empty
   rows/columns), rename, download as CSV / TSV / Excel / JSON / RDS, or
   export everything as one Excel workbook. Every dataset carries the
   generated standalone R script that reproduces it outside the app.
   A **History** tab logs the whole session.

## Project layout

```
WedScraping-App/          the Shiny app
├── global.R              libraries, theme, presets, discovery bundle
├── ui.R                  bslib page + tabs
├── server.R              reactive flow
├── R/
│   ├── scraper_engine.R  fetch / analyze / discover / extract / codegen
│   └── recipes.R         curated site recipes
└── www/styles.css        styling
dev-notes/                scratch scripts kept out of the served www/ folder
```

## Notes

- Scrape responsibly: respect sites' terms of service and robots.txt, and
  keep request volumes polite (the fetcher already retries with backoff and
  sends a normal browser UA).
- Recipes are best-effort sugar — site markup drifts; when a recipe returns
  zero rows the generic discovery tabs still work.
