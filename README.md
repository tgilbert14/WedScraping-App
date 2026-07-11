# Web Scraper Studio

An interactive R Shiny app that turns any web page into a data frame:
**point → analyze → pick → data frame → download**.

**▶ Live demo:** https://019f4f8f-d193-b0fb-71bb-e1d8edb7ac5f.share.connect.posit.cloud/

Paste a URL and the studio fetches the page, figures out what kind of site it
is, and offers you every scrapable structure it can find — no code required
(but it writes the reproducible R code for you anyway).

> The hosted demo runs on Posit Connect Cloud, which has no Chrome binary, so
> the "Render with headless Chrome" toggle is inert there (every other feature
> works). Datacenter IPs also get blocked by some sites more than a home IP —
> run it locally for heavy scraping.

## Run it

```r
shiny::runApp("WedScraping-App")
```

## Deploy

The app is set up for **git-backed Posit Connect Cloud** deployment: the
generated [`WedScraping-App/manifest.json`](WedScraping-App/manifest.json)
pins R 4.5.2 and its 70 CRAN dependencies, and every push to `main`
auto-redeploys. Regenerate the manifest after any package change:

```r
rsconnect::writeManifest(appDir = "WedScraping-App")
```

Requires R ≥ 4.3 with: `shiny`, `bslib`, `shinyjs`, `shinycssloaders`, `rvest`,
`xml2`, `httr`, `dplyr`, `purrr`, `stringr`, `tibble`, `DT`, `jsonlite`.
Optional: `writexl` (Excel export), `chromote` + Chrome (JavaScript-rendered
sites).

## What it does

1. **Point** — paste a URL (press <kbd>Enter</kbd> to analyze) or pick a
   bundled example: 247Sports commits / team rankings / top recruits /
   transfer portal, On3 commits, Wikipedia tables, Books/Quotes to Scrape, a
   faculty directory. A **Recent URLs** dropdown remembers this session's
   pages, and a **Pages to scrape** control follows "next page" links so a
   multi-page list comes back whole (see below).
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
4. **Use** — datasets collect in **My Data**:
   - **Clean** — dedupe, drop empty rows/columns, rename.
   - **Auto-detect column types** — coerces text to integer / numeric /
     logical cleanly (handles thousands separators, currency, `%`, yes/no;
     leaves zero-padded codes and big IDs as text), so the export is
     analysis-ready.
   - **Columns** — untick to drop, drag to reorder.
   - **Download** — CSV / TSV / Excel / JSON / RDS, or every dataset as one
     Excel workbook; or **Copy data (TSV)** straight to the clipboard.
   - Every dataset carries the **generated standalone R script** that
     reproduces it outside the app (with a one-click **Copy** button on each
     code panel). A **History** tab logs the whole session.

### Multi-page scraping (pagination)

Set **Pages to scrape** above 1 and the app follows the site's "next page"
link — `rel="next"`, `.next` controls, next-glyph anchors, or numbered
`?page=N` patterns — stacking every page into one data frame (with a `page`
column marking provenance). It stays on the same site and the same listing
path, so a stray link in a footer can't hijack it. Example: 247Sports
rankings return all recruits across pages instead of one page of 50. The
generated R script emits a matching page-follow loop. (Pagination needs a
direct request; it's skipped under headless-Chrome rendering.)

## Project layout

```
WedScraping-App/          the Shiny app
├── global.R              libraries, theme, presets, discovery bundle
├── ui.R                  bslib page + tabs
├── server.R              reactive flow
├── R/
│   ├── scraper_engine.R  fetch / paginate / analyze / discover / extract /
│   │                     type-inference / codegen
│   └── recipes.R         curated site recipes
└── www/
    ├── styles.css        design system (tokens, dark sidebar, components)
    └── app.js            enter-to-analyze + copy-to-clipboard
dev-notes/                scratch scripts kept out of the served www/ folder
```

## Notes

- Scrape responsibly: respect sites' terms of service and robots.txt, and
  keep request volumes polite (the fetcher retries with backoff, sends a
  normal browser UA, and pauses between paginated page fetches).
- Recipes are best-effort sugar — site markup drifts; when a recipe returns
  zero rows the generic discovery tabs still work.
