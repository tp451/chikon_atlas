# Atlas der Ostasien-Forschung in Norddeutschland — Shiny app

The interactive front end of the atlas. It lets visitors filter East Asia-related
research from ten Northern German universities and explore it as tables, maps,
networks and charts.

The data behind it is produced by `clean_mining.R` in the repository root, which
harvests ORCiD, OpenAlex and Crossref, filters for East Asian subject matter, and
writes the app's input files into this folder. See the README there for the mining
pipeline; this file describes the app.

## Running it

```r
shiny::runApp("shiny")
```

The working directory must be this folder, since the app reads its data by
relative path. It checks for the required files at startup and stops with a list
of what is missing rather than failing part-way through loading.

## Structure

Everything lives in **`app.R`** — UI, server logic and configuration in one file,
organised by banner comments. The tabs are:

| Tab | Content |
|-----|---------|
| **Katalog** | The main view. A filter panel on the left, results on the right across eight sub-tabs. |
| ‣ Personen | Researchers matching the filters, with institution, department, faculty and publication count. |
| ‣ Publikationen | The publications themselves, with year, type, journal and DOI. |
| ‣ Karte | Places named in titles and abstracts, as markers sized by frequency over shaded countries. |
| ‣ Netzwerk (Institutionen) | Researchers, publications and institutions as a bipartite graph. |
| ‣ Netzwerk (Zitationen) | Citation graphs in five modes: citations between authors, author co-citation, reference co-citation, direct citations between publications, and bibliographic coupling. |
| ‣ Förderungen | Funding records, with funder and funder country. |
| ‣ Statistiken | Distribution over time, sites, disciplines, regions and cooperation countries. |
| ‣ Schlagwörter | The most frequent terms from titles and abstracts, as a table and a word cloud. |
| **Anleitungen** | Worked examples of typical searches, illustrated with screenshots. |
| **Dokumentation** | How the data was gathered and processed, for readers of the results. |
| **Datenschutz** | Privacy statement. |
| **Impressum** | Authorship, institutional details, data sources and libraries. |

Filters apply across all sub-tabs at once: region, site, faculty, period, and free
search terms combined with AND/OR. Person, publication and funding tables can be
downloaded as CSV or Excel.

## Data files

Written by `clean_mining.R` as gzip-compressed `.rds`, which loads far faster at
startup than parsing CSV or GeoJSON:

| File | Content |
|------|---------|
| `complete_works_PA.rds` | Publications |
| `complete_researchers_PA.rds` | Researchers, with affiliation history |
| `complete_researchers_PA_latest.rds` | One current record per researcher |
| `complete_funding_PA.rds` | Funding and awards |
| `complete_spacy_PA_keywords.rds` | Terms extracted from titles and abstracts |
| `complete_spacy_PA_geo.rds` | Recognised places with coordinates |
| `counted_coop_countries.rds` | Co-author institution countries per publication |
| `years_normal.rds` | Publication counts per year, for normalisation |
| `references_edges.rds`, `references_meta.rds`, `citation_edges_direct.rds` | Citation network edges and reference metadata |
| `nd_institutions.rds` | Northern German institution names, used by the filters |
| `sf_countries_PA.geojson` | Country polygons for the map; a pipeline input rather than an output |
| `funder_countries.csv` | Funder to country lookup |

## Assets

- **`www/basemap.js`**, **`www/basemap/*.geojson`**, **`www/fonts/*.woff2`** — the
  map's offline basemap (see below)
- **`www/flags/`** — country flag SVGs for the table columns
- **`www/anl_*.jpg`**, **`www/standorte.jpeg`** — screenshots and the site map used
  in the Anleitungen and Dokumentation tabs
- **`www/logo_*.png`**, favicons, **`www/citation.bib`**

## Offline basemap

The map draws its background from Natural Earth data that the app serves itself,
rather than loading raster tiles from a map provider. That keeps map use free of
third-party requests: no visitor's IP address is passed to an outside host, so the
map needs neither an Auftragsverarbeitung nor an entry in the Datenschutzhinweis.

`build_basemap.R` in the repository root produces the files — land, lakes, country
boundaries, province boundaries and place labels as GeoJSON, plus the Open Sans
label typeface — and `www/basemap.js` draws them in the style of CARTO Positron.
It is a one-off build, not part of the mining pipeline; the output is committed to
the repository. Re-run it only to change the level of detail or to pick up a newer
Natural Earth release:

```r
Rscript build_basemap.R
```

It needs `sf`, `dplyr`, `rnaturalearth`, `rnaturalearthdata` and `rmapshaper`. The
cartographic reasoning behind the layer split and the simplification settings is
documented in that script's comments. `app.R` versions the asset URLs by
modification time, so a rebuild reaches browsers that still hold the old files.

## Dependencies

| Purpose | Packages |
|---------|----------|
| Shiny and UI | `shiny`, `shinyjs`, `shinythemes`, `shinyWidgets`, `shinycssloaders`, `htmlwidgets`, `bslib` |
| Data wrangling | `tidyverse` |
| Networks | `igraph`, `visNetwork`, `Matrix` |
| Charts | `ggplot2`, `ggtext`, `ggrepel`, `wordcloud2`, `viridisLite` |
| Maps | `sf`, `leaflet`, `leaflet.extras2` |
| Export | `writexl` |

```r
install.packages(c("shiny", "shinyjs", "shinythemes", "shinyWidgets",
                   "shinycssloaders", "htmlwidgets", "bslib", "tidyverse",
                   "igraph", "visNetwork", "Matrix", "ggplot2", "ggtext",
                   "ggrepel", "wordcloud2", "viridisLite", "sf", "leaflet",
                   "leaflet.extras2", "writexl"))
```

## Credits

Concept, programming and realisation: Jun.-Prof. Dr. Thorben Pelzer.
Funded and initiated by the Chinazentrum CAU Kiel, within the ChiKoN project.
Data sources: ORCiD, OpenAlex, Crossref.
