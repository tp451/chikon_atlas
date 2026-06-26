# clean_mining.R — Documentation

**Atlas der Ostasien-Forschung in Norddeutschland**
Thorben Pelzer, 2025 | CC BY-SA 4.0

Mines, merges, and cleans Asia-related publication data authored by scholars affiliated with Northern German institutions. Data sources: OpenAlex, ORCID, and Crossref.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Configuration](#configuration)
3. [Pipeline Overview](#pipeline-overview)
4. [Section Reference](#section-reference)
5. [Helper Functions](#helper-functions)
6. [Faculty Classification (Ollama LLM)](#faculty-classification-ollama-llm)
7. [Input Files](#input-files)
8. [Output Files](#output-files)
9. [Checkpoints & Restart Points](#checkpoints--restart-points)

---

## Prerequisites

### R Packages

| Category | Packages |
|----------|----------|
| Core data wrangling | `tidyverse` (dplyr, tidyr, readr, ggplot2, ...) |
| Dates | `anytime`, `lubridate` |
| Cleaning / utility | `janitor`, `usethis`, `pbapply` |
| Text / NLP | `stringr`, `stringi`, `xml2`, `htmltools`, `spacyr`, `cld3` |
| Scholarly APIs | `openalexR`, `rcrossref`, `rorcid` |
| Systematic review | `synthesisr` |
| HTTP / JSON | `httr2`, `jsonlite` |
| Geospatial | `sf`, `tidygeocoder` |

### External Services

- **OpenAlex API** — set `OPENALEX_EMAIL` environment variable
- **ORCID API** — uses `rorcid`; configure ORCID token per package docs
- **Crossref API** — via `rcrossref`
- **spaCy** — Python NLP backend; provisioned reproducibly via `setup_python.R` (see [below](#python--spacy-setup))
- **Ollama** — local LLM server for faculty classification (see [below](#faculty-classification-ollama-llm))

### Python / spaCy setup

Sections 8 and 14 run spaCy NER through `spacyr`. spacyr (1.3.x) manages its own
virtual environment named **`r-spacyr`**; `spacy_initialize()` uses it
automatically (or honours `RETICULATE_PYTHON` if you set it).

Provision it once with spacyr's own installer, wrapped in `setup_python.R`:

```r
source("setup_python.R")     # creates r-spacyr + installs spaCy & the 4 models
```

This installs spaCy 3.8.11 plus the four language models the pipeline uses
(`en_core_web_lg`, `de_core_news_lg`, `fr_core_news_lg`, `zh_core_web_lg`).
`clean_mining.R` then uses the venv automatically — no path to configure.

> **Windows note:** spacyr's `r-spacyr` venv is built on the r-miniconda base,
> and reticulate does not activate conda's DLL directories for a *virtualenv*.
> That leaves `python3xx.dll`'s conda dependencies unresolvable and aborts the R
> session with a `0xC0000005` access violation (often surfacing at an unrelated
> line such as the first `read_csv`). Both `setup_python.R` and Section 1 of
> `clean_mining.R` prepend the conda DLL dirs to `PATH` before spaCy initialises,
> which fixes it. To use a different Python entirely, set `RETICULATE_PYTHON`.

| File | Role |
|------|------|
| `setup_python.R` | One-time provisioning of the `r-spacyr` venv via `spacy_install()` |

---

## Configuration

All tuneable parameters live in the `config` list at the top of the script (line 72):

```r
config <- list(
  openalex_email   = Sys.getenv("OPENALEX_EMAIL", "your-email@example.com"),
  year_range       = 2000:2025,
  bbox             = c(xmin = 80, xmax = 150, ymin = 0, ymax = 50),
  api_retry_tries  = 3,
  api_retry_wait   = 30,
  output_dir       = ".",
  ollama_url       = "http://localhost:11434/api/generate",
  ollama_model     = "qwen3.5:9b",
  ollama_cache     = "faculties_ollama.csv"
)
```

| Key | Purpose |
|-----|---------|
| `openalex_email` | Polite-pool email for OpenAlex API |
| `year_range` | Publication years to query (2000–2025) |
| `bbox` | Bounding box for Pacific Asia region (lon 80–150, lat 0–50) |
| `api_retry_tries` | Max retries on API failure |
| `api_retry_wait` | Seconds between retries |
| `output_dir` | Working/output directory |
| `ollama_url` | Ollama REST API endpoint |
| `ollama_model` | LLM model name (default: `qwen3.5:9b`) |
| `ollama_cache` | File to cache LLM predictions across runs |

---

## Pipeline Overview

```
┌─────────────────────────────────────────────────────────────────┐
│  1. Configuration                                               │
│  2. Libraries                                                   │
│  3. Keyword dictionaries (China, Japan, Korea, Taiwan, ...)     │
│  4. External data loading (harmonisation tables, shapefiles)    │
│  5. Helper functions                                            │
├─────────────────────────────────────────────────────────────────┤
│  OPENALEX BRANCH                                                │
│  6.  Fetch institutions & publications for Northern Germany     │
│  7.  Regex keyword matching → Asia-related pubs                 │
│  8.  spaCy NER on abstracts → geographic/org entities           │
│  9.  Geocode extracted entities via OpenStreetMap               │
│  10. Join regex + NLP results → unified Pacific Asia pub set    │
│  11. Unnest authorship → researcher records with affiliations   │
├─────────────────────────────────────────────────────────────────┤
│  ORCID BRANCH                                                   │
│  12. Search ORCID for researchers at Northern German insts      │
│  13. Fetch all works for those researchers                      │
│  14. Fetch Crossref abstracts, run spaCy NER                   │
│  15. Regex + NLP matching (same pipeline as OpenAlex)           │
├─────────────────────────────────────────────────────────────────┤
│  MERGE & CLEAN                                                  │
│  16. Merge OpenAlex + ORCID datasets                            │
│  17. Clean researchers (names, orgs, depts, faculty assignment) │
│  18. Clean publications (dedup, title normalisation)            │
│  19. Clean keywords & geodata                                   │
│  20. Final filtering & CSV/GeoJSON export                       │
└─────────────────────────────────────────────────────────────────┘
```

---

## Section Reference

| # | Line | Title | Purpose |
|---|------|-------|---------|
| 1 | 65 | Configuration | Centralise API credentials, paths, thresholds |
| 2 | 95 | Library Loading | Load all R packages |
| 3 | 138 | Keyword Dictionaries | Define country/region keyword vectors for Asia filtering |
| 4 | 379 | External Dictionary Loading | Load harmonisation tables and spatial reference data |
| 5 | 403 | Helper Function Definitions | Reusable functions (keyword matching, NLP, API, LLM) |
| 6 | 773 | OpenAlex — Fetch Institutions & Pubs | Query all Northern German institutions and their publications |
| 7 | 878 | OpenAlex — Regex Keyword Matching | Tag Asia-related publications via keyword regex |
| 8 | 922 | OpenAlex — NLP Entity Extraction | Run spaCy NER on abstracts for geographic/org entities |
| 9 | 962 | OpenAlex — Geocoding Entities | Geocode GPE/LOC entities via OpenStreetMap |
| 10 | 1018 | OpenAlex — Join Regex + NLP Results | Combine regex- and NLP-matched pubs into Pacific Asia set |
| 11 | 1072 | OpenAlex — Author & Researcher Processing | Unnest authorship, build researcher table with affiliations |
| 12 | 1310 | ORCID — Mine Researchers & Employments | Search ORCID for affiliated researchers, fetch employments |
| 13 | 1442 | ORCID — Mine Works | Fetch all publications for ORCID researchers |
| 14 | 1484 | ORCID/Crossref — Fetch Abstracts & NLP | Get Crossref abstracts, run spaCy NER |
| 15 | 1592 | ORCID — Regex + NLP Matching | Apply same regex/NLP matching to ORCID publications |
| 16 | 1717 | Merge — OpenAlex + ORCID Datasets | Combine both data sources into unified tables |
| 17 | 1822 | Data Cleaning — Researchers | Harmonise names, orgs, depts; assign faculties (manual + LLM) |
| 18 | 2091 | Data Cleaning — Publications | Deduplicate and normalise publication records |
| 19 | 2149 | Data Cleaning — Keywords & Geodata | Clean entity keywords, build final geographic dataset |
| 20 | 2221 | Final Outputs & Exports | Apply final filters, write all output CSVs and GeoJSONs |
| 21 | 2860 | Citation Networks | Unnest `referenced_works` → `references_edges.csv`; fetch reference metadata → `references_meta.csv` (co-citation / bibliographic coupling) |
| 22 | 3037 | Compact Outputs to .rds | Re-save each generated CSV/GeoJSON as a gzip `.rds` written into the `shiny/` app folder, which `app.R` loads at startup |

---

## Helper Functions

Defined in Section 5 (~line 403):

| Function | Signature | Purpose |
|----------|-----------|---------|
| `remove_false_positives` | `(text)` | Strip known false-positive terms ("kawasaki disease", "kyoto protocol", medical jejun- terms) that would otherwise trigger Asia-keyword matches |
| `filter_entities` | `(df)` | Apply standard entity-type and text-quality filters to NLP entity data frames |
| `map_keyword_to_country` | `(kw)` | Map a keyword string to its country of origin via `keyword_country_map` lookup |
| `match_keywords_in_row` | `(i)` | Match Asia keywords against a pre-filtered row; expects `pre_filtered` and `combined_keywords_lower` in calling scope |
| `extract_entities` | `(id, abstract, .counter_env)` | Wrap spaCy NER with progress reporting; uses environment-based counter |
| `unescape_once` | `(x)` | Single-pass HTML entity unescaping |
| `unescape_twice` | `(x)` | Double-pass HTML entity unescaping |
| `fully_unescape_twice` | `(x)` | Deep HTML unescaping via XML parsing |
| `fetch_employments` | `(orcid_id, tries, wait)` | Fetch and flatten ORCID employment records with retry logic |
| `fetch_works` | `(orcid_id, tries, wait)` | Fetch ORCID works with retry logic |
| `classify_faculty_ollama` | `(titles, url, model)` | Classify a researcher into a faculty via local Ollama LLM (see next section) |

---

## Faculty Classification (Ollama LLM)

### Why an LLM?

Each researcher is assigned to one of 8 German university faculties based on their publication titles. An earlier approach used scikit-learn TF-IDF + Random Forest (~75% F1, poor on humanities). The current approach sends titles to a local LLM which has inherent knowledge of academic disciplines and handles mixed German/English text natively.

### Faculty Codes

| Code | Faculty |
|------|---------|
| `mint` | Mathematics, Informatics, Natural Sciences |
| `med` | Medicine, health sciences |
| `sowi` | Social sciences, political science, economics |
| `tech` | Engineering, materials science |
| `phil` | Philosophy, history, linguistics, cultural studies, area studies |
| `agrar` | Agriculture, food science, environmental science |
| `jura` | Law, legal studies |
| `rewi` | Religious studies, theology |

### How It Works

The classification happens in Section 17 after manual faculty lookups:

1. **Manual lookup** — join `faculties.csv` by organisation + department name
2. **Backfill** — within each ORCID group, propagate any known faculty to other rows
3. **LLM classification** — for researchers *still* without a faculty:
   - Load cached predictions from `faculties_ollama.csv`
   - Identify ORCIDs not yet in the cache
   - Aggregate their publication titles from the works table
   - Send each researcher's titles to Ollama as a zero-shot classification prompt
   - Parse the JSON response (`{"faculty": "<code>", "confidence": <0-1>}`)
   - Append new predictions to the cache CSV
4. **Merge** — fill in remaining NA faculties from the predictions

### Setup

```bash
# Install and start Ollama
ollama serve

# Pull the model (once)
ollama pull qwen3.5:9b
```

The script will error with a clear message if Ollama is not reachable.

### Caching

Predictions are cached in `faculties_ollama.csv` (columns: `orcid`, `predicted_faculties`, `prediction_confidence`). On subsequent runs, only researchers not already in the cache are sent to the LLM. Delete this file to force re-classification of all researchers.

### Tuning

- **Model**: change `config$ollama_model` (e.g. `"llama3:8b"`, `"mistral"`)
- **Temperature**: hardcoded to 0 for deterministic output (in `classify_faculty_ollama`)
- **Retries**: 3 attempts per researcher on malformed JSON
- **Title truncation**: titles longer than 3,000 characters are truncated

---

## Input Files

### Provided by the user (must exist before running)

| File | Purpose |
|------|---------|
| `db_sf_filter.csv` | Entity strings to exclude from NLP results |
| `universities.csv` | Organisation name harmonisation table |
| `departments.csv` | Department name harmonisation table |
| `faculties.csv` | Manual faculty classification (org + dept → faculty) |
| `sf_countries_PA.geojson` | Country polygons for Pacific Asia |
| `db_sf_countries.csv` | Country name list for spatial filtering |

### Auto-generated (written by the script, read back at checkpoints)

| File | Written at | Read back at |
|------|-----------|-------------|
| `insts_de.rds` | Section 6 | Section 6 checkpoint |
| `insts_chikon.rds` | Section 6 | Section 6 checkpoint |
| `alex_all_pubs.rds` | Section 6 | Section 7 checkpoint |
| `alex_matched_by_regex.rds` | Section 7 | Section 8 checkpoint |
| `entities_alex_abstracts.rds` | Section 8 | Section 9 checkpoint |
| `entities_osm.geojson` | Section 9 | Section 10 checkpoint |
| `alex_all_pubs_PA.rds` | Section 10 | Section 11 checkpoint |
| `complete_works_PA_alex.csv` | Section 11 | Section 11 checkpoint |
| `complete_researchers_PA_alex.csv` | Section 11 | Section 12 checkpoint |
| `results_complete.rds` | Section 12 | Section 12 checkpoint |
| `my_osu_employment.rds` | Section 12 | Section 13 checkpoint |
| `complete_researchers_orcid.csv` | Section 13 | Section 13 checkpoint |
| `my_orcid_works.rds` | Section 13 | Section 14 checkpoint |
| `abstracts_crossref.csv` | Section 14 | Section 14 checkpoint |
| `entities_orcid_abstracts.csv` | Section 14 | Section 15 checkpoint |
| `orcid_matched_by_regex.csv` | Section 15 | Section 15 checkpoint |
| `orcid_matched_by_spacy.csv` | Section 15 | Section 15 checkpoint |
| `complete_works_orcid_PA_joined.csv` | Section 15 | Section 16 checkpoint |
| `complete_works_PA_preview.csv` | Section 16 | Section 17 checkpoint |
| `complete_researchers_PA_preview.csv` | Section 16 | Section 17 checkpoint |
| `faculties_ollama.csv` | Section 17 | Section 17 (LLM cache) |

---

## Output Files

Written in Section 20:

| File | Content |
|------|---------|
| `complete_works_PA.csv` | Final deduplicated publications (OpenAlex + ORCID) |
| `complete_researchers_PA.csv` | All researcher affiliation records |
| `complete_researchers_PA_latest.csv` | One row per researcher (latest affiliation) |
| `complete_spacy_PA_keywords.csv` | NLP-extracted entities (keywords) |
| `complete_spacy_PA_geo.geojson` | Geocoded entities as spatial features |
| `complete_funding_PA.csv` | Funding information for final publication set |
| `years_normal.csv` | Publication counts by year |
| `counted_coop_countries.csv` | Co-authorship country counts by publication and year |
| `references_edges.csv` | Paper→reference edge list (source for co-citation & bibliographic-coupling networks) |
| `references_meta.csv` | Reference metadata (label/title/year/DOI) for labelling co-citation nodes |
| `citation_edges_direct.csv` | Direct intra-corpus citation edges (citing→cited DOI); requires §6 W-id retention + re-harvest |

---

## Checkpoints & Restart Points

The script writes intermediate results at each major stage. Every section begins with a `# ── CHECKPOINT / RESTART POINT ──` comment followed by a `read_csv` / `read_rds` / `read_sf` call that reloads the data from the previous section's output.

**To restart from a specific section**, run Sections 1–5 (config, libraries, dictionaries, functions), then jump to the desired checkpoint. For example, to iterate on faculty classification without re-running the full API pipeline:

```r
# Run Sections 1-5 first (config, libraries, helpers), then:
# Jump to the Section 17 checkpoint at line ~1832
complete_works_PA <- read_csv("complete_works_PA_preview.csv", show_col_types = F)
complete_researchers_PA <- read_csv("complete_researchers_PA_preview.csv", show_col_types = F)
# ... continue from Section 17 onwards
```
