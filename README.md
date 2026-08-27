# clean_mining.R — Documentation

**Atlas der Ostasien-Forschung in Norddeutschland**
Thorben Pelzer, 2025–2026 | CC BY-SA 4.0

Mines, merges, and cleans Asia-related publication data authored by scholars affiliated with Northern German institutions. Data sources: OpenAlex, ORCID, and Crossref.

---

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Configuration](#configuration)
3. [Pipeline Overview](#pipeline-overview)
4. [Section Reference](#section-reference)
5. [Helper Functions](#helper-functions)
6. [Harvest Resilience (Section 6)](#harvest-resilience-section-6)
7. [Organisation Harmonisation (Sections 16a/16b/17)](#organisation-harmonisation-sections-16a16b17)
8. [Faculty Classification (Ollama LLM)](#faculty-classification-ollama-llm)
9. [Citation Networks (Section 21)](#citation-networks-section-21)
10. [Shiny Compaction (Section 22)](#shiny-compaction-section-22)
11. [Input Files](#input-files)
12. [Output Files](#output-files)
13. [Checkpoints & Restart Points](#checkpoints--restart-points)

---

## Prerequisites

### R Packages

| Category | Packages |
|----------|----------|
| Core data wrangling | `tidyverse` (dplyr, tidyr, readr, ggplot2, ...) |
| Dates | `anytime`, `lubridate` |
| Cleaning / utility | `janitor`, `usethis`, `pbapply` |
| Text / NLP | `stringr`, `stringi`, `stringdist`, `xml2`, `htmltools`, `spacyr`, `cld3` |
| Scholarly APIs | `openalexR` (**≥ 3.0.0**), `rcrossref`, `rorcid` |
| Systematic review | `synthesisr` |
| HTTP / JSON | `httr2`, `jsonlite` |
| Geospatial | `sf`, `tidygeocoder` |
| Python bridge | `reticulate` (called directly by the Windows DLL fix in Section 1) |

`openalexR` below 3.0.0 cannot parse the current OpenAlex works schema — `oa2df` builds a duplicated `id` column and every works fetch fails. Section 1 checks the installed version and stops with an explanatory message rather than letting Section 6 burn its retry budget on an unrecoverable error.

### External Services

- **OpenAlex API** — set `OPENALEX_EMAIL` (polite pool) **and** `OPENALEX_APIKEY`. An API key has been required since February 2026; without one the script warns at load and requests will fail. Keys are free at <https://openalex.org/settings/api>.
- **ORCID API** — uses `rorcid`; configure an ORCID token per the package docs
- **Crossref API** — via `rcrossref`
- **OpenStreetMap / Nominatim** — geocoding via `tidygeocoder` (Sections 9 and 15)
- **spaCy** — Python NLP backend; provisioned reproducibly via `setup_python.R` (see [below](#python--spacy-setup))
- **Ollama** — local LLM server, used for both organisation harmonisation and faculty classification (see [below](#faculty-classification-ollama-llm))

### Python / spaCy setup

Sections 8 and 14 run spaCy NER through `spacyr`. spacyr (1.3.x) manages its own
virtual environment named **`r-spacyr`**, and `spacy_initialize()` uses it
automatically.

Provision it once with spacyr's own installer, wrapped in `setup_python.R`:

```r
source("setup_python.R")     # creates r-spacyr + installs spaCy & the 4 models
```

This installs spaCy 3.8.11 plus the four language models the pipeline uses
(`en_core_web_lg`, `de_core_news_lg`, `fr_core_news_lg`, `zh_core_web_lg`).
`clean_mining.R` then uses the venv automatically — no path to configure. To run
against a different Python installation, pass it to `spacy_initialize()`
directly (`python_executable=`, `virtualenv=` or `condaenv=`).

> **Windows note:** spacyr's `r-spacyr` venv is built on the r-miniconda base,
> and reticulate does not activate conda's DLL directories for a *virtualenv*.
> That leaves `python3xx.dll`'s conda dependencies unresolvable and aborts the R
> session with a `0xC0000005` access violation (often surfacing at an unrelated
> line such as the first `read_csv`). Both `setup_python.R` and Section 1 of
> `clean_mining.R` prepend the conda DLL dirs to `PATH` before spaCy initialises,
> which fixes it.

| File | Role |
|------|------|
| `setup_python.R` | One-time provisioning of the `r-spacyr` venv via `spacy_install()` |

---

## Configuration

All tuneable parameters live in the `config` list at the top of the script (line 84):

```r
config <- list(
  openalex_email   = Sys.getenv("OPENALEX_EMAIL", ""),
  openalex_apikey  = Sys.getenv("OPENALEX_APIKEY", ""),
  year_range       = 2000:2025,
  bbox             = c(xmin = 80, xmax = 150, ymin = 0, ymax = 50),
  api_retry_tries  = 5,
  api_retry_wait   = 10,
  refetch_years    = FALSE,
  ror_chunk_size   = 100L,
  min_window_days  = 7L,
  max_works_per_window = 400,
  harvest_cache    = ".harvest_cache",
  output_dir       = ".",
  ollama_url       = "http://localhost:11434/api/generate",
  ollama_model     = "qwen3.5:9b",
  ollama_cache     = "faculties_ollama.csv",
  org_fuzzy_threshold   = 0.85,
  org_place_tokens      = c("Bremen", "Clausthal", ... "Rostock"),
  org_merge_cache       = "org_merges_ollama.csv",
  org_hierarchy_cache   = "org_hierarchy_ollama.csv",
  org_llm_batch_size    = 20L,
  org_stopwords         = c("of", "the", "der", "die", ... "to")
)
```

| Key | Purpose |
|-----|---------|
| `openalex_email` | Polite-pool email for the OpenAlex API |
| `openalex_apikey` | OpenAlex API key (required since Feb 2026) |
| `year_range` | Publication years to query (2000–2025) |
| `bbox` | Bounding box for Pacific Asia (lon 80–150, lat 0–50) |
| `api_retry_tries` | Max attempts per API call before giving up |
| `api_retry_wait` | Base seconds between retries; Section 6 backs this off exponentially (capped at 120 s, ±20 % jitter) |
| `refetch_years` | `TRUE` re-harvests every year; `FALSE` skips years whose `.rds` already exists |
| `ror_chunk_size` | Institution RORs per works query — keeps the request URL under OpenAlex's 8190-byte limit |
| `max_works_per_window` | Target works per fetched date window; drives the count-based window split |
| `min_window_days` | Reserved; the recursive split currently floors at a one-day span |
| `harvest_cache` | Directory holding completed date windows so a failed year does not discard finished work |
| `output_dir` | Working/output directory; Section 22 writes to `<output_dir>/shiny` |
| `ollama_url` | Ollama REST API endpoint |
| `ollama_model` | LLM model name (default: `qwen3.5:9b`) |
| `ollama_cache` | File caching faculty predictions across runs |
| `org_fuzzy_threshold` | Jaro-Winkler similarity above which two org names become a merge candidate (Section 16a) |
| `org_place_tokens` | City tokens: two org names sharing one are merge candidates, and the shared token is excluded from the substantive-overlap test |
| `org_merge_cache` | Cached LLM same-organisation verdicts |
| `org_hierarchy_cache` | Cached LLM parent/child verdicts |
| `org_llm_batch_size` | Pairs sent to the LLM per request (Sections 16a/16b) |
| `org_stopwords` | Structural words dropped before an org name is tokenised |

---

## Pipeline Overview

```
┌─────────────────────────────────────────────────────────────────┐
│  1. Configuration (+ openalexR version guard, Windows DLL fix)  │
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
│  14. Fetch Crossref abstracts, run spaCy NER                    │
│  15. Regex + NLP matching (same pipeline as OpenAlex)           │
├─────────────────────────────────────────────────────────────────┤
│  MERGE & CLEAN                                                  │
│  16.  Merge OpenAlex + ORCID datasets                           │
│  16a. Detect same-organisation name variants (LLM-verified)     │
│  16b. Detect organisation parent/child hierarchy (LLM-verified) │
│  17.  Clean researchers (names, orgs, depts, faculty assignment)│
│  18.  Clean publications (dedup, title normalisation)           │
│  19.  Clean keywords & geodata (categorise, then snap to grid)  │
│  20.  Final filtering & CSV/GeoJSON export                      │
├─────────────────────────────────────────────────────────────────┤
│  NETWORKS & APP DELIVERY                                        │
│  21. Citation networks: reference edges, metadata, direct edges │
│  22. Compact every output to gzip .rds in shiny/                │
└─────────────────────────────────────────────────────────────────┘
```

---

## Section Reference

| # | Line | Title | Purpose |
|---|------|-------|---------|
| 1 | 78 | Configuration | Centralise API credentials, paths, thresholds; guard the openalexR version; apply the Windows spaCy DLL fix |
| 2 | 178 | Library Loading | Load all R packages |
| 3 | 222 | Keyword Dictionaries | Define country/region keyword vectors for Asia filtering |
| 4 | 461 | External Dictionary Loading | Validate that required inputs exist, then load harmonisation tables and spatial reference data |
| 5 | 492 | Helper Function Definitions | Reusable functions (keyword matching, NLP, HTTP/API, LLM) |
| 6 | 942 | OpenAlex — Fetch Institutions & Pubs | Query all Northern German institutions and harvest their publications year by year |
| 7 | 1307 | OpenAlex — Regex Keyword Matching | Tag Asia-related publications via keyword regex over titles |
| 8 | 1353 | OpenAlex — NLP Entity Extraction | Run spaCy NER on abstracts for geographic/org entities |
| 9 | 1394 | OpenAlex — Geocoding Entities | Geocode GPE/LOC entities via OpenStreetMap, in resumable chunks of 1000 |
| 10 | 1455 | OpenAlex — Join Regex + NLP Results | Combine regex- and NLP-matched pubs into the Pacific Asia set |
| 11 | 1505 | OpenAlex — Author & Researcher Processing | Unnest authorship, guard against affiliation leakage and unreliable institution assignments, build the researcher table |
| 12 | 1900 | ORCID — Mine Researchers & Employments | Search ORCID for affiliated researchers, fetch employment records |
| 13 | 2050 | ORCID — Mine Works | Fetch all publications for the mined ORCID researchers |
| 14 | 2092 | ORCID/Crossref — Fetch Abstracts & NLP | Get Crossref abstracts, run spaCy NER |
| 15 | 2206 | ORCID — Regex + NLP Matching | Apply the same regex/NLP matching to ORCID publications |
| 16 | 2329 | Merge — OpenAlex + ORCID Datasets | Combine both sources into unified works, entity and researcher tables |
| 16a | 2434 | Auto-detect Same-Organisation Variants | Generate candidate org-name pairs (Jaro-Winkler, acronym signature, shared place token), verify every pair with the LLM |
| 16b | 2595 | Auto-detect Organisation Hierarchy | Find orgs whose token sequence is contained in another's, verify each parent/child pair with the LLM |
| 17 | 2733 | Data Cleaning — Researchers | Harmonise names, orgs and departments; apply merges and hierarchy; assign faculties |
| 18 | 3166 | Data Cleaning — Publications | Deduplicate and normalise publication records |
| 19 | 3224 | Data Cleaning — Keywords & Geodata | Clean entity keywords, categorise points on true coordinates before snapping to the one-degree grid, apply toponym corrections |
| 20 | 3475 | Final Outputs & Exports | Apply final filters, derive employment windows, write the output CSVs and GeoJSON |
| 21 | 3644 | Citation Networks | Unnest `referenced_works` into edge lists, fetch reference metadata, derive direct intra-corpus citations |
| 22 | 3815 | Compact Outputs to .rds | Repair text encoding and re-save every output as a gzip `.rds` in the `shiny/` app folder |

Line numbers are current as of this revision. The section banners are greppable if they drift — e.g. `grep -n "^# SECTION 21:" clean_mining.R`.

---

## Helper Functions

Defined in Section 5 (line 492 onwards):

| Function | Signature | Purpose |
|----------|-----------|---------|
| `remove_false_positives` | `(text)` | Strip known false-positive terms ("kawasaki disease", "kyoto protocol", medical jejun- terms) that would otherwise trigger Asia-keyword matches |
| `filter_entities` | `(df)` | Apply the shared entity-type and text-quality filters to an NLP entity data frame |
| `map_keyword_to_country` | `(kw)` | Map a keyword string to its country of origin via the `keyword_country_map` lookup |
| `match_keywords_in_row` | `(i, data, keywords)` | Match Asia keywords against row `i` of a pre-filtered data frame; returns one row per match |
| `auto_spacy_entity` | `(text)` | Detect the language with cld3, initialise the matching spaCy model (en/de/fr/zh) and extract named entities; keeps the active model in a closure to avoid redundant re-initialisation |
| `extract_entities` | `(id, abstract, .counter_env)` | Wrap `auto_spacy_entity()` with progress reporting via an environment-based counter |
| `unescape_once` | `(x)` | Single-pass HTML entity unescaping |
| `unescape_twice` | `(x)` | Double-pass HTML entity unescaping |
| `fully_unescape_twice` | `(x)` | Deep HTML unescaping via XML parsing |
| `match_entities_to_PA` | `(entities, pa_locations)` | Keep GPE/LOC entities whose text matches a geocoded Pacific Asia location |
| `fetch_employments` | `(orcid_id, tries, wait)` | Fetch and flatten ORCID employment records with retry logic |
| `fetch_works` | `(orcid_id, tries, wait)` | Fetch ORCID works with retry logic |
| `classify_faculty_ollama` | `(titles, url, model)` | Classify a researcher into a faculty via the local Ollama LLM (see [below](#faculty-classification-ollama-llm)) |
| `classify_pair_batch_ollama` | `(pairs, system_prompt, user_format, url, model)` | Send a batch of `(a, b)` pairs to Ollama as one numbered prompt and parse one YES/NO per line; used by Sections 16a and 16b |

Section 6 defines four further helpers local to the harvest — `harvest_cache_dir()`, `oa_count()`, `fetch_leaf()` and `fetch_window()` — described in the next section.

---

## Harvest Resilience (Section 6)

`openalexR` raises on the *first* failed page and discards the whole call, so a slice spanning *P* pages only lands with probability *p^P*. At a degraded per-request success rate a full year (tens of thousands of works) never completes, however often it is retried. Section 6 therefore harvests in bounded, cacheable units:

| Mechanism | Behaviour |
|-----------|-----------|
| **ROR chunking** | All institution RORs OR'd into one filter build a ~15.6 KB request URL; OpenAlex rejects URLs over 8190 bytes. Each year is fetched in chunks of `config$ror_chunk_size` RORs and stitched, deduplicating on the OpenAlex work id after every chunk. |
| **Count-based windowing** | `oa_count()` issues one cheap `count_only` request per (chunk, year), and the date range is split up front into `ceiling(n / max_works_per_window)` windows — rather than splitting reactively after a timeout, which cascades on a flaky API. |
| **Window cache** | Every completed window is written to `config$harvest_cache` (default `.harvest_cache/`) and reused on the next run, so repeated runs converge instead of restarting. The per-year cache is cleared once that year is assembled. |
| **Exponential backoff** | Retries wait `api_retry_wait · 2^(attempt−1)`, capped at 120 s with ±20 % jitter. |
| **Recursive halving** | A window that exhausts its retries is halved and retried recursively, down to a one-day span. |
| **Atomic year files** | Each `alex_all_pubs_<year>.rds` is written to a `.tmp` file and renamed, so an interrupted write cannot leave a truncated file that the resume check would trust. |
| **Resume** | A year whose `.rds` already exists is skipped unless `config$refetch_years = TRUE`. |
| **Fail loudly** | If any year fails every attempt, the script stops rather than continuing with a partial corpus — a systemic break otherwise looks identical to "no records found". Years that fetched cleanly but held nothing are reported separately. |

---

## Organisation Harmonisation (Sections 16a/16b/17)

Organisation names arrive from OpenAlex and ORCID in many variants. Three mechanisms reconcile them; all LLM verdicts are cached to CSV, so re-runs only classify genuinely new pairs.

### 16a — Same-organisation variants

Three generators propose candidate pairs from the distinct organisation names:

1. **Jaro-Winkler** similarity ≥ `config$org_fuzzy_threshold`
2. **Acronym signature** equality or prefix (an all-caps token is kept verbatim, every other token contributes its first letter)
3. **Shared place token** plus at least one substantive shared non-place, non-generic token

Every candidate — regardless of generator — is then verified by the LLM, which answers YES only for the *exact same* organisation and NO for merely related ones (sister institutes, branches, departments). Verdicts land in `org_merges_ollama.csv` (`short`, `long`, `llm_confirmed`).

### 16b — Parent/child hierarchy

Candidate parents are organisations whose token sequence is a contiguous subsequence of the child's tokens, bucketed by first token to keep the scan cheap. Each candidate is LLM-verified; for each child the **longest** confirmed parent is taken as the direct parent, and multi-level depth emerges by chaining parent links. Verdicts land in `org_hierarchy_ollama.csv` (`parent`, `child`, `llm_confirmed`).

### 17 — Application

Section 17 combines the manual `universities.csv` with the auto-confirmed merges, **manual entries always winning on conflict**, and resolves each name to its canonical form by iterating the lookup to a fixed point. Two guards keep that walk safe:

- An auto-merge is kept only if its two names share a substantive (non-generic) token.
- The walk stops the moment a hop would drift to a name sharing no substantive token with the *original* organisation — unless the hop is a curated `universities.csv` edge, which is always trusted (these cover legitimate acronym expansions).

Without them, a single bad fuzzy edge would let chains of unrelated universities collapse into one sink, silently rewriting correct affiliations.

Hierarchy promotion runs *after* harmonisation, so it operates on canonical names: a row whose organisation has a confirmed parent has the parent promoted to `organization_name`, and the child name slides into `department_name` only if that slot was empty. The promotion iterates so multi-level chains collapse to the root.

---

## Faculty Classification (Ollama LLM)

### Why an LLM?

Each researcher is assigned to one of 8 German university faculties based on their publication titles. A local LLM brings inherent knowledge of academic disciplines and handles the mixed German/English titles in the corpus natively, without a labelled training set.

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

Precedence, strongest first: per-researcher override → LLM → organisation–department concordance.

The assignment runs in Section 17, in this order:

1. **Concordance lookup** — join `faculties.csv` by organisation + department name
2. **Backfill** — within each ORCID group, propagate any concordance value to the researcher's other rows
3. **LLM classification** — for every distinct ORCID, not only those the concordance left empty:
   - Load cached predictions from `faculties_ollama.csv`
   - Identify ORCIDs not yet in the cache
   - Aggregate their publication titles from the works table
   - Send each researcher's titles to Ollama as a zero-shot classification prompt
   - Parse the JSON response (`{"faculty": "<code>", "confidence": <0-1>}`)
   - Append new predictions to the cache CSV
4. **Merge** — the prediction becomes the value of `faculties` and is also kept in `predicted_faculties`. The concordance applies only where the model has no verdict.
5. **Per-researcher override** — if `researcher_faculties.csv` is present, its entries overwrite `faculties` outright, outranking both the model and the concordance.

`faculties.csv` keys on organisation and department rather than on the individual researcher.

`prediction_confidence` is the model's own self-assessment, not a calibrated probability.

### Setup

```bash
# Install and start Ollama
ollama serve

# Pull the model (once)
ollama pull qwen3.5:9b
```

If Ollama is unreachable, `classify_faculty_ollama()` warns per attempt and returns `NULL`; the researcher's prediction is recorded as `NA` and the pipeline continues. Check the console for warnings if `predicted_faculties` comes back empty across the board.

### Caching

Predictions are cached in `faculties_ollama.csv` (columns: `orcid`, `predicted_faculties`, `prediction_confidence`). On subsequent runs, only researchers not already in the cache are sent to the LLM. Delete this file to force re-classification of all researchers.

### Tuning

- **Model**: change `config$ollama_model` (e.g. `"llama3:8b"`, `"mistral"`). It is shared with Sections 16a/16b.
- **Temperature**: hardcoded to 0 for deterministic output (in `classify_faculty_ollama`)
- **Retries**: 3 attempts per researcher on malformed JSON or an invalid faculty code
- **Title truncation**: titles longer than 3,000 characters are truncated

---

## Citation Networks (Section 21)

Section 21 turns the OpenAlex `referenced_works` list-column into edge lists the Shiny app can assemble on the fly for any filtered selection.

- **Bibliographic coupling** and **co-citation** both come from `references_edges.csv` (`paper_id`, `ref_id`). `paper_id` is the DOI-based key the app already uses, so the edge list joins directly to filtered publications.
- References cited by only **one** corpus paper are dropped: they can form no co-citation or coupling edge, so this bounds the file losslessly (`cocit_min_ref_cites = 2`).
- Reference titles and years are not in the corpus, so metadata for every retained reference is fetched from OpenAlex in batches of 50 and cached in `references_meta.csv` (`ref_id`, `ref_label`, `ref_title`, `ref_year`, `ref_cited_by`, `ref_doi`, `ref_first_author`). Re-runs fetch only what is new, plus any cached row still missing a first author — which is what the app's Author Co-Citation network needs.
- **Direct intra-corpus citations** (`citation_edges_direct.csv`, `from_id` → `to_id`) come from references that are themselves corpus papers, matched on the bare OpenAlex W-id that Section 6 retains as `openalex_id`. This uses the full, unpruned edge set, since a citation is valid even when the cited paper is referenced only once. If the harvested data carries no `openalex_id`, the step is skipped with a message.

---

## Shiny Compaction (Section 22)

Section 22 re-saves each generated CSV/GeoJSON as a gzip binary `.rds` written straight into `<output_dir>/shiny`, so `app.R` loads them with `read_rds()` at startup and no files need copying. It reads the just-written file back from disk rather than the in-memory object, which guarantees the `.rds` matches the `.csv`.

It also repairs text encoding on the way through:

- Bytes that are not valid UTF-8 are reinterpreted from Windows-1252 (falling back to Latin-1) before any regex touches them.
- Named and numeric HTML entities are decoded in two passes, catching double-encoded text.
- Classic UTF-8-read-as-Latin-1 mojibake (`Ã¤`, `Â°`) is round-tripped back.
- `references_meta.csv` gets a scoped pass for cited-author names and titles whose accented byte was destroyed upstream to U+FFFD. Each pattern matches only the corrupted form, so legitimate Nordic and Portuguese spellings are left intact.

Two extra artefacts are written for the app:

| File | Content |
|------|---------|
| `shiny/sf_countries_PA.geojson` | The Section 4 country reference **plus** a merged "Korea" polygon (North ∪ South), so a standalone "Korea" mention shades the whole peninsula. The union goes only into this app copy — never into the `sf_countries` used for the Section 19 spatial join, where it would double-match points already inside either Korea. |
| `shiny/nd_institutions.rds` | A small Northern-German institution name set (a few hundred strings) so the app need not load the full `insts_chikon.rds`: the OpenAlex seed `display_name`s plus their `universities.csv` canonicals, so both raw and merged forms are recognised. |

If the `shiny/` folder does not exist, the section warns and skips rather than failing.

---

## Input Files

### Provided by the user (must exist before running)

Section 4 validates the first six and stops with a list of anything missing.

| File | Purpose |
|------|---------|
| `db_sf_filter.csv` | Entity strings to exclude from NLP results |
| `universities.csv` | Organisation name harmonisation table |
| `departments.csv` | Department name harmonisation table |
| `faculties.csv` | Manual faculty classification (org + dept → faculty) |
| `sf_countries_PA.geojson` | Country polygons for Pacific Asia |
| `db_sf_countries.csv` | Country name list for spatial filtering |
| `researcher_faculties.csv` | *Optional.* Per-researcher faculty overrides (`orcid`, `faculties`); wins over every other faculty source |

### Auto-generated (written by the script, read back at checkpoints)

| File | Written at | Read back at |
|------|-----------|-------------|
| `insts_de.rds` | Section 6 | Sections 6, 11 |
| `insts_chikon.rds` | Section 6 | Sections 6, 22 |
| `alex_all_pubs_<year>.rds` | Section 6 (one per year, atomic) | Section 6 combine step |
| `.harvest_cache/` | Section 6 (completed date windows) | Section 6 on re-run |
| `alex_all_pubs.rds` | Section 6 | Sections 7, 20 |
| `alex_matched_by_regex.rds` | Section 7 | Section 8 |
| `entities_alex_abstracts.rds` | Section 8 | Section 9 |
| `geo_entities_chunk_<n>.csv` | Section 9 (chunks of 1000) | Section 9 recombine + resume |
| `entities_osm.geojson` | Sections 9, 15 | Sections 10, 15 |
| `alex_all_pubs_PA.rds` | Section 10 | Sections 10, 11, 20, 21 |
| `entities_alex_abstracts_PA.csv` | Section 11 | Section 11 |
| `chikon_pubs_unnest.rds` | Section 11 | Section 20 |
| `chikon_pubs_unnest_ror.rds` | Section 11 | Section 20 |
| `complete_works_PA_alex.csv` | Section 11 | Section 11 |
| `complete_researchers_PA_alex.csv` | Section 11 | Section 11 |
| `results_complete.rds` | Section 12 | Section 12 |
| `my_osu_employment.rds` | Section 12 (checkpointed every 100 records) | Section 12 |
| `complete_researchers_orcid.csv` | Section 12 | Section 13 |
| `my_orcid_works.rds` | Section 13 | Sections 13, 14, 20 |
| `abstracts_crossref.csv` | Section 14 | Section 14 |
| `entities_orcid_abstracts.csv` | Section 14 | Section 15 |
| `orcid_matched_by_regex.csv` | Section 15 | Section 15 |
| `geo_entities_added_orcid.geojson` | Section 15 | Section 15 |
| `orcid_matched_by_spacy.csv` | Section 15 | Section 15 |
| `complete_works_orcid_PA_joined.csv` | Section 15 | Section 15 |
| `complete_works_PA_preview.csv` | Section 16 | Sections 16, 17 |
| `entities_complete.csv` | Section 16 | Section 16 |
| `complete_researchers_PA_preview.csv` | Section 16 | Sections 16a, 16b, 17 |
| `org_merges_ollama.csv` | Section 16a (incremental LLM cache) | Sections 16a, 16b, 17 |
| `org_hierarchy_ollama.csv` | Section 16b (incremental LLM cache) | Sections 16b, 17 |
| `faculties_ollama.csv` | Section 17 (incremental LLM cache) | Section 17 |
| `references_meta.csv` | Section 21 (incremental metadata cache) | Sections 21, 22 |

---

## Output Files

Written in Section 20:

| File | Content |
|------|---------|
| `complete_works_PA.csv` | Final deduplicated publications (OpenAlex + ORCID), with OpenAlex `cited_by_count` where available |
| `complete_researchers_PA.csv` | All researcher affiliation records, with derived employment windows |
| `complete_researchers_PA_latest.csv` | One row per researcher (latest affiliation that carries an organisation) |
| `complete_spacy_PA_keywords.csv` | NLP-extracted entities (keywords) |
| `complete_spacy_PA_geo.geojson` | Geocoded entities as spatial features |
| `complete_funding_PA.csv` | Publication–author–funder links (`id`, `orcid`, `organization`); publication metadata joins from `complete_works_PA.csv` on `id` |
| `years_normal.csv` | Publication counts by year (full corpus, for the baseline graph) |
| `counted_coop_countries.csv` | Co-authorship country counts by publication and year |

Written in Section 21:

| File | Content |
|------|---------|
| `references_edges.csv` | Paper → reference edge list (source for co-citation and bibliographic-coupling networks) |
| `references_meta.csv` | Reference metadata (label, title, year, cited-by, DOI, first author) for labelling co-citation nodes |
| `citation_edges_direct.csv` | Direct intra-corpus citation edges (citing → cited DOI) |

Written in Section 22, into `<output_dir>/shiny`:

| File | Content |
|------|---------|
| `*.rds` | Every CSV above, plus `complete_spacy_PA_geo.rds`, encoding-repaired and gzip-compressed for `app.R` |
| `sf_countries_PA.geojson` | Country polygons including the merged Korea polygon |
| `nd_institutions.rds` | Northern-German institution name set for the app's filters |

---

## Checkpoints & Restart Points

The script writes intermediate results at each major stage. Sections that resume from disk begin with a `# ── CHECKPOINT / RESTART POINT ──` comment followed by a `read_csv` / `read_rds` / `read_sf` call that reloads the previous section's output. Sections 17–19 are the exception: they clean in memory from the tables loaded at the Section 17 checkpoint.

**To restart from a specific section**, run Sections 1–5 (config, libraries, dictionaries, external data, helpers), then jump to the desired checkpoint. For example, to iterate on faculty classification without re-running the API pipeline:

```r
# Run Sections 1-5 first (config, libraries, helpers), then jump to Section 17:
complete_works_PA <- read_csv("complete_works_PA_preview.csv", show_col_types = F)
complete_researchers_PA <- read_csv("complete_researchers_PA_preview.csv", show_col_types = F)
# ... continue from Section 17 onwards
```

The three LLM caches (`org_merges_ollama.csv`, `org_hierarchy_ollama.csv`, `faculties_ollama.csv`), the reference metadata cache (`references_meta.csv`) and the Section 6 window cache (`.harvest_cache/`) all make re-runs incremental: only genuinely new work is sent to the API or the model. Delete a cache to force that stage to run again from scratch.
