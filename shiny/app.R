# ==============================================================================
# Atlas der Ostasien-Forschung in Norddeutschland
# (East Asia Research Atlas for Northern Germany)
# ==============================================================================
# 
# A Shiny web application for exploring research activities related to East Asia
# at universities in Northern Germany. This interactive atlas visualizes 
# researchers, publications, and funding data with geographical and temporal 
# filtering capabilities.
#
# Author:       Dr. Thorben Pelzer
# Project:      ChiKoN - China-Kompetenz im Norden 
# Institute:    China Center, Kiel University (CAU)
# Year:         2025
# License:      CC BY-SA 4.0
#
# Data Sources: ORCiD, OpenAlex, Crossref
# R Version:    4.5.1+
# ==============================================================================

# ------------------------------------------------------------------------------
# Required Libraries
# ------------------------------------------------------------------------------

# Core Shiny packages
library(shiny)            # Web application framework
library(shinyjs)          # JavaScript operations in Shiny
library(shinythemes)      # Bootstrap themes for Shiny
library(shinyWidgets)     # Custom input widgets
library(shinycssloaders)  # Loading animations
library(htmlwidgets)      # HTML widget framework
library(bslib)            # Bootstrap styling

# Data manipulation and analysis
library(tidyverse)        # Data wrangling ecosystem

# Network and graph analysis
library(igraph)           # Network analysis
library(visNetwork)       # Interactive network visualization
library(Matrix)           # Sparse matrices for citation-network projections

# Visualization packages
library(ggplot2)          # Grammar of graphics
library(ggtext)           # Enhanced text rendering for ggplot2
library(ggrepel)          # Label positioning for ggplot2
library(wordcloud2)       # Interactive word clouds
library(viridisLite)      # Color scales for visualization

# Geospatial analysis
library(sf)               # Simple features for spatial data
library(leaflet)          # Interactive maps (Karte tab)

# File export
library(writexl)          # Excel file export

# ------------------------------------------------------------------------------
# Application Configuration
# ------------------------------------------------------------------------------

# Set maximum file size
options(shiny.maxRequestSize = 100 * 1024^2)

options(
  shiny.autoreload = FALSE,
  shiny.minified = TRUE,
  bslib.precompiled = TRUE,
  sass.cache = TRUE
)

# ------------------------------------------------------------------------------
# Data Mappings and Configuration
# ------------------------------------------------------------------------------

# University city mappings for Northern German research institutions
city_mapping <- list(
  bremen     = "Bremen",
  clausthal  = c("Clausthal","Clausthal-Zellerfeld"),
  flensburg  = "Flensburg",
  greifswald = "Greifswald",
  hamburg    = "Hamburg",
  kiel       = "Kiel",
  luebeck    = "Lübeck",
  lueneburg  = "Lüneburg",
  oldenburg  = "Oldenburg",
  rostock    = "Rostock"
)

# Regional mappings for East Asian countries and territories
region_mapping <- list(
  china    = "China",
  japan    = "Japan", 
  korea    = c("South Korea", "North Korea", "Korea"),
  taiwan   = "Taiwan",
  maritim  = "maritim",
  sonstige = c("Bhutan","India", "Bangladesh", "Mongolia", "Russia", "Sri Lanka", "Nepal","Kyrgyzstan","Allgemein"),
  sea      = c("Thailand", "Vietnam", "Philippines", "Indonesia", "Brunei", 
               "Myanmar", "Burma", "Laos", "Cambodia", "Malaysia", "Singapore")
)

# Faculty code to display name mappings (CAU-based taxonomy)
faculties_mapping <- c(
  "Agrar & Ernährung"      = "agrar",
  "Geisteswissenschaften"  = "phil", 
  "Jura"                   = "jura",
  "Medizin"                = "med",
  "Naturwissenschaften"    = "mint",
  "Religion"               = "rewi",
  "Technik"                = "tech",
  "Wirtschaft & Soziales"  = "sowi",
  "Nicht zugeordnet"       = "NA"
)

# Faculty mapping table for data processing
faculties_mapping_tbl <- tibble(
  faculty_code = c("agrar", "phil", "jura", "med", "mint", "rewi", "tech", "sowi", NA),
  faculty_label = c(
    "Agrar & Ernährung",
    "Geisteswissenschaften", 
    "Jura",
    "Medizin",
    "Naturwissenschaften",
    "Religion",
    "Technik", 
    "Wirtschaft & Soziales",
    "Nicht zugeordnet"
  )
)

# Simplified regional mappings for display purposes
region_mapping_basic <- list(
  china    = "China",
  japan    = "Japan",
  korea    = "Korea",
  taiwan   = "Taiwan",
  maritim  = "Meeren in Asien",
  sea      = "Südostasien",
  sonstige = "sonstigen Gebieten"
)

# Country-name aliases that should be folded into the canonical sf_countries$NAME
# before the map's country/dot split. Without this, a publication that only
# mentions a German country name (e.g. "Kambodscha") produces a dot inside the
# country instead of highlighting the country polygon.
country_text_aliases <- c(
  "Kambodscha"  = "Cambodia",
  "Bangladesch" = "Bangladesh",
  "Nordkorea"   = "North Korea",
  "Südkorea"    = "South Korea",
  "Philippinen" = "Philippines",
  "Burma"       = "Myanmar",
  "Indien"      = "India",
  "Mongolei"    = "Mongolia",
  "Russland"    = "Russia",
  "Singapur"    = "Singapore"
)

# Award and funding type mappings  
prize_mapping <- c(
  grant          = "Förderung",
  award          = "Preis", 
  "salary-award" = "Vergütete Auszeichnung",
  contract       = "Vertrag"
)

# Publication type mappings (OpenAlex/ORCiD to German labels)
pub_mapping <- c(
  "journal-article"                = "Journalbeitrag",
  "book-chapter"                   = "Buchkapitel", 
  "book"                          = "Monografie",
  "conference-paper"              = "Konferenzbeitrag",
  "conference-poster"             = "Konferenzplakat",
  "conference-abstract"           = "Konferenz-Abstract",
  "conference-output"             = "Konferenz",
  "preprint"                      = "Vorabdruck",
  "working-paper"                 = "Arbeitspapier",
  "report"                        = "Bericht",
  "dissertation-thesis"           = "Abschlussarbeit",
  "edited-book"                   = "Herausgeberschaft",
  "magazine-article"              = "Zeitschriftenbeitrag",
  "newsletter-article"            = "Newsletter-Beitrag",
  "journal-issue"                 = "Sonderausgabe",
  "book-review"                   = "Rezension",
  "blog-post"                     = "Blog-Beitrag",
  "lecture-speech"                = "Vortrag",
  "manual"                        = "Leitfaden",
  "data-set"                      = "Datensatz",
  "online-resource"               = "Online-Ressource",
  "supervised-student-publication" = "Betreuung",
  "other"                         = "Sonstige Veröffentlichung"
)

# ------------------------------------------------------------------------------
# Data Loading
# ------------------------------------------------------------------------------

# The app loads the compacted, gzip-binary .rds files written by clean_mining.R
# §22 (read_rds() is far faster at startup than parsing CSV/GeoJSON). The
# pipeline writes them straight into this app folder. sf_countries_PA is a
# pipeline INPUT (not compacted) and is read from GeoJSON.

# Fail fast with an actionable message if a required data file is missing,
# rather than crashing mid-load with an opaque readr/sf error.
required_data_files <- c(
  "years_normal.rds", "counted_coop_countries.rds", "complete_works_PA.rds",
  "complete_researchers_PA.rds", "complete_researchers_PA_latest.rds",
  "complete_spacy_PA_keywords.rds", "complete_funding_PA.rds",
  "complete_spacy_PA_geo.rds", "sf_countries_PA.geojson"
)
missing_data_files <- required_data_files[!file.exists(required_data_files)]
if (length(missing_data_files) > 0) {
  stop(
    "Cannot start the app — missing required data file(s) in '",
    normalizePath(getwd(), winslash = "/", mustWork = FALSE), "':\n  - ",
    paste(missing_data_files, collapse = "\n  - "),
    "\nRun the data pipeline (clean_mining.R, incl. §22 compaction) and ensure ",
    "the working directory is the app folder.",
    call. = FALSE
  )
}

# Temporal data for publication years
years_normal <- read_rds("years_normal.rds") %>%
  rename(year = publication_year) %>%
  mutate(year = as.numeric(year))

# Cooperation countries data
coop_countries <- read_rds("counted_coop_countries.rds")

# Core research data
complete_works_PA <- read_rds("complete_works_PA.rds")

# Tidy institution names for display: strip a trailing colon and title-case any
# name written in ALL CAPS ("UNIVERSITY OF KIEL" -> "University Of Kiel");
# mixed-case names are otherwise left untouched. Applied at load so every
# downstream view (tables, infoboxes, network labels) is consistent.
fix_inst_caps <- function(x) {
  x <- as.character(x)
  x <- sub("\\s*:+\\s*$", "", x)          # drop trailing colon(s)
  allcaps <- !is.na(x) & grepl("[[:upper:]]", x) & !grepl("[[:lower:]]", x)
  x[allcaps] <- str_to_title(x[allcaps])
  x
}

# Drop garbage department_name values (some ORCID records carry a sentence
# fragment or project-title snippet here, e.g. "w elche Medizin der
# Unsterblichkeit"). Keep a value only if it (a) contains a typical academic-unit
# word OR (b) is short and clean. Blank it when it has no unit word AND is either
# too long (a sentence) or contains a lone lowercase letter (a word-break
# artifact). Conservative: ~17 distinct values are removed, no real departments.
.dept_unit_words <- paste(c(
  "institut","zentrum","centre","center","klinik","clinic","hospital","abteilung",
  "department","departamento","dipartimento","\\bdept\\b","facult","faculty","lehrstuhl",
  "seminar","fachbereich","fachgebiet","arbeitsgruppe","forschung","research","studies",
  "studien","laborator","laboratoire","\\blab\\b","division","\\bunit\\b","\\bgroup\\b",
  "gruppe","chair","section","sektion","programme","\\bprogram\\b","college","akademie",
  "academy","bibliothek","library","museum","school","hochschule","professur","stiftung",
  "foundation","office","initiative","network","consortium","cluster","\\bscience",
  "wissenschaft","graduate","kolleg","beamline","facility","synchrotron","observatory",
  "ministère","ministry","ministerium","escuela","escola","faculté","facultad"),
  collapse = "|")
clean_dept <- function(x) {
  xt <- trimws(as.character(x))
  has_word <- grepl(.dept_unit_words, xt, ignore.case = TRUE)
  nwords   <- str_count(xt, "\\S+")
  stray    <- grepl("(^|\\s)[b-df-hj-np-tv-z](\\s|$)", xt)  # lone lowercase consonant
  bad <- !is.na(x) & nzchar(xt) & !has_word & (nwords > 10 | stray)
  ifelse(bad, NA_character_, x)
}

complete_researchers_PA <- read_rds("complete_researchers_PA.rds")
complete_researchers_PA$organization_name <- fix_inst_caps(complete_researchers_PA$organization_name)
complete_researchers_PA$department_name   <- clean_dept(complete_researchers_PA$department_name)
# Fill missing faculty assignments
complete_researchers_PA$faculties[is.na(complete_researchers_PA$faculties)] <- "NA"

complete_researchers_PA_latest <- read_rds("complete_researchers_PA_latest.rds")
complete_researchers_PA_latest$organization_name <- fix_inst_caps(complete_researchers_PA_latest$organization_name)
complete_researchers_PA_latest$department_name   <- clean_dept(complete_researchers_PA_latest$department_name)

complete_spacy_PA_keywords <- read_rds("complete_spacy_PA_keywords.rds") %>%
  arrange(text)

# Funding and award data
complete_prizes_PA <- read_rds("complete_funding_PA.rds")

# Geospatial data
sf_countries <- read_sf("sf_countries_PA.geojson")   # pipeline input; not compacted
complete_spacy_PA_geo <- local({
  g <- read_rds("complete_spacy_PA_geo.rds")
  # Post-hoc coordinate fix (the data-generation script clean_mining.R is left
  # unchanged by request): these regions were geocoded to bogus points — e.g.
  # "Western China" on the east coast (~120E, 32N), "Central Asia" in the Indian
  # Ocean (~102E, 3N), "South Asia" east of the Philippines (~125E, 11N).
  # Reassign every occurrence to a representative location at app bootup only.
  # All targets are kept inside the project bbox (clean_mining.R config: lon
  # 80-150, lat 0-50). Central Asia's and South Asia's true centres lie at/near
  # lon <80, so they are placed at their eastern, in-bbox extent.
  fixes <- list(
    "western china" = c(90, 36),   # Qinghai / Tibetan Plateau (western interior China)
    "central asia"  = c(82, 45),   # eastern Kazakhstan (Central Asia's eastern, in-bbox edge)
    "south asia"    = c(85, 24)    # eastern Ganges plain (India / Bangladesh)
  )  # c(lon, lat)
  key   <- tolower(trimws(g$text))
  geom  <- sf::st_geometry(g)
  for (nm in names(fixes)) {
    idx <- which(key == nm)
    if (length(idx)) {
      pt <- sf::st_sfc(sf::st_point(fixes[[nm]]), crs = sf::st_crs(g))
      geom[idx] <- pt[rep(1L, length(idx))]
    }
  }
  sf::st_geometry(g) <- geom
  g
})

# Citation-network data (paper -> reference edge list + reference metadata).
# OPTIONAL: produced by clean_mining.R §21. If absent, the citation tab shows a
# "data not available" notice instead of erroring, so the app still starts.
references_edges <- if (file.exists("references_edges.rds")) {
  read_rds("references_edges.rds")
} else {
  tibble(paper_id = character(), ref_id = character())
}
references_meta <- if (file.exists("references_meta.rds")) {
  read_rds("references_meta.rds")
} else {
  tibble(ref_id = character(), ref_label = character(), ref_title = character(),
         ref_year = integer(), ref_cited_by = integer(), ref_doi = character(),
         ref_first_author = character())
}
# ref_first_author may be absent in older metadata caches, or read back as a
# logical all-NA column when the §21 fetch has not populated it — normalise it
# to character so the Author-Co-Citation view degrades gracefully.
if (!"ref_first_author" %in% names(references_meta)) {
  references_meta$ref_first_author <- NA_character_
} else if (!is.character(references_meta$ref_first_author)) {
  references_meta$ref_first_author <- as.character(references_meta$ref_first_author)
}
cocit_available <- nrow(references_edges) > 0
# Author co-citation needs reference first-author names (added to the §21 fetch).
author_cocit_available <- cocit_available && any(!is.na(references_meta$ref_first_author))

# Direct intra-corpus citation edges (citing id -> cited id). Produced by
# clean_mining.R §21 only after a re-harvest that retains OpenAlex W-ids, so it
# is OPTIONAL: absent until then, in which case the direct-citation view shows a
# "data not available" notice.
citation_edges_direct <- if (file.exists("citation_edges_direct.rds")) {
  read_rds("citation_edges_direct.rds")
} else {
  tibble(from_id = character(), to_id = character())
}
direct_cit_available <- nrow(citation_edges_direct) > 0

# Funder -> country (ISO 3166-1 alpha-2, or "EU") for the Förderungen flag
# column. OPTIONAL editorial table; absent funders simply get no flag.
funder_countries <- if (file.exists("funder_countries.csv")) {
  read_csv("funder_countries.csv", show_col_types = FALSE,
           col_types = cols(organization = col_character(), country = col_character()))
} else {
  tibble(organization = character(), country = character())
}

# Country code (or "EU") -> a flag cell: a self-hosted SVG (www/flags/<cc>.svg)
# AND the flag emoji. CSS shows the SVG on desktop (Windows renders emoji only as
# letters) and the emoji on mobile. "" for blank/invalid. cc is validated to two
# A-Z letters, so it is safe to interpolate into the src/alt attributes.
flag_html <- function(cc) {
  cc <- toupper(trimws(as.character(cc)))
  if (is.na(cc) || nchar(cc) != 2L || grepl("[^A-Z]", cc)) return("")
  emoji <- intToUtf8(utf8ToInt(cc) - utf8ToInt("A") + 0x1F1E6)
  code  <- tolower(cc)
  paste0('<img class="flag-svg" src="flags/', code, '.svg" alt="', cc,
         '" title="', cc, '" />',
         '<span class="flag-emoji">', emoji, '</span>')
}

# Country code (or "EU") -> readable German label, for the Förderungen pie chart
# legend. Codes absent here fall back to their raw value.
country_de_names <- c(
  DE = "Deutschland", US = "USA", CN = "China (Festland)", GB = "Großbritannien",
  EU = "Europäische Union", KR = "Südkorea", FR = "Frankreich", JP = "Japan",
  CA = "Kanada", IN = "Indien", AU = "Australien", NL = "Niederlande",
  TW = "Taiwan", MY = "Malaysia", TH = "Thailand", SE = "Schweden",
  BE = "Belgien", ES = "Spanien", CH = "Schweiz", DK = "Dänemark",
  NO = "Norwegen", ID = "Indonesien", AT = "Österreich", SG = "Singapur",
  RU = "Russland", HK = "Hongkong", FI = "Finnland", ZA = "Südafrika",
  VN = "Vietnam", BR = "Brasilien", MN = "Mongolei", SA = "Saudi-Arabien",
  PH = "Philippinen", NZ = "Neuseeland", NP = "Nepal", LK = "Sri Lanka",
  IE = "Irland", HU = "Ungarn", CZ = "Tschechien", BD = "Bangladesch",
  PK = "Pakistan", IT = "Italien", IR = "Iran", CL = "Chile",
  AR = "Argentinien", TR = "Türkei", SI = "Slowenien", PL = "Polen",
  KZ = "Kasachstan", IS = "Island", IL = "Israel", GR = "Griechenland",
  GE = "Georgien", CO = "Kolumbien", AE = "Vereinigte Arabische Emirate",
  PT = "Portugal", MX = "Mexiko", KH = "Kambodscha", MM = "Myanmar",
  UZ = "Usbekistan", KG = "Kirgisistan", EG = "Ägypten")

# Current year for plot credits — falls back to 2026 if the system clock is
# unreachable or returns a non-numeric value.
credit_year <- tryCatch({
  y <- as.integer(format(Sys.Date(), "%Y"))
  if (is.na(y) || y < 2025) 2026L else y
}, error = function(e) 2026L)

plot_caption <- paste0("Suchanfrage via: Pelzer ", credit_year, ", Atlas der Ostasien-Forschung")

# ------------------------------------------------------------------------------
# Helpers
# ------------------------------------------------------------------------------

# Format a publication's author list: "Last, First; First Last; First Last; ..."
# (first author inverted; remaining authors in given-name order; all sorted by surname).
format_authors <- function(last, first) {
  ord <- order(last)
  ordered_last  <- last[ord]
  ordered_first <- first[ord]

  first_author <- str_c(ordered_last[1], ordered_first[1], sep = ", ")
  if (length(ordered_last) > 1) {
    rest <- str_c(ordered_first[-1], ordered_last[-1], sep = " ")
    str_c(c(first_author, rest), collapse = "; ")
  } else {
    first_author
  }
}

# ------------------------------------------------------------------------------
# HTML safety helper
# ------------------------------------------------------------------------------
# Publication titles from OpenAlex/Crossref legitimately contain a small set of
# formatting tags (italics, sub/superscript). The detail panels and tables
# therefore render some data fields as raw HTML — which, with third-party
# profile data (titles, author names, org names, keywords), is a stored-XSS
# vector. sanitize_html() escapes everything first, then re-enables only an
# attribute-free whitelist of formatting tags. Anything else — <script>, event
# handlers, injected attributes/quotes — stays inert as escaped text.
# Vectorised over x. For URLs/attributes, build links with tags$a() instead,
# which escapes attribute values correctly.
sanitize_html <- function(x) {
  escaped <- htmltools::htmlEscape(as.character(x))
  allowed <- c("i", "b", "em", "strong", "sub", "sup")
  for (tag in allowed) {
    escaped <- gsub(paste0("&lt;", tag, "&gt;"), paste0("<", tag, ">"),
                    escaped, ignore.case = TRUE)
    escaped <- gsub(paste0("&lt;/", tag, "&gt;"), paste0("</", tag, ">"),
                    escaped, ignore.case = TRUE)
  }
  escaped
}

# ------------------------------------------------------------------------------
# Citation-network projection engine
# ------------------------------------------------------------------------------
# From the paper->reference edge list (references_edges), build one of two
# networks for a given set of papers, via a single sparse incidence matrix B
# (papers x references, binary):
#   * "cocit"    co-citation        -> crossprod(B)  : references x references
#   * "coupling" bibliographic coupling -> tcrossprod(B) : papers x papers
# Edges are thresholded at min_weight and the graph is capped to the top `cap`
# nodes by total strength so it stays legible and fast. Returns NULL when the
# selection yields no edges. (Benchmarked: well under ~0.1s for typical filters.)
citation_projection <- function(paper_ids, mode = c("cocit", "coupling"),
                                min_weight = 2, cap = 150) {
  mode <- match.arg(mode)
  e <- references_edges[references_edges$paper_id %in% paper_ids, , drop = FALSE]
  if (nrow(e) == 0) return(NULL)

  pf <- factor(e$paper_id)
  rf <- factor(e$ref_id)
  B <- sparseMatrix(i = as.integer(pf), j = as.integer(rf), x = 1,
                    dims = c(nlevels(pf), nlevels(rf)),
                    dimnames = list(levels(pf), levels(rf)))
  B@x[] <- 1  # binarise (a paper either cites a reference or it doesn't)

  M <- if (mode == "cocit") Matrix::crossprod(B) else Matrix::tcrossprod(B)
  diag(M) <- 0
  M <- as(M, "TsparseMatrix")
  keep <- M@i < M@j & M@x >= min_weight          # upper triangle, thresholded
  if (!any(keep)) return(NULL)

  nm <- M@Dimnames[[1]]
  edges <- data.frame(from = nm[M@i[keep] + 1L],
                      to   = nm[M@j[keep] + 1L],
                      weight = M@x[keep],
                      stringsAsFactors = FALSE)

  # Cap to the top `cap` nodes by total strength.
  strength <- tapply(c(edges$weight, edges$weight),
                     c(edges$from, edges$to), sum)
  top <- names(sort(strength, decreasing = TRUE))[seq_len(min(cap, length(strength)))]
  edges <- edges[edges$from %in% top & edges$to %in% top, , drop = FALSE]
  if (nrow(edges) == 0) return(NULL)

  list(node_ids = unique(c(edges$from, edges$to)), edges = edges)
}

# Author co-citation (classic ACA): two cited (first) authors are co-cited when
# a corpus paper references works by both. Built like citation_projection() but
# on a papers x cited-first-author incidence (reference -> first author from
# references_meta). Undirected, weight = number of co-citing papers.
author_cocitation_graph <- function(paper_ids, min_weight = 2, cap = 150) {
  pca <- references_edges[references_edges$paper_id %in% paper_ids, , drop = FALSE]
  if (nrow(pca) == 0) return(NULL)
  fa <- references_meta$ref_first_author[match(pca$ref_id, references_meta$ref_id)]
  keep_rows <- !is.na(fa) & nzchar(fa)
  pca <- data.frame(paper_id = pca$paper_id[keep_rows], author = fa[keep_rows],
                    stringsAsFactors = FALSE)
  pca <- unique(pca)
  if (nrow(pca) == 0) return(NULL)

  pf <- factor(pca$paper_id)
  af <- factor(pca$author)
  B <- sparseMatrix(i = as.integer(pf), j = as.integer(af), x = 1,
                    dims = c(nlevels(pf), nlevels(af)),
                    dimnames = list(levels(pf), levels(af)))
  B@x[] <- 1
  M <- Matrix::crossprod(B)          # authors x authors
  diag(M) <- 0
  M <- as(M, "TsparseMatrix")
  keep <- M@i < M@j & M@x >= min_weight
  if (!any(keep)) return(NULL)
  nm <- M@Dimnames[[1]]
  edges <- data.frame(from = nm[M@i[keep] + 1L], to = nm[M@j[keep] + 1L],
                      weight = M@x[keep], stringsAsFactors = FALSE)
  strength <- tapply(c(edges$weight, edges$weight), c(edges$from, edges$to), sum)
  top <- names(sort(strength, decreasing = TRUE))[seq_len(min(cap, length(strength)))]
  edges <- edges[edges$from %in% top & edges$to %in% top, , drop = FALSE]
  if (nrow(edges) == 0) return(NULL)
  list(node_ids = unique(c(edges$from, edges$to)), edges = edges)
}

# Direct intra-corpus citation graph: keep citation edges whose BOTH endpoints
# are in the current selection, then cap to the top `cap` papers by degree.
# Directed (from = citing paper, to = cited paper). Returns NULL if none.
direct_citation_graph <- function(paper_ids, cap = 150) {
  e <- citation_edges_direct[citation_edges_direct$from_id %in% paper_ids &
                             citation_edges_direct$to_id %in% paper_ids, , drop = FALSE]
  if (nrow(e) == 0) return(NULL)
  deg <- table(c(e$from_id, e$to_id))
  top <- names(sort(deg, decreasing = TRUE))[seq_len(min(cap, length(deg)))]
  e <- e[e$from_id %in% top & e$to_id %in% top, , drop = FALSE]
  if (nrow(e) == 0) return(NULL)
  list(node_ids = unique(c(e$from_id, e$to_id)), edges = e)
}

# Author-level citation graph: who cites whom. Projects the direct paper->paper
# citations (citation_edges_direct) onto their authors — author A cites author B
# when a paper by A references a paper by B. Directed; weight = number of such
# citations. Self-citations (same author) are dropped; capped to the top `cap`
# authors by total strength.
author_citation_graph <- function(paper_ids, cap = 150) {
  e <- citation_edges_direct %>% filter(from_id %in% paper_ids, to_id %in% paper_ids)
  if (nrow(e) == 0) return(NULL)
  pa <- complete_works_PA %>% select(id, orcid) %>% filter(!is.na(orcid)) %>% distinct()
  agg <- e %>%
    inner_join(pa, by = c("from_id" = "id"), relationship = "many-to-many") %>% rename(from = orcid) %>%
    inner_join(pa, by = c("to_id"   = "id"), relationship = "many-to-many") %>% rename(to = orcid) %>%
    filter(from != to) %>%
    count(from, to, name = "weight")
  if (nrow(agg) == 0) return(NULL)
  strength <- tapply(c(agg$weight, agg$weight), c(agg$from, agg$to), sum)
  top <- names(sort(strength, decreasing = TRUE))[seq_len(min(cap, length(strength)))]
  agg <- agg %>% filter(from %in% top, to %in% top)
  if (nrow(agg) == 0) return(NULL)
  list(node_ids = unique(c(agg$from, agg$to)),
       edges = data.frame(from = agg$from, to = agg$to, weight = agg$weight,
                          stringsAsFactors = FALSE))
}

# ------------------------------------------------------------------------------
# Per-tab intro block
# ------------------------------------------------------------------------------
# A small German one-liner shown at the top of each data tab. When `more = TRUE`
# it also shows a link to the Anleitungen tab — used ONLY for views where that
# tab genuinely adds depth (a worked example or fuller explanation), not where
# it merely repeats this one-liner. Link handled server-side via input$goto_anleitungen.
tab_intro <- function(html, more = FALSE) {
  div(
    class = "tab-intro",
    style = paste("background:#eef3f8; border-left:3px solid #428bca;",
                  "padding:7px 12px; margin:2px 0 12px; border-radius:4px;"),
    tags$small(
      HTML(html),
      if (more) {
        tagList(" ",
                tags$a(href = "#",
                       onclick = "Shiny.setInputValue('goto_anleitungen', Math.random(), {priority:'event'}); return false;",
                       "Mehr in den Anleitungen →"))
      }
    )
  )
}

# ------------------------------------------------------------------------------
# Clickable keyword helper
# ------------------------------------------------------------------------------
# Render keyword term(s) as links that drop the term into the search box and
# re-run the existing query filter (see the .kw-link click handler in the UI head
# and the input$keyword_search observer). Vectorised; each term is HTML-escaped
# by tags$a (both the visible text and the data-term attribute).
kw_link <- function(term) {
  vapply(term, function(t)
    as.character(tags$a(href = "#", class = "kw-link", `data-term` = t, t)),
    character(1), USE.NAMES = FALSE)
}

# Build a publication's author list as clickable links (search by author name).
# Mirrors format_authors() formatting; each link's data-term is the full
# name_value, so clicking searches the catalogue for that author. df needs
# columns last, first, name_value.
author_links <- function(df) {
  if (is.null(df) || nrow(df) == 0) return("")
  df <- df[order(df$last), , drop = FALSE]
  disp <- c(paste0(df$last[1], ", ", df$first[1]),
            if (nrow(df) > 1) paste0(df$first[-1], " ", df$last[-1]))
  links <- vapply(seq_along(disp), function(i)
    as.character(tags$a(href = "#", class = "person-link",
                        `data-name` = df$name_value[i], disp[i])),
    character(1))
  paste(links, collapse = "; ")
}

# Bare DOI (strip any doi.org URL prefix) for matching corpus ids and links.
doi_bare <- function(x) sub("^\\s*https?://(dx\\.)?doi\\.org/", "", as.character(x), ignore.case = TRUE)

# Does an institution name a Northern-German corpus location? Detection uses two
# signals: (a) a Northern-German place/state token in the name, and (b) exact
# membership in nd_institutions.rds — the OpenAlex seed institutions plus their
# harmonised canonicals (so e.g. bare "GEOMAR" is recognised), extracted small
# from insts_chikon.rds (the full file is too large to load in the app).
nd_place_tokens <- c("Kiel", "Flensburg", "Clausthal", "Bremen", "Greifswald",
                     "Hamburg", "Lübeck", "Luebeck", "Lubeck", "Lüneburg",
                     "Lueneburg", "Luneburg", "Oldenburg", "Rostock",
                     "Schleswig-Holstein", "Schleswig Holstein", "UKSH",
                     "Mecklenburg-Vorpommern")
nd_institution_set <- if (file.exists("nd_institutions.rds")) {
  tolower(trimws(read_rds("nd_institutions.rds")))
} else character(0)
is_northern_german <- function(org) {
  o <- as.character(org)
  grepl(paste0("\\b(", paste(nd_place_tokens, collapse = "|"), ")\\b"), o, ignore.case = TRUE) |
    tolower(trimws(o)) %in% nd_institution_set
}

# Bulleted list of a researcher's *other* known institutions (from the full
# employment history in complete_researchers_PA). Northern-German ones are shown
# in the site's violet and listed first. `current_org` (the latest affiliation
# already shown) is excluded. Returns NULL when there is no additional one.
prev_institutions_ui <- function(orcid_id, current_org = NA) {
  if (is.null(orcid_id) || is.na(orcid_id)) return(NULL)
  orgs <- complete_researchers_PA$organization_name[
    !is.na(complete_researchers_PA$orcid) & complete_researchers_PA$orcid == orcid_id]
  orgs <- trimws(orgs[!is.na(orgs) & nzchar(orgs)])
  if (length(orgs) == 0) return(NULL)
  # Collapse case variants, keeping the longest (most complete) spelling.
  orgs <- unname(tapply(orgs, tolower(orgs), function(v) v[which.max(nchar(v))]))
  if (!is.na(current_org)) orgs <- orgs[tolower(orgs) != tolower(trimws(current_org))]
  if (length(orgs) == 0) return(NULL)
  nd  <- is_northern_german(orgs)
  ord <- order(!nd, tolower(orgs))          # Northern-German first, then alpha
  orgs <- orgs[ord]; nd <- nd[ord]
  tagList(
    p(tags$b("Frühere Einrichtungen:")),
    tags$ul(lapply(seq_along(orgs), function(i) {
      lbl <- sanitize_html(orgs[i])
      tags$li(HTML(if (nd[i]) paste0(lbl, " <span style='color:#9b0a7d'>&#10003;</span>") else lbl))
    }))
  )
}

# Full infobox for a corpus publication (looked up by its DOI id), matching the
# Publikationen-tab detail: linked authors, title, journal, type, DOI, keywords
# and source. Uses only global data, so it is shared by the Ko-Zitation panel.
corpus_pub_infobox <- function(pub_id) {
  w <- complete_works_PA[complete_works_PA$id == pub_id, , drop = FALSE]
  if (nrow(w) == 0) return(h4(pub_id))
  w1 <- w[1, ]
  authors_df <- complete_researchers_PA_latest[
    complete_researchers_PA_latest$orcid %in% w$orcid,
    c("last", "first", "name_value"), drop = FALSE]
  authors_df <- unique(authors_df[!is.na(authors_df$name_value), , drop = FALSE])
  kw <- sort(unique(complete_spacy_PA_keywords$text[complete_spacy_PA_keywords$id == pub_id]))
  type_lbl <- unname(pub_mapping[w1$type]); if (is.na(type_lbl)) type_lbl <- w1$type
  tagList(
    p(HTML(paste0(author_links(authors_df), " (", sanitize_html(w1$publication_date_year_value), ")"))),
    h4(HTML(sanitize_html(w1$title_title_value))),
    if (!is.na(w1$journal_title_value)) p(HTML(paste0("<i>", sanitize_html(w1$journal_title_value), "</i>"))) else NULL,
    if (!is.na(type_lbl)) p(type_lbl) else NULL,
    if (!is.na(w1$doi) && nzchar(w1$doi)) {
      p(HTML("<b>DOI</b>: "),
        tags$a(href = paste0("https://doi.org/", doi_bare(w1$doi)), target = "_blank", doi_bare(w1$doi)))
    } else NULL,
    if ("cited_by_count" %in% names(w1) && !is.na(w1$cited_by_count) && w1$cited_by_count > 0) {
      p(HTML(paste0("<b>Zitationen</b>: ",
                    format(w1$cited_by_count, big.mark = ".", decimal.mark = ","))))
    } else NULL,
    if (length(kw) > 0) {
      p(HTML(paste0("<b>Schlagwörter</b>: ", paste(kw_link(kw), collapse = ", "))))
    } else NULL,
    if (!is.na(w1$source)) {
      p(HTML(paste0("<span style='color: #888888'>(Datenquelle: ", sanitize_html(w1$source), ")</span>")))
    } else NULL
  )
}

# Infobox for an author node (by ORCiD): name, role, department, organisation
# and an ORCiD link — mirroring the person detail panels.
author_infobox <- function(orcid_id) {
  r <- complete_researchers_PA_latest[complete_researchers_PA_latest$orcid == orcid_id, , drop = FALSE]
  if (nrow(r) == 0) return(h4(orcid_id))
  r1 <- r[1, ]
  ov <- trimws(as.character(orcid_id))
  tagList(
    h4(r1$name_value),
    if (!is.na(r1$role_title)) p(r1$role_title) else NULL,
    if (!is.na(r1$department_name)) p(r1$department_name) else NULL,
    if (!is.na(r1$organization_name)) p(r1$organization_name) else NULL,
    if (!grepl("^no_orcid", ov, ignore.case = TRUE)) {
      p(HTML("<b>ORCiD</b>: "),
        tags$a(href = paste0("https://orcid.org/", ov), target = "_blank", ov))
    } else NULL,
    prev_institutions_ui(orcid_id, r1$organization_name)
  )
}

# Infobox for an author co-citation node (a cited first author, by name): the
# name plus the works by that author that the corpus references.
author_cocit_infobox <- function(author_name) {
  works <- references_meta[!is.na(references_meta$ref_first_author) &
                           references_meta$ref_first_author == author_name,
                           c("ref_title", "ref_year", "ref_doi"), drop = FALSE]
  works <- unique(works)
  works <- works[order(-suppressWarnings(as.numeric(works$ref_year))), , drop = FALSE]
  # If the cited author is also a corpus researcher, link their name to the
  # person profile and show their last known institution.
  r <- complete_researchers_PA_latest[
    !is.na(complete_researchers_PA_latest$name_value) &
      tolower(complete_researchers_PA_latest$name_value) == tolower(author_name), , drop = FALSE]
  is_corpus <- nrow(r) > 0
  name_html <- if (is_corpus)
    as.character(tags$a(href = "#", class = "person-link", `data-name` = author_name, author_name))
  else sanitize_html(author_name)
  last_inst <- if (is_corpus && !is.na(r$organization_name[1])) r$organization_name[1] else NA
  tagList(
    h4(HTML(name_html)),
    if (!is.na(last_inst)) p(last_inst) else NULL,
    p(tags$em(paste0("Zitierte:r Autor:in – ", nrow(works),
                     " im Korpus referenzierte Arbeit(en)."))),
    if (nrow(works) > 0) {
      tags$ul(lapply(seq_len(min(8L, nrow(works))), function(i) {
        doi_b <- doi_bare(ifelse(is.na(works$ref_doi[i]), "", works$ref_doi[i]))
        ttl <- if (nzchar(doi_b))
          paste0("<a href='https://doi.org/", doi_b, "' target='_blank'>",
                 sanitize_html(works$ref_title[i]), "</a>")
        else sanitize_html(works$ref_title[i])
        tags$li(HTML(paste0(ttl, " (", works$ref_year[i], ")")))
      }))
    } else NULL
  )
}


# ==============================================================================
# ACCESSIBILITY HELPERS
# ==============================================================================
# Give each (unchanged) visualization a screen-reader-only text alternative.
# alt_viz() tags the output element as role="img" referring to a hidden caption;
# alt_desc() renders that caption, whose text a matching output$<id>_desc fills in
# server-side from the same reactive data. Sighted users see only the
# visualization; assistive technology announces it via the dynamic description.
alt_viz <- function(viz, id) {
  attrs <- list(role = "img", `aria-labelledby` = paste0(id, "_cap"))
  if (inherits(viz, "shiny.tag")) {
    do.call(tagAppendAttributes, c(list(viz), attrs))
  } else {
    # htmlwidget outputs (leaflet, visNetwork, wordcloud2) return a shiny.tag.list
    # wrapper; attach the ARIA attributes to the widget's actual <div>, not the list.
    idx <- which(vapply(viz, function(el) inherits(el, "shiny.tag"), logical(1)))[1]
    if (!is.na(idx)) viz[[idx]] <- do.call(tagAppendAttributes, c(list(viz[[idx]]), attrs))
    viz
  }
}
alt_desc <- function(id, label) {
  tags$div(id = paste0(id, "_cap"), class = "sr-only",
           paste0(label, ". "),
           textOutput(paste0(id, "_desc"), inline = TRUE))
}
# Compact "label (count)" list of the top-k categories, for the descriptions.
fmt_top <- function(labels, counts, k = 5) {
  labels <- as.character(labels)
  keep <- !is.na(labels) & nzchar(labels) & !is.na(counts)
  labels <- labels[keep]; counts <- counts[keep]
  if (length(labels) == 0) return("keine")
  ord <- order(-counts); labels <- labels[ord]; counts <- counts[ord]
  k <- min(k, length(labels))
  paste(sprintf("%s (%s)", labels[seq_len(k)],
                formatC(round(counts[seq_len(k)]), format = "d", big.mark = ".", decimal.mark = ",")),
        collapse = ", ")
}


# ==============================================================================
# UI DEFINITION
# ==============================================================================
ui <- fluidPage(
  lang = "de",
  useShinyjs(),
  tags$head(
    tags$meta(charset = "UTF-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "author", content = "Thorben Pelzer"),
    tags$meta(name = "description", content = "Interaktive Analyse Ostasien-bezogener Forschung in Norddeutschland – basierend auf Profildaten, NLP und Kartenvisualisierung."),
    tags$meta(property = "og:title", content = "Atlas der Ostasien-Forschung in Norddeutschland"),
    tags$meta(property = "og:description", content = "Interaktive Analyse Ostasien-bezogener Forschung in Norddeutschland – basierend auf Profildaten, NLP und Kartenvisualisierung."),
    tags$meta(property = "og:url", content = "https://turban.shinyapps.io/chikon/"),
    tags$meta(property = "og:type", content = "website"),
    tags$meta(property = "og:image", content = "https://tp451.github.io/projects/atlas/cover_chikon.png"),
    
    
    # Favicon for standard browsers
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    # Mobile Web App Icons
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
    
    tags$script(HTML("
    $(document).on('shown.bs.collapse', function (e) {
      $(e.target).prev('.panel-heading').find('i.fa').removeClass('fa-plus').addClass('fa-minus');
    });
    $(document).on('hidden.bs.collapse', function (e) {
      $(e.target).prev('.panel-heading').find('i.fa').removeClass('fa-minus').addClass('fa-plus');
    });
  ")),

    # Welcome-modal cookie logic (first-visit intro)
    tags$script(HTML("
    (function() {
      function readCookie(name) {
        var match = document.cookie.match(new RegExp('(?:^|; )' + name + '=([^;]*)'));
        return match ? decodeURIComponent(match[1]) : null;
      }
      function writeCookie(name, value, days) {
        var d = new Date();
        d.setTime(d.getTime() + days * 24 * 60 * 60 * 1000);
        document.cookie = name + '=' + encodeURIComponent(value) +
                          '; expires=' + d.toUTCString() +
                          '; path=/; SameSite=Lax';
      }
      var welcomeLastFocused = null;
      function getWelcomeFocusable(overlay) {
        return Array.prototype.slice.call(
          overlay.querySelectorAll('button, [href], input, select, textarea')
        ).filter(function(el) { return !el.disabled && el.offsetParent !== null; });
      }
      function hideWelcome() {
        var overlay = document.getElementById('welcome-modal');
        if (overlay) overlay.classList.remove('is-visible');
        if (welcomeLastFocused && welcomeLastFocused.focus) welcomeLastFocused.focus();
      }
      document.addEventListener('DOMContentLoaded', function() {
        if (readCookie('chikon_skip_intro') === '1') return;
        var overlay = document.getElementById('welcome-modal');
        if (!overlay) return;
        welcomeLastFocused = document.activeElement;
        overlay.classList.add('is-visible');
        var okBtn = document.getElementById('welcome-modal-ok');
        if (okBtn) {
          okBtn.focus();
          okBtn.addEventListener('click', function() {
            var cb = document.getElementById('welcome-modal-dont-show');
            if (cb && cb.checked) {
              writeCookie('chikon_skip_intro', '1', 365);
            }
            hideWelcome();
          });
        }
        // Keep keyboard focus inside the dialog while it is open (focus trap)
        overlay.addEventListener('keydown', function(e) {
          if (e.key !== 'Tab') return;
          var f = getWelcomeFocusable(overlay);
          if (!f.length) return;
          var first = f[0], last = f[f.length - 1];
          if (e.shiftKey && document.activeElement === first) {
            e.preventDefault(); last.focus();
          } else if (!e.shiftKey && document.activeElement === last) {
            e.preventDefault(); first.focus();
          }
        });
        overlay.addEventListener('click', function(e) {
          // backdrop click does NOT dismiss — user must click button
          if (e.target === overlay) e.stopPropagation();
        });
      });
    })();
  ")),
    
    # Keyboard-accessibility enhancements (tooltips, skip link, table rows)
    tags$script(HTML("
    $(function() {
      // Make the hover tooltips (tutorial screenshots, maps) keyboard-reachable:
      // focusable + exposed as buttons. The CSS :focus-within rule reveals them.
      document.querySelectorAll('.tooltip-image').forEach(function(el) {
        if (!el.hasAttribute('tabindex')) el.setAttribute('tabindex', '0');
        if (!el.hasAttribute('role')) el.setAttribute('role', 'button');
      });
      // Skip link: move keyboard focus into the main content, past the navbar.
      $(document).on('click', '#skip-to-content', function(e) {
        e.preventDefault();
        var main = document.querySelector('.tab-content');
        if (main) { main.setAttribute('tabindex', '-1'); main.focus(); }
      });
      // Result tables: make rows focusable on each draw and let Enter/Space
      // select the focused row (previously only a mouse click worked).
      $(document).on('draw.dt', function(e) {
        $(e.target).find('tbody tr').attr('tabindex', '0');
      });
      $(document).on('keydown', 'table.dataTable tbody tr', function(e) {
        if (e.key === 'Enter' || e.key === ' ' || e.keyCode === 13 || e.keyCode === 32) {
          e.preventDefault();
          var tr = this;
          var tableEl = tr.closest('table');
          var host = tr.closest('.datatables');   // Shiny DT output wrapper; its id is the outputId
          if (!tableEl || !host || !window.Shiny || !($.fn && $.fn.dataTable)) return;
          var api = $(tableEl).DataTable();
          var idx = api.row(tr).index();           // 0-based data index (survives paging/sorting)
          if (idx == null || idx < 0) return;
          // Mirror DataTables single-row selection, then set the same inputs a
          // mouse click would (DT ignores scripted clicks, so drive Shiny directly).
          $(tableEl).find('tbody tr.selected').removeClass('selected');
          $(tr).addClass('selected');
          Shiny.setInputValue(host.id + '_rows_selected', [idx + 1], {priority: 'event'});
          Shiny.setInputValue(host.id + '_row_last_clicked', idx + 1, {priority: 'event'});
        }
      });
    });
  ")),

    # Custom CSS styling
    tags$style(HTML("
    
 
/* --- Force full-viewport width (robust fallback) --- */
 html, body {
   box-shadow: none !important;
     margin: 0;
     padding: 0;
     height: 100%;
     min-height: 100vh;
     width: 100%;
     box-sizing: border-box;
 }
/* Make sure common high-level containers won't limit width */
 #page-wrapper, .container, .container-fluid, .app-wrapper, .shiny-app {
     display: block !important;
    /* avoid inline-block shrinkage */
     width: 100% !important;
    /* always take full width */
     min-width: 100% !important;
     max-width: 100% !important;
    /* override any Bootstrap max-width */
     box-sizing: border-box;
}
/* If body is ever set to flex (your media queries do this), ensure the child grows */
 body {
    /* If you need body as flex for wide-aspect centering, keep it. If not, this keeps body a normal block. */
    /* display: flex;
     justify-content:center;
     */
 }
 body > #page-wrapper, body > .container, body > .container-fluid {
     flex: 1 1 auto;
    /* allow wrapper to expand inside a flex body */
     align-self: stretch;
 }
.tooltip-image {
    position: relative;
    cursor: pointer;
    color: #c34113 !important; 
}
.tooltip-image .tooltip-content {
    visibility: hidden;
    position: absolute;
    top: 120%;
    left: 0;               /* align left edge with trigger */
    z-index: 1000;
    background: white;
    padding: 4px;
    border: 1px solid #ccc;
    border-radius: 8px;
    box-shadow: 0 2px 6px rgba(0,0,0,0.2);
    
    max-width: 90vw;       /* never exceed viewport width */
}
.tooltip-image:hover .tooltip-content,
.tooltip-image:focus .tooltip-content,
.tooltip-image:focus-within .tooltip-content {
    visibility: visible;
}
/* Visible keyboard focus for the tooltip triggers (made focusable via JS). */
.tooltip-image:focus-visible {
    outline: 2px solid #428bca;
    outline-offset: 2px;
    border-radius: 3px;
}
/* Right-anchored variant: pops out leftward (for right-floated triggers like
   the Dokumentation map) so the popup doesn't overflow the right edge. */
.tooltip-image.tooltip-right .tooltip-content {
    left: auto;
    right: 0;
}
.tooltip-content img {
    width: 540px;          /* keep intended size (the hover popup only) */
    height: auto;
    border-radius: 6px;
}
 @media(min-aspect-ratio: 16/9) {
     body {
         display: flex;
         justify-content: center;
    }
     #page-wrapper {
         max-width: calc(100vh * (16 / 9));
         width: 100%;
    }
 }
/*  .irs-from, .irs-to {
  display: none !important;
} */
/* Dynamic Heights */
 .dynamic-height {
     height: 90vh;
     margin: 0 auto;
    /* center horizontally */
}
 .dynamic-height-network {
     height: 85vh !important;
}
 .full-width-plot {
     width: 100% !important;
     padding: 0 !important;
     margin: 0 !important;
}
/* Scrollable Content */
 .scrollable-content {
     flex-grow: 1;
     overflow-y: auto;
     padding: 0;
     margin: 0;
     overflow-x: auto;
    /* allow horizontal scroll */
     -webkit-overflow-scrolling: touch;
    /* smooth on mobile */
}
/* Centered Content */
 .center-content {
     width: 100%;
     justify-content: top;
     align-items: center;
     margin: 0;
     padding: 0;
}
/* Navbar Styling */
 .navbar {
     background-color: #9b0a7d;
}
/* Hover Effect for Buttons */
 .btn-group-toggle > .btn:hover, .btn-group-toggle > .btn.active:hover, .btn-group-toggle > .btn:focus {
     background-color: #428bca !important;
     color: #FFFFFF !important;
}
/* Selected (active) filter buttons: light fill in the site violet (#9b0a7d).
   :not(:hover) keeps the blue hover feedback above. */
 .btn.checkbtn.btn-custom.active:not(:hover) {
     background-color: rgba(155, 10, 125, 0.10) !important;
     color: #9b0a7d !important;
}
/* Suchbegriffe field: same light violet fill once text has been entered */
 #query_general:not(:placeholder-shown) {
     background-color: rgba(155, 10, 125, 0.10) !important;
}
/* Employment-filter checkbox: violet box around the checkmark when checked */
 #filter_by_employment {
     accent-color: #9b0a7d;
     width: 1.05em;
     height: 1.05em;
}
/* Remove Padding and Margin between Fluid Rows */
 .row > .col-sm-4, .row > .col-sm-3, .row > .col-sm-6 {
    /* padding: 0.0% !important;
     */
     padding: 0% !important;
     margin: 0 !important;
}
 .row {
     margin-left: 0 !important;
     margin-right: 0 !important;
}
/* Customize Button */
 .well .btn {
     white-space: normal !important;
     word-break: break-word;
}
 .btn.checkbtn.btn-custom {
     font-size: 14px !important;
     line-height: 1 !important;
}
/* Body Background Color */
 body {
     background-color: #ffffff;
}
 small {
     font-size: 14px;
}
/* Funder country flag: SVG on desktop (emoji don't render on Windows), emoji on mobile */
 .flag-svg {
     display: inline-block;
     width: 1.5em;
     height: 1em;
     object-fit: cover;
     vertical-align: -0.12em;
     box-shadow: 0 0 1px rgba(0,0,0,0.4);
}
 .flag-svg[alt='CH'] {
    /* the Swiss flag is square — keep it square rather than stretching it */
     width: 1em;
}
 .flag-emoji {
     display: none;
}
 @media (max-width: 768px) {
     .flag-svg {
         display: none;
    }
     .flag-emoji {
         display: inline;
         font-size: 1.2em;
    }
}
/* Mobile Responsive Design */
 @media (max-width: 768px) {
     .dynamic-height {
         height: auto !important;
         min-height: 300px;
    }
     .col-sm-3, .col-sm-4, .col-sm-6 {
         width: 100% !important;
         margin-bottom: 5px;
    }
     .navbar-brand {
         font-size: 12px !important;
    }
     .btn {
         font-size: 10px !important;
         padding: 2px 2px !important;
    }
     .well, .wellPanel {
         padding: 1px !important;
         margin: 1px 0 !important;
    }
}
/* Show plot by default, hide the message */
 #sfPlotUnavailable {
     display: none;
}
 #networkUnavailable {
     display: none;
     height: 100%;
     justify-content: center;
     align-items: center;
     display: flex;
}
/* On small screens, hide plot and show the message */
 @media (max-width: 1000px), (max-height: 500px) {
     #sfPlotWrapper {
         display: none;
    }
     #sfPlotUnavailable {
         display: block;
    }
}
/* On small screens: hide the network output container, show the message */
 @media (max-width: 768px) {
     #mynetworkid {
         display: none !important;
    }
     #networkUnavailable {
         display: flex !important;
    }
}
 @media (max-width: 480px) {
     .dynamic-height {
         height: auto !important;
         min-height: 250px;
    }
     .navbar-brand {
         font-size: 10px !important;
    }
     .btn {
         font-size: 10px !important;
         padding: 4px 8px !important;
    }
}
/* Loading modal (shown while filter_data runs) */
.loading-modal-body {
    text-align: center;
    padding: 12px 4px;
}
.loading-modal-body h4 {
    margin-top: 16px;
    margin-bottom: 6px;
}
.loading-modal-body p {
    color: #666;
    margin: 0;
}
.loading-spinner {
    display: inline-block;
    width: 3rem;
    height: 3rem;
    border: 4px solid #eee;
    border-top-color: #9b0a7d;
    border-radius: 50%;
    animation: loading-spin 0.9s linear infinite;
}
@keyframes loading-spin {
    to { transform: rotate(360deg); }
}
/* Screen-reader-only text: visually hidden, still announced by assistive tech.
   Carries the dynamic text alternatives (alt text) of the visualizations. */
.sr-only {
    position: absolute !important;
    width: 1px; height: 1px;
    padding: 0; margin: -1px;
    overflow: hidden;
    clip: rect(0, 0, 0, 0);
    white-space: nowrap;
    border: 0;
}
/* Strong, consistent keyboard focus indicator (keyboard users only, not mouse). */
a:focus-visible, button:focus-visible, .btn:focus-visible,
input:focus-visible, select:focus-visible, textarea:focus-visible,
[tabindex]:focus-visible, .nav > li > a:focus-visible {
    outline: 3px solid #428bca !important;
    outline-offset: 2px;
}
/* Skip-to-content link: off-screen until it receives keyboard focus. */
.skip-link {
    position: absolute;
    left: 8px;
    top: -48px;
    z-index: 10060;
    background: #9b0a7d;
    color: #fff !important;
    padding: 8px 14px;
    border-radius: 0 0 6px 6px;
    transition: top 0.15s ease-in;
}
.skip-link:focus { top: 0; }
/* The skip-link target is a focus landing zone, not a control — no big ring. */
.tab-content:focus { outline: none; }
/* First-start welcome modal */
.welcome-modal-overlay {
    display: none;
    position: fixed;
    inset: 0;
    background: rgba(0, 0, 0, 0.5);
    z-index: 10050;
    align-items: center;
    justify-content: center;
    padding: 16px;
}
.welcome-modal-overlay.is-visible {
    display: flex;
}
.welcome-modal-content {
    background: #fff;
    border-radius: 8px;
    padding: 24px 28px;
    max-width: 640px;
    width: 100%;
    max-height: 90vh;
    overflow-y: auto;
    box-shadow: 0 6px 24px rgba(0, 0, 0, 0.25);
}
.welcome-modal-content h3 {
    margin-top: 0;
    color: #9b0a7d;
}
.welcome-modal-content ul {
    padding-left: 1.2em;
}
.welcome-modal-actions {
    display: flex;
    flex-wrap: wrap;
    align-items: center;
    justify-content: space-between;
    gap: 12px;
    margin-top: 20px;
}
.welcome-modal-actions label {
    font-weight: normal;
    margin: 0;
    font-size: 0.9em;
    color: #444;
}
    "))
  ),
  title = "Atlas der Ostasien-Forschung in Norddeutschland",
  theme = shinytheme("united"),

  # Skip link — lets keyboard users jump past the navbar to the main content.
  tags$a(id = "skip-to-content", href = "#", class = "skip-link", "Zum Inhalt springen"),

  # First-visit welcome modal (shown unless the chikon_skip_intro cookie is set)
  tags$div(
    id = "welcome-modal",
    class = "welcome-modal-overlay",
    role = "dialog",
    `aria-modal` = "true",
    `aria-labelledby` = "welcome-modal-title",
    tags$div(
      class = "welcome-modal-content",
      tags$h3(id = "welcome-modal-title", "Willkommen im Atlas der Ostasien-Forschung in Norddeutschland"),
      tags$p(
        "Diese Datenbank bündelt Forschungsaktivitäten zu Ostasien an Universitäten in ",
        "Norddeutschland und macht sie durchsuchbar und visualisierbar. Die zugrundeliegenden ",
        "Daten stammen aus ORCiD, OpenAlex und Crossref (Stand: Juni 2026)."
      ),
      tags$p("Sie können hier:"),
      tags$ul(
        tags$li("Forschende, Publikationen und Förderungen nach Region, Standort, Fachrichtung und Zeitraum filtern,"),
        tags$li("Suchbegriffe in Titeln und Namen kombinieren (UND/ODER-Logik),"),
        tags$li("die Ergebnisse als interaktive Karte, Netzwerkgraph, Schlagwortwolke oder Statistik betrachten,"),
        tags$li("die gefilterten Daten als Excel- oder CSV-Datei exportieren.")
      ),
      tags$p(
        "Konkrete Anwendungsbeispiele und Hintergründe zur Methodik finden Sie unter den ",
        tags$b("Reitern „Anleitungen\" und „Dokumentation\""), "."
      ),
      tags$div(
        class = "welcome-modal-actions",
        tags$label(
          tags$input(type = "checkbox", id = "welcome-modal-dont-show"),
          " Diese Information nicht mehr anzeigen. ",
          tags$em("Nur wenn diese Option aktiviert ist"),
          ", wird ein funktionales Cookie gesetzt, um die Auswahl zu merken; ansonsten werden keine Cookies gesetzt."
        ),
        tags$button(
          id = "welcome-modal-ok",
          class = "btn btn-primary",
          type = "button",
          "Verstanden"
        )
      )
    )
  ),

  navbarPage(id = "main_navbar",
             "🌐 Atlas der Ostasien-Forschung in Norddeutschland (2025)",
             tabPanel("Katalog",
                      fluidPage(
                        fluidRow(
                          # Left panel for queries
                          column(3, div(class = "dynamic-height",
                                        wellPanel(
                                          div(
                                            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.5em;",
                                            
                                            # Group title and tooltip together
                                            div(
                                              style = "display: flex; align-items: center; gap: 0.3em;",
                                              h4("Forschungsregion"),
                                              tooltip(
                                                fontawesome::fa("info-circle", a11y = "sem", 
                                                                title = "Publikationen müssen eine der gewählten Regionen behandeln, können sich aber zusätzlich auch mit nicht ausgewählten Regionen befassen."),
                                                "Publikationen müssen eine der gewählten Regionen behandeln, können sich aber zusätzlich auch mit nicht ausgewählten Regionen befassen."
                                              )
                                            ),
                                            
                                            # Toggle with label, nudged up for baseline alignment
                                            div(
                                              style = "margin-top: 4px;",
                                              checkboxInput("select_all_regions", "Alle / keine", value = FALSE)
                                            )
                                          ),
                                          checkboxGroupButtons(
                                            inputId = "regions",
                                            choices = c("🏯 China (Festland)" = "china",
                                                        "🗾️ Japan" = "japan",
                                                        "⛰️️️ Korea" = "korea",
                                                        "🌊 Maritim" = "maritim",
                                                        "🗺️ Sonstige" = "sonstige",
                                                        "🌏️️ Südostasien" = "sea",
                                                        "🏞️️️ Taiwan" = "taiwan"
                                            ),
                                            size = "sm",
                                            selected=c("china","maritim"),
                                            status = "custom",
                                            checkIcon = list(
                                              yes = icon("ok",
                                                         lib = "glyphicon"),
                                              no = icon("remove",
                                                        lib = "glyphicon"))
                                          ),
                                          # Label row with title + tooltip grouped tightly, toggle aligned right
                                          div(
                                            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.5em;",
                                            
                                            # Group title and tooltip together
                                            div(
                                              style = "display: flex; align-items: center; gap: 0.3em;",
                                              h4("Standorte"),
                                              tooltip(
                                                fontawesome::fa("info-circle", a11y = "sem", 
                                                                title = "Forschende müssen an einem der Standorte gewirkt haben, aber nicht zwingend heute dort beschäftigt sein."),
                                                "Forschende müssen an einem der Standorte gewirkt haben, aber nicht zwingend heute dort beschäftigt sein."
                                              )
                                            ),
                                            
                                            # Toggle with label, nudged up for baseline alignment
                                            div(
                                              style = "margin-top: 4px;",
                                              checkboxInput("select_all", "Alle / keine", value = FALSE)
                                            )
                                          ),
                                          checkboxGroupButtons(
                                            inputId = "places",
                                            choices = c("🟠 Bremen" = "bremen",
                                                        "🟢️ Clausthal" = "clausthal",
                                                        "🔵️️ Flensburg" = "flensburg",
                                                        "🟤️ Greifswald" = "greifswald",
                                                        "🔴️️ Hamburg" = "hamburg",
                                                        "🟣️ Kiel" = "kiel",
                                                        "🟢️️ Lübeck" = "luebeck",
                                                        "🟠️ Lüneburg" = "lueneburg",
                                                        "🔵️️ Oldenburg" = "oldenburg",
                                                        "🔵️️ Rostock" = "rostock"
                                            ),
                                            size = "sm",
                                            selected=c("kiel"),
                                            status = "custom",
                                            checkIcon = list(
                                              yes = icon("ok",
                                                         lib = "glyphicon"),
                                              no = icon("remove",
                                                        lib = "glyphicon"))
                                          ),
                                          
                                          div(
                                            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.5em;",
                                            
                                            # Group title and tooltip together
                                            div(
                                              style = "display: flex; align-items: center; gap: 0.3em;",
                                              h4("Fachrichtungen"),
                                              tooltip(
                                                fontawesome::fa("info-circle", a11y = "sem", 
                                                                title = "Ein Teil der Zuordnungen wurde automatisiert vorgenommen. Vereinzelte Fehler sind möglich."),
                                                "Ein Teil der Zuordnungen wurde automatisiert vorgenommen. Vereinzelte Fehler sind möglich."
                                              ),
                                            ),
                                            # Toggle with label, nudged up for baseline alignment
                                            div(
                                              style = "margin-top: 4px;",
                                              checkboxInput("select_all_fach", "Alle / keine", value = FALSE)
                                            )
                                          ),
                                          checkboxGroupButtons(
                                            inputId = "faculties",
                                            choices = c("🥦 Agrar & Ernährung" = "agrar",
                                                        "🎓 Geisteswissenschaften" = "phil",
                                                        "⚖️️ Jura" = "jura",
                                                        "⚕️️ Medizin" = "med",
                                                        "🧪️ Naturwissenschaften" = "mint",
                                                        "⛪️ Religion" = "rewi",
                                                        "🛠️️ Technik" = "tech",
                                                        "📈️ Wirtschaft & Soziales" = "sowi"#,
                                                        #"❓ Nicht zugeordnet" =  "NA"
                                            ),
                                            size = "sm",
                                            selected=c("agrar","mint","med","phil","jura","tech","rewi","sowi"),
                                            status = "custom",
                                            checkIcon = list(
                                              yes = icon("ok",
                                                         lib = "glyphicon"),
                                              no = icon("remove",
                                                        lib = "glyphicon"))
                                          ),
                                          
                                          div(
                                            style = "display: flex; align-items: center; margin-bottom: 0.2em;",
                                            h4("Publikationszeitraum", style = "margin: 0;"),
                                            span(
                                              style = "display: inline-block; margin-left: 0.3em;",
                                              tooltip(
                                                fontawesome::fa("info-circle", a11y = "sem",
                                                                title = "Das Jahr 2025 ist nur bis einschließlich September im Datensatz enthalten."),
                                                "Das Jahr 2025 ist nur bis einschließlich September im Datensatz enthalten."
                                              )
                                            )
                                          ),
                                          div(
                                            sliderInput(
                                              inputId = "time_range",
                                              label = "",
                                              sep="",
                                              min=2000,
                                              max=2025,
                                              value = c(2000,2025),
                                              step = 1)
                                          ),
                                          
                                          # Label row with title + tooltip grouped tightly, toggle aligned right
                                          div(
                                            style = "display: flex; align-items: center; justify-content: start; gap: 0.5em; margin-bottom: 0.5em;",
                                            
                                            # Group title and tooltip together
                                            div(
                                              style = "display: flex; align-items: center; gap: 0.3em;",
                                              h4("Suchbegriffe"),
                                              tooltip(
                                                fontawesome::fa("info-circle", a11y = "sem", 
                                                                title = "Mehrere Begriffe bitte mit Leerzeichen trennen."),
                                                "Mehrere Begriffe bitte mit Leerzeichen trennen."
                                              )
                                            ),
                                            
                                            # Toggle with label, nudged up for baseline alignment
                                            div(
                                              style = "margin-top: 15px;",
                                              radioButtons(
                                                inputId = "logic_radio",
                                                label = NULL,
                                                inline = TRUE,
                                                choices = c("Mindestens ein Begriff" = "or", "Alle" = "and"),
                                                selected = "and"
                                              )
                                            )
                                          ),
                                          
                                          # Text input below
                                          textInput("query_general", label = NULL, value = "", placeholder = "Rein optional: Suche ohne Begriffe ist möglich."),
                                          
                                          checkboxInput("filter_by_employment", 
                                                        label = "Auf Publikationen während der Anstellung am Standort begrenzen", 
                                                        value = TRUE),
                                          
                                          div(
                                            style = "display: flex; gap: 0.5em; align-items: center; flex-wrap: wrap;",
                                            uiOutput("action_button", inline = TRUE),  # go button (rendered server-side)
                                            # Always clickable; its colour (grey vs red) is toggled by the
                                            # filter_is_active observer to signal whether filters are active.
                                            actionBttn(
                                              inputId = "reset",
                                              label = "Zurücksetzen",
                                              style = "simple",
                                              color = "default"
                                            )
                                          ),
                                          tags$script(HTML("
                                            $(document).on('keyup', function(e) {
                                            if (e.which == 13 && $(e.target).is('#query_general')) {  // Check if Enter is released inside the textInput
                                            $('#go').click();
                                            }
                                            });
                                            // Clickable keywords: drop the term into the search and re-filter.
                                            $(document).on('click', '.kw-link', function(e) {
                                              e.preventDefault();
                                              Shiny.setInputValue('keyword_search', {term: $(this).attr('data-term'), nonce: Math.random()}, {priority: 'event'});
                                            });
                                            // Clickable author names: open that person on the Personen tab.
                                            $(document).on('click', '.person-link', function(e) {
                                              e.preventDefault();
                                              Shiny.setInputValue('person_link', {name: $(this).attr('data-name'), nonce: Math.random()}, {priority: 'event'});
                                            });
                                          "))
                                        ))
                          ),
                          
                          column(9, 
                                 tabsetPanel(id = "main_tabs",
                                             tabPanel("Personen", value="tab_personen",
                                                      tab_intro("<b>Personen:</b> Forschende mit Ostasien-Bezug an den erfassten norddeutschen Standorten. Ein Klick auf eine Zeile öffnet Details und Publikationen.", more = TRUE), 
                                                      column(9,
                                                             div(class = "center-content scrollable-content",
                                                                 wellPanel(class = "center-content  scrollable-content",
                                                                           withSpinner(DT::dataTableOutput("dataTablePerson", width = "100%"))
                                                                 ))),
                                                      column(3, div(class = "center-content",
                                                                    wellPanel(
                                                                      div(
                                                                        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 0.5em;",
                                                                        downloadButton("download_excel_person", "Auswahl sichern (Excel)"),
                                                                        downloadButton("download_csv_person", "Auswahl sichern (CSV)")
                                                                      )
                                                                    ),wellPanel(
                                                                      uiOutput("dynamic_ui_person")
                                                                    )
                                                      )
                                                      ),
                                             ),
                                             tabPanel("Publikationen", value = "Publikationen",
                                                      tab_intro("<b>Publikationen:</b> Die gefilterten Veröffentlichungen mit Ostasien-Bezug. Ein Klick auf einen Eintrag öffnet Titel, DOI und Schlagwörter."),
                                                      column(9,
                                                             div(class = "center-content scrollable-content",
                                                                 wellPanel(class = "center-content scrollable-content",
                                                                           withSpinner(DT::dataTableOutput("dataTablePub", width = "100%"))
                                                                 ))),
                                                      column(3, div(class = "center-content",
                                                                    wellPanel(
                                                                      div(
                                                                        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 0.5em;",
                                                                        downloadButton("download_excel_pub", "Auswahl sichern (Excel)"),
                                                                        downloadButton("download_csv_pub", "Auswahl sichern (CSV)")
                                                                      )
                                                                    ),wellPanel(
                                                                      uiOutput("dynamic_ui_pubs")
                                                                    )
                                                      )
                                                      ),
                                             ),
                                             tabPanel("Karte",
                                                      tab_intro("<b>Karte:</b> Geografische Verteilung der in Titeln und Abstracts erkannten Orte (Region Asien-Pazifik); Größe und Färbung spiegeln die Häufigkeit."),
                                                      div(class = "center-content full-width-plot",
                                                                   wellPanel(
                                                                     div(id = "sfPlotWrapper",
                                                                         withSpinner(alt_viz(leafletOutput("sfPlot", width = "100%", height = "80vh"), "sfPlot")), alt_desc("sfPlot", "Interaktive Karte der in den Publikationen genannten Orte")
                                                                     ),
                                                                     div(id = "sfPlotUnavailable",
                                                                         tags$p("Karte auf kleinen Bildschirmen nicht verfügbar.", style = "text-align:center; font-weight:bold;")
                                                                     )
                                                                   )
                                             )),
                                             tabPanel(
                                               "Netzwerk (Institutionen)",
                                               tab_intro("<b>Netzwerk:</b> Verbindungen zwischen Forschenden, Publikationen, Einrichtungen und Mittelgebern der aktuellen Auswahl. Ein Klick auf einen Knoten zeigt Details.", more = TRUE),
                                               column(
                                                 9,
                                                 div(
                                                   class = "center-content",
                                                   wellPanel(
                                                     class = "dynamic-height-network center-content",
                                                     alt_viz(visNetworkOutput("mynetworkid", height = "95%"), "mynetworkid"), alt_desc("mynetworkid", "Interaktiver Netzwerkgraph aus Personen, Publikationen, Einrichtungen und Fördermittelgebern"),
                                                     div(
                                                       id = "networkUnavailable",
                                                       style = "display: none; text-align: center; font-weight: bold;",
                                                       "Visualisierung auf kleinen Bildschirmen nicht verfügbar."
                                                     )
                                                   )
                                                 )
                                               ),
                                               column(
                                                 3,
                                                 div(
                                                   class = "center-content",
                                                   wellPanel(
                                                     uiOutput("dynamic_ui_netzwerk")
                                                   )
                                                 )
                                               )
                                             ),

                                             tabPanel(
                                               "Netzwerk (Zitationen)",
                                               tab_intro("<b>Zitationsnetzwerke:</b> Fünf Sichten auf die Literaturbasis (Datengrundlage: OpenAlex). Standard ist die <i>Ko-Zitation der Autor:innen</i> – wer wird gemeinsam zitiert? Die Regler steuern Knotenzahl und Mindeststärke.", more = TRUE),
                                               column(
                                                 9,
                                                 div(
                                                   class = "center-content",
                                                   wellPanel(
                                                     class = "dynamic-height-network center-content",
                                                     alt_viz(visNetworkOutput("cocitNetwork", height = "95%"), "cocitNetwork"), alt_desc("cocitNetwork", "Interaktives Zitationsnetzwerk")
                                                   )
                                                 )
                                               ),
                                               column(
                                                 3,
                                                 div(
                                                   class = "center-content",
                                                   wellPanel(
                                                     radioButtons(
                                                       "cocit_mode", "Netzwerktyp",
                                                       choices = c(
                                                         "Ko-Zitation (Autor:innen)" = "author_cocit",
                                                         "Ko-Zitation (Referenzen)" = "cocit",
                                                         "Zitationen zw. Autor:innen" = "author",
                                                         "Direkte Zitationen (Publikationen)" = "direct",
                                                         "Bibliogr. Kopplung (Publikationen)" = "coupling"
                                                       ),
                                                       selected = "author"
                                                     ),
                                                     sliderInput("cocit_cap", "Max. Knoten",
                                                                 min = 30, max = 400, value = 150, step = 10),
                                                     conditionalPanel(
                                                       condition = "input.cocit_mode != 'direct' && input.cocit_mode != 'author'",
                                                       sliderInput("cocit_min_weight",
                                                                   "Mindest-Verknüpfungsstärke",
                                                                   min = 2, max = 10, value = 2, step = 1)
                                                     )
                                                   ),
                                                   wellPanel(
                                                     uiOutput("dynamic_ui_cocit")
                                                   )
                                                 )
                                               )
                                             ),

                                             tabPanel("Förderungen",
                                                      tab_intro("<b>Förderungen:</b> Beteiligte Mittelgeber der gefilterten Publikationen, nach Häufigkeit.", more = TRUE),
                                                      column(9,
                                                             div(class = "center-content scrollable-content",
                                                                 wellPanel(class = "center-content scrollable-content",
                                                                           withSpinner(DT::dataTableOutput("dataTablePrize", width = "100%"))
                                                                 ))),
                                                      column(3, div(class = "center-content",
                                                                    wellPanel(
                                                                      div(
                                                                        style = "display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 0.5em;",
                                                                        downloadButton("download_excel_prize", "Auswahl sichern (Excel)"),
                                                                        downloadButton("download_csv_prize", "Auswahl sichern (CSV)")
                                                                      )
                                                                    ),
                                                                    wellPanel(
                                                                      uiOutput("dynamic_ui_prize")
                                                                    ),
                                                                    wellPanel(
                                                                      tags$p(tags$b("Gefilterte Publikationen nach Herkunftsregion der Fördereinheit"),
                                                                             style = "text-align:center; font-size:0.85em; margin-bottom:0.4em;"),
                                                                      withSpinner(alt_viz(plotOutput("foerderRegionPie", height = "320px"), "foerderRegionPie")), alt_desc("foerderRegionPie", "Kreisdiagramm der Herkunftsländer der Fördermittelgeber")
                                                                    )
                                                      )
                                                      ),
                                             ),
                                             tabPanel("Statistiken",
                                                      tab_intro("<b>Statistiken:</b> Auswertungen der Auswahl nach Fachbereich, Zeit, Kooperationsländern und Standorten."),
                                                      # First row
                                                      fluidRow(
                                                        column(6,
                                                               div(class = "center-content scrollable-content",
                                                                   wellPanel(
                                                                     withSpinner(alt_viz(plotOutput("barChart_disziplin", width = "100%"), "barChart_disziplin")), alt_desc("barChart_disziplin", "Balkendiagramm: Publikationen je Fachrichtung")
                                                                   )
                                                               )
                                                        ),
                                                        column(6,
                                                               div(class = "center-content scrollable-content",
                                                                   wellPanel(
                                                                     withSpinner(alt_viz(plotOutput("barChart_zeit", width = "100%"), "barChart_zeit")), alt_desc("barChart_zeit", "Liniendiagramm: Publikationen pro Jahr")
                                                                   )
                                                               )
                                                        )
                                                      ),
                                                      # Second row
                                                      fluidRow(
                                                        column(12,
                                                               div(class = "center-content scrollable-content",
                                                                   wellPanel(
                                                                     withSpinner(alt_viz(plotOutput("barChart_koop", width = "100%"), "barChart_koop")), alt_desc("barChart_koop", "Liniendiagramm: Kooperationsländer der Ko-Autor:innen im Zeitverlauf")
                                                                   )
                                                               )
                                                        )
                                                      ),
                                                      # Third row
                                                      fluidRow(
                                                        column(6,
                                                               div(class = "center-content scrollable-content",
                                                                   wellPanel(
                                                                     withSpinner(alt_viz(plotOutput("barChart_region", width = "100%"), "barChart_region")), alt_desc("barChart_region", "Balkendiagramm: Publikationen je Forschungsregion")
                                                                   )
                                                               )
                                                        ),
                                                        column(6,
                                                               div(class = "center-content scrollable-content",
                                                                   wellPanel(
                                                                     withSpinner(alt_viz(plotOutput("barChart_standort", width = "100%"), "barChart_standort")), alt_desc("barChart_standort", "Balkendiagramm: Forschende je Standort")
                                                                   )
                                                               )
                                                        )
                                                      )
                                                      
                                             ),
                                             tabPanel("Schlagwörter",
                                                      tab_intro("<b>Schlagwörter:</b> Häufige Eigennamen (inhaltlich &amp; geografisch) aus Titeln und Abstracts der gefilterten Publikationen."),
                                                      div(class = "center-content scrollable-content",
                                                                         wellPanel(div(class = "center-content",  
                                                                                       h4(HTML(paste0("<br><b>Begriffshäufungen</b>"))),
                                                                                       DT::dataTableOutput("wordTable", width = "100%"),
                                                                                       h4(HTML(paste0("<b>Schlagwortwolke</b>")),tooltip(
                                                                                         fontawesome::fa("info-circle", a11y = "sem", title = "Visualisierung erkannter Eigennamen (inhaltlich & geografisch) in Titeln und Abstracts der gefilterten Publikationen. Der Algorithmus entfernt Begriffe, wenn es der Darstellung dient."),
                                                                                         "Visualisierung erkannter Eigennamen (inhaltlich & geografisch) in Titeln und Abstracts der gefilterten Publikationen. Der Algorithmus entfernt Begriffe, wenn es der Darstellung dient."
                                                                                       )),
                                                                                       withSpinner(alt_viz(wordcloud2Output("wordCloud", width = "100%"), "wordCloud")), alt_desc("wordCloud", "Wortwolke der häufigsten Schlagwörter")
                                                                         ),
                                                                         ))
                                             )),
                          ),
                        ),
                      )),
             tabPanel("Anleitungen",
                      fluidRow(
                        column(6, div(class = "dynamic-height", style = "padding-right: 6px;",
                                      wellPanel(tags$small(HTML(paste0(
                                        "<p><h4>Die einzelnen Ansichten</h4></p>
                                         <p><b>Personen</b> listet die Forschenden der aktuellen Auswahl mit ihrer letztdokumentierten Einrichtung und der Zahl ihrer Ostasien-bezogenen Publikationen. Ein Klick auf eine Zeile öffnet Detailinformationen.</p>
                                         <p><b>Publikationen</b> zeigt die gefilterten Veröffentlichungen. Ein Klick auf einen Eintrag öffnet Titel, Quelle, DOI und die per Eigennamenerkennung gewonnenen Schlagwörter.</p>
                                         <p><b>Karte</b> verortet die in Titeln und Abstracts erkannten Orte (Region Asien-Pazifik); Punktgröße und Färbung spiegeln die Häufigkeit der Nennung wider.</p>
                                         <p><b>Netzwerk</b> verknüpft Forschende, Publikationen, Einrichtungen und Mittelgeber der Auswahl zu einem Graphen. Ein Klick auf einen Knoten zeigt weiterführende Informationen.</p>
                                         <p><b>Zitationsnetzwerke</b> erschließen die Literaturbasis aus den Referenzlisten der Publikationen (Datengrundlage: OpenAlex). Fünf Sichten stehen zur Wahl:</p>
                                         <ul>
                                           <li><b>Ko-Zitation (Autor:innen)</b> – zwei zitierte Autor:innen werden verbunden, wenn eine Korpus-Publikation Arbeiten von beiden zitiert (intellektuelle Grundlage eines Feldes auf Autor:innen-Ebene).</li>
                                           <li><b>Ko-Zitation (Referenzen)</b> – dieselbe Idee auf Werkebene: zwei Referenzen, die gemeinsam zitiert werden.</li>
                                           <li><b>Zitationen zw. Autor:innen</b> – wer zitiert wen direkt (gerichtet).</li>
                                           <li><b>Direkte Zitationen (Publikationen)</b> – Zitationen zwischen den Korpus-Publikationen selbst (gerichtet).</li>
                                           <li><b>Bibliografische Kopplung</b> – Publikationen, die gemeinsame Referenzen teilen.</li>
                                         </ul>
                                         <p>Die Regler steuern die maximale Knotenzahl und – außer bei den gerichteten Zitationssichten – die Mindeststärke der Verknüpfung.</p>
                                         <p><b>Förderungen</b> listet die in den Publikationen genannten Mittelgeber, sortiert nach Häufigkeit.</p>
                                         <p><b>Statistiken</b> fasst die Auswahl grafisch zusammen: nach Fachbereich, im zeitlichen Verlauf, nach Kooperationsländern und nach Standorten.</p>
                                         <p><b>Schlagwörter</b> zeigt die häufigsten erkannten Eigennamen (inhaltlich und geografisch) als Tabelle und als Wortwolke.</p>"
                                      )))),
                                      wellPanel(tags$small(HTML(paste0(
                                        "<p><h4>Anwendungsbeispiele</h4></p>
                                         <p><b>Expertisen-Recherche</b></p>
                                         <p>Eine Hamburger Journalistin möchte eine Interview-Partnerin zum Thema „Südchinesisches Meer“ finden. Sie
                                         aktiviert in der <span class='tooltip-image'>Suchmaske<span class='tooltip-content'>
                                        <img src='anl_1_1.jpg' alt='Screenshot der Suchmaske mit Filteroptionen'></span></span> alle Forschungsregionen und Fachrichtungen, selektiert nur Hamburg als Standort
                                         und gibt die <span class='tooltip-image'>Suchbegriffe<span class='tooltip-content'><img src='anl_1_2.jpg' alt='Screenshot des Suchbegriff-Felds mit UND/ODER-Auswahl'></span></span> „south china sea“ ein. Sie wählt die Option <span class='tooltip-image'>„alle“<span class='tooltip-content'><img src='anl_1_2.jpg' alt='Screenshot des Suchbegriff-Felds mit UND/ODER-Auswahl'></span></span>️️, damit nur Publikationen gefunden
                                         werden, die alle drei Wörter beinhalten. Die Ergebnis-Tabelle mit den relevanten Personen sortiert sie nach
                                         <span class='tooltip-image'>„Publikationen“<span class='tooltip-content'>
                                        <img src='anl_1_3.jpg' alt='Screenshot der nach Publikationszahl sortierten Ergebnistabelle'></span></span>, um die Personen auszumachen, die in der Vergangenheit besonders häufig zum Thema geschrieben haben.
                                         Sie selektiert eine führende Person und klickt in der rechten Spalte auf <span class='tooltip-image'>„Publikationen anzeigen“<span class='tooltip-content'><img src='anl_1_4.jpg' alt='Screenshot der Schaltfläche „Publikationen anzeigen“'></span></span>, um sich über
                                         die Titel der Veröffentlichungen ein genaueres Bild von der Expertise der Person zu machen. Anschließend klickt
                                         die Journalistin noch auf den <span class='tooltip-image'>ORCiD-Link<span class='tooltip-content'>
                                        <img src='anl_1_5.jpg' alt='Screenshot des ORCiD-Links in der Ergebnisspalte'></span></span>, um eine Kontaktadresse des Forschenden ausfindig zu machen.</p>
                                         <p><b>Forschungssicherheit</b></p>
                                         <p>Ein Verwaltungsangestellter aus Kiel ist damit beauftragt, herauszufinden, an welchen Stellen in der Vergangenheit
                                         an seiner Universität mit Drittmitteln des „China Scholarship Council“ (CSC) geforscht wurde. Da er sich nicht sicher sein
                                         kann, ob vorhandene Informationen vollständig sind und sich die Einzelbefragung der Professor:innen als nicht ergiebig
                                         erweist, möchte er seine vorhandenen Daten mit externen Daten abgleichen, um mögliche Lücken zu füllen. Er wählt
                                         daher als <span class='tooltip-image'>Standort-Filter<span class='tooltip-content'>
                                        <img src='anl_2_1.jpg' alt='Screenshot des Standort-Filters'></span></span> nur Kiel aus, lässt aber bei den anderen Filtern alle Optionen aktiv. Nachdem er die Suchanfrage
                                         abgeschickt hat, klickt er in der Ergebnis-Spalte auf den Reiter <span class='tooltip-image'>„Förderungen“<span class='tooltip-content'>
                                        <img src='anl_2_2.jpg' alt='Screenshot des Reiters „Förderungen“'></span></span> und sieht die Anzahl der Kieler
                                         Publikationen je Fördermittelgeber. Es findet hier also all jene Publikationen, bei denen eine CSC-Förderung angegeben
                                         und maschinell erkannt wurde. Er markiert den CSC-Eintrag und wählt in der rechten Spalte <span class='tooltip-image'>„Publikationen anzeigen“<span class='tooltip-content'>
                                        <img src='anl_2_3.jpg' alt='Screenshot der Schaltfläche „Publikationen anzeigen“'></span></span>,
                                         um seine Informationen mittels des externen Datensatzes des <i>Atlas</i> zu ergänzen.</p>
                                         <p><b>Forschungsschwerpunkte</b></p>
                                         <p>In Bremen möchte eine Wissenschaftlerin ein neues interdisziplinäres Verbundprojekt in Norddeutschland aufbauen und sucht
                                         Mitstreiter:innen, die das Forschungsgebiet „Klimawandel“ aus sozio-kultureller Perspektive erforschen. Für eine globale Perspektive
                                         fehlt dem Projektvorhaben bislang noch Expertise zur Rezeption des Klimawandels in ost- und südostasiatischen Regionen. Aufgrund des
                                         sozio-kulturellen Fokus begrenzt sie in den Filter-Einstellungen die <span class='tooltip-image'>Fachrichtungen<span class='tooltip-content'>
                                        <img src='anl_3_1.jpg' alt='Screenshot des Fachrichtungs-Filters'></span></span> auf die zwei Optionen „Geisteswissenschaften“ und „Wirtschaft & Soziales“.
                                         Bei den Suchbegriffen wählt sie <span class='tooltip-image'>thematisch passende Begriffe<span class='tooltip-content'>
                                        <img src='anl_3_2.jpg' alt='Screenshot der Suchbegriff-Eingabe mit Verknüpfungsoption'></span></span>, die sie mit Leerzeichen trennt. Da sie die Option
                                         <span class='tooltip-image'>„Mindestens ein Begriff“<span class='tooltip-content'>
                                        <img src='anl_3_2.jpg' alt='Screenshot der Suchbegriff-Eingabe mit Verknüpfungsoption'></span></span> gewählt hat, werden alle Publikationen gefunden, die zumindest einen der Begriffe in der Überschrift oder als
                                         Schlüsselwort aufzählen. In den Suchergebnissen klickt die Forscherin auf den Reiter <span class='tooltip-image'>„Netzwerk“<span class='tooltip-content'>
                                        <img src='anl_3_3.jpg' alt='Screenshot des Reiters „Netzwerk“'></span></span>, um sich einen visuellen
                                         Ersteinruck davon zu machen, an welchen Institutionen passende Forschungsschwerpunkte zu finden sind. Sie wählt im Netzwerk-Graph
                                         einzelne Publikationen aus und klickt auf den <span class='tooltip-image'>DOI-Link<span class='tooltip-content'>
                                        <img src='anl_3_4.jpg' alt='Screenshot des DOI-Links im Netzwerk-Graphen'></span></span>, um automatisch mittels der elektronischen Lizenzen ihrer Universitätsbibliothek
                                         ausgewählte Artikel herunterzuladen und einen Eindruck von der Forschung zu bekommen.
                                         </p>")))))),
                        column(6, div(class = "dynamic-height", style = "padding-left: 6px;",
                                      wellPanel(tags$small(HTML(paste0(
                                        "<p><h4>Fragen & Antworten</h4></p>
                                         <p><i>Warum ist meine Publikation nicht in der Datenbank gelistet?</i></p>
                                         <p>Die Datenbank greift auf öffentliche Datenquellen zurück. Das Forschungsprojekt soll zeigen, welche Daten
                                         aus öffentlichen Zugängen akquiriert werden können. Es gibt zwar einen Anspruch auf Repräsentanz, aber keinen Anspruch
                                         auf Vollständigkeit. Ein nachträgliches Einpflegen selektiver Daten ist weder möglich, noch erwünscht, da es
                                         dem Forschungsdesign zuwiderlaufen würde.</p>
                                         <p><i>Warum sind von einer Person nur einzelne Publikationen verzeichnet?</i></p>
                                         <p>Mit ihrem vollständigen Publikationsverzeichnis wurden nur jene Forschenden erfasst, die zum Zeitpunkt der Datenerhebung an einem der norddeutschen Standorte tätig zu sein schienen. Von Forschenden, die mittlerweile andernorts wirken, sind hingegen nur jene Beiträge enthalten, die während ihrer Zeit an einem der Standorte erschienen und entsprechend mit diesem verknüpft sind. Wer also nach der Erhebung den Standort gewechselt hat, erscheint unter Umständen nur mit wenigen oder einzelnen Publikationen.</p>
                                         <p><i>Ich habe einen Fehler in einem Eintrag bemerkt. Wem melde ich das?</i></p>
                                         <p>Die Einträge spiegeln öffentliche Datenbanken wider. Es entspricht nicht dem Forschungsdesign, einzelne
                                         Einträge nachträglich zu verändern.</p>
                                         <p><i>Wieso taucht ein Forschender mehr als einmal auf?</i></p>
                                         <p>Der/die Forschende hat sich mehr als einmal für eine ORCiD (Forscher:innen-Identifikationsnummer) registriert. Wir gehen davon aus,
                                         dass sich jede/r Forschende nur einmal in ihrer/seiner Karriere für eine ORCiD registriert und es sich hier
                                         im Zweifelsfall im Namensvettern handelt.</p>
                                         <p><i>Wird die Datenbank permanent aktualisiert?</i></p>
                                         <p>Nein, es handelt sich um ein statisches Datenset, das den Stand von öffentlich zugänglichen Datenbanken im
                                         Juni 2026 repräsentiert. Unter Umständen wird es in der Zukunft neue Auflagen des <i>Atlas</i> geben,
                                         die dann eine aktualisierte Zeitspanne behandeln.</p>"
                                      )))),
                                      div(style = "height: 12px;"),
                                      wellPanel(tags$small(HTML(paste0(
                                        "<p><h4>Rohdaten</h4></p>
               <p>Das komplette Datenset kann über das Online-Repositorium
               <a href=\"https://doi.org/10.5281/zenodo.17745729\">Zenodo</a>
               heruntergeladen werden.</p>
               <p><h4>Programmcode</h4></p>
               <p>Die Programmcodes für die Datenschürfung sowie für das Online-Interface sind über das Versionsverwaltungs-Portal
               <a href=\"https://github.com/tp451/chikon_atlas\">GitHub</a> einsehbar.</p>"
                                      ))))
                                      )),
                      )),
             tabPanel("Dokumentation",
                      
                      fluidRow(
                        column(12, div(class = "dynamic-height scrollable-content",
                                       wellPanel(tags$small(HTML(paste0(
                                         "<p><h4>Technische Dokumentation</h4>Text: <a href=\"https://pelzer.blog\" target=\"_blank\">Thorben Pelzer</a><br>
                                         September 2025, aktualisiert Juni 2026</p>
                                         <p>Die Datenerhebung fasst zehn universitäre Standorte ins Auge, die für den Raum Norddeutschland fächerübergreifend von besonderer Relevanz sind. Die Liste dieser Standorte ergibt sich aus der Kombination aller deutscher Universitäten, die Mitglied des Verbunds Norddeutscher Universitäten (<a href=\"https://www.uni-nordverbund.de/\">VNU</a>) sind und/oder als Partner des Projekts „Chinakompetenz im Norden“ (<a href=\"https://www.uni-kiel.de/de/international/chikon\">ChiKoN</a>), in dessen Kontext diese Erhebung entstanden ist, auftreten. Die Liste der Standorte spiegelt also nicht die vollständige Hochschullandschaft Norddeutschlands wieder, jedoch wäre dies auch kaum realisierbar, da sich das Netz aus teils stark spezialisierten kleineren Fachhochschulen, Privatuniversitäten und Kunstakademien selbst für den begrenzten norddeutschen Raum als sehr unübersichtlich erweist.</p>
                                         <div style=\"float: right; margin-left: 15px; margin-bottom: 15px;\">
                                         <span class='tooltip-image tooltip-right'>
                                         <img src=\"standorte.jpeg\" height=\"300px\" alt=\"Karte der zehn untersuchten Universitätsstandorte in Norddeutschland\">
                                         <span class='tooltip-content'><img src=\"standorte.jpeg\" alt=\"Karte der zehn untersuchten Universitätsstandorte in Norddeutschland\"></span>
                                         </span>
                                         </div>
                                         <p>Für die Datenerhebung wurde die Schnittstelle („Application programming interface(s)“, kurz API) der <a href=\"https://orcid.org/\" target=\"_blank\">ORCiD</a>-Organisation, die die gleichnamige „Open researcher and contributor ID“ (ORCiD) vergibt und über dessen Webseite Forschende ihre Forschungsaktivitäten verwalten können, sowie die <a href=\"https://doi.org/10.48550/arXiv.2205.01833\" target=\"_blank\">OpenAlex</a>-Datenbank der <a href=\"https://ourresearch.org/\" target=\"_blank\">OurResearch</a>-Organisation ausgelesen. Dies erlaubte es im ersten Schritt, all jene Forschenden zu identifizieren, die entweder selbst einen der oben genannten Standorte als ihren Arbeitsplatz hinterlegt haben, oder die über bibliografische Informationen aus Publikationen dort verortbar sind. Selbstredend ist die Profilsammlung nicht vollständig, da nicht alle Forschenden ein ORCiD-Profil besitzen oder es ausreichend pflegen.
                                         <p>Die kombinierte Erhebung über OpenAlex und ORCiD eignet sich insofern, als dass (1.) es sich um weitverbreitete Plattformen für Forschungsprofile mit auslesbarer API handelt, (2.) die Kombination aus externen und eigenen Publikationsinformationen Lücken der beiden Datenbanken gegenseitig schließt, und (3.) der Bezug auf eine universelle Plattform institutionsunabhängige Vergleichsmöglichkeiten schafft, die bei der Auslesung von universitätseigenen Forschungsinformationssystemen nicht möglich wäre, da die Akzeptanz dieser von Institution zu Institution schwankt.</p>
                                         <p>Im zweiten Schritt wurden dann alle Publikationen und alle Förderungen, die entweder über bibliografische Informationen öffentlich sind oder zu denen die Forschenden selbst Informationen auf ihrem Profil hinterlegt haben bzw., falls diese Berechtigung durch die Forschenden erteilt wurde, Drittanbieter wie <a href=\"https://search.crossref.org/\" target=\"_blank\">Crossref</a> oder <a href=\"https://www.scopus.com/\" target=\"_blank\">Scopus</a> Informationen auf dem ORCiD-Profil hinterlegt haben, geschürft. Die so gewonnenen Metadaten können je nach Eintrag leicht variieren, beinhalten aber in der Regel Informationen wie den Publikations- bzw. Förderzeitpunkt, das Publikationsformat (Journalbeitrag, Monografie, etc.), Name des Journals bzw. Verlags und ähnliche zitationsrelevante Datenpunkte.</p>
                                         <p>Im Sinne der übergeordneten Fragestellung interessiert sich die Datenanalyse für all jene katalogisierten Publikationen und Förderprojekte, die einen Ostasien-Bezug aufweisen. Dabei sollen, wie in der Einleitung herausgestellt, bewusst disziplinenübergreifend alle Beiträge mit regionalem bzw. thematischen Ostasien-Bezug gesammelt werden, ganz gleich, ob sich diese bewusst und gezielt mit den Ländern – etwa ihrer Kultur oder Gesellschaft – als Schwerpunkt ihrer Analyse auseinandersetzen oder sich der eigentliche Fokus der Arbeit lediglich geografisch in diesen Ländern befindet. Schließlich interessiert sich die vorliegende Arbeit für jegliche Formen der Landesexpertise an deutschen Hochschulen und damit auch jener China- und Ostasienkompetenz, die sich gar nicht aktiv als solche wahrnimmt bzw. sich selbst nicht als solche identifiziert.</p>
                                         <p>Um also aus den 320.000 (ORCiD) bzw. 460.000 (OpenAlex) norddeutschen Publikationen jene Einträge herauszufiltern, die sich mit Ostasien auseinandersetzen, müssen die Einträge inhaltlich geprüft werden. Eine Prüfung der Volltexte ist nicht durchführbar, da die Volltexte bei den Verlange nicht einheitlich hinterlegt sind und ohnehin oftmals hinter einer Bezahlschranke liegen. Stattdessen sollen als Mittelweg die Zusammenfassungen (Abstracts) der Einträge geschürft werden, um diese danach inhaltlich zu analysieren.</p>
                                         <p>Während die Mehrheit der OpenAlex-Einträge ein Abstract beinhaltet, beinhalten nur die wenigsten ORCiD-Einträge ein Abstract, welches in der Regel händisch durch den Forschenden hinterlegt werden müsste. Über die Organisation Crossref dagegen, die verlagsübergreifend Publikations-Metadaten verwaltet und bereitstellt, sind in vielen, aber nicht allen, Fällen Abstracts mit den <a href=\"https://www.doi.org/\" target=\"_blank\">DOIs</a> („Digital object identifiers“), also mit den eindeutigen Identifikationsnummern der Publikationen, verknüpft. Da Crossref eine frei zugängliche API anbietet, können die Publikations-Metadaten von ORCiD so um zusätzliche Abstracts erweitert werden – Gesetz dem Fall, dass in den ORCiD-Publikationsdaten eine DOI korrekt hinterlegt ist. Die Verbindung des ORCiD-Katalogs mit den Crossref-Metadaten ermöglicht es daher, Details über den Inhalt einer Publikation zu erlangen, die über die Überschrift und, wenn vorhanden, einzelne Keywords hinausgehen.</p>
                                         <p>Mit den nun gewonnen Daten – Überschrift der Publikation und, wo vorhanden, Keywords und Abstract – wird auf zwei einander ergänzende Weisen ermittelt, welche geografischen Räume in den Texten besprochen werden. Zum einen durchsuchen sogenannte reguläre Ausdrücke („Regular expressions“, kurz Regex) die Metadaten nach einer vorab festgelegten Liste von Schlüsselbegriffen wie „China“ oder „Beijing“. Zum anderen kommt maschinelle Sprachverarbeitung („Natural language processing“, kurz NLP) zum Einsatz, genauer die Eigennamenerkennung („Named-entity recognition“, kurz NER); ihr Vorteil ist, dass keine Liste an relevanten Ortsnamen vordefiniert werden muss, sondern eine undefinierte Anzahl von Orten – klein wie groß, Städte wie Flüsse, bekannt oder unerwartet – gefunden werden kann. Die Eigennamenerkennung nutzt hierbei grammatikalische Regeln, beispielsweise Deklinationen und Pronomina, um die Funktion und den Inhalt einzelner Worte eines Satzes zu bestimmen. Die Treffer beider Verfahren werden anschließend zusammengeführt.</p>
                                         <p>Im letzten Schritt können dann alle mittels der Eigennamenerkennung erkannten Orte („Locations“, kurz LOC) und geopolitische Einheiten („Geopolitical entities“, kurz GPE) über die „<a href=\"https://nominatim.org/release-docs/develop/api/Overview/\" target=\"_blank\">Nominatim</a>“-API des Geolokalisationsdienstes OpenStreetMap (<a href=\"https://www.openstreetmap.org/\" target=\"_blank\">OSM</a>) geografisch verortet werden. Das heißt, die die OSM-Suchfunktion ermittelt die wahrscheinlichsten geografischen Koordinaten für alle auftauchenden Raumbezeichnungen. Diese können dann auf einer Karte verzeichnet werden und eine Raumanalyse filtert die Ergebnisse so, dass am Ende nur noch jene Publikationen übrigbleiben, die von Orten sprechen, welche sich in der Region Asien-Pazifik befinden.</p>
                                         <p>Nach der ausgiebigen Datenschürfung und -verarbeitung bleibt also ein Korpus aller Forschender in Norddeutschland übrig, die nachweislich zu Ostasien gearbeitet haben bzw. deren Publikationen zumindest prominent in Überschrift oder Abstract Orte in der Region benennen.</p>
                                         <p>Die Einteilung der Forschenden in einen von acht Fachbereichen erfolgte zunächst manuell auf Basis der letztgemeldeten Einrichtung der Forschenden. Wo sich auf diesem Weg kein Fachbereich ergab, wurde ergänzend ein lokal betriebenes KI-Sprachmodell (das quelloffene Modell „Qwen“) herangezogen, das aus den Publikationstiteln einer Person einen Fachbereich vorschlägt; die manuelle Zuordnung hat dabei stets Vorrang. Die Taxonomie orientiert sich an den <a href=\"https://www.uni-kiel.de/de/universitaet/einrichtungen-fakultaeten/fakultaeten-gemeinsame-einrichtungen\">Fakultäten der CAU</a>, muss mit diesen aber nicht kongruent sein.</p>
                                         <p>Über die Publikationen hinaus werden zudem die Literaturverzeichnisse (Referenzen) der erfassten Beiträge ausgewertet, soweit sie über OpenAlex vorliegen. Sie bilden die Grundlage für die Zitationsnetzwerke des Atlas – etwa die Frage, welche Autor:innen oder Werke häufig gemeinsam zitiert werden.</p>
                                         <p>Mit dem gefilterten Korpus können die eigentlichen Analysen angestellt werden.</p>"
                                       )))),
                                       
                        )),
                      )
                      
             ),
             
             tabPanel("Datenschutz",
                      
                      fluidRow(
                        column(12, div(class = "dynamic-height scrollable-content",
                                       wellPanel(tags$small(HTML(paste0(
                                         "
                                          <h2>Datenschutzerklärung nach der DSGVO</h2>
    
                                          <p>Dem Chinazentrum CAU Kiel ist Datenschutz ein wichtiges Anliegen. Wir legen deshalb auch bei der mit unserer Aufgabenerfüllung verbundenen Verarbeitung personenbezogener Daten Wert auf eine datensparsame Datenverarbeitung.<br/>
                                          Diese Datenschutzerklärung bezieht sich auch auf die Verarbeitung personenbezogener Daten und Informationen im Sinne des § 25 TDDDG im Rahmen dieses Internetauftritts, einschließlich der dort angebotenen Dienste.</p>
                                          <h3>Name und Anschrift des Verantwortlichen</h3>
                                          <p>Der Verantwortliche im Sinne der Datenschutz-Grundverordnung und anderer nationaler Datenschutzgesetze der Mitgliedsstaaten sowie sonstiger datenschutzrechtlicher Bestimmungen ist die:</p>
                                              
                                          <p><b>Christian-Albrechts-Universität zu Kiel</b></p>
                                          <p>Christian-Albrechts-Platz 4</br>
                                          24118 Kiel, Germany</br>
                                          Telefon: +49 (0)431 880-00</br>
                                          E-Mail: <a href=\"mailto:mail@uni-kiel.de\">mail@uni-kiel.de</a></p>
                                              
                                          <p><u>Interner Ansprechpartner:</u></p>
                                          <p>Chinazentrum CAU Kiel</br>
                                          <a href=\"https://www.chinazentrum.uni-kiel.de/de/team/dr.-angelika-messner\" target=\"_blank\">Prof. Dr. Angelika Messner</a></br>
                                          Leibnizstr. 10, 3. Etage</br>
                                          24118 Kiel</br>
                                          Telefon: +49 431 880-4571</br>
                                          E-Mail: <a href=\"mailto:office@chinazentrum.uni-kiel.de\">office@chinazentrum.uni-kiel.de</a></p>
                                          <h3>Name und Anschrift des Datenschutzbeauftragten</h3>
                                          <p>Bitte wenden Sie sich in allen Fragen rund um das Thema Datenschutz und Datensicherheit direkt an unseren Datenschutzbeauftragten: </p>
                                          <p>actago GmbH</br>
                                          Weidenstraße 66</br>
                                          94405 Landau a. d. Isar</br>
                                          Telefon: +49 (0)9951 99990-500</br>
                                          E-Mail: <a href=\"mailto:datenschutz@uv.uni-kiel.de\">datenschutz@uv.uni-kiel.de</a></br>
                                          Internet: <a href=\"https://www.actago.de\" target=\"_blank\">www.actago.de</a></p>
                                              
                                          <h3>Allgemeine Informationen</h3>
                                          <h4>Zwecke und Rechtsgrundlagen für die Verarbeitung personenbezogener Daten</h4>
                                          <p>Zweck der Verarbeitung ist die Erfüllung der uns vom Gesetzgeber zugewiesenen öffentlichen Aufgaben.<br/>
                                          Die Rechtsgrundlage für die Verarbeitung Ihrer Daten ergibt sich, soweit nichts anderes angegeben ist, aus § 3 Abs. 1 des Schleswig-Holsteinischen Gesetzes zum Schutz personenbezogener Daten (LDSG (SH)) in Verbindung mit Art. 6 Abs. 1 lit. e der Datenschutzgrundverordnung (DSGVO). Demnach ist es uns erlaubt, die zur Erfüllung einer uns obliegenden Aufgabe erforderlichen Daten zu verarbeiten.
                                          Soweit Sie in eine Verarbeitung eingewilligt haben, stützt sich die Datenverarbeitung auf Art. 6 Abs. 1 lit. a DSGVO.</p>
                                          <h4>Empfänger von personenbezogenen Daten</h4>
                                          <p>Der technische Betrieb unserer Datenverarbeitungssysteme erfolgt durch Posit Software, PBC (250 Northern Ave Suite 420 Boston, MA 02210, Tel. 844-448-1212).</p>
                                              
                                          <p>Gegebenenfalls werden Ihre Daten an die zuständigen Aufsichts- und Rechnungsprüfungsbehörden zur Wahrnehmung der jeweiligen Kontrollrechte übermittelt.</p>
                                          <h4>Dauer der Speicherung der personenbezogenen Daten</h4>
                                          <p>Ihre Daten werden nur so lange gespeichert, wie dies unter Beachtung gesetzlicher Aufbewahrungsfristen zur Aufgabenerfüllung erforderlich ist.</p>
                                          <h4>Ihre Rechte</h4>
                                          <p>Soweit wir von Ihnen personenbezogene Daten verarbeiten, stehen Ihnen als Betroffener nachfolgende Rechte zu:</p>
                                              
                                          <p><ul><li>Sie können Auskunft dazu verlangen, ob wir personenbezogene Daten von Ihnen verarbeiten. Ist dies der Fall, so haben Sie ein Recht auf Auskunft über diese Daten sowie auf weitere mit der Verarbeitung zusammenhängende Informationen (Art. 15 DSGVO). Bitte beachten Sie, dass dieses Auskunftsrecht in bestimmten Fällen eingeschränkt oder ausgeschlossen sein kann (vgl. insbesondere § 9 LDSG (SH)).</li>
                                          <li>Sollten unrichtige personenbezogene Daten verarbeitet werden, steht Ihnen ein Recht auf Berichtigung zu (Art. 16 DSGVO).</li>
                                          <li>Liegen die gesetzlichen Voraussetzungen vor, so können Sie die Löschung oder Einschränkung der Verarbeitung verlangen (Art. 17 und 18 DSGVO).</li>
                                          Das Recht auf Löschung nach Art. 17 Abs. 1 und 2 DSGVO besteht jedoch dann nicht, wenn die Verarbeitung personenbezogener Daten zur Wahrnehmung einer Aufgabe im öffentlichen Interesse oder in Ausübung öffentlicher Gewalt erforderlich ist (Art. 17 Abs. 3 lit. b DSGVO)</li>
                                          <li>Falls Sie in die Verarbeitung eingewilligt haben und die Verarbeitung auf dieser Einwilligung beruht, können Sie die Einwilligung jederzeit für die Zukunft widerrufen.</li>
                                          Die Rechtmäßigkeit der aufgrund der Einwilligung bis zum Widerruf erfolgten Datenverarbeitung wird durch diesen nicht berührt.</li>
                                          <li>Sie haben das Recht, aus Gründen, die sich aus Ihrer besonderen Situation ergeben, jederzeit gegen die Verarbeitung Ihrer Daten Widerspruch einzulegen (Art. 21 DSGVO). Sofern die gesetzlichen Voraussetzungen vorliegen, verarbeiten wir in der Folge Ihre personenbezogenen Daten nicht mehr.</li></ul>
                                              
                                          <p>Weitere Einschränkungen, Modifikationen und gegebenenfalls Ausschlüsse der vorgenannten Rechte können sich aus der Datenschutz-Grundverordnung oder nationalen Rechtsvorschriften ergeben.
                                          Ausführlichere Informationen zu diesen Rechten erhalten Sie auch bei unserem Datenschutzbeauftragten.</p>
                                          <h3>Beschwerderecht bei der Aufsichtsbehörde</h3>
                                          <p>Weiterhin besteht ein Beschwerderecht beim ULD – Unabhängiges Landeszentrum für Datenschutz Schleswig-Holstein. Diesen können Sie unter folgenden Kontaktdaten erreichen:</p>
                                              
                                          <p>Postanschrift: Postfach 71 16, 24103 Kiel</br>
                                          Adresse: Holstenstraße 98, 24103 Kiel</br>
                                          Telefon:  0431 988-1200</br>
                                          Telefax:  0431 988-1223</br>
                                          Online-Meldung: <a href=\"https://www.datenschutzzentrum.de/meldungen/\" target=\"_blank\">https://www.datenschutzzentrum.de/meldungen/</a></p>
                                          <h3>Informationen zum Internetauftritt</h3>
                                          <h4>Protokollierung</h4>
                                          <p>Wenn Sie diese oder andere Internetseiten aufrufen, übermitteln Sie über Ihren Internetbrowser Daten an unseren Webserver. Die folgenden Daten werden während einer laufenden Verbindung zur Kommunikation zwischen Ihrem Internetbrowser und unserem Webserver aufgezeichnet:</p>
                                              
                                          <p><ul>
                                          <li>Datum und Uhrzeit der Anforderung</li>
                                          <li>Name der angeforderten Datei</li>
                                          <li>Seite, von der aus die Datei angefordert wurde</li>
                                          <li>Zugriffsstatus (Datei übertragen, Datei nicht gefunden, etc.)</li>
                                          <li>verwendete Webbrowser und verwendetes Betriebssystem</li>
                                          <li>vollständige IP-Adresse des anfordernden Rechners</li>
                                          <li>übertragene Datenmenge.</li>
                                          </ul></p>
                                              
                                          <p>Aus Gründen der technischen Sicherheit, insbesondere zur Abwehr von Angriffsversuchen auf unseren Webserver, werden diese Daten von uns gespeichert. Nach spätestens sieben Tagen werden die Daten durch Verkürzung der IP-Adresse auf Domain-Ebene anonymisiert, so dass es nicht mehr möglich ist, einen Bezug auf einzelne Nutzer herzustellen.</p>
                                          <h4>Sichere Datenübertragung</h4>
                                          <p>Mit Aufruf dieses Informationsangebots bieten wir eine mit HTTPS und Perfect Forward Secrecy verschlüsselte Verbindung, welche mindestens mit dem Verschlüsselungsprotokoll TLS 1.2 gesichert ist an, sodass Ihre Daten bei der Datenübertragung vor einer Kenntnisnahme durch Dritte geschützt sind. Wir empfehlen Ihnen, Ihren Internetbrowser zur Nutzung dieser Möglichkeit aktuell zu halten.</p>
                                          <h3>Cookies</h3>
                                          <p>Zur korrekten technischen und funktionellen Bereitstellung dieses Informationsangebots verwenden wir Cookies. Cookies sind kleine Textdateien, die auf dem von Ihnen verwendeten Gerät gespeichert werden.</p>
                                          Rechtsgrundlage für die Speicherung von Informationen sowie die Verarbeitung personenbezogener Daten mittels technisch notwendiger Cookies ist § 25 Abs. 2 TDDDG sowie Art. 6 Abs. 1 lit. e DSGVO i.V.m. § 3 LDSG (SH).</p>
                                          <p>Technisch notwendige Cookies sind nur für die jeweils aktuelle Sitzung gültig und werden automatisch gelöscht, sobald Sie Ihren Browser schließen.</p>
                                          <p>Die Verwendung funktionaler Cookies ist freiwillig. Wenn diese Cookies blockiert werden, ist die Bereitstellung bestimmter Funktionen ggf. nicht in vollem Umfang möglich.</p>
                                          <p>Rechtsgrundlage für die Verwendung technisch nicht notwendiger Cookies ist eine Einwilligung des Nutzers gemäß § 25 Abs. 1 TDDDG i.V.m. Art. 6 Abs. 1 lit. a DSGVO.</p>
                                          <p>Beim Zugriff auf dieses Internetangebot werden von uns Cookies (kleine Dateien) auf Ihrem Gerät gespeichert. Diese haben eine Gültigkeit von:</p>
                                              
                                          <ul>
                                          <li>Name: session<br/>Zweck: technisch notwendig (Aufrechterhaltung der Shiny-Sitzung)<br/>Speicherdauer: Ende der Session</li>
                                          <li>Name: chikon_skip_intro<br/>Zweck: funktionales Cookie zur Speicherung der Auswahl, die Begrüßungs-Information nicht erneut anzuzeigen. Dieses Cookie wird ausschließlich dann gesetzt, wenn die entsprechende Auswahl im Begrüßungsfenster aktiv bestätigt wurde.<br/>Speicherdauer: 365 Tage</li>
                                          </ul>
                                          <h3>Kontaktaufnahme per E-Mail</h3>
                                          <h4>Beschreibung und Umfang der Datenverarbeitung</h4>
                                          <p>Eine Kontaktaufnahme ist über die bereitgestellte E-Mail-Adresse möglich. Dabei werden Ihre mit der E-Mail übermittelten personenbezogenen Daten gespeichert. Es erfolgt in diesem Zusammenhang keine Weitergabe der Daten an Dritte.</p>
                                          <p>Die Daten werden ausschließlich für die Verarbeitung der Konversation verwendet.</p>
                                          <h4>Rechtsgrundlage für die Datenverarbeitung</h4>
                                          <p>Rechtsgrundlage für die Verarbeitung Ihrer personenbezogenen Daten, die im Zuge einer Übersendung einer E-Mail übermittelt werden, ist Art. 6 Abs. 1 lit. e DSGVO i.V.m. § 3 LSDG (SH). Zielt die Kontaktaufnahme per E-Mail auf den Abschluss eines Vertrages ab, so ist zusätzliche Rechtsgrundlage für die Verarbeitung Art. 6 Abs. 1 lit. b DSGVO.</p>
                                          <h4>Zweck der Datenverarbeitung</h4>
                                          <p>Die sonstigen während des Absendevorgangs verarbeiteten personenbezogenen Daten dienen dazu, einen Missbrauch des Kontaktformulars zu verhindern und die Sicherheit unserer informationstechnischen Systeme sicherzustellen.</p>
                                          <h4>Dauer der Speicherung</h4>
                                          <p>Ihre personenbezogenen Daten werden gelöscht, sobald sie für die Erreichung des Zweckes ihrer Erhebung nicht mehr erforderlich sind. Für die personenbezogenen Daten, die per E-Mail übersandt wurden, ist dies dann der Fall, wenn die jeweilige Konversation mit Ihnen beendet ist. Beendet ist die Konversation dann, wenn sich aus den Umständen entnehmen lässt, dass der betroffene Sachverhalt abschließend geklärt ist.</p>
                                          <p>Die während des Absendevorgangs zusätzlich erhobenen personenbezogenen Daten werden spätestens nach einer Frist von sieben Tagen gelöscht.</p>
                                          <h4>Widerspruchs- und Beseitigungsmöglichkeit</h4>
                                          <p>Sie haben jederzeit die Möglichkeit, der Verarbeitung Ihrer personenbezogenen Daten im Rahmen der Kontaktaufnahme per E-Mail für die Zukunft zu widersprechen. In einem solchen Fall kann die Konversation zwischen Ihnen und uns nicht fortgeführt werden. Alle personenbezogenen Daten, die im Zuge der Kontaktaufnahme gespeichert wurden, werden in diesem Fall gelöscht.</p>
                                          <h3>Elektronische Post (E-Mail)</h3>
                                          <p>Informationen, die Sie unverschlüsselt per Elektronische Post (E-Mail) an uns senden, können möglicherweise auf dem Übertragungsweg von Dritten gelesen werden. Wir können in der Regel auch Ihre Identität nicht überprüfen und wissen nicht, wer sich hinter einer E-Mail-Adresse verbirgt. Eine rechtssichere Kommunikation durch einfache E-Mail ist daher nicht gewährleistet. Wir setzen - wie viele E-Mail-Anbieter - Filter gegen unerwünschte Werbung („SPAM-Filter“) ein, die in seltenen Fällen auch normale E-Mails fälschlicherweise automatisch als unerwünschte Werbung einordnen und löschen. E-Mails, die schädigende Programme („Viren“) enthalten, werden von uns in jedem Fall automatisch gelöscht.</p>
                                          <p>Wenn Sie Bedenken bei der Übermittlung von personenbezogen Daten oder anderen sensiblen Daten haben, stimmen Sie vor der Übermittlung eine geeignete Verschlüsselung mit dem Empfänger (unseren Mitarbeitern) ab, oder nutzen Briefpost.</p>
                                          <h3>Aktive Komponenten</h3>
                                          <p>Im Informationsangebot werden aktive Komponenten wie Javascript, Java-Applets oder Active-X-Controls verwendet. Diese Funktion kann durch die Einstellung Ihres Internetbrowsers von Ihnen abgeschaltet werden.</p>
                                          <h3>Hinweis zur Datenschutzerklärung</h3>
                                          <p>Soweit nichts anderes geregelt ist, unterliegt die Nutzung sämtlicher Informationen, die wir über Sie haben, dieser Datenschutzerklärung.</p>
                                          <p>Der Verantwortliche behält es sich vor, diese Datenschutzerklärung den erforderlichen Sicherheitsmaßnahmen entsprechend der technologischen Entwicklung fortlaufend anzupassen, und wird etwaige Änderungen an dieser Stelle bekannt geben.</p>
                                          <p>Stand: November 25</p>
                                              
                                          <h3>Weitere Hinweise</h3>
                                          <h4>Technische und organisatorische Maßnahmen</h4>
                                          <p>Der Verantwortliche hat technische und organisatorische Maßnahmen ergriffen, um Ihre Daten vor Verlust, Zerstörung oder unberechtigtem Zugriff zu schützen.</p>
                                          <p>Zudem werden sowohl die Beschäftigten des Verantwortlichen als auch etwaige Dienstleistungs-unternehmen zur Verschwiegenheit und zur Einhaltung der datenschutzrechtlichen Bestimmungen verpflichtet.</p>
                                          <h4>SSL- bzw. TLS-Verschlüsselung</h4>
                                          <p>Aus Sicherheitsgründen und zum Schutz der Übertragung vertraulicher Inhalte, die Sie an uns als Seitenbetreiber senden, nutzt unsere Website eine SSL-bzw. TLS-Verschlüsselung. Damit sind Daten, die Sie über diese Website übermitteln, für Dritte nicht mitlesbar. Sie erkennen eine verschlüsselte Verbindung an der „https://“ Adresszeile Ihres Browsers und am Schloss-Symbol in der Browserzeile.</p>
                                              
                                         "
                                       )))),
                                       
                        )),
                      )
                      
             ),
             
             tabPanel("Impressum",
                      
                      fluidRow(
                        column(12, div(class = "dynamic-height scrollable-content",
                                       wellPanel(tags$small(HTML(paste0(
                                         "<p><b>Autor:innenschaft</b></p><p>
                                  Konzeption, Programmierung, Realisierung: <a href=\"https://pelzer.blog\" target=\"_blank\">Jun.-Prof. Dr. Thorben Pelzer</a> (<a href=\"https://huma.hkust.edu.hk/people/thorben-pelzer-0\">HKUST</a>)</br>
                                  Ideen und Förderung: <a href=\"https://www.chinazentrum.uni-kiel.de/de/\" target=\"_blank\">Chinazentrum CAU Kiel</a></br>
                                  Zitationsvorschlag: Pelzer, Thorben. „Atlas der Ostasien-Forschung in Norddeutschland.“ Kiel: CAU Chinazentrum, 2025. <a href=\"citation.bib\"  download=\"citation.bib\">📚</a></p>
                                  
                                  <p><b>Verwendete Daten und Bibliotheken</b></p><p>Datengrundlage: Crossref, OpenAlex, ORCiD (Stand: Juni 2026)<br>
                                  Realisiert mit u.a. <a href=\"https://doi.org/10.32614/RJ-2023-089\" target=\"_blank\">openalexR</a>, <a href=\"https://doi.org/10.32614/CRAN.package.rorcid\" target=\"_blank\">rOrcid</a>, <a href=\"https://doi.org/10.32614/CRAN.package.rcrossref\" target=\"_blank\">rCrossref</a>,
                                  <a href=\"https://doi.org/10.32614/CRAN.package.spacyr\" target=\"_blank\">SpacyR</a>, <a href=\"https://doi.org/10.21105/joss.03544\" target=\"_blank\">TidyGeocoder</a>, <a href=\"https://shiny.posit.co/\" target=\"_blank\">Shiny</a></p>
                                 
                                  <p><b>Verantwortlich (institutionell)</b></p>
                                  <p>Christian-Albrechts-Universität zu Kiel</br>
                                  Christian-Albrechts-Platz 4</br>
                                  24118 Kiel, Germany</br>
                                  Telefon: +49 (0)431 880-00</br>
                                  E-Mail: <a href\"mailto:mail@uni-kiel.de\">mail@uni-kiel.de</a></br>
                                  USt-IdNr.: DE 811317279</br>
                                  V.i.S.d.P. & gesetzliche Vertretung: Das Präsidium</br>
                                  Zuständige Aufsichtsbehörde: MBWFK, Brunswiker Straße 16-22, 24105 Kiel</p>
                                  <p><b>Interner Ansprechpartner</b></p>
                                      
                                  <p><a href=\"https://www.chinazentrum.uni-kiel.de/de/\"  target=\"_blank\">Chinazentrum CAU Kiel</a></br>
                                  <a href=\"https://www.chinazentrum.uni-kiel.de/de/team/dr.-angelika-messner\" target=\"_blank\">Prof. Dr. Angelika Messner</a></br>
                                  Leibnizstr. 10, 3. Etage</br>
                                  24118 Kiel</br>
                                  Tel.: +49 (0)431 880-4571</br>
                                  <a href=\"mailto:office@chinazentrum.uni-kiel.de\">office@chinazentrum.uni-kiel.de</a></p>
                                  <p><b>Drittmittelprojekt</b></p>
                              
                                  <p>ChiKoN - China-Kompetenz im Norden</br>
                                  Fördermaßnahme <a href=\"https://www.internationales-buero.de/de/regio_china_ausbau_der_china_kompetenz_in_der_wissenschaft.php\" target=\"_blank\">Regio-China</a> des BMFTR (2023–2026)</br>
                                  Projektleitung:	<a href=\"https://www.chinazentrum.uni-kiel.de/de/team/dr.-angelika-messner\" target=\"_blank\">Prof. Dr. Angelika Messner</a></br>
                                  Projektkoordinatorin:	Jana Brokate</br>
                                  Studentische Hilfskraft: Tim Feldmann</br></p>
    
                                 <p><a href=\"https://www.uni-kiel.de/de/international/chikon\" target=\"_blank\"><img height=90px src=\"logo_chikon.png\" alt=\"ChiKoN-Logo\"></a>
                                 <img height=90px src=\"logo_bmftr.png\" alt=\"BMFTR-Logo\"></p>
                                 "
                                       )))),
                                       
                        )),
                      )
                      
             )
             
  ))

# ==============================================================================
# SERVER LOGIC
# ==============================================================================
server <- function(input, output, session) {
  
  # Reactive values for data filtering and state management
  filtered_complete_researchers_PA_latest <- reactiveVal(data.frame())
  filtered_persons <- reactiveVal(data.frame())
  filtered_cities <- reactiveVal()
  filtered_regions <- reactiveVal()
  filtered_pubs <- reactiveVal()
  filtered_pubs_summed <- reactiveVal()
  # Publications-tab view. Defaults to the full filtered_pubs_summed(), but the
  # "Publikationen anzeigen" links narrow it to one person/funder WITHOUT
  # altering filtered_pubs_summed() (which the map, word cloud and keyword table
  # read), so those broad visualizations stay in sync with the active filter.
  pub_table_view <- reactiveVal()
  filtered_prizes <- reactiveVal()
  filtered_prizes_counted <- reactiveVal()
  filtered_faculties <- reactiveVal()
  filtered_researchers <-  reactiveVal()
  
  # User interaction tracking
  clicked_orcid <- reactiveVal()
  clicked_organization <- reactiveVal()
  has_started <- reactiveVal()
  has_refiltered <- reactiveVal()
  
  # Visualization data
  plotted_points_var <- reactiveVal()
  plotted_points_countries_var <- reactiveVal()
  # Researchers whose work is in the current filter — computed ONCE here and
  # shared by the network node/edge builders below. Previously this same
  # `complete_researchers_PA %>% filter(orcid %in% filtered_pubs()$orcid)` was
  # re-run 6+ times per network rebuild.
  researchers_in_view <- reactive({
    req(filtered_pubs())
    complete_researchers_PA %>%
      filter(orcid %in% filtered_pubs()$orcid)
  })

  # Network graph data as lazy reactives (previously built eagerly inside
  # filter_data()). As reactives they compute only when first read — i.e. when
  # the Netzwerk tab's outputs render — and cache until the filter changes, so
  # the expensive build is skipped entirely if that tab is never opened.
  network_nodes_shared <- reactive({
    req(filtered_pubs())
    riv <- researchers_in_view()

    filtered_pubs() %>%
      select(title_title_value, label) %>%
      mutate(id = title_title_value, title = title_title_value, label = "") %>%
      mutate(group = "publication") %>%
      mutate(id = paste0(id, "_", group)) %>%
      select(-title_title_value) %>%
      rbind(
        riv %>%
          select(name_value, orcid) %>%
          mutate(id = orcid, title = name_value, label = name_value) %>%
          mutate(group = "person") %>%
          mutate(id = paste0(id, "_", group)) %>%
          select(-name_value, -orcid)
      ) %>%
      rbind(
        filtered_prizes() %>%
          mutate(id = organization, title = organization, label = organization) %>%
          mutate(group = "funding") %>%
          mutate(id = paste0(id, "_", group)) %>%
          select(group, title, id, label) %>%
          unique()
      ) %>%
      rbind(
        riv %>%
          select(organization_name) %>%
          mutate(id = organization_name, title = organization_name, label = organization_name) %>%
          mutate(group = "institution") %>%
          mutate(id = paste0(id, "_", group)) %>%
          select(-organization_name)
      ) %>%
      rbind(
        riv %>%
          select(department_name, organization_name) %>%
          mutate(id = paste0(organization_name, "_", department_name), title = department_name, label = "") %>%
          mutate(group = "institution_sm") %>%
          mutate(id = paste0(id, "_", group)) %>%
          select(-department_name, -organization_name)
      ) %>%
      group_by(id) %>%
      slice(1) %>%
      ungroup() %>%
      drop_na()
  })

  network_edges_shared <- reactive({
    req(filtered_pubs())
    riv <- researchers_in_view()

    filtered_pubs() %>%
      select(orcid, title_title_value) %>%
      rename(from = orcid, to = title_title_value) %>%
      mutate(from = paste0(from, "_person"),
             to = paste0(to, "_publication")) %>%
      rbind(
        riv %>%
          filter(!is.na(department_name)) %>%
          select(orcid, department_name, organization_name) %>%
          mutate(department_name = paste0(organization_name, "_", department_name)) %>%
          select(-organization_name) %>%
          rename(from = orcid, to = department_name) %>%
          mutate(from = paste0(from, "_person"),
                 to = paste0(to, "_institution_sm"))
      ) %>%
      rbind(
        riv %>%
          filter(is.na(department_name)) %>%
          select(orcid, organization_name) %>%
          rename(from = orcid, to = organization_name) %>%
          mutate(from = paste0(from, "_person"),
                 to = paste0(to, "_institution"))
      ) %>%
      rbind(
        riv %>%
          select(organization_name, department_name) %>%
          mutate(department_name = paste0(organization_name, "_", department_name)) %>%
          rename(from = organization_name, to = department_name) %>%
          mutate(from = paste0(from, "_institution"),
                 to = paste0(to, "_institution_sm"))
      ) %>%
      rbind(
        filtered_prizes() %>%
          select(orcid, organization) %>%
          rename(from = orcid, to = organization) %>%
          mutate(from = paste0(from, "_person"),
                 to = paste0(to, "_funding"))
      ) %>%
      drop_na() %>%
      filter(from != to) %>%
      filter(from %in% network_nodes_shared()$id, to %in% network_nodes_shared()$id) %>%
      unique()
  })
  
  first_run <- reactiveVal(TRUE)

  # The Zurücksetzen button is never disabled — it is always clickable. Instead
  # its colour signals whether any filter differs from the defaults: grey
  # (bttn-default) at defaults, red (bttn-danger) when a filter is active.
  # filter_is_active is (re)computed at the end of filter_data().
  filter_is_active <- reactiveVal(FALSE)
  observe({
    if (isTRUE(filter_is_active())) {
      shinyjs::runjs("$('#reset').closest('.bttn').removeClass('bttn-default').addClass('bttn-danger');")
    } else {
      shinyjs::runjs("$('#reset').closest('.bttn').removeClass('bttn-danger').addClass('bttn-default');")
    }
  })
  
  output$action_button <- renderUI({
    actionBttn(
      inputId = "go",
      label = HTML("> Ergebnisse aktualisieren <"),
      style = "simple",
      color = "primary"
    )
  })
  
  observeEvent(input$select_all_fach, {
    all_choices <- c("agrar", "phil", "jura", "med", "mint",
                     "rewi", "tech","sowi")
    
    updateCheckboxGroupButtons(
      session,
      inputId = "faculties",
      selected = if (input$select_all_fach) all_choices else character(0)
    )
  }, ignoreInit = TRUE)  # <-- this prevents firing on load
  
  observeEvent(input$select_all_regions, {
    all_choices <- c("japan", "korea", "maritim", "sonstige", "china", 
                     "sea", "taiwan")
    
    updateCheckboxGroupButtons(
      session,
      inputId = "regions",
      selected = if (input$select_all_regions) all_choices else character(0)
    )
  }, ignoreInit = TRUE)  # <-- this prevents firing on load
  
  observeEvent(input$select_all, {
    
    all_choices <- c("bremen", "clausthal", "flensburg", "greifswald", "hamburg", 
                     "kiel", "luebeck", "lueneburg", "oldenburg", "rostock")
    
    updateCheckboxGroupButtons(
      session,
      inputId = "places",
      selected = if (input$select_all) all_choices else character(0)
    )
  }, ignoreInit = TRUE)  # <-- this prevents firing on load
  
  # ------------------------------------------------------------------------------
  # Visualization: East Asia Geographic Distribution Map
  # ------------------------------------------------------------------------------
  # This function creates a map showing geographic distribution of research activities
  # across East Asia, with point clustering and country-level coloring
  
  output$sfPlot <- renderLeaflet({

    datatable_pubs <- filtered_pubs_summed()
    plotted_points <- plotted_points_var()
    plotted_points_countries <- plotted_points_countries_var()

    # Drop region-level entities that carry no point coordinate (empty geometry —
    # e.g. "East Asia", "Korea"); they have no marker position, and leaving them
    # in makes leaflet warn about invalid lat/lon.
    if (!is.null(plotted_points) && nrow(plotted_points) > 0)
      plotted_points <- plotted_points[!sf::st_is_empty(plotted_points), , drop = FALSE]

    if(nrow(plotted_points)>0){
      
      # Round coordinates to 1 decimal place for geographic clustering
      # This groups nearby locations together to reduce visual clutter
      coords_rounded <- st_cast(plotted_points, "POINT") %>%
        st_coordinates() %>%
        as.data.frame() %>%
        mutate(X = round(X, 0), Y = round(Y, 0))
      
      # Build one st_point per rounded coordinate
      rounded_geometry <- lapply(seq_len(nrow(coords_rounded)), function(i) {
        st_point(c(coords_rounded$X[i], coords_rounded$Y[i]))
      })
      
      # Combine the new geometries into an sf object
      plotted_points_rounded <- st_sf(geometry = st_sfc(rounded_geometry), crs = st_crs(plotted_points)) %>%
        cbind(plotted_points %>% st_drop_geometry %>% select(n,text))
    } else{
      plotted_points_rounded <- plotted_points
    }
    # # Now, we can proceed with summarizing the data (sum_n) by rounded coordinates
    plotted_points_summary <- plotted_points_rounded %>%
      group_by(geometry) %>%
      summarise(sum_n = sum(n), .groups = "drop") %>%
      left_join(as.data.frame(plotted_points_rounded) %>% select(geometry, text), by = "geometry") %>%
      group_by(geometry) %>%
      slice(1) %>%
      ungroup()
    
    # Interactive base map framed on the Asia-Pacific region (zoom/pan enabled).
    title_html <- paste0(
      "<b>Orte in Publikationen aus ",
      htmltools::htmlEscape(str_to_title(paste0(filtered_cities(), collapse = ", "))),
      "</b><br><span style='font-weight:normal'>",
      formatC(nrow(datatable_pubs), format = "d", big.mark = ".", decimal.mark = ","),
      " gefilterte Publikationen · Markergröße = Häufigkeit</span>")

    m <- leaflet(options = leafletOptions(minZoom = 2)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # Default view zoomed on the East/Southeast-Asia + India core. India's far
      # western tip (~68-70°E) and Sri Lanka's south sit just outside the initial
      # frame, but the polygons are complete now — pan/zoom out to see them.
      fitBounds(70, 8, 146, 49) %>%
      addControl(HTML(title_html), position = "topright")

    # Whole countries that are named, shaded by mention frequency.
    has_countries <- nrow(plotted_points_countries) > 0 &&
      any(plotted_points_countries$n > 0, na.rm = TRUE)
    if (has_countries) {
      pal_c <- colorNumeric(c("#f7c7e3", "#9b0a7d"), domain = plotted_points_countries$n)
      m <- m %>% addPolygons(
        data = plotted_points_countries,
        layerId = ~NAME,
        weight = 1.2, color = ~pal_c(n), fillColor = ~pal_c(n), fillOpacity = 0.2,
        label = ~lapply(paste0("<b>", htmltools::htmlEscape(NAME), "</b>: ", n,
                               " Nennung(en)<br><i>Klicken: zu Suchbegriffen hinzufügen</i>"), HTML))
    }

    # Located places as circle markers: radius ~ count, exact count on hover.
    if (nrow(plotted_points_summary) > 0) {
      sn  <- plotted_points_summary$sum_n
      rad <- if (length(unique(sn)) > 1) {
        4 + 18 * (sqrt(sn) - sqrt(min(sn))) / (sqrt(max(sn)) - sqrt(min(sn)))
      } else 8
      m <- m %>% addCircleMarkers(
        data = plotted_points_summary,
        layerId = ~text,
        radius = rad, stroke = FALSE, fillColor = "#9b0a7d", fillOpacity = 0.6,
        label = ~lapply(paste0("<b>", htmltools::htmlEscape(str_to_title(text)),
                               "</b>: ", sum_n, " Nennung(en)<br><i>Klicken: zu Suchbegriffen hinzufügen</i>"), HTML))
    }

    if (has_countries) {
      m <- m %>% addLegend(position = "bottomright", pal = pal_c,
                           values = plotted_points_countries$n,
                           title = "Nennungen<br>als Staat", opacity = 0.6)
    }
    m

  })
  
  # ------------------------------------------------------------------------------
  # Visualization: World Cloud
  # ------------------------------------------------------------------------------
  
  output$wordCloud <- renderWordcloud2({
    datatable_pubs <- filtered_pubs_summed()
    
    if (!is.null(datatable_pubs) && nrow(datatable_pubs) > 0) {
      # Ensure clicked_node has a valid path
      keywords <- complete_spacy_PA_keywords %>%
        filter(id %in% datatable_pubs$id)
      
      # Merge and clean up
      word_counts <- keywords %>%
        group_by(id,text) %>%
        slice(1) %>%
        ungroup() %>%
        select(id,text) %>%
        unique() %>%
        select(text) %>%
        count(text) %>%
        filter(n>2)
      
      colnames(word_counts) <- c("word","freq")
      
      # Calculate IQR and filter out extreme outliers
      q1 <- quantile(word_counts$freq, 0.25)
      q3 <- quantile(word_counts$freq, 0.75)
      iqr <- q3 - q1
      upper_bound <- q3 + 10 * iqr
      
      # Filter out words with frequency greater than the upper bound
      word_counts <- word_counts %>% filter(freq <= upper_bound)
      
      
      word_counts <- word_counts %>%
        mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
      
      set.seed(1337)
      
      wordcloud2(data=word_counts, size=1.5, shuffle=F, backgroundColor="transparent")
    }
  })
  
  # ------------------------------------------------------------------------------
  # Network Graph Output  
  # ------------------------------------------------------------------------------
  
  output$mynetworkid <- renderVisNetwork({
    
    # Convert to igraph
    network_nodes <- network_nodes_shared()
    network_edges <- network_edges_shared()

    # No results → nothing to plot. Bail out quietly instead of letting
    # graph_from_edgelist() error on an empty (0-row) edge list.
    req(nrow(network_edges) > 0)

    # Build igraph directly from edges
    g <- graph_from_edgelist(
      as.matrix(network_edges[, c("from", "to")]),
      directed = FALSE
    )
    
    # Components
    comps <- components(g)
    max_size <- max(comps$csize)
    keep_comps <- which(comps$csize >= 0.05 * max_size)
    
    # Subgraph with only large-enough components
    keep_nodes <- V(g)[comps$membership %in% keep_comps]
    g_sub <- induced_subgraph(g, keep_nodes)
    
    # Convert back to visNetwork format
    network_nodes <- network_nodes[network_nodes$id %in% V(g_sub)$name, ]
    network_edges <- as_data_frame(g_sub, what = "edges")

    set.seed(1337)
    visNetwork(network_nodes, network_edges) %>%
      addFontAwesome()  %>%
      visGroups(groupname = "institution", shape = "icon", icon = list(code = "f19c", size = 40, color = "#9b0a7d"))  %>%
      visGroups(groupname = "institution_sm", shape = "icon", icon = list(code = "f19c", size = 15, color = "#9b0a7d"))  %>%
      visGroups(groupname = "person", shape = "icon", icon = list(code = "f007", size = 40, color = "#c34113"))  %>%
      visGroups(groupname = "publication", shape = "icon", icon = list(code = "f1ea", size = 15, color = "#428bca")) %>%
      visGroups(groupname = "funding", shape = "icon", icon = list(code = "f0d6", size = 40, color = "#428bca")) %>%
      visIgraphLayout(layout = "layout_with_fr") %>%
      visPhysics(stabilization = F, timestep=.35, minVelocity=10, maxVelocity=50, solver="forceAtlas2Based") %>%
      visEdges(smooth = FALSE) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 2, hover = T)) %>%
      visEvents(click = "function(nodes) {
                  if(nodes.nodes.length > 0) {
                    Shiny.onInputChange('clicked_node', nodes.nodes[0]);
                  } else {
                    Shiny.onInputChange('clicked_node', null);
                  }
                }")
  })
  
  # Reactive observer to filter data — listens to button click.
  # The loading modal is shown first; shinyjs::delay yields a frame so the
  # browser can paint it before the (synchronous) filter_data() runs.
  observeEvent(input$go, {
    showModal(modalDialog(
      title = NULL,
      div(class = "loading-modal-body",
          tags$div(class = "loading-spinner", role = "status"),
          h4("Anfrage wird verarbeitet …"),
          p("Die Ergebnisse werden zusammengestellt.")
      ),
      footer = NULL,
      easyClose = FALSE,
      fade = FALSE,
      size = "s"
    ))
    shinyjs::delay(50, {
      filter_data()
      has_started("T")
      removeModal()
    })
  })
  
  observeEvent(input$main_tabs, {
    if (!is.null(has_started()) && !is.null(has_refiltered()) && input$main_tabs != "Publikationen") {
      filter_data()
      has_refiltered(NULL)
    }
  })
  
  observeEvent(input$link_to_tab2, {
    if(is.na(clicked_orcid())==F){
      filtered_ids <- filtered_pubs() %>% filter(orcid==clicked_orcid())
      pub_table_view(filtered_pubs() %>%
                             filter(id %in% filtered_ids$id) %>%
                             group_by(title_title_value, journal_title_value, doi, source, publication_date_year_value, type, id) %>% # group by all non-name cols
                             summarise(
                               authors = format_authors(last, first),
                               .groups = "drop"
                             ))
      has_refiltered("F")
    }
    updateTabsetPanel(session, "main_tabs", selected = "Publikationen")
  })

  # Jump to the Anleitungen tab from any tab's "Mehr in den Anleitungen" link.
  observeEvent(input$goto_anleitungen, {
    updateNavbarPage(session, "main_navbar", selected = "Anleitungen")
  })

  # Clicking a Schlagwort drops it into the search box as a Suchbegriff and
  # re-runs the existing query filter, then shows the Publikationen results.
  observeEvent(input$keyword_search, {
    term <- input$keyword_search$term
    if (is.null(term) || !nzchar(term)) return(invisible())
    updateTextInput(session, "query_general", value = term)
    filter_data(query_override = term)
    has_started("T")
    updateTabsetPanel(session, "main_tabs", selected = "Publikationen")
  })

  # Clicking a place marker or country on the Karte appends that name to the
  # existing Suchbegriffe (space-separated, de-duplicated), re-runs the filter,
  # and returns to the Personen tab — drilling from the map straight to people.
  add_map_term <- function(term) {
    if (is.null(term) || !nzchar(term)) return(invisible())
    current <- input$query_general
    if (is.null(current)) current <- ""
    have <- tolower(strsplit(trimws(current), "\\s+")[[1]])
    have <- have[nzchar(have)]
    new_query <- if (tolower(term) %in% have) trimws(current) else trimws(paste(current, term))
    updateTextInput(session, "query_general", value = new_query)
    filter_data(query_override = new_query)
    has_started("T")
    has_refiltered(NULL)                 # keep the main_tabs observer from re-filtering with a stale query
    updateTabsetPanel(session, "main_tabs", selected = "tab_personen")
  }
  observeEvent(input$sfPlot_marker_click, add_map_term(input$sfPlot_marker_click$id))
  observeEvent(input$sfPlot_shape_click,  add_map_term(input$sfPlot_shape_click$id))

  # Clicking an author name (in a publication's infobox) jumps to the Personen
  # tab and selects that researcher, loading their profile. Pub authors are in
  # the filtered person list by construction, so no re-filter is needed.
  person_table_proxy <- DT::dataTableProxy("dataTablePerson")
  observeEvent(input$person_link, {
    nm <- input$person_link$name
    if (is.null(nm) || !nzchar(nm)) return(invisible())
    updateTabsetPanel(session, "main_tabs", selected = "tab_personen")  # the Personen tab's value=, not its title
    op <- ordered_persons()
    if (is.null(op) || nrow(op) == 0 || !"name_value" %in% names(op)) return(invisible())
    idx <- which(tolower(op$name_value) == tolower(nm))[1]
    if (is.na(idx)) return(invisible())
    # Let the tab switch + table render before selecting the row.
    shinyjs::delay(300, {
      try(DT::selectPage(person_table_proxy, ceiling(idx / 20)), silent = TRUE)
      DT::selectRows(person_table_proxy, idx)
    })
  })

  # Reset all filters to their defaults and re-run the (default) search.
  observeEvent(input$reset, {
    updateCheckboxGroupButtons(session, "regions", selected = c("china", "maritim"))
    updateCheckboxGroupButtons(session, "places",  selected = c("kiel"))
    updateCheckboxGroupButtons(session, "faculties",
                               selected = c("agrar", "mint", "med", "phil",
                                            "jura", "tech", "rewi", "sowi"))
    updateSliderInput(session, "time_range", value = c(2000, 2025))
    updateTextInput(session, "query_general", value = "")
    updateRadioButtons(session, "logic_radio", selected = "and")
    updateCheckboxInput(session, "filter_by_employment", value = TRUE)
    showModal(modalDialog(
      title = NULL,
      div(class = "loading-modal-body",
          tags$div(class = "loading-spinner", role = "status"),
          h4("Anfrage wird verarbeitet …"),
          p("Die Ergebnisse werden zusammengestellt.")
      ),
      footer = NULL, easyClose = FALSE, fade = FALSE, size = "s"
    ))
    # Wait for the input resets to round-trip to the client and back, so
    # filter_data() reads the restored default values.
    shinyjs::delay(350, {
      filter_data()
      has_started("T")
      removeModal()
    })
  })

  observeEvent(input$link_to_tab3, {
    if(is.na(clicked_organization())==F){
      filtered_funds <- complete_prizes_PA %>% filter(organization %in% clicked_organization())
      pub_table_view(filtered_pubs() %>%
                             filter(id %in% filtered_funds$id) %>%
                             group_by(title_title_value, journal_title_value, doi, source, publication_date_year_value, type, id) %>% # group by all non-name cols
                             summarise(
                               authors = format_authors(last, first),
                               .groups = "drop"
                             ))
      has_refiltered("F")
    }
    updateTabsetPanel(session, "main_tabs", selected = "Publikationen")
  })
  
  # ------------------------------------------------------------------------------
  # Core Data Filtering Function
  # ------------------------------------------------------------------------------
  # This function applies all user-selected filters to the research data
  
  filter_data <- function(query_override = NULL) {
    req(input$places)  # Make sure places are selected
    req(input$regions)
    req(input$time_range)
    req(input$faculties)
    
    # Parse user search query. A query_override (e.g. from a clicked keyword)
    # takes precedence over the search box.
    query <- if (is.null(query_override)) input$query_general else query_override
    query_words <- strsplit(query, "\\s+")[[1]]
    query_words <- query_words[nzchar(query_words)]  # drop empty tokens (e.g. leading space)
    logic <- input$logic_radio
    
    # Store selected faculty filters for use across reactive expressions
    selected_faculties <- input$faculties
    filtered_faculties(selected_faculties)
    
    # Translate user-selected city codes to actual city names for filtering
    selected_cities <- unlist(city_mapping[input$places])
    pattern <- paste(selected_cities, collapse = "|")
    
    # Store filtered cities for use in UI display
    filtered_cities(selected_cities)
    
    filtered_researchers(
      complete_researchers_PA %>%
        filter(
          (grepl(pattern, organization_address_city, ignore.case = TRUE) |
             grepl(pattern, organization_name, ignore.case = TRUE)) &
            employment_start <= input$time_range[[2]] &
            employment_end   >= input$time_range[[1]]
        ) %>%
        filter(faculties %in% filtered_faculties())
    )
    
    # Process East Asian region selections
    selected_regions <- unlist(region_mapping[input$regions], use.names = FALSE)
    filtered_regions(unlist(region_mapping_basic[input$regions], use.names = FALSE))
    
    # Filter geographic data to match selected East Asian regions
    relevant_complete_spacy_PA_geo <- complete_spacy_PA_geo %>%
      filter(tolower(ADMIN) %in% tolower(selected_regions))
    
    # Create initial publication dataset: join researchers with their publications, 
    # apply temporal filters, and translate publication types to German labels
    filtered_pubs(
      complete_researchers_PA_latest %>%
        # Keep only researchers with employments in filtered cities
        filter(orcid %in% filtered_researchers()$orcid) %>%
        
        # Join filtered employments (renamed to avoid overwriting existing columns)
        left_join(
          filtered_researchers() %>%
            select(orcid, employment_start, employment_end) %>%
            rename(filtered_start = employment_start,
                   filtered_end   = employment_end),
          by = "orcid",
          relationship = "many-to-many"
        ) %>%
        
        # Join publications
        left_join(
          complete_works_PA,
          by = "orcid",
          relationship = "many-to-many"
        ) %>%
        
        # Keep publications within the relevant employment intervals
        # Optional filter by employment interval
        { if (input$filter_by_employment)
          filter(., publication_date_year_value >= filtered_start &
                   publication_date_year_value <= filtered_end)
          else
            .
        } %>%
        
        # Apply the global time range filter
        filter(publication_date_year_value >= input$time_range[[1]] &
                 publication_date_year_value <= input$time_range[[2]]) %>%
        
        # Arrange by researcher name and publication year
        arrange(last, first, publication_date_year_value) %>%
        
        # Map publication type
        mutate(type = pub_mapping[type]) %>%
        
        # Remove duplicates if a publication overlaps multiple employments
        distinct(orcid, id, .keep_all = TRUE)
    )
    
    # Apply geographic constraint: only include publications with East Asian geo-references
    filtered_pubs(filtered_pubs() %>%
                    filter(id %in% relevant_complete_spacy_PA_geo$id))
    
    # Apply text search filters if user provided a search query
    if (query != "" && length(query_words) > 0) {
      pubs <- filtered_pubs()
      # Extracted keywords (Schlagwörter) of the current pubs, so a search also
      # finds publications where the term is an extracted entity — not only those
      # with it in the title/name/department/organisation.
      kw_sub <- complete_spacy_PA_keywords[complete_spacy_PA_keywords$id %in% pubs$id, ]

      # Literal (non-regex) case-insensitive match of one term against a column.
      # Lowercase both sides + fixed = TRUE so terms like "(", "[" or "*" are
      # treated as text, not regex — a regex grepl would error and abort the
      # whole filter. NA cells (e.g. missing department) become FALSE, not NA.
      term_in_col <- function(term, col) {
        hit <- grepl(tolower(term), tolower(col), fixed = TRUE)
        hit[is.na(hit)] <- FALSE
        hit
      }
      # Does this term appear in ANY searched column OR as an extracted keyword?
      term_in_any <- function(term) {
        kw_ids <- kw_sub$id[grepl(tolower(term), tolower(kw_sub$text), fixed = TRUE)]
        term_in_col(term, pubs$title_title_value) |
          term_in_col(term, pubs$name_value) |
          term_in_col(term, pubs$organization_name) |
          term_in_col(term, pubs$department_name) |
          (pubs$id %in% kw_ids)
      }

      per_word <- lapply(query_words, term_in_any)
      if (logic == "or") {
        # Option (a): keep a row if ANY query word matches somewhere (OR logic)
        keep <- Reduce(`|`, per_word)
      } else {
        # Option (b): keep a row only if EVERY query word matches somewhere (AND)
        keep <- Reduce(`&`, per_word)
      }
      filtered_pubs(pubs[keep, ])
    }
    
    filtered_pubs_summed(filtered_pubs() %>%
                           group_by(title_title_value, journal_title_value, doi, source, publication_date_year_value, type, id) %>% # group by all non-name cols
                           summarise(
                             authors = format_authors(last, first),
                             .groups = "drop"
                           ))

    # Reset the publications-tab view to the full summary on every (re)filter.
    pub_table_view(filtered_pubs_summed())

    filtered_researchers(
      filtered_researchers() %>%
        filter(orcid %in% filtered_pubs()$orcid)
    )

    filtered_prizes(
      complete_prizes_PA %>%
        filter(orcid %in% filtered_researchers()$orcid) %>%
        filter(id %in% filtered_pubs()$id)
    )
    
    filtered_prizes_counted( 
      filtered_prizes() %>%
        select(id,organization) %>%
        unique() %>%
        count(organization) %>%
        select(organization,n) %>%
        arrange(-n,organization) %>%
        rename("Beteiligter Mittelgeber" = organization, Publikationen = n)  )
    
    # Update the reactive value with the filtered dataset
    filtered_complete_researchers_PA_latest(
      complete_researchers_PA_latest %>% 
        filter(orcid %in% filtered_pubs()$orcid)
    )
    
    plotted_points <- complete_spacy_PA_geo %>%
      filter(id %in% filtered_pubs()$id) %>%
      # as.character() guards the empty-selection case: ifelse() returns a
      # length-0 *logical* when there are no rows, which then can't join to the
      # character sf_countries$NAME below.
      mutate(text = as.character(ifelse(text %in% names(country_text_aliases),
                                        unname(country_text_aliases[text]),
                                        text))) %>%
      select(id,text) %>%
      unique() %>%
      count(text) %>%
      arrange(-n)
    
    plotted_points_countries <- plotted_points %>%
      filter(text %in% sf_countries$NAME) %>%
      st_drop_geometry() %>%
      rename(NAME=text) %>%
      left_join(sf_countries,by="NAME") %>%
      arrange(n) %>%
      st_as_sf()
    
    plotted_points <- plotted_points %>%
      filter(!text %in% sf_countries$NAME)
    
    plotted_points_var(plotted_points)
    plotted_points_countries_var(plotted_points_countries)
    
    # Network graph data (nodes/edges) is built lazily in the
    # network_nodes_shared() / network_edges_shared() reactives below, so it is
    # only computed when the Netzwerk tab is viewed, not on every filter_data() run.

    # Signal whether any filter differs from its default → colours the
    # Zurücksetzen button (red = active, grey = at defaults). Uses `query` (the
    # override-aware effective query) so keyword searches register immediately.
    any_active <- FALSE
    if (nzchar(query)) any_active <- TRUE
    if (!identical(logic, "and")) any_active <- TRUE
    if (!setequal(input$regions, c("china", "maritim"))) any_active <- TRUE
    if (!setequal(input$places, c("kiel"))) any_active <- TRUE
    if (!setequal(input$faculties, c("agrar", "mint", "med", "phil",
                                     "jura", "tech", "rewi", "sowi"))) any_active <- TRUE
    if (!identical(as.numeric(input$time_range), c(2000, 2025))) any_active <- TRUE
    if (!isTRUE(input$filter_by_employment)) any_active <- TRUE
    filter_is_active(any_active)
  }
  
  # ------------------------------------------------------------------------------
  # Logics First Startup
  # ------------------------------------------------------------------------------
  
  # --- Trigger once on startup, after inputs have populated ---
  observe({
    req(input$places, input$regions, input$time_range, input$faculties)
    if (isTRUE(first_run())) {
      first_run(FALSE)
      filter_data()
      has_started("T")
    }
  })
  
  # ------------------------------------------------------------------------------
  # Data Table: Researchers
  # ------------------------------------------------------------------------------
  
  # Single ordered source of truth for the researcher table AND its detail
  # panel. A clicked row index (input$dataTablePerson_rows_selected) must map to
  # the same researcher in both; deriving the order in two separate pipelines
  # risks divergence on ties. Build the ordered frame once here (with orcid +
  # all detail columns) and index this same frame in both places.
  ordered_persons <- reactive({
    base <- filtered_complete_researchers_PA_latest()
    if (is.null(base) || nrow(base) == 0) return(base)
    base %>%
      # nsum = publication count per researcher; missing → 0 (not NA)
      mutate(nsum = as.integer(table(filtered_pubs()$orcid)[orcid])) %>%
      mutate(nsum = ifelse(is.na(nsum), 0L, nsum)) %>%
      arrange(last, first, organization_name, nsum)
  })

  # Render the DataTable
  dataTablePerson <- reactive({
    filtered_data <- ordered_persons()

    # Check if the filtered data is empty
    if (is.null(filtered_data) || nrow(filtered_data) == 0) {
      return(data.frame("Keine Ergebnisse" = "Bitte ändern Sie Ihre Filter.", check.names = FALSE))
    }

    filtered_data %>%
      select(last, first, organization_name, nsum) %>%
      rename(Familienname = last, Vorname = first, "Publikationen" = nsum,
             "Letztdokumentierte Einrichtung" = organization_name)
  })
  
  output$dataTablePerson <- DT::renderDataTable({
    DT::datatable(dataTablePerson() ,
                  selection = list(mode = "single"),
                  options = list(
                    
                    orderClasses = TRUE, 
                    pageLength = 20,
                    dom = 'tip',
                    language = list(
                      paginate = list(
                        "first" = "Erste",  # First page
                        "previous" = "Zurück",  # Previous page
                        "next" = "Weiter",  # Next page
                        "last" = "Letzte"  # Last page
                      ),
                      info = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen",  # Change info text
                      infoEmpty = "Zeige 0 bis 0 von 0 Einträgen",  # When no entries
                      infoFiltered = "(gefiltert aus _MAX_ Einträgen)",  # Filtered info
                      lengthMenu = "Zeige _MENU_ Einträge"  # Length menu
                    ),
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().body()).css({'background-color': 'transparent'});",
                      "$(this.api().table().header()).css({'background-color': 'transparent'});",
                      "$('.dataTables_info').css({'background-color': 'transparent'});",
                      "$('.dataTables_paginate').css({'background-color': 'transparent'});",
                      "}"
                    )
                  ))
  })
  
  # ------------------------------------------------------------------------------
  # Data Table: Publications
  # ------------------------------------------------------------------------------
  
  dataTablePub <- reactive ({
    datatable_pubs <- pub_table_view()

    # Check if the filtered data is empty
    if (is.null(datatable_pubs) || nrow(datatable_pubs) == 0) {
      return(data.frame("Keine Ergebnisse" = "Bitte ändern Sie Ihre Filter.", check.names = FALSE))
    }
    
    datatable_pubs %>%  
      arrange(publication_date_year_value, title_title_value, authors ) %>%
      select(publication_date_year_value, title_title_value, authors ) %>%
      rename(Titel = title_title_value, "Autor:innen" = authors, "Jahr" = publication_date_year_value)
  })
  
  # Render the DataTable
  output$dataTablePub <- DT::renderDataTable({
    pub_tbl <- dataTablePub()
    # escape = FALSE below preserves legitimate title formatting (italics,
    # sub/superscript), so neutralize any script/markup in third-party title
    # and author text first. (dataTablePub() itself stays raw for CSV export.)
    if ("Titel" %in% names(pub_tbl)) {
      pub_tbl$Titel <- sanitize_html(pub_tbl$Titel)
    }
    if ("Autor:innen" %in% names(pub_tbl)) {
      pub_tbl[["Autor:innen"]] <- sanitize_html(pub_tbl[["Autor:innen"]])
    }
    DT::datatable(pub_tbl,
                  selection = list(mode = "single"),  # <-- only one row selectable
                  options = list(
                    
                    orderClasses = TRUE, 
                    pageLength = 15,
                    dom = 'tip',
                    language = list(
                      paginate = list(
                        "first" = "Erste",  # First page
                        "previous" = "Zurück",  # Previous page
                        "next" = "Weiter",  # Next page
                        "last" = "Letzte"  # Last page
                      ),
                      info = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen",  # Change info text
                      infoEmpty = "Zeige 0 bis 0 von 0 Einträgen",  # When no entries
                      infoFiltered = "(gefiltert aus _MAX_ Einträgen)",  # Filtered info
                      lengthMenu = "Zeige _MENU_ Einträge"  # Length menu
                    ),
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().body()).css({'background-color': 'transparent'});",
                      "$(this.api().table().header()).css({'background-color': 'transparent'});",
                      "$('.dataTables_info').css({'background-color': 'transparent'});",
                      "$('.dataTables_paginate').css({'background-color': 'transparent'});",
                      "}"
                    )
                  ),escape=F
    )
  })
  
  # ------------------------------------------------------------------------------
  # Data Table: Funding
  # ------------------------------------------------------------------------------
  
  dataTablePrize <- reactive({

    df <- filtered_prizes_counted()

    # Defensive checks
    if (is.null(df) || nrow(df) == 0) {
      return(data.frame("Keine Ergebnisse" = "Bitte ändern Sie Ihre Filter.", check.names = FALSE))
    }
    # Insert a middle "Region" column with the funder's country flag (SVG/emoji).
    cc   <- funder_countries$country[match(df[["Beteiligter Mittelgeber"]], funder_countries$organization)]
    flag <- vapply(cc, flag_html, character(1))
    out  <- data.frame(df[["Beteiligter Mittelgeber"]], flag, df[["Publikationen"]],
                       check.names = FALSE, stringsAsFactors = FALSE)
    names(out) <- c("Beteiligter Mittelgeber", "Region", "Publikationen")
    out

  })
  
  # Render the DataTable
  output$dataTablePrize <- DT::renderDataTable({
    prize_tbl <- dataTablePrize()
    DT::datatable(prize_tbl,
                  selection = list(mode = "single"),  # <-- only one row selectable
                  options = list(

                    orderClasses = TRUE,
                    pageLength = 20,
                    dom = 'tip',
                    language = list(
                      paginate = list(
                        "first" = "Erste",  # First page
                        "previous" = "Zurück",  # Previous page
                        "next" = "Weiter",  # Next page
                        "last" = "Letzte"  # Last page
                      ),
                      info = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen",  # Change info text
                      infoEmpty = "Zeige 0 bis 0 von 0 Einträgen",  # When no entries
                      infoFiltered = "(gefiltert aus _MAX_ Einträgen)",  # Filtered info
                      lengthMenu = "Zeige _MENU_ Einträge"  # Length menu
                    ),
                    initComplete = htmlwidgets::JS(
                      "function(settings, json) {",
                      "$(this.api().table().body()).css({'background-color': 'transparent'});",
                      "$(this.api().table().header()).css({'background-color': 'transparent'});",
                      "$('.dataTables_info').css({'background-color': 'transparent'});",
                      "$('.dataTables_paginate').css({'background-color': 'transparent'});",
                      "}"
                    )),
                  # Render the Region column's flag HTML; escape every other column.
                  # Referencing names(prize_tbl) also covers the "Keine Ergebnisse"
                  # placeholder (no Region column) without an "escape column not found" error.
                  escape = setdiff(names(prize_tbl), "Region")
    )
  })

  # ------------------------------------------------------------------------------
  # Förderungen: pie chart of filtered publications by funder country/region.
  # A publication funded from several regions counts once per region, so slices
  # describe the regional spread of funded publications rather than a partition.
  # ------------------------------------------------------------------------------
  output$foerderRegionPie <- renderPlot({
    fp <- filtered_prizes()
    req(!is.null(fp) && nrow(fp) > 0)

    region_counts <- fp %>%
      distinct(id, organization) %>%
      left_join(funder_countries, by = "organization") %>%
      filter(!is.na(country) & nzchar(trimws(country))) %>%
      distinct(id, country) %>%
      count(country, name = "n") %>%
      arrange(desc(n))
    req(nrow(region_counts) > 0)

    # Code -> German label; unmapped codes keep their raw value.
    region_counts$region <- ifelse(
      region_counts$country %in% names(country_de_names),
      country_de_names[region_counts$country], region_counts$country)

    # Keep the largest slices; fold the long tail into "Sonstige".
    topn <- 8L
    if (nrow(region_counts) > topn) {
      keep <- region_counts[seq_len(topn), c("region", "n")]
      rest <- tibble(region = "Sonstige",
                     n = sum(region_counts$n[(topn + 1L):nrow(region_counts)]))
      region_counts <- bind_rows(keep, rest)
    } else {
      region_counts <- region_counts[, c("region", "n")]
    }

    region_counts$region <- factor(region_counts$region, levels = region_counts$region)
    pal <- viridisLite::plasma(nrow(region_counts), begin = 0.1, end = 0.9, direction = -1)

    ggplot(region_counts, aes(x = "", y = n, fill = region)) +
      geom_col(width = 1, color = "white", linewidth = 0.4) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = pal) +
      theme_void() +
      theme(legend.position = "right",
            legend.title = element_blank(),
            legend.text = element_text(size = 11),
            plot.caption = element_text(size = 8, color = "grey50"),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            legend.background = element_rect(fill = "transparent", color = NA),
            legend.key = element_rect(fill = "transparent", color = NA)) +
      labs(caption = plot_caption)
  }, bg = "transparent")

  # ------------------------------------------------------------------------------
  # Word Frequency Table
  # ------------------------------------------------------------------------------
  
  output$wordTable <- DT::renderDataTable({
    datatable_pubs <- filtered_pubs_summed()
    
    if (!is.null(datatable_pubs) && nrow(datatable_pubs) > 0) {
      # Ensure clicked_node has a valid path
      keywords <- complete_spacy_PA_keywords %>%
        filter(id %in% datatable_pubs$id)
      
      # Merge and clean up
      word_counts <- keywords %>% filter(id %in% datatable_pubs$id) %>%
        unique() %>%
        select(text) %>%
        count(text)
      
      colnames(word_counts) <- c("word","freq")
      wt <- as.data.frame(word_counts %>% arrange(-freq) %>% rename(Frequenz=freq,Begriff=word))
      wt$Begriff <- kw_link(wt$Begriff)  # clickable keyword -> Suchbegriff
      DT::datatable(wt,
                    options = list(orderClasses = TRUE, pageLength = 5,  # Number of rows to display per page
                                   dom = 'tip',  # Table control elements (e.g., 't' for table, 'i' for info, 'p' for pagination)
                                   language = list(
                                     paginate = list(
                                       "first" = "Erste",  # First page
                                       "previous" = "Zurück",  # Previous page
                                       "next" = "Weiter",  # Next page
                                       "last" = "Letzte"  # Last page
                                     ),
                                     info = "Zeige _START_ bis _END_ von _TOTAL_ Einträgen",  # Change info text
                                     infoEmpty = "Zeige 0 bis 0 von 0 Einträgen",  # When no entries
                                     infoFiltered = "(gefiltert aus _MAX_ Einträgen)",  # Filtered info
                                     lengthMenu = "Zeige _MENU_ Einträge"  # Length menu
                                   ),
                                   initComplete = htmlwidgets::JS(
                                     "function(settings, json) {",
                                     "$(this.api().table().body()).css({'background-color': 'transparent'});",
                                     "$(this.api().table().header()).css({'background-color': 'transparent'});",
                                     "$('.dataTables_info').css({'background-color': 'transparent'});",
                                     "$('.dataTables_paginate').css({'background-color': 'transparent'});",
                                     "}"
                                   )
                    ), escape = FALSE)  # Begriff holds kw_link() HTML (term already escaped); Frequenz is numeric
    }
  })
  
  # ------------------------------------------------------------------------------
  # Researchers Table UI
  # ------------------------------------------------------------------------------
  
  output$dynamic_ui_person <- renderUI({
    # Index the same ordered frame the table is rendered from, so the row index
    # maps to the same researcher.
    datatable_persons <- ordered_persons()

    clicked_node <- datatable_persons[input$dataTablePerson_rows_selected, ]

    if (!is.null(clicked_node) && nrow(clicked_node) > 0) {

      # Create dynamic UI. role/department/organization are plain-text fields,
      # so p() auto-escapes them; the ORCiD link is built with tags$a() so the
      # href is attribute-escaped (a crafted value cannot break out).
      tagList(
        h4(clicked_node$name_value),
        if (!is.na(clicked_node$role_title)) {
          p(clicked_node$role_title)
        } else {
          NULL
        },
        if (!is.na(clicked_node$department_name)) {
          p(clicked_node$department_name)
        } else {
          NULL
        },
        if (!is.na(clicked_node$organization_name)) {
          p(clicked_node$organization_name)
        } else {
          NULL
        },
        if (!is.na(clicked_node$orcid)) {
          # always set clicked_orcid, even if format is wrong
          clicked_orcid(clicked_node$orcid)

          orcid_val <- trimws(as.character(clicked_node$orcid))   # ensure string, strip spaces

          if (!grepl("^no_orcid", orcid_val, ignore.case = TRUE)) {
            # anything not starting with "no_orcid" → clickable link
            p(HTML("<b>ORCiD</b>: "),
              tags$a(href = paste0("https://orcid.org/", orcid_val),
                     target = "_blank", orcid_val))
          } else {
            # if it starts with "no_orcid" → plain text
            p(HTML("<b>ORCiD</b>: Keine ORCiD verfügbar"))
          }

        } else {
          NULL
        },
        prev_institutions_ui(clicked_node$orcid, clicked_node$organization_name),
        actionLink("link_to_tab2", "🗐 Publikationen anzeigen"),
      )
    } else {
      h4("Klicken Sie auf einen Eintrag für weiterführende Informationen.")
    }
  })
  
  # ------------------------------------------------------------------------------
  # Publication Table UI
  # ------------------------------------------------------------------------------
  
  output$dynamic_ui_pubs <- renderUI({

    datatable_pubs <- pub_table_view() %>% arrange(publication_date_year_value, title_title_value, authors)


    clicked_node <- datatable_pubs[input$dataTablePub_rows_selected, ]

    if (!is.null(clicked_node) && nrow(clicked_node) > 0) {
      # Ensure clicked_node has a valid path
      path_value <- clicked_node$id
      # The publication's (filtered) authors, for clickable author links.
      authors_df <- filtered_pubs() %>%
        filter(id == path_value) %>%
        distinct(last, first, name_value)
      keywords <- complete_spacy_PA_keywords %>%
        filter(id == path_value)

      # Merge and clean up
      keywords <- keywords %>%
        group_by(id,text) %>%
        slice(1) %>%
        ungroup() %>%
        select(id,text) %>%
        unique() %>%
        arrange(text)

      # Create dynamic UI. All third-party data fields are run through
      # sanitize_html(); the DOI link is built with tags$a() so the href is
      # attribute-escaped and a crafted DOI cannot break out.
      tagList(
        p(HTML(paste0(author_links(authors_df), " (", sanitize_html(clicked_node$publication_date_year_value), ")"))),

        h4(HTML(sanitize_html(clicked_node$title_title_value))),
        if (!is.na(clicked_node$journal_title_value)) {
          p(HTML(paste0("<i>", sanitize_html(clicked_node$journal_title_value), "</i>")))
        } else {
          NULL
        },
        p(clicked_node$type),
        if (!is.na(clicked_node$doi)) {
          p(HTML("<b>DOI</b>: "),
            tags$a(href = paste0("https://doi.org/", clicked_node$doi),
                   target = "_blank", clicked_node$doi))
        } else {
          NULL
        },
        local({
          cbc <- complete_works_PA$cited_by_count[match(path_value, complete_works_PA$id)]
          if (length(cbc) && !is.na(cbc) && cbc > 0) {
            p(HTML(paste0("<b>Zitationen</b>: ",
                          format(cbc, big.mark = ".", decimal.mark = ","))))
          } else NULL
        }),
        if (nrow(keywords) > 0) {
          p(HTML(paste0("<b>Schlagwörter</b>: ", paste(kw_link(keywords %>% pull(text) %>% unique()), collapse = ", "))))
        } else {
          NULL
        },
        if (!is.na(clicked_node$source)) {
          p(HTML(paste0("<span style='color: #888888'>(Datenquelle: ", sanitize_html(clicked_node$source), ")</span>")))
        } else {
          NULL
        }
      )
    } else {
      h4("Klicken Sie auf einen Eintrag für weiterführende Informationen.")
    }
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Location Distribution Bar Chart
  # ------------------------------------------------------------------------------
  output$barChart_standort <- renderPlot({
    researcher_counts <- filtered_complete_researchers_PA_latest() %>%
      mutate(
        organization_address_city = ifelse(
          is.na(organization_address_city) | 
            !organization_address_city %in% unlist(city_mapping, use.names = FALSE),
          "außerhalb",
          organization_address_city
        )
      ) %>%
      count(organization_address_city) %>%
      arrange(desc(n))
    
    ggplot(researcher_counts, aes(x = organization_address_city, y = n)) +
      geom_bar(stat = 'identity', fill = "#9b0a7d") +
      theme_minimal() +
      labs(title = "Letztdokumentierter Standort", x = "Standort", y = "Anzahl Forschende",
           caption = plot_caption)  +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Faculty Distribution Bar Chart  
  # ------------------------------------------------------------------------------
  output$barChart_disziplin <- renderPlot({
    
    # Map faculty codes to German display labels and filter by publication authors
    disziplinen <- complete_researchers_PA %>%
      filter(orcid %in% filtered_pubs()$orcid) %>%
      filter(faculties %in% filtered_faculties()) %>%
      left_join(faculties_mapping_tbl, by = c("faculties" = "faculty_code")) %>%
      select(faculty_label, orcid) %>%
      unique()

    pub_counts <- filtered_pubs() %>%
      select(orcid, id) %>%
      left_join(disziplinen, by = "orcid", relationship = "many-to-many") %>%
      select(faculty_label, id) %>%
      unique() %>%
      count(faculty_label) %>%
      arrange(desc(n))

    ggplot(pub_counts, aes(x = faculty_label, y = n)) +
      geom_bar(stat = 'identity', fill = "#9b0a7d") +
      theme_minimal() +
      labs(title = "Assoziierte Fachrichtungen", x = "Fachrichtung", y = "Anzahl Publikationen",
           subtitle = "(Ein Teil der Zuordnungen wurde automatisiert vorgenommen)",
           caption = plot_caption)  +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Region Distribution Bar Chart  
  # ------------------------------------------------------------------------------
  
  output$barChart_region <- renderPlot({
    
    unique_admins <- complete_spacy_PA_geo %>%
      st_drop_geometry() %>%
      select(id,ADMIN) %>%
      unique()
    
    counted_by_admin <- filtered_pubs() %>%
      select(-doi) %>%
      left_join(unique_admins,by="id",relationship = "many-to-many") %>%
      filter(!is.na(ADMIN))
    
    pub_counts <- counted_by_admin %>% select(id,ADMIN) %>% unique() %>%
      count(ADMIN) %>%
      arrange(desc(n))
    
    ggplot(pub_counts, aes(x = ADMIN, y = n)) +
      geom_bar(stat = 'identity', fill = "#9b0a7d") +
      theme_minimal() +
      labs(title = "Forschungsregionen der Publikationen", x = "Forschungsregion",
           y = "Anzahl Publikationen", subtitle = "(Publikationen können mehrere Forschungsregionen behandeln)",
           caption = plot_caption)  +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 12))
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Publication Year Line Plot
  # ------------------------------------------------------------------------------
  
  output$barChart_zeit <- renderPlot({
    pubs <- filtered_pubs() %>% group_by(id,publication_date_year_value) %>% slice(1) %>% ungroup()
    
    yearly_counts <- table(pubs$publication_date_year_value)
    df <- data.frame(
      year = as.numeric(names(yearly_counts)),
      count = as.numeric(yearly_counts)
    )
    # No publications in the selection -> nothing to chart. Stop here so the
    # empty max()/scale_factor and ggplot range (min(x)/max(x)) don't warn.
    req(nrow(df) > 0)

    # Join with years_normal by year
    df <- merge(df, years_normal, by = "year", all.x = TRUE)
    
    # Calculate normalized count
    df$normalized <- df$count / df$n
    
    # Scale to match plot range
    scale_factor <- max(df$count, na.rm = TRUE) / max(df$normalized, na.rm = TRUE)
    
    ggplot(df, aes(x = year)) +
      # Raw counts
      geom_line(aes(y = count), color = "#9b0a7d", linewidth = .5) +
      geom_point(aes(y = count), color = "#9b0a7d", size = 2) +
      
      # Normalized counts scaled to match
      geom_line(aes(y = normalized * scale_factor), color = "#0a579b", linewidth = .5, linetype = "dashed") +
      geom_point(aes(y = normalized * scale_factor), color = "#0a579b", size = 2) +
      
      scale_y_continuous(
        name = "Anzahl Publikationen",
        sec.axis = sec_axis(
          transform = ~ . / scale_factor * 100,  # <- now labeled as %
          name = "Gesamtanteil (%)"
        )
      ) +
      labs(
        title = "Publikationen pro Jahr",
        subtitle = "(absolut & Anteil am Gesamtaufkommen aller\nPublikationen mit personeller Norddeutschland-Verbindung)",
        caption = plot_caption,
        x = "Jahr"
      ) +
      theme_minimal() +
      theme(
        axis.title.y.left = element_text(color = "#9b0a7d"),
        axis.title.y.right = element_text(color = "#0a579b")
      )
  })
  
  # ------------------------------------------------------------------------------
  # Visualization: Cooperation Partners Line Plot
  # ------------------------------------------------------------------------------
  
  output$barChart_koop <- renderPlot({
    
    filtered_coop_countries <- coop_countries %>%
      filter(id %in% filtered_pubs()$id) %>%
      mutate(n = as.numeric(n)) %>%
      group_by(publication_year, authorships_affiliations_country_code) %>%
      summarise(total_n = sum(n, na.rm = TRUE), .groups = "drop")
    
    country_totals <- filtered_coop_countries %>%
      group_by(authorships_affiliations_country_code) %>%
      summarise(total = sum(total_n, na.rm = TRUE), .groups = "drop")
    
    threshold <- quantile(country_totals$total, 0.95)  # change 0.75 to desired percentile
    
    top_countries <- country_totals %>%
      filter(total >= threshold) %>%
      pull(authorships_affiliations_country_code)
    
    filtered_coop_countries_cleaned <- filtered_coop_countries %>%
      mutate(
        country_grouped = ifelse(authorships_affiliations_country_code %in% top_countries,
                                 authorships_affiliations_country_code,
                                 "Sonstige")
      ) %>%
      group_by(publication_year, country_grouped) %>%
      summarise(total_n = sum(total_n, na.rm = TRUE), .groups = "drop")
    
    # Remove Sonstige from plotting data
    filtered_coop_countries_cleaned <- filtered_coop_countries_cleaned %>%
      filter(country_grouped != "Sonstige")
    
    # Shapes: assign unique shape per major country, Sonstige gets shape 16 (solid circle)
    major_countries <- unique(filtered_coop_countries_cleaned$country_grouped)
    major_countries <- major_countries[major_countries != "Sonstige"]
    
    # Assign shapes 0,1,2,... for major countries
    major_shapes <- setNames(seq(0, length(major_countries) - 1), major_countries)
    shape_values <- c(major_shapes, Sonstige = 16)  # 16 = solid circle
    
    # Add group color column for color mapping
    filtered_coop_countries_cleaned <- filtered_coop_countries_cleaned %>%
      mutate(
        group_color = ifelse(country_grouped == "Sonstige", "Sonstige", "Hauptländer")
      )
    
    # Get unique groups (including Sonstige)
    unique_countries <- unique(filtered_coop_countries_cleaned$country_grouped)
    
    # Prepare alternating colors for all groups (repeat colors if needed)
    alt_colors <- rep(c("#9b0a7d", "#0a579b","#c34113"), length.out = length(unique_countries))
    names(alt_colors) <- unique_countries
    
    if (nrow(filtered_coop_countries) == 0) {
      return(
        ggplot() + 
          annotate("text", x = 0, y = 0, label = "Keine Daten verfügbar", size = 5, hjust = 0) +
          labs(
            title = "Länder der Institutionen der Ko-Autor:innen",
            x = "Jahr",
            y = "Anzahl Publikationen") +
          theme_minimal() +
          theme(
            axis.title.y = element_text(color = "black"),
            axis.title.x = element_text(color = "black"),
            legend.position = "bottom",
            legend.box = "vertical",
            legend.title = element_blank()
          )
      )
    }
    
    ggplot(filtered_coop_countries_cleaned, aes(x = publication_year, y = total_n, group = country_grouped)) +
      geom_line(aes(color = country_grouped), linewidth = 0.5) +
      geom_point(aes(shape = country_grouped, color = country_grouped), size = 4) +
      
      scale_color_manual(
        values = alt_colors,
        name = NULL
      ) +
      scale_shape_manual(
        values = shape_values,
        name = NULL
      ) +
      
      scale_y_continuous(
        breaks = function(limits) {
          rng <- limits[2] - limits[1]
          step <- ifelse(rng > 20, 5,
                         ifelse(rng > 10, 2, 1))
          
          min_int <- floor(limits[1])
          max_int <- ceiling(limits[2])
          seq(min_int, max_int, by = step)
        }
      ) +
      
      labs(
        title = "Länder der Institutionen der Ko-Autor:innen",
        subtitle = "Oberste 5% der Länder (außer Norddeutschland)",
        caption = plot_caption,
        x = "Jahr",
        y = "Anzahl Publikationen"
      ) +
      
      theme_minimal() +
      theme(
        axis.title.y = element_text(color = "black"),
        axis.title.x = element_text(color = "black"),
        legend.position = "bottom",
        legend.box = "vertical",
        legend.title = element_blank()
      )
  })
  
  # ------------------------------------------------------------------------------
  # Data Table Output: Third Party Funding
  # ------------------------------------------------------------------------------
  
  output$dynamic_ui_prize <- renderUI({
    datatable_prizes <- filtered_prizes_counted()
    clicked_node <- datatable_prizes[input$dataTablePrize_rows_selected, ]
    
    if (!is.null(clicked_node) && nrow(clicked_node) > 0) {
      tagList(
        h4(clicked_organization()),
        if (!is.na(clicked_node$'Beteiligter Mittelgeber')) {
          actionLink("link_to_tab3", "🗐 Publikationen anzeigen")
        } else {
          NULL
        }
      )
    } else {
      h4("Klicken Sie auf einen Eintrag für weiterführende Informationen.")
    }
  })
  
  observeEvent(input$dataTablePrize_rows_selected, {
    datatable_prizes <- filtered_prizes_counted()
    clicked_node <- datatable_prizes[input$dataTablePrize_rows_selected, ]
    
    if (!is.null(clicked_node) && nrow(clicked_node) > 0) {
      if (!is.na(clicked_node$'Beteiligter Mittelgeber')) {
        clicked_organization(clicked_node$'Beteiligter Mittelgeber')
      } else {
        clicked_organization(NULL)
      }
    }
  })
  
  # ------------------------------------------------------------------------------
  # Data Export Logics
  # ------------------------------------------------------------------------------
  
  # Download as Excel
  output$download_excel_person <- downloadHandler(
    filename = function() {
      paste0("atlas_personen_filter_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dataTablePerson(), path = file)
    }
  )
  
  # Download as CSV
  output$download_csv_person <- downloadHandler(
    filename = function() {
      paste0("atlas_personen_filter_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataTablePerson(), file, row.names = FALSE, 
                fileEncoding = "windows-1252")
    }
  )
  
  # Download as Excel
  output$download_excel_prize <- downloadHandler(
    filename = function() {
      paste0("atlas_foerderungen_filter_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dataTablePrize(), path = file)
    }
  )
  
  # Download as CSV
  output$download_csv_prize <- downloadHandler(
    filename = function() {
      paste0("atlas_foerderungen_filter_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataTablePrize(), file, row.names = FALSE)
    }
  )
  
  # Download as Excel
  output$download_excel_pub <- downloadHandler(
    filename = function() {
      paste0("atlas_publikationen_filter_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      write_xlsx(dataTablePub(), path = file)
    }
  )
  
  # Download as CSV
  output$download_csv_pub <- downloadHandler(
    filename = function() {
      paste0("atlas_publikationen_filter_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dataTablePub(), file, row.names = FALSE)
    }
  )
  
  # ------------------------------------------------------------------------------
  # Network Graph UI
  # ------------------------------------------------------------------------------
  
  output$dynamic_ui_netzwerk <- renderUI({
    clicked_node <- input$clicked_node
    
    if (!is.null(clicked_node)) {
      
      network_nodes <- network_nodes_shared()
      network_edges <- network_edges_shared()
      
      group <- (network_nodes[network_nodes$id == clicked_node, ])$group
      clicked_node_base <- sub("_(person|publication|funding|institution|institution_sm)$", "", clicked_node)
      
      if (group == "person") {
        node_data <- complete_researchers_PA_latest[complete_researchers_PA_latest$orcid == clicked_node_base, ]
        clicked_orcid(node_data$orcid)
        tagList(
          h4(node_data$name_value),
          if (!is.na(node_data$role_title)) p(node_data$role_title) else NULL,
          if (!is.na(node_data$department_name)) p(node_data$department_name) else NULL,
          if (!is.na(node_data$organization_name)) p(node_data$organization_name) else NULL,
          if (!is.na(node_data$orcid)) {
            # always set clicked_orcid, even if format is wrong
            clicked_orcid(node_data$orcid)

            orcid_val <- trimws(as.character(node_data$orcid))   # ensure string, strip spaces

            if (!grepl("^no_orcid", orcid_val, ignore.case = TRUE)) {
              # anything not starting with "no_orcid" → clickable link
              p(HTML("<b>ORCiD</b>: "),
                tags$a(href = paste0("https://orcid.org/", orcid_val),
                       target = "_blank", orcid_val))
            } else {
              p(HTML("<b>ORCiD</b>: Keine ORCiD verfügbar"))
            }
            
          } else {
            NULL
          },
          actionLink("link_to_tab2", "🗐 Publikationen anzeigen")
        )
      } else if (group == "publication") {
        node_data <- complete_works_PA[complete_works_PA$title_title_value == clicked_node_base, ] %>%
          mutate(type = pub_mapping[type])
        authors <- (complete_researchers_PA_latest %>% filter(orcid %in% node_data$orcid))$name_value
        node_data <- node_data %>% slice(1)
        tagList(
          p(HTML(paste0(sanitize_html(paste(authors, collapse = ", ")), " (", sanitize_html(node_data$publication_date_year_value), ")"))),
          h4(node_data$title_title_value),
          if (!is.na(node_data$journal_title_value)) p(HTML(paste0("<i>", sanitize_html(node_data$journal_title_value), "</i>"))) else NULL,
          p(node_data$type),
          if (!is.na(node_data$doi)) p(HTML("<b>DOI</b>: "), tags$a(href = paste0("https://doi.org/", node_data$doi), target = "_blank", node_data$doi)) else NULL
        )
      } else if (group == "funding") {
        node_data <- complete_prizes_PA[complete_prizes_PA$organization == clicked_node_base, ] %>% slice(1)
        clicked_organization(node_data$organization)
        tagList(
          h4(node_data$organization),
          actionLink("link_to_tab3", "🗐 Publikationen anzeigen")
        )
      } else if (group == "institution") {
        h4(clicked_node_base)
      } else if (group == "institution_sm") {
        clicked_node_base <- sub(".*_(?=[^_]+$)", "", clicked_node_base, perl = TRUE)
        h4(clicked_node_base)
      }else {
        h4("Klicken Sie auf einen Eintrag für weiterführende Informationen.")
      }

    } else {
      h4("Klicken Sie auf einen Eintrag für weiterführende Informationen.")
    }
  })

  # ------------------------------------------------------------------------------
  # Citation Network (co-citation / bibliographic coupling / direct citation)
  # ------------------------------------------------------------------------------
  # Lazy: only runs when this tab's output renders; recomputes on filter change,
  # network-type toggle, or slider change. The "Max. Knoten" and "Mindest-
  # Verknüpfungsstärke" sliders make the display limits user-tunable per view.
  output$cocitNetwork <- renderVisNetwork({
    req(filtered_pubs())
    mode <- if (is.null(input$cocit_mode)) "author" else input$cocit_mode
    cap  <- if (is.null(input$cocit_cap)) 150 else input$cocit_cap

    # ---- Author co-citation (undirected; nodes = cited first authors) ----
    if (mode == "author_cocit") {
      validate(need(author_cocit_available,
                    "Autor:innen-Ko-Zitation ist nicht verfügbar (references_meta.csv mit Erstautor:innen – die §21-Metadatenabfrage muss laufen)."))
      mw <- if (is.null(input$cocit_min_weight)) 2 else input$cocit_min_weight
      g <- author_cocitation_graph(unique(filtered_pubs()$id), min_weight = mw, cap = cap)
      validate(need(!is.null(g) && nrow(g$edges) > 0,
                    "Für diese Auswahl ergibt sich kein Autor:innen-Ko-Zitationsnetzwerk. Bitte erweitern Sie die Filter oder senken Sie die Mindest-Verknüpfungsstärke."))
      ids <- g$node_ids   # cited first-author names
      # A cited author who is themselves a corpus researcher is highlighted
      # (orange) like an internal publication; purely external cited authors are
      # grey — the same corpus/external split used for the reference network.
      in_corpus <- tolower(ids) %in% tolower(complete_researchers_PA_latest$name_value)
      nodes <- data.frame(id = ids, label = ids,
                          title = htmltools::htmlEscape(ids),
                          group = ifelse(in_corpus, "person", "person_ext"),
                          stringsAsFactors = FALSE)
      edges <- g$edges
      wspan <- max(1, max(edges$weight) - min(edges$weight))
      edges$width <- 1 + 4 * (edges$weight - min(edges$weight)) / wspan
      edges$title <- paste0("Gemeinsam zitiert: ", edges$weight)

      set.seed(1337)
      return(
        visNetwork(nodes, edges) %>%
          addFontAwesome() %>%
          visGroups(groupname = "person", shape = "icon",
                    icon = list(code = "f007", size = 20, color = "#c34113")) %>%   # corpus author (orange)
          visGroups(groupname = "person_ext", shape = "icon",
                    icon = list(code = "f007", size = 20, color = "#999999")) %>%   # cited, not in corpus (grey)
          visIgraphLayout(layout = "layout_with_fr") %>%
          visEdges(color = list(color = "#d0d0d0", highlight = "#777777"), smooth = FALSE) %>%
          visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
          visEvents(click = "function(nodes) {
                      if(nodes.nodes.length > 0) {
                        Shiny.onInputChange('cocit_clicked_node', nodes.nodes[0]);
                      } else {
                        Shiny.onInputChange('cocit_clicked_node', null);
                      }
                    }")
      )
    }

    # ---- Author-to-author citation (directed; nodes = authors) ----
    if (mode == "author") {
      validate(need(direct_cit_available,
                    "Autor:innen-Zitationsdaten sind nicht verfügbar (citation_edges_direct.csv – ein Re-Harvest mit OpenAlex-W-IDs ist nötig)."))
      g <- author_citation_graph(unique(filtered_pubs()$id), cap = cap)
      validate(need(!is.null(g) && nrow(g$edges) > 0,
                    "Für diese Auswahl gibt es keine korpusinternen Zitationen zwischen Autor:innen. Bitte erweitern Sie die Filter."))
      ids <- g$node_ids
      r <- complete_researchers_PA_latest[match(ids, complete_researchers_PA_latest$orcid), ]
      label <- ifelse(is.na(r$name_value), ids, r$name_value)
      nodes <- data.frame(id = ids, label = label,
                          title = htmltools::htmlEscape(label),
                          group = "person", stringsAsFactors = FALSE)
      wspan <- max(1, max(g$edges$weight) - min(g$edges$weight))
      edges <- data.frame(from = g$edges$from, to = g$edges$to,
                          width = 1 + 4 * (g$edges$weight - min(g$edges$weight)) / wspan,
                          title = paste0("Zitationen: ", g$edges$weight),
                          stringsAsFactors = FALSE)

      set.seed(1337)
      return(
        visNetwork(nodes, edges) %>%
          addFontAwesome() %>%
          visGroups(groupname = "person", shape = "icon",
                    icon = list(code = "f007", size = 25, color = "#c34113")) %>%
          visIgraphLayout(layout = "layout_with_fr") %>%
          visEdges(color = list(color = "#d0d0d0", highlight = "#777777"), smooth = FALSE,
                   arrows = list(to = list(enabled = TRUE, scaleFactor = 0.6))) %>%
          visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
          visEvents(click = "function(nodes) {
                      if(nodes.nodes.length > 0) {
                        Shiny.onInputChange('cocit_clicked_node', nodes.nodes[0]);
                      } else {
                        Shiny.onInputChange('cocit_clicked_node', null);
                      }
                    }")
      )
    }

    # ---- Direct intra-corpus citation (directed; nodes = corpus papers) ----
    if (mode == "direct") {
      validate(need(direct_cit_available,
                    "Direkte Zitationsdaten sind nicht verfügbar (citation_edges_direct.csv – ein Re-Harvest mit OpenAlex-W-IDs ist nötig)."))
      g <- direct_citation_graph(unique(filtered_pubs()$id), cap = cap)
      validate(need(!is.null(g) && nrow(g$edges) > 0,
                    "Für diese Auswahl gibt es keine korpusinternen Zitationen. Bitte erweitern Sie die Filter."))
      ids <- g$node_ids
      w <- complete_works_PA[match(ids, complete_works_PA$id), ]
      nodes <- data.frame(
        id = ids,
        label = ifelse(is.na(w$label), ids, w$label),
        title = htmltools::htmlEscape(ifelse(is.na(w$title_title_value), ids, w$title_title_value)),
        group = "publication", stringsAsFactors = FALSE)
      edges <- data.frame(from = g$edges$from_id, to = g$edges$to_id,
                          stringsAsFactors = FALSE)

      set.seed(1337)
      return(
        visNetwork(nodes, edges) %>%
          addFontAwesome() %>%
          visGroups(groupname = "publication", shape = "icon",
                    icon = list(code = "f1ea", size = 15, color = "#428bca")) %>%
          visIgraphLayout(layout = "layout_with_fr") %>%
          visEdges(color = list(color = "#d0d0d0", highlight = "#777777"), smooth = FALSE,
                   arrows = list(to = list(enabled = TRUE, scaleFactor = 0.6))) %>%
          visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
          visEvents(click = "function(nodes) {
                      if(nodes.nodes.length > 0) {
                        Shiny.onInputChange('cocit_clicked_node', nodes.nodes[0]);
                      } else {
                        Shiny.onInputChange('cocit_clicked_node', null);
                      }
                    }")
      )
    }

    # ---- Co-citation / bibliographic coupling (shared sparse-matrix engine) ----
    validate(need(cocit_available,
                  "Zitationsdaten sind nicht verfügbar (references_edges.csv fehlt)."))
    mw <- if (is.null(input$cocit_min_weight)) 2 else input$cocit_min_weight
    proj <- citation_projection(unique(filtered_pubs()$id), mode = mode,
                                min_weight = mw, cap = cap)
    validate(need(!is.null(proj) && nrow(proj$edges) > 0,
                  "Für diese Auswahl ergibt sich kein Zitationsnetzwerk. Bitte erweitern Sie die Filter oder senken Sie die Mindest-Verknüpfungsstärke."))

    ids <- proj$node_ids
    if (mode == "cocit") {
      m <- references_meta[match(ids, references_meta$ref_id), ]
      ttl   <- ifelse(is.na(m$ref_title), ids, m$ref_title)
      # A cited reference that is itself a corpus publication (its DOI is in the
      # dataset) is shown blue like a corpus pub; the rest — cited but NOT part
      # of the north-German dataset — are grey "external" references.
      ref_doi_bare <- doi_bare(ifelse(is.na(m$ref_doi), "", m$ref_doi))
      in_corpus <- nzchar(ref_doi_bare) & tolower(ref_doi_bare) %in% tolower(complete_works_PA$id)
      grp   <- ifelse(in_corpus, "publication", "reference")
      # Only label the internal (corpus) publications; external references stay
      # unlabelled to keep the graph readable (their title still shows on hover).
      label <- ifelse(in_corpus, ifelse(is.na(m$ref_label), ids, m$ref_label), "")
    } else {
      w <- complete_works_PA[match(ids, complete_works_PA$id), ]
      label <- ifelse(is.na(w$label), ids, w$label)
      ttl   <- ifelse(is.na(w$title_title_value), ids, w$title_title_value)
      grp   <- "publication"
    }
    nodes <- data.frame(id = ids, label = label,
                        title = htmltools::htmlEscape(ttl),
                        group = grp, stringsAsFactors = FALSE)

    edges <- proj$edges
    wspan <- max(1, max(edges$weight) - min(edges$weight))
    edges$width <- 1 + 4 * (edges$weight - min(edges$weight)) / wspan
    edges$title <- paste0(
      if (mode == "cocit") "Ko-Zitationen: " else "Geteilte Referenzen: ",
      edges$weight)

    set.seed(1337)
    visNetwork(nodes, edges) %>%
      addFontAwesome() %>%
      visGroups(groupname = "publication", shape = "icon",
                icon = list(code = "f1ea", size = 15, color = "#428bca")) %>%   # corpus pub (blue)
      visGroups(groupname = "reference",   shape = "icon",
                icon = list(code = "f1ea", size = 15, color = "#999999")) %>%   # cited, not in dataset (grey)
      visIgraphLayout(layout = "layout_with_fr") %>%
      visEdges(color = list(color = "#d0d0d0", highlight = "#777777"), smooth = FALSE) %>%
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, hover = TRUE)) %>%
      visEvents(click = "function(nodes) {
                  if(nodes.nodes.length > 0) {
                    Shiny.onInputChange('cocit_clicked_node', nodes.nodes[0]);
                  } else {
                    Shiny.onInputChange('cocit_clicked_node', null);
                  }
                }")
  })

  output$dynamic_ui_cocit <- renderUI({
    id <- input$cocit_clicked_node
    mode <- if (is.null(input$cocit_mode)) "author" else input$cocit_mode
    if (is.null(id) || identical(id, "")) {
      return(h4("Klicken Sie auf einen Knoten für weiterführende Informationen."))
    }

    # Author co-citation nodes (id = cited author name) → cited-author infobox.
    if (mode == "author_cocit") {
      return(author_cocit_infobox(id))
    }
    # Author direct-citation nodes (id = ORCiD) → author infobox.
    if (mode == "author") {
      return(author_infobox(id))
    }

    # Resolve the clicked node to a corpus publication id (DOI) if it is one:
    # coupling/direct nodes already are; a cocit reference is iff its DOI is in
    # the dataset.
    corpus_id <- NULL
    meta_row  <- NULL
    if (mode == "cocit") {
      meta_row <- references_meta[references_meta$ref_id == id, , drop = FALSE]
      if (nrow(meta_row) > 0) {
        d <- tolower(doi_bare(ifelse(is.na(meta_row$ref_doi[1]), "", meta_row$ref_doi[1])))
        if (nzchar(d)) {
          hit <- complete_works_PA$id[tolower(complete_works_PA$id) == d]
          if (length(hit) > 0) corpus_id <- hit[1]
        }
      }
    } else if (id %in% complete_works_PA$id) {
      corpus_id <- id
    }

    if (!is.null(corpus_id)) {
      # Corpus publication → full infobox (authors, journal, keywords, …).
      corpus_pub_infobox(corpus_id)
    } else if (mode == "cocit" && !is.null(meta_row) && nrow(meta_row) > 0) {
      # External reference: cited, but not part of the north-German dataset.
      m <- meta_row[1, ]
      doi_b <- doi_bare(ifelse(is.na(m$ref_doi), "", m$ref_doi))
      au <- m$ref_first_author
      has_au <- !is.na(au) && nzchar(au)
      # Only link the author if they are themselves a corpus researcher; a purely
      # external cited author has no person profile, so show plain text.
      au_corpus <- has_au && tolower(au) %in% tolower(complete_researchers_PA_latest$name_value)
      au_html <- if (au_corpus)
        as.character(tags$a(href = "#", class = "person-link", `data-name` = au, au))
      else if (has_au) sanitize_html(au) else NULL
      tagList(
        # First author (year). Linked only when the author is in the corpus.
        if (has_au) p(HTML(paste0(au_html, " (", m$ref_year, ")"))) else NULL,
        h4(HTML(sanitize_html(m$ref_title))),
        p(paste0(if (!has_au) paste0(m$ref_year, " · "), m$ref_cited_by, "× zitiert (OpenAlex)")),
        if (nzchar(doi_b)) {
          p(HTML("<b>DOI</b>: "),
            tags$a(href = paste0("https://doi.org/", doi_b), target = "_blank", doi_b))
        } else NULL,
        p(tags$em("Zitierte Referenz – nicht Teil des norddeutschen Datensatzes."))
      )
    } else {
      # cocit node whose metadata has not been fetched yet (pre-§21 run).
      tagList(
        h4(id),
        p(tags$em("Metadaten zu dieser Referenz werden nach dem nächsten Daten-Update geladen."))
      )
    }
  })

  # ============================================================================
  # Accessibility: dynamic text alternatives (alt text) for the visualizations.
  # Each output$<id>_desc fills the visually-hidden caption created by alt_desc()
  # from the SAME cached reactive data the plot uses, so screen-reader users get a
  # data-driven summary that updates with the filters — without altering any plot.
  # (Outputs are suspended while their tab is hidden, so they add no overhead.)
  # ============================================================================

  output$sfPlot_desc <- renderText({
    tryCatch({
      pts  <- plotted_points_var()
      ctr  <- plotted_points_countries_var()
      npub <- tryCatch(nrow(filtered_pubs_summed()), error = function(e) 0L)
      n_places <- if (is.null(pts)) 0L else nrow(pts)
      n_ctr    <- if (is.null(ctr)) 0L else nrow(ctr)
      if (n_places == 0L && n_ctr == 0L)
        return("Für die aktuelle Auswahl sind keine Orte verortet.")
      parts <- sprintf("Weltkarte der in %s gefilterten Publikationen genannten Orte der Region Asien-Pazifik.",
                       formatC(npub, format = "d", big.mark = ".", decimal.mark = ","))
      if (n_places > 0L)
        parts <- c(parts, sprintf("%d Orte als Punkte (Punktgröße = Häufigkeit); am häufigsten genannt: %s.",
                                  n_places, fmt_top(str_to_title(pts$text), pts$n)))
      if (n_ctr > 0L)
        parts <- c(parts, sprintf("%d Länder flächig nach Nennungshäufigkeit eingefärbt; am häufigsten: %s.",
                                  n_ctr, fmt_top(ctr$NAME, ctr$n)))
      paste(parts, collapse = " ")
    }, error = function(e) "Interaktive Karte der in den Publikationen genannten Orte.")
  })

  output$mynetworkid_desc <- renderText({
    tryCatch({
      nodes <- network_nodes_shared()
      edges <- network_edges_shared()
      if (is.null(nodes) || nrow(nodes) == 0L)
        return("Für die aktuelle Auswahl liegt kein Netzwerk vor.")
      g  <- table(nodes$group)
      gv <- function(x) if (x %in% names(g)) as.integer(g[[x]]) else 0L
      sprintf(paste0("Netzwerkgraph mit %d Knoten und %d Verbindungen: %d Personen, %d Publikationen, ",
                     "%d Institutionen, %d Fachbereiche, %d Fördermittelgeber. Kanten verbinden Personen mit ",
                     "ihren Publikationen, Einrichtungen und Förderungen. Diese Angaben sind auch in den Tabellen ",
                     "„Personen“, „Publikationen“ und „Förderungen“ abrufbar."),
              nrow(nodes), if (is.null(edges)) 0L else nrow(edges),
              gv("person"), gv("publication"), gv("institution"), gv("institution_sm"), gv("funding"))
    }, error = function(e) "Interaktiver Netzwerkgraph aus Personen, Publikationen, Einrichtungen und Fördermittelgebern.")
  })

  output$cocitNetwork_desc <- renderText({
    tryCatch({
      pubs <- filtered_pubs()
      npub <- if (is.null(pubs)) 0L else length(unique(pubs$id))
      mode <- if (is.null(input$cocit_mode)) "author" else input$cocit_mode
      lab <- switch(mode,
        author_cocit = "Ko-Zitation der Autor:innen – wer wird gemeinsam zitiert",
        cocit        = "Ko-Zitation der Referenzen – welche zitierten Werke treten gemeinsam auf",
        author       = "gerichtete Zitationen zwischen Autor:innen",
        direct       = "gerichtete Zitationen zwischen Publikationen",
        coupling     = "bibliografische Kopplung – Publikationen mit gemeinsamen Referenzen",
        mode)
      knoten <- if (mode %in% c("direct", "coupling")) "Publikationen"
                else if (mode == "cocit") "zitierte Werke" else "Autor:innen"
      sprintf(paste0("Interaktives Zitationsnetzwerk, Ansicht „%s“, auf Basis von %d gefilterten Publikationen. ",
                     "Knoten sind %s; dickere Kanten bedeuten häufigere gemeinsame bzw. gerichtete Zitationen. ",
                     "Die zugrunde liegenden Publikationen sind in der Publikationen-Tabelle einsehbar."),
              lab, npub, knoten)
    }, error = function(e) "Interaktives Zitationsnetzwerk der gefilterten Publikationen.")
  })

  output$foerderRegionPie_desc <- renderText({
    tryCatch({
      fp <- filtered_prizes()
      if (is.null(fp) || nrow(fp) == 0L)
        return("Keine Förderungen in der aktuellen Auswahl.")
      rc <- fp %>% distinct(id, organization) %>%
        left_join(funder_countries, by = "organization") %>%
        filter(!is.na(country) & nzchar(trimws(country))) %>%
        distinct(id, country) %>% count(country, name = "n") %>% arrange(desc(n))
      if (nrow(rc) == 0L)
        return("Für die geförderten Publikationen ist kein Herkunftsland der Mittelgeber bekannt.")
      rc$region <- ifelse(rc$country %in% names(country_de_names),
                          country_de_names[rc$country], rc$country)
      sprintf("Kreisdiagramm der Herkunftsländer der Fördermittelgeber: %d Länder, %d geförderte Publikationen. Größte Anteile: %s.",
              nrow(rc), length(unique(fp$id)), fmt_top(rc$region, rc$n))
    }, error = function(e) "Kreisdiagramm der Herkunftsländer der Fördermittelgeber.")
  })

  output$barChart_standort_desc <- renderText({
    tryCatch({
      base <- filtered_complete_researchers_PA_latest()
      if (is.null(base) || nrow(base) == 0L)
        return("Keine Forschenden in der aktuellen Auswahl.")
      rc <- base %>%
        mutate(city = ifelse(is.na(organization_address_city) |
                               !organization_address_city %in% unlist(city_mapping, use.names = FALSE),
                             "außerhalb", organization_address_city)) %>%
        count(city) %>% arrange(desc(n))
      sprintf("Balkendiagramm der Anzahl Forschender je letztdokumentiertem Standort (%d Forschende gesamt): %s.",
              sum(rc$n), fmt_top(rc$city, rc$n, k = 8))
    }, error = function(e) "Balkendiagramm der Anzahl Forschender je Standort.")
  })

  output$barChart_disziplin_desc <- renderText({
    tryCatch({
      pubs <- filtered_pubs()
      if (is.null(pubs) || nrow(pubs) == 0L)
        return("Keine Publikationen in der aktuellen Auswahl.")
      disz <- complete_researchers_PA %>%
        filter(orcid %in% pubs$orcid) %>%
        filter(faculties %in% filtered_faculties()) %>%
        left_join(faculties_mapping_tbl, by = c("faculties" = "faculty_code")) %>%
        select(faculty_label, orcid) %>% unique()
      pc <- pubs %>% select(orcid, id) %>%
        left_join(disz, by = "orcid", relationship = "many-to-many") %>%
        select(faculty_label, id) %>% unique() %>%
        filter(!is.na(faculty_label)) %>% count(faculty_label) %>% arrange(desc(n))
      sprintf("Balkendiagramm der Anzahl Publikationen je Fachrichtung. Größte Fachrichtungen: %s.",
              fmt_top(pc$faculty_label, pc$n))
    }, error = function(e) "Balkendiagramm der Anzahl Publikationen je Fachrichtung.")
  })

  output$barChart_region_desc <- renderText({
    tryCatch({
      pubs <- filtered_pubs()
      if (is.null(pubs) || nrow(pubs) == 0L)
        return("Keine Publikationen in der aktuellen Auswahl.")
      ua <- complete_spacy_PA_geo %>% st_drop_geometry() %>% select(id, ADMIN) %>% unique()
      pc <- pubs %>% select(id) %>%
        left_join(ua, by = "id", relationship = "many-to-many") %>%
        filter(!is.na(ADMIN)) %>% unique() %>% count(ADMIN) %>% arrange(desc(n))
      sprintf("Balkendiagramm der Anzahl Publikationen je Forschungsregion: %s.",
              fmt_top(pc$ADMIN, pc$n))
    }, error = function(e) "Balkendiagramm der Anzahl Publikationen je Forschungsregion.")
  })

  output$barChart_zeit_desc <- renderText({
    tryCatch({
      pubs <- filtered_pubs()
      if (is.null(pubs) || nrow(pubs) == 0L)
        return("Keine Publikationen in der aktuellen Auswahl.")
      pubs <- pubs %>% group_by(id, publication_date_year_value) %>% slice(1) %>% ungroup()
      yc <- table(pubs$publication_date_year_value)
      if (length(yc) == 0L) return("Keine Publikationen in der aktuellen Auswahl.")
      yrs <- as.numeric(names(yc)); cnt <- as.numeric(yc); peak <- which.max(cnt)
      sprintf(paste0("Liniendiagramm der Publikationen pro Jahr von %d bis %d, insgesamt %d Publikationen. ",
                     "Höchststand %d mit %d Publikationen. Eine gestrichelte Linie zeigt zusätzlich den Anteil ",
                     "am gesamten norddeutschen Publikationsaufkommen."),
              min(yrs), max(yrs), sum(cnt), yrs[peak], cnt[peak])
    }, error = function(e) "Liniendiagramm der Publikationen pro Jahr.")
  })

  output$barChart_koop_desc <- renderText({
    tryCatch({
      pubs <- filtered_pubs()
      if (is.null(pubs) || nrow(pubs) == 0L)
        return("Keine Kooperationsländer in der aktuellen Auswahl.")
      fc <- coop_countries %>% filter(id %in% pubs$id) %>% mutate(n = as.numeric(n))
      if (nrow(fc) == 0L) return("Keine Kooperationsländer in der aktuellen Auswahl.")
      tot <- fc %>% group_by(authorships_affiliations_country_code) %>%
        summarise(total = sum(n, na.rm = TRUE), .groups = "drop") %>% arrange(desc(total))
      sprintf(paste0("Liniendiagramm der Kooperationsländer (Institutionen der Ko-Autor:innen) im Zeitverlauf; ",
                     "dargestellt sind die obersten 5 %% der Länder. Häufigste Länder insgesamt: %s."),
              fmt_top(tot$authorships_affiliations_country_code, tot$total))
    }, error = function(e) "Liniendiagramm der Kooperationsländer der Ko-Autor:innen im Zeitverlauf.")
  })

  output$wordCloud_desc <- renderText({
    tryCatch({
      dt <- filtered_pubs_summed()
      if (is.null(dt) || nrow(dt) == 0L)
        return("Keine Schlagwörter in der aktuellen Auswahl.")
      paste0("Wortwolke der häufigsten in Titeln und Abstracts erkannten Schlagwörter; die Schriftgröße ",
             "entspricht der Häufigkeit. Dieselben Begriffe stehen mit genauen Häufigkeiten in der Tabelle ",
             "„Begriffshäufungen“ darüber.")
    }, error = function(e) "Wortwolke der häufigsten Schlagwörter der gefilterten Publikationen.")
  })

}

# ==============================================================================
# RUN APPLICATION
# ==============================================================================
shinyApp(ui = ui, server = server)
