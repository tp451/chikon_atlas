############################################################
#  Atlas der Ostasien-Forschung in Norddeutschland         #
#  Mining von ORCiD, Crossref, & OpenAlex                  #
#  Thorben Pelzer 2025                                     #
#  CC BY-SA 4.0                                            #
############################################################
#
# Purpose:
#   Mine, merge, and clean Asia-related publication data
#   authored by scholars affiliated with Northern German
#   institutions. Data sources: OpenAlex, ORCID, Crossref.
#
# Inputs (external CSVs / GeoJSONs):
#   - db_sf_filter.csv        : entity strings to exclude
#   - universities.csv        : org-name harmonisation table
#   - departments.csv         : dept-name harmonisation table
#   - faculties.csv           : faculty classification table
#   - faculties_ollama.csv    : cached LLM faculty predictions (auto-generated)
#   - org_merges_ollama.csv   : cached LLM same-org verdicts (auto-generated, Section 16a)
#   - org_hierarchy_ollama.csv: cached LLM parent/child verdicts (auto-generated, Section 16b)
#   - sf_countries_PA.geojson : country polygons (Pacific Asia)
#   - db_sf_countries.csv     : country names for sf_filter
#
# Outputs:
#   - complete_works_PA.csv              : final publications
#   - complete_researchers_PA.csv        : researcher records
#   - complete_researchers_PA_latest.csv : latest affiliation
#   - complete_spacy_PA_keywords.csv     : NLP-extracted entities
#   - complete_spacy_PA_geo.geojson      : geocoded entities
#   - complete_funding_PA.csv            : funding information
#   - years_normal.csv                   : pub counts by year
#   - counted_coop_countries.csv         : co-author countries
#
# Workflow:
#   1. Configure  -->  2. Load libraries  -->  3. Define keywords
#   4. Load external dicts  -->  5. Define helpers
#   6. OpenAlex: fetch institutions & publications
#   7. OpenAlex: regex keyword matching
#   8. OpenAlex: NLP entity extraction (spaCy)
#   9. OpenAlex: geocode entities
#  10. OpenAlex: join regex + NLP results
#  11. OpenAlex: author & researcher processing
#  12. ORCID: mine researchers & employments
#  13. ORCID: mine works
#  14. ORCID/Crossref: fetch abstracts & NLP
#  15. ORCID: regex + NLP matching
#  16. Merge: OpenAlex + ORCID datasets
# 16a. Auto-detect same-org variants (LLM-verified)
# 16b. Auto-detect organisation hierarchy (LLM-verified, multi-level)
#  17. Data cleaning: researchers
#  18. Data cleaning: publications
#  19. Data cleaning: keywords & geodata
#  20. Final outputs & exports
#
# Dependencies:
#   tidyverse, anytime, lubridate, janitor, usethis, pbapply,
#   stringr, stringi, stringdist, xml2, htmltools, spacyr, cld3,
#   openalexR, rcrossref, rorcid, synthesisr, sf, tidygeocoder,
#   httr2, jsonlite
#
# Usage:
#   Source the script section-by-section, using the
#   checkpoint .rds/.csv files to restart after expensive
#   API operations. Each section reads its own checkpoint
#   at the top and writes one at the bottom.
############################################################


# ══════════════════════════════════════════════
# SECTION 1: Configuration
# ──────────────────────────────────────────────
# Purpose: Centralise all configurable parameters
#          (API credentials, paths, thresholds).
# ══════════════════════════════════════════════

config <- list(
  openalex_email   = Sys.getenv("OPENALEX_EMAIL", ""),
  openalex_apikey  = Sys.getenv("OPENALEX_APIKEY", ""),
  # spaCy Python: managed by spacyr in its own "r-spacyr" virtualenv (provision
  # it with setup_python.R). To use a different Python, set the RETICULATE_PYTHON
  # env var (spacyr honours it); SPACY_PYTHON overrides just the virtualenv name.
  year_range       = 2000:2025,
  bbox             = c(xmin = 80, xmax = 150, ymin = 0, ymax = 50),
  api_retry_tries  = 3,
  api_retry_wait   = 30,
  output_dir       = ".",
  # Ollama LLM faculty classifier
  ollama_url       = "http://localhost:11434/api/generate",
  ollama_model     = "qwen3.5:9b",
  ollama_cache     = "faculties_ollama.csv",
  # Org auto-merging (Section 16a) and hierarchy detection (Section 16b)
  org_fuzzy_threshold   = 0.85,
  org_place_tokens      = c("Bremen", "Clausthal", "Clausthal-Zellerfeld",
                            "Flensburg", "Greifswald", "Hamburg", "Kiel",
                            "Lübeck", "Luebeck", "Lüneburg", "Lueneburg",
                            "Oldenburg", "Rostock"),
  org_merge_cache       = "org_merges_ollama.csv",
  org_hierarchy_cache   = "org_hierarchy_ollama.csv",
  org_llm_batch_size    = 20L,
  # City patterns shared by merge + hierarchy (lowercased, used for skip-list)
  org_stopwords         = c("of", "the", "der", "die", "das", "den", "des",
                            "zu", "in", "im", "am", "auf", "and", "und",
                            "for", "für", "fur", "an", "von", "vom", "to")
)

options(openalexR.mailto = config$openalex_email)
if (nzchar(config$openalex_apikey)) {
  options(openalexR.apikey = config$openalex_apikey)
} else {
  warning("No OpenAlex API key set. Since Feb 2026, an API key is required. ",
          "Get one free at https://openalex.org/settings/api and set OPENALEX_APIKEY env var.")
}

# ── spaCy Python (Windows DLL fix) ─────────────────────────────────────────
# spacyr loads spaCy from its "r-spacyr" virtualenv. On Windows that venv is
# layered on the r-miniconda base, and reticulate does NOT activate conda's
# DLL directories for a *virtualenv*, so python3xx.dll's conda dependencies
# fail to resolve — a failed native load that aborts the R session with a
# 0xC0000005 access violation (it surfaces on whatever line is running, often
# the first read_csv). Prepending the conda DLL dirs to PATH here, before spaCy
# is ever initialised (Sections 8, 14), makes those dependencies resolvable.
# Harmless if the venv is not conda-based: the dirs simply won't exist.
if (.Platform$OS.type == "windows") {
  conda_base <- tryCatch(reticulate::miniconda_path(), error = function(e) "")
  if (nzchar(conda_base) && dir.exists(conda_base)) {
    dll_dirs <- file.path(conda_base, c(".", "Library/bin", "Library/mingw-w64/bin",
                                        "Library/usr/bin", "DLLs", "bin", "Scripts"))
    dll_dirs <- dll_dirs[dir.exists(dll_dirs)]
    if (length(dll_dirs)) {
      dll_dirs <- normalizePath(dll_dirs, winslash = "\\", mustWork = FALSE)
      Sys.setenv(PATH = paste(c(dll_dirs, Sys.getenv("PATH")), collapse = ";"))
    }
  }
}


# ══════════════════════════════════════════════
# SECTION 2: Library Loading
# ──────────────────────────────────────────────
# Purpose: Load all required R packages.
# ══════════════════════════════════════════════

# --- Core tidyverse tools for data manipulation and visualization ---
library(tidyverse)    # Includes ggplot2, dplyr, tidyr, readr, etc.

# --- Date and time handling ---
library(anytime)      # Convert various date/time formats easily
library(lubridate)    # Tools for working with dates and times

# --- Data cleaning and utility packages ---
library(janitor)      # Clean column names and data frames
library(usethis)      # Workflow tools for package and project setup
library(pbapply)      # Progress bar support for apply functions

# --- Text and string processing ---
library(stringr)      # Consistent wrappers for string operations
library(stringi)      # Comprehensive string processing (Unicode aware)
library(stringdist)   # String distance/similarity (Jaro-Winkler, used in Section 16a)
library(xml2)         # Work with XML (for unescaping)
library(htmltools)    # Work with HTML (for unescaping)
library(spacyr)       # Interface to 'spaCy' for NLP tasks
library(cld3)         # Language detection

# --- Bibliographic and scholarly metadata retrieval ---
library(openalexR)    # Interface to the OpenAlex scholarly database
library(rcrossref)    # Access to Crossref publication metadata
library(rorcid)       # Interface to ORCID researcher metadata

# --- Literature review and synthesis tools ---
library(synthesisr)   # Functions for systematic reviewing and search data handling

# --- HTTP and JSON (for Ollama LLM faculty classification) ---
library(httr2)        # Modern HTTP client for API calls
library(jsonlite)     # JSON parsing

# --- Geospatial tools ---
library(sf)           # Simple features for spatial vector data
library(tidygeocoder) # Geocoding using various APIs


# ══════════════════════════════════════════════
# SECTION 3: Keyword Dictionaries
# ──────────────────────────────────────────────
# Purpose: Define country/region keyword vectors used
#          for regex matching of publication titles.
#          unique() is applied when building the combined
#          vector to guard against accidental duplicates.
# ══════════════════════════════════════════════

institutions <- c("Kiel",
                  "Flensburg",
                  "Clausthal",
                  "Clausthal-Zellerfeld",
                  "Bremen",
                  "Greifswald",
                  "Hamburg",
                  "Lübeck",
                  "Lüneburg",
                  "Oldenburg",
                  "Rostock")

keywords_china <- c("China", "Chines", "Sino-", "sinolog",
                    "Chongqing", "Shanghai", "Beijing", "Chengdu",
                    "Guangzhou", "Shenzhen", "Tianjin", "Wuhan",
                    "Xi'an", "Hangzhou", "Dongguan", "Foshan",
                    "Nanjing", "Shenyang", "Jinan", "Qingdao",
                    "Harbin", "Zhengzhou", "Changsha", "Kunming",
                    "Dalian", "Changchun", "Xiamen", "Ningbo",
                    "Taiyuan", "Zhongshan", "Ürümqi", "Suzhou",
                    "Shantou", "Hefei", "Shijiazhuang", "Fuzhou",
                    "Nanning", "Wenzhou", "Changzhou", "Nanchang",
                    "Guiyang", "Tangshan", "Wuxi", "Lanzhou",
                    "Handan", "Hohhot", "Weifang", "Jiangmen",
                    "Zibo", "Linyi", "Nantong", "Huizhou",
                    "Zhuhai", "Luoyang",
                    "Anhui", "Fujian", "Gansu", "Guangdong",
                    "Guangxi", "Guizhou", "Hainan", "Hebei", "Heilongjiang", "Henan",
                    "Hubei", "Hunan", "Jiangsu", "Jiangxi", "Jilin", "Liaoning",
                    "Inner Mongolia", "Ningxia", "Qinghai", "Shaanxi",
                    "Shandong", "Shanxi", "Sichuan", "Tibet",
                    "Xinjiang", "Yunnan", "Zhejiang", "Hong Kong", "Hongkong", "Macau",
                    "Chongqing")

keywords_japan <- c("Japan", "Japanes", "Japanis", "Japano", "Nippon", "Nihon",
                    "Tokyo", "Osaka", "Nagoya", "Sapporo",
                    "Fukuoka", "Kobe", "Kyoto", "Yokohama",
                    "Hiroshima", "Sendai", "Chiba", "Kitakyushu",
                    "Kawasaki", "Saitama", "Niigata", "Shizuoka",
                    "Hamamatsu", "Okayama", "Hachioji", "Utsunomiya",
                    "Naha", "Matsuyama", "Kanazawa", "Fukushima",
                    "Gifu", "Toyama", "Nagasaki", "Kumamoto",
                    "Oita", "Matsumoto", "Fukui", "Kagoshima",
                    "Akita", "Yamagata", "Nagano",
                    "Wakayama", "Miyagi", "Ishikawa",
                    "Tohoku", "Hokkaido", "Kansai", "Chugoku",
                    "Kyushu", "Okinawa", "Chubu", "Shikoku",
                    "Niihama", "Tottori", "Takayama", "Himeji",
                    "Takamatsu", "Kochi",
                    "Ibaraki", "Gunma", "Tochigi",
                    "Yamanashi", "Ehime", "Kagawa", "Shimane")

keywords_korea <- c("Korea", "Choson", "Hanguk",
                    "Seoul", "Busan", "Incheon", "Daegu",
                    "Daejeon", "Gwangju", "Suwon", "Ulsan",
                    "Seongnam", "Goyang", "Yongin", "Changwon",
                    "Cheongju", "Jeonju", "Cheonan", "Hwaseong",
                    "Gimhae", "Pyeongtaek", "Pohang", "Jinju",
                    "Gwangmyeong", "Guri", "Anyang", "Uijeongbu",
                    "Bucheon", "Gangneung", "Chuncheon", "Jeju",
                    "Gyeongju", "Samcheok", "Mokpo", "Yeosu",
                    "Suncheon", "Ansan", "Paju", "Gimpo",
                    "Wonju", "Geoje", "Yangsan", "Gwangyang",
                    "Jeolla", "Gyeonggi", "Chungcheong",
                    "Gangwon", "Gyeongsang", "Sejong")

keywords_taiwan <- c("Taiwan", "Taiwanes", "Taiwanis", "Taipei", "Taichung", "Tainan",
                     "Kaohsiung", "Hsinchu", "Keelung",
                     "Chiayi", "Pingtung", "Taoyuan", "Changhua",
                     "Yilan", "Miaoli", "Nantou", "Taitung",
                     "Hualien", "Penghu", "Kinmen", "Matsu",
                     "New Taipei", "Zhubei", "Douliu",
                     "Hengchun", "Alishan", "Yunlin",
                     "Lanyu", "Orchid Island", "Green Island")

keywords_india <- c("India", "Indisch", "Bharat", "Hindustan",
                    "Kolkata", "Patna", "Ranchi", "Raipur", "Guwahati", "Bhubaneswar",
                    "Sikkim", "Manipur", "Mizoram", "Nagaland", "Meghalaya", "Tripura", "Assam",
                    "West Bengal", "Bihar", "Odisha", "Chhattisgarh", "Jharkhand")

keywords_vietnam <- c("Vietnam", "Viet Nam", "Vietnames",
                      "Hanoi", "Ho Chi Minh", "Saigon", "Da Nang",
                      "Hai Phong", "Can Tho", "Bien Hoa", "Hue",
                      "Nha Trang", "Vung Tau", "Quy Nhon", "Ha Long",
                      "Thanh Hoa", "Nam Dinh", "Thai Binh", "Buon Ma Thuot",
                      "Soc Trang", "Vinh", "Long Xuyen", "Bac Ninh",
                      "Quang Ninh", "Dong Nai", "Binh Duong", "An Giang",
                      "Da Lat", "Tay Ninh", "Phan Thiet", "Binh Thuan",
                      "Dak Lak", "Quang Ngai", "Lao Cai", "Cao Bang",
                      "Ha Tinh", "Nghe An")

keywords_burma <- c("Burma", "Myanmar", "Burmes",
                    "Yangon", "Rangoon", "Naypyidaw", "Mandalay",
                    "Mawlamyine", "Taunggyi", "Pathein",
                    "Monywa", "Sittwe", "Myitkyina", "Meiktila",
                    "Hpa-An", "Pyay", "Magway", "Sagaing",
                    "Shan State", "Kachin", "Rakhine",
                    "Mon State", "Kayin", "Kayah", "Ayeyarwady",
                    "Tanintharyi", "Bamar")

keywords_laos <- c("Laos", "Lao", "Laoti",
                   "Vientiane", "Luang Prabang", "Savannakhet",
                   "Pakse", "Thakhek", "Phonsavan", "Muang Xai",
                   "Sam Neua", "Champasak", "Xayaboury", "Attapeu",
                   "Bolikhamsai", "Khammouane", "Sekong", "Saravane",
                   "Oudomxay", "Luang Namtha", "Xieng Khouang")

keywords_kyrgyzstan <- c("Kyrgyzstan", "Kyrgyz", "Kirgiz", "Kyrgyz Republic",
                         "Bishkek", "Jalal-Abad", "Karakol",
                         "Tokmok", "Talas", "Naryn", "Batken",
                         "Issyk-Kul", "Chuy", "Cholpon-Ata", "Kochkor", "Kyzyl-Kiya")

keywords_bangladesh <- c("Bangladesh", "Dhaka",
                         "Chittagong", "Khulna", "Rajshahi",
                         "Sylhet", "Barisal", "Rangpur", "Mymensingh",
                         "Narayanganj", "Gazipur", "Comilla", "Bogra",
                         "Jessore", "Dinajpur", "Pabna", "Tangail",
                         "Noakhali",
                         "Brahmanbaria", "Kushtia", "Narsingdi",
                         "Barishal")

keywords_nepal <- c("Nepal", "Kathmandu", "Lalitpur",
                    "Bhaktapur", "Pokhara", "Biratnagar", "Birgunj",
                    "Butwal", "Hetauda", "Janakpur", "Dharan",
                    "Itahari", "Nepalgunj", "Dhangadhi", "Bharatpur",
                    "Lumbini", "Chitwan", "Bagmati", "Gandaki",
                    "Koshi", "Karnali", "Sudurpashchim", "Province 1",
                    "Madhesh", "Rupandehi", "Dolakha", "Rasuwa",
                    "Sindhupalchok")

keywords_cambodia <- c("Cambodia", "Khmer",
                       "Phnom Penh", "Siem Reap", "Battambang",
                       "Sihanoukville", "Kampong Cham", "Kampot",
                       "Pursat", "Kampong Thom", "Kampong Chhnang",
                       "Prey Veng", "Svay Rieng", "Kratie", "Stung Treng",
                       "Ratanakiri", "Mondulkiri", "Pailin", "Banteay Meanchey",
                       "Oddar Meanchey", "Koh Kong")

keywords_philippines <- c("Filipin", "Philippin", "Pilipin",
                          "Pinoy", "Pinay",
                          "Manila", "Quezon", "Davao", "Zamboanga",
                          "Iloilo", "Bacolod", "Baguio", "Cagayan de Oro",
                          "General Santos", "Calamba", "Tacloban",
                          "Taguig", "Makati", "Mandaluyong",
                          "Las Piñas", "Parañaque", "Valenzuela", "Marikina",
                          "Pasay", "Muntinlupa", "Antipolo", "Tagaytay",
                          "Palawan", "Batangas", "Leyte")

keywords_sri_lanka <- c("Sri Lanka", "Ceylon",
                        "Colombo", "Kandy", "Jaffna",
                        "Negombo", "Batticaloa", "Trincomalee", "Anuradhapura",
                        "Kurunegala", "Ratnapura", "Matara", "Badulla",
                        "Nuwara Eliya", "Polonnaruwa", "Hambantota",
                        "Vavuniya", "Kalutara", "Gampaha", "Matale",
                        "Sabaragamuwa")

keywords_mongolia <- c("Mongolia", "Ulaanbaat", "Ulan Bat",
                       "Erdenet", "Darkhan", "Choibalsan", "Mörön",
                       "Ölgii", "Sükhbaatar", "Zuunmod",
                       "Kharkhorin", "Tsetserleg", "Khovd", "Bulgan",
                       "Dornod", "Dornogovi", "Khuvsgul", "Orkhon", "Selenge",
                       "Bayankhongor", "Arkhangai", "Zavkhan",
                       "Khentii", "Uvs", "Töv")

keywords_thailand <- c("Thai", "Bangkok",
                       "Chiang Mai", "Chiang Rai", "Phuket",
                       "Pattaya", "Hat Yai", "Nakhon Ratchasima",
                       "Udon Thani", "Khon Kaen", "Ubon Ratchathani",
                       "Surat Thani", "Nakhon Sawan", "Rayong",
                       "Lampang", "Nakhon Pathom", "Chonburi",
                       "Ayutthaya", "Samut Prakan", "Krabi", "Kanchanaburi",
                       "Phetchaburi", "Lopburi", "Mae Sot", "Sukhothai", "Saraburi")

keywords_malaysia <- c("Malay",
                       "Kuala Lumpur", "Putrajaya",
                       "Johor Bahru", "Ipoh", "Shah Alam", "Petaling Jaya",
                       "Kota Kinabalu", "Kuching", "Seremban", "Alor Setar",
                       "Melaka", "Malacca", "Butterworth",
                       "Sandakan", "Sibu", "Bintulu",
                       "Kuantan", "Langkawi", "Penang", "Selangor",
                       "Sabah", "Sarawak", "Pahang",
                       "Kelantan", "Terengganu", "Negeri Sembilan",
                       "Johor", "Kedah", "Labuan")

keywords_bhutan <- c("Bhutan", "Druk Yul",
                     "Thimphu", "Punakha", "Phuentsholing",
                     "Wangdue Phodrang", "Trongsa", "Mongar",
                     "Trashigang", "Samdrup Jongkhar", "Gelephu",
                     "Bumthang", "Zhemgang", "Dagana",
                     "Chukha", "Lhuentse", "Pemagatshel",
                     "Tsirang", "Trashi Yangtse")

keywords_general <- c("East Asia", "Southeast Asia", "Pacific Asia")

# Named list mapping countries to their keyword vectors (used for lookups)
keyword_country_map <- list(
  China       = keywords_china,
  Japan       = keywords_japan,
  Korea       = keywords_korea,
  Taiwan      = keywords_taiwan,
  India       = keywords_india,
  Vietnam     = keywords_vietnam,
  Myanmar     = keywords_burma,
  Laos        = keywords_laos,
  Kyrgyzstan  = keywords_kyrgyzstan,
  Bangladesh  = keywords_bangladesh,
  Nepal       = keywords_nepal,
  Cambodia    = keywords_cambodia,
  Philippines = keywords_philippines,
  `Sri Lanka` = keywords_sri_lanka,
  Mongolia    = keywords_mongolia,
  Thailand    = keywords_thailand,
  Malaysia    = keywords_malaysia,
  Bhutan      = keywords_bhutan,
  Allgemein   = keywords_general
)

# Combine and lowercase all keywords, removing duplicates
# The regex matches keyword stems followed by optional suffixes: e.g. "japan" matches "japanese"
combined_keywords_lower <- unique(tolower(c(
  keywords_china, keywords_taiwan, keywords_japan, keywords_korea,
  keywords_thailand, keywords_mongolia, keywords_sri_lanka,
  keywords_philippines, keywords_cambodia, keywords_nepal,
  keywords_bangladesh, keywords_kyrgyzstan, keywords_laos,
  keywords_burma, keywords_vietnam, keywords_india,
  keywords_malaysia, keywords_bhutan, keywords_general
)))

# Build regex: \b(keyword1|keyword2|...)[a-z]* to match stems + suffixes
combined_pattern <- str_c("\\b(", str_c(combined_keywords_lower, collapse = "|"), ")[a-z]*")


# ══════════════════════════════════════════════
# SECTION 4: External Dictionary Loading
# ──────────────────────────────────────────────
# Purpose: Load harmonisation tables and spatial
#          reference data used throughout the pipeline.
# ══════════════════════════════════════════════

# Validate that all required input files exist before loading
required_files <- c("db_sf_filter.csv", "universities.csv", "departments.csv",
                    "faculties.csv", "sf_countries_PA.geojson", "db_sf_countries.csv")
missing <- required_files[!file.exists(required_files)]
if (length(missing) > 0) {
  stop("Missing required input files in ", getwd(), ":\n  ",
       paste(missing, collapse = "\n  "),
       "\nSee documentation for required inputs.")
}

dict_not_locations <- read_csv("db_sf_filter.csv", show_col_types = F, col_names = F)

universities <- read_csv("universities.csv", show_col_types = F) %>% drop_na() %>% unique()
departments <- read_csv("departments.csv", show_col_types = F) %>% drop_na() %>% unique()
faculties <- read_csv("faculties.csv", show_col_types = F) %>%
  select(organization_name, department_name, faculties)

sf_countries <- read_sf("sf_countries_PA.geojson")

sf_filter_countries <- read_csv("db_sf_countries.csv", col_names = F, show_col_types = F)
sf_filter <- read_csv("db_sf_filter.csv", col_names = F, show_col_types = F) %>%
  filter(!X1 %in% sf_filter_countries$X1)


# ══════════════════════════════════════════════
# SECTION 5: Helper Function Definitions
# ──────────────────────────────────────────────
# Purpose: Reusable functions for keyword matching,
#          NLP entity extraction, HTML unescaping,
#          and API data retrieval.
# ══════════════════════════════════════════════

#' Remove false-positive keyword matches from text
#'
#' Strips known false-positive terms (e.g. "kawasaki disease",
#' "kyoto protocol", medical jejun- terms) that would otherwise
#' trigger Asia-keyword matches.
#'
#' @param text Character vector of text to clean.
#' @return Character vector with false positives removed.
remove_false_positives <- function(text) {
  false_positives <- c("kyoto protocol", "kawasaki disease",
                       "jejunal", "jejuni", "jejunum",
                       "jejunoskopie", "jejunostomy")
  for (fp in false_positives) text <- str_remove_all(text, fp)
  text
}

#' Filter NLP entities to relevant types and clean text
#'
#' Applies standard entity-type and text-quality filters used
#' for both OpenAlex and ORCID entity sets.
#'
#' @param df A data frame with columns: id, text, ent_type.
#' @return Filtered and cleaned data frame.
filter_entities <- function(df) {
  df %>%
    filter(ent_type %in% c("ORG", "PERSON", "WORK_OF_ART", "EVENT",
                           "LAW", "FAC", "PRODUCT", "LOC", "GPE", "LANGUAGE")) %>%
    filter(nchar(text) > 3) %>%
    filter(!(tolower(text) %in% dict_not_locations$X1 & ent_type %in% c("LOC", "GPE"))) %>%
    filter(!sapply(strsplit(text, "\\s+"), function(words) any(nchar(words) == 1))) %>%
    filter(!grepl("\\bet al\\b", text, ignore.case = TRUE)) %>%
    filter(!grepl("^[A-Z]{1}\\.?[A-Z]{1}\\.?$", text, ignore.case = TRUE)) %>%
    filter(!grepl("^[A-Z]{2}$", gsub("\\.", "", text, ignore.case = TRUE))) %>%
    # Remove leading articles and trailing "of"
    mutate(text = sub("^(The|the|A|a)\\s+", "", text)) %>%
    mutate(text = sub("\\s+of$", "", text)) %>%
    # Remove entries starting with digits or containing >1 digit
    filter(!grepl("^\\d", text)) %>%
    filter(lengths(gregexpr("\\d", text)) <= 1) %>%
    group_by(id, text) %>%
    slice(1) %>%
    ungroup()
}

#' Map a keyword string to its country of origin
#'
#' Looks up a keyword in `keyword_country_map` and returns
#' the corresponding country name, or NA if not found.
#'
#' @param kw Character scalar: the keyword to look up.
#' @return Country name (character) or NA_character_.
map_keyword_to_country <- function(kw) {
  kw_lower <- tolower(kw)
  for (country in names(keyword_country_map)) {
    if (kw_lower %in% tolower(keyword_country_map[[country]])) return(country)
  }
  NA_character_
}

#' Match Asia keywords against a pre-filtered row
#'
#' Called via pblapply on each row of the pre-filtered data.
#'
#' @param i       Row index into `data`.
#' @param data    Data frame with `search_text` and `row_id` columns.
#' @param keywords Character vector of lowercase keywords to match.
#' @return A tibble with row_id and matched_keyword, or NULL.
match_keywords_in_row <- function(i, data, keywords) {
  row <- data[i, ]
  matched <- keywords[str_detect(row$search_text, str_c("\\b", keywords, "[a-z]*"))]

  if (length(matched) > 0) {
    tibble(
      row_id = row$row_id,
      matched_keyword = matched
    )
  } else {
    NULL
  }
}

#' Auto-detect language and run spaCy NER
#'
#' Uses cld3 to detect language, then initialises the appropriate
#' spaCy model (en/de/fr/zh) and extracts named entities.
#' Maintains state via a closure environment to avoid redundant
#' model re-initialisation.
#'
#' @param text Character scalar: the text to process.
#' @return A data frame of entities, or NULL on failure.
auto_spacy_entity <- local({

  # Environment to store persistent variables
  .env <- new.env()
  .env$last_init <- NULL

  function(text) {
    lang_model_map <- list(
      en = "en_core_web_lg",
      de = "de_core_news_lg",
      fr = "fr_core_news_lg",
      zh = "zh_core_web_lg",
      `zh-Hant` = "zh_core_web_lg"
    )

    lang <- cld3::detect_language(text)

    if (is.na(lang)) {
      message("Skipping: Language could not be detected.")
      return(NULL)
    }

    # Heuristic for Traditional Chinese
    if (lang == "zh" && grepl("[\u3100-\u312F\u31A0-\u31BF\u2E80-\u2EFF\uF900-\uFAFF]", text)) {
      lang <- "zh-Hant"
    }

    if (!lang %in% names(lang_model_map)) {
      message("Skipping: No spaCy model available for detected language: ", lang)
      return(NULL)
    }

    model <- lang_model_map[[lang]]

    # Initialize spaCy if not already initialized with the right model.
    # Capture success as a flag: a bare return(NULL) inside the error handler
    # only returns from the handler closure, not from auto_spacy_entity, so on
    # a failed init execution would otherwise fall through to extraction below.
    init_ok <- tryCatch({
      if (!identical(.env$last_init, model)) {
        if (!is.null(.env$last_init)) spacy_finalize()
        spacy_initialize(model = model)
        .env$last_init <- model
      }
      TRUE
    }, error = function(e) {
      message("Skipping: Failed to initialize spaCy model: ", model)
      FALSE
    })

    if (!init_ok) return(NULL)

    # Extract entities
    result <- tryCatch({
      ents <- spacy_extract_entity(text)
      if (nrow(ents) > 0) {
        message("Entities successfully extracted: ", nrow(ents), " token(s) found.")
      } else {
        message("Entity extraction completed: no entities found.")
      }
      ents
    }, error = function(e) {
      message("Skipping: Failed to extract entities.")
      return(NULL)
    })

    return(result)
  }
})

#' Extract NLP entities for one publication
#'
#' Wraps auto_spacy_entity() with progress reporting.
#' Uses an environment for the counter to avoid global assignment.
#'
#' @param id    Publication identifier.
#' @param abstract Text to extract entities from.
#' @param .counter_env An environment containing `counter` and `n`.
#' @return A data frame of entities with `id` column, or NULL.
extract_entities <- function(id, abstract, .counter_env) {
  .counter_env$counter <- .counter_env$counter + 1
  message(sprintf("[%d/%d] Processing ID: %s", .counter_env$counter, .counter_env$n, id))

  entities <- auto_spacy_entity(abstract)
  if (is.null(entities) || nrow(entities) == 0) return(NULL)

  entities$id <- id
  return(entities)
}

#' Unescape HTML entities (single pass)
#' @param x Character scalar with HTML entities.
#' @return Unescaped character string.
unescape_once <- function(x) {
  as.character(HTML(x))
}

#' Unescape HTML entities (double pass for doubly-escaped text)
#' @param x Character scalar.
#' @return Unescaped character string.
unescape_twice <- function(x) {
  unescape_once(unescape_once(x))
}

#' Deep HTML unescaping using XML parsing (handles complex entities)
#' @param x Character scalar with deeply escaped HTML.
#' @return Clean unescaped string.
fully_unescape_twice <- function(x) {
  xml_text(read_html(paste0("<x>", xml_text(read_html(paste0("<x>", x, "</x>"))), "</x>")))
}

#' Match NER entities against Pacific Asia geocoded locations
#'
#' Filters entity data to GPE/LOC types whose text matches
#' geocoded locations within the Pacific Asia bounding box.
#'
#' @param entities  Data frame with columns: id, text, ent_type.
#' @param pa_locations Character vector of PA location names (already lowercase).
#' @return Filtered data frame of matching entities.
match_entities_to_PA <- function(entities, pa_locations) {
  entities %>%
    filter(tolower(text) %in% pa_locations & ent_type %in% c("GPE", "LOC"))
}

#' Fetch and flatten ORCID employment records with retry
#'
#' @param orcid_id Character scalar: the ORCID identifier.
#' @param tries    Number of retry attempts (default from config).
#' @param wait     Seconds to wait between retries.
#' @return A tibble of employment records, or empty tibble.
fetch_employments <- function(orcid_id,
                              tries = config$api_retry_tries,
                              wait  = config$api_retry_wait) {
  for (attempt in 1:tries) {
    message("Fetching: ", orcid_id, " (attempt ", attempt, ")")

    res <- tryCatch(
      orcid_employments(orcid_id),
      error = function(e) e
    )

    if (!inherits(res, "error") && length(res) > 0) {
      inner <- res[[1]]
      aff <- inner$`affiliation-group`

      if (is.null(aff) || !is.data.frame(aff) || nrow(aff) == 0) {
        return(tibble())
      }

      emp_tbl <- aff %>%
        select(summaries) %>%
        unnest_longer(summaries) %>%
        unnest_wider(summaries) %>%
        mutate(orcid = orcid_id)

      return(emp_tbl)
    }

    message("  Failed: ", ifelse(inherits(res, "error"), res$message, "unknown"))
    if (attempt < tries) {
      message("  Waiting ", wait, " seconds before retry...")
      Sys.sleep(wait)
    }
  }

  message("  Giving up on ", orcid_id)
  return(tibble())
}

#' Fetch ORCID works with retry
#'
#' @param orcid_id Character scalar: the ORCID identifier.
#' @param tries    Number of retry attempts.
#' @param wait     Seconds to wait between retries.
#' @return A tibble of works, or empty tibble.
fetch_works <- function(orcid_id,
                        tries = config$api_retry_tries,
                        wait  = config$api_retry_wait) {
  for (attempt in 1:tries) {
    message("Fetching works: ", orcid_id, " (attempt ", attempt, ")")

    res <- tryCatch(
      orcid_works(orcid_id),
      error = function(e) e
    )

    if (!inherits(res, "error") && length(res) > 0) {
      works <- res[[1]]$works

      if (is.null(works) || !is.data.frame(works) || nrow(works) == 0) {
        return(tibble())
      }

      works_tbl <- works %>%
        mutate(orcid = orcid_id)

      return(works_tbl)
    }

    message("  Failed: ", ifelse(inherits(res, "error"), res$message, "unknown"))
    if (attempt < tries) {
      message("  Waiting ", wait, " seconds before retry...")
      Sys.sleep(wait)
    }
  }

  message("  Giving up on ", orcid_id)
  return(tibble())
}

#' Classify a researcher into a faculty using a local Ollama LLM
#'
#' Sends concatenated publication titles to the Ollama API and parses
#' a JSON response with faculty code and confidence score.
#'
#' @param titles Character string of concatenated publication titles.
#' @param url    Ollama API endpoint (default from config).
#' @param model  Model name (default from config).
#' @return Named list with `faculty` (character) and `confidence` (numeric),
#'         or NULL on failure after retries.
classify_faculty_ollama <- function(titles,
                                    url   = config$ollama_url,
                                    model = config$ollama_model) {
  valid_faculties <- c("mint", "med", "sowi", "tech", "phil", "agrar", "jura", "rewi")

  prompt <- paste0(
    'Classify this researcher\'s publications into exactly ONE faculty based on their title(s).\n',
    '\n',
    'Faculties:\n',
    '- mint: Mathematics, Informatics, Natural Sciences (physics, chemistry, biology, geosciences, computer science, oceanography, marine science)\n',
    '- med: Medicine, dentistry, health sciences, clinical research, pharmacology, epidemiology\n',
    '- sowi: Social sciences, political science, economics, sociology, education, psychology\n',
    '- tech: Engineering, mechanical/electrical/civil engineering, materials science, robotics\n',
    '- phil: Philosophy, history, linguistics, literature, cultural studies, arts, area studies, anthropology\n',
    '- agrar: Agriculture, food science, nutritional science, environmental/ecological science, forestry\n',
    '- jura: Law, legal studies, criminology\n',
    '- rewi: Religious studies, theology (where distinct from philosophy faculty)\n',
    '\n',
    'Important rules:\n',
    '- Pick the SINGLE best-fitting faculty.\n',
    '- If titles span multiple fields, choose the dominant one.\n',
    '- Area studies (e.g. "Chinese history", "Japanese politics") -> phil if humanities-focused, sowi if social-science-focused.\n',
    '- Respond with ONLY a JSON object, no other text.\n',
    '\n',
    'Researcher\'s publication titles:\n',
    substr(titles, 1, 3000), '\n',
    '\n',
    'Respond with ONLY: {"faculty": "<code>", "confidence": <0.0-1.0>}'
  )

  for (attempt in 1:3) {
    result <- tryCatch({
      resp <- request(url) |>
        req_body_json(list(
          model   = model,
          prompt  = prompt,
          stream  = FALSE,
          think   = FALSE,
          options = list(temperature = 0, num_predict = 80),
          system  = "You are a faculty classification assistant. Output ONLY valid JSON."
        )) |>
        req_timeout(120) |>
        req_perform()

      raw_text <- resp_body_json(resp)$response
      json_match <- str_extract(raw_text, "\\{[^}]+\\}")
      if (is.na(json_match)) {
        warning(sprintf("  Attempt %d: no JSON in response: %s", attempt, raw_text))
        return(NULL)
      }

      parsed <- fromJSON(json_match)
      fac  <- tolower(trimws(parsed$faculty))
      conf <- as.numeric(parsed$confidence)

      if (!fac %in% valid_faculties) {
        warning(sprintf("  Attempt %d: invalid faculty '%s'", attempt, fac))
        return(NULL)
      }

      list(faculty = fac, confidence = conf)
    }, error = function(e) {
      warning(sprintf("  Attempt %d error: %s", attempt, conditionMessage(e)))
      NULL
    })

    if (!is.null(result)) return(result)
  }

  NULL
}


#' Classify a batch of (a, b) pairs as YES/NO via a local Ollama LLM.
#'
#' Sends one numbered prompt for the whole batch and parses one YES/NO per line.
#' Used by Sections 16a (same-org detection) and 16b (parent/child hierarchy).
#'
#' @param pairs       List of length-2 character vectors (or a 2-column tibble).
#' @param system_prompt System prompt describing the YES/NO decision rule.
#' @param user_format  A two-argument function (a, b) -> character used to format one pair.
#' @param url          Ollama API endpoint.
#' @param model        Ollama model name.
#' @return Logical vector of length(pairs); NA on parse failures.
classify_pair_batch_ollama <- function(pairs,
                                       system_prompt,
                                       user_format = function(a, b) paste0(a, " | ", b),
                                       url   = config$ollama_url,
                                       model = config$ollama_model) {
  if (length(pairs) == 0) return(logical(0))

  numbered <- vapply(seq_along(pairs), function(i) {
    p <- pairs[[i]]
    sprintf("%d. %s", i, user_format(p[[1]], p[[2]]))
  }, character(1))
  user_content <- paste(numbered, collapse = "\n")

  result <- tryCatch({
    resp <- request(url) |>
      req_body_json(list(
        model   = model,
        prompt  = user_content,
        stream  = FALSE,
        think   = FALSE,
        options = list(temperature = 0, num_predict = length(pairs) * 15L),
        system  = system_prompt
      )) |>
      req_timeout(180) |>
      req_perform()

    raw_text <- resp_body_json(resp)$response

    out <- rep(NA, length(pairs))
    for (line in strsplit(raw_text, "\n", fixed = TRUE)[[1]]) {
      m <- regmatches(line, regexec("^\\s*(\\d+)[.)]\\s*(YES|NO)\\b", line, ignore.case = TRUE))[[1]]
      if (length(m) == 3) {
        idx <- as.integer(m[2])
        if (!is.na(idx) && idx >= 1 && idx <= length(pairs)) {
          out[idx] <- toupper(m[3]) == "YES"
        }
      }
    }
    out
  }, error = function(e) {
    warning(sprintf("classify_pair_batch_ollama error: %s", conditionMessage(e)))
    rep(NA, length(pairs))
  })

  result
}


# ══════════════════════════════════════════════
# SECTION 6: OpenAlex — Fetch Institutions & Publications
# ──────────────────────────────────────────────
# Purpose: Retrieve all institutions in Northern Germany
#          and fetch their complete publication records
#          from the OpenAlex API, year by year.
# Input:   OpenAlex API, `institutions` vector
# Output:  insts_chikon.rds, alex_all_pubs.rds
# ══════════════════════════════════════════════

# Get all institutions located in Germany
insts_de <- oa_fetch(
  entity = "institutions",
  country_code = "DE",
  has_ror = TRUE
)

write_rds(insts_de, "insts_de.rds")

# ── CHECKPOINT / RESTART POINT ──
insts_de <- read_rds("insts_de.rds")

# Filter to relevant Northern German cities
insts_chikon <- insts_de %>%
  select(-country_code) %>%
  unnest(geo) %>%
  filter(city %in% institutions)

insts_chikon_display <- oa_fetch(
  entity = "institutions",
  country_code = "DE",
  display_name.search = institutions,
  has_ror = TRUE
) %>%
  select(-geo, -relevance_score)

insts_chikon <- insts_chikon %>%
  select(colnames(insts_chikon_display)) %>%
  rbind(insts_chikon_display) %>%
  unique()

insts_chikon$ror_long <- insts_chikon$ror
insts_chikon$ror <- sub(".*org/", "", insts_chikon$ror)

write_rds(insts_chikon, "insts_chikon.rds")

# ── CHECKPOINT / RESTART POINT ──
insts_chikon <- read_rds("insts_chikon.rds")

# Fetch all publications for each year in range
years <- config$year_range

expected_cols <- c("title", "authorships", "doi", "publication_year", "type",
                    "source_display_name", "abstract", "keywords",
                    "funders", "first_page", "last_page", "volume", "issue",
                    "issn_l", "fwci", "cited_by_count", "counts_by_year",
                    "referenced_works", "referenced_works_count",
                    "is_oa", "is_oa_anywhere", "oa_status", "language")

for (i in years) {
  message("Fetching year: ", i)

  # Retry transient API failures. A real error returns a sentinel so we retry;
  # a successful-but-empty fetch (NULL/0 rows) is a legitimate "no records"
  # result and must NOT be retried — distinguish the two via the sentinel.
  alex_all_pubs <- NULL
  for (attempt in seq_len(config$api_retry_tries)) {
    result <- tryCatch(
      oa_fetch(
        entity = "works",
        from_publication_date = paste0(i, "-01-01"),
        to_publication_date = paste0(i, "-12-31"),
        authorships.institutions.ror = insts_chikon$ror_long
      ),
      error = function(e) {
        message("  Attempt ", attempt, "/", config$api_retry_tries,
                " failed for year ", i, ": ", conditionMessage(e))
        structure(list(), class = "oa_fetch_error")
      }
    )
    if (!inherits(result, "oa_fetch_error")) {
      alex_all_pubs <- result  # may be NULL = legitimately no records
      break
    }
    if (attempt < config$api_retry_tries) Sys.sleep(config$api_retry_wait)
  }

  if (is.null(alex_all_pubs) || nrow(alex_all_pubs) == 0) {
    warning("No records found (or all retries failed) for year ", i,
            " — skipping.")
    next
  }

  # Preserve the OpenAlex work-ID as a bare W-id BEFORE the DOI-based `id` is
  # created in Section 7 (which would otherwise overwrite it). This is what lets
  # references (also OpenAlex W-ids) be matched back to corpus papers — i.e. it
  # enables a direct intra-corpus citation network (Section 21). Requires a
  # re-harvest to populate for already-fetched years.
  if ("id" %in% names(alex_all_pubs)) {
    alex_all_pubs <- alex_all_pubs %>%
      mutate(openalex_id = sub(".*/", "", id))
  }

  # Select only columns that exist; missing ones become NA
  alex_all_pubs <- alex_all_pubs %>%
    select(any_of(c(expected_cols, "openalex_id")))

  write_rds(alex_all_pubs, paste0("alex_all_pubs_", i, ".rds"))
}

# Read and combine all year-files. Skip any years whose fetch was skipped or
# failed so one bad year doesn't crash the whole combine on a missing file.
file_names <- paste0("alex_all_pubs_", years, ".rds")
existing_files <- file_names[file.exists(file_names)]

if (length(existing_files) < length(file_names)) {
  warning("Missing year file(s) — excluded from combine: ",
          paste(setdiff(file_names, existing_files), collapse = ", "))
}

alex_all_pubs <- existing_files %>%
  map(read_rds) %>%
  bind_rows()

# Create stable identifiers: use DOI when available, else row-based fallback
alex_all_pubs <- alex_all_pubs %>%
  mutate(
    cleaned_doi = str_remove(doi, "https?://(dx\\.)?doi\\.org/"),
    id = if_else(
      !is.na(cleaned_doi) & cleaned_doi != "",
      cleaned_doi,
      paste0("no_doi_", row_number())
    )
  ) %>%
  select(-cleaned_doi)

# Strip inline HTML tags from abstracts
alex_all_pubs <- alex_all_pubs %>%
  mutate(abstract = str_remove_all(abstract, "<[^>]+>.*?</[^>]+>"))

# Clean abstract text: remove leading noise, "Context/Summary/Abstract:" prefixes,
# and trailing "correspondence" boilerplate
alex_all_pubs <- alex_all_pubs %>%
  mutate(
    abstract = abstract %>%
      str_replace_all("^\\s*[^A-Za-z]*", "") %>%
      str_replace("^((?i)context|summary|abstract)\\b[:\\s-]*", "") %>%
      str_replace("(?i)correspondence[^\\.]*.*$", "") %>%
      str_trim() %>%
      str_replace("(?i)^.*abstract:\\s*", "")
  )

write_rds(alex_all_pubs, "alex_all_pubs.rds")


# ══════════════════════════════════════════════
# SECTION 7: OpenAlex — Regex Keyword Matching
# ──────────────────────────────────────────────
# Purpose: Identify Asia-related publications by
#          matching keyword dictionary against titles.
# Input:   alex_all_pubs.rds
# Output:  alex_matched_by_regex.rds
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
# Requires: Sections 1-5 (config, libraries, keywords, dicts, helpers)
alex_all_pubs <- read_rds("alex_all_pubs.rds")

# Prepare searchable text: lowercase title, remove false positives
alex_all_pubs <- alex_all_pubs %>%
  mutate(
    row_id = row_number(),
    search_text = map_chr(
      str_c(title, sep = " "),
      ~ .x %>%
        iconv(from = "", to = "UTF-8", sub = " ") %>%
        str_replace_all("[^[:print:]\n\r\t]", " ") %>%
        tolower() %>%
        remove_false_positives() %>%
        str_squish()
    )
  )

# Fast pre-filter using vectorized regex
pre_filtered <- alex_all_pubs %>%
  filter(str_detect(search_text, combined_pattern))

# Keyword extraction with progress bar (only on filtered rows)
matched_keywords_df <- pblapply(seq_len(nrow(pre_filtered)), match_keywords_in_row,
                                data = pre_filtered, keywords = combined_keywords_lower) %>%
  bind_rows()

# Final output: one row per (publication, keyword) match
alex_matched_by_regex <- matched_keywords_df %>%
  left_join(alex_all_pubs, by = "row_id") %>%
  select(-search_text, -row_id)

write_rds(alex_matched_by_regex, "alex_matched_by_regex.rds")
rm(matched_keywords_df, pre_filtered)


# ══════════════════════════════════════════════
# SECTION 8: OpenAlex — NLP Entity Extraction (spaCy)
# ──────────────────────────────────────────────
# Purpose: Run spaCy NER on all abstracts to find
#          Asia-related geographic entities missed by regex.
# Input:   alex_all_pubs.rds
# Output:  entities_alex_abstracts.rds
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
# Requires: Sections 1-5, alex_all_pubs (in memory from Sec 7)
alex_matched_by_regex <- read_rds("alex_matched_by_regex.rds")

spacy_initialize(model = "en_core_web_lg")

abstracts <- alex_all_pubs %>%
  group_by(id) %>% slice(1) %>% ungroup() %>%
  filter(!is.na(abstract), trimws(abstract) != "NA") %>%
  select(id, abstract)

# Remove false-positive terms before NLP
abstracts <- abstracts %>%
  mutate(abstract = remove_false_positives(abstract))

# Set up counter environment (avoids global <<- assignment)
.counter_env <- new.env(parent = emptyenv())
.counter_env$counter <- 0
.counter_env$n <- nrow(abstracts)

entities_alex_abstracts <- purrr::map2_dfr(
  abstracts$id,
  abstracts$abstract,
  extract_entities,
  .counter_env = .counter_env
)

write_rds(entities_alex_abstracts, "entities_alex_abstracts.rds")

rm(abstracts)


# ══════════════════════════════════════════════
# SECTION 9: OpenAlex — Geocoding Entities
# ──────────────────────────────────────────────
# Purpose: Geocode GPE/LOC entities via OpenStreetMap
#          to determine which are in Pacific Asia.
# Input:   entities_alex_abstracts.rds
# Output:  entities_osm.geojson
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
entities_alex_abstracts <- read_rds("entities_alex_abstracts.rds")

# Prepare geographic entities for geocoding
geo_entities_complete_alex <- entities_alex_abstracts %>%
  filter(ent_type %in% c("GPE", "LOC")) %>%
  select(id, text) %>%
  mutate(text = sub("^\\s*the\\s+", "", text, ignore.case = TRUE)) %>%
  filter(nchar(text) >= 4 & !grepl("[0-9]", text)) %>%
  mutate(text = tolower(text)) %>%
  distinct() %>%
  filter(!text %in% dict_not_locations$X1) %>%
  filter(!tolower(text) %in% dict_not_locations$X1) %>%
  count(text, sort = TRUE) %>%
  filter(n > 1)

# Split into chunks of 1000 for batched geocoding
chunks <- split(geo_entities_complete_alex,
                ceiling(seq_along(geo_entities_complete_alex$text) / 1000))

# Auto-detect resume point: skip chunks that already have output files
existing_chunks <- list.files(pattern = "geo_entities_chunk_\\d+\\.csv$") %>%
  str_extract("\\d+") %>% as.integer()
start_chunk <- if (length(existing_chunks) > 0) max(existing_chunks) + 1L else 1L
message("Geocoding: starting from chunk ", start_chunk, " of ", length(chunks))

if (start_chunk <= length(chunks)) {
walk2(chunks[start_chunk:length(chunks)], start_chunk:length(chunks), function(chunk, i) {
  message("Processing chunk ", i, " of ", length(chunks))

  result <- geocode(chunk, address = text, method = "osm",
                    progress_bar = TRUE, timeout = 20)

  write_csv(result, paste0("geo_entities_chunk_", i, ".csv"))
})
}

# Recombine all chunks
all_results <- list.files(pattern = "geo_entities_chunk_.*\\.csv$") %>%
  map_dfr(read_csv)

entities_osm <- all_results %>%
  select(-n) %>%
  unique() %>%
  drop_na() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%
  unique() %>%
  filter(!text %in% dict_not_locations$X1)

write_sf(entities_osm, "entities_osm.geojson", append = FALSE)


# ══════════════════════════════════════════════
# SECTION 10: OpenAlex — Join Regex + NLP Results
# ──────────────────────────────────────────────
# Purpose: Combine regex- and NLP-matched publications
#          into a single Pacific-Asia publication set.
#          Spatial bounding box filters NLP entities to
#          the target region (80-150°E, 0-50°N).
# Input:   entities_osm.geojson, alex_matched_by_regex.rds
# Output:  alex_all_pubs_PA.rds
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
entities_osm <- read_sf("entities_osm.geojson") %>%
  filter(!text %in% dict_not_locations$X1)

# Bounding box for Pacific Asia: 80-150°E longitude, 0-50°N latitude
bbox <- st_as_sfc(st_bbox(config$bbox, crs = 4326))
entities_osm_PA <- entities_osm[st_within(entities_osm, bbox, sparse = FALSE), ]

alex_matched_by_spacy <- match_entities_to_PA(entities_alex_abstracts, entities_osm_PA$text)

# Combine regex + NLP matches
alex_all_pubs_PA <- alex_all_pubs %>%
  filter(id %in% alex_matched_by_spacy$id | id %in% alex_matched_by_regex$id) %>%
  write_rds("alex_all_pubs_PA.rds")

# ── CHECKPOINT / RESTART POINT ──
alex_all_pubs_PA <- read_rds("alex_all_pubs_PA.rds")

# Print summary statistics (after checkpoint load, so alex_all_pubs_PA is available)
print(paste("Themed publications found via OpenAlex:",
            alex_all_pubs_PA %>% select(id) %>% unique() %>% nrow()))
print(paste0("Of these, via NLP: ",
             alex_matched_by_spacy %>% select(id) %>% unique() %>% nrow(),
             ", unique: ",
             alex_matched_by_spacy %>% select(id) %>% unique() %>%
               filter(!id %in% alex_matched_by_regex$id) %>% nrow()))
print(paste0("Via regex: ",
             alex_matched_by_regex %>% select(id) %>% unique() %>% nrow(),
             ", unique: ",
             alex_matched_by_regex %>% select(id) %>% unique() %>%
               filter(!id %in% alex_matched_by_spacy$id) %>% nrow()))
print(paste("Overlap:",
            (alex_matched_by_regex %>% select(id) %>% unique() %>% nrow()) -
              (alex_matched_by_regex %>% select(id) %>% unique() %>%
                 filter(!id %in% alex_matched_by_spacy$id) %>% nrow())))

rm(alex_all_pubs)


# ══════════════════════════════════════════════
# SECTION 11: OpenAlex — Author & Researcher Processing
# ──────────────────────────────────────────────
# Purpose: Unnest authorship data, create researcher
#          records with institution affiliations,
#          harmonise org/dept names via lookup tables.
# Input:   alex_all_pubs_PA.rds, insts_chikon.rds
# Output:  entities_alex_abstracts_PA.csv,
#          complete_works_PA_alex.csv,
#          complete_researchers_PA_alex.csv
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
# Requires: Sections 1-5, insts_chikon.rds, insts_de.rds,
#           entities_alex_abstracts (in memory from Sec 8),
#           alex_matched_by_regex (in memory from Sec 7)
alex_all_pubs_PA <- read_rds("alex_all_pubs_PA.rds")

# Finalise keyword data frames (alex) — apply shared entity filter
entities_alex_abstracts_PA <- entities_alex_abstracts %>%
  filter(id %in% alex_all_pubs_PA$id) %>%
  filter_entities() %>%
  select(id, ent_type, text) %>%
  unique()

# Add regex-matched keywords as LOC entities
to_add <- alex_matched_by_regex %>%
  select(id, matched_keyword) %>%
  rename(text = matched_keyword) %>%
  mutate(text = str_to_title(text)) %>%
  mutate(ent_type = "LOC") %>%
  unique()

entities_alex_abstracts_PA <- entities_alex_abstracts_PA %>%
  rbind(to_add) %>%
  unique()

write_csv(entities_alex_abstracts_PA, "entities_alex_abstracts_PA.csv")

# ── CHECKPOINT / RESTART POINT ──
entities_alex_abstracts_PA <- read_csv("entities_alex_abstracts_PA.csv", show_col_types = F)

rm(entities_alex_abstracts)

# Unnest authorship affiliations
chikon_pubs_unnest <- alex_all_pubs_PA %>%
  unnest(authorships, names_sep = "_") %>%
  unnest(authorships_affiliations, names_sep = "_") %>%
  select(id, title, authorships_display_name, authorships_orcid, authorships_affiliation_raw,
         authorships_affiliations_country_code, authorships_affiliations_display_name,
         authorships_affiliations_ror, doi, publication_year, type, source_display_name,
         funders, first_page, last_page, volume, issue)

chikon_pubs_unnest$ror <- sub(".*org/", "", chikon_pubs_unnest$authorships_affiliations_ror)
chikon_pubs_unnest$orcid <- sub(".*org/", "", chikon_pubs_unnest$authorships_orcid)
chikon_pubs_unnest$doi <- sub(".*org/", "", chikon_pubs_unnest$doi)

# When an author's raw affiliation maps to multiple orgs, set raw to NA
chikon_pubs_unnest <- chikon_pubs_unnest %>%
  group_by(authorships_orcid, authorships_affiliation_raw) %>%
  mutate(
    org_count = n_distinct(authorships_affiliations_display_name),
    authorships_affiliation_raw = if_else(org_count > 1, NA_character_, authorships_affiliation_raw)
  ) %>%
  select(-org_count) %>%
  ungroup()

# ── Guard: reject unreliable OpenAlex institution assignments ────────────────
# Two OpenAlex disambiguation failures attach phantom employers to researchers:
#
#  (1) BARE-CITY guesses. An institution-LESS raw affiliation (just a city, e.g.
#      "Kiel, Germany") gets resolved to a specific — often wrong — institution
#      in that city (a political scientist who wrote only "Kiel" was tagged
#      "Clinical Research Center Kiel"). When the raw string, stripped of country
#      words and punctuation, is nothing more than the resolved institution's own
#      city, we drop the guess.
#
#  (2) MAGNET RORs. A few OpenAlex institutions act as catch-alls that absorb
#      many unrelated affiliations. `bad_rors` is a hand-curated, individually
#      verified blocklist; every row resolved to one is dropped. Add an entry
#      only after confirming its assigned raws are wildly unrelated AND none
#      genuinely name that institution:
#        - 05sw1mq09  "Clinical Research Center Kiel": OpenAlex inflates this tiny
#          facility to ~11k works; in this corpus its 51 rows include a street
#          address, "Planton GmbH", dairy research and an economics centre, and
#          NONE name a clinical/molecular-biology unit. It is the origin of the
#          spurious "Institute of Clinical Molecular Biology" affiliations that
#          Section 17 then produces (economists Görg/Loy/Revilla Diez, etc.).
#          Real IKMB biologists reach Kiel via other RORs, so they are unaffected.
#
# Rows that genuinely name an institution, or carry no raw string at all (a
# structured match to a trustworthy ROR), are left untouched.
# Ref: db-kiel affiliation audit 2026-08.
bad_rors <- c("05sw1mq09")   # verified OpenAlex catch-all RORs — see note above
if (!exists("insts_de")) insts_de <- readRDS("insts_de.rds")
.aff_country <- c("germany","deutschland","usa","us","uk","china","prc","england","scotland",
  "wales","france","italy","spain","india","japan","korea","switzerland","schweiz","austria",
  "osterreich","österreich","netherlands","europe","de","gb","fr")
.aff_norm <- function(x) {
  x <- tolower(ifelse(is.na(x), "", x))
  x <- str_replace_all(x, "[^a-zäöüß]+", " ")
  vapply(str_split(str_squish(x), " "),
         function(t) str_c(t[nzchar(t) & !t %in% .aff_country], collapse = " "),
         character(1))
}
.ror_city <- insts_de %>%
  transmute(ror = sub(".*org/", "", ror), geo) %>%
  unnest(geo) %>%
  transmute(ror, city_norm = .aff_norm(city)) %>%
  filter(nzchar(city_norm)) %>%
  distinct(ror, .keep_all = TRUE)
chikon_pubs_unnest <- chikon_pubs_unnest %>%
  left_join(.ror_city, by = "ror") %>%
  mutate(
    raw_norm     = .aff_norm(authorships_affiliation_raw),
    is_city_only = !is.na(authorships_affiliation_raw) &
                   nzchar(str_trim(authorships_affiliation_raw)) &
                   !is.na(authorships_affiliations_display_name) &
                   nzchar(authorships_affiliations_display_name) &
                   (raw_norm == "" | (!is.na(city_norm) & raw_norm == city_norm)),
    is_bad_ror   = !is.na(ror) & ror %in% bad_rors,
    drop_org     = is_city_only | is_bad_ror)
message(sprintf("Section 11: dropped %d bare-city + %d magnet-ROR institution guesses",
                sum(chikon_pubs_unnest$is_city_only, na.rm = TRUE),
                sum(chikon_pubs_unnest$is_bad_ror, na.rm = TRUE)))
chikon_pubs_unnest <- chikon_pubs_unnest %>%
  mutate(
    authorships_affiliations_display_name =
      if_else(drop_org, NA_character_, authorships_affiliations_display_name),
    ror = if_else(drop_org, NA_character_, ror)) %>%
  select(-city_norm, -raw_norm, -is_city_only, -is_bad_ror, -drop_org)

# Extract funder names from funders list-column
funder_names_str <- sapply(chikon_pubs_unnest$funders, function(x) {
  if (!is.data.frame(x)) return("")
  paste(x[["display_name"]], collapse = "; ")
})

chikon_pubs_unnest$funder_display_names <- funder_names_str

chikon_pubs_unnest <- chikon_pubs_unnest %>%
  select(-authorships_orcid, -funders, -authorships_affiliations_ror)

# Assign filler ORCIDs to authors without one
filler_orcids <- chikon_pubs_unnest %>%
  filter(is.na(orcid)) %>%
  distinct(authorships_display_name) %>%
  mutate(filler_orcid = paste0("no_orcid_", row_number()))

chikon_pubs_unnest <- chikon_pubs_unnest %>%
  left_join(filler_orcids, by = "authorships_display_name") %>%
  mutate(orcid = ifelse(is.na(orcid), filler_orcid, orcid)) %>%
  select(-filler_orcid)

chikon_pubs_unnest$authorships_affiliation_raw <- sub(",.*", "", chikon_pubs_unnest$authorships_affiliation_raw)

# Filter to authors at relevant Northern German institutions
chikon_pubs_unnest_ror <- chikon_pubs_unnest %>%
  filter(ror %in% insts_chikon$ror)

# Save for reuse in Section 20 (funding, co-authorship exports)
write_rds(chikon_pubs_unnest, "chikon_pubs_unnest.rds")
write_rds(chikon_pubs_unnest_ror, "chikon_pubs_unnest_ror.rds")

# Build Alex-sourced works table
complete_works_PA_alex <- chikon_pubs_unnest_ror %>%
  rename(
    title_title_value = title,
    publication_date_year_value = publication_year,
    journal_title_value = source_display_name) %>%
  mutate(title_translated_title_value = "",
         title_translated_title_language_code = "",
         path = orcid,
         url_value = "",
         publication_date_month_value = "",
         source_source_orcid_path = orcid) %>%
  select(-authorships_display_name,
         -authorships_affiliation_raw,
         -authorships_affiliations_display_name,
         -first_page,
         -last_page,
         -volume,
         -issue,
         -ror,
         -funder_display_names
  ) %>%
  filter(!is.na(title_title_value)) %>%
  mutate(label = str_extract(title_title_value, "^\\S+(\\s+\\S+){0,3}")) %>%
  mutate(label = if_else(str_count(title_title_value, "\\s+") > 3,
                         paste0(label, "\u2026"),
                         label))

complete_works_PA_alex$type[complete_works_PA_alex$type == "article"] <- "journal-article"

# Deduplicate by first-4-words of title per author/type
complete_works_PA_alex <- complete_works_PA_alex %>%
  mutate(filter_title = tolower(title_title_value) %>%
           str_split(" ") %>%
           map_chr(~ str_c(head(.x, 4), collapse = " "))) %>%
  group_by(filter_title, orcid, type) %>%
  slice(1) %>%
  ungroup() %>%
  select(-filter_title)

write_csv(complete_works_PA_alex, "complete_works_PA_alex.csv")

# ── CHECKPOINT / RESTART POINT ──
complete_works_PA_alex <- read_csv("complete_works_PA_alex.csv")

# Create researcher records from authorship data
complete_researchers_PA_alex <- complete_works_PA_alex %>%
  select(orcid) %>%
  unique() %>%
  left_join(chikon_pubs_unnest, by = "orcid") %>%
  select(orcid, authorships_display_name,
         authorships_affiliation_raw, authorships_affiliations_display_name, publication_year) %>%
  rename(organization_name = authorships_affiliations_display_name,
         name_value = authorships_display_name,
         start_date_year_value = publication_year) %>%
  mutate(department_name =
           ifelse(organization_name == authorships_affiliation_raw,
                  NA, authorships_affiliation_raw
           ))

complete_researchers_PA_alex <- complete_researchers_PA_alex %>%
  select(-authorships_affiliation_raw) %>%
  mutate(role_title = NA,
         url_value = NA,
         end_date_year_value = NA,
         start_date_day_value = NA,
         end_date_day_value = NA,
         start_date_month_value = NA,
         end_date_month_value = NA) %>%
  mutate(last = sub(".*\\s(\\S+)$", "\\1", name_value)) %>%
  mutate(first = sub("\\s\\S+$", "", name_value)) %>%
  left_join(insts_de %>% select(-country_code) %>% unnest(geo) %>%
              filter(city %in% institutions) %>%
              select(display_name, city) %>%
              rename(organization_address_city = city, organization_name = display_name),
            by = "organization_name", relationship = "many-to-many") %>%
  mutate(department_name = mapply(function(organization_name, department_name) gsub(organization_name, "", department_name, fixed = TRUE), organization_name, department_name)) %>%
  mutate(department_name = sub("\\s{2,}.*", "", department_name)) %>%
  mutate(department_name = sub("^[^a-zA-Z]*", "", department_name)) %>%
  mutate(department_name = trimws(department_name))

# Harmonise department names
complete_researchers_PA_alex <- complete_researchers_PA_alex %>%
  left_join(universities %>% rename(department_name = organization_name, department_name_harmonised = organization_name_harmonised), by = c("department_name"), relationship = "many-to-many") %>%
  mutate(department_name =
           ifelse(!is.na(department_name_harmonised),
                  department_name_harmonised,
                  department_name)) %>%
  select(-department_name_harmonised) %>%
  unique()

# Harmonise organisation names
complete_researchers_PA_alex <- complete_researchers_PA_alex %>%
  left_join(universities, by = c("organization_name"), relationship = "many-to-many") %>%
  mutate(organization_name =
           ifelse(!is.na(organization_name_harmonised),
                  organization_name_harmonised,
                  organization_name)) %>%
  select(-organization_name_harmonised)

complete_researchers_PA_alex <- complete_researchers_PA_alex %>%
  left_join(departments, by = c("department_name", "organization_name"), relationship = "many-to-many") %>%
  mutate(department_name =
           ifelse(!is.na(department_name_harmonised),
                  department_name_harmonised,
                  department_name)) %>%
  select(-department_name_harmonised) %>%
  mutate(department_name = ifelse(department_name == "", NA, department_name)) %>%
  unique() %>%
  mutate(department_name =
           ifelse(department_name %in% organization_name, NA, department_name)) %>%
  mutate(department_name =
           ifelse(department_name %in% universities$organization_name, NA, department_name)) %>%
  mutate(department_name =
           ifelse(department_name %in% universities$organization_name_harmonised, NA, department_name)) %>%
  left_join(faculties, by = c("organization_name", "department_name"), relationship = "many-to-many") %>%
  unique()

# Keep rows with department info; if none in group, keep one row
count_mentions <- complete_researchers_PA_alex %>%
  count(name_value, organization_name)

complete_researchers_PA_alex <- complete_researchers_PA_alex %>%
  left_join(count_mentions, by = c("name_value", "organization_name"), relationship = "many-to-many") %>%
  group_by(name_value, organization_name) %>%
  filter(
    !is.na(department_name) | row_number() == 1
  ) %>%
  ungroup() %>%
  select(-n) %>%
  unique()

complete_researchers_PA_alex <- complete_researchers_PA_alex %>%
  group_by(orcid, organization_name, department_name) %>%
  arrange(-start_date_year_value) %>%
  slice(1) %>%
  ungroup()

complete_researchers_PA_alex <- complete_researchers_PA_alex %>%
  select(-end_date_year_value, -start_date_day_value, -end_date_day_value, -start_date_month_value, -end_date_month_value)

write_csv(complete_researchers_PA_alex, "complete_researchers_PA_alex.csv")

# ── CHECKPOINT / RESTART POINT ──
# Requires: Sections 1-5, insts_chikon.rds
complete_researchers_PA_alex <- read_csv("complete_researchers_PA_alex.csv", show_col_types = F)


# ══════════════════════════════════════════════
# SECTION 12: ORCID — Mine Researchers & Employments
# ──────────────────────────────────────────────
# Purpose: Search ORCID for all researchers affiliated
#          with Northern German institutions, then fetch
#          their employment records.
# Input:   insts_chikon.rds, institutions vector
# Output:  results_complete.rds, my_osu_employment.rds,
#          complete_researchers_orcid.csv
# ══════════════════════════════════════════════

# Collect ORCID search results into a list, then bind_rows once
# (avoids growing a data frame row-by-row in a loop)
results_list <- list()

for (i in c(insts_chikon$display_name, institutions)) {
  # Remove any literal double quotes from institution names to prevent

  # breaking the quoted ORCID API query (e.g. 'Cluster of Excellence "Inflammation at Interfaces"')
  i <- gsub('"', '', i)
  cat("\n=== Institution:", i, "===\n")

  results_count <- tryCatch(
    {
      count <- base::attr(
        rorcid::orcid_search(current_inst = paste0('"', i, '"')),
        "found"
      )
      cat("Total results:", count, "\n")
      count
    },
    error = function(e) {
      message("Failed to get results count for ", i, " - skipping.")
      return(0)
    }
  )

  if (is.null(results_count) || results_count == 0) {
    message("No results for: ", i)
    next
  }

  results_steps <- seq(from = 0, to = results_count, by = 200)
  institution_results <- list()

  for (page in results_steps) {
    res <- NULL
    for (attempt in seq_len(config$api_retry_tries)) {
      Sys.sleep(2)

      res <- tryCatch(
        {
          my_orcids <- rorcid::orcid_search(
            current_inst = paste0('"', i, '"'),
            rows = 200,
            start = page
          )
          cat("Fetched page:", page, "\n")
          my_orcids
        },
        error = function(e) {
          message("Error on page ", page, " for ", i,
                  " (attempt ", attempt, "/", config$api_retry_tries, ") - retrying in ",
                  config$api_retry_wait, "s...")
          Sys.sleep(config$api_retry_wait)
          NULL
        }
      )

      if (!is.null(res)) break
    }

    if (is.null(res)) {
      message("Giving up on page ", page, " for ", i)
    } else {
      institution_results[[as.character(page)]] <- res
    }
  }

  # Combine this institution's results
  results <- institution_results %>%
    purrr::compact() %>%
    purrr::map_dfr(as_tibble) %>%
    janitor::clean_names()

  results_list[[i]] <- results
}

results_complete <- bind_rows(results_list) %>% unique()

write_rds(results_complete, "results_complete.rds")

# ── CHECKPOINT / RESTART POINT ──
results_complete <- read_rds("results_complete.rds")

results <- results_complete %>% unique()

# Get employment data for each mined ORCID entry
my_osu_orcid_ids <- results$orcid

employment_list <- vector("list", length(my_osu_orcid_ids))

for (idx in seq_along(my_osu_orcid_ids)) {
  id <- my_osu_orcid_ids[idx]
  emp <- fetch_employments(id)
  if (nrow(emp) == 0) next

  employment_list[[idx]] <- emp

  # Save checkpoint every 100 records
  if (idx %% 100 == 0 || idx == length(my_osu_orcid_ids)) {
    my_osu_employment <- bind_rows(compact(employment_list))
    write_rds(my_osu_employment, "my_osu_employment.rds")
    message("  Checkpoint: ", nrow(my_osu_employment), " employment records (", idx, "/", length(my_osu_orcid_ids), ")")
  }
}

my_osu_employment <- bind_rows(compact(employment_list))
write_rds(my_osu_employment, "my_osu_employment.rds")

# ── CHECKPOINT / RESTART POINT ──
my_osu_employment <- read_rds("my_osu_employment.rds")

# Clean up column names from ORCID API nested structure
my_osu_employment_data <- my_osu_employment %>%
  select(-orcid)

names(my_osu_employment_data) <- names(my_osu_employment_data) %>%
  stringr::str_replace(., "employment-summary.", "") %>%
  stringr::str_replace(., "source.source.", "") %>%
  stringr::str_replace(., "organization.disambiguated.", "")

complete_researchers_orcid <- my_osu_employment_data %>%
  mutate(orcid = str_extract(path, "(?<=/)[^/]+(?=/)"))

# Tidy up column names
complete_researchers_orcid <- complete_researchers_orcid %>%
  rename(department_name = `department-name`,
         role_title = `role-title`,
         organization_name = organization.name,
         name_value = name.value,
         url_value = url,
         organization_address_city = organization.address.city,
         start_date_year_value = `start-date.year.value`
  ) %>%
  select(name_value, orcid, organization_address_city, organization_name, department_name, role_title, start_date_year_value, url_value)

write_csv(complete_researchers_orcid, "complete_researchers_orcid.csv")


# ══════════════════════════════════════════════
# SECTION 13: ORCID — Mine Works
# ──────────────────────────────────────────────
# Purpose: Fetch all publications for the mined
#          ORCID researchers.
# Input:   complete_researchers_orcid.csv
# Output:  my_orcid_works.rds
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
complete_researchers_orcid <- read_csv("complete_researchers_orcid.csv", show_col_types = F)

all_orcids <- unique(complete_researchers_orcid$orcid)

# Load existing results if available (supports resuming)
if (file.exists("my_orcid_works.rds")) {
  my_orcid_works <- read_rds("my_orcid_works.rds")
  done_orcids <- unique(my_orcid_works$orcid)
} else {
  my_orcid_works <- tibble()
  done_orcids <- character()
}

remaining_orcids <- setdiff(all_orcids, done_orcids)

# Fetch works for remaining ORCIDs
results <- map(remaining_orcids, function(orcid) {
  message(orcid, " (", which(all_orcids == orcid), " of ", length(all_orcids), ")")
  works <- fetch_works(orcid)
  if (nrow(works) == 0) return(NULL)
  works
})

# Bind all results at once
new_orcid_works <- bind_rows(compact(results))

my_orcid_works <- bind_rows(my_orcid_works, new_orcid_works)
print(paste(nrow(my_orcid_works %>% select(title.title.value, type, `journal-title.value`) %>% unique())))

write_rds(my_orcid_works %>% unique(), "my_orcid_works.rds")


# ══════════════════════════════════════════════
# SECTION 14: ORCID/Crossref — Fetch Abstracts & NLP
# ──────────────────────────────────────────────
# Purpose: Fetch abstracts from Crossref for ORCID works,
#          then run spaCy NER to extract entities.
# Input:   my_orcid_works.rds
# Output:  abstracts_crossref.csv,
#          entities_orcid_abstracts.csv
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
complete_works_orcid <- read_rds("my_orcid_works.rds")

complete_works_orcid <- complete_works_orcid %>%
  rename(put_code = `put-code`,
         "title_title_value" = "title.title.value",
         url_value = `url.value`,
  )

complete_works_orcid$doi <- ifelse(
  grepl("doi\\.org/", complete_works_orcid$url_value),
  sub(".*doi\\.org/", "", complete_works_orcid$url_value),
  NA
)

complete_works_orcid <- complete_works_orcid %>%
  mutate(
    id = ifelse(
      !is.na(doi) & doi != "",
      doi,
      paste0("orcid_", cumsum(is.na(doi) | doi == ""))
    )
  )

# Add lowercase searchable text
complete_works_orcid <- complete_works_orcid %>%
  mutate(
    row_id = row_number(),
    search_text = str_c(title_title_value, sep = " ") %>% tolower()
  )

# Fetch abstracts from Crossref
complete_works_orcid <- complete_works_orcid %>%
  mutate(title_prefix = str_extract(title_title_value, "^\\S+(\\s+\\S+){0,4}"))

dois <- complete_works_orcid %>% select(doi) %>% unique() %>% drop_na()

abstracts_crossref <- map_dfr(seq_len(nrow(dois)), function(i) {
  doi <- dois$doi[i]
  abstract_get <- NA

  message(paste0("Publication #", i, " of ", nrow(dois), ": ", doi))

  for (attempt in 1:1) {
    abstract_get <- tryCatch({
      cr_abstract(doi)
    }, error = function(e) {
      message(paste0("  Attempt ", attempt, "/", config$api_retry_tries,
                     " failed for DOI ", doi, ": ", e$message))
      NA
    })

    if (!is.na(abstract_get) && abstract_get != "") break

    # if (attempt < config$api_retry_tries) Sys.sleep(config$api_retry_wait)
  }

  if (is.na(abstract_get) || abstract_get == "") abstract_get <- NA

  tibble(doi = doi, abstract = abstract_get)
})

# Clean abstract text
abstracts_crossref <- abstracts_crossref %>% mutate(
  abstract = abstract %>%
    str_replace_all("^\\s*[^A-Za-z]*", "") %>%
    str_replace("(?i)^(context|summary|abstract)[:\\s-]*", "") %>%
    str_replace("(?i)correspondence[^\\.]*.*$", "") %>%
    str_trim() %>%
    str_replace("(?i)^.*abstract:\\s*", "")
)

abstracts_crossref <- abstracts_crossref %>%
  drop_na()

write_csv(abstracts_crossref, "abstracts_crossref.csv")

# ── CHECKPOINT / RESTART POINT ──
abstracts_crossref <- read_csv("abstracts_crossref.csv", show_col_types = F)

# Remove false positives before NLP
abstracts_crossref <- abstracts_crossref %>%
  mutate(abstract = remove_false_positives(abstract))

# Run NLP on Crossref abstracts
spacy_initialize(model = "en_core_web_lg")

.counter_env <- new.env(parent = emptyenv())
.counter_env$counter <- 0
.counter_env$n <- nrow(abstracts_crossref)

entities_orcid_abstracts <- purrr::map2_dfr(
  abstracts_crossref$doi,
  abstracts_crossref$abstract,
  extract_entities,
  .counter_env = .counter_env
)

# FIX: was write_csv(..., "entities_orcid_abstracts.rds") — CSV with .rds extension.
# Now correctly writes as .csv.
write_csv(entities_orcid_abstracts, "entities_orcid_abstracts.csv")


# ══════════════════════════════════════════════
# SECTION 15: ORCID — Regex + NLP Matching
# ──────────────────────────────────────────────
# Purpose: Apply the same regex and NLP matching pipeline
#          to ORCID works that was applied to OpenAlex.
# Input:   complete_works_orcid, entities_orcid_abstracts.csv
# Output:  orcid_matched_by_regex.csv,
#          orcid_matched_by_spacy.csv
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
entities_orcid_abstracts <- read_csv("entities_orcid_abstracts.csv", show_col_types = F)

# Regex matching on ORCID titles
pre_filtered <- complete_works_orcid %>%
  filter(str_detect(search_text, combined_pattern))

# Remove false positives from pre-filtered search text
pre_filtered <- pre_filtered %>%
  mutate(search_text = remove_false_positives(search_text))

matched_keywords_df <- pblapply(seq_len(nrow(pre_filtered)), match_keywords_in_row,
                                data = pre_filtered, keywords = combined_keywords_lower) %>%
  bind_rows()

orcid_matched_by_regex <- matched_keywords_df %>%
  left_join(complete_works_orcid, by = "row_id") %>%
  select(-search_text, -row_id)

# Clean up
orcid_matched_by_regex$title_title_value <- trimws(orcid_matched_by_regex$title_title_value, which = "left")

orcid_matched_by_regex <- orcid_matched_by_regex %>%
  group_by(title_title_value, type, path, matched_keyword) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(orcid = str_extract(path, "(?<=/)[^/]+(?=/)"))

orcid_matched_by_regex <- orcid_matched_by_regex %>%
  group_by(title_title_value, type, orcid, matched_keyword) %>%
  slice(1) %>%
  ungroup()

write_csv(orcid_matched_by_regex, "orcid_matched_by_regex.csv")

# ── CHECKPOINT / RESTART POINT ──
orcid_matched_by_regex <- read_csv("orcid_matched_by_regex.csv", guess_max = 10000, show_col_types = F)

# Geocode new ORCID entities not already in entities_osm
geo_entities_added_orcid <- entities_orcid_abstracts %>%
  filter(ent_type %in% c("GPE", "LOC")) %>%
  select(id, text) %>%
  mutate(text = sub("^\\s*the\\s+", "", text, ignore.case = TRUE)) %>%
  filter(nchar(text) >= 4 & !grepl("[0-9]", text)) %>%
  mutate(text = tolower(text)) %>%
  unique() %>%
  filter(!text %in% dict_not_locations$X1) %>%
  filter(!tolower(text) %in% dict_not_locations$X1) %>%
  filter(!text %in% entities_osm$text) %>%
  filter(!tolower(text) %in% tolower(entities_osm$text)) %>%
  count(text) %>% arrange(-n) %>%
  filter(n > 1) %>%
  geocode(address = text, method = "osm")

geo_entities_added_orcid <- geo_entities_added_orcid %>%
  filter(!text %in% dict_not_locations$X1) %>%
  select(-n) %>%
  unique() %>%
  drop_na() %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

write_sf(geo_entities_added_orcid, "geo_entities_added_orcid.geojson", append = FALSE)

# ── CHECKPOINT / RESTART POINT ──
geo_entities_added_orcid <- read_sf("geo_entities_added_orcid.geojson")

# Merge new geocoded entities into main set
entities_osm <- entities_osm %>%
  drop_na() %>%
  rbind(geo_entities_added_orcid) %>%
  unique() %>%
  write_sf("entities_osm.geojson", append = FALSE)

# ── CHECKPOINT / RESTART POINT ──
entities_osm <- read_sf("entities_osm.geojson") %>%
  filter(!text %in% dict_not_locations$X1)

# Apply bounding box filter to find Pacific Asia entities
entities_osm_PA <- entities_osm[st_within(entities_osm, bbox, sparse = FALSE), ]
rm(entities_osm)

orcid_matched_by_spacy <- match_entities_to_PA(entities_orcid_abstracts, entities_osm_PA$text)

write_csv(orcid_matched_by_spacy, "orcid_matched_by_spacy.csv")

# ── CHECKPOINT / RESTART POINT ──
orcid_matched_by_spacy <- read_csv("orcid_matched_by_spacy.csv", show_col_types = F)

# Summary statistics
nrow(orcid_matched_by_spacy %>% filter(!id %in% orcid_matched_by_regex$id) %>% select(id) %>% unique())
nrow(orcid_matched_by_regex %>% filter(!id %in% orcid_matched_by_spacy$id) %>% select(id) %>% unique())
nrow(orcid_matched_by_regex %>% filter(id %in% orcid_matched_by_spacy$id) %>% select(id) %>% unique())

# Join ORCID/Crossref regex & NLP matches
complete_works_orcid_PA_spacy <- complete_works_orcid %>%
  mutate(orcid = str_extract(path, "(?<=/)[^/]+(?=/)")) %>%
  select(id, orcid, doi, type, title_title_value, `publication-date.year.value`, `journal-title.value`, url_value, path) %>%
  filter(id %in% orcid_matched_by_spacy$id)

complete_works_orcid_PA_joined <- orcid_matched_by_regex %>% select(colnames(complete_works_orcid_PA_spacy)) %>%
  mutate(source = "ORCiD") %>%
  rbind(complete_works_orcid_PA_spacy %>%
          mutate(source = "ORCiD, Crossref")) %>%
  rename(publication_date_year_value = `publication-date.year.value`,
         journal_title_value = `journal-title.value`) %>%
  unique()

write_csv(complete_works_orcid_PA_joined, "complete_works_orcid_PA_joined.csv")

# ── CHECKPOINT / RESTART POINT ──
complete_works_orcid_PA_joined <- read_csv("complete_works_orcid_PA_joined.csv", show_col_types = F)


# ══════════════════════════════════════════════
# SECTION 16: Merge — OpenAlex + ORCID Datasets
# ──────────────────────────────────────────────
# Purpose: Combine OpenAlex and ORCID publication/entity
#          datasets into unified tables.
# Input:   complete_works_PA_alex.csv,
#          complete_works_orcid_PA_joined.csv,
#          entities from both pipelines
# Output:  complete_works_PA_preview.csv,
#          entities_complete.csv,
#          complete_researchers_PA_preview.csv
# ══════════════════════════════════════════════

complete_works_PA <- (complete_works_PA_alex %>%
                        mutate(source = "OpenAlex") %>%
                        select(colnames(complete_works_orcid_PA_joined), label)
) %>%
  rbind(complete_works_orcid_PA_joined %>%
          mutate(label = str_extract(title_title_value, "^\\S+(\\s+\\S+){0,3}")) %>%
          mutate(label = if_else(str_count(title_title_value, "\\s+") > 3,
                                 paste0(label, "\u2026"),
                                 label))) %>%
  select(-path, -url_value)

write_csv(complete_works_PA, "complete_works_PA_preview.csv")

# ── CHECKPOINT / RESTART POINT ──
complete_works_PA <- read_csv("complete_works_PA_preview.csv", show_col_types = F)

# Add ORCID/Crossref entity keywords — apply shared entity filter
entities_orcid_abstracts_PA <- entities_orcid_abstracts %>%
  filter(id %in% orcid_matched_by_spacy$id) %>%
  filter_entities()

entities_orcid_regex_PA <- orcid_matched_by_regex %>%
  select(id, matched_keyword) %>%
  rename(text = matched_keyword) %>%
  mutate(text = str_to_title(text)) %>%
  mutate(ent_type = "LOC") %>%
  unique()

# Combine all entity sources
entities_complete <- entities_alex_abstracts_PA %>%
  rbind(entities_orcid_abstracts_PA %>% select(text, id, ent_type)) %>%
  rbind(entities_orcid_regex_PA)

entities_complete$text <- sub("^(The|the|A|a)\\s+", "", entities_complete$text)
entities_complete$text <- sub("\\s+of$", "", entities_complete$text)

entities_complete <- entities_complete %>%
  filter(nchar(text) > 3) %>%
  filter(!sapply(strsplit(text, "\\s+"), function(words) any(nchar(words) == 1))) %>%
  filter(!grepl("\\bet al\\b", text, ignore.case = TRUE)) %>%
  filter(!grepl("^[A-Z]{1}\\.?[A-Z]{1}\\.?$", text, ignore.case = TRUE)) %>%
  filter(!grepl("^[A-Z]{2}$", gsub("\\.", "", text, ignore.case = TRUE))) %>%
  unique()

# Expand keyword stems to full display forms for LOC entities
entities_complete <- entities_complete %>%
  mutate(
    text = if_else(
      ent_type == "LOC",
      text %>%
        str_replace_all("\\bChines\\b", "Chinese") %>%
        str_replace_all("\\bsinolog\\b", "Sinology") %>%
        str_replace_all("\\bJapanes\\b", "Japanese") %>%
        str_replace_all("\\bJapanis\\b", "Japanisch") %>%
        str_replace_all("\\bTaiwanes\\b", "Taiwanese") %>%
        str_replace_all("\\bTaiwanis\\b", "Taiwanisch") %>%
        str_replace_all("\\bBurmes\\b", "Burmese") %>%
        str_replace_all("\\bVietnames\\b", "Vietnamese") %>%
        str_replace_all("\\bPhilippin\\b", "Philippines"),
      text
    )
  )

entities_complete <- entities_complete %>% unique()

write_csv(entities_complete, "entities_complete.csv")

# ── CHECKPOINT / RESTART POINT ──
entities_complete <- read_csv("entities_complete.csv", show_col_types = F)

# Get ORCID info on all available researchers
orcids_to_add <- complete_researchers_PA_alex %>%
  select(orcid) %>%
  rbind(complete_works_orcid_PA_joined %>%
          select(orcid)) %>%
  unique()

researchers_to_add <- complete_researchers_orcid %>%
  filter(orcid %in% orcids_to_add$orcid) %>%
  mutate(last = sub(".*\\s(\\S+)$", "\\1", name_value)) %>%
  mutate(first = sub("\\s\\S+$", "", name_value)) %>%
  left_join(faculties, by = c("organization_name", "department_name"), relationship = "many-to-many") %>%
  select(colnames(complete_researchers_PA_alex)) %>%
  unique()

complete_researchers_PA <- complete_researchers_PA_alex %>%
  rbind(researchers_to_add) %>%
  unique()

write_csv(complete_researchers_PA, "complete_researchers_PA_preview.csv")


# ══════════════════════════════════════════════
# SECTION 16a: Auto-detect Same-Organisation Variants (LLM)
# ──────────────────────────────────────────────
# Purpose: Surface candidate org-name pairs via three
#          complementary generators (Jaro-Winkler,
#          acronym signature, shared place token + token
#          overlap), then ALWAYS verify each pair with a
#          local Ollama LLM regardless of generator. The
#          confirmed pairs augment universities.csv at
#          the Section 17 join (manual file wins on
#          conflict).
# Input:   complete_researchers_PA_preview.csv
# Output:  org_merges_ollama.csv (incremental cache of
#          short, long, llm_confirmed)
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
complete_researchers_PA <- read_csv("complete_researchers_PA_preview.csv", show_col_types = F)

# Token splitter used by every generator: lowercase, split on whitespace/dashes/slashes/commas,
# drop empty pieces and stopwords.
.org_tokens_of <- function(name) {
  if (is.na(name) || !nzchar(name)) return(character(0))
  toks <- str_split(tolower(name), "[\\s\\-/,]+")[[1]]
  toks[nzchar(toks) & !toks %in% tolower(config$org_stopwords)]
}

# Acronym signature: an all-caps token (>= 2 chars) is kept verbatim;
# every other token contributes its uppercase first letter.
.org_acronym <- function(name) {
  if (is.na(name) || !nzchar(name)) return("")
  toks <- str_split(name, "[\\s\\-/,]+")[[1]]
  toks <- toks[nzchar(toks) & !tolower(toks) %in% config$org_stopwords]
  pieces <- vapply(toks, function(t) {
    if (str_detect(t, "^[A-ZÄÖÜ]{2,}$")) t
    else toupper(substr(t, 1, 1))
  }, character(1))
  paste0(pieces, collapse = "")
}

distinct_orgs <- complete_researchers_PA %>%
  filter(!is.na(organization_name), nzchar(organization_name)) %>%
  distinct(organization_name) %>%
  pull(organization_name)
message(sprintf("Section 16a: %d distinct organisation names to compare", length(distinct_orgs)))

# Pre-compute per-org features
sig_acro   <- vapply(distinct_orgs, .org_acronym, character(1))
sig_tokens <- lapply(distinct_orgs, .org_tokens_of)
sig_lower  <- tolower(distinct_orgs)
place_lc   <- tolower(config$org_place_tokens)
sig_places <- lapply(sig_tokens, function(t) intersect(t, place_lc))

# ─── Generator 1: Jaro-Winkler ≥ threshold (vectorised matrix) ───
message("  G1: computing pairwise Jaro-Winkler …")
jw_mat <- 1 - stringdist::stringdistmatrix(sig_lower, sig_lower, method = "jw", p = 0.1)
jw_hits <- which(jw_mat >= config$org_fuzzy_threshold & upper.tri(jw_mat), arr.ind = TRUE)
g1 <- if (nrow(jw_hits) > 0) {
  tibble(i = jw_hits[, "row"], j = jw_hits[, "col"])
} else {
  tibble(i = integer(), j = integer())
}
rm(jw_mat, jw_hits)

# ─── Generator 2: acronym signature equality / prefix ───
message("  G2: acronym signature matching …")
g2 <- list()
keep_acro <- which(nchar(sig_acro) >= 2)
for (i in keep_acro) {
  a <- sig_acro[i]
  cand <- keep_acro[keep_acro != i &
                    (sig_acro[keep_acro] == a |
                     startsWith(sig_acro[keep_acro], a) |
                     startsWith(a, sig_acro[keep_acro]))]
  if (length(cand) > 0) {
    g2[[length(g2) + 1]] <- tibble(i = pmin(i, cand), j = pmax(i, cand))
  }
}
g2 <- if (length(g2)) bind_rows(g2) %>% distinct() else tibble(i = integer(), j = integer())

# ─── Generator 3: shared place token + at least one substantive shared non-place token ───
message("  G3: shared place + token overlap …")
generic <- c("university", "universität", "universitat", "institute", "institut",
             "zentrum", "centre", "center", "hochschule", "research", "forschung",
             "gmbh", "ev", "e.v")
g3 <- list()
for (place in place_lc) {
  in_place <- which(vapply(sig_places, function(p) place %in% p, logical(1)))
  if (length(in_place) < 2) next
  cmb <- combn(in_place, 2)
  keep <- apply(cmb, 2, function(pair) {
    ti <- setdiff(sig_tokens[[pair[1]]], place_lc)
    tj <- setdiff(sig_tokens[[pair[2]]], place_lc)
    common <- intersect(ti[nchar(ti) >= 4], tj[nchar(tj) >= 4])
    length(setdiff(common, generic)) > 0
  })
  if (any(keep)) {
    g3[[length(g3) + 1]] <- tibble(i = cmb[1, keep], j = cmb[2, keep])
  }
}
g3 <- if (length(g3)) bind_rows(g3) %>% distinct() else tibble(i = integer(), j = integer())

# ─── Union of all generators ───
all_pairs <- bind_rows(g1, g2, g3) %>% distinct()
message(sprintf("  Candidates: G1=%d  G2=%d  G3=%d  union=%d",
                nrow(g1), nrow(g2), nrow(g3), nrow(all_pairs)))

# Cast (i, j) -> (shorter name, longer name) for stable cache keys
cand_df <- if (nrow(all_pairs) > 0) {
  a <- distinct_orgs[all_pairs$i]
  b <- distinct_orgs[all_pairs$j]
  ord_short <- nchar(a) <= nchar(b)
  tibble(short = ifelse(ord_short, a, b),
         long  = ifelse(ord_short, b, a)) %>% distinct()
} else tibble(short = character(), long = character())

# Load cached LLM verdicts
cache <- if (file.exists(config$org_merge_cache)) {
  read_csv(config$org_merge_cache, show_col_types = FALSE)
} else {
  tibble(short = character(), long = character(), llm_confirmed = logical())
}

todo <- cand_df %>% anti_join(cache, by = c("short", "long"))
message(sprintf("  Cached: %d | New (to verify): %d", nrow(cache), nrow(todo)))

if (nrow(todo) > 0) {
  sys_prompt <- paste0(
    "You are an expert on German research organisations. ",
    "For each numbered pair, answer YES only if the two names refer to the ",
    "EXACT SAME organisation (abbreviations, alternative spellings, ",
    "language variants, and harmonised punctuation are allowed). Answer NO ",
    "if they are distinct organisations, even if closely related (e.g. ",
    "sister institutes, branches, or departments of the same parent). ",
    "Reply with ONLY '<number>. YES' or '<number>. NO', one per line. ",
    "Example:\n1. YES\n2. NO"
  )
  batch <- as.integer(config$org_llm_batch_size)
  n <- nrow(todo)
  start_time <- Sys.time()
  for (b0 in seq.int(1L, n, by = batch)) {
    chunk <- todo[b0:min(b0 + batch - 1L, n), ]
    pairs <- mapply(c, chunk$short, chunk$long, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    chunk$llm_confirmed <- classify_pair_batch_ollama(pairs, sys_prompt)
    cache <- bind_rows(cache, chunk %>% select(short, long, llm_confirmed))
    write_csv(cache, config$org_merge_cache)

    done <- min(b0 + batch - 1L, n)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    rate <- done / max(elapsed, 0.01)
    eta  <- (n - done) / rate
    message(sprintf("  [%d/%d] %.1f min elapsed, ~%.1f min remaining (confirmed so far: %d)",
                    done, n, elapsed, eta,
                    sum(cache$llm_confirmed %in% TRUE)))
  }
}

message(sprintf("Section 16a: %d confirmed same-org pairs in %s",
                sum(cache$llm_confirmed %in% TRUE), config$org_merge_cache))


# ══════════════════════════════════════════════
# SECTION 16b: Auto-detect Organisation Hierarchy (LLM)
# ──────────────────────────────────────────────
# Purpose: Detect parent/child relationships among
#          organisations. Candidate parents are orgs whose
#          token sequence is a contiguous subsequence of
#          the child's tokens. Every candidate is
#          LLM-verified. For each child the LONGEST
#          confirmed parent is taken as the DIRECT parent,
#          and multi-level depth emerges by chaining parent
#          links. Output is a separate artifact (does NOT
#          mutate flat researcher rows).
# Input:   complete_researchers_PA_preview.csv,
#          (optional) org_merges_ollama.csv for canonical
#          name normalisation prior to hierarchy detection
# Output:  org_hierarchy_ollama.csv (parent, child, llm_confirmed)
#          (resolution to a single direct parent happens
#           inside Section 17 where the hierarchy is
#           applied to researcher rows)
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
complete_researchers_PA <- read_csv("complete_researchers_PA_preview.csv", show_col_types = F)

distinct_orgs <- complete_researchers_PA %>%
  filter(!is.na(organization_name), nzchar(organization_name)) %>%
  distinct(organization_name) %>%
  pull(organization_name)

# Optional: fold confirmed Step-16a merges so canonical names are used everywhere
if (file.exists(config$org_merge_cache)) {
  merges <- read_csv(config$org_merge_cache, show_col_types = FALSE) %>%
    filter(llm_confirmed %in% TRUE) %>%
    distinct(short, long)
  if (nrow(merges) > 0) {
    remap <- setNames(merges$long, merges$short)
    distinct_orgs <- unique(ifelse(distinct_orgs %in% names(remap),
                                   unname(remap[distinct_orgs]),
                                   distinct_orgs))
  }
}
message(sprintf("Section 16b: %d distinct (post-merge) organisations", length(distinct_orgs)))

# Token sequences, lowercased, stopwords removed (preserve order — token order matters here)
tok_of <- lapply(distinct_orgs, function(s) {
  toks <- str_split(tolower(s), "[\\s\\-/,]+")[[1]]
  toks[nzchar(toks) & !toks %in% tolower(config$org_stopwords)]
})

# Index by FIRST token so the parent search is bucketed.
first_tok <- vapply(tok_of, function(t) if (length(t)) t[1] else NA_character_, character(1))
buckets <- split(seq_along(distinct_orgs), first_tok)

is_subseq <- function(small, big) {
  ns <- length(small); nb <- length(big)
  if (ns == 0 || ns >= nb) return(FALSE)
  for (k in seq_len(nb - ns + 1L)) {
    if (identical(big[k:(k + ns - 1L)], small)) return(TRUE)
  }
  FALSE
}

# For each child, candidate parents are orgs whose first token appears anywhere
# inside the child's token sequence — much cheaper than scanning all orgs.
candidates <- list()
n_orgs <- length(distinct_orgs)
for (j in seq_len(n_orgs)) {
  child_toks <- tok_of[[j]]
  if (length(child_toks) < 2) next
  for (k in seq_along(child_toks)) {
    bk <- buckets[[child_toks[k]]]
    if (is.null(bk)) next
    for (i in bk) {
      if (i == j) next
      pt <- tok_of[[i]]
      if (length(pt) >= length(child_toks)) next
      if (is_subseq(pt, child_toks)) {
        candidates[[length(candidates) + 1L]] <-
          c(parent = distinct_orgs[i], child = distinct_orgs[j])
      }
    }
  }
  if (j %% 500 == 0) message(sprintf("  Hierarchy scan: %d/%d, %d candidates so far",
                                     j, n_orgs, length(candidates)))
}

cand_df <- if (length(candidates)) {
  do.call(rbind, lapply(candidates, function(p) tibble(parent = unname(p["parent"]),
                                                       child  = unname(p["child"])))) %>%
    distinct()
} else tibble(parent = character(), child = character())
message(sprintf("Section 16b: %d candidate parent/child pairs", nrow(cand_df)))

cache <- if (file.exists(config$org_hierarchy_cache)) {
  read_csv(config$org_hierarchy_cache, show_col_types = FALSE)
} else {
  tibble(parent = character(), child = character(), llm_confirmed = logical())
}

todo <- cand_df %>% anti_join(cache, by = c("parent", "child"))
message(sprintf("  Cached: %d | New (to verify): %d", nrow(cache), nrow(todo)))

if (nrow(todo) > 0) {
  sys_prompt <- paste0(
    "You are an expert on German research organisations and their structure. ",
    "For each numbered pair (Parent: X | Child: Y), answer YES only if Y is a ",
    "subsidiary, faculty, institute, department, branch, or research centre of ",
    "X. Answer NO if X and Y are independent organisations that merely share a ",
    "name token (for instance, two unrelated institutes both located in the ",
    "same city). Reply with ONLY '<number>. YES' or '<number>. NO', one per ",
    "line. Example:\n1. YES\n2. NO"
  )
  fmt <- function(parent, child) sprintf("Parent: %s | Child: %s", parent, child)

  batch <- as.integer(config$org_llm_batch_size)
  n <- nrow(todo)
  start_time <- Sys.time()
  for (b0 in seq.int(1L, n, by = batch)) {
    chunk <- todo[b0:min(b0 + batch - 1L, n), ]
    pairs <- mapply(c, chunk$parent, chunk$child, SIMPLIFY = FALSE, USE.NAMES = FALSE)
    chunk$llm_confirmed <- classify_pair_batch_ollama(pairs, sys_prompt, user_format = fmt)
    cache <- bind_rows(cache, chunk %>% select(parent, child, llm_confirmed))
    write_csv(cache, config$org_hierarchy_cache)

    done <- min(b0 + batch - 1L, n)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    rate <- done / max(elapsed, 0.01)
    eta  <- (n - done) / rate
    message(sprintf("  [%d/%d] %.1f min elapsed, ~%.1f min remaining (confirmed so far: %d)",
                    done, n, elapsed, eta,
                    sum(cache$llm_confirmed %in% TRUE)))
  }
}

message(sprintf("Section 16b: %d confirmed parent/child pairs cached in %s",
                sum(cache$llm_confirmed %in% TRUE), config$org_hierarchy_cache))


# ══════════════════════════════════════════════
# SECTION 17: Data Cleaning — Researchers
# ──────────────────────────────────────────────
# Purpose: Clean and harmonise researcher records:
#          names, cities, organisations, departments,
#          faculty assignments, employment windows.
# Input:   complete_researchers_PA_preview.csv,
#          complete_works_PA_preview.csv
# Output:  (in-memory cleaned tables for final export)
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
# Requires: Sections 1-5, insts_chikon.rds, insts_de.rds
complete_works_PA <- read_csv("complete_works_PA_preview.csv", show_col_types = F)
complete_researchers_PA <- read_csv("complete_researchers_PA_preview.csv", show_col_types = F)

# Filter to researchers affiliated with relevant institutions
complete_researchers_PA <- complete_researchers_PA %>%
  group_by(orcid) %>%
  filter(any(str_detect(department_name, str_c(c(insts_chikon$display_name, institutions), collapse = "|")) |
               str_detect(organization_name, str_c(c(insts_chikon$display_name, institutions), collapse = "|")))) %>%
  ungroup()

complete_researchers_PA <- complete_researchers_PA %>%
  group_by(orcid, organization_name, department_name) %>%
  arrange(-start_date_year_value) %>%
  slice(1) %>%
  ungroup()

# Clean name formatting
complete_researchers_PA <- complete_researchers_PA %>%
  mutate(
    last = str_remove(last, "^Dr.-Ing.\\s+"),
    last = str_remove(last, ",\\s*Dr\\.$"),
    last = str_remove(last, ",\\s*Prof\\.$"),
    last = str_trim(last)
  ) %>%
  unique()

complete_researchers_PA <- complete_researchers_PA %>%
  mutate(
    first = str_trim(as.character(first)),
    last = str_trim(as.character(last))
  )

# Fix city name variants
complete_researchers_PA <- complete_researchers_PA %>%
  mutate(organization_address_city = case_match(
    organization_address_city,
    "Rostock Germany"    ~ "Rostock",
    "Flensburg, Germany" ~ "Flensburg",
    "Beijing/Hamburg"    ~ "Hamburg",
    .default = organization_address_city
  ))

complete_researchers_PA$name_value <- str_to_title(complete_researchers_PA$name_value)
complete_researchers_PA <- complete_researchers_PA %>%
  mutate(name_value = str_remove(name_value, "^Dr.-Ing.\\s+")) %>%
  unique()

# Within each org, drop NA departments if named ones exist
complete_researchers_PA <- complete_researchers_PA %>%
  group_by(orcid, organization_name) %>%
  filter(!(is.na(department_name) & any(!is.na(department_name)))) %>%
  ungroup()

# Harmonise org/dept names via lookup tables.
# Combine manual universities.csv with auto-confirmed pairs from Section 16a;
# the manual file always wins on conflict.
#
# GUARD (2026-08): the Section-16a fuzzy/acronym generators occasionally link
# two unrelated institutions that merely share a name fragment (e.g. the acronym
# prefix "UM" of "University of Münster" matches "University Medical Center …").
# Because the lookup below is iterated to a FIXED POINT, one bad edge lets whole
# chains of unrelated universities collapse into a single sink — observed:
# Trier → Münster → UKE → Universitätsklinikum Hamburg-Eppendorf, silently
# rewriting correct affiliations. We therefore (a) keep an auto-merge only when
# its two names share a substantive (non-generic) token, and (b) stop the walk
# the moment a hop would drift to a name sharing nothing with the ORIGINAL org.
# Manual universities.csv edges stay trusted (they cover legitimate acronym
# expansions such as UKE → Universitätsklinikum Hamburg-Eppendorf).
.org_generic <- c("university","universitat","universitaet","universities","universität",
  "hochschule","hospital","klinik","klinikum","clinic","clinical","medical","medicine",
  "center","centre","zentrum","institute","institut","research","forschung","school",
  "college","faculty","fakultat","fakultät","department","abteilung","gmbh","foundation",
  "stiftung","academy","akademie","laboratory","laboratorium")
.org_subtok <- function(s) {
  t <- str_split(tolower(ifelse(is.na(s), "", s)), "[^a-z0-9äöüß]+")[[1]]
  unique(t[nchar(t) >= 4 & !t %in% .org_generic])
}
.org_share <- function(a, b) length(intersect(.org_subtok(a), .org_subtok(b))) > 0

universities_combined <- universities
if (file.exists(config$org_merge_cache)) {
  auto_merges <- read_csv(config$org_merge_cache, show_col_types = FALSE) %>%
    filter(llm_confirmed %in% TRUE) %>%
    distinct(short, long) %>%
    rename(organization_name = short,
           organization_name_harmonised = long) %>%
    anti_join(universities, by = "organization_name") %>%
    # Drop fuzzy merges whose two names share no substantive token (GUARD above).
    filter(mapply(.org_share, organization_name, organization_name_harmonised))
  if (nrow(auto_merges) > 0) {
    # Manual file wins, and keep exactly ONE mapping per org key. Without the
    # distinct(), universities_combined held ~600 duplicate keys and the join
    # below (many-to-many) fanned every researcher row out across them, exploding
    # the table and SCATTERING org names instead of merging them.
    universities_combined <- bind_rows(universities, auto_merges) %>%
      distinct(organization_name, .keep_all = TRUE)
    message(sprintf("Section 17: %d auto-confirmed merges added (universities.csv had %d)",
                    nrow(auto_merges), nrow(universities)))
  }
}

# Resolve each org name to its canonical form by iterating the lookup to a fixed
# point. A single join is not enough: universities.csv uses a SHORT canonical
# ("CAU Kiel") while org_merges_ollama.csv points variants at a LONG name, so one
# hop strands variants at the long name (e.g. "Christian-Albrechts-Universität zu
# Kiel"). Treating the manual canonicals as terminal collapses those chains AND
# breaks the short<->long cycles the two sources form together.
org_remap <- setNames(universities_combined$organization_name_harmonised,
                      universities_combined$organization_name)
org_remap <- org_remap[!is.na(org_remap) &
                       !names(org_remap) %in% universities$organization_name_harmonised]
# Trusted manual edges (always followed, even when the two names share no token,
# e.g. an acronym and its expansion).
.curated_keys <- paste0(universities$organization_name, " @@> ",
                        universities$organization_name_harmonised)
# Walk the lookup to a fixed point, but stop if a hop would drift to a name that
# shares no substantive token with the ORIGINAL org (unless it is a curated edge).
# This keeps legitimate multi-hop collapses — every variant of one institution
# shares a token — while blocking the fuzzy chains that funnel unrelated
# universities into one sink.
resolve_org <- function(org, remap, max_iter = 20L) {
  start <- org
  for (i in seq_len(max_iter)) {
    if (is.na(org) || !(org %in% names(remap))) break
    nxt <- unname(remap[[org]])
    if (!(paste0(org, " @@> ", nxt) %in% .curated_keys) && !.org_share(start, nxt)) break
    org <- nxt
  }
  org
}
# Resolve once per distinct name (cheap) and map the result onto every row.
.org_canon <- unique(complete_researchers_PA$organization_name)
.org_canon <- setNames(vapply(.org_canon, resolve_org, character(1), remap = org_remap),
                       .org_canon)
complete_researchers_PA <- complete_researchers_PA %>%
  mutate(organization_name = unname(.org_canon[organization_name]))

# Apply auto-detected hierarchy (Section 16b) AFTER universities-harmonisation so the
# parent/child lookups use canonical names. For each row whose org has a confirmed
# parent (longest match), promote: organization_name -> parent; the child name slides
# into department_name iff that slot was empty. Iterate so multi-level chains
# (A -> B -> C) collapse to the root in one pass.
if (file.exists(config$org_hierarchy_cache)) {
  hierarchy <- read_csv(config$org_hierarchy_cache, show_col_types = FALSE) %>%
    filter(llm_confirmed %in% TRUE) %>%
    mutate(plen = nchar(parent)) %>%
    group_by(child) %>%
    arrange(desc(plen), .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(child, parent)

  if (nrow(hierarchy) > 0) {
    repeat {
      before <- complete_researchers_PA$organization_name
      complete_researchers_PA <- complete_researchers_PA %>%
        left_join(hierarchy, by = c("organization_name" = "child")) %>%
        mutate(
          department_name = ifelse(!is.na(parent) & (is.na(department_name) | !nzchar(department_name)),
                                   organization_name,
                                   department_name),
          organization_name = ifelse(!is.na(parent), parent, organization_name)
        ) %>%
        select(-parent)
      if (identical(before, complete_researchers_PA$organization_name)) break
    }
    message(sprintf("Section 17: hierarchy promotion applied (%d parent/child rules)",
                    nrow(hierarchy)))
  }
}

complete_researchers_PA <- complete_researchers_PA %>%
  left_join(departments, by = c("organization_name", "department_name"), relationship = "many-to-many") %>%
  mutate(department_name =
           ifelse(!is.na(department_name_harmonised),
                  department_name_harmonised,
                  department_name)) %>%
  select(-department_name_harmonised)

complete_researchers_PA <- complete_researchers_PA %>%
  mutate(department_name = ifelse(department_name == organization_name,
                                  NA, department_name))

complete_researchers_PA <- complete_researchers_PA %>%
  group_by(across(all_of(setdiff(names(.), c("last", "first"))))) %>%
  arrange(last, first) %>%
  slice(1) %>%
  ungroup() %>%
  unique()

# Drop garbage department_name values: some ORCID records carry a sentence
# fragment or project-title snippet here (e.g. "w elche Medizin der
# Unsterblichkeit"). Keep a value only if it has a typical academic-unit word, or
# is short and clean; blank it when it has no unit word AND is either very long
# (a sentence) or contains a lone lowercase letter (a word-break artifact).
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
complete_researchers_PA <- complete_researchers_PA %>%
  mutate(department_name = {
    xt <- trimws(as.character(department_name))
    bad <- !is.na(department_name) & nzchar(xt) &
      !grepl(.dept_unit_words, xt, ignore.case = TRUE) &
      (str_count(xt, "\\S+") > 10 | grepl("(^|\\s)[b-df-hj-np-tv-z](\\s|$)", xt))
    ifelse(bad, NA_character_, department_name)
  })

# Latest affiliation snapshot. Rows whose organisation is blank/"NA" are sorted
# last within each ORCID, so slice(1) takes the most recent employment that
# actually carries an organisation (falling back to a blank one only if none
# exists) — otherwise the "letztdokumentierte Einrichtung" can come out empty.
complete_researchers_PA_latest <- complete_researchers_PA %>%
  group_by(orcid) %>%
  arrange(is.na(organization_name) | organization_name == "NA" | trimws(organization_name) == "",
          -start_date_year_value, desc(organization_address_city)) %>%
  slice(1) %>%
  ungroup()

# Faculty assignment chain: manual lookup -> backfill within orcid -> Ollama LLM
complete_researchers_PA <- complete_researchers_PA %>%
  select(-faculties) %>%
  left_join(faculties, by = c("organization_name", "department_name"), relationship = "many-to-many")

complete_researchers_PA <- complete_researchers_PA %>%
  mutate(faculties = ifelse(faculties == "NA.", NA, faculties)) %>%
  unique()

# Backfill within ORCID: if one affiliation row has a faculty, share it
complete_researchers_PA <- complete_researchers_PA %>%
  group_by(orcid) %>%
  mutate(faculties = ifelse(is.na(faculties), first(faculties[!is.na(faculties)]), faculties)) %>%
  ungroup()

complete_researchers_PA$faculties[complete_researchers_PA$faculties == "tecvh"] <- "tech"

# ── Ollama LLM faculty classification ──────────────────────────────────────────
# Classify every researcher with the LLM based on their publication titles,
# regardless of whether they already have a manually-assigned faculty.
# The prediction is kept in `predicted_faculties` (separate column) so it
# can later be compared against the existing `faculties` value.
# Previously predicted ORCIDs are cached in faculties_ollama.csv so the
# LLM is only called for genuinely new researchers.

# Predict for every distinct ORCID
all_orcids <- complete_researchers_PA %>% distinct(orcid)
message(sprintf("Researchers to classify (all): %d", nrow(all_orcids)))

# Load cached predictions from previous runs
cached_predictions <- if (file.exists(config$ollama_cache)) {
  read_csv(config$ollama_cache, show_col_types = FALSE) %>%
    select(orcid, predicted_faculties, prediction_confidence)
} else {
  tibble(orcid = character(), predicted_faculties = character(),
         prediction_confidence = numeric())
}

# Determine which ORCIDs still need a fresh LLM call
already_predicted <- all_orcids %>% filter(orcid %in% cached_predictions$orcid)
to_predict        <- all_orcids %>% filter(!orcid %in% cached_predictions$orcid)
message(sprintf("  Cached: %d | New (to classify): %d", nrow(already_predicted), nrow(to_predict)))

if (nrow(to_predict) > 0) {
  # Build per-researcher title strings from the works table
  researcher_titles <- complete_works_PA %>%
    filter(orcid %in% to_predict$orcid,
           !is.na(title_title_value) & title_title_value != "") %>%
    group_by(orcid) %>%
    summarise(titles = paste(title_title_value, collapse = " "), .groups = "drop")

  to_predict <- to_predict %>% left_join(researcher_titles, by = "orcid")

  new_predictions <- vector("list", nrow(to_predict))
  start_time <- Sys.time()

  for (i in seq_len(nrow(to_predict))) {
    row <- to_predict[i, ]

    if (is.na(row$titles) || trimws(row$titles) == "") {
      new_predictions[[i]] <- tibble(
        orcid = row$orcid, predicted_faculties = NA_character_,
        prediction_confidence = NA_real_
      )
      next
    }

    result <- classify_faculty_ollama(row$titles)

    new_predictions[[i]] <- tibble(
      orcid                 = row$orcid,
      predicted_faculties   = if (!is.null(result)) result$faculty    else NA_character_,
      prediction_confidence = if (!is.null(result)) result$confidence else NA_real_
    )

    if (i %% 5 == 0 || i == nrow(to_predict)) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      rate <- i / max(elapsed, 0.01)
      eta  <- (nrow(to_predict) - i) / rate
      message(sprintf("[%d/%d] %.1f min elapsed, ~%.1f min remaining", i, nrow(to_predict), elapsed, eta))
    }
  }

  new_preds_df <- bind_rows(new_predictions)

  # Append new predictions to the cache and save
  cached_predictions <- bind_rows(cached_predictions, new_preds_df)
  write_csv(cached_predictions, config$ollama_cache)
  message(sprintf("Saved %d total cached predictions to %s", nrow(cached_predictions), config$ollama_cache))
}

# Merge predictions into researchers table as a SEPARATE column AND
# supplement `faculties` with the LLM prediction where the manual / lookup
# value is missing. Both columns are kept so the manual vs. predicted values
# can still be diffed downstream.
complete_researchers_PA <- complete_researchers_PA %>%
  left_join(
    cached_predictions %>% select(orcid, predicted_faculties, prediction_confidence),
    by = "orcid"
  ) %>%
  mutate(faculties = ifelse(
    (is.na(faculties) | faculties %in% c("NA", "NA.")) & !is.na(predicted_faculties),
    predicted_faculties,
    faculties
  ))

complete_researchers_PA$faculties[is.na(complete_researchers_PA$faculties)] <- "NA"
# ── End Ollama classification ──────────────────────────────────────────────────

# ── Per-researcher manual faculty override ──────────────────────────────────────
# A curated researcher-level faculty table, each entry assigned from the
# researcher's own publication record. It takes priority over both the
# organisation-department concordance and the title-model fallback, and is the
# appropriate source where a researcher has no department or the organisation
# name resolves ambiguously across faculties.
if (file.exists("researcher_faculties.csv")) {
  researcher_faculties <- read_csv("researcher_faculties.csv", show_col_types = FALSE) %>%
    filter(!is.na(orcid), !is.na(faculties), faculties != "") %>%
    distinct(orcid, .keep_all = TRUE) %>%
    transmute(orcid, override_faculties = faculties)
  complete_researchers_PA <- complete_researchers_PA %>%
    left_join(researcher_faculties, by = "orcid") %>%
    mutate(faculties = ifelse(!is.na(override_faculties), override_faculties, faculties)) %>%
    select(-override_faculties)
}

# Force valid UTF-8 encoding
complete_researchers_PA[] <- lapply(complete_researchers_PA, function(x) {
  if (is.character(x)) iconv(x, from = "UTF-8", to = "UTF-8") else x
})
complete_researchers_PA_latest[] <- lapply(complete_researchers_PA_latest, function(x) {
  if (is.character(x)) iconv(x, from = "UTF-8", to = "UTF-8") else x
})

# City-based institution filter
city_mapping <- list(
  bremen     = "Bremen",
  clausthal  = c("Clausthal", "Clausthal-Zellerfeld"),
  flensburg  = "Flensburg",
  greifswald = "Greifswald",
  hamburg    = "Hamburg",
  kiel       = "Kiel",
  luebeck    = "Lübeck",
  lueneburg  = "Lüneburg",
  oldenburg  = "Oldenburg",
  rostock    = "Rostock"
)

pattern <- paste(unlist(city_mapping), collapse = "|")
filtered_researchers <- complete_researchers_PA %>%
  filter(grepl(pattern, organization_address_city, ignore.case = TRUE) |
           grepl(pattern, organization_name, ignore.case = TRUE))
complete_researchers_PA <- complete_researchers_PA %>%
  filter(orcid %in% filtered_researchers$orcid)

# Derive employment windows: start = this row's year, end = next row's year
# Sorted by orcid and start year so lead() gives the next position's start
complete_researchers_PA <- complete_researchers_PA %>%
  arrange(orcid, start_date_year_value) %>%
  group_by(orcid) %>%
  mutate(
    employment_start = start_date_year_value,
    employment_end   = lead(start_date_year_value, default = Inf)
  ) %>%
  ungroup() %>%
  mutate(employment_start = ifelse(is.na(employment_start), 0, employment_start)) %>%
  mutate(employment_end = ifelse(is.na(employment_end), Inf, employment_end))

# Try to assign ORCIDs to researchers without known ORCIDs:
# If name+org matches a known ORCID holder, use that ORCID
complete_researchers_PA <- complete_researchers_PA %>%
  group_by(name_value, organization_name) %>%
  mutate(
    replacement_orcid = first(orcid[!str_starts(orcid, "no_orcid")], default = NA)
  ) %>%
  ungroup()

# Build mapping of placeholder -> real orcids
orcid_map <- complete_researchers_PA %>%
  filter(str_starts(orcid, "no_orcid")) %>%
  filter(!is.na(replacement_orcid)) %>%
  select(orcid, replacement_orcid) %>% unique()

complete_researchers_PA <- complete_researchers_PA %>%
  mutate(
    orcid = if_else(str_starts(orcid, "no_orcid") & !is.na(replacement_orcid),
                    replacement_orcid,
                    orcid)) %>%
  select(-replacement_orcid)

complete_researchers_PA <- complete_researchers_PA %>%
  filter(last != "AUTHOR_ID")


# ══════════════════════════════════════════════
# SECTION 18: Data Cleaning — Publications
# ──────────────────────────────────────────────
# Purpose: Deduplicate and clean publication records.
# Input:   complete_works_PA (in memory)
# Output:  (in-memory cleaned table for final export)
# ══════════════════════════════════════════════

complete_works_PA <- complete_works_PA %>%
  group_by(orcid, type, title_title_value) %>%
  arrange(desc(!is.na(doi))) %>%
  filter(title_title_value != "" & !is.na(title_title_value)) %>%
  slice(1) %>%
  ungroup()

# Fuzzy dedup: compare first 4 words of lowercased title
complete_works_PA <- complete_works_PA %>%
  mutate(filter_title = tolower(title_title_value) %>%
           str_split(" ") %>%
           map_chr(~ str_c(head(.x, 4), collapse = " "))) %>%
  group_by(type, orcid, filter_title) %>%
  arrange(desc(!is.na(doi))) %>%
  slice(1) %>%
  ungroup() %>%
  select(-"filter_title")

complete_works_PA <- complete_works_PA %>%
  mutate(type = ifelse(type == "magazine-article", "journal-article", type))

# Deep-unescape HTML in titles
complete_works_PA <- complete_works_PA %>%
  mutate(title_title_value = purrr::map_chr(title_title_value, fully_unescape_twice)) %>%
  group_by(type, orcid, title_title_value) %>%
  slice(1) %>%
  ungroup()

# Map placeholder ORCIDs to discovered real ones
complete_works_PA <- complete_works_PA %>%
  left_join(
    orcid_map,
    by = "orcid", relationship = "many-to-many") %>%
  mutate(
    orcid = coalesce(replacement_orcid, orcid)
  ) %>%
  select(-replacement_orcid) %>%
  unique()

# Convert ALL-CAPS titles to Title Case
complete_works_PA <- complete_works_PA %>%
  mutate(
    title_title_value = if_else(
      str_detect(title_title_value, "^[A-Z0-9[:punct:] ]+$"),
      str_to_title(str_to_lower(title_title_value)),
      title_title_value
    )
  )


# ══════════════════════════════════════════════
# SECTION 19: Data Cleaning — Keywords & Geodata
# ──────────────────────────────────────────────
# Purpose: Clean entity keywords and build the final
#          geocoded dataset with country assignments.
# Input:   entities_complete, entities_osm_PA
# Output:  (in-memory cleaned tables for final export)
# ══════════════════════════════════════════════

complete_spacy_PA_keywords <- entities_complete %>%
  filter(!text %in% c("Herein", "Hintergrund", "Zusammenfassung", "Ziel", "Conclusion", "Conclusions", "Methods", "The")) %>%
  unique()

complete_spacy_PA_keywords <- complete_spacy_PA_keywords[
  !grepl("^\\d", complete_spacy_PA_keywords$text) &
    lengths(gregexpr("\\d", complete_spacy_PA_keywords$text)) <= 1,
]

# Geodata cleaning.
# IMPORTANT: decide each point's country/maritime category on its TRUE geocoded
# coordinates FIRST, and only afterward snap to the one-degree grid (needed
# downstream for dedup/clustering). Snapping before the spatial join pushes
# coastal and small-island points across the coastline, mislabelling genuine land
# places (e.g. Taipei, Hong Kong, Pearl River Delta) as "maritim" and inflating
# the maritime residual.
entities_osm_PA <- entities_osm_PA %>%
  filter(!text %in% sf_filter$X1) %>%
  st_make_valid()

complete_spacy_PA_geo <- st_as_sf(entities_osm_PA, crs = 4326)

# (1) categorise on the accurate coordinates
complete_spacy_PA_geo <- st_join(complete_spacy_PA_geo, sf_countries[, c("ADMIN")])
complete_spacy_PA_geo <- complete_spacy_PA_geo %>%
  mutate(ADMIN = if_else(lengths(st_within(complete_spacy_PA_geo, sf_countries)) == 0, "maritim", ADMIN))

# (2) category is now fixed — snap coordinates to the one-degree grid for the
# downstream dedup/merge (this no longer changes any ADMIN).
st_geometry(complete_spacy_PA_geo) <-
  st_make_valid(st_set_precision(st_geometry(complete_spacy_PA_geo), 1))

complete_spacy_PA_geo <- complete_spacy_PA_geo %>%
  mutate(text = str_to_title(text)) %>%
  filter(!is.na(text)) %>%
  # many-to-many is intended: a place name maps to every publication-entity row
  # that mentions it (one geo row per place-per-publication, for the map counts).
  left_join(entities_complete %>% filter(ent_type %in% c("GPE", "LOC")), by = "text",
            relationship = "many-to-many") %>%
  # Eastern China and Southwest China are macro-regions of the mainland: count them
  # as China. The other broad regions carry no usable point and are marked
  # "Allgemein" (kept, not plotted). Both groups have their geometry emptied below.
  mutate(ADMIN = case_when(
    text %in% c("Eastern China", "Southwest China") ~ "China",
    text %in% c("Far East Asia", "Southeast Asia", "East Asia", "Northeast Asia",
                "South-East Asia", "Eastern Asia", "Eastern Eurasia", "Southern Asia",
                "Inner Asia", "Western Central Asia") ~ "Allgemein",
    TRUE ~ ADMIN))

complete_spacy_PA_geo <- complete_spacy_PA_geo %>%
  mutate(
    geometry = case_when(
      text %in% c("Far East Asia", "Southeast Asia", "East Asia", "Northeast Asia",
                  "South-East Asia", "Eastern Asia", "Eastern Eurasia", "Southern Asia",
                  "Inner Asia", "Western Central Asia", "Eastern China",
                  "Southwest China") ~ st_sfc(st_geometrycollection(), crs = st_crs(.)),
      TRUE ~ geometry
    )
  ) %>%
  select(-ent_type)

# Map regex keyword matches to countries using lookup instead of case_when
df <- orcid_matched_by_regex %>%
  select(matched_keyword, id) %>%
  rbind(alex_matched_by_regex %>% select(matched_keyword, id)) %>%
  unique()

df <- df %>%
  mutate(ADMIN = sapply(matched_keyword, map_keyword_to_country))

df <- df %>%
  rename(text = matched_keyword)

df <- df %>%
  # many-to-many is intended: each matched keyword joins to every geocode of the
  # place it names (and a place can be named by several keyword rows).
  left_join(entities_osm_PA, by = "text", relationship = "many-to-many") %>%
  mutate(text = if_else((st_is_empty(geometry)) & (!str_to_lower(text) %in% str_to_lower(keywords_general)), ADMIN, text)) %>%
  mutate(text = str_to_title(text))

df <- st_as_sf(df, crs = 4326)

complete_spacy_PA_geo <- complete_spacy_PA_geo %>%
  rbind(df) %>%
  unique()

# Final non-location pass: drop entities listed in db_sf_filter.csv, matched
# case-insensitively against the now title-cased text (the earlier filter runs
# before str_to_title(), so title-cased additions like "Niobium" / "Kiel Area"
# would slip through). Re-read the files so terms added to db_sf_filter.csv apply
# even without re-running Section 4. Country names (db_sf_countries.csv) are kept.
.nonloc    <- read_csv("db_sf_filter.csv",    col_names = FALSE, show_col_types = FALSE)$X1
.nonloc_co <- tryCatch(read_csv("db_sf_countries.csv", col_names = FALSE, show_col_types = FALSE)$X1,
                       error = function(e) character(0))
.nonloc    <- setdiff(tolower(trimws(.nonloc)), tolower(trimws(.nonloc_co)))
complete_spacy_PA_geo <- complete_spacy_PA_geo %>%
  filter(!tolower(trimws(text)) %in% .nonloc)

# Hong Kong, Macau and Singapore are cities, not sea. They fall into the "maritim"
# residual because sf_countries_PA carries no Hong Kong or Macau polygon and
# Singapore's geocode is displaced by the coordinate-precision grid. Assign each its
# own ADMIN (matched on the toponym text) so the maritime residual holds only genuine
# offshore places, and so a place that is only Hong Kong or Macau is not counted as China.
city_hongkong  <- c("hong kong", "hongkong", "hong kong island", "kowloon",
                    "kowloon peninsula", "hong kong observatory",
                    "hong kong's", "hong kong’s", "hong kong special administrative region")
city_macau     <- c("macau", "macao")
city_singapore <- c("singapore")
complete_spacy_PA_geo <- complete_spacy_PA_geo %>%
  mutate(ADMIN = case_when(
    str_to_lower(text) %in% city_hongkong  ~ "Hong Kong",
    str_to_lower(text) %in% city_macau     ~ "Macau",
    str_to_lower(text) %in% city_singapore ~ "Singapore",
    TRUE ~ ADMIN
  ))

# Correct a known geocoding error: the bare toponym "Yunnan" resolves to a park of
# that name in Singapore rather than the Chinese province, which places it outside
# every land polygon. Move the point to the provincial centre and assign it to China.
.yunnan <- which(str_to_lower(complete_spacy_PA_geo$text) == "yunnan")
if (length(.yunnan)) {
  st_geometry(complete_spacy_PA_geo)[.yunnan] <-
    st_sfc(st_point(c(102.7, 25.0)), crs = st_crs(complete_spacy_PA_geo))[rep(1L, length(.yunnan))]
  complete_spacy_PA_geo$ADMIN[.yunnan] <- "China"
}

# Correct the geocode for Taipei: its true position (~121.56 E, 25.04 N) is rounded
# by the one-degree grid to 122 E, 25 N, which lands offshore east of the city and so
# drops into the maritime residual instead of Taiwan. Move it to the city centre.
.taipei <- which(str_to_lower(complete_spacy_PA_geo$text) == "taipei")
if (length(.taipei)) {
  st_geometry(complete_spacy_PA_geo)[.taipei] <-
    st_sfc(st_point(c(121.56, 25.04)), crs = st_crs(complete_spacy_PA_geo))[rep(1L, length(.taipei))]
  complete_spacy_PA_geo$ADMIN[.taipei] <- "Taiwan"
}

# Correct the geocode for "Japan Sea": Nominatim resolves the bare string to a point
# east of Honshu (~142 E, 39 N), i.e. in the Pacific on the wrong side of Japan. Move
# it to a representative point in the Sea of Japan, west of Honshu. It remains maritime
# (open water outside every land polygon), so corpus membership is unchanged; only the
# map marker moves to the correct side. ("Sea Of Japan" already resolves west of Japan.)
.japansea <- which(str_to_lower(complete_spacy_PA_geo$text) == "japan sea")
if (length(.japansea)) {
  st_geometry(complete_spacy_PA_geo)[.japansea] <-
    st_sfc(st_point(c(135, 40)), crs = st_crs(complete_spacy_PA_geo))[rep(1L, length(.japansea))]
  complete_spacy_PA_geo$ADMIN[.japansea] <- "maritim"
}

# ── Fixed toponym corrections ─────────────────────────────────────────────────
# Broad supranational regions → "Allgemein" (kept, not plotted). German, French,
# possessive and official country-name variants → their canonical country with an
# empty geometry, so they shade the country polygon. Standalone "Korea" → its own
# bucket. A fixed set of sub-national toponyms that geocode offshore → an on-land
# point. The stray Palau polygon hit → maritim. (db_sf_filter drops applied above.)
.tl   <- function(x) tolower(trimws(as.character(x)))
.crs  <- st_crs(complete_spacy_PA_geo)
.empt <- function(n) st_sfc(rep(list(st_geometrycollection()), n), crs = .crs)

.i <- which(.tl(complete_spacy_PA_geo$text) %in% .tl(c(
  "South Asia", "Central Asia", "Asien", "Asie", "High Asia", "Ne Eurasia",
  "South Asia's", "South Asia’s", "South Asia''")))
if (length(.i)) {
  complete_spacy_PA_geo$ADMIN[.i] <- "Allgemein"
  st_geometry(complete_spacy_PA_geo)[.i] <- .empt(length(.i))
}

.variant <- c(
  "chine" = "China", "japon" = "Japan", "volksrepublik china" = "China",
  "volksrepublik" = "China", "people's republic of china" = "China",
  "people’s republic of china" = "China", "people's republic" = "China",
  "people’s republic" = "China", "nordkorea" = "North Korea",
  "north korea's" = "North Korea", "north korea’s" = "North Korea",
  "north korean's" = "North Korea", "north korean’s" = "North Korea",
  "südkorea" = "South Korea", "south-korea" = "South Korea",
  "south korea's" = "South Korea", "south korea’s" = "South Korea",
  "sri lanka's" = "Sri Lanka", "sri lanka’s" = "Sri Lanka",
  "philippinen" = "Philippines")
.k <- .tl(complete_spacy_PA_geo$text); .i <- which(.k %in% names(.variant))
if (length(.i)) {
  complete_spacy_PA_geo$text[.i]  <- unname(.variant[.k[.i]])
  complete_spacy_PA_geo$ADMIN[.i] <- unname(.variant[.k[.i]])
  st_geometry(complete_spacy_PA_geo)[.i] <- .empt(length(.i))
}

.i <- which(.tl(complete_spacy_PA_geo$text) %in% c("korea", "corée", "coree"))
if (length(.i)) {
  complete_spacy_PA_geo$text[.i]  <- "Korea"
  complete_spacy_PA_geo$ADMIN[.i] <- "Korea"
  st_geometry(complete_spacy_PA_geo)[.i] <- .empt(length(.i))
}

.pts <- tibble::tribble(
  ~text_l,               ~ADMIN,        ~lon,    ~lat,
  "pearl river delta",   "China",       113.3,   23.1,
  "pearl river estuary", "China",       113.3,   23.1,
  "hangzhou",            "China",       120.2,   30.3,
  "xiamen",              "China",       118.1,   24.6,
  "hubei province",      "China",       114.3,   30.6,
  "shenhu",              "China",       118.7,   24.7,
  "north tianshan of",   "China",        86.0,   43.0,
  "thai binh",           "Vietnam",     106.3,   20.4,
  "red river delta",     "Vietnam",     105.8,   21.0,
  "peninsular malaysia", "Malaysia",    101.7,    3.2,
  "butterworth",         "Malaysia",    100.4,    5.4,
  "puerto galera",       "Philippines", 121.0,   13.5,
  "bandung district",    "Indonesia",   107.6,   -6.9)
for (.r in seq_len(nrow(.pts))) {
  .i <- which(.tl(complete_spacy_PA_geo$text) == .pts$text_l[.r])
  if (length(.i)) {
    st_geometry(complete_spacy_PA_geo)[.i] <-
      st_sfc(st_point(c(.pts$lon[.r], .pts$lat[.r])), crs = .crs)[rep(1L, length(.i))]
    complete_spacy_PA_geo$ADMIN[.i] <- .pts$ADMIN[.r]
  }
}

complete_spacy_PA_geo$ADMIN[complete_spacy_PA_geo$ADMIN == "Palau"] <- "maritim"

# One geo row per (id, text). Two passes above can emit the same place for the
# same publication — the entity pass on the snapped one-degree grid (step 2) and
# the keyword pass joined to the un-snapped entities_osm_PA — with different
# geometries and occasionally different ADMINs, so collapsing only *exact*
# duplicates keeps both. That double-counts the place on the map (e.g. Tibet at a
# snapped and a precise point) and, when a grid-snapped copy falls offshore, tags
# one publication as both a land place AND "maritim" (Penghu, Palawan) or as both
# a country and the merged "Korea" bucket (Seoul, Busan). Keep exactly one row
# per (id, text): prefer a specific-country ADMIN over the "Korea"/"Allgemein"
# buckets and the "maritim" residual, then a precise (off-grid) coordinate over
# its snapped copy.
.rank <- ifelse(complete_spacy_PA_geo$ADMIN == "maritim", 3L,
         ifelse(complete_spacy_PA_geo$ADMIN %in% c("Allgemein", "Korea"), 2L, 1L))
.ispt <- as.character(st_geometry_type(complete_spacy_PA_geo)) == "POINT" &
           !st_is_empty(complete_spacy_PA_geo)
.xy   <- matrix(NA_real_, nrow = nrow(complete_spacy_PA_geo), ncol = 2)
.xy[.ispt, ] <- t(vapply(st_geometry(complete_spacy_PA_geo)[.ispt],
                         function(p) as.numeric(p)[1:2], numeric(2)))
.ongrid <- is.na(.xy[, 1]) | (.xy[, 1] == round(.xy[, 1]) & .xy[, 2] == round(.xy[, 2]))
complete_spacy_PA_geo <- complete_spacy_PA_geo[order(.rank, .ongrid), ]
complete_spacy_PA_geo <- complete_spacy_PA_geo[
  !duplicated(paste(complete_spacy_PA_geo$id, complete_spacy_PA_geo$text, sep = "\r")), ]
rm(.tl, .crs, .empt, .variant, .k, .i, .pts, .r, .rank, .ispt, .xy, .ongrid)


# ══════════════════════════════════════════════
# SECTION 20: Final Outputs & Exports
# ──────────────────────────────────────────────
# Purpose: Apply final filters, harmonise pub counts,
#          and write all output files.
# Output:  complete_works_PA.csv,
#          complete_researchers_PA.csv,
#          complete_researchers_PA_latest.csv,
#          complete_spacy_PA_keywords.csv,
#          complete_spacy_PA_geo.geojson,
#          complete_funding_PA.csv,
#          years_normal.csv,
#          counted_coop_countries.csv
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
# Requires: alex_all_pubs.rds (Section 6), my_orcid_works.rds (Section 13),
#           chikon_pubs_unnest.rds, chikon_pubs_unnest_ror.rds (Section 11)
alex_all_pubs        <- read_rds("alex_all_pubs.rds")
complete_works_orcid <- read_rds("my_orcid_works.rds")
chikon_pubs_unnest     <- read_rds("chikon_pubs_unnest.rds")
chikon_pubs_unnest_ror <- read_rds("chikon_pubs_unnest_ror.rds")

# Harmonise final pub count with cleaning
false_matches <- complete_works_PA %>%
  filter(!id %in% complete_spacy_PA_geo$id)

complete_works_PA <- complete_works_PA %>%
  filter(!id %in% false_matches$id) %>%
  filter(!is.na(title_title_value) & title_title_value != "") %>%
  mutate(temp = tolower(label)) %>%
  group_by(orcid, type, temp) %>%
  arrange(desc(doi)) %>%
  slice(1) %>%
  ungroup() %>%
  select(-temp)

complete_works_PA <- complete_works_PA %>%
  filter(orcid %in% complete_researchers_PA_latest$orcid)

# Get full pub count per year (for graph)
years_normal <- alex_all_pubs %>%
  select(id, publication_year) %>%
  rbind(complete_works_orcid %>% rename(publication_year = `publication-date.year.value`) %>% select(id, publication_year)) %>%
  unique() %>%
  count(publication_year)

write_csv(years_normal, "years_normal.csv")

# Get co-authorship country information (for graph)
counted_coop_countries <- chikon_pubs_unnest %>%
  filter(id %in% complete_works_PA$id) %>%
  filter(!orcid %in% chikon_pubs_unnest_ror$orcid) %>%
  select(id, authorships_affiliations_country_code, publication_year) %>%
  unique() %>%
  count(id, publication_year, authorships_affiliations_country_code)

write_csv(counted_coop_countries, "counted_coop_countries.csv")

# Get funding information
funding_alex <- chikon_pubs_unnest %>%
  filter(id %in% complete_works_PA$id) %>%
  filter(!is.na(funder_display_names)) %>%
  filter(funder_display_names != "") %>%
  mutate(type = "Publikationsangabe") %>%
  rename(organization = funder_display_names,
         year = publication_year) %>%
  mutate(label = str_extract(title, "^\\S+(\\s+\\S+){0,3}")) %>%
  mutate(label = if_else(str_count(title, "\\s+") > 3,
                         paste0(label, "\u2026"),
                         label)) %>%
  select(-first_page, -last_page, -volume, -issue, -ror, -source_display_name,
         -authorships_display_name, -authorships_affiliation_raw, -authorships_affiliations_display_name) %>%
  separate_rows(organization, sep = "\\s*;\\s*") %>%
  left_join(
    orcid_map,
    by = "orcid", relationship = "many-to-many") %>%
  mutate(
    orcid = coalesce(replacement_orcid, orcid)
  ) %>%
  select(-replacement_orcid) %>%
  unique()

# Attach OpenAlex citation counts (cited_by_count) by DOI, for the publication
# detail view in the app. ORCID-only works (not in OpenAlex) get NA.
cite_src <- if (exists("alex_all_pubs_PA")) alex_all_pubs_PA else
  tryCatch(read_rds("alex_all_pubs_PA.rds"), error = function(e) NULL)
if (!is.null(cite_src) && all(c("id", "cited_by_count") %in% names(cite_src))) {
  cite_lookup <- cite_src %>%
    filter(!is.na(id)) %>%
    transmute(id_l = tolower(id), cited_by_count) %>%
    group_by(id_l) %>%
    summarise(cited_by_count = suppressWarnings(max(cited_by_count, na.rm = TRUE)),
              .groups = "drop")
  complete_works_PA <- complete_works_PA %>%
    mutate(cited_by_count = cite_lookup$cited_by_count[match(tolower(id), cite_lookup$id_l)])
} else {
  complete_works_PA$cited_by_count <- NA_integer_
}

# Drop work-less orphan researchers: keep only ORCIDs present in the works corpus.
# Author-identity fragmentation (name/id variants across OpenAlex and ORCID) can
# leave a Northern-German employment row whose publications are attributed to a
# sibling id; such rows carry no work in the corpus, are invisible in the app, and
# are absent from the analytic corpus. Removing them keeps the researcher tables
# consistent with the works table.
complete_researchers_PA <- complete_researchers_PA %>%
  filter(orcid %in% complete_works_PA$orcid)
complete_researchers_PA_latest <- complete_researchers_PA_latest %>%
  filter(orcid %in% complete_works_PA$orcid)

# Write final output files
write_csv(complete_works_PA, "complete_works_PA.csv")
write_csv(complete_researchers_PA, "complete_researchers_PA.csv")
write_csv(complete_researchers_PA_latest, "complete_researchers_PA_latest.csv")
write_csv(entities_complete, "complete_spacy_PA_keywords.csv")
write_csv(unique(funding_alex %>% filter(id %in% complete_works_PA$id)), "complete_funding_PA.csv")
write_sf(complete_spacy_PA_geo, "complete_spacy_PA_geo.geojson", append = FALSE)


# ──────────────────────────────────────────────
# SECTION 21: Citation Networks (co-citation & bibliographic coupling)
# ──────────────────────────────────────────────
# Purpose: Extract paper -> reference edges from the OpenAlex `referenced_works`
#          list-column so the Shiny app can build, on the fly for any filtered
#          selection, two complementary networks:
#            * co-citation        — references co-cited by the same papers
#            * bibliographic coupling — papers that share references
# Output:  references_edges.csv  (paper_id, ref_id)
#          references_meta.csv   (ref_id, ref_label, ref_title, ref_year,
#                                 ref_cited_by, ref_doi)
# Notes:   - paper_id matches complete_works_PA$id (the DOI-based key the app
#            already uses), so the app joins the edge list to filtered pubs by id.
#          - References cited by only ONE corpus paper are dropped: they can form
#            no co-citation or coupling edge, so this bounds the files losslessly.
#          - Reference titles/years are not in the corpus, so they are fetched
#            from OpenAlex (only needed to LABEL co-citation nodes; coupling nodes
#            are corpus papers and already have titles).
# ══════════════════════════════════════════════

# ── CHECKPOINT / RESTART POINT ──
alex_all_pubs_PA <- read_rds("alex_all_pubs_PA.rds")

# A reference must be cited by at least this many corpus papers to be kept.
# >= 2 keeps everything that can possibly form an edge (lossless). Metadata is
# fetched for references at or above `cocit_meta_min_cites`; set equal to
# `cocit_min_ref_cites` (2) so EVERY reference that can appear as a co-citation
# node gets a readable label (no bare W-ids), at the cost of a larger one-time
# OpenAlex pull (~48k refs, ~9 MB) — see references_meta.csv.
cocit_min_ref_cites  <- 2L
cocit_meta_min_cites <- 2L

# 1. Unnest paper -> reference edges; normalise OpenAlex URLs to bare W-ids and
#    keep only references of works that actually survive into the app. The full
#    (pre-pruning) set is retained for the direct-citation join in step 4.
references_edges_all <- alex_all_pubs_PA %>%
  select(paper_id = id, referenced_works) %>%
  filter(lengths(referenced_works) > 0) %>%
  unnest_longer(referenced_works) %>%
  mutate(ref_id = sub(".*/", "", referenced_works)) %>%
  filter(!is.na(paper_id), !is.na(ref_id), nzchar(ref_id)) %>%
  filter(paper_id %in% complete_works_PA$id) %>%
  distinct(paper_id, ref_id)

# 2. Drop singleton references (lossless for co-citation/coupling) and rank the
#    rest by corpus usage.
references_counts <- references_edges_all %>%
  count(ref_id, name = "corpus_cites")
keep_refs <- references_counts %>% filter(corpus_cites >= cocit_min_ref_cites)
references_edges <- references_edges_all %>% semi_join(keep_refs, by = "ref_id")

write_csv(references_edges, "references_edges.csv")
message("Citation edges: ", nrow(references_edges), " (",
        n_distinct(references_edges$paper_id), " papers, ",
        n_distinct(references_edges$ref_id), " references)")

# 3. Fetch metadata for the kept references so the app can label co-citation
#    nodes. Cached in references_meta.csv: re-runs only fetch new references.
#    NOTE: `identifier =` is openalexR's by-id fetch argument — verify it against
#    your installed openalexR version on first run.
meta_targets <- keep_refs %>%
  filter(corpus_cites >= cocit_meta_min_cites) %>%
  pull(ref_id)

meta_cache <- "references_meta.csv"
references_meta <- if (file.exists(meta_cache)) {
  read_csv(meta_cache, show_col_types = FALSE)
} else {
  tibble(ref_id = character(), ref_label = character(), ref_title = character(),
         ref_year = integer(), ref_cited_by = integer(), ref_doi = character(),
         ref_first_author = character())
}
# Older caches may lack ref_first_author (added later) or have read it back as a
# logical all-NA column — normalise to character so the backfill below can
# detect the gaps.
if (!"ref_first_author" %in% names(references_meta)) {
  references_meta$ref_first_author <- NA_character_
} else {
  references_meta$ref_first_author <- as.character(references_meta$ref_first_author)
}
# Fetch references new to the cache, PLUS any cached rows still missing a first
# author (e.g. cached before the authorships extraction was fixed) so the
# Author-Co-Citation data backfills on the next run instead of staying all-NA.
missing_fa <- references_meta$ref_id[is.na(references_meta$ref_first_author)]
to_fetch <- union(setdiff(meta_targets, references_meta$ref_id),
                  intersect(meta_targets, missing_fa))
message("Reference metadata: ", length(to_fetch), " new of ",
        length(meta_targets), " targets")

if (length(to_fetch) > 0) {
  batches <- split(to_fetch, ceiling(seq_along(to_fetch) / 50))
  new_meta <- list()
  for (b in seq_along(batches)) {
    res <- NULL
    for (attempt in seq_len(config$api_retry_tries)) {
      res <- tryCatch(
        oa_fetch(entity = "works", identifier = batches[[b]], verbose = FALSE),
        error = function(e) {
          message("  meta batch ", b, " attempt ", attempt, " failed: ",
                  conditionMessage(e))
          NULL
        })
      if (!is.null(res)) break
      if (attempt < config$api_retry_tries) Sys.sleep(config$api_retry_wait)
    }
    if (is.null(res) || nrow(res) == 0) next
    # First author of each reference, for the app's Author Co-Citation network.
    # openalexR (3.x) returns authors in the `authorships` list-column: tibbles
    # with `display_name` and `author_position` ("first"/"middle"/"last").
    ref_first_author <- if ("authorships" %in% names(res)) {
      vapply(res$authorships, function(a) {
        if (!is.data.frame(a) || nrow(a) == 0 || !"display_name" %in% names(a))
          return(NA_character_)
        i <- if ("author_position" %in% names(a) &&
                 any(a$author_position == "first", na.rm = TRUE)) {
          which(a$author_position == "first")[1]
        } else 1L
        as.character(a$display_name[i])
      }, character(1))
    } else rep(NA_character_, nrow(res))

    new_meta[[b]] <- res %>%
      transmute(
        ref_id       = sub(".*/", "", id),
        ref_title    = title,
        ref_year     = publication_year,
        ref_cited_by = cited_by_count,
        ref_doi      = doi,
        ref_first_author = .env$ref_first_author,
        # Short "TitleSnippet (Year)" label, mirroring the funding label style
        # above — reliable across openalexR versions (no nested author parsing).
        ref_label    = paste0(
          str_extract(title, "^\\S+(\\s+\\S+){0,4}"),
          if_else(str_count(title, "\\s+") > 4, "…", ""),
          " (", publication_year, ")")
      )
    message("  fetched meta batch ", b, "/", length(batches))
  }
  fetched <- bind_rows(new_meta)
  references_meta <- references_meta %>%
    filter(!ref_id %in% fetched$ref_id) %>%   # drop stale versions of re-fetched rows
    bind_rows(fetched) %>%
    distinct(ref_id, .keep_all = TRUE)
  write_csv(references_meta, meta_cache)
  message("Reference metadata cached: ", nrow(references_meta), " rows")
}

# 4. Direct intra-corpus citation edges: a reference that is itself a corpus
#    paper (citing DOI -> cited DOI). Requires the OpenAlex W-id retained in
#    Section 6 (`openalex_id`); present only after a re-harvest, so this is
#    skipped gracefully on older data. Uses the FULL edge set (a citation is
#    valid even if the cited paper is referenced only once).
if ("openalex_id" %in% names(alex_all_pubs_PA)) {
  corpus_wid <- alex_all_pubs_PA %>%
    filter(!is.na(openalex_id), nzchar(openalex_id)) %>%
    select(openalex_id, cited_paper_id = id) %>%
    distinct()
  citation_edges_direct <- references_edges_all %>%
    inner_join(corpus_wid, by = c("ref_id" = "openalex_id")) %>%
    filter(paper_id != cited_paper_id) %>%
    transmute(from_id = paper_id, to_id = cited_paper_id) %>%
    distinct()
  write_csv(citation_edges_direct, "citation_edges_direct.csv")
  message("Direct intra-corpus citation edges: ", nrow(citation_edges_direct))
} else {
  message("openalex_id absent (pre-retention harvest) — direct-citation edge ",
          "list skipped. Re-harvest (Section 6 W-id retention) to enable.")
}


# ──────────────────────────────────────────────
# SECTION 22: Compact outputs to .rds
# ──────────────────────────────────────────────
# Re-save each generated CSV/GeoJSON as a gzip binary .rds written straight into
# the Shiny app folder (config$output_dir/shiny), so app.R loads them with
# read_rds() at startup and no files need copying. Reading the just-written file
# back (rather than the in-memory object) guarantees the .rds matches the .csv.
# ══════════════════════════════════════════════
app_dir <- file.path(config$output_dir, "shiny")
if (!dir.exists(app_dir)) {
  warning("App folder '", app_dir, "' not found — skipping .rds compaction. ",
          "Create it (or adjust the path) and re-run Section 22.")
} else {
  message("Compacting outputs to .rds in '", app_dir, "' …")

  for (f in c("years_normal.csv", "counted_coop_countries.csv",
              "complete_works_PA.csv", "complete_researchers_PA.csv",
              "complete_researchers_PA_latest.csv", "complete_spacy_PA_keywords.csv",
              "complete_funding_PA.csv", "references_edges.csv",
              "references_meta.csv", "citation_edges_direct.csv")) {
    if (file.exists(f)) {
      write_rds(read_csv(f, show_col_types = FALSE),
                file.path(app_dir, sub("\\.csv$", ".rds", f)),
                compress = "gz")
    }
  }

  if (file.exists("complete_spacy_PA_geo.geojson")) {
    write_rds(read_sf("complete_spacy_PA_geo.geojson"),
              file.path(app_dir, "complete_spacy_PA_geo.rds"),
              compress = "gz")
  }

  # Country polygons for the app/figures = the Section-4 country reference PLUS a
  # merged "Korea" polygon (North + South Korea union), so a standalone "Korea"
  # mention shades the whole peninsula. The union goes ONLY into this app copy,
  # never into the sf_countries used for the Section-19 spatial join (a Korea
  # polygon there would double-match points already inside North or South Korea).
  if (exists("sf_countries")) {
    .cty <- sf_countries
    if (!"Korea" %in% .cty$NAME) {
      .pair <- .cty[.cty$NAME %in% c("North Korea", "South Korea"), ]
      .kr <- .pair[1, ]
      st_geometry(.kr) <- st_union(st_geometry(.pair))
      .kr$NAME <- "Korea"
      if ("ADMIN" %in% names(.kr)) .kr$ADMIN <- "Korea"
      .cty <- rbind(.cty, .kr)
    }
    write_sf(.cty, file.path(app_dir, "sf_countries_PA.geojson"),
             append = FALSE, delete_dsn = TRUE)
    rm(.cty)
  }

  # Small Northern-German institution name set for the app (a few hundred
  # strings, ~6 KB), so it doesn't have to load the full 3.7 MB insts_chikon.rds.
  # = the OpenAlex seed display_names PLUS their universities.csv canonicals, so
  # both raw and merged forms (e.g. bare "GEOMAR") are recognised.
  if (file.exists("insts_chikon.rds") && file.exists("universities.csv")) {
    .insts <- read_rds("insts_chikon.rds")
    .u <- read_csv("universities.csv", show_col_types = FALSE) %>% drop_na() %>% unique()
    .nd <- unique(c(.insts$display_name,
                    .u$organization_name_harmonised[
                      tolower(.u$organization_name) %in% tolower(.insts$display_name)]))
    .nd <- sort(unique(.nd[!is.na(.nd) & nzchar(.nd)]))
    write_rds(.nd, file.path(app_dir, "nd_institutions.rds"), compress = "gz")
  }

  message("Compacting done: .rds written to ",
          normalizePath(app_dir, winslash = "/", mustWork = FALSE))
}
