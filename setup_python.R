############################################################
#  setup_python.R                                          #
#  One-time Python/spaCy provisioning for the              #
#  Atlas der Ostasien-Forschung mining pipeline.           #
#                                                          #
#  spacyr (1.3.x) manages its own virtual environment      #
#  named "r-spacyr". This script uses spacyr's own         #
#  installer to create it and download the language        #
#  models clean_mining.R needs, so the pipeline runs       #
#  reproducibly on any machine.                            #
#                                                          #
#  Usage (from the project directory):                     #
#      source("setup_python.R")                            #
#                                                          #
#  Safe to re-run: spacy_install() skips what already      #
#  exists (pass force = TRUE to rebuild from scratch).     #
############################################################

library(spacyr)
library(reticulate)

spacy_version <- "3.8.11"   # pin for reproducibility; models below match 3.8.x
lang_models   <- c("en_core_web_lg",   # English
                   "de_core_news_lg",  # German
                   "fr_core_news_lg",  # French
                   "zh_core_web_lg")   # Chinese (incl. Traditional)

# ── Windows DLL fix (same as clean_mining.R) ──────────────────────────────
# spacy_install builds the "r-spacyr" venv on the r-miniconda base and then
# shells out to it to download models; that subprocess must be able to load
# python3xx.dll's conda dependencies. Put the conda DLL dirs on PATH first.
if (.Platform$OS.type == "windows") {
  conda_base <- tryCatch(miniconda_path(), error = function(e) "")
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

# ── 1. Install spaCy itself via spacyr's installer ────────────────────────
# Creates/updates the "r-spacyr" virtualenv (and installs Python if none is
# found). We pass a SINGLE model here on purpose: spacy_install() forwards
# lang_models to spacy_download_langmodel(), whose guard
#   if (!force & py_check_installed(lang_models))
# errors with "condition has length > 1" when given a vector (a spacyr 1.3 bug).
# The remaining models are fetched one at a time in step 2.
message("Installing spaCy ", spacy_version, " into the 'r-spacyr' virtualenv …")
spacy_install(version     = spacy_version,
              lang_models = lang_models[1],
              ask         = FALSE)

# ── 2. Download each language model individually (avoids the vector bug) ───
message("Downloading language models (this fetches ~2 GB the first time)…")
for (m in lang_models) {
  message("  ", m, " …")
  spacy_download_langmodel(m)
}

# ── 3. Verify: initialise each model and run a sample parse ────────────────
message("\nVerifying language models…")
for (m in lang_models) {
  spacy_initialize(model = m)
  message("  ", m, ": OK")
  spacy_finalize()
}
spacy_initialize(model = "en_core_web_lg")
cat("\nSample parse (sanity check):\n")
print(spacy_parse("Kiel and Hamburg are in Northern Germany."))
spacy_finalize()

message("\n────────────────────────────────────────────────────────")
message("Done. The 'r-spacyr' virtualenv is ready with all four models.")
message("clean_mining.R will use it automatically — no further setup needed.")
message("────────────────────────────────────────────────────────")
