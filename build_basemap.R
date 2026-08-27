# ==============================================================================
# build_basemap.R — offline vector basemap for the Shiny map ("Karte" tab)
# ==============================================================================
# Project:      Atlas der Ostasien-Forschung in Norddeutschland
# Institute:    China Center, Kiel University (CAU)
#
# WHAT IT DOES
# ------------
# Builds the map's background out of Natural Earth (public domain) as a handful of
# small GeoJSON files under shiny/www/basemap/, and fetches the label typeface into
# shiny/www/fonts/. The app serves both itself, so drawing the map costs the
# visitor's browser no request to any third party — the alternative, raster tiles
# from a map CDN, would hand out every visitor's IP address on every tile.
#
# The look follows the CARTO Positron style, and the layers are split the way that
# style draws them: land is an unstroked fill, with country and province lines as
# separate layers on top. Stroking the land polygons instead would outline every
# coastline as well.
#
# WHEN TO RUN
# -----------
# Rarely. The output is static reference geography and is committed to the repo,
# and this script is not part of the clean_mining.R data pipeline. Re-run it to
# change the level of detail or to pick up a newer Natural Earth release:
#
#   Rscript build_basemap.R
#
# Everything is downloaded here, at build time. Nothing is fetched when the app
# runs. app.R detects the rebuilt files by their modification time and versions
# their URLs, so browsers holding an older copy fetch the new one.
# ==============================================================================

library(sf)
library(dplyr)
library(rnaturalearth)      # Natural Earth loaders (build time only)
library(rnaturalearthdata)  # offline 1:50m countries
library(rmapshaper)         # topology-preserving simplification (Visvalingam)

# Spherical geometry, set explicitly because the land step depends on it. It is
# sf's default, but with s2 off st_union treats longitude as a flat axis and tears
# Russia apart across the antimeridian; st_area and st_difference likewise report
# nonsense there. Keep it on when verifying anything in this script.
sf_use_s2(TRUE)

out_dir  <- file.path("shiny", "www", "basemap")
font_dir <- file.path("shiny", "www", "fonts")
for (d in c(out_dir, font_dir)) if (!dir.exists(d)) dir.create(d, recursive = TRUE)

# Coordinates are written at 3 decimals (~110 m). The map caps out at zoom 8,
# where one pixel is ~600 m, so this is below the visible threshold and roughly
# halves the file size.
write_geo <- function(x, name, precision = 3) {
  f <- file.path(out_dir, name)
  suppressWarnings(st_write(x, f, delete_dsn = TRUE, quiet = TRUE,
                            layer_options = paste0("COORDINATE_PRECISION=", precision)))
  cat(sprintf("  %-18s %5d KB  (%d features)\n", name, round(file.size(f) / 1024), nrow(x)))
}

# Natural Earth ships these layers with dozens of attribute columns; none of them
# are used (the basemap is never clicked, and the interactive country layer in the
# app is sf_countries_PA.geojson), and dropping them shrinks the files noticeably.
geometry_only <- function(x) st_sf(geometry = st_geometry(x))

# rmapshaper's keep_shapes only protects polygons: on line layers it will happily
# simplify a whole segment out of existence, and at keep = 0.35 it emptied ten
# country borders totalling ~290 km. Any segment that does not survive is put back
# unsimplified — a boundary going missing is far worse than a few extra vertices.
simplify_lines <- function(x, keep) {
  g <- st_geometry(x)
  s <- st_geometry(ms_simplify(st_sf(geometry = g), keep = keep, keep_shapes = TRUE))
  gone <- st_is_empty(s)
  if (any(gone)) {
    s[gone] <- g[gone]
    cat(sprintf("    (%d segment(s) restored unsimplified)\n", sum(gone)))
  }
  st_sf(geometry = s)
}

# Fusing a line layer into a single MULTILINESTRING drops one GeoJSON feature
# wrapper per boundary segment — thousands of them, costing more than the
# coordinates do — and leaves leaflet one path object to manage instead of
# hundreds. Simplification turns some MULTILINESTRINGs into plain LINESTRINGs,
# which trips st_combine up, so everything is cast back to one type first.
as_one_line <- function(x) {
  g <- st_geometry(x)
  st_sf(geometry = st_combine(st_cast(g[!st_is_empty(g)], "MULTILINESTRING")))
}

cat("Building offline basemap in ", out_dir, "\n", sep = "")

# ── 1. Land ───────────────────────────────────────────────────────────────────
# Whole world at 1:50m so panning out of the Asia-Pacific frame still shows a map.
#
# Deliberately NOT simplified, and dissolved into a single landmass. Both matter,
# because the app draws these same coastlines a second time as the shaded country
# polygons of sf_countries_PA.geojson:
#
#   * Unsimplified, so the two coincide exactly. That file is Natural Earth 1:50m
#     itself — with spherical geometry on, 26 of its 30 countries are
#     vertex-identical to ne_countries(scale = 50), the other four (Brunei,
#     Singapore, Micronesia, Palau) differing by at most 0.0008 % of area. Touch
#     the geometry here and the two outlines separate, leaving slivers of bare
#     land outside the shaded country, or shading spilling into the sea.
#
#   * Dissolved, so the land layer holds no country boundaries that could fail to
#     line up. mapshaper keeps shared borders consistent only when it simplifies
#     every polygon at once; simplifying any subset leaves each border between a
#     treated and an untreated neighbour mismatched. Hong Kong and Macau show this
#     most clearly, being separate admin-0 units in Natural Earth and absent from
#     sf_countries_PA.
#
# Nothing is lost by dissolving, since the borders are their own layer drawn on
# top, and it is cheaper than keeping the polygons apart (1.4 MB against 2.2 MB):
# every shared border would otherwise be stored twice, once per adjoining country.
land <- ne_countries(scale = 50, returnclass = "sf") |>
  st_make_valid() |>
  st_geometry() |>
  st_union() |>          # needs s2: a planar union mangles Russia across the antimeridian
  st_sf(geometry = _)
# 4 decimals (~11 m) rather than the 3 used elsewhere, so that rounding alone
# cannot walk the coastline away from the app's full-precision polygons.
write_geo(land, "land.geojson", precision = 4)

# ── 2. Lakes ──────────────────────────────────────────────────────────────────
# Without these, the Caspian, Baikal, Balkhash etc. read as land. Only the lakes
# Natural Earth itself considers map-worthy at small scale (scalerank <= 2).
lakes <- ne_download(scale = 50, type = "lakes", category = "physical",
                     returnclass = "sf") |>
  filter(scalerank <= 2) |>
  geometry_only() |>
  ms_simplify(keep = 0.35, keep_shapes = TRUE) |>
  st_make_valid()
write_geo(lakes, "lakes.geojson")

# ── 3. Country boundaries ─────────────────────────────────────────────────────
# Land boundaries only — the layer deliberately excludes coastlines, which is why
# it is separate from the land polygons above.
borders <- ne_download(scale = 50, type = "admin_0_boundary_lines_land",
                       category = "cultural", returnclass = "sf") |>
  geometry_only() |>
  simplify_lines(keep = 0.35) |>
  as_one_line()
write_geo(borders, "borders.geojson")

# ── 4. Province / state boundaries ────────────────────────────────────────────
# Drawn as thin dashed lines from zoom 5 on, as Positron does.
#
# Taken from the 1:10m data, because Natural Earth's 1:50m admin-1 lines cover
# nine large countries only (China, India, Indonesia, Russia, Australia and four
# outside Asia) and would leave Japan, Korea, Vietnam, Thailand, the Philippines
# and the rest of the study region without internal boundaries. 1:10m covers 197
# countries but runs to ~2 MB worldwide, so it is cut to the countries the atlas
# analyses — read from the app's own country file, so the two cannot drift apart —
# together with the nine that 1:50m already covered.
#
# Every FEATURECLA is kept, including the "statistical" classes. Natural Earth
# files much of western China under those, and filtering them out drops 40 of
# China's 76 boundary segments and 23 of India's 88, taking the Tibet and Xinjiang
# borders with them.
analysis_countries <- read_sf(file.path("shiny", "sf_countries_PA.geojson"))$NAME
legacy_50m_countries <- c("Australia", "Brazil", "Canada", "China", "India",
                          "Indonesia", "Russia", "South Africa",
                          "United States of America")
admin1 <- ne_download(scale = 10, type = "admin_1_states_provinces_lines",
                      category = "cultural", returnclass = "sf") |>
  filter(ADM0_NAME %in% union(analysis_countries, legacy_50m_countries)) |>
  geometry_only() |>
  simplify_lines(keep = 0.12) |>
  as_one_line()
write_geo(admin1, "admin1.geojson")

# ── 5. Place labels ───────────────────────────────────────────────────────────
# `minzoom` is the lowest zoom level at which a place is drawn. Natural Earth's
# SCALERANK is exactly this notion of "how early does this city appear", so it
# maps onto zoom levels almost directly; POP_MAX only splits the wider ranks.
# The thresholds are deliberately stricter than Positron's, because this map
# carries its own data markers on top and the labels must not compete with them.
places <- ne_download(scale = 50, type = "populated_places", category = "cultural",
                      returnclass = "sf") |>
  transmute(
    name = NAME,
    # Positron sets its most prominent cities in uppercase and leaves the rest in
    # mixed case. Checked against the original tiles, that split falls exactly on
    # SCALERANK <= 1: Tokyo, Shanghai, Beijing, Hong Kong, Seoul, Taipei, Chengdu
    # and Ürümqi are capitalised, while Tianjin, Chongqing, Pyongyang, Jinan and
    # Zhengzhou (all SCALERANK 2) are not.
    caps = as.integer(SCALERANK <= 1),
    minzoom = case_when(
      # Rank 0 alone at zoom 3 — a couple of dozen cities worldwide. Splitting it
      # from rank 1 keeps the opening view uncluttered, and stops that view from
      # being set entirely in capitals: the mixed-case ranks only start at zoom 4,
      # so everything visible before then would otherwise be a capitalised name.
      SCALERANK == 0 ~ 3L,
      SCALERANK == 1 ~ 4L,
      SCALERANK == 2 & POP_MAX >= 3e6 ~ 4L,
      SCALERANK == 2 ~ 5L,
      SCALERANK == 3 & POP_MAX >= 1.2e6 ~ 6L,
      SCALERANK == 3 & POP_MAX >= 8e5 ~ 7L,
      SCALERANK == 4 & POP_MAX >= 1.5e6 ~ 7L,
      SCALERANK <= 4 & POP_MAX >= 8e5 ~ 8L,
      TRUE ~ 99L
    )
  ) |>
  filter(minzoom <= 8) |>
  arrange(minzoom)
write_geo(places, "places.geojson")
cat("  labels per zoom:  ",
    paste(sprintf("z%d=%d", as.integer(names(table(places$minzoom))),
                  as.integer(table(places$minzoom))), collapse = "  "), "\n")

# ── 6. Label typeface ─────────────────────────────────────────────────────────
# Positron sets its labels in Open Sans. Serving it from the app keeps the map
# looking the same without the browser ever calling fonts.gstatic.com — which
# would reintroduce exactly the third-party request this basemap exists to avoid.
# Open Sans is Apache-2.0 licensed, so redistributing it in the repo is fine.
# Only the regular weight is needed, in the two Latin subsets that cover the
# place names (latin-ext carries Ōsaka, Ürümqi, ...).
fonts <- c(
  "open-sans-400-latin.woff2" =
    "https://fonts.gstatic.com/s/opensans/v44/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTS-muw.woff2",
  "open-sans-400-latin-ext.woff2" =
    "https://fonts.gstatic.com/s/opensans/v44/memvYaGs126MiZpBA-UvWbX2vVnXBbObj2OVTSGmu1aB.woff2"
)
for (nm in names(fonts)) {
  f <- file.path(font_dir, nm)
  download.file(fonts[[nm]], f, mode = "wb", quiet = TRUE)
  cat(sprintf("  %-18s %5d KB\n", nm, round(file.size(f) / 1024)))
}

cat(sprintf("Total basemap: %d KB, fonts: %d KB\n",
            round(sum(file.size(list.files(out_dir, full.names = TRUE))) / 1024),
            round(sum(file.size(list.files(font_dir, full.names = TRUE))) / 1024)))
cat("Sources: Natural Earth (naturalearthdata.com, public domain);",
    "Open Sans (Apache-2.0).\n")
