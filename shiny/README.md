# East Asia Research Atlas

A Shiny application for exploring East Asia research in Northern Germany.

## Application Structure

### Main Application
- **`app.R`** - Complete Shiny application containing all UI, server logic, and configurations

### Data Files
- **`db_sf_filter.csv`** - Spatial filter data for East Asia region identification
- **`db_sf_countries.csv`** - Country-level geographic data  
- **`years_normal.csv`** - Temporal data for publication years
- **`faculties.csv`** - Faculty and institutional data
- **`complete_works_PA.csv`** - Core research and publications data
- Additional CSV files with researcher, geographic, and bibliometric data

### Assets
- **`standorte.jpeg`** - Map image showing research locations
- **`logo_*.png`** - Institutional logos and branding assets
- **`citation.bib`** - Citation reference file

## Key Features

- **Single-File Architecture**: All functionality contained in one comprehensive app.R file
- **Well-Documented Code**: Extensive comments explaining data processing and visualization logic
- **Interactive Visualizations**: Maps, charts, and network graphs for data exploration
- **Comprehensive Documentation**: Detailed technical documentation included in the application

## Technical Details

- **Author**: Dr. Thorben Pelzer
- **Framework**: R Shiny
- **Data Sources**: ORCiD, OpenAlex, Crossref
- **Visualization**: ggplot2, visNetwork, wordcloud2, leaflet/sf

## Usage

Run the application using:
```r
# Option 1: Direct execution
shiny::runApp("app.R")

# Option 2: Source and run
source("app.R")
shinyApp(ui = ui, server = server)
```

## Dependencies

The application requires the following R packages:
- **Core Shiny**: `shiny`, `shinyjs`, `shinythemes`, `shinyWidgets`, `shinycssloaders`, `htmlwidgets`, `bslib`
- **Data Processing**: `tidyverse` (includes `dplyr`, `ggplot2`, etc.)
- **Visualization**: `ggtext`, `ggrepel`, `wordcloud2`, `viridisLite`, `visNetwork`, `igraph`
- **Geospatial**: `sf` 
- **Export**: `writexl`

Install dependencies with:
```r
install.packages(c("shiny", "shinyjs", "shinythemes", "shinyWidgets", 
                   "shinycssloaders", "htmlwidgets", "bslib", "tidyverse",
                   "ggtext", "ggrepel", "wordcloud2", "viridisLite", 
                   "visNetwork", "igraph", "sf", "writexl"))
```