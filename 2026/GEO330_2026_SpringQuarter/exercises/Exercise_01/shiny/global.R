## global.R
## Mode Share Transportation Dashboard — GEO/SUD 3/430 Exercise #1
## C. Scott Smith, PhD AICP
## Loaded once per R process (shared across all user sessions)

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(DT)
library(htmltools)
library(scales)

options(scipen = 999, digits = 2)

# setwd(dir = file.path(getwd(), "2026/GEO330_2026_SpringQuarter/exercises/Exercise_01/shiny"))

# rsconnect::writeManifest(
#   appDir = ".",
#   appFiles = c(
#     "app.R",
#     "global.R",
#     "Exercise_01.RData",   # now at root, not data/
#     "styles.css"
#   )
# )

# ── Load RData ────────────────────────────────────────────────────────────────
# Objects: contains
# "latest_year"                       "data_dir"                          "us_counties_geom"                 
# "us_divisions_geom"                 "natl_avgs"                         "mode_colors"                      
# "all_divisions"                     "us_states_geom"                    "county_geom"                      
# "modeshare_latest_raw"              "us_divisions_sub_geom"             ".Random.seed"                     
# "map_palettes"                      "mode_cols"                         "all_states"                       
# "modeshare_county_period_pivoted"   "modeshare_county_period"           "state_division_lookup"            
# "all_years"                         "fx_create_bins"                    "modeshare_latest_geom"            
# "modeshare_county_period_formatted"

load(file = "Exercise_01.RData")
