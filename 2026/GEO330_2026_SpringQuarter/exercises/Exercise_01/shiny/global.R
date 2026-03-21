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

data_dir <- file.path(getwd(), "2026/GEO330_2026_SpringQuarter/exercises/Exercise_01/shiny", "data")

# ── Load RData ────────────────────────────────────────────────────────────────
# Objects: modeshare_county_period, modeshare_county_period_formatted,
#          modeshare_county_period_pivoted, us_counties_geom,
#          us_divisions_geom, us_states_geom
load(file.path(data_dir, "Exercise_01.RData"))

# Ensure plain data frame (rbindlist produces data.table)
modeshare_county_period_formatted <- as.data.frame(modeshare_county_period_formatted)
modeshare_county_period_pivoted   <- as.data.frame(modeshare_county_period_pivoted)

# ── County geometry (contiguous US, WGS84) ───────────────────────────────────
county_geom <- us_counties_geom %>%
  select(GEOID_county = GEOID) %>%
  st_transform(4326)

# ── Division boundary overlay (for map layer) ────────────────────────────────
modeshare_latest_raw <- modeshare_county_period_formatted %>%
  filter(year == max(year), !state_name %in% c("Alaska", "Hawaii"))

modeshare_latest_geom <- county_geom %>%
  inner_join(modeshare_latest_raw, by = "GEOID_county") %>%
  drop_na(division_name)

us_divisions_sub_geom <- modeshare_latest_geom %>%
  group_by(division_name) %>%
  summarise(geometry = st_union(geometry), .groups = "drop")

# ── Lookup structures ─────────────────────────────────────────────────────────
all_years     <- sort(unique(modeshare_county_period_formatted$year))
latest_year   <- max(all_years)
all_divisions <- sort(unique(modeshare_county_period_formatted$division_name))

state_division_lookup <- modeshare_county_period_formatted %>%
  select(state_name, division_name) %>%
  distinct() %>%
  filter(!state_name %in% c("Alaska", "Hawaii")) %>%
  arrange(state_name)

all_states <- sort(unique(state_division_lookup$state_name))

# ── Mode display names, column names, colors, map palettes ───────────────────
mode_cols <- c(
  "Drove Alone"    = "pct_drovealone",
  "Carpool"        = "pct_carpool",
  "Transit"        = "pct_transit",
  "Walk"           = "pct_walk",
  "Bicycle"        = "pct_bicycle",
  "Work from Home" = "pct_fromhome",
  "Taxi"           = "pct_taxi",
  "Motorcycle"     = "pct_motorcycle",
  "Other"          = "pct_other"
)

mode_colors <- c(
  "Drove Alone"    = "#636363",
  "Carpool"        = "#8da0cb",
  "Transit"        = "#009BA6",
  "Walk"           = "#9F1928",
  "Bicycle"        = "#080967",
  "Work from Home" = "#E0A100",
  "Taxi"           = "#984ea3",
  "Motorcycle"     = "#ff7f00",
  "Other"          = "#a65628"
)

map_palettes <- c(
  "Drove Alone"    = "Greys",
  "Carpool"        = "Purples",
  "Transit"        = "Greens",
  "Walk"           = "Blues",
  "Bicycle"        = "Reds",
  "Work from Home" = "YlOrBr",
  "Taxi"           = "Purples",
  "Motorcycle"     = "Oranges",
  "Other"          = "Greys"
)

# ── Quintile-bin helper: deduplicates breaks to handle zero-inflated data ─────
fx_create_bins <- function(df, col) {
  vals   <- df[[col]][!is.na(df[[col]])]
  breaks <- unique(quantile(vals, probs = seq(0, 1, 0.2), na.rm = TRUE))
  # Nudge upper bound so the maximum value is included in the last bin
  breaks[length(breaks)] <- breaks[length(breaks)] + .Machine$double.eps * 1e6
  breaks
}

# ── National average mode share for latest year (value boxes) ────────────────
natl_avgs <- modeshare_latest_raw %>%
  summarise(across(all_of(unname(mode_cols)), \(x) mean(x, na.rm = TRUE)))

