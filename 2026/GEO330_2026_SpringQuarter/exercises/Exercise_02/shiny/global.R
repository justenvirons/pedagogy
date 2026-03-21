## global.R
## Divvy Bikeshare Explorer — GEO 330 Sustainable Urban Transportation
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
library(DT)
library(htmltools)
library(scales)
library(readr)

options(scipen = 999, digits = 2)


data_dir <- file.path(getwd(), "2026/GEO330_2026_SpringQuarter/exercises/Exercise_02/shiny", "data")

# ── Community areas: local cache with 30-day TTL ──────────────────────────────
# Cache stores already-processed sf (comarea_id, comarea_name, geometry only)
geojson_cache <- file.path(data_dir, "community_areas_cache.geojson")
socrata_url   <- "https://data.cityofchicago.org/resource/igwz-8jzy.geojson"

cache_age <- if (file.exists(geojson_cache)) {
  as.numeric(difftime(Sys.time(), file.mtime(geojson_cache), units = "days"))
} else {
  Inf
}

if (cache_age > 30) {
  tryCatch({
    raw <- st_read(socrata_url, quiet = TRUE) %>%
      st_transform(4326) %>%
      mutate(
        comarea_id   = as.numeric(area_numbe),
        comarea_name = str_to_title(community)
      ) %>%
      select(comarea_id, comarea_name)
    st_write(raw, geojson_cache, delete_dsn = TRUE, quiet = TRUE)
  }, error = function(e) {
    message("Community areas fetch failed, using cache: ", e$message)
  })
}

community_areas <- st_read(geojson_cache, quiet = TRUE)

# ── Trip summary tables ───────────────────────────────────────────────────────
# Loads: trips_by_date, trips_by_yearmonth, trips_by_yearmonth_ca
load(file.path(data_dir, "divvy_trips_summary_tables_latest.RData"))

ses_attributes <- read_csv(
  file.path(data_dir, "chicago_health_atlas_indicators.csv"),
  show_col_types = FALSE
)

# ── Quintile helper functions (identical to Exercise_02.qmd) ─────────────────
fx_ntile <- function(x, na.rm = TRUE) ntile(x, 5)

fx_ntile_label <- function(x, na.rm = TRUE) {
  case_when(
    x == 1 ~ "Lowest",
    x == 2 ~ "Low",
    x == 3 ~ "Moderate",
    x == 4 ~ "High",
    x == 5 ~ "Highest"
  )
}

# ── community_areas_ses: geometry + SES attributes + quintile labels ──────────
community_areas_ses <- community_areas %>%
  left_join(ses_attributes %>% select(-Name), by = "comarea_id") %>%
  mutate_at(vars(svi_2020:poverty_2021), list(ntile = fx_ntile)) %>%
  mutate_at(vars(svi_2020_ntile:poverty_2021_ntile), list(label = fx_ntile_label)) %>%
  select(-ends_with("ntile"))

# ── Cumulative snapshot for choropleth (latest yearmonth = all trips to date) ─
total_days <- as.integer(nrow(trips_by_date))

trips_by_communityarea <- trips_by_yearmonth_ca %>%
  drop_na(yearmonth) %>%
  filter(yearmonth == max(yearmonth))

trips_by_communityarea_ses <- community_areas_ses %>%
  left_join(
    trips_by_communityarea %>% select(-starts_with("yearmonth")),
    by = "comarea_id"
  ) %>%
  mutate(
    fr_avg_daily   = fr_trips_cumul / total_days,
    to_avg_daily   = to_trips_cumul / total_days,
    fr_daily_per1k = fr_avg_daily   / pop_2021 * 1000,
    to_daily_per1k = to_avg_daily   / pop_2021 * 1000
  )

# ── Value box scalars ─────────────────────────────────────────────────────────
total_trips  <- max(trips_by_date$trips_cumul)
avg_daily    <- round(mean(trips_by_date$trips))
peak_day_row <- trips_by_date %>% slice_max(trips, n = 1)

# ── SES indicator choices and column mappings ─────────────────────────────────
ses_choices <- c(
  "Social Vulnerability Index" = "svi_2020",
  "Neighborhood Safety"        = "safety_2018",
  "Median Household Income"    = "hhinc_2021",
  "Hardship Index"             = "hardship_2021",
  "Poverty Rate"               = "poverty_2021"
)

ses_label_cols <- c(
  svi_2020      = "svi_2020_ntile_label",
  safety_2018   = "safety_2018_ntile_label",
  hhinc_2021    = "hhinc_2021_ntile_label",
  hardship_2021 = "hardship_2021_ntile_label",
  poverty_2021  = "poverty_2021_ntile_label"
)

# ── Pre-compute SES trend data for all 5 indicators ───────────────────────────
make_ses_trends <- function(label_col) {
  trips_by_yearmonth_ca %>%
    left_join(
      community_areas_ses %>%
        st_drop_geometry() %>%
        select(comarea_id, pop_2021, ses_ntile = all_of(label_col)),
      by = "comarea_id"
    ) %>%
    group_by(yearmonth, ses_ntile) %>%
    summarise(
      tot_pop        = sum(pop_2021, na.rm = TRUE),
      fr_trips       = sum(fr_trips,  na.rm = TRUE),
      to_trips       = sum(to_trips,  na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    drop_na(tot_pop) %>%
    mutate(
      fr_trips_per1k = fr_trips / tot_pop * 1000,
      to_trips_per1k = to_trips / tot_pop * 1000,
      ses_label      = factor(ses_ntile, levels = c("Highest", "High", "Moderate", "Low", "Lowest"))
    )
}

ses_trends_list        <- lapply(ses_label_cols, make_ses_trends)
names(ses_trends_list) <- names(ses_choices)   # keyed by display name

# ── Shared color palette for quintile line chart ──────────────────────────────
quintile_colors <- c("darkred", "orange", "gold", "lightblue", "steelblue")

# ── SES indicator descriptions (displayed reactively in sidebar) ──────────────
ses_descriptions <- c(
  "Social Vulnerability Index" = paste(
    "The CDC/ATSDR Social Vulnerability Index (SVI) measures a community's",
    "resilience to disasters and public health emergencies. Scores range from",
    "0 to 100, with higher values indicating greater vulnerability. The index",
    "combines indicators across four themes: socioeconomic status, household",
    "composition, minority status, and housing and transportation."
  ),
  "Neighborhood Safety" = paste(
    "Neighborhood safety reflects the percentage of residents who report feeling",
    "safe in their community. Higher scores indicate greater perceived safety.",
    "Perceived safety influences willingness to walk, bike, and use public",
    "spaces — and therefore the likelihood of using bikeshare systems."
  ),
  "Median Household Income" = paste(
    "Median household income is the midpoint of all household income values",
    "within a community area. It reflects the typical economic resources",
    "available to families and is closely associated with access to — and",
    "cost-sensitivity around — transportation options including bikeshare."
  ),
  "Hardship Index" = paste(
    "The hardship index is a composite of six socioeconomic indicators:",
    "crowded housing, poverty, unemployment, age dependency (under 18 or",
    "over 64), single-parent households, and per capita income. Scores range",
    "from 0 to 100, with higher values reflecting greater community hardship."
  ),
  "Poverty Rate" = paste(
    "The poverty rate is the percentage of residents living below the federal",
    "poverty line. Higher rates indicate greater economic disadvantage and",
    "may reflect reduced ability to afford transportation alternatives,",
    "including annual or monthly bikeshare memberships."
  )
)

# ── Tab 2 metric title lookup ─────────────────────────────────────────────────
metric_titles <- c(
  fr_trips_cumul = "Total Trips (from) — Quintiles",
  to_trips_cumul = "Total Trips (to) — Quintiles",
  fr_avg_daily   = "Avg Daily Trips (from) — Quintiles",
  to_avg_daily   = "Avg Daily Trips (to) — Quintiles",
  fr_daily_per1k = "Daily Trips per 1K Pop (from) — Quintiles",
  to_daily_per1k = "Daily Trips per 1K Pop (to) — Quintiles"
)
