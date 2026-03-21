## global.R
## Transportation & Public Health Dashboard — GEO/SUD 3/430 Exercise #4
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

data_dir <- file.path(getwd(), "2026/GEO330_2026_SpringQuarter/exercises/Exercise_04/shiny", "data")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

# ── Indicator definitions ─────────────────────────────────────────────────────
indicators <- data.frame(
  name    = c("obesity", "safety", "sidewalk", "violence", "transit", "health", "inactivity"),
  label   = c("Adult Obesity Rate", "Neighborhood Safety Rate", "Sidewalk Quality Rate",
              "Neighborhood Violence Rate", "Transit Stop Access Rate",
              "Overall Health Status Rate", "Physical Inactivity Rate"),
  scc_key = c("JG8YZKL", "JG8SRFE", "JG8IQGF", "JG8QFWF", "JG8OWII", "JG8OLNB", "JG8MIQE"),
  chi_key = c("HCSOBP",  "HCSNSP",  "HCSNSQP", "HCSNVP",  "HCSWTSP", "HCSOHSP", "HCSPAP"),
  stringsAsFactors = FALSE
)

ind_choices <- setNames(indicators$name, indicators$label)

ind_label <- function(nm) indicators$label[indicators$name == nm]

# ── Cache age helper ──────────────────────────────────────────────────────────
cache_age <- function(path) {
  if (!file.exists(path)) return(Inf)
  as.numeric(difftime(Sys.time(), file.mtime(path), units = "days"))
}

# ── Cache: Chicago community areas geometry (30-day TTL) ─────────────────────
chi_geom_cache <- file.path(data_dir, "geometry_chicago.geojson")
if (cache_age(chi_geom_cache) > 30) {
  tryCatch({
    chi_raw <- st_read(
      "https://metopio.blob.core.windows.net/lalage/atlas/3/shapes/communityareas.topo.json",
      quiet = TRUE
    ) %>%
      st_as_sf() %>%
      st_set_crs(4326) %>%
      rename(geoid = id) %>%
      mutate(
        geoid    = str_replace(geoid, "1714000-", ""),
        location = "Chicago, IL",
        type     = "community area"
      )
    st_write(chi_raw, chi_geom_cache, delete_dsn = TRUE, quiet = TRUE)
  }, error = function(e) message("Chicago geometry fetch failed: ", e$message))
}
community_areas <- st_read(chi_geom_cache, quiet = TRUE) %>% st_set_crs(4326)

# ── Cache: SCC municipalities geometry (30-day TTL) ───────────────────────────
scc_geom_cache <- file.path(data_dir, "geometry_scc.geojson")
if (cache_age(scc_geom_cache) > 30) {
  tryCatch({
    scc_raw <- st_read(
      "https://metopio.blob.core.windows.net/lalage/atlas/6/shapes/SCC-place.topo.json",
      quiet = TRUE
    ) %>%
      st_as_sf() %>%
      st_set_crs(4326) %>%
      rename(geoid = id) %>%
      mutate(
        sqmi     = as.numeric(st_area(st_transform(., 3435))) * 0.0000003861,
        location = "Suburban Cook County",
        type     = "municipality"
      ) %>%
      filter(sqmi >= 4) %>%
      select(-sqmi)
    st_write(scc_raw, scc_geom_cache, delete_dsn = TRUE, quiet = TRUE)
  }, error = function(e) message("SCC geometry fetch failed: ", e$message))
}
places_cook_county <- st_read(scc_geom_cache, quiet = TRUE) %>% st_set_crs(4326)

# ── Novel Cook County geography (Chicago + SCC) ───────────────────────────────
novel_geo_cook <- places_cook_county %>%
  bind_rows(community_areas) %>%
  st_as_sf() %>%
  st_make_valid()

# ── Cache: Health indicator data (7-day TTL) ──────────────────────────────────
ind_cache <- file.path(data_dir, "indicators_all_cache.csv")
if (cache_age(ind_cache) > 7) {
  tryCatch({
    ind_list <- list()
    for (i in seq_len(nrow(indicators))) {
      url_scc <- paste0(
        "https://cookcountyhealthatlas.org/api/v1/data/",
        "?format=json&layer=place&period=2022&population=&topic=",
        indicators$scc_key[i]
      )
      res_scc <- jsonlite::fromJSON(url_scc)
      ind_list[[paste0("scc_", i)]] <- res_scc$results %>%
        as.data.frame() %>%
        select(estimate = v, se, geoid = g, layer = l) %>%
        mutate(topic = indicators$name[i])

      url_chi <- paste0(
        "https://chicagohealthatlas.org/api/v1/data/",
        "?format=json&layer=neighborhood&period=2021-2022&population=&topic=",
        indicators$chi_key[i]
      )
      res_chi <- jsonlite::fromJSON(url_chi)
      ind_list[[paste0("chi_", i)]] <- res_chi$results %>%
        as.data.frame() %>%
        select(estimate = v, se, geoid = g, layer = l) %>%
        mutate(
          topic = indicators$name[i],
          geoid = str_replace(geoid, "1714000-", "")
        )
    }
    write_csv(bind_rows(ind_list), ind_cache)
  }, error = function(e) message("Indicator fetch failed: ", e$message))
}

indicators_raw <- read_csv(ind_cache, show_col_types = FALSE,
                           col_types = cols(geoid = col_character()))

# ── Pivot wide and join with geometry ─────────────────────────────────────────
indicators_wide <- indicators_raw %>%
  pivot_wider(
    id_cols    = c(geoid, layer),
    names_from = topic,
    values_from = c(estimate, se),
    values_fn  = \(x) mean(x, na.rm = TRUE)
  ) %>%
  rename_with(~ str_remove(., "^estimate_"), starts_with("estimate_"))

indicators_geo <- novel_geo_cook %>%
  left_join(indicators_wide, by = "geoid") %>%
  st_as_sf() %>%
  drop_na(layer)

# ── Bivariate palette (3×3: X class 1-3, Y class 1-3) ────────────────────────
biv_pal <- c(
  "1-1" = "#e8e8e8", "2-1" = "#b5c4e1", "3-1" = "#6c9ecb",
  "1-2" = "#e4c8dc", "2-2" = "#9ab0cd", "3-2" = "#4b93c1",
  "1-3" = "#d15fa0", "2-3" = "#8a5fa0", "3-3" = "#3b4994"
)

fx_biv_color <- function(x_vals, y_vals) {
  x_class <- ntile(x_vals, 3)
  y_class <- ntile(y_vals, 3)
  key     <- paste0(x_class, "-", y_class)
  col     <- unname(biv_pal[key])
  ifelse(is.na(col), "#cccccc", col)
}

# ── Quintile-bin helper (deduplicates breaks for zero-inflated data) ──────────
fx_quintile_pal <- function(vals, palette = "YlOrRd") {
  breaks <- unique(quantile(vals, probs = seq(0, 1, 0.2), na.rm = TRUE))
  breaks[length(breaks)] <- breaks[length(breaks)] + .Machine$double.eps * 1e6
  colorBin(palette, domain = vals, bins = breaks, na.color = "#cccccc")
}

# ── Full-extent reset button (shared across all maps) ─────────────────────────
cook_bbox    <- st_bbox(novel_geo_cook)
reset_map_js <- JS(sprintf(
  "function(btn, map){ map.fitBounds([[%.4f, %.4f], [%.4f, %.4f]]); }",
  cook_bbox["ymin"], cook_bbox["xmin"],
  cook_bbox["ymax"], cook_bbox["xmax"]
))

# ── Jurisdiction choices ───────────────────────────────────────────────────────
juris_choices <- c("All" = "all", "Chicago" = "Chicago, IL",
                   "Suburban Cook County" = "Suburban Cook County")
