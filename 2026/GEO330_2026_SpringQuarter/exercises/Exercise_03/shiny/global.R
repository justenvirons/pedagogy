## global.R
## CTA L Ridership Dashboard — GEO/SUD 3/430 Exercise #3
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
library(readr)
library(zoo)

options(scipen = 999, digits = 2)

data_dir <- file.path(getwd(), "2026/GEO330_2026_SpringQuarter/exercises/Exercise_03", "data")

# ── Color palettes ────────────────────────────────────────────────────────────
pal_ccvi <- c("LOW" = "#E2F11A", "MEDIUM" = "#E0A100", "HIGH" = "#9F1928")

pal_line <- c(
  "Blue"   = "#3498db",
  "Brown"  = "#964B00",
  "Green"  = "#1e8449",
  "Multi"  = "#424949",
  "Orange" = "#e67e22",
  "Pink"   = "#f5b7b1",
  "Red"    = "#e74c3c"
)

all_lines <- sort(names(pal_line))

# ── Community areas with CCVI — read directly from local shapefile ────────────
# Shapefile columns: comname, ccviscore, ccvicat, ses, hhcomp, nopcp, mobrat,
#   essen, age65, comcon, covinc, covhsp, covmor, plus _n ntile variants
community_areas_ccvi <- st_read(
  file.path(data_dir, "community_areas_ccvi.shp"), quiet = TRUE
) %>%
  st_transform(4326) %>%
  rename(
    comarea_name = comname,
    ccvi_score   = ccviscore,
    ccvi_cat     = ccvicat,
    rank_ses     = ses,
    rank_mobrat  = mobrat,
    rank_essen   = essen,
    rank_covinc  = covinc,
    rank_covhsp  = covhsp,
    rank_covmor  = covmor
  ) %>%
  mutate(
    comarea_id  = row_number(),
    ccvi_cat    = factor(ccvi_cat, levels = c("LOW", "MEDIUM", "HIGH"), ordered = TRUE)
  )

# ── Local: CTA rail stations shapefile ────────────────────────────────────────
rail_stations_raw <- st_read(
  file.path(data_dir, "CTA_RAIL_STATIONS.shp"), quiet = TRUE
) %>%
  st_transform(4326) %>%
  select(
    station_id        = STATION_ID,
    station_name      = SHORTNAME,
    station_name_long = LONGNAME,
    station_line      = LEGEND
  ) %>%
  mutate(station_line = recode(station_line,
    "BL" = "Blue", "BR" = "Brown", "GR" = "Green",
    "ML" = "Multi", "OR" = "Orange", "PK" = "Pink", "RD" = "Red"
  ))

# ── Local: CTA rail lines shapefile ──────────────────────────────────────────
# COLOR field contains line names: Blue, Brown, Green, Multi, Orange, Pink, Red
rail_lines <- st_read(
  file.path(data_dir, "CTA_RAIL_LINES.shp"), quiet = TRUE
) %>%
  st_transform(4326) %>%
  rename(line_label = COLOR) %>%
  mutate(
    display_color = {
      dc <- unname(pal_line[line_label])
      ifelse(is.na(dc), "#424949", dc)
    }
  )

# ── Local: Monthly ridership CSV ──────────────────────────────────────────────
ridership_attributes <- read_csv(
  file.path(data_dir, "CTA_Average_Rail_Station_Ridership_1999_2024.csv"),
  show_col_types = FALSE
) %>%
  select(
    station_id  = RIDERSHIP_ID,
    year        = YEAR,
    month       = MONTH,
    day_type    = DAY_TYPE,
    rides_total = TOTAL_RIDES,
    rides_avg   = DAILY_AVG_RIDES
  ) %>%
  mutate(year_month = as.Date(paste0(year, "-", month, "-01")))

# ── Fetch system-wide daily boardings (7-day cache) ───────────────────────────
daily_cache <- file.path(data_dir, "daily_boardings_cache.csv")
daily_age   <- if (file.exists(daily_cache))
  as.numeric(difftime(Sys.time(), file.mtime(daily_cache), units = "days")) else Inf

if (daily_age > 7) {
  tryCatch({
    daily_raw <- read_csv(
      "https://data.cityofchicago.org/api/views/6iiy-9s97/rows.csv?accessType=DOWNLOAD",
      show_col_types = FALSE
    ) %>%
      mutate(service_date = as.Date(service_date, "%m/%d/%Y")) %>%
      distinct() %>%
      arrange(service_date)
    write_csv(daily_raw, daily_cache)
  }, error = function(e) message("Daily boardings fetch failed, using cache: ", e$message))
}

ridership_daily <- read_csv(daily_cache, show_col_types = FALSE) %>%
  mutate(service_date = as.Date(service_date))

# ── Fetch COVID-19 weekly data (30-day cache; Chicago reporting ended May 2024) ─
covid_cache <- file.path(data_dir, "covid19_cache.csv")
covid_age   <- if (file.exists(covid_cache))
  as.numeric(difftime(Sys.time(), file.mtime(covid_cache), units = "days")) else Inf

if (covid_age > 30) {
  tryCatch({
    covid_raw <- read_csv(
      paste0("https://data.cityofchicago.org/api/views/naz8-j4nc/",
             "rows.csv?accessType=DOWNLOAD&bom=true&format=true"),
      show_col_types = FALSE
    )
    write_csv(covid_raw, covid_cache)
  }, error = function(e) message("COVID-19 fetch failed, using cache: ", e$message))
}

covid19_cases <- read_csv(covid_cache, show_col_types = FALSE) %>%
  rename(Cases = `Cases - Total`, Deaths = `Deaths - Total`) %>%
  mutate(Date = as.Date(Date, "%m/%d/%Y")) %>%
  drop_na(Cases) %>%
  mutate(across(where(is.numeric), ~ replace_na(., 0))) %>%
  arrange(Date) %>%
  mutate(
    Cases7d  = round(rollmean(Cases,  k = 7, fill = 0, align = "right"), 1),
    Deaths7d = round(rollmean(Deaths, k = 7, fill = 0, align = "right"), 1)
  )

# ── Spatial join: stations → nearest Chicago community area ───────────────────
ca_proj  <- community_areas_ccvi %>% st_transform(3435)
sta_proj <- rail_stations_raw    %>% st_transform(3435)

in_chicago  <- lengths(st_intersects(sta_proj, ca_proj)) > 0
sta_chicago <- sta_proj[in_chicago, ]

nn_idx    <- st_nearest_feature(sta_chicago, ca_proj)
ca_lookup <- community_areas_ccvi %>%
  st_drop_geometry() %>%
  select(comarea_id, comarea_name, ccvi_score, ccvi_cat,
         rank_ses, rank_mobrat, rank_essen,
         rank_covinc, rank_covhsp, rank_covmor) %>%
  slice(nn_idx)

rail_stations <- sta_chicago %>%
  st_transform(4326) %>%
  bind_cols(ca_lookup)

# ── Pre-computed aggregations ─────────────────────────────────────────────────
ridership_by_month <- ridership_attributes %>%
  filter(day_type == "Weekday") %>%
  group_by(year_month) %>%
  summarise(rides_total = sum(rides_total), .groups = "drop")

station_ccvi_lookup <- rail_stations %>%
  st_drop_geometry() %>%
  select(station_id, ccvi_cat)

station_line_lookup <- rail_stations %>%
  st_drop_geometry() %>%
  select(station_id, station_line)

ridership_by_ccvi <- ridership_attributes %>%
  filter(day_type == "Weekday") %>%
  left_join(station_ccvi_lookup, by = "station_id") %>%
  group_by(year_month, ccvi_cat) %>%
  summarise(rides_total = sum(rides_total), .groups = "drop") %>%
  drop_na(ccvi_cat) %>%
  mutate(ccvi_cat = factor(ccvi_cat, levels = c("LOW", "MEDIUM", "HIGH"), ordered = TRUE))

ridership_by_line <- ridership_attributes %>%
  filter(day_type == "Weekday") %>%
  left_join(station_line_lookup, by = "station_id") %>%
  group_by(year_month, station_line) %>%
  summarise(rides_total = sum(rides_total), .groups = "drop") %>%
  drop_na(station_line)

station_ridership_weekday <- ridership_attributes %>%
  filter(day_type == "Weekday") %>%
  select(station_id, year_month, rides_total)

# ── Value box scalars (Tab 2) ─────────────────────────────────────────────────
rides_2019 <- ridership_by_month %>%
  filter(format(year_month, "%Y") == "2019") %>%
  pull(rides_total) %>% sum(na.rm = TRUE)

latest_year_avail <- max(ridership_attributes$year)

rides_latest <- ridership_by_month %>%
  filter(format(year_month, "%Y") == as.character(latest_year_avail)) %>%
  pull(rides_total) %>% sum(na.rm = TRUE)

recovery_pct <- round(rides_latest / rides_2019 * 100, 1)

# ── Station change map: month choices ─────────────────────────────────────────
all_month_choices <- {
  ms <- sort(unique(station_ridership_weekday$year_month))
  setNames(as.character(ms), format(ms, "%B %Y"))
}
