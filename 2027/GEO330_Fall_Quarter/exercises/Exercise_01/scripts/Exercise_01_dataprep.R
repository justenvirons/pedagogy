## Header ------------------------------------------------------------------

## Script name: Exercise_01_dataprep.R
## Purpose of script: Create dataset for methods exercise #1 concerning mode share
## Author: C. Scott Smith, PhD AICP
## Date Created: 2026-09-06
## Date Last Updated: 2026-09-09
## Email: c.scott.smith@depaul.edu
##
## Notes:
##   

# Activate packages -------------------------------------------------------
library(censusapi) # used to download census attribute data
library(tigris) # used to download census geographies/geometries
library(tidyverse) # used for data wrangling
library(data.table) # used for data wrangling
library(tidyr) # used for data wrangling
library(dplyr)  # used for data wrangling
library(openxlsx) # used for reading/writing from/to Excel
library(sf) # used for reading geography data

# Set working directory ---------------------------------------------------
setwd("2027/GEO330_Fall_Quarter/exercises/Exercise_01")

# Retrieve API keys
carto_api_key <- Sys.getenv("CARTO_API_KEY")
census_api_key <- Sys.getenv("CENSUS_API_KEY")

# Download attribute data from ACS by county ------------------------------------------
grouplist <- c("B08301")
yearlist <- c(2010:2024)

for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", "B01001_001E",agroupname),
                           region = "county:*", # tracts
                           # regionin="*", # places, counties, not msas
                           key=CENSUS_API_KEY)
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains(c("EA", 
                                                  "MA", 
                                                  "GEO_ID", 
                                                  "M_1")),
                                      -ends_with("M"))
    acs_group$year<-ayear # append data with data year
    acs_group$GEOID_county<-paste0(state,county)
    assign(paste(agroup,"county",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
    print(paste0("Downloading data for ",as.character(ayear)))
  }
}

apattern <- paste(agroup,"county",sep="_")
alist_dfs <- mget(ls(pattern = apattern))
modeshare_county_latest<- rbindlist(alist_dfs)
rm(list = ls(pattern = apattern))

# Download census geographies using tigris --------------------------------
us_states_geom <- states(class="sf")
us_counties_geom <- counties(class="sf", cb=TRUE, resolution = "20m")
us_divisions_geom <- divisions(class="sf")

us_counties_geom <- st_read(county_filename_shp) %>%
  mutate(STATEFP_NO = as.numeric(STATEFP)) %>%
  filter(STATEFP_NO <= 56, STATEFP_NO != 15, STATEFP_NO != 2)



# Download UTM zones
utm_zones <- st_read("data/utm_zones.geojson") %>%
  st_set_crs(4326)

county_centroids <- us_counties_geom %>%
  st_centroid()

county_centroids_zoned <- county_centroids %>%
  st_join(utm_zones %>% select(ZONE), join = st_within) %>%
  st_drop_geometry() %>%
  select(GEOID, ZONE)

us_counties_with_zone <- us_counties_geom %>%
  left_join(county_centroids_zoned, by = "GEOID")

# transform mode share counts to percentages
# reference field names for means of transportation to work table
# https://api.census.gov/data/2020/acs/acs5/groups/B08301.html

modeshare_county_latest_formatted <- modeshare_county_latest %>%
  left_join(us_states_geom %>% 
              st_drop_geometry() %>%
              select(
                state_name = NAME,
                GEOID_state = STATEFP,
                GEOID_division = DIVISION),
            by = c("state" = "GEOID_state")) %>%
  left_join(us_divisions_geom %>% 
              st_drop_geometry() %>%
              select(
                division_name = NAME,
                GEOID_division = GEOID),
            by = "GEOID_division") %>%
  drop_na(division_name) %>%
rename(county_name = NAME,
       total_population = B01001_001E,
       workers16pl = B08301_001E,
       drovealone = B08301_003E,
       carpool = B08301_004E,
       transit = B08301_010E,
       taxi = B08301_016E,
       motorcycle = B08301_017E,
       bicycle = B08301_018E,
       walk = B08301_019E,
       fromhome = B08301_021E) %>%
  mutate(other = workers16pl - drovealone - carpool - transit - taxi - motorcycle - bicycle - walk - fromhome,
         pct_drovealone = drovealone/workers16pl*100,
         pct_carpool=carpool/workers16pl*100,
         pct_transit=transit/workers16pl*100,
         pct_taxi=taxi/workers16pl*100,
         pct_motorcycle=motorcycle/workers16pl*100,
         pct_bicycle=bicycle/workers16pl*100,
         pct_walk=walk/workers16pl*100,
         pct_fromhome=fromhome/workers16pl*100,
         pct_other=other/workers16pl*100) %>%
  select(GEOID_county,
         county_name,
         state_name,
         division_name,
         year,
         total_population,
         workers16pl,
         total_population,
         drovealone,
         carpool,
         transit,
         taxi,
         motorcycle,
         bicycle,
         walk,
         fromhome,
         pct_drovealone:pct_other)

# Mode share wide ---------------------------------------------------------
modeshare_county_period_pivoted <- modeshare_county_latest_formatted %>%
  select(county_name:year,pct_drovealone:pct_other) %>%
  pivot_longer(cols = c(pct_drovealone:pct_other),
               names_to = "mode",
               values_to = "percent") %>%
  mutate(mode = str_replace(mode, "pct_",""),
         mode = str_replace(mode, "drovealone","drove alone"),
         mode = str_replace(mode, "fromhome","from home"),
         order = case_when(mode == "drove alone" ~ 1,
                           mode == "motorcycle" ~ 2,
                           mode == "taxi" ~ 3,
                           mode == "carpool" ~ 4,
                           mode == "transit" ~ 5,
                           mode == "bicycle" ~ 6,
                           mode == "walk" ~ 7,
                           mode == "from home" ~ 8,
                           mode == "other" ~ 9)) %>%
  arrange(order)

# join counties to latest population data
us_counties_population_geo <- us_counties_geom %>%
  select(NAME,
         STATE_NAME,
         GEOID) %>%
  left_join(modeshare_county_latest_formatted %>%
              filter(year) %>%  
              select("GEOID",
                     total_population = B01001_001E),
            by="GEOID")  %>%
  mutate(total_pop_q = ntile(total_pop, 5)) %>%
  st_as_sf()


# API Keys
CARTO_API_KEY <- "cb1_31xh_1_06fb99adb466f0f5a0392d7d"
CENSUS_API_KEY <- "8f6a0a83c8a2466e3e018a966846c86412d0bb6e"

# Create plot function
# option set
modeBarButtonsList <- list("toImage")

# function for creating modeshare plot
fx_plot_modeshare <- function(geoid = "all") {
  
  plot_data <- modeshare_county_latest_formatted %>%
    { if (geoid != "all") filter(., GEOID_county == geoid) else . } %>%
    select(year, pct_fromhome, pct_bicycle, pct_transit, pct_walk) %>%
    drop_na() %>%
    group_by(year) %>%
    summarise(`from home` = mean(pct_fromhome),
              `bicycle`   = mean(pct_bicycle),
              `walk`      = mean(pct_walk),
              `transit`   = mean(pct_transit))
  
  plot_ly(data = plot_data,
          x = ~year,
          y = ~`from home`,
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(color = "#E0A100"),
          line = list(color = "#E0A100"),
          name = 'from home',
          hovertemplate = 'from home: %{y:.1f}<extra></extra>') %>%
    add_trace(x = ~year,
              y = ~`walk`,
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(color = "#9F1928"),
              line = list(color = "#9F1928"),
              name = 'walk',
              hovertemplate = 'walk: %{y:.1f}<extra></extra>') %>%
    add_trace(x = ~year,
              y = ~`transit`,
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(color = "#009BA6"),
              line = list(color = "#009BA6"),
              name = 'transit',
              hovertemplate = 'transit: %{y:.1f}<extra></extra>') %>%
    add_trace(x = ~year,
              y = ~`bicycle`,
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(color = "#080967"),
              line = list(color = "#080967"),
              name = 'bicycle',
              hovertemplate = 'bicycle: %{y:.1f}<extra></extra>') %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Commute Mode Share (%)"),
      legend = list(
        font = list(size = 10),
        orientation = "h",
        xanchor = "center",
        x = 0.5, y = -0.1),
      hovermode = "x unified")
}

# Save in RData file format -----------------------------------------------

save(modeshare_county_latest_formatted,
     modeshare_county_period_pivoted,
     us_counties_population_geo,
     us_states_geom,
     us_divisions_geom,
     utm_zones,
     CARTO_API_KEY,
     fx_plot_modeshare,
     file = paste0(getwd(),"/data/Exercise_01.RData"))

