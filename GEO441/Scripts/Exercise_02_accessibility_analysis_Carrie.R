## Script header ------------------------------------------------------

## Script name: Exercise_02_accessibility_analysis.R
## Purpose of script: Download, process input layers for accessibility analysis
## Author: C. Scott Smith, PhD AICP
## Date Created: 2022-03-30
## Date Last Updated: 2022-03-30
## Email: c.scott.smith@depaul.edu
##
## Notes: Use this script to download census tract and GTFS transit stop/station
## data, create origin and destination shapefiles to analyze in ArcGIS Pro. 

## IMPORTANT: Leave script as is aside from code located within "START EDIT" and
## "END EDIT" tags. Edit those areas as directed.

# Install (if needed) and attach R packages -----------------------------------------
library(tidytransit) # used to input, summarize GTFS data
library(sp) # used to create sf from X,Y coordinates
library(ggplot2) # used to create map with multiple layers
library(dplyr)
library(tidyverse)
library(tigris)
library(sf)
library(units)

# Part One: Data download and preparation -----------------------------------------
# Download place and census tract data and create origin centroids from census
# tracts. State and place can be same as what you used in Exercise #1.

# START EDIT
place_code = "60000" # insert selected place code from GEOID column in places_subject_tables.xlsx spreadsheet
state_code = "42" # insert associated state code from GEOID column in places_subject_tables.xlsx spreadsheet
crs_code = 26918 # insert projected coordinate system (aka coordinate reference system) code associated with place location
# END EDIT

place_geom <- places(state=state_code, 
                     cb=TRUE, 
                     class="sf") %>% 
  filter(GEOID==paste0(state_code,place_code)) %>%
  st_transform(crs=crs_code) %>%
  select(place_name = NAME,
         geoid_place = GEOID)

tracts_geom <- tracts(state=state_code, 
                      cb=TRUE, 
                      class="sf") %>% 
  st_transform(crs=crs_code) %>%
  select(geoid_state = STATEFP,
         geoid_county = COUNTYFP,
         geoid_tract = GEOID) %>%
  st_intersection(place_geom) %>%
  st_as_sf() %>%
  mutate(dimension = st_dimension(geometry)) %>%
  filter(dimension==2) %>%
  select(-dimension) %>%
  st_make_valid() %>%
  st_cast("POLYGON") %>%
  mutate(tract_sqmi = set_units(st_area(geometry),"mi^2")) %>%
  filter(tract_sqmi > set_units(0.01,"mi^2"))

# create origin centroids from census tracts
tracts_centroids_geom <- tracts_geom %>%
  st_centroid() %>%
  st_as_sf()

# plot basic map of place,  census tracts and centroids
plot(place_geom['geoid_place'])
plot(tracts_geom['geoid_tract'])
plot(tracts_centroids_geom['geoid_tract'])

# Download GTFS data catalog from
# https://github.com/MobilityData/mobility-database-catalogs for complete list
# of GTFS data in US by state, city and transit provider.

gtfs_data_catalog <- read_csv("https://storage.googleapis.com/storage/v1/b/mdb-csv/o/sources.csv?alt=media") %>%
  filter(location.country_code=="US") %>%
  select(state_name = location.subdivision_name,
         city_name = location.municipality,
         transit_provider = provider,
         gtfs_url = urls.direct_download)

# On https://transitfeeds.com/feeds website, search/identify URL for transit
# data within your city. Download zip file to an appropriate folder. Provide
# complete path to where you downloaded gtfs data which includes file name.
# Hint: Make sure to use forward slashes instead of backslashes in path name.

# START EDIT
gtfs_filename = "~/OneDrive/GEO 441/Exercise 2/gtfs.zip"
# gtfs_filename = "E:/OneDrive - Cook County Health/git_repos/justenvirons/pedagogy/GEO441/Data/gtfs.zip"
# END EDIT

# input GTFS data from file
local_gtfs <- read_gtfs(gtfs_filename)
