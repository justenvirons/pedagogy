## ---------------------------
##
## Script name: Purpose of script: Author: C. Scott Smith, PhD AICP Date
## Created: 2022-01-08 Date Last Updated: 2022-01-08 Email:
## c.scott.smith@depaul.edu ---------------------------
##
## Notes: In-class exercise for GEO 3/446, using R and ArcGIS Pro to measure
## neighborhood (origins) access to transit stations (destinations)
##
##
## ---------------------------


# Activate packages -------------------------------------------------------
library(dplyr)
library(tidyverse)
library(tigris)
library(sf)
library(units)

# Import origin (census tract centroids), destination (public transit
# stations GTFS) data -----------------------------------------

# download study area boundary (Cook County) and origin locations (census block groups for Cook County)
# alternatively, download from https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html
cc_county_geom <- counties("IL", cb=TRUE, class="sf", year="2019") %>% filter(COUNTYFP=="031") %>% st_transform(crs = 26916) %>% select(GEOID)
cc_tracts_geom <- tracts("IL", cb=TRUE, class="sf", year="2019") %>% filter(COUNTYFP=="031") %>% st_transform(crs = 26916) %>% select(GEOID)

cc_tracts_geom <- tracts("IL", cb=TRUE, class="sf", year="2019") %>% filter(COUNTYFP=="031") %>% select(GEOID)

st_write(cc_tracts_geom, "exercise_03/layers/cc_tracts_geom.shp", append=FALSE)

  # create origin centroids from tracts
cc_tracts_centroids <- cc_tracts_geom %>%
  st_centroid() %>%
  st_as_sf()

# alternatively, create regular (hexagonal or square) grid using below code
cc_hexagons <- cc_county_geom %>%
  st_make_grid(cellsize = 1609, square = FALSE) %>%
  st_transform(crs=26916) %>%
  st_cast("POLYGON") %>%
  st_as_sf() %>%
  rename(geometry = x) %>%
  mutate(sqmi = st_area(geometry),
         sqmi = set_units(sqmi, mi^2),
         hexid = row_number())

cc_hexagons_centroids <- cc_hexagons %>%
  st_centroid() %>%
  st_as_sf()

# in web browser, download destination locations from https://transitfeeds.com/feeds
# extract contents of zip file into folder
# import stops/stations data from stops.txt into R, using below code

farmers_markets <- read_csv("https://data.cityofchicago.org/api/views/xwk2-c3ec/rows.csv?accessType=DOWNLOAD&bom=true&format=true") %>%
  drop_na(LATITUDE) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE")) %>%
  st_set_crs(4326)

cta_stops <- read_csv("exercise_03/data/stops.txt") %>%
  st_as_sf(coords = c("stop_lon", "stop_lat")) %>%
  st_set_crs(4326) %>%
  st_transform(crs=26916) %>%
  mutate(type = if_else(location_type==1, 
                        "train",
                        "bus")) %>%
  select(stop_id,
         type)

# spatially join train stations with census tracts (i.e., container approach) ------------------------

# sample code using your own data

# destinations_per_origin_polygon <- destinations %>% 
#   st_join(origin_polygons) %>%
#   st_drop_geometry() %>%
#   group_by(origin_polygon_unique_id) %>%
#   summarise(count=n())

cta_train_stations_tract <- cta_stops %>% 
  filter(type=="train") %>%
  st_join(cc_tracts_geom) %>%
  st_drop_geometry() %>%
  group_by(GEOID) %>%
  summarise(count=n())

# join counts with census tract polygons
cc_tracts_stations_geom <- cc_tracts_geom %>%
  left_join(cta_train_stations_tract, by="GEOID") %>%
  mutate(count = ifelse(is.na(count), 0, count))

# export as shapefiles the study area boundary, origin locations (as polygons and points/centroids) and desination locations (as points)
st_write(cc_county_geom, "exercise_03/layers/cc_county_geom.shp", append=FALSE)
st_write(cc_tracts_centroids, "exercise_03/layers/cc_tracts_centroids.shp", append=FALSE)
st_write(cc_tracts_geom, "exercise_03/layers/cc_tracts_geom.shp", append=FALSE)
st_write(cta_stops, "exercise_03/layers/cta_stops.shp", append=FALSE)


