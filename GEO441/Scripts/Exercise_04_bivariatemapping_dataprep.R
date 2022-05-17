## Script header ------------------------------------------------------

## Script name: 
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2022-05-05
## Date Last Updated: 2022-05-17
## Email: c.scott.smith@depaul.edu
##

## Notes: Use this script to download race, ethnicity and other socioeconomic
## data from the latest Social Vulnerability Index (SVI) published by the CDC
## as well as spatial data from the City of Chicago's Building Code Scofflaw
## List. A shapefile will be generated and imported into ArcGIS Pro to create a
## series of bivariate association maps to complete GEO 441 exercise #4.

## IMPORTANT: Leave script as is aside from code located within "START EDIT" and
## "END EDIT" tags. Edit those areas as directed.

# Install (if needed) and attach R packages -----------------------------------------

library(tigris)  # used to access census geometries 
library(tidyverse) # used for data wrangling
library(dplyr) # used for data wrangling, piping code
library(sf) # used for processing, transforming spatial features
library(units) # used for setting units in feature layers
library(sp) # used to create sf from X,Y coordinates

# import building code scofflaw and census tract boundary data from the City of Chicago data portal -------------------------
building_scofflaw_list <- read_csv("https://data.cityofchicago.org/api/views/hgat-td99/rows.csv?accessType=DOWNLOAD")

# convert building scofflaw table to simple feature dataset
coordinates(building_scofflaw_list) = ~LONGITUDE+LATITUDE # create point from lat, longs
building_scofflaw_list_geom <- building_scofflaw_list %>%
  st_as_sf() %>% 
  st_set_crs(4326) %>%
  st_transform(crs=26916)

# map unsafe buildings by community area
plot(building_scofflaw_list_geom['COMMUNITY AREA'])

# download census tracts for the city of Chicago
chicago_census_tracts <- st_read("https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON") %>%
  select(comarea_no = commarea,
         fips = geoid10) %>%
  st_transform(crs=26916)

# plot city of Chicago census tracts by fips code
plot(chicago_census_tracts['fips'])

# Import SVI data from the CDC website ------------------------------------
# Download Illinois SVI shapefile data by census tract from CDC website. Unzip file and extract into a folder within your project directory. 
# Specify below the folder within which the SVI shapefile is located.

# START EDIT
apathname = "GEO441/Layers/"
# END EDIT

svi_illinois <- st_read(paste0(apathname,"SVI2018_ILLINOIS_tract.shp")) %>%
  st_drop_geometry() %>%
  rename(fips = FIPS)

svi_chicago_by_census_tract <-  chicago_census_tracts %>% left_join(svi_illinois,by="fips")


# Write the housing and SVI shapefiles to your project directory ----------
st_write(building_scofflaw_list_geom, paste0(apathname,"building_scofflaw_list.shp"), append=FALSE)
st_write(svi_chicago_by_census_tract, paste0(apathname,"svi_chicago_by_census_tract.shp"), append=FALSE)
