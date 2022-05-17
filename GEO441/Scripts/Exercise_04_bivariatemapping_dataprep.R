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


# import building code scofflaw data from the City of Chicago data portal -------------------------
building_scofflaw_list <- read_csv("https://data.cityofchicago.org/api/views/hgat-td99/rows.csv?accessType=DOWNLOAD")

# convert building scofflaw table to simple feature dataset
transit_stops <- local_gtfs$stops # save stops data as separate data table
coordinates(transit_stops) = ~stop_lon+stop_lat # create point from lat, longs
transit_stops_geom <- transit_stops %>%
  st_as_sf() %>% 
  st_set_crs(4326) %>%
  st_transform(crs=crs_code) %>%
  select(-stop_url,
         -stop_desc)



# Import SVI data from the CDC website ------------------------------------


