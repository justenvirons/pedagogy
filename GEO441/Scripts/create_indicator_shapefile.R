## Script header ------------------------------------------------------

## Script name: 
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2022-03-30
## Date Last Updated: 2022-03-30
## Email: c.scott.smith@depaul.edu
##
## Notes: Use this script to download census data and create a custom indicator
## shapefile by census tract. The indicator shapefile will be used in ArcGIS Pro
## to create maps for your community health atlas and complete GEO 441 exercise #1. 

## IMPORTANT: Leave script as is aside from code located within "START EDIT" and
## "END EDIT" tags. Edit those areas as directed.

# Install (if needed) and attach R packages -----------------------------------------

# install.packages(c("censusapi",
#                    "tigris",
#                    "tidyverse",
#                    "dplyr",
#                    "sf",
#                    "clipr",
#                    "units"))

library(censusapi) # used to access census attribute data
library(tigris)  # used to access census geometries 
library(tidyverse) # used for data wrangling
library(dplyr) # used for data wrangling, piping code
library(sf) # used for processing, transforming spatial features
library(clipr) # used for copying tables to clipboard
library(units) # used for setting units in feature layers

# Assign US Census API key code variable -----------------------------------------

# Note that the key below is my personal key code. Feel free to use this key for
# Exercise #1. However, I please request your own census API key code via URL
# below for future exercises.
# https://api.census.gov/data/key_signup.html

akey = "8f6a0a83c8a2466e3e018a966846c86412d0bb6e"

# FYI. To download a comprehensive list of census APIs, uncomment and run code
# on next line.
# apis <- listCensusApis()

# Part One: Download, project spatial data for selected community ----------------------
# identify appropriate NAD 83 UTM zone and projected coordinate system using the World
# UTM Grid in ArcGIS Pro and the https://spatialreference.org website.

# START EDIT
place_code = "60000" # insert selected place code from GEOID column in places_subject_tables.xlsx spreadsheet
state_code = "42" # insert associated state code from GEOID column in places_subject_tables.xlsx spreadsheet
crs_code = 26918 # insert projected coordinate system (aka coordinate reference system) code associated with place location
# END EDIT

state_geom <- states(cb=TRUE, 
                     class="sf") %>%
  filter(STATEFP == state_code) %>%
  st_transform(crs=crs_code) %>%
  select(state_name = NAME,
         geoid_state = GEOID)

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

# plot basic map of state, place and census tracts
plot(state_geom['geoid_state'])
plot(place_geom['geoid_place'])
plot(tracts_geom['dimension'])

# Part Two: Select indicators from table-specific variable lists ---------------------------------

# download complete list of variables for selected tables (use to identify
# particular indicators you'd like to include in atlas)
# you can also browse tables, values using census URL
# https://data.census.gov/cedsci/table

# START EDIT
ayear = 2020 # insert data year
agrouplist = c("S0101","S0801","S2801","S2301") # insert list of selected ACS subject tables (in quotes, separated by commas)
# END EDIT

for(agroup in agrouplist) {
  acs_groups_vars <- listCensusMetadata(
    name = "acs/acs5/subject",
    vintage = ayear,
    group = agroup,
    type = "variables")
  acs_groups_vars$year<-ayear
  acs_groups_vars <- acs_groups_vars %>% 
    filter(!str_detect(name,"EA"),!str_detect(name,"M")) %>% 
    mutate(label = str_replace_all(label, "Estimate!!",""),
           label = str_replace(label, "!!"," "),
           label = str_replace_all(label, "!!"," "))
  assign(paste("vars_",agroup,"_",ayear, sep=""),acs_groups_vars)
  rm(acs_groups_vars)
}

# Download variable data for selected tables by community/place. Examine values
# in these tables to be certain that the variables you select are represented as
# percentages, not counts and that missing values are minimal.

for (agroup in agrouplist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5/subject",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = paste0("place:",place_code),
                           regionin=paste0("state:",state_code), # places, counties, not msas
                           key=akey)
    head(acs_group)
    acs_group <- acs_group %>% 
      select(-contains("EA"),
             -contains("MA"),
             -contains("NAME_1"),
             -contains("M_1"),
             -contains("M")) %>%
      mutate(year<-ayear) %>%
      t() %>%
      as.data.frame()
    assign(paste("data_place",agroup,ayear,sep="_"),acs_group)
    rm(acs_group)
}

# Part Three: Download indicator data and create associated shapefile --------
# Once you are comfortable with the indicators you’ve selected, modify the lists
# below with specific variable names ("aindicatorvarlist"). Also rename the
# variables in the process so they are more intuitive ("aindicatornameslist").
# Be certain that the variables and corresponding names are in identical order.

# START EDIT
aindicatorvarlist <- c("S0101_C01_001E",
                       "S0101_C01_034E",
                       "S0801_C01_003E",
                       "S0801_C01_010E",
                       "S0801_C01_013E",
                       "S2301_C02_013E") # insert list of selected tables (in quotes, separated by commas)

aindicatornameslist <- c("totalpop",
                         "agedep",
                         "drovealone",
                         "walked",
                         "fromhome",
                         "lfblack") # rename variables (max length = 8 chars) in order
# END EDIT

# download the variables by census tract, transform table based on information provided in above lists 

acs_group <- getCensus(
  name = "acs/acs5/subject",
  vintage = ayear,
  vars = c("NAME", aindicatorvarlist),
  region = paste0("tract:*"),
  regionin = paste0("state:", state_code),
  # places, counties, not msas
  key = akey
)
head(acs_group)
acs_group <- acs_group %>%
  mutate(geoid_tract = paste0(state, county, tract),
         year = ayear) %>%
  select(
    -contains("EA"),-contains("MA"),-contains("NAME_1"),-contains("M_1"),-contains("M"),-(state:tract)
  ) %>%
  rename_at(vars(aindicatorvarlist), ~ aindicatornameslist) %>%
  as.data.frame()
acs_group[acs_group < 0] <- NA
assign(paste("indicators_tract", ayear, sep = "_"), acs_group)
rm(acs_group)

# Perform an attribute join to attach indicator data to census tract geographies

indicators_tract_2020_geom <- tracts_geom %>%
  left_join(indicators_tract_2020, by="geoid_tract")

# Write census geographies and indicator layer to shapefiles.
# revise path name for layers directory if necessary

# START EDIT
apathname = "Layers/"
# END EDIT

st_write(indicators_tract_2020_geom,paste0(apathname,"indicators_tract.shp"), append = FALSE)
st_write(state_geom,paste0(apathname,"state.shp"), append = FALSE)
st_write(place_geom,paste0(apathname,"place.shp"), append = FALSE)
