## ---------------------------
##
## Script name: 
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2022-03-30
## Date Last Updated: 2022-03-30
## Email: c.scott.smith@depaul.edu
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

# Install (if necessary) and activate packages

# install.packages(c("censusapi",
#                     "tigris",
#                     "tidyverse",
#                     "dplyr",
#                     "sf"))

library(censusapi) # used to access census attribute data
library(tigris)  # used to access census geometries 
library(tidyverse)
library(dplyr) 
library(sf)
library(clipr)

# Part One: Select profile community, download, transform spatial  --------
# Download  census geographies
# cb = cartographic boundary
US_Places_geom <- places(cb=TRUE, class="sf")
IL_Tracts_geom <- tracts("IL", cb=TRUE, class="sf")

# Project to appropriate UTM zone
# Project to UTM North Zone 16
IL_Counties_geom <- st_transform(IL_Counties_geom, crs = 26916)
IL_Tracts_geom <- st_transform(IL_Tracts_geom, crs = 26916)
IL_Places_geom <- st_transform(IL_Places_geom, crs = 26916)

# Filter counties layer to get return only Cook County
CookCounty_geom <- IL_Counties_geom %>%
  filter(COUNTYFP=="031")

# Use censusapi package to retrieve attribute data from the different datasets
# Use below to list all census apis available
# If you don't have one, request a data key using following link 
# https://api.census.gov/data/key_signup.html

apis <- listCensusApis()

# Download latest list of ACS 5-year tables
# source api: https://api.census.gov/data/2019/acs/acs5

ayear <- "2020"

# download detailed table list
acs_groups_tables <- listCensusMetadata(
  name = "acs/acs5",
  vintage = ayear,
  type = "groups")
acs_groups_tables$year<-ayear # add year variable
assign(paste("acs_5yr_tables_detailed_",ayear,sep=""),acs_groups_tables) # change name of dataframe
rm(acs_groups_tables)

# download subject tables list
acs_groups_tables <- listCensusMetadata(
  name = "acs/acs5/subject",
  vintage = ayear,
  type = "groups")
acs_groups_tables$year<-ayear # add year variable
assign(paste("acs_5yr_tables_subject_",ayear,sep=""),acs_groups_tables) # change name of dataframe
rm(acs_groups_tables)

# download variables for ACS data table

# use for detailed tables 
agroup <- "B01001"
acs_groups_vars <- listCensusMetadata(
  name = "acs/acs5",
  vintage = ayear,
  group = agroup,
  type = "variables")
acs_groups_vars$year<-ayear
acs_groups_vars <- acs_groups_vars %>% 
  filter(!str_detect(name,"EA"),!str_detect(name,"M")) %>% 
  mutate(label = str_replace_all(label, "Estimate!!",""),
         label = str_replace(label, "!!"," "),
         label = str_replace_all(label, "!!"," "))
assign(paste("table_",agroup,"_",ayear, sep=""),acs_groups_vars)
rm(acs_groups_vars)

# use for subject tables
agroup <- "B01001"
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
assign(paste("table_",agroup,"_",ayear, sep=""),acs_groups_vars)
rm(acs_groups_vars)

# Use write_clip to copy to clipboard (and paste to Excel or other program) or
# use write_csv to write data table to computer

# example
write_clip(groupvars_B28001_2020)
write_csv(groupvars_B28001_2020,"C:/Users/scott/Desktop/delete/groupvars_B28001_2019.csv")

# Part Two: Create themes, select variables/indicators to highlight -----------------

# The following code allows you to download datasets from multiple tables
grouplist <- c("B01001")

# Download data for tracts
ayear = "2020"
astateFIPS = "*" # state in which place is located
acountyFIPS = "031" # county in which place is located

for (agroup in grouplist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "tract:*", 
                           regionin= paste0("state:",astateFIPS,"+county:",acountyFIPS), 
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group <- acs_group %>% select(-contains("M"))
    acs_group$year<-ayear 
    acs_group$GEOID<-paste0(state,county,tract)
    assign(paste(agroup,ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
}

for (agroup in grouplist) {
  agroupname = paste("group(",agroup,")",sep="")
  acs_group <- getCensus(name = "acs/acs5",
                         vintage = ayear,
                         vars = c("NAME", agroupname),
                         region = "place:*", 
                         regionin= paste0("state:",astateFIPS), 
                         key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
  attach(acs_group)
  acs_group <- acs_group %>% select(-contains("EA"))
  acs_group <- acs_group %>% select(-contains("MA"))
  acs_group <- acs_group %>% select(-contains("GEO_ID"))
  acs_group <- acs_group %>% select(-contains("M_1"))
  acs_group <- acs_group %>% select(-contains("M"))
  acs_group$year<-ayear 
  assign(paste(agroup,ayear,sep="_"),acs_group)
  rm(acs_group)
  detach(acs_group)
}

# 
population_by_place_2020 <- B01001_2020 %>%
  mutate(GEOID = paste0(state,place)) %>%
  select(state, place, GEOID, pop_2020 = B01001_001E) %>%
  left_join(US_Places_geom, by="GEOID") %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  filter(pop_2020>=100000)

write_clip(population_by_place_2020)
  