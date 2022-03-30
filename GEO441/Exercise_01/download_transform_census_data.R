# Name: Census ACS and TIGER/Line file downloads

# Purpose: Downloads TIGER/Line files and ACS attribute data. Joins TIGER/Line
# geometries with attribute data.
#
# Author: C. Scott Smith 
# Organization: DePaul University
# Contact Email: c.scott.smith@depaul.edu
# 
# Date Created: 2/28/2020 
# Last Updated: 2/28/2020 

# Install (if necessary) and activate packages

# install.packages(c("censusapi",
#                     "tigris",
#                     "tidyverse",
#                     "dplyr",
#                     "sf"))

library(censusapi)
library(tigris) 
library(tidyverse)
library(dplyr) 
library(sf) 

# Part One: Select profile community, download, transform spatial  --------
# Download  census geographies
# cb = cartographic boundary
US_Places_geom <- places(cb=TRUE, class="sf")
US_State_geom <- states(resolution='500k', cb=TRUE, class="sf")
IL_Counties_geom <- counties("IL", cb=TRUE, class="sf")
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
astateFIPS = "17" # state in which place is located
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



