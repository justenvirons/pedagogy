## Header ------------------------------------------------------------------

## Script name: Exercise_01_dataprep.R
## Purpose of script: Create dataset for methods exercise #1 concerning mode share
## Author: C. Scott Smith, PhD AICP
## Date Created: 2023-01-04
## Date Last Updated: 2024-01-06
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
setwd("C:/Users/juste/OneDrive - DePaul University/gitrepos/pedagogy/GEO330_2025_WinterQuarter/exercises/Exercise_01")

# Download attribute data from ACS by county ------------------------------------------
grouplist <- c("B08301")
yearlist <- c(2010:2023)

for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", "B01001_001E",agroupname),
                           region = "county:*", # tracts
                           # regionin="*", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
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
  }
}

apattern <- paste(agroup,"county",sep="_")
alist_dfs <- mget(ls(pattern = apattern))
modeshare_county_2000_latest<- rbindlist(alist_dfs)

# export as csv file
write_csv(modeshare_county_2000_latest, 
          file = paste0(getwd(),"/data/modeshare_county_2000_2023.csv"))

# Download census geographies using tigris --------------------------------
us_states_geom <- states(class="sf")
us_counties_geom <- counties(class="sf", cb=TRUE, resolution = "20m")
us_divisions_geom <- divisions(class="sf", resolution = "20m")
  
# transform mode share counts to percentages
# reference field namees for means of transporrtation to work table
# https://api.census.gov/data/2020/acs/acs5/groups/B08301.html

modeshare_county_2000_latest_formatted <- modeshare_county_2000_latest %>%
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

# export as csv file
write_csv(modeshare_county_2000_latest_formatted, 
          file = paste0(getwd(),"/data/modeshare_county_formatted_2000_latest.csv"))

write_csv(modeshare_county_2000_latest_formatted %>% 
  select(year, county_name:workers16pl, pct_drovealone:pct_other, drovealone:fromhome), 
  file = paste0(getwd(),"/data/modeshare_county_latest.csv"))


# Mode share wide ---------------------------------------------------------
modeshare_county_period_pivoted <- modeshare_county_2000_latest_formatted %>%
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

write_csv(modeshare_county_period_pivoted, 
          file = paste0(getwd(),"/data/modeshare_county_period_pivoted.csv"))


# Save in RData file format -----------------------------------------------

save(modeshare_county_period,
     modeshare_county_period_formatted,
     modeshare_county_period_pivoted,
     us_counties_geom,
     us_states_geom,
     us_divisions_geom,
     file = paste0(getwd(),"/data/Exercise_01.RData"))

