## Header ------------------------------------------------------------------

## Script name: Exercise_01_dataprep.R
## Purpose of script: Create dataset for methods exercise #1 concerning mode share
## Author: C. Scott Smith, PhD AICP
## Date Created: 2023-01-04
## Date Last Updated: 2023-01-04
## Email: c.scott.smith@depaul.edu
##
## Notes:
##   



smartlocation_db <- st_read("C:/Users/scott/Desktop/SmartLocationDatabaseV3/SmartLocationDatabase.gdb")
smartlocation_db_layers <- st_layers(dsn = "C:/Users/scott/Desktop/SmartLocationDatabaseV3/SmartLocationDatabase.gdb")

walkability_index <- st_read("C:/Users/scott/Desktop/WalkabilityIndex/Natl_WI.gdb")
walkability_index_layers <- st_layers(dsn = "C:/Users/scott/Desktop/WalkabilityIndex/Natl_WI.gdb")



# Activate packages
library(censusapi)
library(tigris) 
library(tidyverse)
library(data.table)
library(tidyr)
library(dplyr) 
library(openxlsx)
library(sf)

# Download places
grouplist <- c("B08301")
yearlist <- c(2010:2021)

for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", "B01001_001E",agroupname),
                           region = "place:*", # tracts
                           # regionin="*", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group <- acs_group %>% select(-contains("M"))
    acs_group$year<-ayear 
    acs_group$GEOID_place<-paste0(state,place)
    assign(paste(agroup,"place",ayear,sep="_"),acs_group)
    rm(acs_group)
    detach(acs_group)
  }
}

apattern_place <- paste(agroup,"place",sep="_")
alist_place <- mget(ls(pattern = apattern_place))
modeshare_place_2000_2020 <- rbindlist(alist_place)

plot(us_divisions_geom)['GEOID']

# download census geographies
us_divisions_geom <- divisions(class="sf")
us_regions_geom <- regions(class="sf")
us_states_geom <- states(class="sf")
us_places_geom <- places(cb=TRUE, class="sf")

# join geographic ids (state, region and divisino) to places dataset
modeshare_place_2000_2020_geoids <- us_places_geom %>%
  st_drop_geometry() %>%
  select(GEOID_place = GEOID,
         GEOID_state = STATEFP,
         place_name = NAME,
         state_name = STATE_NAME) %>%
  left_join(us_states_geom %>%
              st_drop_geometry() %>%
              select(GEOID_state = GEOID,
                     GEOID_region = REGION,
                     GEOID_division = DIVISION),
            by = "GEOID_state") %>%
  left_join(us_regions_geom %>% 
              st_drop_geometry() %>% 
              select(GEOID_region = GEOID,
                     region_name = NAME),
            by = "GEOID_region") %>%
  left_join(us_divisions_geom %>% 
              st_drop_geometry() %>% 
              select(GEOID_division = GEOID,
                     division_name = NAME),
            by = "GEOID_division") %>%
  left_join(modeshare_place_2000_2020, 
            by = "GEOID_place")

# transform mode share counts to percentages
# reference field namees for means of transporrtation to work table
# https://api.census.gov/data/2020/acs/acs5/groups/B08301.html

modeshare_place_2000_2020_geoids_pcts <- modeshare_place_2000_2020_geoids %>%
rename(total_population = B01001_001E,
       workers16pl = B08301_001E,
       drovealone = B08301_003E,
       carpool = B08301_004E,
       transit = B08301_010E,
       taxi = B08301_016E,
       motorcycle = B08301_017E,
       bicycle = B08301_018E,
       walk = B08301_019E,
       fromhome = B08301_021E) %>%
  filter(total_population >= 10000) %>%
mutate(type = case_when(total_population < 50000 ~ "Small",
                        total_population >= 50000 &  total_population < 250000 ~ "Medium",
                        total_population > 250000 ~ "Large"),
       other = workers16pl - drovealone - carpool - transit - taxi - motorcycle - bicycle - walk - fromhome,
       pct_drovealone = drovealone/workers16pl*100,
       pct_carpool=carpool/workers16pl*100,
       pct_transit=transit/workers16pl*100,
       pct_taxi=taxi/workers16pl*100,
       pct_motorcycle=motorcycle/workers16pl*100,
       pct_bicycle=bicycle/workers16pl*100,
       pct_walk=walk/workers16pl*100,
       pct_fromhome=fromhome/workers16pl*100,
       pct_other=other/workers16pl*100) %>%
  select(year,
         place_name,
         state_name,
         region_name,
         division_name,
         type,
         total_population,
         workers16pl,
         pct_drovealone:pct_other)

modeshare_place_2000_2020_geoids_pcts %>% 
  group_by(type) %>%
  summarise(n())

# save in RData file format 
save(modeshare_place_2000_2020_geoids_pcts, file = "GEO330_2023_WinterQuarter/data/modeshare_place_2000_2020_geoids.RData")
write_csv(modeshare_place_2000_2020_geoids_pcts, file = "GEO330_2023_WinterQuarter/data/modeshare_place_2000_2020_geoids.csv")

