# Title: data_transformation_mapping.R 

# Purpose: Downloads TIGER/Line files, Census ACS attribute and CDC PLACES data.
# Joins TIGER/Line geometries with attribute data. Map using basic plot and
# leaflet.
#
# Author: C. Scott Smith 
# Organization: DePaul University
# Contact Email: c.scott.smith@depaul.edu
# 
# Date Created: 1/8/2021 
# Last Updated: 1/11/2021 

# Activate R packages (install if necessary using)
library(tidyverse)
library(dplyr) 
library(clipr)
library(sf)
library(tigris)
library(censusapi)
library(ggplot2)
library(leaflet)

# Download census block, census tract and place geographies
# cb = cartographic boundary
# Project both to UTM North Zone 16
# https://geocompr.robinlovelace.net/reproj-geo-data.html
cc_county_geom <- counties("IL", cb=TRUE, class="sf", year="2019") %>% filter(COUNTYFP=="031") %>% st_transform(crs = 26916)
cc_tracts_geom <- tracts("IL", cb=TRUE, class="sf", year="2019") %>% filter(COUNTYFP=="031") %>% st_transform(crs = 26916)
cc_places_geom <- places("IL", cb=TRUE, class="sf", year="2019") %>% st_transform(crs = 26916)

# Use censusapi package to retrieve attribute data from the different datasets
# Use below to list all census apis available
apis <- listCensusApis()

# Download complete list of ACS 5-year detailed tables
# https://www.census.gov/programs-surveys/acs
ayear <- "2019"
acs_groups_tables <- listCensusMetadata(
  name = "acs/acs5",
  vintage = ayear,
  type = "groups")
acs_groups_tables$year<-ayear # add year variable
assign(paste("acs_tables_",ayear,sep=""),acs_groups_tables) # change name of dataframe
rm(acs_groups_tables)

# Tables of interest
# 
# B01001: SEX BY AGE
# B17001: POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE
# B22010: RECEIPT OF FOOD STAMPS/SNAP BY DISABILITY STATUS
# B27010: TYPES OF HEALTH INSURANCE COVERAGE BY AGE (Civilian noninstitutionalized population)
# B03002: HISPANIC OR LATINO ORIGIN BY RACE
# B25044: TENURE BY VEHICLES AVAILABLE
# B08126: MEANS OF TRANSPORTATION TO WORK BY INDUSTRY
# B26001: GROUP QUARTERS
# B26101: GROUP QUARTERS TYPE BY SEX AND AGE
# B25024: UNITS IN STRUCTURE
# B25014: TENURE PER OCCUPANTS PER ROOM
# B23022: SEX BY WORK STATUS IN THE PAST 12 MONTHS BY USUAL HOURS WORKED PER WEEK
# B18101: SEX BY AGE BY DISABILITY STATUS
# B25106: HOUSING COSTS AS PERCENTAGE OF INCOME
# B28011: INTERNET SUBSCRIPTIONS
# B28001: DESKTOP OR LAPTOP COMPUTER
# C16002: HOUSEHOLD LANGUAGE BY HOUSEHOLD LIMITED ENGLISH SPEAKING STATUS
# S2401: WORKERS BY INDUSTRY ESSENTIAL WORKERS

# Variables for ACS data table
ayear <- "2019"
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
assign(paste("acs_table_vars_",agroup,"_",ayear, sep=""),acs_groups_vars)
rm(acs_groups_vars)

# download data for all variables in listed tables
grouplist <- c("B01001","B03002")
yearlist <- c(2019)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    acs_group <- getCensus(name = "acs/acs5",
                           vintage = ayear,
                           vars = c("NAME", agroupname),
                           region = "tract:*", # tracts
                           regionin="state:17&county:031", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    head(acs_group)
    attach(acs_group)
    acs_group <- acs_group %>% select(-contains("EA"))
    acs_group <- acs_group %>% select(-contains("M"))
    acs_group <- acs_group %>% select(-contains("MA"))
    acs_group <- acs_group %>% select(-contains("NAME_1"))
    acs_group <- acs_group %>% select(-contains("GEO_ID"))
    acs_group <- acs_group %>% select(-contains("M_1"))
    acs_group$year<-ayear 
    acs_group$GEOID<-paste(state,county,tract,sep="")
    assign(paste(agroup,ayear,sep="_"),acs_group)
    rm(acs_group)
  }
}

# POPULATION BY AGE, GENDER
B01001_2019_sub <- B01001_2019 %>%
  rowwise() %>%
  mutate(TotPop = B01001_001E,
         PopDep = sum(c_across(B01001_003E:B01001_006E)) + sum(c_across(B01001_020E:B01001_025E)) + sum(c_across(B01001_027E:B01001_030E)) + sum(c_across(B01001_044E:B01001_049E)),
         Pop18Un = sum(c_across(B01001_003E:B01001_006E)) + sum(c_across(B01001_027E:B01001_030E)),
         Pop65Pl = sum(c_across(B01001_020E:B01001_025E)) + sum(c_across(B01001_044E:B01001_049E)),
         Pop12Pl = B01001_005E*3/5 + sum(c_across(B01001_006E:B01001_025E)) + B01001_029E*3/5 + sum(c_across(B01001_030E:B01001_049E)),
         Pop16Pl = B01001_006E*2/3 + sum(c_across(B01001_007E:B01001_025E)) + B01001_030E*2/3 + sum(c_across(B01001_031E:B01001_049E))) %>%
  select(GEOID,TotPop:Pop16Pl) %>%
  mutate_at(vars(PopDep:Pop16Pl), .funs = (list(pct = ~(./TotPop*100))))

# B03002: HISPANIC OR LATINO ORIGIN BY RACE         
B03002_2019_sub <- B03002_2019 %>%
  rowwise() %>%
  mutate(TotalRace = B03002_001E,
         PopBlk = B03002_004E,
         PopAsn = B03002_006E,
         PopLat = B03002_012E,
         PopClr = B03002_001E-B03002_003E,
         PopWht = B03002_003E) %>%
  select(GEOID, TotalRace:PopWht) %>%
  mutate_at(vars(PopBlk:PopWht), .funs = (list(pct = ~(./TotalRace*100))))

# join census tracts with census data
cc_tracts_geom_census <- cc_tracts_geom %>% 
  left_join(B01001_2019_sub, by="GEOID") %>% 
  left_join(B03002_2019_sub, by="GEOID")

# import PLACES data from CDC
# Notice: This is a VERY large dataset and make take a while to download
places_2021_tract <- read_csv("https://chronicdata.cdc.gov/api/views/cwsq-ngmh/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

cc_places_2021_tract <- places_2021_tract %>%
  filter(CountyFIPS=="17031")

places_2021_tract_measures <- places_2021_tract %>% 
  group_by(Category,Measure, MeasureId) %>% 
  summarise()

write_csv(cc_places_2021_tract, "cc_places_2021_tract.csv")

# now for some data wrangling
# https://github.com/rstudio/cheatsheets/blob/main/tidyr.pdf
cc_places_2021_tract_sub <- cc_places_2021_tract %>% 
  filter(MeasureId=="CSMOKING" | 
           MeasureId=="CANCER" | 
           MeasureId=="ASTHMA" | 
           MeasureId=="DIABETES"| 
           MeasureId=="COPD" | 
           MeasureId=="SLEEP") %>%
  select(GEOID = LocationName, MeasureId, Data_Value) %>%
  pivot_wider(names_from = "MeasureId", values_from = c("Data_Value"))

# join census tracts with table with PLACES data 
cc_tracts_geom_census_places <- cc_tracts_geom_census %>% 
  left_join(cc_places_2021_tract_sub, by="GEOID") %>%
  st_transform(crs=4326)

# create maps
# https://jtr13.github.io/cc19/different-ways-of-plotting-u-s-map-in-r.html

# create basic legend with five categories
pal <- colbrewer.pal(5, "OrRd")

plot(cc_tracts_geom_census_places["PopDep_pct"], 
     main = "% Dependent Population", 
     breaks = "quantile", nbreaks=5,
     pal = pal)

# create interactive map in leaflet
# https://rstudio.github.io/leaflet/colors.html
# https://rstudio.github.io/leaflet/choropleths.html
# https://www.r-graph-gallery.com/179-show-a-map-with-leaflet-r.html

pal_fun <- colorQuantile("RdBu", NULL, n = 5, reverse = TRUE) # creates color pattern for all maps

# create descriptive statistics pop-up
attach(cc_tracts_geom_census_places)
tract_popup <- paste("<strong>Tract #: </strong>", GEOID,
                     "<br>Population: ", round(TotPop,digits=2),"<br>",
                     "% 18 Under: ", round(Pop18Un_pct,digits=2),"<br>",
                     "% 65 Older: ", round(Pop65Pl_pct,digits=2),"<br>",
                     "Cancer Rate: ", round(CANCER,digits=2),"<br>",
                     sep="")

leaflet(cc_tracts_geom_census_places) %>% 
  addPolygons(stroke = FALSE, 
              fillColor = ~pal_fun(DIABETES), 
              fillOpacity = 0.5, 
              smoothFactor = 0.5, 
              popup = tract_popup) %>% 
  addTiles() %>% 
  addLegend(pal = pal_fun, 
            values = DIABETES, 
            opacity = 0.7, 
            title = NULL, 
            position = "bottomright") %>% 
  addProviderTiles(providers$CartoDB.Positron)
