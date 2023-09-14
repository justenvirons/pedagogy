## Script header ------------------------------------------------------------------
## Script name: Exercise_01.R
## Purpose of script:
## Author: C. Scott Smith, PhD AICP
## Date Created: 2022-09-12
## Date Last Updated: 2022-09-12
## Email: c.scott.smith@depaul.edu
##
## Notes: Instructions for carrying out GEO 3/446 Exercise #1
##   

# Install and activate packages -------------------------------------------------------
# update.packages(ask=FALSE) # if needed, update all available packages installed on your system

library(censusapi) # retrieving census attribute data (population, housing) 
library(tigris) # retrieving census geometries
library(sf) # manipulating geometry data
library(leaflet) # making interactive maps
library(dplyr)
library(tidyverse)
library(ggplot2) # cartographic
library(biscale)
library(corrplot)
library(scales)
library(clipr)
library(patchwork)
library(RColorBrewer)

# Data formatting options -----------------------------------------------------------------
# turn off scientific notation
options(scipen=999,
        digits = 2)

## Download county census geometries in contiguous US with tigris package ---------------------------------------------
# cb = cartographic boundary
# sf = simple feature

us_counties_geom <- counties(cb=TRUE, class="sf", year = 2020) %>%
  mutate(STATEFP_NO = as.numeric(STATEFP)) %>%
  filter(STATEFP_NO <= 56, STATEFP_NO != 15, STATEFP_NO != 2)

# plot counties by state
plot(us_counties_geom['STATEFP'])

# Download census attributes using censusapi package ----------------------
# If you don't have a census api key, request one using following link 
# https://api.census.gov/data/key_signup.html

censusapikey <- "8f6a0a83c8a2466e3e018a966846c86412d0bb6e" # Scott's key

# Download 2020 American Community Survey (ACS) 5-year population estimates by county
# source api for these data
# https://api.census.gov/data/2020/acs/acs5

agroup <- c("B01001")
varlist <- c("B01001_001E")
yearlist <- c(2020)
for (ayear in yearlist) {
  agroupname = paste("group(",agroup,")",sep="")
  acs_group <- getCensus(name = "acs/acs5",
                         vintage = ayear,
                         vars = c("NAME",varlist),
                         region = "county:*",
                         regionin="state:*",
                         key=censusapikey)
  attach(acs_group)
  acs_group <- acs_group %>% select(-contains(c("EA",
                                                "MA",
                                                "GEO_ID",
                                                "M_1")))
  acs_group$year<-ayear 
  acs_group$GEOID<-paste0(state,county)
  assign(paste(agroup,ayear,sep="_"),acs_group)
  rm(acs_group)
  detach(acs_group)
}

# Join population data to county geometries
us_counties_population_geo <- us_counties_geom %>%
  select(NAME,
         STATE_NAME,
         GEOID) %>%
  left_join(B01001_2020 %>% 
              select("GEOID",
                     pop_2020 = B01001_001E),
            by="GEOID")  %>%
  mutate(pop_2020_q = ntile(pop_2020, 5)) %>%
  st_as_sf()

# Summary table of counties by population quintile category
us_counties_population_geo %>% 
  st_drop_geometry() %>%
  group_by(pop_2020_q) %>%
  summarise(count = n(),
            min_pop = min(pop_2020),
            max_pop = max(pop_2020),
            avg_pop = mean(pop_2020),
            sd_pop = sd(pop_2020))

# Make interactive map of US counties by population using leaflet package

counties_bins <- c(0, 8958, 18945, 36947, 999999, Inf) # loosely based on quintile breaks
counties_pal <- colorBin(
  palette="viridis", 
  domain=us_counties_population_geo$pop_2020, 
  na.color="transparent",
  bins = counties_bins,
  reverse = TRUE
  )

labels = sprintf(
  "<strong>%s, %s</strong><br/>
  Population (2020): %s<br/>
  GEOID: %s <br/>
  Quintile group: %s",
  us_counties_population_geo$NAME, 
  us_counties_population_geo$STATE_NAME,
  format(us_counties_population_geo$pop_2020, big.mark = ","),
  us_counties_population_geo$GEOID,
  us_counties_population_geo$pop_2020_q) %>% 
  lapply(htmltools::HTML)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(group = "US counties",
              data=us_counties_population_geo %>% st_transform(crs=4326),
              # fillColor = "orange",
              fillColor = ~counties_pal(pop_2020),
              weight = 0.5,
              opacity = 0.5,
              color = "white",
              dashArray = "3",
              fillOpacity = 0.7,
              highlight = highlightOptions(
                weight = 5,
                color = "#666",
                dashArray = "",
                fillOpacity = 1,
                bringToFront = FALSE),
              label = labels,
              labelOptions = labelOptions(
                style = list("font-weight" = "normal", padding = "3px 8px"),
                textsize = "15px",
                direction = "auto")) %>%
  addLegend("topright", pal = counties_pal, values = us_counties_population_geo$pop_2020,
            title = paste0("Population (2020)"),
            opacity = 1) %>%
  addScaleBar(position = "bottomleft")

# START REVIEW
# view a dataframe of counties with >= 1 million population
# select one of the counties to examine in exercise #1
view(us_counties_population_geo %>% 
       select(-pop_2020_q) %>%
       st_drop_geometry() %>% 
       filter(pop_2020 >= 1000000) %>% 
       arrange(-pop_2020))
# END REVIEW

# Download, project spatial data for selected county ----------------------
# identify appropriate NAD 83 UTM zone and projected coordinate system using the World
# UTM Grid in ArcGIS Pro and the https://spatialreference.org website.

# START EDIT
county_fips = "17031" # insert the unique five-character code from the GEOID column in us_counties_large_population dataframe
crs_code = 4326 # insert projected coordinate system (aka coordinate reference system) code associated with place location
# END EDIT

tract_boundaries_geom <- tracts(state =  substr(county_fips,1,2), 
                           county = substr(county_fips,3,5), 
                           cb=TRUE, 
                           class="sf", 
                           year = 2010) %>%
  # st_transform(crs=crs_code) %>%
  # mutate(SQMI = ALAND*0.0000003861) %>%
  mutate(SQMI = CENSUSAREA,
         GEOID = paste0(STATE,COUNTY,TRACT)) %>%
  select(GEOID, SQMI) %>%
  st_as_sf()

plot(tract_boundaries_geom['SQMI'])

# Import health prevalence "PLACES" data from CDC -----------------------------------
# Descriptions of PLACES indicators
# https://www.cdc.gov/places/measure-definitions/index.html
# PLACES data portal
# https://chronicdata.cdc.gov/browse?q=PLACES%202021
# Notice: This is a large data file, so wait for download to complete before proceeding 

places_2021_tracts_all <- read_csv("https://chronicdata.cdc.gov/api/views/cwsq-ngmh/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

# create data dictionary for PLACES measures
# select at least four measures to evaluate in your county
places_data_dictionary_summary <- places_2021_tracts_all %>%
  filter(CountyFIPS==county_fips) %>%
  group_by(Category, Measure, MeasureId) %>%
  summarise(mean = mean(Data_Value),  
            min = min(Data_Value), 
            max = max(Data_Value), 
            sd = sd(Data_Value))

# START EDIT
# Replace the four MeasureIDs in this list with those you'd like to evaluate
places_measures_list <- c("CANCER","CHD","STROKE","DIABETES")
# END EDIT

# create summary data table for PLACES measures
places_data_dictionary_summary_sel <- places_data_dictionary_summary %>%
  filter(MeasureId %in% places_measures_list)

# copy and paste this summary table into your exercise #1 slide deck
write_clip(places_data_dictionary_summary_sel)

# create detailed data table of selected PLACES measures in wide format
places_2021_tracts_sel <- places_2021_tracts_all %>%
  filter(CountyFIPS==county_fips, MeasureId %in% places_measures_list) %>%
  select(GEOID = LocationName,
         MeasureId,
         Data_Value,
         Population = TotalPopulation) %>%
  pivot_wider(names_from=MeasureId, 
              values_from = Data_Value)

# perform attribute join detailed data table to census geometries mapping of PLACES measures
places_2021_tracts_sel_geom <- places_2021_tracts_sel %>% 
  left_join(tract_boundaries_geom, by = "GEOID") %>%
  mutate(Population_density = Population/SQMI) %>%
  st_as_sf()

# Import Social Vulnerability Index (SVI) data from CDC --------
svi_2018_tracts_all <- read_csv("https://data.cdc.gov/api/views/4d8n-kk8a/rows.csv?accessType=DOWNLOAD&bom=true&format=true")

# Import data dictionary from course GitHub
svi_data_dicionary <- read_csv("https://raw.githubusercontent.com/justenvirons/pedagogy/main/GEO346_2022_FallQuarter/Exercise_01/data/svi_2018_datadictionary.csv") %>%
  filter(str_detect(Name, "EP_|RPL_")) %>%
  arrange(Theme)

# START EDIT
# Replace the four SVI variable names in this list with those you'd like to evaluate
svi_factors_list <- c("RPL_THEME1","RPL_THEME2","RPL_THEME3","RPL_THEME4")
# END EDIT

# create detailed data table of selected SVI factors 
svi_2018_tracts_sel <- svi_2018_tracts_all %>%
  filter(substr(FIPS,1,5)==county_fips) %>%
  select(GEOID = FIPS,
         contains(svi_factors_list)) %>%
  mutate(across(where(is.numeric), ~na_if(., -999))) %>%
  mutate_at(svi_factors_list, function(x) rescale(x,to = c(0,100)))

# Create comprehensive table containing both PLACES and SVI data ----------
svi_places_tracts_sel_geom <- svi_2018_tracts_sel %>%
  left_join(places_2021_tracts_sel_geom, by = "GEOID") %>%
  st_as_sf() 

st_write(svi_places_tracts_sel_geom, "GEO346_2022_FallQuarter/Exercise_01/maps/svi_places_tracts_sel_geom.shp")

# Create bivariate correlation plot ---------------------------------------
# Add bivariate correlation plot to exercise #1 slide deck

svi_places_tracts_sel_sub <- svi_places_tracts_sel_geom %>%
  st_drop_geometry() %>%
  drop_na() %>%
  select(-c(GEOID,Population,SQMI,Population_density))

variable_biv_correlations <- cor(svi_places_tracts_sel_sub)

corrplot(variable_biv_correlations, method="number", col=c("black","white"), order="hclust", bg="lightgrey", tl.col = "grey")
corrplot(variable_biv_correlations, col=c("black","white"), order="hclust", bg="lightgrey", tl.col = "grey")

# Create custom single variable and bivariate thematic/chorpleth maps -------------------------------------
#  

# START EDIT
# modify variable(s) to be mapped and legend names/titles for all maps

# map1.PLACES
map1.Places <- ggplot() + geom_sf(data=svi_places_tracts_sel_geom,
                               mapping = aes(fill = ntile(CHD, 5)),
                               size = 0.1) + 
  scale_fill_distiller(name="CHD", 
                       palette = "Greys", 
                       breaks = pretty_breaks(),
                       direction = 1) + 
  theme_minimal()

ggsave(map1.Places,
       filename = "GEO346_2022_FallQuarter/Exercise_01/maps/map1_CHD.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")
       
# map1.SVI
map1.SVI <- ggplot() + geom_sf(data=svi_places_tracts_sel_geom,
                   mapping = aes(fill = ntile(RPL_THEME1, 5)),
                   size = 0.1) + 
  scale_fill_distiller(name="Socioeconomic", 
                       palette = "Greys", 
                       breaks = pretty_breaks(n=4),
                       direction = 1) + 
  theme_minimal()

ggsave(map1.SVI,
       filename = "GEO346_2022_FallQuarter/Exercise_01/maps/map1_SVITheme1.png",
       scale = 1,
       dpi = 300,
       width = 5.5,
       height = 5.5,
       units="in")

# create custom bivariate map based on two component variables
# bivariate map1 data
map1.data <- bi_class(svi_places_tracts_sel_geom, 
                 x = CHD, 
                 y = RPL_THEME1, 
                 style = "quantile", dim = 3)

# bivariate map 1 plot
# Available palettes: "Bluegill", "BlueGold", "BlueOr", "BlueYl",
# "Brown"/"Brown2", "DkBlue"/"DkBlue2", "DkCyan"/"DkCyan2",
# "DkViolet"/"DkViolet2", "GrPink"/"GrPink2", "PinkGrn", "PurpleGrn", or
# "PurpleOr"

map1 <- ggplot() +
  geom_sf(data = map1.data, 
          mapping = aes(fill = bi_class), 
          color = "grey", 
          size = 0.01, 
          show.legend = FALSE) +
  bi_scale_fill(pal = "BlueGold", dim = 3) + 
  theme_minimal()

# bivariate map 1 legend
map1.legend <- bi_legend(pal = "BlueGold",
                    dim = 3,
                    xlab = "Higher Prevalence",
                    ylab = "Higher SVI",
                    size = 5,
                    pad_width = 1) + 
  theme_minimal()

map1.bivariate <- map1 + map1.legend + plot_layout(widths = c(4,1))

ggsave(map1.bivariate,
       filename = "GEO346_2022_FallQuarter/Exercise_01/maps/map1_bivariate.png",
       scale = 1,
       dpi = 300,
       width = 6.5,
       height = 5.5,
       units="in")


# END EDIT
