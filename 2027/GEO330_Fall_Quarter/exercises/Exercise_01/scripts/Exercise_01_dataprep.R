## Header ------------------------------------------------------------------

## Script name: Exercise_01_dataprep.R
## Purpose of script: Build every dataset and plotting/mapping function used
##   by Exercise_01.qmd, and save them all to data/Exercise_01.RData so the
##   exercise document (and each student's own Exercise_01.R script) can
##   simply load() the file and render/run without re-downloading anything.
## Author: C. Scott Smith, PhD AICP
## Date Created: 2026-09-06
## Date Last Updated: 2026-09-10
## Email: c.scott.smith@depaul.edu
##
## Notes:
##   Run this script top to bottom from a fresh R session. It downloads ACS
##   commute mode share data (2010-2024) and Census TIGER/Line geographies,
##   builds every data frame/sf object the exercise needs, defines every
##   plotting/mapping function called in Exercise_01.qmd, and finishes by
##   saving all of it to data/Exercise_01.RData.

# Activate packages -------------------------------------------------------
library(censusapi)          # download census attribute data
library(tigris)              # download census geographies/geometries
library(tidyverse)           # data wrangling
library(data.table)          # data wrangling (rbindlist)
library(sf)                  # simple features geometry data
library(plotly)              # interactive plots used by every fx_plot_* function
library(leaflet)             # interactive maps used by every fx_*map* function
library(leaflet.extras)      # fullscreen/reset map controls
library(leafsync)            # syncs pan/zoom across side-by-side leaflet maps
library(spatstat.geom)       # spatial point pattern objects (fx_map_pedfatalityhotspot)
library(spatstat.explore)    # kernel density estimation (fx_map_pedfatalityhotspot)
library(terra)                # raster processing (fx_map_pedfatalityhotspot)
library(classInt)            # Jenks natural breaks for map classes

# Set working directory ---------------------------------------------------
setwd("2027/GEO330_Fall_Quarter/exercises/Exercise_01")

# API keys ------------------------------------------------------------------
# Prefer keys set as environment variables; fall back to the working keys
# below so the script still runs unattended (e.g. in a fresh Posit Cloud
# project) if those environment variables aren't set.
CENSUS_API_KEY <- Sys.getenv("CENSUS_API_KEY", unset = "8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
CARTO_API_KEY  <- Sys.getenv("CARTO_API_KEY",  unset = "cb1_31xh_1_06fb99adb466f0f5a0392d7d")

# Download attribute data from ACS by county -------------------------------
# reference field names for means of transportation to work table
# https://api.census.gov/data/2020/acs/acs5/groups/B08301.html
acs_table <- "B08301"
yearlist  <- 2010:2024

acs_list <- vector("list", length(yearlist))
names(acs_list) <- yearlist

for (ayear in yearlist) {
  agroupname <- paste0("group(", acs_table, ")")
  acs_group <- getCensus(name = "acs/acs5",
                          vintage = ayear,
                          vars = c("NAME", "B01001_001E", agroupname),
                          region = "county:*",
                          key = CENSUS_API_KEY)

  acs_group <- acs_group %>%
    select(-contains(c("EA", "MA", "GEO_ID", "M_1")), -ends_with("M"))

  acs_group$year <- ayear
  acs_group$GEOID_county <- paste0(acs_group$state, acs_group$county)

  acs_list[[as.character(ayear)]] <- acs_group
  message("Downloaded ACS data for ", ayear)
}

modeshare_county_raw <- rbindlist(acs_list)
rm(acs_list, acs_group)

# Download census geographies using tigris ---------------------------------
us_states_geom    <- states(class = "sf")
us_counties_geom  <- counties(class = "sf", cb = TRUE, resolution = "20m")
us_divisions_geom <- divisions(class = "sf")

# Format mode share data: join state/division names, rename fields, and
# transform mode share counts to percentages ------------------------------
modeshare_county_latest_formatted <- modeshare_county_raw %>%
  left_join(us_states_geom %>%
              st_drop_geometry() %>%
              select(state_name = NAME,
                     GEOID_state = STATEFP,
                     GEOID_division = DIVISION),
            by = c("state" = "GEOID_state")) %>%
  left_join(us_divisions_geom %>%
              st_drop_geometry() %>%
              select(division_name = NAME,
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
         pct_carpool = carpool/workers16pl*100,
         pct_transit = transit/workers16pl*100,
         pct_taxi = taxi/workers16pl*100,
         pct_motorcycle = motorcycle/workers16pl*100,
         pct_bicycle = bicycle/workers16pl*100,
         pct_walk = walk/workers16pl*100,
         pct_fromhome = fromhome/workers16pl*100,
         pct_other = other/workers16pl*100) %>%
  select(GEOID_county,
         county_name,
         state_name,
         division_name,
         year,
         total_population,
         workers16pl,
         drovealone,
         carpool,
         transit,
         taxi,
         motorcycle,
         bicycle,
         walk,
         fromhome,
         pct_drovealone:pct_other)

# Latest year (2024) subset, contiguous US only, for county-level maps ----
modeshare_county_latest <- modeshare_county_latest_formatted %>%
  filter(year == 2024,
         state_name != "Alaska",
         state_name != "Hawaii")

# Join latest year data to county geometries for mapping -------------------
modeshare_county_latest_geom <- us_counties_geom %>%
  mutate(sqmi = ALAND / 2589988.11) %>%
  select(GEOID_county = GEOID, sqmi) %>%
  left_join(modeshare_county_latest, by = "GEOID_county") %>%
  mutate(pop_density = total_population/sqmi) %>%
  st_as_sf() %>%
  st_transform(4326) %>%
  drop_na()

us_states_sub_geom <- modeshare_county_latest_geom %>%
  group_by(state_name) %>%
  summarize(geometry = st_union(geometry))

us_divisions_sub_geom <- modeshare_county_latest_geom %>%
  group_by(division_name) %>%
  summarize(geometry = st_union(geometry))

# Mode share long format, for the interquartile range box plot -------------
modeshare_county_period_pivoted <- modeshare_county_latest_formatted %>%
  select(county_name:year, pct_drovealone:pct_other) %>%
  pivot_longer(cols = c(pct_drovealone:pct_other),
               names_to = "mode",
               values_to = "percent") %>%
  mutate(mode = str_replace(mode, "pct_", ""),
         mode = str_replace(mode, "drovealone", "drove alone"),
         mode = str_replace(mode, "fromhome", "from home"),
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

# Download UTM zones, used to pick a local projected CRS for the pedestrian
# fatality hot spot density surface -----------------------------------------
utm_zones <- st_read("data/utm_zones.geojson") %>%
  st_set_crs(4326)

# Define plotting and mapping functions used in Exercise_01.qmd -----------

# function for creating mode share plot
fx_plot_modeshare <- function(geoid = "all") {

  modeBarButtonsList <- list("toImage")

  plot_data <- modeshare_county_latest_formatted %>%
    { if (geoid != "all") filter(., GEOID_county == geoid) else . } %>%
    select(year, pct_fromhome, pct_bicycle, pct_transit, pct_walk) %>%
    drop_na() %>%
    group_by(year) %>%
    summarise(`from home` = mean(pct_fromhome),
              `bicycle`   = mean(pct_bicycle),
              `walk`      = mean(pct_walk),
              `transit`   = mean(pct_transit))

  plot_ly(data = plot_data,
          x = ~year,
          y = ~`from home`,
          type = 'scatter',
          mode = 'lines+markers',
          marker = list(color = "#E0A100"),
          line = list(color = "#E0A100"),
          name = 'from home',
          hovertemplate = 'from home: %{y:.1f}<extra></extra>') %>%
    add_trace(x = ~year,
              y = ~`walk`,
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(color = "#9F1928"),
              line = list(color = "#9F1928"),
              name = 'walk',
              hovertemplate = 'walk: %{y:.1f}<extra></extra>') %>%
    add_trace(x = ~year,
              y = ~`transit`,
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(color = "#009BA6"),
              line = list(color = "#009BA6"),
              name = 'transit',
              hovertemplate = 'transit: %{y:.1f}<extra></extra>') %>%
    add_trace(x = ~year,
              y = ~`bicycle`,
              type = 'scatter',
              mode = 'lines+markers',
              marker = list(color = "#080967"),
              line = list(color = "#080967"),
              name = 'bicycle',
              hovertemplate = 'bicycle: %{y:.1f}<extra></extra>') %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Commute Mode Share (%)"),
      legend = list(
        font = list(size = 10),
        orientation = "h",
        xanchor = "center",
        x = 0.5, y = -0.1),
      hovermode = "x unified") %>%
    config(displaylogo = FALSE,
           modeBarButtons = list(modeBarButtonsList),
           toImageButtonOptions = list(
             format = "png",
             filename = "mode_share_chart"
           ))
}

# function for creating pedestrian commuters plot
fx_plot_activetrans <- function(geoid = "all") {

  modeBarButtonsList <- list("toImage")

  plot_data <- modeshare_county_latest_formatted %>%
    { if (geoid != "all") filter(., GEOID_county == geoid) else . } %>%
    select(year, walk) %>%
    drop_na() %>%
    group_by(year) %>%
    summarise(`walk`      = sum(walk))

  plot_ly(data = plot_data) %>%
    add_trace(
      x = ~ year,
      y = ~ `walk`,
      type = 'scatter',
      mode = 'lines+markers',
      marker = list(color = "#9F1928"),
      line = list(color = "#9F1928"),
      name = 'walk',
      hovertemplate = 'walk: %{y:.1f}<extra></extra>'
    )  %>%
    layout(
      xaxis = list(title = ""),
      yaxis = list(title = "Walk Commuters"),
      legend = list(
        font = list(size = 10),
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.1
      ),
      hovermode = "x unified"
    ) %>%
    config(
      displaylogo = FALSE,
      modeBarButtons = list(modeBarButtonsList),
      toImageButtonOptions = list(format = "png", filename = "mode_share_chart")
    )
}

# function for creating quintile bins for a requested transportation mode
fx_create_bins <- function(pct_mode) {
  bins_mode <- modeshare_county_latest %>%
    mutate(bins = ntile(get(pct_mode), 5)) %>%
    group_by(bins) %>%
    summarise(max = round(max(get(pct_mode)), 2)) %>%
    select(max) %>%
    unlist()

  bins_mode[1] <- 0 # set initial bin to 0
  bins_mode[5] <- bins_mode[5] + 0.01 # round up quintile to include all values

  return(bins_mode)
}

# function for creating a mode-specific county mode share map
fx_create_map <- function(pct_mode, bincolors, legend_title) {

  # format/generate popup labels for map
  countylabels <- sprintf(
    "<strong>%s Division</strong><br/>
    <strong>%s</strong><br/>
    Total workers (16+): %s<br/>
    Walk: %0.1f%%<br/>
    Bicycle: %0.1f%%<br/>
    Transit: %0.1f%%<br/>
    Drove Alone: %0.1f%%<br/>
    Home: %0.1f%%",
    modeshare_county_latest_geom$division_name,
    modeshare_county_latest_geom$county_name,
    format(modeshare_county_latest_geom$workers16pl, big.mark = ","),
    modeshare_county_latest_geom$pct_walk,
    modeshare_county_latest_geom$pct_bicycle,
    modeshare_county_latest_geom$pct_transit,
    modeshare_county_latest_geom$pct_drovealone,
    modeshare_county_latest_geom$pct_fromhome) %>%
    lapply(htmltools::HTML)

  # create mode-specific color palette
  pal_mode <- colorBin(bincolors, domain = modeshare_county_latest_geom[[pct_mode]], bins = fx_create_bins(pct_mode))

  # create mode-specific leaflet map
  mode_map <- leaflet(modeshare_county_latest_geom) %>%
    addTiles(urlTemplate = paste0(
      "https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=",
      CARTO_API_KEY
    )) %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery (satellite)") %>%
    addPolygons(
      fillColor = ~ pal_mode(get(pct_mode)),
      weight = 0.5,
      opacity = 0.75,
      color = "white",
      fillOpacity = 0.75,
      label = countylabels,
      group = "Counties"
    ) %>%
    addPolylines(
      data = us_states_sub_geom,
      stroke = TRUE,
      weight = 2,
      opacity = 1,
      fillOpacity = 0,
      color = "#FFF",
      group = "States"
    ) %>%
    addPolylines(
      data = us_divisions_sub_geom,
      stroke = TRUE,
      weight = 2,
      opacity = 1,
      fillOpacity = 0,
      color = "black",
      group = "Divisions"
    ) %>%
    addLegend(
      "bottomleft",
      pal = pal_mode,
      values = ~ pct_mode,
      title = legend_title,
      labFormat = labelFormat(suffix = "%"),
      opacity = 1
    ) %>%
    addResetMapButton() %>%
    addFullscreenControl() %>%
    addLayersControl(
      baseGroups = c("Positron (minimal)", "World Imagery (satellite)"),
      overlayGroups = c("Counties", "States", "Divisions"),
      options = layersControlOptions(collapsed = FALSE)
    )

  return(mode_map)
}

# function for plotting fatalities by mode and the yearly pedestrian fatality trend
fx_plot_pedfatalitytrend <- function(crashes_fars) {

  modeBarButtonsList <- list("toImage")

  # Identify pedestrian-involved fatal crashes
  # harm_ev == 8 (Pedestrian) captures the crash's first harmful event as a pedestrian strike;
  # peds > 0 also works as a check that at least one pedestrian was involved
  pedestrian_fatalities <- crashes_fars %>%
    filter(harm_ev == 8 | peds > 0) %>%
    group_by(year) %>%
    summarise(total_fatalities = sum(fatals, na.rm = TRUE),
              total_crashes = n(),
              .groups = "drop") %>%
    arrange(year)

  all_fatalities <- crashes_fars %>%
    select(harm_ev, persons, year) %>%
    mutate(collision_type = case_when(
      harm_ev == 9         ~ "Pedalcyclist",
      harm_ev == 8          ~ "Pedestrian",
      is.na(harm_ev)        ~ "Other",   # missing/unknown -- can't confirm type
      TRUE                  ~ "Motor Vehicle"
    )) %>%
    group_by(year, collision_type) %>%
    summarise(n = sum(persons, na.rm = TRUE), .groups = "drop") %>%
    complete(year, collision_type, fill = list(n = 0)) %>%
    arrange(year)

  colors <- c(
    "Motor Vehicle" = "#4C72B0",
    "Pedestrian"    = "#9F1928", # matches "walk" color in mode share plots above
    "Pedalcyclist"  = "#080967", # matches "bicycle" color in mode share plots above
    "Other"         = "#C44E52"
  )

  # Set a consistent stacking order (bottom to top)
  category_order <- c("Motor Vehicle", "Pedestrian", "Pedalcyclist", "Other")
  all_fatalities$collision_type <- factor(all_fatalities$collision_type, levels = category_order)

  # Share of fatalities each mode represents within a given year, for hover text
  all_fatalities <- all_fatalities %>%
    group_by(year) %>%
    mutate(total = sum(n),
           pct = if_else(total > 0, n / total * 100, 0)) %>%
    ungroup() %>%
    select(-total)

  # Build the stack one trace per mode, with an explicit fillcolor, rather than
  # relying on plot_ly's automatic color/colors domain mapping -- that mapping
  # was not reliably honoring the requested hex values above.
  plot_by_mode <- plot_ly(
    data = filter(all_fatalities, collision_type == "Motor Vehicle"),
    x = ~year,
    y = ~n,
    type = "scatter",
    mode = "none",
    stackgroup = "one",
    fillcolor = colors[["Motor Vehicle"]],
    name = "Motor Vehicle",
    customdata = ~pct,
    hovertemplate = "%{fullData.name}: %{y} (%{customdata:.1f}% of total)<extra></extra>"
  ) %>%
    add_trace(
      data = filter(all_fatalities, collision_type == "Pedalcyclist"),
      x = ~year,
      y = ~n,
      type = "scatter",
      mode = "none",
      stackgroup = "one",
      fillcolor = colors[["Pedalcyclist"]],
      name = "Bicyclist",
      customdata = ~pct,
      hovertemplate = "%{fullData.name}: %{y} (%{customdata:.1f}% of total)<extra></extra>"
    ) %>%
    add_trace(
      data = filter(all_fatalities, collision_type == "Pedestrian"),
      x = ~year,
      y = ~n,
      type = "scatter",
      mode = "none",
      stackgroup = "one",
      fillcolor = colors[["Pedestrian"]],
      name = "Pedestrian",
      customdata = ~pct,
      hovertemplate = "%{fullData.name}: %{y} (%{customdata:.1f}% of total)<extra></extra>"
    ) %>%
    add_trace(
      data = filter(all_fatalities, collision_type == "Other"),
      x = ~year,
      y = ~n,
      type = "scatter",
      mode = "none",
      stackgroup = "one",
      fillcolor = colors[["Other"]],
      name = "Other",
      customdata = ~pct,
      hovertemplate = "%{x}<br>%{fullData.name}: %{y} (%{customdata:.1f}% of total)<extra></extra>"
    ) %>%
    layout(
      title = "Fatalities by Mode and Year, 2010 to 2024",
      xaxis = list(title = "", dtick = 2),
      yaxis = list(title = "Number of Fatalities"),
      legend = list(
          text = "Collision Type",
          font = list(size = 10),
          orientation = "h",
          xanchor = "center",
          x = 0.5, y = -0.1),
      hovermode = "x unified"
    ) %>%
    config(displaylogo = FALSE,
           modeBarButtons = list(modeBarButtonsList))

  plot_pedestrian <- plot_ly(
    pedestrian_fatalities,
    x = ~year,
    y = ~total_fatalities,
    type = "scatter",
    mode = "lines+markers",
    line = list(color = "firebrick", width = 3),
    marker = list(color = "firebrick", size = 8)
  ) %>%
    layout(
      title = "Pedestrian Fatalities by Year, 2010 to 2024",
      xaxis = list(title = "", dtick = 2),
      yaxis = list(title = "Number of Pedestrian Fatalities"),
      hovermode = "x unified"
    ) %>%
    config(displaylogo = FALSE,
           modeBarButtons = list(modeBarButtonsList))

  list(plot_by_mode, plot_pedestrian)
}

# function for mapping pedestrian fatalities across four multi-year periods
fx_map_pedfatalityperiods <- function(ped_fatalities, county_geoid) {

  us_county_boundary <- modeshare_county_latest_geom %>%
    select(county_name, GEOID_county) %>%
    filter(GEOID_county == county_geoid)

  ## Step A: define rolling 4 year periods across 2010 to 2024
  period_breaks <- c(2010, 2014, 2018, 2022, 2025)
  period_labels <- c("2010 to 2013", "2014 to 2017", "2018 to 2021", "2022 to 2024")

  ped_fatalities <- ped_fatalities %>%
    mutate(period = cut(year, breaks = period_breaks, right = FALSE, labels = period_labels))

  ## Step B: common map extent (unname to avoid named vector JSON warnings)
  county_bbox <- st_bbox(us_county_boundary)
  bbox_vals <- unname(county_bbox)
  names(bbox_vals) <- c("xmin", "ymin", "xmax", "ymax")

  ## Step C: build one map per period
  period_maps <- lapply(period_labels, function(p) {

    ped_period <- ped_fatalities %>% filter(period == p)

    m <- leaflet() %>%
      addTiles(urlTemplate = paste0(
        "https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=",
        CARTO_API_KEY
      )) %>%
      addPolygons(
        data = us_county_boundary,
        fill = FALSE, color = "black", weight = 1.5, opacity = 1
      ) %>%
      fitBounds(
        lng1 = as.numeric(bbox_vals["xmin"]), lat1 = as.numeric(bbox_vals["ymin"]),
        lng2 = as.numeric(bbox_vals["xmax"]), lat2 = as.numeric(bbox_vals["ymax"])
      ) %>%
      addResetMapButton() %>%
      addControl(
        html = paste0("<strong>", p, "</strong> (n = ", nrow(ped_period), ")"),
        position = "topright"
      )

    if (nrow(ped_period) > 0) {
      m <- m %>%
        addCircleMarkers(
          data = ped_period, radius = 4, color = "firebrick",
          stroke = FALSE, fillOpacity = 0.7
        )
    }

    m
  })

  ## Step D: arrange as a synced 2x2 grid (pan and zoom linked across all panels)
  latticeView(
    period_maps[[1]], period_maps[[2]], period_maps[[3]], period_maps[[4]],
    ncol = 2,
    sync = "all"
  )
}

# function for mapping the pedestrian fatality hotspot density surface
fx_map_pedfatalityhotspot <- function(ped_fatalities, county_geoid) {

  ## Determine local UTM CRS from county centroid
  us_county_boundary <- modeshare_county_latest_geom %>%
    filter(GEOID_county == county_geoid) %>%
    select(county_name, GEOID_county) %>%
    st_set_crs(4326)

  county_centroid <- st_centroid(us_county_boundary)

  county_centroid_zoned <- county_centroid %>%
    st_join(utm_zones %>% select(ZONE), join = st_within) %>%
    st_drop_geometry()

  local_crs <- county_centroid_zoned$ZONE + 32600

  ## Estimate the kernel density (KDE) surface
  ped_ft  <- st_transform(ped_fatalities, crs = local_crs)
  ped_win <- as.owin(st_convex_hull(st_union(ped_ft)))
  ped_ppp <- as.ppp(st_coordinates(ped_ft), W = ped_win)

  ped_density <- density.ppp(ped_ppp, sigma = bw.diggle(ped_ppp), edge = TRUE)

  # Rescale from events per square meter to events per square kilometer (x 1e6)
  # so values aren't vanishingly small. This is purely for readable legend
  # labels and does not change the pattern of the surface.
  dens_r <- as.data.frame(ped_density) %>%
    rename(value = 3) %>%
    mutate(value = value * 1e6) %>%
    terra::rast(type = "xyz", crs = paste0("EPSG:", local_crs))

  dens_r_ll <- terra::project(dens_r, "EPSG:4326", method = "bilinear")

  ## Clip the raster to the county boundary
  county_vect <- vect(us_county_boundary)
  dens_r_ll <- terra::crop(dens_r_ll, county_vect)
  dens_r_ll <- terra::mask(dens_r_ll, county_vect)

  ## Bin density into readable classes (natural breaks on log scale)
  dens_vals <- terra::values(dens_r_ll, na.rm = TRUE)

  # log1p handles the near-zero values gracefully (log1p(0) = 0)
  log_vals <- log1p(dens_vals)

  breaks_log <- classIntervals(log_vals, n = 5, style = "jenks")$brks %>%
    unique()

  # Convert breakpoints back to the original density scale for the legend/palette
  breaks <- expm1(breaks_log)

  dens_pal <- colorBin(
    palette  = "YlOrRd",
    domain   = dens_vals,
    bins     = breaks,
    na.color = "transparent"
  )

  ## Map density surface with county boundary and fatality points
  leaflet() %>%
    addTiles(urlTemplate = paste0(
        "https://basemaps.cartocdn.com/rastertiles/light_all/{z}/{x}/{y}.png?key=",
        CARTO_API_KEY
      )) %>%
    addRasterImage(
      dens_r_ll, colors = dens_pal, opacity = 0.65,
      group = "Density Surface"
    ) %>%
    addPolygons(
      data = us_county_boundary,
      fill = FALSE, color = "darkgrey", weight = 2, opacity = 1,
      group = "County Boundary"
    ) %>%
    addCircleMarkers(
      data = ped_fatalities, radius = 3, color = "#555555", weight = 1,
      fillOpacity = 0, stroke = TRUE,
      group = "Pedestrian Fatalities"
    ) %>%
    addLegend(
      pal = dens_pal, values = dens_vals,
      title = "Pedestrian fatality<br>density (per km²)",
      position = "bottomright",
      labFormat = labelFormat(digits = 1)
    ) %>%
      addResetMapButton() %>%
      addFullscreenControl() %>%
    addLayersControl(
      overlayGroups = c("Density Surface", "County Boundary", "Pedestrian Fatalities"),
      options = layersControlOptions(collapsed = FALSE)
    )
}

# Save in RData file format -----------------------------------------------
# Everything a student's Exercise_01.R script or Exercise_01.qmd needs after
# a single load("data/Exercise_01.RData") call: every data frame/sf object
# referenced in the document, the CARTO tile key, and every plotting/mapping
# function called anywhere in the exercise.

save(modeshare_county_latest,
     modeshare_county_latest_geom,
     modeshare_county_latest_formatted,
     modeshare_county_period_pivoted,
     us_states_sub_geom,
     us_divisions_sub_geom,
     utm_zones,
     CARTO_API_KEY,
     fx_plot_modeshare,
     fx_plot_activetrans,
     fx_create_bins,
     fx_create_map,
     fx_plot_pedfatalitytrend,
     fx_map_pedfatalityperiods,
     fx_map_pedfatalityhotspot,
     file = paste0(getwd(), "/data/Exercise_01.RData"))
