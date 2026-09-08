#############################################################################
# Shiny app: draw a line on the pedalcyclist hotspot map and export it as
# KML or KMZ in EPSG:4326 (the "world" / lon-lat projection KML requires).
#
# Requires: shiny, leaflet, leaflet.extras, sf, geojsonsf, zip
#   install.packages(c("shiny", "leaflet.extras", "geojsonsf", "zip"))
#
# Assumes `leaflet_map` (the leaflet object built in step 5 of the main
# analysis script) already exists in the environment/session.
#############################################################################

library(shiny)
library(leaflet)
library(leaflet.extras)
library(sf)
library(geojsonsf)
library(zip)

## ---------------------------------------------------------------
## UI
## ---------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Pedalcyclist Crash Hotspots - Draw & Export"),
  p("Use the polyline tool (top-left of the map) to draw a line, then download it below."),
  leafletOutput("map", height = 700),
  br(),
  downloadButton("download_kml", "Download as KML"),
  downloadButton("download_kmz", "Download as KMZ")
)

## ---------------------------------------------------------------
## Server
## ---------------------------------------------------------------
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    # Reuse the existing hotspot map and layer on drawing tools.
    # Only the polyline tool is enabled -- flip the *Options flags to TRUE
    # if you also want polygons, markers, etc.
    leaflet_map %>%
      addDrawToolbar(
        targetGroup    = "drawn",
        polylineOptions = drawPolylineOptions(
          shapeOptions = drawShapeOptions(color = "blue", weight = 4)
        ),
        polygonOptions      = FALSE,
        circleOptions       = FALSE,
        rectangleOptions    = FALSE,
        markerOptions       = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)
      )
  })
  
  # Holds the most recently drawn feature as an sf object (CRS 4326).
  # leaflet.extras fires `input$<mapId>_draw_new_feature` with a GeoJSON
  # Feature (as a nested R list) every time a shape is finished.
  drawn_sf <- reactiveVal(NULL)
  
  observeEvent(input$map_draw_new_feature, {
    feat_json <- jsonlite::toJSON(input$map_draw_new_feature, auto_unbox = TRUE)
    geom <- geojsonsf::geojson_sf(feat_json)
    st_crs(geom) <- 4326   # leaflet draws in lon/lat, so this just labels it
    drawn_sf(geom)
  })
  
  # Also update on edits (e.g., if the user drags a vertex) if desired:
  observeEvent(input$map_draw_edited_features, {
    feat_json <- jsonlite::toJSON(input$map_draw_edited_features, auto_unbox = TRUE)
    geom <- geojsonsf::geojson_sf(feat_json)
    st_crs(geom) <- 4326
    drawn_sf(geom)
  })
  
  output$download_kml <- downloadHandler(
    filename = function() "drawn_line.kml",
    content = function(file) {
      req(drawn_sf())
      st_write(drawn_sf(), file, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
    }
  )
  
  output$download_kmz <- downloadHandler(
    filename = function() "drawn_line.kmz",
    content = function(file) {
      req(drawn_sf())
      # KMZ is just a zipped KML -- Google Earth expects the inner file
      # to be named doc.kml by convention
      tmp_dir <- tempfile()
      dir.create(tmp_dir)
      tmp_kml <- file.path(tmp_dir, "doc.kml")
      st_write(drawn_sf(), tmp_kml, driver = "KML", delete_dsn = TRUE, quiet = TRUE)
      zip::zip(zipfile = file, files = "doc.kml", root = tmp_dir)
    }
  )
}

shinyApp(ui, server)