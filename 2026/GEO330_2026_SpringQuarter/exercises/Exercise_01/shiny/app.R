## app.R
## Mode Share Transportation Dashboard — GEO/SUD 3/430 Exercise #1
## C. Scott Smith, PhD AICP
## UI + Server; shared data objects loaded from global.R

library(shiny)
library(bslib)
library(bsicons)
library(dplyr)
library(tidyr)
library(stringr)
library(sf)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(DT)
library(htmltools)
library(scales)

options(scipen = 999, digits = 2)

# setwd(dir = file.path(getwd(), "2026/GEO330_2026_SpringQuarter/exercises/Exercise_01/shiny"))

rsconnect::writeManifest(
  appDir = ".",
  appFiles = c(
    "app.R",
    "Exercise_01.RData",   # now at root, not data/
    "styles.css"
  )
)

# ── Load RData ────────────────────────────────────────────────────────────────
# Objects: contains
# "latest_year"                       "data_dir"                          "us_counties_geom"                 
# "us_divisions_geom"                 "natl_avgs"                         "mode_colors"                      
# "all_divisions"                     "us_states_geom"                    "county_geom"                      
# "modeshare_latest_raw"              "us_divisions_sub_geom"             ".Random.seed"                     
# "map_palettes"                      "mode_cols"                         "all_states"                       
# "modeshare_county_period_pivoted"   "modeshare_county_period"           "state_division_lookup"            
# "all_years"                         "fx_create_bins"                    "modeshare_latest_geom"            
# "modeshare_county_period_formatted"

load(file = "Exercise_01.RData")

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- bslib::page_navbar(
  title  = "Mode Share Transportation Dashboard",
  theme  = bslib::bs_theme(bootswatch = "flatly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Tab 1: Mode Share Trends ──────────────────────────────────────────────
  bslib::nav_panel(
    "Mode Share Trends",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,

        strong("Geography"),
        selectizeInput(
          "trend_division", "Census Division(s)",
          choices  = all_divisions,
          multiple = TRUE,
          options  = list(placeholder = "All divisions")
        ),
        selectizeInput(
          "trend_state", "State(s)",
          choices  = all_states,
          multiple = TRUE,
          options  = list(placeholder = "All states")
        ),
        hr(),
        strong("Modes to Display"),
        checkboxGroupInput(
          "trend_modes",
          label  = NULL,
          choices  = names(mode_cols),
          selected = c("Walk", "Bicycle", "Transit", "Work from Home")
        )
      ),

      bslib::layout_columns(
        col_widths = c(3, 3, 3, 3),
        bslib::value_box(
          title    = textOutput("vbox_walk_title",     inline = TRUE),
          value    = textOutput("vbox_walk",           inline = TRUE),
          showcase = bs_icon("person-walking"),
          theme    = "danger"
        ),
        bslib::value_box(
          title    = textOutput("vbox_bicycle_title",  inline = TRUE),
          value    = textOutput("vbox_bicycle",        inline = TRUE),
          showcase = bs_icon("bicycle"),
          theme    = "primary"
        ),
        bslib::value_box(
          title    = textOutput("vbox_transit_title",  inline = TRUE),
          value    = textOutput("vbox_transit",        inline = TRUE),
          showcase = bs_icon("bus-front"),
          theme    = "info"
        ),
        bslib::value_box(
          title    = textOutput("vbox_fromhome_title", inline = TRUE),
          value    = textOutput("vbox_fromhome",       inline = TRUE),
          showcase = bs_icon("house"),
          theme    = "success"
        )
      ),
      bslib::card(
        bslib::card_header(textOutput("trend_chart_title", inline = TRUE)),
        plotlyOutput("plot_trend", height = "320px")
      ),
      bslib::card(
        bslib::card_header(textOutput("boxplot_title", inline = TRUE)),
        plotlyOutput("plot_boxplot", height = "320px")
      )
    )
  ),

  # ── Tab 2: County Explorer ────────────────────────────────────────────────
  bslib::nav_panel(
    "County Explorer",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,

        strong("Filter Counties"),
        selectInput(
          "tbl_year", "Year",
          choices  = rev(all_years),
          selected = latest_year
        ),
        selectizeInput(
          "tbl_division", "Census Division(s)",
          choices  = all_divisions,
          multiple = TRUE,
          options  = list(placeholder = "All divisions")
        ),
        selectizeInput(
          "tbl_state", "State(s)",
          choices  = all_states,
          multiple = TRUE,
          options  = list(placeholder = "All states")
        ),
        hr(),
        strong("Rank by Mode"),
        selectInput(
          "tbl_mode", label = NULL,
          choices = names(mode_cols)
        ),
        hr(),
        strong("Population Filter"),
        sliderInput(
          "tbl_pop", "Workers 16+ (thousands)",
          min   = 0,
          max   = 5000,
          value = c(0, 5000),
          step  = 50
        )
      ),

      bslib::layout_columns(
        col_widths = c(4, 4, 4),
        bslib::value_box(
          title    = "Counties Shown",
          value    = textOutput("tbl_n_counties", inline = TRUE),
          showcase = bs_icon("map"),
          theme    = "primary"
        ),
        bslib::value_box(
          title    = "Top County",
          value    = textOutput("tbl_top_county", inline = TRUE),
          showcase = bs_icon("trophy"),
          theme    = "success"
        ),
        bslib::value_box(
          title    = textOutput("tbl_avg_label", inline = TRUE),
          value    = textOutput("tbl_avg_value", inline = TRUE),
          showcase = bs_icon("bar-chart"),
          theme    = "info"
        )
      ),
      bslib::card(
        bslib::card_header(textOutput("tbl_title", inline = TRUE)),
        DTOutput("table_counties")
      )
    )
  ),

  # ── Tab 3: County Maps ────────────────────────────────────────────────────
  bslib::nav_panel(
    "County Maps",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        width = 280,

        strong("Map Settings"),
        selectInput(
          "map_mode", "Transportation Mode",
          choices  = names(mode_cols)[!names(mode_cols) %in% c("Taxi", "Motorcycle", "Other")],
          selected = "Walk"
        ),
        selectInput(
          "map_year", "Year",
          choices  = rev(all_years),
          selected = latest_year
        ),
        hr(),
        p(class = "text-muted small",
          "Hover over any county to see its mode share and geographic details. ",
          "Toggle division boundaries and basemap using the layers control. ",
          "Click the expand icon to view the map full screen.")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(textOutput("map_title", inline = TRUE)),
        leafletOutput("map_modes", height = "620px")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Update state choices when division selection changes ────────────────────
  observeEvent(input$trend_division, {
    if (length(input$trend_division) > 0) {
      states_in_div <- state_division_lookup %>%
        filter(division_name %in% input$trend_division) %>%
        pull(state_name)
      updateSelectizeInput(session, "trend_state",
                           choices  = states_in_div,
                           selected = character(0))
    } else {
      updateSelectizeInput(session, "trend_state",
                           choices  = all_states,
                           selected = character(0))
    }
  }, ignoreNULL = FALSE)

  observeEvent(input$tbl_division, {
    if (length(input$tbl_division) > 0) {
      states_in_div <- state_division_lookup %>%
        filter(division_name %in% input$tbl_division) %>%
        pull(state_name)
      updateSelectizeInput(session, "tbl_state",
                           choices  = states_in_div,
                           selected = character(0))
    } else {
      updateSelectizeInput(session, "tbl_state",
                           choices  = all_states,
                           selected = character(0))
    }
  }, ignoreNULL = FALSE)

  # ── Tab 1: filtered data for trend chart ────────────────────────────────────
  trend_filtered <- reactive({
    df <- modeshare_county_period_formatted
    if (length(input$trend_state) > 0) {
      df <- df %>% filter(state_name %in% input$trend_state)
    } else if (length(input$trend_division) > 0) {
      df <- df %>% filter(division_name %in% input$trend_division)
    }
    df
  })

  geo_label <- reactive({
    if (length(input$trend_state) > 0) {
      paste(input$trend_state, collapse = ", ")
    } else if (length(input$trend_division) > 0) {
      paste(input$trend_division, collapse = ", ")
    } else {
      "All US Counties"
    }
  })

  # ── Value boxes: reactive averages for latest year + selected geography ───────
  vbox_avgs <- reactive({
    trend_filtered() %>%
      filter(year == latest_year) %>%
      summarise(
        walk     = mean(pct_walk,     na.rm = TRUE),
        bicycle  = mean(pct_bicycle,  na.rm = TRUE),
        transit  = mean(pct_transit,  na.rm = TRUE),
        fromhome = mean(pct_fromhome, na.rm = TRUE)
      )
  })

  vbox_title_suffix <- reactive({
    paste0(geo_label(), " (", latest_year, ")")
  })

  output$vbox_walk_title     <- renderText({ paste("Avg Walk Mode Share —",          vbox_title_suffix()) })
  output$vbox_bicycle_title  <- renderText({ paste("Avg Bicycle Mode Share —",       vbox_title_suffix()) })
  output$vbox_transit_title  <- renderText({ paste("Avg Transit Mode Share —",       vbox_title_suffix()) })
  output$vbox_fromhome_title <- renderText({ paste("Avg Work from Home Mode Share —", vbox_title_suffix()) })

  output$vbox_walk     <- renderText({ paste0(round(vbox_avgs()$walk,     1), "%") })
  output$vbox_bicycle  <- renderText({ paste0(round(vbox_avgs()$bicycle,  1), "%") })
  output$vbox_transit  <- renderText({ paste0(round(vbox_avgs()$transit,  1), "%") })
  output$vbox_fromhome <- renderText({ paste0(round(vbox_avgs()$fromhome, 1), "%") })

  output$trend_chart_title <- renderText({
    paste("Average Commute Mode Share —", geo_label(), "(2010–2023)")
  })
  output$boxplot_title <- renderText({
    paste("Distribution of Commute Mode Share across US Counties —", geo_label())
  })

  output$plot_trend <- renderPlotly({
    req(length(input$trend_modes) > 0)

    cols   <- mode_cols[input$trend_modes]
    colors <- mode_colors[input$trend_modes]

    df <- trend_filtered() %>%
      group_by(year) %>%
      summarise(across(all_of(unname(cols)), \(x) mean(x, na.rm = TRUE)), .groups = "drop")

    p <- plot_ly()
    for (i in seq_along(cols)) {
      mode_name <- names(cols)[i]
      col_name  <- cols[i]
      p <- p %>% add_trace(
        data          = df,
        x             = ~year,
        y             = as.formula(paste0("~`", col_name, "`")),
        type          = "scatter",
        mode          = "lines+markers",
        name          = mode_name,
        line          = list(color = colors[i]),
        marker        = list(color = colors[i]),
        hovertemplate = paste0(mode_name, ": %{y:.1f}%<extra></extra>")
      )
    }
    p %>% layout(
      xaxis     = list(title = "Year", dtick = 1),
      yaxis     = list(title = "Commute Mode Share (%)"),
      legend    = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.18),
      hovermode = "x unified"
    )
  })

  output$plot_boxplot <- renderPlotly({
    req(length(input$trend_modes) > 0)

    df <- trend_filtered() %>%
      filter(year == max(year)) %>%
      select(county_name, state_name, division_name,
             all_of(unname(mode_cols[input$trend_modes]))) %>%
      pivot_longer(
        cols      = all_of(unname(mode_cols[input$trend_modes])),
        names_to  = "col",
        values_to = "percent"
      ) %>%
      mutate(
        mode    = names(mode_cols)[match(col, mode_cols)],
        percent = round(percent, 1),
        order   = match(mode, input$trend_modes)
      ) %>%
      drop_na(percent)

    plot_ly(
      df,
      x               = ~reorder(mode, order),
      y               = ~percent,
      color           = ~mode,
      colors          = mode_colors[input$trend_modes],
      type            = "box",
      jitter          = 0.4,
      boxpoints       = "suspectedoutliers",
      quartilemethod  = "inclusive",
      marker          = list(size = 2),
      showlegend      = FALSE,
      hovertemplate   = "%{x}: %{y:.1f}%<extra></extra>"
    ) %>%
      config(displaylogo = FALSE) %>%
      layout(
        xaxis = list(title = "Travel Mode"),
        yaxis = list(title = "Commute Mode Share (%)")
      )
  })

  # ── Tab 2: county explorer ──────────────────────────────────────────────────
  tbl_filtered <- reactive({
    col <- mode_cols[[input$tbl_mode]]
    df  <- modeshare_county_period_formatted %>%
      filter(year == as.integer(input$tbl_year))

    if (length(input$tbl_state) > 0) {
      df <- df %>% filter(state_name %in% input$tbl_state)
    } else if (length(input$tbl_division) > 0) {
      df <- df %>% filter(division_name %in% input$tbl_division)
    }

    pop_min <- input$tbl_pop[1] * 1000
    pop_max <- input$tbl_pop[2] * 1000
    df <- df %>% filter(workers16pl >= pop_min, workers16pl <= pop_max)

    df %>%
      select(county_name, state_name, division_name,
             total_population, workers16pl,
             all_of(unname(mode_cols))) %>%
      arrange(desc(.data[[col]]))
  })

  output$tbl_n_counties <- renderText({ comma(nrow(tbl_filtered())) })

  output$tbl_top_county <- renderText({
    df  <- tbl_filtered()
    col <- mode_cols[[input$tbl_mode]]
    if (nrow(df) == 0) return("—")
    top <- df %>% slice_max(.data[[col]], n = 1, with_ties = FALSE)
    paste0(top$county_name, ", ", top$state_name)
  })

  output$tbl_avg_label <- renderText({
    paste("Avg", input$tbl_mode, "(", input$tbl_year, ")")
  })
  output$tbl_avg_value <- renderText({
    col <- mode_cols[[input$tbl_mode]]
    paste0(round(mean(tbl_filtered()[[col]], na.rm = TRUE), 1), "%")
  })

  output$tbl_title <- renderText({
    paste("County Rankings by", input$tbl_mode, "Mode Share —", input$tbl_year)
  })

  output$table_counties <- renderDT({
    df  <- tbl_filtered()
    col <- mode_cols[[input$tbl_mode]]

    # Rename columns for display
    display_names <- c(
      "County", "State", "Division",
      "Total Population", "Workers 16+",
      names(mode_cols)
    )

    datatable(
      df,
      rownames  = FALSE,
      colnames  = display_names,
      class     = "cell-border stripe",
      extensions = "Buttons",
      options   = list(
        pageLength = 15,
        order      = list(list(which(names(df) == col) - 1L, "desc")),
        dom        = "Bfrtip",
        buttons    = list(list(extend = "csv", text = "Download CSV"))
      )
    ) %>%
      formatRound(c("total_population", "workers16pl"), digits = 0, mark = ",") %>%
      formatRound(unname(mode_cols), digits = 1) %>%
      formatStyle(
        col,
        background = styleColorBar(range(df[[col]], na.rm = TRUE), "#d9edf7"),
        backgroundSize    = "100% 90%",
        backgroundRepeat  = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ── Tab 3: county choropleth map ────────────────────────────────────────────
  map_sf <- reactive({
    col      <- mode_cols[[input$map_mode]]
    year_sel <- as.integer(input$map_year)

    df <- modeshare_county_period_formatted %>%
      filter(year == year_sel, !state_name %in% c("Alaska", "Hawaii"))

    county_geom %>%
      inner_join(df, by = "GEOID_county") %>%
      drop_na(division_name)
  })

  output$map_title <- renderText({
    paste(input$map_mode, "Mode Share by County —", input$map_year)
  })

  output$map_modes <- renderLeaflet({
    df      <- map_sf()
    col     <- mode_cols[[input$map_mode]]
    palette <- map_palettes[[input$map_mode]]
    bins    <- fx_create_bins(df, col)
    pal     <- colorBin(palette, domain = df[[col]], bins = bins, na.color = "transparent")

    labels <- sprintf(
      "<strong>%s Division</strong><br/>
       <strong>%s County, %s</strong><br/>
       Workers 16+: %s<br/>
       Walk: %.1f%%<br/>
       Bicycle: %.1f%%<br/>
       Transit: %.1f%%<br/>
       Drove Alone: %.1f%%<br/>
       Work from Home: %.1f%%",
      df$division_name,
      df$county_name, df$state_name,
      format(df$workers16pl, big.mark = ","),
      df$pct_walk,
      df$pct_bicycle,
      df$pct_transit,
      df$pct_drovealone,
      df$pct_fromhome
    ) %>% lapply(HTML)

    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       group = "Positron (minimal)") %>%
      addProviderTiles(providers$Esri.WorldImagery,
                       group = "World Imagery (satellite)") %>%
      addPolygons(
        fillColor   = pal(df[[col]]),
        label       = labels,
        weight      = 0.5,
        opacity     = 0.75,
        color       = "white",
        fillOpacity = 0.75,
        group       = "Counties"
      ) %>%
      addPolylines(
        data        = us_divisions_sub_geom,
        weight      = 2,
        opacity     = 1,
        fillOpacity = 0,
        color       = "black",
        group       = "Divisions"
      ) %>%
      addLegend(
        "bottomleft",
        pal      = pal,
        values   = df[[col]],
        title    = paste(input$map_mode, "Mode Share"),
        labFormat = labelFormat(between = "% &ndash; ", suffix = "%",
                                transform = \(x) round(x, 1)),
        opacity  = 1
      ) %>%
      addResetMapButton() %>%
      addLayersControl(
        baseGroups    = c("Positron (minimal)", "World Imagery (satellite)"),
        overlayGroups = c("Counties", "Divisions"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
}

shinyApp(ui, server)
