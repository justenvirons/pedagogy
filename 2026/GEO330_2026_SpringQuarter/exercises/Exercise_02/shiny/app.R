## app.R
## Divvy Bikeshare Explorer — GEO 330 Sustainable Urban Transportation
## C. Scott Smith, PhD AICP
## UI + Server only; shared data objects loaded from global.R

library(leaflet.extras)

# ── Shared helper: community area hover labels for Tab 2 map ──────────────────
ca_hover_labels <- function(df) {
  sprintf(
    "<strong>%s</strong><br/>
     <i>Total trips (from):</i> %s<br/>
     <i>Total trips (to):</i> %s<br/>
     <i>Avg daily trips (from):</i> %.1f<br/>
     <i>Avg daily trips (to):</i> %.1f<br/>
     <i>Daily trips per 1K pop (from):</i> %.2f<br/>
     <i>Daily trips per 1K pop (to):</i> %.2f",
    df$comarea_name,
    prettyNum(df$fr_trips_cumul, big.mark = ","),
    prettyNum(df$to_trips_cumul, big.mark = ","),
    df$fr_avg_daily,
    df$to_avg_daily,
    df$fr_daily_per1k,
    df$to_daily_per1k
  ) %>% lapply(HTML)
}

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title  = "Divvy Bikeshare Explorer",
  theme  = bs_theme(bootswatch = "flatly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Tab 1: Systemwide Trends ─────────────────────────────────────────────────
  nav_panel(
    "Systemwide Trends",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = "Total Trips (2013–2025)",
        value    = comma(total_trips),
        showcase = bs_icon("bicycle"),
        theme    = "primary"
      ),
      value_box(
        title    = "Average Daily Trips",
        value    = comma(avg_daily),
        showcase = bs_icon("calendar-day"),
        theme    = "info"
      ),
      value_box(
        title    = "Peak Single Day",
        value    = format(peak_day_row$date, "%b %d, %Y"),
        p(comma(peak_day_row$trips), "trips"),
        showcase = bs_icon("graph-up"),
        theme    = "success"
      )
    ),
    card(
      card_header("Cumulative Divvy Trips — June 2013 through December 2025"),
      plotlyOutput("plot_cumul", height = "280px")
    ),
    card(
      card_header("Monthly Divvy Trips by Year"),
      plotlyOutput("plot_monthly", height = "280px")
    ),
    card(
      card_header("Daily Divvy Trips"),
      plotlyOutput("plot_daily", height = "280px")
    )
  ),

  # ── Tab 2: Trips by Community Area ───────────────────────────────────────────
  nav_panel(
    "Trips by Community Area",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        strong("Map Metric"),
        selectInput(
          "tab2_metric", label = NULL,
          choices = c(
            "Total Trips (from)"             = "fr_trips_cumul",
            "Total Trips (to)"               = "to_trips_cumul",
            "Avg Daily Trips (from)"         = "fr_avg_daily",
            "Avg Daily Trips (to)"           = "to_avg_daily",
            "Daily Trips per 1K Pop (from)"  = "fr_daily_per1k",
            "Daily Trips per 1K Pop (to)"    = "to_daily_per1k"
          )
        ),
        hr(),
        p(class = "text-muted small",
          "Hover over a community area to see detailed trip statistics.",
          "Colors represent quintile (1 of 5 equal-count) groupings.")
      ),
      card(
        card_header("Divvy Trips by Community Area (Quintile Classification)"),
        leafletOutput("map_ca", height = "600px")
      )
    )
  ),

  # ── Tab 3: Equity Analysis ────────────────────────────────────────────────────
  nav_panel(
    "Equity Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 270,
        strong("Socioeconomic Indicator"),
        selectInput(
          "ses_var", label = NULL,
          choices  = ses_choices,
          selected = "poverty_2021"
        ),
        uiOutput("ses_description_ui"),
        hr(),
        p(class = "text-muted small",
          "Compare Divvy ridership patterns to community socioeconomic conditions ",
          "across Chicago's 77 community areas. Select an indicator to update all panels.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header(textOutput("ses_map_title", inline = TRUE)),
          leafletOutput("map_ses", height = "380px")
        ),
        card(
          full_screen = TRUE,
          card_header("Total Divvy Trips (from) by Community Area"),
          leafletOutput("map_trips", height = "380px")
        )
      ),
      card(
        card_header(textOutput("plot_ses_title", inline = TRUE)),
        plotlyOutput("plot_ses", height = "320px")
      ),
      card(
        card_header("Summary: Trips by Socioeconomic Quintile (2013–2025)"),
        DTOutput("table_ses")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Tab 1: static time-series charts ────────────────────────────────────────
  output$plot_cumul <- renderPlotly({
    plot_ly(
      trips_by_date, x = ~date, y = ~trips_cumul,
      type = "scatter", mode = "lines",
      line        = list(color = "steelblue"),
      fill        = "tozeroy",
      fillcolor   = "rgba(70,130,180,0.15)",
      hovertemplate = "Cumulative trips: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = "Cumulative Trips"),
        hovermode = "x unified"
      )
  })

  output$plot_monthly <- renderPlotly({
    plot_ly(
      trips_by_yearmonth, x = ~yearmonth, y = ~trips,
      type   = "bar",
      marker = list(color = "steelblue"),
      hovertemplate = "Monthly trips: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = "Monthly Trips"),
        hovermode = "x unified"
      )
  })

  output$plot_daily <- renderPlotly({
    plot_ly(
      trips_by_date, x = ~date, y = ~trips,
      type = "scatter", mode = "lines",
      line = list(color = "steelblue", width = 0.5),
      hovertemplate = "Daily trips: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = "Daily Trips"),
        hovermode = "x unified"
      )
  })

  # ── Tab 2: community area choropleth with proxy update ───────────────────────
  output$map_ca <- renderLeaflet({
    pal <- colorQuantile("RdYlBu",
                         domain  = trips_by_communityarea_ses$fr_trips_cumul,
                         n       = 5,
                         reverse = TRUE)
    leaflet(trips_by_communityarea_ses) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(fr_trips_cumul),
        label       = ca_hover_labels(trips_by_communityarea_ses),
        weight      = 2, opacity = 1, color = "white",
        dashArray   = "3", fillOpacity = 0.7,
        layerId     = ~comarea_id
      ) %>%
      addLegend("bottomleft", pal = pal, values = ~fr_trips_cumul,
                title   = metric_titles[["fr_trips_cumul"]],
                opacity = 1)
  })

  observeEvent(input$tab2_metric, {
    col  <- input$tab2_metric
    vals <- trips_by_communityarea_ses[[col]]
    pal  <- colorQuantile("RdYlBu", domain = vals, n = 5, reverse = TRUE)

    leafletProxy("map_ca", data = trips_by_communityarea_ses) %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(
        fillColor   = pal(vals),
        label       = ca_hover_labels(trips_by_communityarea_ses),
        weight      = 2, opacity = 1, color = "white",
        dashArray   = "3", fillOpacity = 0.7,
        layerId     = ~comarea_id
      ) %>%
      addLegend("bottomleft", pal = pal, values = vals,
                title   = metric_titles[[col]],
                opacity = 1)
  })

  # ── Tab 3: equity analysis ────────────────────────────────────────────────────

  # Display name for selected SES indicator
  ses_name <- reactive({
    names(ses_choices)[ses_choices == input$ses_var]
  })

  output$ses_description_ui <- renderUI({
    p(class = "small text-secondary", ses_descriptions[[ses_name()]])
  })

  output$ses_map_title  <- renderText({ ses_name() })
  output$plot_ses_title <- renderText({
    paste("Monthly Divvy Trips per 1,000 Residents by", ses_name(), "Quintile")
  })

  # SES choropleth — redraws when indicator changes (domain changes with it)
  output$map_ses <- renderLeaflet({
    df       <- trips_by_communityarea_ses
    df$ses_v <- df[[input$ses_var]]
    pal      <- colorQuantile("RdYlBu", domain = df$ses_v, n = 5, reverse = TRUE)
    labels   <- sprintf(
      "<strong>%s</strong><br/><i>%s:</i> %.1f",
      df$comarea_name, ses_name(), df$ses_v
    ) %>% lapply(HTML)
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(ses_v), label = labels,
        weight      = 2, opacity = 1, color = "white",
        dashArray   = "3", fillOpacity = 0.7
      ) %>%
      addLegend("bottomleft", pal = pal, values = ~ses_v,
                title   = paste0(ses_name(), "<br/>(Quintiles)"),
                opacity = 1)
  })

  # Trips choropleth — static data, rendered once
  output$map_trips <- renderLeaflet({
    df  <- trips_by_communityarea_ses
    pal <- colorQuantile("RdYlBu", domain = df$fr_trips_cumul, n = 5, reverse = TRUE)
    labels <- sprintf(
      "<strong>%s</strong><br/><i>Total trips (from):</i> %s",
      df$comarea_name, prettyNum(df$fr_trips_cumul, big.mark = ",")
    ) %>% lapply(HTML)
    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(fr_trips_cumul), label = labels,
        weight      = 2, opacity = 1, color = "white",
        dashArray   = "3", fillOpacity = 0.7
      ) %>%
      addLegend("bottomleft", pal = pal, values = ~fr_trips_cumul,
                title   = "Total Trips (from)<br/>(Quintiles)",
                opacity = 1)
  })

  # Pre-computed trend data lookup — O(1), no computation in reactive
  ses_trend <- reactive({
    ses_trends_list[[ses_name()]]
  })

  output$plot_ses <- renderPlotly({
    df <- ses_trend()
    plot_ly(
      df, x = ~yearmonth, y = ~fr_trips_per1k,
      type  = "scatter", mode = "lines+markers",
      color = ~ses_label, colors = quintile_colors,
      text  = ~ses_label,
      hovertemplate = "%{text}: %{y:.1f}<extra></extra>"
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = "Trips per 1,000 Residents"),
        legend    = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.18),
        hovermode = "x unified"
      )
  })

  output$table_ses <- renderDT({
    df <- ses_trend() %>%
      group_by(ses_label) %>%
      summarise(
        tot_pop  = max(tot_pop),
        fr_trips = sum(fr_trips),
        to_trips = sum(to_trips),
        .groups  = "drop"
      ) %>%
      mutate(
        pop_pct      = tot_pop  / sum(tot_pop)  * 100,
        fr_trips_pct = fr_trips / sum(fr_trips) * 100,
        to_trips_pct = to_trips / sum(to_trips) * 100
      ) %>%
      arrange(desc(ses_label))

    datatable(
      df,
      rownames = FALSE,
      colnames = c("SES Quintile", "Population", "Trips (from)", "Trips (to)",
                   "% Population", "% Trips (from)", "% Trips (to)"),
      class   = "cell-border stripe",
      options = list(pageLength = 5, dom = "t")
    ) %>%
      formatRound(c("tot_pop", "fr_trips", "to_trips"), digits = 0, mark = ",") %>%
      formatRound(c("pop_pct", "fr_trips_pct", "to_trips_pct"), digits = 1)
  })
}

shinyApp(ui, server)
