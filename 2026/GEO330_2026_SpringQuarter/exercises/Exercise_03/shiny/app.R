## app.R
## CTA L Ridership Dashboard — GEO/SUD 3/430 Exercise #3
## C. Scott Smith, PhD AICP
## UI + Server only; shared data objects loaded from global.R

library(leaflet.extras)

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title  = "CTA L Ridership Dashboard",
  theme  = bs_theme(bootswatch = "flatly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Tab 1: CCVI Map ──────────────────────────────────────────────────────────
  nav_panel(
    "CCVI Map",
    card(
      full_screen = TRUE,
      card_header("COVID-19 Community Vulnerability Index (CCVI) by Community Area"),
      leafletOutput("map_ccvi", height = "600px")
    ),
    p(class = "text-muted small mt-2",
      "The Chicago COVID-19 Community Vulnerability Index (CCVI) identifies neighborhoods ",
      "disproportionately impacted by COVID-19. Scores combine sociodemographic, ",
      "epidemiological, and occupational factors. Hover over community areas to view ",
      "CCVI scores and ranked sub-indicators.")
  ),

  # ── Tab 2: Ridership Trends ───────────────────────────────────────────────────
  nav_panel(
    "Ridership Trends",
    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = "2019 Annual Boardings (Pre-COVID)",
        value    = comma(rides_2019),
        showcase = bs_icon("train-front"),
        theme    = "primary"
      ),
      value_box(
        title    = paste0(latest_year_avail, " Annual Boardings"),
        value    = comma(rides_latest),
        showcase = bs_icon("calendar-check"),
        theme    = "info"
      ),
      value_box(
        title    = "Recovery Rate vs. 2019",
        value    = paste0(recovery_pct, "%"),
        showcase = bs_icon("graph-up-arrow"),
        theme    = if (recovery_pct >= 90) "success" else "warning"
      )
    ),
    card(
      card_header("Total Monthly CTA L Boardings — January 1999 through December 2024"),
      plotlyOutput("plot_monthly", height = "260px")
    ),
    card(
      card_header("Average Weekday CTA L Rail Boardings by Day"),
      plotlyOutput("plot_daily", height = "260px")
    ),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center w-100",
          span("COVID-19 Weekly Trend, Chicago"),
          div(style = "width: 260px;",
            selectInput(
              "covid_metric", label = NULL,
              choices  = c(
                "Weekly Cases (7-day avg)"  = "Cases7d",
                "Weekly Deaths (7-day avg)" = "Deaths7d"
              ),
              selected = "Deaths7d",
              width    = "100%"
            )
          )
        )
      ),
      plotlyOutput("plot_covid", height = "260px")
    )
  ),

  # ── Tab 3: Ridership by Category ─────────────────────────────────────────────
  nav_panel(
    "Ridership by Category",
    layout_sidebar(
      sidebar = sidebar(
        width = 230,
        strong("L Lines"),
        p(class = "text-muted small",
          "Select lines to display in the by-line chart.",
          "The CCVI chart always shows all three vulnerability categories."),
        checkboxGroupInput(
          "selected_lines",
          label    = NULL,
          choices  = all_lines,
          selected = all_lines
        )
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header("Monthly Boardings by CCVI Category"),
          plotlyOutput("plot_ccvi", height = "400px")
        ),
        card(
          full_screen = TRUE,
          card_header("Monthly Boardings by L Line"),
          plotlyOutput("plot_line", height = "400px")
        )
      )
    )
  ),

  # ── Tab 4: Station Change Map ─────────────────────────────────────────────────
  nav_panel(
    "Station Change Map",
    layout_sidebar(
      sidebar = sidebar(
        width = 260,
        strong("Comparison Periods"),
        selectInput(
          "baseline_month", "Baseline period:",
          choices  = all_month_choices,
          selected = "2019-11-01"
        ),
        selectInput(
          "comp_month", "Compare to:",
          choices  = all_month_choices,
          selected = tail(all_month_choices, 1)
        ),
        hr(),
        strong("L Lines"),
        checkboxGroupInput(
          "map_lines",
          label    = NULL,
          choices  = all_lines,
          selected = all_lines
        ),
        hr(),
        p(class = "text-muted small",
          "Circle size reflects the magnitude of ridership change. ",
          HTML("<span style='color:#2980b9;'>&#9679;</span> Blue = ridership gain &nbsp;"),
          HTML("<span style='color:#c0392b;'>&#9679;</span> Red = ridership loss."),
          br(), "Hover over stations for details.")
      ),
      tagList(
        # Stats row — computed reactively from selected inputs
        uiOutput("tab4_stats"),
        card(
          full_screen = TRUE,
          card_header(textOutput("map_change_title", inline = TRUE)),
          leafletOutput("map_change", height = "560px")
        )
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Tab 1: CCVI choropleth ────────────────────────────────────────────────────
  output$map_ccvi <- renderLeaflet({
    pal <- colorFactor(
      palette = unname(pal_ccvi[c("LOW", "MEDIUM", "HIGH")]),
      levels  = c("LOW", "MEDIUM", "HIGH"),
      ordered = TRUE
    )

    fmt <- function(x) ifelse(is.na(x), "N/A", sprintf("%.0f", x))

    labels <- sprintf(
      "<strong>%s</strong><br/>
       <i>CCVI Score:</i> %s<br/>
       <i>CCVI Category:</i> %s<br/><br/>
       <em>Ranked scores (1 = least, 77 = most vulnerable):</em>
       <ul style=\"list-style-type:square; margin:4px 0 0 0; padding-left:18px;\">
         <li>Socioeconomic Status: %s</li>
         <li>Essential Workers: %s</li>
         <li>Mobility Ratio: %s</li>
         <li>COVID-19 Incidence Rate: %s</li>
         <li>COVID-19 Hospitalization Rate: %s</li>
         <li>COVID-19 Crude Mortality Rate: %s</li>
       </ul>",
      community_areas_ccvi$comarea_name,
      ifelse(is.na(community_areas_ccvi$ccvi_score), "N/A",
             sprintf("%.1f", community_areas_ccvi$ccvi_score)),
      as.character(community_areas_ccvi$ccvi_cat),
      fmt(community_areas_ccvi$rank_ses),
      fmt(community_areas_ccvi$rank_essen),
      fmt(community_areas_ccvi$rank_mobrat),
      fmt(community_areas_ccvi$rank_covinc),
      fmt(community_areas_ccvi$rank_covhsp),
      fmt(community_areas_ccvi$rank_covmor)
    ) %>% lapply(HTML)

    leaflet(community_areas_ccvi) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(ccvi_cat),
        label       = labels,
        weight      = 2, opacity = 1, color = "white",
        dashArray   = "3", fillOpacity = 0.7
      ) %>%
      addLegend("bottomleft",
                pal     = pal,
                values  = ~ccvi_cat,
                title   = "CCVI Category",
                opacity = 1)
  })

  # ── Tab 2: Ridership trend charts ─────────────────────────────────────────────
  output$plot_monthly <- renderPlotly({
    plot_ly(
      ridership_by_month, x = ~year_month, y = ~rides_total,
      type = "scatter", mode = "lines",
      line = list(color = "#080967", width = 0.75),
      hovertemplate = "Total boardings: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = "Total Monthly Boardings"),
        hovermode = "x unified"
      )
  })

  output$plot_daily <- renderPlotly({
    df <- ridership_daily %>% filter(day_type == "W")
    plot_ly(
      df, x = ~service_date, y = ~rail_boardings,
      type = "scatter", mode = "lines",
      line = list(color = "#9F1928", width = 0.5),
      hovertemplate = "Rail boardings: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = "Avg Weekday Rail Boardings"),
        hovermode = "x unified"
      )
  })

  output$plot_covid <- renderPlotly({
    col   <- input$covid_metric
    label <- if (col == "Cases7d") "Cases (7-day avg)" else "Deaths (7-day avg)"
    plot_ly(
      covid19_cases, x = ~Date, y = covid19_cases[[col]],
      type = "scatter", mode = "lines",
      line = list(color = "#9F1928", width = 0.75),
      hovertemplate = paste0(label, ": %{y:,.1f}<extra></extra>")
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = label),
        hovermode = "x unified"
      )
  })

  # ── Tab 3: Ridership by CCVI category and by line ─────────────────────────────
  output$plot_ccvi <- renderPlotly({
    plot_ly(
      ridership_by_ccvi,
      x             = ~year_month,
      y             = ~rides_total,
      color         = ~ccvi_cat,
      colors        = pal_ccvi,
      type          = "scatter",
      mode          = "lines",
      text          = ~as.character(ccvi_cat),
      hovertemplate = "%{text}: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = "Total Monthly Boardings"),
        legend    = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.18),
        hovermode = "x unified"
      )
  })

  output$plot_line <- renderPlotly({
    req(length(input$selected_lines) > 0)
    df <- ridership_by_line %>% filter(station_line %in% input$selected_lines)
    validate(need(nrow(df) > 0, "No data for selected lines."))

    active_lines <- sort(unique(df$station_line))
    plot_ly(
      df,
      x             = ~year_month,
      y             = ~rides_total,
      color         = ~station_line,
      colors        = pal_line[active_lines],
      type          = "scatter",
      mode          = "lines",
      text          = ~station_line,
      hovertemplate = "%{text}: %{y:,}<extra></extra>"
    ) %>%
      layout(
        xaxis     = list(title = ""),
        yaxis     = list(title = "Total Monthly Boardings"),
        legend    = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.18),
        hovermode = "x unified"
      )
  })

  # ── Tab 4: Station change map ─────────────────────────────────────────────────
  comp_data <- reactive({
    req(input$baseline_month, input$comp_month, length(input$map_lines) > 0)
    validate(need(
      input$baseline_month != input$comp_month,
      "Baseline and comparison periods must be different."
    ))

    base_ym <- as.Date(input$baseline_month)
    comp_ym <- as.Date(input$comp_month)

    baseline   <- station_ridership_weekday %>%
      filter(year_month == base_ym) %>%
      select(station_id, rides_baseline = rides_total)

    comparison <- station_ridership_weekday %>%
      filter(year_month == comp_ym) %>%
      select(station_id, rides_comp = rides_total)

    change_df <- baseline %>%
      inner_join(comparison, by = "station_id") %>%
      mutate(
        net_chg = rides_comp - rides_baseline,
        pct_chg = (rides_comp - rides_baseline) / rides_baseline * 100,
        marker_radius = {
          r   <- abs(pct_chg)
          rng <- range(r, na.rm = TRUE)
          if (is.na(diff(rng)) || diff(rng) == 0) rep(5, n())
          else rescale(r, to = c(3, 12), from = rng)
        }
      )

    rail_stations %>%
      select(station_id, station_name, station_line, comarea_name, ccvi_cat) %>%
      inner_join(change_df, by = "station_id") %>%
      filter(station_line %in% input$map_lines)
  })

  # Stats row above map
  output$tab4_stats <- renderUI({
    df <- comp_data()

    base_label <- names(all_month_choices)[all_month_choices == input$baseline_month]
    comp_label <- names(all_month_choices)[all_month_choices == input$comp_month]

    total_base <- sum(df$rides_baseline, na.rm = TRUE)
    total_comp <- sum(df$rides_comp,     na.rm = TRUE)
    total_net  <- total_comp - total_base
    total_pct  <- if (total_base > 0) round(total_net / total_base * 100, 1) else NA

    net_theme  <- if (!is.na(total_net) && total_net >= 0) "success" else "danger"
    pct_label  <- if (!is.na(total_pct)) paste0(total_pct, "%") else "N/A"
    net_label  <- paste0(if (total_net >= 0) "+" else "", comma(total_net))

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = paste("Baseline Boardings —", base_label),
        value    = comma(total_base),
        showcase = bs_icon("train-front"),
        theme    = "primary"
      ),
      value_box(
        title    = paste("Comparison Boardings —", comp_label),
        value    = comma(total_comp),
        showcase = bs_icon("calendar-check"),
        theme    = "info"
      ),
      value_box(
        title    = "Net Change (Boardings / %)",
        value    = paste0(net_label, " / ", pct_label),
        showcase = bs_icon(if (total_net >= 0) "graph-up-arrow" else "graph-down-arrow"),
        theme    = net_theme
      )
    )
  })

  output$map_change_title <- renderText({
    base_label <- names(all_month_choices)[all_month_choices == input$baseline_month]
    comp_label <- names(all_month_choices)[all_month_choices == input$comp_month]
    paste("CTA L Station Ridership Change:", base_label, "vs.", comp_label)
  })

  output$map_change <- renderLeaflet({
    df       <- comp_data()
    lines_sf <- rail_lines %>% filter(line_label %in% input$map_lines)
    validate(need(nrow(df) > 0, "No station data for selected filters."))

    base_label <- names(all_month_choices)[all_month_choices == input$baseline_month]
    comp_label <- names(all_month_choices)[all_month_choices == input$comp_month]

    labels <- sprintf(
      "<strong>%s Station</strong><br/>
       <strong>%s Line</strong><br/>
       <i>Community Area:</i> %s<br/>
       <i>CCVI Category:</i> %s<br/>
       %s boardings: %s<br/>
       %s boardings: %s<br/>
       Net change: %s<br/>
       Percent change: %.1f%%",
      df$station_name,
      df$station_line,
      df$comarea_name,
      as.character(df$ccvi_cat),
      base_label, format(df$rides_baseline, big.mark = ","),
      comp_label, format(df$rides_comp,     big.mark = ","),
      format(df$net_chg, big.mark = ","),
      df$pct_chg
    ) %>% lapply(HTML)

    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,  group = "Positron (minimal)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
      addPolylines(
        data    = lines_sf,
        color   = ~display_color,
        weight  = 3,
        opacity = 0.85,
        group   = "Rail Lines"
      ) %>%
      addCircleMarkers(
        data        = df,
        radius      = ~marker_radius,
        color       = ~if_else(pct_chg < 0, "#c0392b", "#2980b9"),
        fillColor   = ~if_else(pct_chg < 0, "#c0392b", "#2980b9"),
        fillOpacity = 0.65,
        weight      = 1.5,
        opacity     = 1,
        label       = labels,
        group       = "Stations"
      ) %>%
      addResetMapButton() %>%
      addLayersControl(
        baseGroups    = c("Positron (minimal)", "World Imagery"),
        overlayGroups = c("Rail Lines", "Stations"),
        options       = layersControlOptions(collapsed = FALSE)
      )
  })
}

shinyApp(ui, server)
