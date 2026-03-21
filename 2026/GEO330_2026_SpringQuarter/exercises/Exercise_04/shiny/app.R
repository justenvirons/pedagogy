## app.R
## Transportation & Public Health Dashboard — GEO/SUD 3/430 Exercise #4
## C. Scott Smith, PhD AICP
## UI + Server only; shared data objects loaded from global.R

# ── UI ────────────────────────────────────────────────────────────────────────
ui <- page_navbar(
  title  = "Transportation & Public Health Dashboard",
  theme  = bs_theme(bootswatch = "flatly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Tab 1: Cook County Geography ─────────────────────────────────────────────
  nav_panel(
    "Cook County Geography",
    p(class = "text-muted small mt-2",
      "This map displays the novel Cook County geography used in this exercise: 77 Chicago ",
      "community areas and suburban Cook County municipalities with at least 4 square miles of ",
      "area (N\u2248125). Together these form a combined geography of N\u2248202 units for ",
      "comparing transportation access and public health outcomes across the region. Hover over ",
      "any geography to view its name, geographic ID, type, and location."),
    layout_columns(
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header("Cook County Novel Geography: Chicago Community Areas & Suburban Municipalities"),
        leafletOutput("map_geo", height = "580px")
      ),
      card(
        full_screen = TRUE,
        card_header("Cook County Geographies"),
        DTOutput("table_geo")
      )
    )
  ),

  # ── Tab 2: Indicator Explorer ─────────────────────────────────────────────────
  nav_panel(
    "Indicator Explorer",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        strong("Health Indicator"),
        selectInput(
          "ind_select",
          label    = NULL,
          choices  = ind_choices,
          selected = "transit"
        ),
        hr(),
        strong("Jurisdiction"),
        radioButtons(
          "ind_juris",
          label    = NULL,
          choices  = juris_choices,
          selected = "all"
        ),
        hr(),
        p(class = "text-muted small",
          "Indicators are drawn from the ",
          tags$a("Chicago Health Atlas", href = "https://chicagohealthatlas.org",
                 target = "_blank"),
          " (2021\u20132022) and the ",
          tags$a("Cook County Health Atlas", href = "https://cookcountyhealthatlas.org",
                 target = "_blank"),
          " (2022). Values represent the estimated rate (%) for each geography. ",
          "The map uses quintile classification. Use the jurisdiction filter to ",
          "compare Chicago community areas and suburban municipalities separately.")
      ),
      tagList(
        uiOutput("ind_stats"),
        layout_columns(
          col_widths = c(7, 5),
          card(
            full_screen = TRUE,
            card_header(textOutput("ind_map_title", inline = TRUE)),
            leafletOutput("map_ind", height = "500px")
          ),
          card(
            full_screen = TRUE,
            card_header("Indicator Values by Geography"),
            DTOutput("table_ind")
          )
        )
      )
    )
  ),

  # ── Tab 3: Correlation Analysis ───────────────────────────────────────────────
  nav_panel(
    "Correlation Analysis",
    layout_sidebar(
      sidebar = sidebar(
        width = 230,
        strong("X-Axis Indicator"),
        selectInput(
          "corr_x",
          label    = NULL,
          choices  = ind_choices,
          selected = "transit"
        ),
        hr(),
        strong("Y-Axis Indicator"),
        selectInput(
          "corr_y",
          label    = NULL,
          choices  = ind_choices,
          selected = "health"
        ),
        hr(),
        strong("Jurisdiction"),
        radioButtons(
          "corr_juris",
          label    = NULL,
          choices  = juris_choices,
          selected = "all"
        ),
        hr(),
        p(class = "text-muted small",
          tags$strong("Pearson r"), " measures the linear correlation between two ",
          "indicators across geographies. Values range from \u22121 (perfect negative) ",
          "to +1 (perfect positive). The OLS regression line shows the direction and ",
          "slope of the relationship. The full correlation matrix (right) shows pairwise ",
          "Pearson r values for all seven indicators.")
      ),
      layout_columns(
        col_widths = c(6, 6),
        card(
          full_screen = TRUE,
          card_header(textOutput("scatter_title", inline = TRUE)),
          plotlyOutput("plot_scatter", height = "380px")
        ),
        card(
          full_screen = TRUE,
          card_header("Correlation Matrix — All Indicators"),
          plotlyOutput("plot_corr", height = "380px")
        )
      )
    )
  ),

  # ── Tab 4: Bivariate Map ──────────────────────────────────────────────────────
  nav_panel(
    "Bivariate Map",
    layout_sidebar(
      sidebar = sidebar(
        width = 250,
        strong("X-Axis Indicator"),
        selectInput(
          "biv_x",
          label    = NULL,
          choices  = ind_choices,
          selected = "transit"
        ),
        hr(),
        strong("Y-Axis Indicator"),
        selectInput(
          "biv_y",
          label    = NULL,
          choices  = ind_choices,
          selected = "health"
        ),
        hr(),
        strong("Bivariate Legend"),
        uiOutput("biv_legend"),
        hr(),
        p(class = "text-muted small",
          "The bivariate map classifies each geography into one of nine categories ",
          "based on tertiles (3 equal-count groups) of each indicator. Darker blue ",
          "indicates high values on both indicators; pink/magenta indicates high Y ",
          "and low X; light grey indicates low values on both. All Cook County ",
          "geographies are shown regardless of jurisdiction.")
      ),
      card(
        full_screen = TRUE,
        card_header(textOutput("biv_map_title", inline = TRUE)),
        leafletOutput("map_biv", height = "580px")
      )
    )
  )
)

# ── Server ────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  # ── Reactive: filter indicators_geo by jurisdiction ───────────────────────────
  filtered_ind <- reactive({
    req(input$ind_juris)
    df <- indicators_geo
    if (input$ind_juris != "all") {
      df <- df %>% filter(location == input$ind_juris)
    }
    df
  })

  filtered_corr <- reactive({
    req(input$corr_juris)
    df <- indicators_geo
    if (input$corr_juris != "all") {
      df <- df %>% filter(location == input$corr_juris)
    }
    df
  })

  # ── Tab 1: Cook County Geography map ─────────────────────────────────────────
  output$map_geo <- renderLeaflet({
    df <- novel_geo_cook

    pal <- colorFactor(
      palette = c("community area" = "#8a9db5", "municipality" = "#4a5568"),
      domain  = df$type
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>
       <i>GeoID:</i> %s<br/>
       <i>Type:</i> %s<br/>
       <i>Location:</i> %s",
      df$name, df$geoid, df$type, df$location
    ) %>% lapply(HTML)

    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(type),
        fillOpacity = 0.6,
        color       = "white",
        weight      = 1,
        opacity     = 1,
        label       = labels,
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "4px 8px"),
          textsize  = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        "bottomleft",
        pal     = pal,
        values  = ~type,
        title   = "Geography Type",
        opacity = 0.85
      ) %>%
      addEasyButton(easyButton(
        icon    = "fa-home",
        title   = "Reset to full extent",
        onClick = reset_map_js
      ))
  })

  output$table_geo <- renderDT({
    df <- novel_geo_cook %>%
      st_drop_geometry() %>%
      select(Name = name, `GeoID` = geoid, Type = type, Location = location) %>%
      arrange(Location, Name)

    datatable(
      df,
      rownames  = FALSE,
      class     = "table table-striped table-hover table-sm",
      options   = list(
        pageLength = 20,
        dom        = "frtip",
        scrollX    = TRUE
      )
    )
  })

  # ── Tab 2: Indicator Explorer — value boxes ────────────────────────────────────
  output$ind_map_title <- renderText({
    req(input$ind_select)
    paste(ind_label(input$ind_select), "— Cook County Geographies (Quintile Classification)")
  })

  output$ind_stats <- renderUI({
    req(input$ind_select)
    col <- input$ind_select

    df_all <- indicators_geo %>% st_drop_geometry()
    df_chi <- df_all %>% filter(location == "Chicago, IL")
    df_scc <- df_all %>% filter(location == "Suburban Cook County")

    avg_all <- round(mean(df_all[[col]], na.rm = TRUE), 1)
    avg_chi <- round(mean(df_chi[[col]], na.rm = TRUE), 1)
    avg_scc <- round(mean(df_scc[[col]], na.rm = TRUE), 1)

    lbl <- ind_label(col)

    layout_columns(
      col_widths = c(4, 4, 4),
      value_box(
        title    = paste("Cook County Avg —", lbl),
        value    = paste0(avg_all, "%"),
        showcase = bs_icon("bar-chart-fill"),
        theme    = "primary"
      ),
      value_box(
        title    = paste("Chicago Avg —", lbl),
        value    = paste0(avg_chi, "%"),
        showcase = bs_icon("buildings"),
        theme    = "info"
      ),
      value_box(
        title    = paste("Suburban Cook Avg —", lbl),
        value    = paste0(avg_scc, "%"),
        showcase = bs_icon("house-fill"),
        theme    = "secondary"
      )
    )
  })

  # ── Tab 2: Indicator choropleth — initial render ───────────────────────────────
  output$map_ind <- renderLeaflet({
    req(input$ind_select)
    df  <- filtered_ind()
    col <- input$ind_select
    validate(need(nrow(df) > 0, "No data available for selected filters."))
    validate(need(col %in% names(df), "Indicator not found in data."))

    vals <- df[[col]]
    pal  <- fx_quintile_pal(vals)

    labels <- sprintf(
      "<strong>%s</strong><br/>
       <i>Location:</i> %s<br/>
       <i>%s:</i> %.1f%%",
      df$name, df$location, ind_label(col), vals
    ) %>% lapply(HTML)

    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~pal(vals),
        fillOpacity = 0.75,
        color       = "white",
        weight      = 1,
        opacity     = 1,
        label       = labels,
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "4px 8px"),
          textsize  = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        "bottomleft",
        pal     = pal,
        values  = vals,
        title   = paste0(ind_label(col), "<br/>(Quintiles)"),
        labFormat = labelFormat(suffix = "%"),
        opacity = 0.85,
        layerId = "ind_legend"
      ) %>%
      addEasyButton(easyButton(
        icon    = "fa-home",
        title   = "Reset to full extent",
        onClick = reset_map_js
      ))
  })

  # ── Tab 2: Indicator choropleth — reactive updates via leafletProxy ────────────
  observeEvent(list(input$ind_select, input$ind_juris), {
    req(input$ind_select)
    df  <- filtered_ind()
    col <- input$ind_select
    if (nrow(df) == 0 || !(col %in% names(df))) return()

    vals <- df[[col]]
    pal  <- fx_quintile_pal(vals)

    labels <- sprintf(
      "<strong>%s</strong><br/>
       <i>Location:</i> %s<br/>
       <i>%s:</i> %.1f%%",
      df$name, df$location, ind_label(col), vals
    ) %>% lapply(HTML)

    proxy <- leafletProxy("map_ind", data = df)
    proxy %>%
      clearShapes() %>%
      removeControl("ind_legend") %>%
      addPolygons(
        fillColor   = ~pal(vals),
        fillOpacity = 0.75,
        color       = "white",
        weight      = 1,
        opacity     = 1,
        label       = labels,
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "4px 8px"),
          textsize  = "13px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        "bottomleft",
        pal     = pal,
        values  = vals,
        title   = paste0(ind_label(col), "<br/>(Quintiles)"),
        labFormat = labelFormat(suffix = "%"),
        opacity = 0.85,
        layerId = "ind_legend"
      )
  }, ignoreInit = TRUE)

  # ── Tab 2: Indicator table ─────────────────────────────────────────────────────
  output$table_ind <- renderDT({
    req(input$ind_select)
    df  <- filtered_ind() %>% st_drop_geometry()
    col <- input$ind_select
    validate(need(nrow(df) > 0, "No data available."))
    validate(need(col %in% names(df), "Indicator not found in data."))

    tbl <- df %>%
      select(Name = name, Location = location, Value = !!sym(col)) %>%
      filter(!is.na(Value)) %>%
      mutate(Quintile = ntile(Value, 5)) %>%
      arrange(desc(Value))

    datatable(
      tbl,
      rownames  = FALSE,
      class     = "table table-striped table-hover table-sm",
      options   = list(
        pageLength = 20,
        dom        = "frtip",
        scrollX    = TRUE
      )
    ) %>%
      formatRound("Value", digits = 1)
  })

  # ── Tab 3: Scatter plot title ─────────────────────────────────────────────────
  output$scatter_title <- renderText({
    req(input$corr_x, input$corr_y)
    paste(ind_label(input$corr_x), "vs.", ind_label(input$corr_y))
  })

  # ── Tab 3: Scatter plot ───────────────────────────────────────────────────────
  output$plot_scatter <- renderPlotly({
    req(input$corr_x, input$corr_y)
    df  <- filtered_corr() %>% st_drop_geometry()
    xcol <- input$corr_x
    ycol <- input$corr_y

    validate(need(xcol %in% names(df), "X indicator not found."))
    validate(need(ycol %in% names(df), "Y indicator not found."))
    validate(need(xcol != ycol, "Please select two different indicators."))

    df_plot <- df %>%
      select(name, location, x = !!sym(xcol), y = !!sym(ycol)) %>%
      filter(!is.na(x), !is.na(y))

    validate(need(nrow(df_plot) >= 3, "Not enough data to compute correlation."))

    r <- cor(df_plot$x, df_plot$y, use = "complete.obs")
    n <- nrow(df_plot)

    col_map <- c("Chicago, IL" = "#555555", "Suburban Cook County" = "#aaaaaa")

    # OLS regression
    fit        <- lm(y ~ x, data = df_plot)
    x_sorted   <- sort(df_plot$x)
    y_fitted   <- predict(fit, newdata = data.frame(x = x_sorted))

    p <- plot_ly() %>%
      add_markers(
        data        = df_plot,
        x           = ~x,
        y           = ~y,
        color       = ~location,
        colors      = col_map,
        text        = ~paste0(
          "<b>", name, "</b><br>",
          "Location: ", location, "<br>",
          ind_label(xcol), ": ", round(x, 1), "%<br>",
          ind_label(ycol), ": ", round(y, 1), "%"
        ),
        hovertemplate = "%{text}<extra></extra>",
        marker      = list(size = 8, opacity = 0.75)
      ) %>%
      add_lines(
        x    = x_sorted,
        y    = y_fitted,
        name = "OLS Fit",
        line = list(color = "black", dash = "dash", width = 1.5),
        hoverinfo = "skip",
        showlegend = TRUE,
        inherit = FALSE
      ) %>%
      layout(
        xaxis       = list(title = paste0(ind_label(xcol), " (%)")),
        yaxis       = list(title = paste0(ind_label(ycol), " (%)")),
        hovermode   = "closest",
        legend      = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.2),
        annotations = list(
          list(
            x         = 0.02,
            y         = 0.98,
            xref      = "paper",
            yref      = "paper",
            text      = paste0("r = ", round(r, 2), "  (n=", n, ")"),
            showarrow = FALSE,
            font      = list(size = 13, color = "#333333"),
            align     = "left",
            bgcolor   = "rgba(255,255,255,0.7)",
            borderpad = 4
          )
        )
      )

    p
  })

  # ── Tab 3: Correlation heatmap ─────────────────────────────────────────────────
  output$plot_corr <- renderPlotly({
    req(input$corr_juris)
    df <- filtered_corr() %>%
      st_drop_geometry() %>%
      select(all_of(indicators$name)) %>%
      filter(if_any(everything(), ~ !is.na(.)))

    validate(need(nrow(df) >= 3, "Not enough data to compute correlation matrix."))

    cor_mat  <- cor(df, use = "pairwise.complete.obs")
    col_labs <- indicators$label

    n_labs    <- length(col_labs)
    hover_mat <- matrix("", nrow = n_labs, ncol = n_labs)
    for (i in seq_len(n_labs)) {
      for (j in seq_len(n_labs)) {
        hover_mat[i, j] <- paste0(
          col_labs[i], "<br>",
          col_labs[j], "<br>",
          "r = ", round(cor_mat[i, j], 2)
        )
      }
    }

    plot_ly(
      type         = "heatmap",
      z            = cor_mat,
      x            = col_labs,
      y            = col_labs,
      colorscale   = list(list(0, "#1a1a1a"), list(0.5, "#f0f0f0"), list(1, "#1a1a1a")),
      zmin         = -1,
      zmax         = 1,
      text         = hover_mat,
      hoverinfo    = "text",
      texttemplate = "%{z:.2f}",
      showscale    = TRUE
    ) %>%
      layout(
        yaxis  = list(autorange = "reversed"),
        xaxis  = list(tickangle = -35),
        margin = list(l = 160, b = 140, t = 20, r = 20)
      )
  })

  # ── Tab 4: Bivariate legend ───────────────────────────────────────────────────
  output$biv_legend <- renderUI({
    req(input$biv_x, input$biv_y)

    cell_size  <- 36
    border_css <- "1px solid #ccc"

    # 3 rows (Y: high → low = class 3, 2, 1) × 3 cols (X: low → high = class 1, 2, 3)
    rows <- lapply(3:1, function(yc) {
      cells <- lapply(1:3, function(xc) {
        key <- paste0(xc, "-", yc)
        col <- if (!is.na(biv_pal[key])) biv_pal[key] else "#cccccc"
        tags$td(
          style = paste0(
            "width:", cell_size, "px;",
            "height:", cell_size, "px;",
            "background-color:", col, ";",
            "border:", border_css, ";",
            "padding:0;"
          )
        )
      })
      tags$tr(cells)
    })

    # X-axis labels below
    x_lbl_row <- tags$tr(
      lapply(c("Low", "Mid", "High"), function(l) {
        tags$td(
          style = paste0(
            "width:", cell_size, "px;",
            "text-align:center;",
            "font-size:10px;",
            "color:#555;",
            "padding-top:2px;"
          ),
          l
        )
      })
    )

    grid_table <- tags$table(
      style = "border-collapse:collapse;",
      tags$tbody(c(rows, list(x_lbl_row)))
    )

    # Wrap with Y-axis label on left, X-axis label below
    y_label <- tags$div(
      style = paste0(
        "writing-mode:vertical-rl;",
        "transform:rotate(180deg);",
        "font-size:10px;",
        "color:#555;",
        "text-align:center;",
        "height:", cell_size * 3, "px;",
        "line-height:1.2;",
        "margin-right:4px;"
      ),
      paste0(ind_label(input$biv_y), " \u2192 High")
    )

    x_label <- tags$div(
      style = "font-size:10px;color:#555;text-align:center;margin-top:2px;",
      paste0(ind_label(input$biv_x), " \u2192 High")
    )

    tagList(
      tags$div(
        style = "display:flex;align-items:flex-start;",
        y_label,
        grid_table
      ),
      x_label
    )
  })

  # ── Tab 4: Bivariate map title ────────────────────────────────────────────────
  output$biv_map_title <- renderText({
    req(input$biv_x, input$biv_y)
    paste0(
      "Bivariate Map: ", ind_label(input$biv_x),
      " \u00d7 ", ind_label(input$biv_y),
      " — Cook County"
    )
  })

  # ── Tab 4: Bivariate map ──────────────────────────────────────────────────────
  output$map_biv <- renderLeaflet({
    req(input$biv_x, input$biv_y)
    xcol <- input$biv_x
    ycol <- input$biv_y

    df <- indicators_geo
    validate(need(xcol %in% names(df), "X indicator not found in data."))
    validate(need(ycol %in% names(df), "Y indicator not found in data."))
    validate(need(xcol != ycol, "Please select two different indicators."))

    x_vals <- df[[xcol]]
    y_vals <- df[[ycol]]

    df$fill_color <- fx_biv_color(x_vals, y_vals)

    x_class <- ntile(x_vals, 3)
    y_class <- ntile(y_vals, 3)
    x_label_map <- c("1" = "Low", "2" = "Mid", "3" = "High")
    y_label_map <- c("1" = "Low", "2" = "Mid", "3" = "High")

    biv_class_label <- paste0(
      "X: ", x_label_map[as.character(x_class)],
      " / Y: ", y_label_map[as.character(y_class)]
    )

    labels <- sprintf(
      "<strong>%s</strong><br/>
       <i>Location:</i> %s<br/>
       <i>%s:</i> %.1f%%<br/>
       <i>%s:</i> %.1f%%<br/>
       <i>Bivariate Class:</i> %s",
      df$name,
      df$location,
      ind_label(xcol), round(x_vals, 1),
      ind_label(ycol), round(y_vals, 1),
      biv_class_label
    ) %>% lapply(HTML)

    leaflet(df) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor   = ~fill_color,
        fillOpacity = 0.8,
        color       = "white",
        weight      = 1,
        opacity     = 1,
        label       = labels,
        labelOptions = labelOptions(
          style     = list("font-weight" = "normal", padding = "4px 8px"),
          textsize  = "13px",
          direction = "auto"
        )
      ) %>%
      addEasyButton(easyButton(
        icon    = "fa-home",
        title   = "Reset to full extent",
        onClick = reset_map_js
      ))
  })
}

shinyApp(ui, server)
