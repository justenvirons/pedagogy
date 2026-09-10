# ==============================================================================
# FARS Stacked Area Chart: Fatalities by Year and Collision Type
# ==============================================================================
# Groups collisions into categories using the FARS "First Harmful Event"
# (HARM_EV) variable from the ACCIDENT file.
#
# IMPORTANT: To qualify for inclusion in FARS at all, a crash must involve a
# motor vehicle traveling on a public trafficway (that's the FARS eligibility
# criterion). This means EVERY crash in the dataset already involves a motor
# vehicle -- including rollovers, fixed-object strikes, animal strikes, and
# other "non-collision" events. HARM_EV just tells you what was struck, not
# whether a vehicle was involved.
#
# So here, only Pedestrian and Pedalcyclist are broken out as their own
# categories (motor vehicle strikes a person/cyclist); everything else is
# already a motor-vehicle crash by definition and rolls into "Motor Vehicle."
# "Other" is reserved for genuinely unknown/missing HARM_EV values.
#
#   - Pedalcyclist    (HARM_EV == 9)
#   - Pedestrian      (HARM_EV == 8)
#   - Motor Vehicle   (everything else with a valid HARM_EV code)
#   - Other           (missing / unknown HARM_EV)
#
# Adjust the `classify_collision()` function below if your FARS extract uses
# different codes/years, or if you'd rather classify off of a different
# field (e.g., a pre-existing "PBTYPE" or manner-of-collision variable).
# ==============================================================================

library(dplyr)
library(tidyr)
library(plotly)

# ------------------------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------------------------
# Point this at your FARS accident-level file(s). If you have multiple years
# saved separately (e.g., accident_2018.csv ... accident_2022.csv), read and
# row-bind them first so you end up with one data frame containing YEAR and
# HARM_EV columns.
#
# Example for a single combined file:
fars_data <- read.csv("fars_accident_data.csv", stringsAsFactors = FALSE)

# Example for multiple yearly files instead, comment out the line above and
# uncomment this block:
# files <- list.files("fars_data", pattern = "accident_.*\\.csv$", full.names = TRUE)
# fars_data <- bind_rows(lapply(files, read.csv, stringsAsFactors = FALSE))

# ------------------------------------------------------------------------------
# 2. CLASSIFY COLLISION TYPE
# ------------------------------------------------------------------------------
classify_collision <- function(harm_ev) {
  case_when(
    harm_ev == 9         ~ "Pedalcyclist",
    harm_ev == 8          ~ "Pedestrian",
    is.na(harm_ev)        ~ "Other",   # missing/unknown -- can't confirm type
    TRUE                  ~ "Motor Vehicle"  # every other HARM_EV code is
                                              # already a motor-vehicle crash
                                              # by FARS's inclusion criteria
  )
}

# Adjust column names here (harm_ev, persons, year) to match your data --
# FARS raw files typically use upper case (HARM_EV, YEAR); some import paths
# (e.g. haven::read_sas, or your own renaming) lowercase them instead.
plot_data <- fars_data %>%
  select(harm_ev, persons, year) %>%
  mutate(collision_type = classify_collision(harm_ev)) %>%
  group_by(year, collision_type) %>%
  summarise(n = sum(persons, na.rm = TRUE), .groups = "drop") %>%  # must name
                                                                     # the output
                                                                     # column (n = ...)
                                                                     # or ~n below
                                                                     # won't resolve
  complete(year, collision_type, fill = list(n = 0)) %>%  # ensure every year/
  arrange(year)                                            # category combo exists,
                                                             # so the area doesn't
                                                             # show false gaps/dips

# Set a consistent stacking order (bottom to top)
category_order <- c("Motor Vehicle", "Pedestrian", "Pedalcyclist", "Other")
plot_data$collision_type <- factor(plot_data$collision_type, levels = category_order)

# ------------------------------------------------------------------------------
# 4. BUILD STACKED AREA CHART
# ------------------------------------------------------------------------------
# Stacked areas in plotly use type = "scatter" with a shared `stackgroup` --
# there isn't a dedicated "area" trace type. mode = "none" hides the line/marker
# and just shows the filled area; use mode = "lines" instead if you want a
# visible border on each band.
colors <- c(
  "Motor Vehicle" = "#4C72B0",
  "Pedestrian"    = "#DD8452",
  "Pedalcyclist"  = "#55A868",
  "Other"         = "#C44E52"
)

fig <- plot_ly(
  data = plot_data,
  x = ~year,
  y = ~n,
  color = ~collision_type,
  colors = colors,
  type = "scatter",
  mode = "none",
  stackgroup = "one",
  hovertemplate = "%{x}<br>%{fullData.name}: %{y}<extra></extra>"
) %>%
  layout(
    title = "FARS Fatalities by Year and Collision Type",
    xaxis = list(title = "Year", dtick = 1),
    yaxis = list(title = "Number of Fatalities"),
    legend = list(title = list(text = "Collision Type")),
    hovermode = "x unified"
  )

fig

# To save as a standalone HTML file:
# htmlwidgets::saveWidget(fig, "fars_stacked_plot.html", selfcontained = TRUE)
