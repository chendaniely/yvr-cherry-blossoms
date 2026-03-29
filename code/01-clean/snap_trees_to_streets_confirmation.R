library(sf)
library(dplyr)
library(mapgl)

# --- Load intermediates saved by snap_trees_to_streets.R ---
streets     <- st_read("data/final/streets.geojson")
snapped     <- st_read("data/processed/snapped.geojson")
nearest_idx <- readRDS("data/processed/nearest_idx.rds")

# --- Build snap lines (slow step) ---
snap_lines <- st_nearest_points(
  st_read("data/final/cherry_blossoms.geojson"),
  streets[nearest_idx, ]
) %>%
  st_as_sf() %>%
  mutate(street_label = streets$street_label[nearest_idx])

# --- Confirm snapping visually ---
maplibre(style = openfreemap_style("bright"), bounds = snapped) %>%
  add_line_layer(
    id           = "all-streets",
    source       = streets,
    line_color   = "#aaaaaa",
    line_width   = 1,
    line_opacity = 0.4
  ) %>%
  add_line_layer(
    id             = "snap-lines",
    source         = snap_lines,
    line_color     = "#2563eb",
    line_width     = 1,
    line_opacity   = 0.6,
    line_dasharray = c(2, 1)
  ) %>%
  add_circle_layer(
    id                  = "cherry-trees",
    source              = snapped,
    circle_radius       = 4,
    circle_color        = "#e11d48",
    circle_opacity      = 0.9,
    circle_stroke_color = "white",
    circle_stroke_width = 1,
    tooltip             = "street_label",
    popup               = concat(
      "<b>", get_column("common_name"), "</b><br/>",
      get_column("address"), "<br/>",
      "Street: ", get_column("street_label")
    )
  )
