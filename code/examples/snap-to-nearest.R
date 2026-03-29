library(sf)
library(tidyverse)
library(mapgl)

street_colors <- c("Oak St" = "#e76f51", "Elm Ave" = "#2a9d8f", "Cedar Rd" = "#457b9d", "Maple Blvd" = "#9b5de5")

# --- 1. Toy streets (3 line segments) ---
streets <- st_sf(
  id   = 1:4,
  name = c("Oak St", "Elm Ave", "Cedar Rd", "Maple Blvd"),
  geometry = st_sfc(
    st_linestring(rbind(c(0, 0), c(10, 0))),   # horizontal
    st_linestring(rbind(c(0, 8), c(10, 8))),   # horizontal (upper)
    st_linestring(rbind(c(5, -1), c(5, 10))),  # vertical
    st_linestring(rbind(c(0, 5), c(4, 10))),   # diagonal
    crs = 4326
  )
)

# --- 2. Toy trees (points scattered around) ---
trees <- st_sf(
  id = 1:9,
  geometry = st_sfc(
    st_point(c(1,  -1.2)),
    st_point(c(3,   1.0)),
    st_point(c(7,  -0.8)),
    st_point(c(2,   4.0)),
    st_point(c(8,   4.5)),
    st_point(c(3,   7.0)),
    st_point(c(6,   9.2)),
    st_point(c(9,   6.5)),
    st_point(c(4.5, 3.5)),
    crs = 4326
  )
)

# --- Plot: raw data before snapping ---
ggplot() +
  geom_sf(data = streets, aes(color = name), linewidth = 2.5) +
  geom_sf_text(data = streets, aes(label = name, color = name), size = 3.5, show.legend = FALSE) +
  geom_sf(data = trees, size = 4, shape = 16, color = "grey30") +
  scale_color_manual(values = street_colors, name = "Street") +
  labs(
    title    = "Streets and trees (before snapping)",
    subtitle = "Which street does each point belong to?"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- 3. Snap each tree to its nearest street ---
nearest_idx <- st_nearest_feature(trees, streets)

trees <- trees %>%
  mutate(
    nearest_street_id   = nearest_idx,
    nearest_street_name = streets$name[nearest_idx]
  )

# --- 4. Count trees per street segment ---
tree_counts <- trees %>%
  st_drop_geometry() %>%
  count(nearest_street_id, nearest_street_name, name = "tree_count")

streets_counted <- streets %>%
  left_join(tree_counts, by = c("id" = "nearest_street_id"))

print(streets_counted %>% st_drop_geometry() %>% select(name, tree_count))

# --- 5. Build snap lines (tree → nearest point on its street) ---
# trees already has a geometry column, so best to create a new object
# this step is for plotting the lines on which points snap to which line
snap_lines <- trees %>%
  mutate(geometry = st_sfc(map2(
    geometry,
    streets$geometry[nearest_street_id],
    ~ st_nearest_points(st_sfc(.x, crs = 4326), st_sfc(.y, crs = 4326))[[1]]
  ), crs = 4326)) %>%
  select(tree_id = id)

# --- 6. Plot ---
ggplot() +
  geom_sf(data = streets, aes(color = name), linewidth = 2.5) +
  geom_sf(data = snap_lines, color = "grey55", linetype = "dashed", linewidth = 0.6) +
  geom_sf(data = trees, aes(color = nearest_street_name), size = 4, shape = 16) +
  geom_sf_label(
    data  = streets_counted,
    aes(label = paste0(name, "\n(", tree_count, " trees)"), color = name),
    size  = 3.5,
    nudge_y = -1.2,
    show.legend = FALSE
  ) +
  scale_color_manual(values = street_colors, name = "Nearest street") +
  labs(
    title    = "Snapping points to nearest line segment",
    subtitle = "Dashed lines show each point's assignment to its closest street"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# --- 7. Maplibre plot ---
# just random points on a real map overlay
# this is just to show a point
streets_colored <- streets_counted %>% mutate(color = street_colors[name])
trees_colored   <- trees %>% mutate(color = street_colors[nearest_street_name])

maplibre(bounds = streets_colored) %>%
  add_line_layer(
    id           = "streets",
    source       = streets_colored,
    line_color   = get_column("color"),
    line_width   = 4,
    tooltip      = "name"
  ) %>%
  add_line_layer(
    id             = "snap-lines",
    source         = snap_lines,
    line_color     = "#888888",
    line_width     = 1.5,
    line_dasharray = c(2, 2)
  ) %>%
  add_circle_layer(
    id                  = "trees",
    source              = trees_colored,
    circle_radius       = 6,
    circle_color        = get_column("color"),
    circle_stroke_color = "white",
    circle_stroke_width = 1,
    tooltip             = "nearest_street_name"
  )
