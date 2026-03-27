library(arrow)
library(sf)
library(leaflet)
library(dplyr)
library(tidyr)
library(mapgl)
library(tidyverse)

# --- 1. Load trees ---
trees <- st_read("data/public-trees.geojson")

# --- 2. Filter Japanese flowering cherry varieties ---
cherry_names <- unique(trees$common_name)[stringr::str_detect(
  unique(trees$common_name),
  "cherry|CHERRY"
)]

jap_flower_cherry <- cherry_names[stringr::str_detect(cherry_names, "JAP|jap|FLOWER|flower")]

sf_filtered <- trees %>%
  filter(common_name %in% jap_flower_cherry)

# --- 3. Load streets ---
streets_pub <- st_read("data/public-streets.geojson")
streets_non <- st_read("data/non-city-streets.geojson")
streets_lan <- st_read("data/lanes.geojson")

streets_pub <- st_transform(streets_pub, st_crs(sf_filtered))
streets_non <- st_transform(streets_non, st_crs(sf_filtered))
streets_lan <- st_transform(streets_lan, st_crs(sf_filtered))

# --- 4. Combine streets ---
streets <- bind_rows(
  mutate(streets_pub, street_type = "Public"),
  mutate(streets_non, street_type = "Non-city"),
  mutate(streets_lan, street_type = "Lane")
)

# --- 5. Snap trees to nearest street, count per segment ---
nearest_idx            <- st_nearest_feature(sf_filtered, streets)
sf_filtered$street_id  <- nearest_idx
sf_filtered$street_name <- streets$hblock[nearest_idx]   # adjust column name if needed

tree_counts <- sf_filtered %>%
  st_drop_geometry() %>%
  count(street_id, street_name, name = "tree_count")

streets_counted <- streets %>%
  mutate(street_id = row_number()) %>%
  left_join(tree_counts, by = "street_id") %>%
  mutate(tree_count = tidyr::replace_na(tree_count, 0))

# --- 6. Normalize by street length (trees per 100m) ---
streets_counted <- streets_counted %>%
  mutate(
    length_m = as.numeric(st_length(.)),
    density  = tree_count / length_m * 100
  )

hot_streets <- streets_counted %>% filter(tree_count > 0)

# --- 7. Palettes ---
pal_street <- colorNumeric(
  palette  = "YlOrRd",
  domain   = hot_streets$density,
  na.color = "transparent"
)

pal_trees <- colorFactor(
  palette = "Set1",
  domain  = sf_filtered$common_name
)

# --- 8. Plot ---
leaflet() %>%
  addTiles() %>%
  addPolylines(
    data    = streets,
    color   = "#aaaaaa",
    weight  = 1,
    opacity = 0.4,
    group   = "All streets"
  ) %>%
  addPolylines(
    data    = hot_streets,
    color   = ~pal_street(density),
    weight  = 4,
    opacity = 0.9,
    label   = ~paste0(street_name, ": ", tree_count, " trees (", round(density, 1), "/100m)"),
    group   = "Cherry density"
  ) %>%
  addCircleMarkers(
    data        = sf_filtered,
    radius      = 4,
    color       = ~pal_trees(common_name),
    fillColor   = ~pal_trees(common_name),
    fillOpacity = 0.8,
    stroke      = FALSE,
    label       = ~paste0(common_name, " — ", address),
    popup       = ~paste0("<b>", common_name, "</b><br/>", address),
    group       = "Cherry trees"
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = pal_street,
    values   = hot_streets$density,
    title    = "Trees per 100m"
  ) %>%
  addLegend(
    position = "bottomleft",
    pal      = pal_trees,
    values   = sf_filtered$common_name,
    title    = "Cherry variety"
  ) %>%
  addLayersControl(
    overlayGroups = c("All streets", "Cherry density", "Cherry trees"),
    options       = layersControlOptions(collapsed = FALSE)
  )



# --- Top hot streets map ---

# Summarise by street name (aggregates across block segments)
street_summary <- streets_counted %>%
  st_drop_geometry() %>%
  filter(tree_count > 0) %>%
  group_by(hblock) %>%
  summarise(
    total_trees   = sum(tree_count),
    total_length  = sum(length_m),
    density       = total_trees / total_length * 100
  ) %>%
  arrange(desc(density))

# Define "worth visiting" threshold
top_streets <- street_summary %>%
  filter(total_trees >= 10, density >= 10) %>%  # tweak these thresholds
  arrange(-total_trees)

n_spots <- 10   # change this to however many you want

# Rejoin geometry
top_streets_sf <- streets_counted %>%
  filter(hblock %in% top_streets$hblock) %>%
  group_by(hblock) %>%
  summarise(
    total_trees  = sum(tree_count),
    total_length = sum(length_m),
    density      = total_trees / total_length * 100,
    geometry     = st_union(geometry)
  ) %>%
  slice_max(density, n = n_spots)

# Representative point per street (for TSP later)
top_streets_sf <- top_streets_sf %>%
  mutate(centroid = st_centroid(geometry)) %>%
  select(-centroid)

pal_hot <- colorNumeric(
  palette  = "YlOrRd",
  domain   = top_streets_sf$density
)

leaflet() %>%
  addTiles() %>%
  addPolylines(
    data    = streets,
    color   = "#aaaaaa",
    weight  = 1,
    opacity = 0.3,
    group   = "All streets"
  ) %>%
  addPolylines(
    data        = top_streets_sf,
    color       = ~pal_hot(density),
    weight      = 5,
    opacity     = 0.9,
    label       = ~paste0(hblock, " | ", total_trees, " trees | ", round(density, 1), "/100m"),
    popup       = ~paste0(
      "<b>", hblock, "</b><br/>",
      "Trees: ", total_trees, "<br/>",
      "Length: ", round(total_length), "m<br/>",
      "Density: ", round(density, 1), " trees/100m"
    ),
    group       = "Hot streets"
  ) %>%
  addLegend(
    position = "bottomright",
    pal      = pal_hot,
    values   = top_streets_sf$density,
    title    = "Trees per 100m"
  ) %>%
  addLayersControl(
    overlayGroups = c("All streets", "Hot streets"),
    options       = layersControlOptions(collapsed = FALSE)
  )












# Traveling salesman -----
library(spopt)

# --- 1. Prepare stops for spopt ---
# spopt needs an sf point object with an id column
# We use the centroid of each hot street as the "stop"
tsp_stops <- top_streets_sf %>%
  mutate(
    id       = row_number(),
    geometry = st_centroid(geometry)   # convert lines -> points
  ) %>%
  select(id, hblock, total_trees, density, geometry)

# --- 2. Build the cost matrix (Euclidean, no routing engine needed) ---
# For a quick prototype we use straight-line distance
# Replace with r5r/OSRM travel times for real driving order
coords <- st_coordinates(tsp_stops)

cost_matrix <- as.matrix(dist(coords))   # n x n distance matrix

# --- 3. Run TSP ---
# Open route: start at stop 1, end anywhere (no need to return to start)
result <- route_tsp(
  tsp_stops,
  start        = 1,
  end          = NULL,      # open route — don't return to start
  cost_matrix  = cost_matrix
)

# --- 4. Check the result ---
meta <- attr(result, "spopt")
cat(sprintf("Optimized route distance: %.0f m\n",  meta$total_cost))
cat(sprintf("Nearest-neighbor baseline: %.0f m\n", meta$nn_cost))
cat(sprintf("Improvement: %.1f%%\n",               meta$improvement_pct))

# --- 5. View visit order ---
result %>%
  st_drop_geometry() %>%
  arrange(.visit_order) %>%
  select(.visit_order, hblock, total_trees, density)

# --- 6. Plot the TSP route on the map ---
result_ordered <- result %>%
  filter(!is.na(.visit_order)) %>%
  arrange(.visit_order) %>%
  mutate(label = as.character(.visit_order))

# Draw route lines by connecting stops in order
route_line <- result_ordered %>%
  st_coordinates() %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_combine() %>%               # combine all points into one geometry
  st_cast("LINESTRING") %>%      # cast to linestring
  st_as_sf()                     # wrap back into sf dataframe










### MAP






pal_hot <- colorNumeric("YlOrRd", domain = top_streets_sf$density)
varieties <- unique(sf_filtered$common_name)
n_varieties <- length(varieties)
variety_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_varieties)


maplibre(style = openfreemap_style("bright"), bounds = top_streets_sf) %>%
  add_line_layer(
    id            = "all-streets",
    source        = streets,
    line_color    = "#aaaaaa",
    line_width    = 1,
    line_opacity  = 0.3
  ) %>%
  add_line_layer(
    id           = "hot-streets",
    source       = top_streets_sf,
    line_color   = interpolate(
      column   = "density",
      values   = c(
        min(top_streets_sf$density),
        median(top_streets_sf$density),
        max(top_streets_sf$density)
      ),
      stops    = c("#ffffb2", "#fd8d3c", "#bd0026"),
      na_color = "transparent"
    ),
    line_width   = 5,
    line_opacity = 0.9,
    tooltip      = "hblock",
    popup        = concat(
      "<b>", get_column("hblock"), "</b><br/>",
      "Trees: ", get_column("total_trees"), "<br/>",
      "Density: ", number_format(get_column("density"), 1), " trees/100m"
    )
  ) %>%
  add_circle_layer(
    id                   = "cherry-trees",
    source               = sf_filtered,
    circle_radius        = 4,
    circle_color         = match_expr(
  column = "common_name",
  values = unique(sf_filtered$common_name),
  stops  = variety_colors
),
    circle_opacity       = 0.8,
    circle_stroke_color  = "white",
    circle_stroke_width  = 1,
    tooltip              = "address",
    popup                = concat(
      "<b>", get_column("common_name"), "</b><br/>",
      get_column("address")
    )
  )


# --- 2. TSP route map ---

result_ordered <- result %>%
  filter(!is.na(.visit_order)) %>%
  arrange(.visit_order) %>%
  mutate(label = as.character(.visit_order))

# Build route line connecting stops in visit order
route_line <- result_ordered %>%
  st_coordinates() %>%
  as.data.frame() %>%
  st_as_sf(coords = c("X", "Y"), crs = 4326) %>%
  st_combine() %>%
  st_cast("LINESTRING") %>%
  st_as_sf()

maplibre(style = openfreemap_style("bright"), bounds = result_ordered) %>%
  add_line_layer(
    id           = "all-streets",
    source       = streets,
    line_color   = "#aaaaaa",
    line_width   = 1,
    line_opacity = 0.3
  ) %>%
  add_line_layer(
    id           = "tsp-route",
    source       = route_line,
    line_color   = "#2563eb",
    line_width   = 3,
    line_opacity = 0.8,
    line_dasharray = c(2, 1)
  ) %>%
  add_circle_layer(
    id                  = "tsp-stops",
    source              = result_ordered,
    circle_radius       = 10,
    circle_color        = "white",
    circle_stroke_color = "#2563eb",
    circle_stroke_width = 2,
    tooltip             = "hblock",
    popup               = concat(
      "<b>Stop ", get_column(".visit_order"), "</b><br/>",
      get_column("hblock"), "<br/>",
      "Trees: ", get_column("total_trees"), "<br/>",
      "Density: ", number_format(get_column("density"), 1), " trees/100m"
    )
  ) %>%
  add_symbol_layer(
    id         = "tsp-labels",
    source     = result_ordered,
    text_field = get_column("label"),
    text_size  = 11,
    text_color = "#2563eb"
  )


##

# Build a color vector that scales to any number of varieties
sf_filtered_clean <- sf_filtered %>%
  select(common_name, address, geometry)

varieties <- unique(sf_filtered_clean$common_name)
n_varieties <- length(varieties)
variety_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(n_varieties)

# map variety names to colors as a named vector
color_map <- setNames(variety_colors, varieties)

# add color as a plain column — avoids match_expr limitations
sf_filtered_clean <- sf_filtered_clean %>%
  mutate(color = color_map[common_name])




base <- maplibre(style = openfreemap_style("bright"), bounds = result_ordered) %>%
  add_line_layer(
    id           = "all-streets",
    source       = streets,
    line_color   = "#aaaaaa",
    line_width   = 1,
    line_opacity = 0.3
  )

base_trees <- base %>%
  add_circle_layer(
    id                  = "cherry-trees",
    source              = sf_filtered_clean,
    circle_radius       = 4,
    circle_color        = get_column("color"),   # use the pre-computed color column
    circle_opacity      = 0.9,
    circle_stroke_color = "white",
    circle_stroke_width = 0.5,
    tooltip             = "address",
    popup               = concat(
      "<b>", get_column("common_name"), "</b><br/>",
      get_column("address")
    )
  ) %>%
  add_categorical_legend(
    legend_title = "Cherry variety",
    values       = varieties,
    colors       = variety_colors,
    position     = "bottom-left"
  )

  
  
base_trees %>%
  add_line_layer(
    id             = "tsp-route",
    source         = route_line,
    line_color     = "#2563eb",
    line_width     = 3,
    line_opacity   = 0.8,
    line_dasharray = c(2, 1)
  ) %>%
  add_circle_layer(
    id                  = "tsp-stops",
    source              = result_ordered,
    circle_radius       = 10,
    circle_color        = "white",
    circle_stroke_color = "#2563eb",
    circle_stroke_width = 2,
    tooltip             = "hblock",
    popup               = concat(
      "<b>Stop ", get_column(".visit_order"), "</b><br/>",
      get_column("hblock"), "<br/>",
      "Trees: ", get_column("total_trees"), "<br/>",
      "Density: ", number_format(get_column("density"), 1), " trees/100m"
    )
  ) %>%
  add_symbol_layer(
    id         = "tsp-labels",
    source     = result_ordered,
    text_field = get_column("label"),
    text_size  = 11,
    text_color = "#2563eb"
  )



# traveling sales man using the roads

library(sfnetworks)
library(igraph)
library(tidygraph)


streets_lines <- streets %>%
  st_cast("MULTILINESTRING") %>%   # ensure it's multilinestring first
  st_cast("LINESTRING")            # then explode to individual linestrings


# --- 1. Build road network from your streets geometry ---
net <- as_sfnetwork(streets_lines, directed = FALSE) %>%
  activate("edges") %>%
  mutate(weight = edge_length())


# --- 2. Find nearest node on network for each stop ---
tsp_stops <- top_streets_sf %>%
  mutate(geometry = st_centroid(geometry))

nearest_nodes <- st_nearest_feature(tsp_stops, net %>% activate("nodes") %>% st_as_sf())

# --- 3. Build cost matrix using network shortest paths ---

# check for problems
any(is.infinite(cost_matrix))
any(is.na(cost_matrix))
sum(is.infinite(cost_matrix))
sum(is.na(cost_matrix))

# --- fix 1: clean up the network topology ---
net <- as_sfnetwork(streets_lines, directed = FALSE) %>%
  tidygraph::convert(to_spatial_subdivision) %>%   # split edges at intersections
  convert(to_spatial_smooth) %>%        # remove degree-2 nodes
  activate("edges") %>%
  mutate(weight = edge_length())

# --- fix 2: rebuild cost matrix ---
cost_matrix <- igraph::distances(
  net,
  v       = nearest_nodes,
  to      = nearest_nodes,
  weights = igraph::E(net)$weight
)

# --- fix 3: if still Inf, snap stops to largest connected component only ---
components <- igraph::components(net)
largest_component <- which.max(components$csize)

# keep only nodes in the largest component
net_connected <- net %>%
  activate("nodes") %>%
  filter(igraph::components(net)$membership == largest_component)

# re-snap stops to the connected network
nearest_nodes <- st_nearest_feature(
  tsp_stops,
  net_connected %>% activate("nodes") %>% st_as_sf()
)

cost_matrix <- igraph::distances(
  net_connected,
  v       = nearest_nodes,
  to      = nearest_nodes,
  weights = igraph::E(net_connected)$weight
)

# confirm no more Inf
any(is.infinite(cost_matrix))
any(is.na(cost_matrix))



n_stops <- nrow(tsp_stops)
cost_matrix <- matrix(0, nrow = n_stops, ncol = n_stops)

for (i in seq_len(n_stops)) {
  for (j in seq_len(n_stops)) {
    if (i != j) {
      path <- igraph::shortest_paths(
        net,
        from    = nearest_nodes[i],
        to      = nearest_nodes[j],
        weights = igraph::E(net)$weight
      )$vpath[[1]]

      cost_matrix[i, j] <- if (length(path) > 0) {
        sum(igraph::E(net)$weight[igraph::get.edge.ids(
          net,
          vp = as.vector(rbind(head(path, -1), tail(path, -1)))
        )])
      } else {
        Inf
      }
    }
  }
}

# --- 4. Run TSP ---
result <- route_tsp(
  tsp_stops,
  start       = 1,
  end         = NULL,
  cost_matrix = cost_matrix
)

meta <- attr(result, "spopt")
cat(sprintf("Optimized route: %.0f m\n",  meta$total_cost))
cat(sprintf("Improvement: %.1f%%\n",      meta$improvement_pct))

# --- 5. Get actual road geometry for each leg ---
result_ordered <- result %>%
  filter(!is.na(.visit_order)) %>%
  arrange(.visit_order) %>%
  mutate(stop_idx = row_number())   # add explicit index

# index nearest_nodes by position, not by id column
ordered_nodes <- nearest_nodes[result_ordered$stop_idx]

route_legs <- lapply(seq_len(nrow(result_ordered) - 1), function(i) {
  from_node <- as.integer(ordered_nodes[i])
  to_node   <- as.integer(ordered_nodes[i + 1])

  path <- igraph::shortest_paths(
    net,
    from    = from_node,
    to      = to_node,
    weights = igraph::E(net)$weight
  )$vpath[[1]]

  edge_ids <- igraph::get.edge.ids(
    net,
    vp = as.vector(rbind(
      head(as.integer(path), -1),
      tail(as.integer(path), -1)
    ))
  )

  net %>%
    activate("edges") %>%
    st_as_sf() %>%
    slice(edge_ids) %>%
    mutate(leg = i)
})

route_line <- bind_rows(route_legs)


# final map?

sf_filtered_clean <- sf_filtered %>%
  select(common_name, address, geometry) %>%
  mutate(color = color_map[common_name])

result_ordered <- result_ordered %>%
  mutate(label = as.character(.visit_order))

maplibre(style = openfreemap_style("bright"), bounds = sf_filtered_clean) %>%
  # background streets
  add_line_layer(
    id           = "all-streets",
    source       = streets,
    line_color   = "#aaaaaa",
    line_width   = 1,
    line_opacity = 0.3
  ) %>%
  # actual road-following TSP route
  # add_line_layer(
  #   id             = "tsp-route",
  #   source         = route_line,
  #   line_color     = "#2563eb",
  #   line_width     = 3,
  #   line_opacity   = 0.8,
  #   line_dasharray = c(2, 1)
  # ) %>%
  # cherry tree points
  add_circle_layer(
    id                  = "cherry-trees",
    source              = sf_filtered_clean,
    circle_radius       = 4,
    circle_color        = get_column("color"),
    circle_opacity      = 0.9,
    circle_stroke_color = "white",
    circle_stroke_width = 0.5,
    tooltip             = "address",
    popup               = concat(
      "<b>", get_column("common_name"), "</b><br/>",
      get_column("address")
    )
  ) %>%
  # TSP stop circles
  add_circle_layer(
    id                  = "tsp-stops",
    source              = result_ordered,
    circle_radius       = 12,
    circle_color        = "white",
    circle_stroke_color = "#2563eb",
    circle_stroke_width = 2,
    tooltip             = "hblock",
    popup               = concat(
      "<b>Stop ", get_column(".visit_order"), "</b><br/>",
      get_column("hblock"), "<br/>",
      "Trees: ", get_column("total_trees"), "<br/>",
      "Density: ", number_format(get_column("density"), 1), " trees/100m"
    )
  ) %>%
  # TSP stop number labels
  add_symbol_layer(
    id         = "tsp-labels",
    source     = result_ordered,
    text_field = get_column("label"),
    text_size  = 11,
    text_color = "#2563eb"
  ) %>%
  add_categorical_legend(
    legend_title = "Cherry variety",
    values       = varieties,
    colors       = variety_colors,
    position     = "bottom-left"
  )
