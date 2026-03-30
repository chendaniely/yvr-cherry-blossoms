library(sf)
library(dplyr)
library(tidyr)

# --- Load data ---
streets <- st_read("data/final/streets.geojson")
snapped <- st_read("data/final/snapped-tree-street.geojson")

# --- Count trees per street segment ---
tree_counts <- snapped %>%
  st_drop_geometry() %>%
  count(street_id, street_label, street_type, name = "tree_count")

# --- Join counts to street segments; compute length + per-100m density ---
streets_joined <- streets %>%
  mutate(street_id = row_number()) %>%
  left_join(tree_counts, by = "street_id")

# --- Data check: confirm street_label is consistent across the join ---
# If street_label.x != street_label.y it means the id<->name mapping drifted
# between the streets file and the snapped file (e.g. streets was regenerated)
label_mismatches <- streets_joined %>%
  st_drop_geometry() %>%
  filter(!is.na(street_label.y)) %>%
  filter(street_label.x != street_label.y)

if (nrow(label_mismatches) > 0) {
  warning(sprintf(
    "%d segment(s) have mismatched street_label after join — streets.geojson may be out of sync with snapped-tree-street.geojson. Re-run 01-clean scripts.",
    nrow(label_mismatches)
  ))
  print(label_mismatches %>% select(street_id, street_label.x, street_label.y))
} else {
  message("street_label check passed — labels consistent across join.")
}

streets_counted <- streets_joined %>%
  select(-street_label.y, -street_type.y) %>%
  rename(street_label = street_label.x, street_type = street_type.x) %>%
  mutate(
    tree_count = replace_na(tree_count, 0),
    length_m = as.numeric(st_length(geometry)),
    density = tree_count / length_m * 100
  )

# --- Aggregate to street level (combine all block segments per street name) ---
# This is the "which street to visit" view — sums across all blocks of a street
street_summary <- streets_counted %>%
  st_drop_geometry() %>%
  filter(tree_count > 0) %>%
  group_by(street_label, street_type) %>%
  summarise(
    total_trees = sum(tree_count),
    total_length = sum(length_m),
    n_segments = n(),
    .groups = "drop"
  ) %>%
  mutate(density = total_trees / total_length * 100) %>%
  arrange(desc(total_trees)) |>
  filter(!is.na(street_label))


# --- Top streets by raw count ---
top_by_count <- street_summary %>%
  slice_max(total_trees, n = 20)

# --- Top streets by density (trees per 100m) ---
# Require at least 5 trees to filter out short stub segments with inflated density
top_by_density <- street_summary %>%
  filter(total_trees >= 5) %>%
  slice_max(density, n = 20)

# --- Rejoin geometry for mapping ---
# Union all block segments per street into a single linestring
streets_sf <- streets_counted %>%
  filter(tree_count > 0) %>%
  group_by(street_label, street_type) %>%
  summarise(
    total_trees = sum(tree_count),
    total_length = sum(length_m),
    density = sum(tree_count) / sum(length_m) * 100,
    geometry = st_union(geometry),
    .groups = "drop"
  )

# --- Save ---
st_write(
  streets_sf,
  "data/processed/streets_counted.geojson",
  delete_dsn = TRUE
)
write.csv(
  street_summary,
  "data/processed/street_summary.csv",
  row.names = FALSE
)
