library(sf)
library(dplyr)
library(tidyr)

# --- Load data ---
streets <- st_read("data/final/streets.geojson")
snapped <- st_read("data/final/snapped-tree-street.geojson")

# --- Bloom-readiness thresholds (adjust these to tune which trees count) ---
# Japanese flowering cherries typically first bloom at ~5 years; mature trees are
# more reliable. Height and diameter serve as proxies for structural maturity.

# from claude:
# The height:diameter ratio is left here as a potential future filter — a low
# ratio (squat tree) may indicate stress or slow growth, but the evidence for
# it predicting bloom is weaker than age/size alone, so it is commented out.


min_age_years <- 5 # years since planting
min_height_m <- 2.5 # metres
min_diameter_cm <- 7 # centimetres (trunk diameter)
# min_hd_ratio  <- 0.3  # height_m / diameter_cm — uncomment to test

blooming_trees <- snapped

# the age has 72% of data missing
# there's not enough trees measured under 2.5m to make a change in the analysis
# snapped%>%
#   filter(
#     #!is.na(age_years), # too much missing data in age to be useful
#     !is.na(height_m),
#     !is.na(diameter_cm),
#     #age_years >= min_age_years, # too much missing data to be useful
#     height_m >= min_height_m,
#     diameter_cm >= min_diameter_cm
#   )

# --- Count trees per street segment ---
tree_counts <- blooming_trees %>%
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
