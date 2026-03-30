library(sf)
library(dplyr)

# --- Load final data ---
streets <- st_read("data/final/streets.geojson")
cherry_trees <- st_read("data/final/cherry_blossoms.geojson")

# --- Snap each tree to its nearest street segment ---
nearest_idx <- st_nearest_feature(cherry_trees, streets)

snapped <- cherry_trees %>%
  mutate(
    street_id = nearest_idx,
    street_label = streets$street_label[nearest_idx],
    street_type = streets$street_type[nearest_idx]
  )

# --- Save final output ---
st_write(snapped, "data/final/snapped-tree-street.geojson", delete_dsn = TRUE)

# --- Save intermediates for confirmation script ---
saveRDS(nearest_idx, "data/processed/nearest_idx.rds")
st_write(snapped, "data/processed/snapped.geojson", delete_dsn = TRUE)
