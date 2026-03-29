library(sf)
library(dplyr)

# --- Load raw street files ---
streets_pub <- st_read("data/original/public-streets.geojson")
streets_non <- st_read("data/original/non-city-streets.geojson")
streets_lan <- st_read("data/original/lanes.geojson")

# --- Standardize CRS to WGS84 ---
streets_pub <- st_transform(streets_pub, 4326)
streets_non <- st_transform(streets_non, 4326)
streets_lan <- st_transform(streets_lan, 4326)

# --- Combine with street type label ---
streets <- bind_rows(
  mutate(streets_pub, street_type = "Public"),
  mutate(streets_non, street_type = "Non-city"),
  mutate(streets_lan, street_type = "Lane")
)

# --- Save ---
st_write(streets, "data/final/streets.geojson", delete_dsn = TRUE)
