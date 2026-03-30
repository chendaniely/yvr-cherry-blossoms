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

# --- Build a unified street_label per source ---
# public-streets: hblock is already "6200 ALBERTA ST"
streets_pub <- streets_pub %>%
  mutate(
    street_type = "Public",
    street_label = hblock
  )

# non-city-streets: streetname only; prepend block number when available
streets_non <- streets_non %>%
  mutate(
    street_type = "Non-city",
    street_label = if_else(
      !is.na(from_hblk) & from_hblk != "",
      paste(from_hblk, streetname),
      streetname
    )
  )

# lanes: std_street + from_hundred_block prefix
streets_lan <- streets_lan %>%
  mutate(
    street_type = "Lane",
    street_label = paste(from_hundred_block, std_street)
  )

# --- Combine ---
streets <- bind_rows(streets_pub, streets_non, streets_lan)

# --- Save ---
st_write(streets, "data/final/streets.geojson", delete_dsn = TRUE)
