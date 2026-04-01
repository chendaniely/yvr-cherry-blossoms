library(sf)
library(dplyr)
library(osmdata)
library(here)

vancouver <- st_read(here("data/original/local-area-boundary.geojson"), quiet = TRUE)

bbox_4326 <- vancouver %>%
  st_transform(4326) %>%
  st_bbox()

set_overpass_url("https://overpass.kumi.systems/api/interpreter")

osm_raw <- opq(bbox = bbox_4326, timeout = 120) %>%
  add_osm_feature(
    key = "highway",
    value = c(
      "primary",
      "secondary",
      "tertiary",
      "residential",
      "unclassified",
      "service",
      "living_street"
    )
  ) %>%
  osmdata_sf()

osm_streets <- osm_raw$osm_lines %>%
  select(geometry) %>%
  st_transform(st_crs(vancouver))

st_write(osm_streets, here("data/processed/osm_streets.geojson"), delete_dsn = TRUE)
