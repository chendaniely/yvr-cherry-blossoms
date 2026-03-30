library(sf)
library(tidyverse)
library(mapgl)

# --- Load streets and pick one to explore ---
streets <- st_read("data/final/streets.geojson")

one_street <- streets %>%
  filter(std_street == "GRANVILLE ST") %>%
  mutate(
    segment_id = row_number(),
    # cycle colors so adjacent segments are visually distinct
    segment_color = rep(
      c("#e76f51", "#2a9d8f", "#457b9d", "#9b5de5"),
      length.out = n()
    )
  )

cat("Number of segments:", nrow(one_street), "\n")

# --- Map: each block is a separate segment ---
maplibre(bounds = one_street) %>%
  add_line_layer(
    id = "segments",
    source = one_street,
    line_color = get_column("segment_color"),
    line_width = 5,
    tooltip = concat(
      "Segment ",
      get_column("segment_id"),
      ": ",
      get_column("street_label")
    )
  )
