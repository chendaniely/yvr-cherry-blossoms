library(sf)
library(dplyr)
library(stringr)

# i have no idea what is an actual cherry blossom tree
# there are a few tree common names that look like it will fit this description
# this filtering affects all the trees that are plotted

# --- Load raw trees ---
trees <- st_read("data/original/public-trees.geojson")

# --- Filter to Japanese flowering cherry varieties ---
cherry_names <- unique(trees$common_name)[str_detect(
  unique(trees$common_name),
  "cherry|CHERRY"
)]

jap_flower_cherry <- cherry_names[str_detect(
  cherry_names,
  "JAP|jap|FLOWER|flower"
)]

# NOTE: the age will be frozen from last compute time, not actual to current time
# this shouldn't be an issue since we are about years (and 72% of the data is missing)
cherry_blossoms <- trees %>%
  filter(common_name %in% jap_flower_cherry) %>%
  mutate(
    age_years = as.numeric(difftime(Sys.Date(), as.Date(date_planted), units = "days")) / 365.25
  )

# --- Save ---
st_write(
  cherry_blossoms,
  "data/final/cherry_blossoms.geojson",
  delete_dsn = TRUE
)
