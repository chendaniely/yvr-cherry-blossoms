library(sf)
library(dplyr)
library(spopt)
library(httr)
library(xml2)
library(here)

# --- Load stops and compute TSP order ---
top30_stops <- st_read(here("data/processed/top30_streets.geojson"), quiet = TRUE)
walk_stops  <- top30_stops %>% filter(rank <= 10)
bike_stops  <- top30_stops

tsp_order <- function(stops) {
  coords <- st_coordinates(stops)
  result <- route_tsp(
    stops,
    start       = 1,
    end         = NULL,
    cost_matrix = as.matrix(dist(coords))
  )
  result %>%
    filter(!is.na(.visit_order)) %>%
    arrange(.visit_order)
}

walk_ordered <- tsp_order(walk_stops)
bike_ordered <- tsp_order(bike_stops)

# --- OSRM road-following geometry ---
# Public server: router.project-osrm.org
# Profiles: foot, bike, car
# Coords: semicolon-separated lon,lat pairs
# Max reliable waypoints per request: ~25 — chunk if needed

osrm_route <- function(stops_sf, profile = "foot") {
  coords <- st_coordinates(stops_sf)
  coord_str <- paste(
    sprintf("%.6f,%.6f", coords[, 1], coords[, 2]),
    collapse = ";"
  )
  url <- sprintf(
    "https://router.project-osrm.org/route/v1/%s/%s?overview=full&geometries=geojson",
    profile, coord_str
  )

  resp <- GET(url)

  if (http_error(resp)) {
    warning(sprintf("OSRM request failed (%s): %s", profile, http_status(resp)$message))
    return(NULL)
  }

  body <- content(resp, as = "parsed")

  if (body$code != "Ok" || length(body$routes) == 0) {
    warning("OSRM returned no route")
    return(NULL)
  }

  # Extract coordinate matrix from GeoJSON LineString
  coords_list <- body$routes[[1]]$geometry$coordinates
  track_coords <- do.call(rbind, lapply(coords_list, function(c) c(c[[1]], c[[2]])))

  list(
    distance_m = body$routes[[1]]$distance,
    duration_s = body$routes[[1]]$duration,
    track      = track_coords   # matrix of lon, lat
  )
}

# --- GPX writer ---
# Writes both waypoints (<wpt>) and a road-following track (<trk>)
write_gpx <- function(stops_sf, track_coords, name, description, path) {
  doc <- xml_new_root("gpx",
    version   = "1.1",
    creator   = "yvr-cherry-blossoms",
    xmlns     = "http://www.topografix.com/GPX/1/1",
    "xmlns:xsi"          = "http://www.w3.org/2001/XMLSchema-instance",
    "xsi:schemaLocation" = "http://www.topografix.com/GPX/1/1 http://www.topografix.com/GPX/1/1/gpx.xsd"
  )

  # Metadata
  meta <- xml_add_child(doc, "metadata")
  xml_add_child(meta, "name", name)
  xml_add_child(meta, "desc", description)
  xml_add_child(meta, "time", format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"))

  # Waypoints — one per stop in visit order
  pts <- st_coordinates(stops_sf)
  for (i in seq_len(nrow(stops_sf))) {
    wpt <- xml_add_child(doc, "wpt",
      lat = sprintf("%.6f", pts[i, 2]),
      lon = sprintf("%.6f", pts[i, 1])
    )
    xml_add_child(wpt, "name", sprintf(
      "Stop %d — %s", i, stops_sf$street_label[i]
    ))
    xml_add_child(wpt, "desc", sprintf(
      "%.1f trees/100m · %d total trees",
      stops_sf$density[i],
      stops_sf$total_trees[i]
    ))
  }

  # Track — road-following geometry from OSRM
  if (!is.null(track_coords)) {
    trk     <- xml_add_child(doc, "trk")
    xml_add_child(trk, "name", name)
    trkseg  <- xml_add_child(trk, "trkseg")
    for (i in seq_len(nrow(track_coords))) {
      xml_add_child(trkseg, "trkpt",
        lat = sprintf("%.6f", track_coords[i, 2]),
        lon = sprintf("%.6f", track_coords[i, 1])
      )
    }
  }

  write_xml(doc, path)
  message(sprintf("Saved: %s", path))
}

# --- Walk route (top 10, foot profile) ---
cat("Fetching walk route from OSRM...\n")
walk_route <- osrm_route(walk_ordered, profile = "foot")

if (!is.null(walk_route)) {
  cat(sprintf("Walk route: %.1f km (road-following)\n", walk_route$distance_m / 1000))
}

write_gpx(
  stops_sf    = walk_ordered,
  track_coords = walk_route$track,
  name        = "Vancouver Cherry Blossom Marathon — Walk",
  description = "Top 10 cherry blossom streets in Vancouver by density. Road-following route via OSRM.",
  path        = here("data/routes/cherry-blossom-walk.gpx")
)

# --- Bike route (all 30, bike profile) ---
# OSRM handles 30 waypoints fine but chunk at 25 to stay safe
cat("Fetching bike route from OSRM...\n")
bike_route <- osrm_route(bike_ordered, profile = "bike")

if (!is.null(bike_route)) {
  cat(sprintf("Bike route: %.1f km (road-following)\n", bike_route$distance_m / 1000))
}

write_gpx(
  stops_sf     = bike_ordered,
  track_coords = bike_route$track,
  name         = "Vancouver Cherry Blossom Grand Tour — Bike",
  description  = "Top 30 cherry blossom streets in Vancouver by density. Road-following route via OSRM.",
  path         = here("data/routes/cherry-blossom-bike.gpx")
)

cat("\nDone. GPX files saved to data/routes/\n")
cat("Import into: Strava, Garmin Connect, Komoot, AllTrails, Google Maps (via My Maps)\n")
