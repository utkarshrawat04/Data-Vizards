STOP_BOX <- list(lat = c(22.30, 22.85), lon = c(88.10, 88.65))       # Geographic bounding box for valid Kolkata bus stops

tidy_stop_name <- function(x) {
  x <- stringr::str_to_title(stringr::str_squish(x))
  stringr::str_replace_all(x, "\\b[B-DF-HJ-NP-TV-Zb-df-hj-np-tv-z]{2,4}\\b", function(w) toupper(w))
}      # Clean and format stop names: title case, preserving acronyms and short capitalized words without vowels

load_stops <- function(path = "data/bus_stops.xlsx") {
  s <- readxl::read_excel(path)
  s <- s |>
    transmute(stop_id,
              stop_name = tidy_stop_name(stop_name),
              stop_lat = as.numeric(stop_lat), stop_lon = as.numeric(stop_lon),
              stop_type = stringr::str_to_title(location_type),     # Stop / Junction / Terminal
              is_junction = is_junction == "Y") |>
    filter(!is.na(stop_lat), !is.na(stop_lon),
           between(stop_lat, STOP_BOX$lat[1], STOP_BOX$lat[2]),
           between(stop_lon, STOP_BOX$lon[1], STOP_BOX$lon[2])) |>
    distinct(stop_lat, stop_lon, .keep_all = TRUE)
  s
}    # Load and clean bus stop records from Excel, filtering by bounding box and removing duplicates


dist_m <- function(lat1, lon1, lat2, lon2) {
  r <- pi / 180
  a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2
  2 * 6371000 * asin(sqrt(pmin(1, a)))
}    # Calculate great-circle distance in metres between coordinates using the Haversine formula

add_stop_distance <- function(crashes, stops) {
  n <- nrow(crashes)
  crashes$nearest_stop <- NA_character_
  crashes$nearest_stop_type <- NA_character_
  crashes$dist_stop_m <- NA_real_
  crashes$dist_junction_m <- NA_real_
  junc <- stops$is_junction | stops$stop_type %in% c("Junction", "Terminal")
  for (i in which(!is.na(crashes$lat) & !is.na(crashes$lon))) {
    d <- dist_m(crashes$lat[i], crashes$lon[i], stops$stop_lat, stops$stop_lon)
    k <- which.min(d)
    crashes$nearest_stop[i] <- stops$stop_name[k]
    crashes$nearest_stop_type[i] <- stops$stop_type[k]
    crashes$dist_stop_m[i] <- d[k]
    crashes$dist_junction_m[i] <- min(d[junc])
  }
  crashes$bus <- ifelse(crashes$vehicle == "Bus", "Bus", "Other vehicles")
  crashes
}          # Calculate nearest bus stop, stop type, distance in metres, and distance to nearest junction for each crash