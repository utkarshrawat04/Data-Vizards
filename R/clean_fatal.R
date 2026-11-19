source("R/clean_data.R")                  # Import core parsing and standardization utilities

# Remove trailing parenthetical notes/directions from road user and vehicle strings
strip_direction <- function(x) str_squish(str_replace(coalesce(x, ""), "\\s*\\([^)]*\\)\\s*$", ""))
strip_vehicle_dir <- function(x) str_squish(str_replace(coalesce(x, ""), "\\s*\\([NSEWnsew\\- ]+\\)\\s*$", ""))

# Parse hour strictly first, falling back to regex extraction for text ranges (e.g., "18.15hrs & 18.30hrs")
parse_hour_loose <- function(v) {
  h <- parse_hour(v)
  if (is.na(h) && !is.na(v)) {
    m <- str_match(as.character(v), "(\\d{1,2})\\s*[.:]\\s*(\\d{2})")
    if (!is.na(m[1, 2])) {
      h2 <- as.integer(m[1, 2])
      if (h2 >= 0 && h2 <= 23) h <- h2
    }
  }
  h
}

FATAL_STATION_FIX <- c(
  "Hasting" = "Hastings", "Taratola" = "Taratala", "Taraola" = "Taratala", "Tartala" = "Taratala",
  "Manicktola" = "Manicktala", "Maniktala" = "Manicktala", "Burtala" = "Burtolla", "Burtola" = "Burtolla",
  "Ekbalpur" = "Ekbalpore", "Shakespear" = "Shakespeare Sarani", "Shakespeare" = "Shakespeare Sarani",
  "Jorasankho" = "Jorasanko", "Bhawanipore" = "Bhowanipore", "Kareya" = "Karaya",
  "Parkstreet" = "Park Street", "Parnashree" = "Parnasree", "K L C" = "Kolkata Leather Complex")  # Lookup map standardizing misspelled police station names in fatal case headers

fatal_station <- function(x) {
  s <- str_replace(coalesce(x, ""), regex("\\s*(P\\.?\\s?S\\.?\\s?)?CASE\\b.*$", ignore_case = TRUE), "")
  s <- station_clean(s)
  ifelse(s %in% names(FATAL_STATION_FIX), unname(FATAL_STATION_FIX[s]), s)
}   # Extract police station name from fatal case reference prefix and normalize spelling

load_fatal <- function(path = "data/data.xlsx", years = 2021:2023) {
  all <- lapply(years, function(y) {
    raw <- suppressMessages(
      read_excel(path, sheet = paste0("Fatal - ", y), col_names = FALSE, skip = 1, col_types = "text"))
    col <- function(k) if (ncol(raw) >= k) raw[[k]] else rep(NA_character_, nrow(raw))
    raw <- raw[!is.na(suppressWarnings(as.numeric(col(1)))), ]     # Keep numbered records only and exclude summary rows
    
    hr      <- vapply(col(6), parse_hour_loose, integer(1), USE.NAMES = FALSE)
    dts     <- do.call(c, lapply(col(5), parse_date))
    date_ok <- !is.na(dts) & as.integer(format(dts, "%Y")) == y
    ll      <- t(vapply(col(15), parse_latlon, numeric(2), USE.NAMES = FALSE))
    hel     <- str_to_lower(str_squish(coalesce(col(10), "")))
    
    tibble(
      year          = y,
      month         = ifelse(date_ok, as.integer(format(dts, "%m")), NA_integer_),
      date          = as.Date(ifelse(date_ok, dts, NA), origin = "1970-01-01"),
      hour          = hr,
      station       = fatal_station(col(3)),
      place         = str_squish(col(4)),
      vehicle_raw   = str_squish(col(13)),
      vehicle       = vehicle_group(strip_vehicle_dir(col(13))),
      road_user_raw = str_squish(col(8)),
      road_user     = road_user_group(strip_direction(col(8))),
      lat           = ll[, 1],
      lon           = ll[, 2],
      sex           = str_extract(str_to_upper(coalesce(col(9), "")), "[MF]"),
      age           = suppressWarnings(as.integer(str_extract(col(11), "\\d+"))),
      helmet        = case_when(str_detect(hel, "^w.*out") ~ "Without helmet",
                                str_detect(hel, "^w")      ~ "With helmet",
                                TRUE                       ~ NA_character_),
      accident_type = str_squish(col(12)),
      persons       = coalesce(suppressWarnings(as.integer(col(17))), 1L)
    )
  })
  bind_rows(all) %>%
    mutate(
      month_start = as.Date(ifelse(is.na(month), NA, sprintf("%d-%02d-01", year, month))),
      severity    = "Fatal"
    )
}  # Load and standardize fatal crash sheets across specified years, adding fatal-specific victim fields

load_busstops <- function(path = "data/BUSSTOP.xlsx") {
  s <- suppressMessages(read_excel(path))
  s <- transmute(s,
                 stop_id       = as.integer(stop_id),
                 stop_name     = str_to_title(str_squish(stop_name)),
                 stop_lat      = as.numeric(stop_lat),
                 stop_lon      = as.numeric(stop_lon),
                 location_type = str_to_title(location_type),
                 junction      = coalesce(is_junction == "Y", FALSE))
  filter(s, !is.na(stop_lat), !is.na(stop_lon),
         stop_lat >= KOL_LAT[1], stop_lat <= KOL_LAT[2],
         stop_lon >= KOL_LON[1], stop_lon <= KOL_LON[2])
}  # Load WBTC bus stop coordinates and filter to valid Kolkata bounding box

haversine <- function(lat1, lon1, lat2, lon2) {
  rad <- pi / 180
  a <- sin((lat2 - lat1) * rad / 2)^2 +
    cos(lat1 * rad) * cos(lat2 * rad) * sin((lon2 - lon1) * rad / 2)^2
  2 * 6371000 * asin(pmin(1, sqrt(a)))
}  # Compute Haversine great-circle distance in metres between coordinates

nearest_stop <- function(lat, lon, stops) {
  n <- length(lat)
  dist <- rep(NA_real_, n)
  id   <- rep(NA_integer_, n)
  for (i in which(!is.na(lat) & !is.na(lon))) {
    d <- haversine(lat[i], lon[i], stops$stop_lat, stops$stop_lon)
    k <- which.min(d)
    dist[i] <- d[k]
    id[i]   <- stops$stop_id[k]
  }
  list(dist = dist, id = id)
}   # Find minimum distance and corresponding stop ID for each geocoded crash

# Combine fatal and non-fatal records and append nearest stop/junction distances
build_crash_data <- function(path = "data/data.xlsx", stops_path = "data/BUSSTOP.xlsx") {
  nf    <- load_nonfatal(path)
  fa    <- load_fatal(path)
  stops <- load_busstops(stops_path)
  crashes <- bind_rows(nf, fa) %>% mutate(crash_id = row_number())
  
  a <- nearest_stop(crashes$lat, crashes$lon, stops)                      # Distance to any stop, terminal, or junction
  j <- nearest_stop(crashes$lat, crashes$lon, stops[stops$junction, ])    # Distance to junction stops and terminals only
  crashes$d_all    <- a$dist
  crashes$stop_all <- a$id
  crashes$d_jn     <- j$dist
  crashes$stop_jn  <- j$id
  list(crashes = crashes, stops = stops)
}


if (sys.nframe() == 0 || identical(Sys.getenv("RUN_CLEAN"), "1")) {
  built <- build_crash_data()
  dir.create("data", showWarnings = FALSE)
  saveRDS(built$crashes, "data/crashes_all.rds")
  saveRDS(built$stops, "data/busstops_clean.rds")
  message("Saved ", nrow(built$crashes), " crashes (", sum(built$crashes$severity == "Fatal"), " fatal) and ",
          nrow(built$stops), " bus stops to data/")
}  # Execute full data cleaning pipeline and serialize RDS files when run directly