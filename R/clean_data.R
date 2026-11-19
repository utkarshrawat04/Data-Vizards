# Load required packages for Excel ingestion, data manipulation, and string cleaning
suppressPackageStartupMessages({
  library(readxl); library(dplyr); library(stringr); library(tibble)
})

KOL_LAT <- c(22.40, 22.75)                 # Valid latitude bounding box for Kolkata
KOL_LON <- c(88.20, 88.55)                 # Valid longitude bounding box for Kolkata

# Extract integer hour (0-23) from POSIXct timestamps, numeric values, or mixed text strings
parse_hour <- function(v) {
  if (is.null(v) || length(v) == 0 || is.na(v)[1]) return(NA_integer_)
  h <- NA_real_
  if (inherits(v, "POSIXct")) {
    secs <- as.numeric(format(v, "%H")) * 3600 + as.numeric(format(v, "%M")) * 60 +
      as.numeric(format(v, "%S"))
    if (as.numeric(format(v, "%Y")) <= 1899 ||
        (format(v, "%Y-%m-%d") == "1899-12-31")) {
      h <- floor(secs / 3600)
    } else {
      h <- as.numeric(format(v, "%d"))
    }
  } else if (is.numeric(v)) {
    h <- if (v >= 100 && v == floor(v)) floor(v / 100) else floor(v + 1e-6)
  } else {
    s <- str_to_lower(as.character(v))
    s <- str_replace_all(s, "hrs?\\.?|hours?", "")
    s <- str_squish(s)
    m <- str_match(s, "^(\\d{1,2})\\s*[:.]\\s*(\\d{1,2})")
    if (!is.na(m[1, 2])) {
      h <- as.numeric(m[1, 2])
    } else {
      m <- str_match(s, "^(\\d{3,4})$")
      if (!is.na(m[1, 2])) h <- floor(as.numeric(m[1, 2]) / 100)
      else {
        m <- str_match(s, "^(\\d{1,2})$")
        if (!is.na(m[1, 2])) h <- as.numeric(m[1, 2])
      }
    }
  }
  if (is.na(h) || h < 0 || h > 23) NA_integer_ else as.integer(h)
}

# Parse Date objects from POSIXct timestamps, Excel serial numbers, or DD/MM/YYYY strings
parse_date <- function(v) {
  if (is.null(v) || length(v) == 0 || is.na(v)[1]) return(as.Date(NA))
  if (inherits(v, "POSIXct")) return(as.Date(v))
  if (is.numeric(v)) return(as.Date(v, origin = "1899-12-30"))
  s <- as.character(v)
  m <- str_match(s, "(\\d{1,2})\\s*[.\\-/]\\s*(\\d{1,2})\\s*[.\\-/]\\s*(\\d{2,4})")
  if (is.na(m[1, 1])) return(as.Date(NA))
  d <- as.integer(m[1, 2]); mo <- as.integer(m[1, 3]); y <- as.integer(m[1, 4])
  if (y < 100) y <- y + 2000
  suppressWarnings(as.Date(sprintf("%04d-%02d-%02d", y, mo, d), format = "%Y-%m-%d"))
}

# Extract decimal latitude and longitude coordinates from DMS or malformed decimal strings
parse_latlon <- function(s) {
  out <- c(lat = NA_real_, lon = NA_real_)
  if (is.na(s)) return(out)
  s <- as.character(s)
  if (str_detect(s, "°")) {
    n <- as.numeric(str_extract_all(s, "\\d+\\.?\\d*")[[1]])
    if (length(n) >= 6) {
      lat <- n[1] + n[2] / 60 + n[3] / 3600
      lon <- n[4] + n[5] / 60 + n[6] / 3600
      cand <- c(lat, lon)
    } else return(out)
  } else {
    s2 <- str_replace_all(s, "(\\d{2}),(\\d{3,})", "\\1.\\2")
    s2 <- str_replace_all(s2, "(\\d{2})\\.\\s+(\\d)", "\\1.\\2")
    s2 <- str_replace_all(s2, "(?<![\\d.])(22|88)(\\d{5,})(?![\\d.])", "\\1.\\2")
    cand <- as.numeric(str_extract_all(s2, "\\d{2}\\.\\d+")[[1]])
  }
  lat <- cand[cand >= KOL_LAT[1] & cand <= KOL_LAT[2]][1]
  lon <- cand[cand >= KOL_LON[1] & cand <= KOL_LON[2]][1]
  if (!is.na(lat) && !is.na(lon)) out[] <- c(lat, lon)
  out
}

# Map raw offending vehicle descriptions into eight standardized vehicle categories
vehicle_group <- function(x) {
  s <- str_to_upper(str_squish(coalesce(x, "")))
  case_when(
    s == "" | s %in% c("-", "--", "UN", "UK", "NA", "N/A") |
      str_detect(s, "UNKNOWN|UNIDENTIF|NOT KNOWN|^UNK|^UN/|ESCAPED") ~ "Unknown",
    str_detect(s, "BUS|\\bBAS\\b|CSTC|WBTC|SBSTC|NBSTC") ~ "Bus",
    str_detect(s, "TAXI|TEXI|\\bCAB\\b|UBER|\\bOLA\\b|\\bCAR\\b|\\bP/?\\s?C\\b|PVT\\.?\\s?CAR|SUMO|JEEP|SUV|INNOVA|SCORPIO|TAVERA|BOLERO|ERTIGA|SWIFT|ALTO|MARUTI|FOUR WHEELER|4 ?WHEELER|TRAVELLER") ~ "Car / Taxi",
    str_detect(s, "LORRY|TRUCK|TRACK|HGV|LGV|MGV|L\\.?G\\.?V|TRAIL[EO]R|TATA ?AC|407|709|TANKER|DUMPER|DRIAMPER|CONTAINER|GOOD|PICK ?UP|MAGIC|MAXX|TEMPO|MATADOR|CHOTA|\\bVAN\\b") ~ "Truck / Goods",
    str_detect(s, "MOTOR ?CYC|\\bM/?C\\b|^MC|M/CYCLE|SCOOT|TWO ?WHEEL|2 ?WHEEL|BIKE|MOPED|BULLET|ENFIELD|PUJSAR|PULSAR|ACTIVA|\\b2W\\b") ~ "Two-wheeler",
    str_detect(s, "AUTO|RICKSHAW|RIKSHA|TOTO|E-?RICK|TUKTUK") ~ "Auto / Rickshaw",
    str_detect(s, "CYCLE|BI-?CYC") ~ "Bicycle",
    TRUE ~ "Other"
  )
}

# Map raw victim road user descriptions into eight standardized road user categories
road_user_group <- function(x) {
  s <- str_to_upper(str_squish(coalesce(x, "")))
  case_when(
    s == "" | s %in% c("-", "--", "NA", "N/A", "UNKNOWN") ~ "Unknown",
    str_detect(s, "PULL|LABOU?R|WORKER|POLICE|CONSTABLE|HOME GUARD|VOLUNTEER|OTHER") ~ "Other",
    str_detect(s, "^P[A-Z]*D[A-Z]*S?T?R|^PED|PEDES|PADES|PEDIS|PEDAS|WALK") ~ "Pedestrian",
    str_detect(s, "PIL+[IO]+N|CO-?RIDER") ~ "Pillion rider",
    str_detect(s, "MOTO|M/?CYC|M/?C\\b|^MC|SCOOT|P/RIDER|^RIDER|BIKE|TWO ?WHEEL") ~ "Rider",
    str_detect(s, "CYC|CYL|CLIST|^BI[ -]?C") ~ "Cyclist",
    str_detect(s, "P[AE]SS|BOARD") ~ "Passenger",
    str_detect(s, "DRIVER|OWNER|CONDUCTOR|CONTUCTOR|HELPER|KHALASI") ~ "Driver / Owner",
    TRUE ~ "Other"
  )
}

# Standardize police station names by stripping division codes and correcting spelling variants
station_clean <- function(x) {
  s <- str_to_upper(str_squish(coalesce(x, "")))
  s <- str_replace_all(s, "\\.", " ")
  s <- str_replace(s, "\\s*\\(.*$", "")
  s <- str_replace(s, "\\s*-?\\s*P\\s?S\\b.*$", "")
  s <- str_replace(s, "\\s*-\\s*(CD|SD|ED|PD|ND|SED|SWD|SSD|NSD|EZ|TD|HQ|DD)\\s*$", "")
  s <- str_replace(s, "\\s*(TRAFFIC )?GUARD$", " TRAFFIC GUARD")
  s <- str_squish(s)
  fix <- c("AIPORE" = "ALIPORE", "ALLIPORE" = "ALIPORE",
           "GARDENREACH" = "GARDEN REACH", "GARDEN-REACH" = "GARDEN REACH",
           "BOWBAZAR" = "BOWBAZAR", "BOW BAZAR" = "BOWBAZAR",
           "BURRABAZAR" = "BURRABAZAR", "BURRA BAZAR" = "BURRABAZAR",
           "TOLLYGUNGE" = "TOLLYGUNGE", "TOLLYGANJ" = "TOLLYGUNGE",
           "BENIAPUKUR" = "BENIAPUKUR", "BENIAPUKUR " = "BENIAPUKUR",
           "SHAKESPEARE SARANI" = "SHAKESPEARE SARANI",
           "SAKESPEARE SARANI" = "SHAKESPEARE SARANI",
           "PRAGATI MAIDAN" = "PRAGATI MAIDAN",
           "THAKURPUKUR" = "THAKURPUKUR", "THAKUR PUKUR" = "THAKURPUKUR",
           "NEW MARKET" = "NEW MARKET", "NEWMARKET" = "NEW MARKET",
           "NEW ALIPORE" = "NEW ALIPORE", "NEW ALIPORE " = "NEW ALIPORE")
  s <- str_replace(s, "[ ,/_-]+(ND ?& ?NSD|PS_?SED|PSSSD|A- ?ESD|ND|SD|ED|CD|PD|ESD|SSD|SWD|NSD|SED)$", "")
  s <- str_replace(s, "[ ,]+$", "")
  s <- str_replace(s, "^(AMHERST ST|AMHERST STREET).*$", "AMHERST STREET")
  s <- str_replace(s, "^(BHOWANIPORE|BHOWNAIPORE).*$", "BHOWANIPORE")
  s <- str_replace(s, "^(CHARU ?MARKET|CHAUMARKET).*$", "CHARU MARKET")
  s <- str_replace(s, "^GIRISH PARK.*$", "GIRISH PARK")
  s <- str_replace(s, "^(BURRABZAR|BURRABAZAR)$", "BURRABZAR")
  s <- str_replace(s, "^(BURTALLA|BURTOLLA)$", "BURTOLLA")
  s <- str_replace(s, "^(CHETYLA|CHETLA)$", "CHETLA")
  s <- str_replace(s, "^(CHITPORE|CHITPUR)$", "CHITPORE")
  s <- str_replace(s, "^(GOLF GREEEN|GOLFGREEN|GOLF GREEN)$", "GOLF GREEN")
  s <- str_replace(s, "^(HARESTREET|HARE STREET)$", "HARE STREET")
  s <- str_replace(s, "^(JORABAGAN|JOTABAGAN)$", "JORABAGAN")
  s <- str_replace(s, "^(KLC|KOLKATA LEATHER COMPLEX)$", "KOLKATA LEATHER COMPLEX")
  s <- str_replace(s, "^(KSB)$", "KASBA")
  s <- str_replace(s, "^(NP|NORTH PORT.*)$", "NORTH PORT")
  s <- str_replace(s, "^SP$", "SOUTH PORT")
  s <- str_replace(s, "^PARK STR?EERT$|^PARK ST$", "PARK STREET")
  s <- str_replace(s, "^(POSTA|POASTA)$", "POSTA")
  s <- str_replace(s, "^TAGRA$", "TANGRA")
  s <- str_replace(s, "^(TARATALA).*$", "TARATALA")
  s <- str_replace(s, "^(TOPSIA).*$", "TOPSIA")
  s <- str_replace(s, "^(ULTADANG|ULTADANGA).*$", "ULTADANGA")
  s <- str_replace(s, "^(WEST PORT).*$", "WEST PORT")
  s <- str_replace(s, "^(BANSDRONI).*$", "BANSDRONI")
  s <- str_replace(s, "^(ENTALLY).*$", "ENTALLY")
  s <- ifelse(s %in% names(fix), unname(fix[s]), s)
  s <- ifelse(s == "", NA_character_, str_to_title(s))
  s <- ifelse(s == "Kolkata Leather Complex", "Kolkata Leather Complex", s)
  s
}

MONTHS <- c("JANUARY","FEBRUARY","MARCH","APRIL","MAY","JUNE","JULY",
            "AUGUST","SEPTEMBER","OCTOBER","NOVEMBER","DECEMBER") # Month lookup vector for sheet text parsing

# Load and standardize non-fatal crash sheets across specified years into one tidy tibble
load_nonfatal <- function(path = "data/data.xlsx", years = 2021:2023) {
  all <- lapply(years, function(y) {
    sheet <- paste0("Non Fatal - ", y)
    ct <- rep("text", 19); ct[c(8, 9)] <- "list"
    raw <- suppressMessages(
      read_excel(path, sheet = sheet, col_names = FALSE, skip = 3, col_types = ct))
    raw <- raw[!is.na(raw[[1]]) | !is.na(raw[[3]]), ] # Drop empty spacer rows
    
    hr   <- vapply(raw[[9]], parse_hour, integer(1))
    dts  <- do.call(c, lapply(raw[[8]], parse_date))
    mon_txt <- str_to_upper(str_replace_all(coalesce(raw[[2]], ""), "[^A-Za-z]", ""))
    mon_col <- match(mon_txt, MONTHS)
    date_ok <- !is.na(dts) & as.integer(format(dts, "%Y")) == y
    month   <- ifelse(date_ok, as.integer(format(dts, "%m")), mon_col)
    ll <- t(vapply(raw[[11]], parse_latlon, numeric(2)))
    
    tibble(
      year          = y,
      month         = as.integer(month),
      date          = as.Date(ifelse(date_ok, dts, NA), origin = "1970-01-01"),
      hour          = hr,
      station       = station_clean(raw[[3]]),
      place         = str_squish(raw[[10]]),
      vehicle_raw   = str_squish(raw[[13]]),
      vehicle       = vehicle_group(raw[[13]]),
      road_user_raw = str_squish(raw[[19]]),
      road_user     = road_user_group(raw[[19]]),
      lat           = ll[, 1],
      lon           = ll[, 2]
    )
  })
  bind_rows(all) %>%
    filter(!is.na(month)) %>%
    mutate(
      month_start = as.Date(sprintf("%d-%02d-01", year, month)),
      severity    = "Non-fatal",
      crash_id    = row_number()
    )
}