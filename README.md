# Data Wizards — Kolkata Road Crashes Dashboard (2021–2023)

## Project Structure

```text
Data Wizards/
├── data/
│   ├── bus_stops.xlsx
│   ├── ch4_crashes_clean.rds
│   ├── crashes_ch2.rds
│   ├── data.xlsx
│   ├── fatal_clean.csv
│   └── nonfatal_slim.csv
├── R/
│   ├── bus_stops.R
│   ├── clean_data.R
│   ├── clean_fatal.R
│   └── shared_theme.R
├── .RData
├── .Rhistory
├── app_ch2.R
├── app_ch3.R
├── app_ch4.R
└── app_explore.R

```

---

## R-Related Files & What They Do

### Root R Scripts (Shiny Apps)

* `app_ch2.R`: Chapter 2 R Shiny app ("The Volume Problem") — explores hourly/monthly crash trends, station-hour heatmaps, and station/point maps for Kolkata crashes (2021–2023).
* `app_ch3.R`: Chapter 3 R Shiny app ("The Severity Problem") — analyzes fatal crashes using an age-sex pyramid, victim-vs-vehicle grid, day-vs-night comparison/fatality rates, and two-wheeler helmet waffle chart.
* `app_ch4.R`: Chapter 4 R Shiny app ("The main culprit: the bus") — examines bus crash proximity to bus stops via a Leaflet map, cumulative distance curve, top stops chart, victim breakdown, and fatal vehicle context.
* `app_explore.R`: Side-by-side fatal vs. non-fatal exploration Shiny app — compares severity distributions, lethality rates, 24-hour profiles, indexed yearly trends, a summary table, Leaflet cluster/point maps, and bus-stop proximity metrics across filters.

### `R/` Helper Scripts

* `R/clean_data.R`: Parses hours, dates, and Kolkata lat/lon coordinates, and standardizes vehicle groups, road user groups, police station names, and non-fatal crash sheets.
* `R/clean_fatal.R`: Cleans fatal crash sheets (including victim demographics, helmet status, and station names extracted from case references) and builds/saves combined crash and bus stop datasets.
* `R/bus_stops.R`: Loads and cleans Kolkata bus stop coordinates, formats stop names, calculates Haversine distances, and adds nearest stop and junction distances to crashes.
* `R/shared_theme.R`: Defines shared color constants, `bslib` theme, CSS styling, and helper functions (`fmt`, `tidy`, `chart`, `strip_item`) across dashboard chapters.

### R Workspace & Cached Data Files

* `.RData` & `.Rhistory`: Saved R workspace objects and command history from R sessions.
* `data/ch4_crashes_clean.rds` & `data/crashes_ch2.rds`: Cached R serialized data objects used for faster loading in the Chapter 4 and Chapter 2 Shiny apps.

### Non-R Data Files (`data/`)

* `bus_stops.xlsx`, `data.xlsx`, `fatal_clean.csv`, and `nonfatal_slim.csv` are raw/tabular Excel and CSV datasets read by the R scripts.



---

## App File Dependencies

Each R Shiny app depends on these files in the project:

* `app_ch2.R` = (`R/clean_data.R`, `R/clean_fatal.R`, `R/shared_theme.R`, `data/crashes_ch2.rds`, `data/data.xlsx`)
* `app_ch3.R` = (`R/shared_theme.R`, `R/clean_fatal.R`, `data/fatal_clean.csv`, `data/nonfatal_slim.csv`)
* `app_ch4.R` = (`R/clean_data.R`, `R/clean_fatal.R`, `R/bus_stops.R`, `R/shared_theme.R`, `data/data.xlsx`, `data/bus_stops.xlsx`, `data/ch4_crashes_clean.rds`)
* `app_explore.R` = (`R/clean_data.R`, `R/clean_fatal.R`, `R/bus_stops.R`, `R/shared_theme.R`, `data/accidents.rds`, `data/crashes_ch2.rds`, `data/data.xlsx`, `data/bus_stops.xlsx`)****
