# ------------------------------------------------------------------
# deploy.R - publish the Chapter 2 and Explore dashboards to shinyapps.io
#
# Run from the project folder (the one that holds app_ch2.R, app_explore.R,
# R/ and data/):  setwd() to it first, then  source("deploy.R")
#
# ONE-OFF SETUP:
#   install.packages("rsconnect")
#   rsconnect::setAccountInfo(name = "datavizards-unsw", token = "<TOKEN>", secret = "<SECRET>")
#
# Live URLs:
#   https://datavizards-unsw.shinyapps.io/chapter2-when-where/
#   https://datavizards-unsw.shinyapps.io/explore-fatal-vs-nonfatal/
# ------------------------------------------------------------------
ACCOUNT <- "datavizards-unsw"          # must match the name in setAccountInfo(); check with rsconnect::accounts()
DEPLOY  <- c("chapter2", "explore")    # which apps to publish; use just one of them to deploy only that app

# Work from the folder that holds this script (the project folder), wherever R is currently pointing.
script_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL)   # source("deploy.R")
if (is.null(script_dir) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
  script_dir <- tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) NULL)  # run from the editor
if (!is.null(script_dir) && file.exists(file.path(script_dir, "app_ch2.R"))) setwd(script_dir)
if (!file.exists("app_ch2.R"))
  stop("Cannot find app_ch2.R in ", getwd(), ".\n",
       "Put deploy.R in the project folder (next to app_ch2.R, R/ and data/), or run setwd(\"<that folder>\") first.", call. = FALSE)
message("Deploying from: ", getwd())

# Both apps read cleaned .rds files, so the raw police spreadsheet is NOT uploaded.
# data/crashes_ch2.rds is created the first time app_ch2.R runs locally (delete it to rebuild).
# Explore uses data/accidents.rds if you have it, otherwise it falls back to crashes_ch2.rds.
# data/bus_stops.xlsx is optional: without it Explore simply leaves out the bus-stop section.
APPS <- list(
  chapter2 = list(
    name       = "chapter2-when-where",
    title      = "Kolkata Road Crashes - Ch2 When & Where",
    app_file   = "app_ch2.R",
    r_files    = c("R/clean_data.R", "R/clean_fatal.R", "R/shared_theme.R"),
    data_files = "data/crashes_ch2.rds",
    optional   = character()
  ),
  explore = list(
    name       = "explore-fatal-vs-nonfatal",
    title      = "Kolkata Road Crashes - Explore",
    app_file   = "app_explore.R",
    r_files    = c("R/clean_data.R", "R/clean_fatal.R", "R/bus_stops.R", "R/shared_theme.R"),
    data_files = if (file.exists("data/accidents.rds")) "data/accidents.rds" else "data/crashes_ch2.rds",
    optional   = "data/bus_stops.xlsx"
  )
)

if (!requireNamespace("rsconnect", quietly = TRUE)) stop("Install rsconnect first: install.packages(\"rsconnect\")", call. = FALSE)
if (!ACCOUNT %in% rsconnect::accounts()$name)
  stop("Account '", ACCOUNT, "' is not set up. Run rsconnect::setAccountInfo(...) first (see the top of this file).", call. = FALSE)
stopifnot(all(DEPLOY %in% names(APPS)))

# shinyapps.io deploys a folder whose entry file is app.R, so stage a clean copy of each app.
stage <- function(app) {
  required <- c(app$app_file, app$r_files, app$data_files)
  missing  <- required[!file.exists(required)]
  if (length(missing)) stop("Missing files: ", paste(missing, collapse = ", "), call. = FALSE)
  
  optional <- app$optional[file.exists(app$optional)]
  if (length(app$optional) && !length(optional))
    message("Note: ", paste(app$optional, collapse = ", "), " not found - ", app$name, " will be deployed without it.")
  
  dir <- file.path("deploy", app$name)
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "R"), recursive = TRUE)
  dir.create(file.path(dir, "data"), recursive = TRUE)
  ok <- c(file.copy(app$app_file, file.path(dir, "app.R")),
          file.copy(app$r_files, file.path(dir, "R")),
          file.copy(c(app$data_files, optional), file.path(dir, "data")))
  if (!all(ok)) stop("Could not copy all files into ", dir, call. = FALSE)
  dir
}

result <- character()
for (key in DEPLOY) {
  app <- APPS[[key]]
  message("\n=== Deploying ", app$name, " ===")
  result[[key]] <- tryCatch({
    dir <- stage(app)
    rsconnect::deployApp(dir, appName = app$name, appTitle = app$title,
                         account = ACCOUNT, forceUpdate = TRUE, launch.browser = FALSE)
    paste0("https://", ACCOUNT, ".shinyapps.io/", app$name, "/")
  }, error = function(e) paste("FAILED -", conditionMessage(e)))
}

message("\n--- Summary ---")
for (key in names(result)) message(sprintf("%-9s %s", key, result[[key]]))