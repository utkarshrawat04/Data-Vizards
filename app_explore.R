library(shiny)
library(bslib)
library(dplyr)
library(leaflet)
library(plotly)
library(htmltools)

source("R/clean_data.R")
source("R/clean_fatal.R")
source("R/bus_stops.R")
source("R/shared_theme.R")                  # Load helper scripts, bus stops, and shared theme

pct <- function(a, b) round(100 * a / b, 1) # Calculate percentage to one decimal place

d <- if (file.exists("data/accidents.rds")) {
  readRDS("data/accidents.rds")
} else if (file.exists("data/crashes_ch2.rds")) {
  readRDS("data/crashes_ch2.rds")
} else {
  message("Building accidents dataset from source sheets...")
  df <- bind_rows(load_nonfatal("data/data.xlsx"), load_fatal("data/data.xlsx")) |>
    mutate(crash_id = row_number())
  dir.create("data", showWarnings = FALSE)
  saveRDS(df, "data/accidents.rds")
  df
}                                         # Load cached crash data or rebuild from Excel if missing

# Validate required columns and normalize severity/period fields
need_cols <- c("severity", "year", "hour", "victim", "offender", "lat", "lon")
if (!all(need_cols %in% names(d))) {
  if ("road_user" %in% names(d) && !"victim" %in% names(d)) d$victim <- d$road_user
  if ("vehicle" %in% names(d) && !"offender" %in% names(d)) d$offender <- d$vehicle
}

if (!"period" %in% names(d)) d$period <- ifelse(is.na(d$hour), NA, ifelse(d$hour >= 6 & d$hour < 18, "Day", "Night"))
d$severity <- ifelse(tolower(as.character(d$severity)) %in% c("fatal", "f"), "Fatal", "Non-fatal")

sev_fill <- c("Fatal" = MAROON,    "Non-fatal" = YELLOW) # Fill colors by severity
sev_line <- c("Fatal" = "#3D0F28", "Non-fatal" = "#8A6200") # Outline colors by severity

# Indexed year-over-year trend plot (start year = 100)
trend_plot <- function(df) {
  y <- df |> count(severity, year) |> group_by(severity) |> arrange(year) |>
    mutate(idx = if (first(n) > 0) 100 * n / first(n) else NA_real_) |> ungroup() |>
    filter(!is.na(idx), idx > 0)
  
  base_year <- min(y$year, na.rm = TRUE)
  lo <- min(y$idx, 100); hi <- max(y$idx, 100); pad <- max(5, 0.2 * (hi - lo))
  p <- plot_ly()
  for (s in intersect(names(sev_fill), unique(y$severity))) {
    x <- filter(y, severity == s)
    p <- add_trace(p, data = x, x = ~as.character(year), y = ~idx, type = "scatter",
                   mode = "lines+markers", name = s, customdata = ~n,
                   line = list(color = sev_fill[[s]], width = 3),
                   marker = list(color = sev_fill[[s]], size = 8, line = list(color = sev_line[[s]], width = 1)),
                   hovertemplate = "%{x}: %{customdata} crashes (index %{y:.0f})<extra>%{fullData.name}</extra>")
  }
  p |> tidy(legend = list(orientation = "h", x = 0, y = 1.12), margin = list(t = 10),
            xaxis = list(title = "", type = "category", gridcolor = GRID),
            yaxis = list(title = paste0("Crashes (", base_year, " = 100)"), range = c(lo - pad, hi + pad),
                         gridcolor = GRID, zeroline = FALSE),
            shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = 100, y1 = 100,
                               line = list(color = "#999999", dash = "dot", width = 1))))
}

# Time bands and top-share summary helper
TIME_BANDS <- c("Night (00-05)", "Morning (06-11)", "Afternoon (12-17)", "Evening (18-23)")
time_band  <- function(h) cut(h, breaks = c(-1, 5, 11, 17, 23), labels = TIME_BANDS)  

top_share <- function(x, k = 1) {
  x <- x[!is.na(x)]
  none <- list(label = NA_character_, share = NA_real_, n = 0L)
  if (!length(x)) return(none)
  t <- sort(table(x), decreasing = TRUE)
  if (k > length(t)) return(none)
  list(label = names(t)[k], share = round(100 * t[[k]] / length(x), 1), n = as.integer(t[[k]]))
}

# Fatal-vs-non-fatal comparison table and chart builders
ORDERED <- c("year", "hour", "time_band", "age_group", "month", "stop_band") # Variables with natural ordering

cat_levels <- function(x, col) {
  u <- unique(x[!is.na(x)])
  if (col == "month") return(u[order(match(toupper(substr(as.character(u), 1, 3)), toupper(month.abb)))])
  if (is.factor(x)) return(levels(x)[levels(x) %in% as.character(u)])
  sort(u)
}                                         # Extract ordered factor/category levels

compare_tbl <- function(df, col) {
  x <- df |> filter(!is.na(.data[[col]])) |>
    mutate(cat = as.character(.data[[col]])) |> count(severity, cat, name = "n")
  if (!nrow(x)) return(NULL)
  cats <- unique(x$cat)
  g <- expand.grid(severity = names(sev_fill), cat = cats, stringsAsFactors = FALSE) |>
    left_join(mutate(x, severity = as.character(severity)), by = c("severity", "cat"))
  g$n[is.na(g$n)] <- 0L
  g <- g |> group_by(severity) |> mutate(share = if (sum(n) > 0) round(100 * n / sum(n), 1) else 0) |> ungroup()
  lv <- if (col %in% ORDERED) as.character(cat_levels(df[[col]], col)) else {
    tot <- tapply(g$n, g$cat, sum); names(sort(tot, decreasing = TRUE))}
  lv <- lv[lv %in% cats]
  g$cat <- factor(g$cat, levels = lv)
  g$severity <- factor(g$severity, levels = names(sev_fill))
  g
}                                         # Compute counts and within-severity shares by category

compare_bar <- function(df, col, mode = "share", top = 12) {
  g <- compare_tbl(df, col); if (is.null(g)) return(NULL)
  lv <- levels(g$cat)
  if (!(col %in% ORDERED) && length(lv) > top) {
    lv <- lv[seq_len(top)]; g <- filter(g, cat %in% lv); g$cat <- factor(as.character(g$cat), levels = lv)
  }
  g$val <- if (mode == "share") g$share else g$n
  g$hov <- paste0(g$n, " crashes (", g$share, "% of ", tolower(as.character(g$severity)), " crashes)")
  vt <- if (mode == "share") "% of that severity's crashes" else "Count of crashes"
  if (col %in% ORDERED) {
    plot_ly(g, x = ~cat, y = ~val, color = ~severity, colors = sev_fill, type = "bar",
            customdata = ~hov, hovertemplate = "%{x}: %{customdata}<extra>%{fullData.name}</extra>") |>
      tidy(barmode = "group", xaxis = list(title = "", categoryorder = "array", categoryarray = lv),
           yaxis = list(title = vt, gridcolor = GRID, rangemode = "tozero"),
           legend = list(orientation = "h", x = 0, y = 1.12), margin = list(t = 10))
  } else {
    plot_ly(g, x = ~val, y = ~cat, color = ~severity, colors = sev_fill, type = "bar", orientation = "h",
            customdata = ~hov, hovertemplate = "%{y}: %{customdata}<extra>%{fullData.name}</extra>") |>
      tidy(barmode = "group", yaxis = list(title = "", categoryorder = "array", categoryarray = rev(lv)),
           xaxis = list(title = vt, gridcolor = GRID, rangemode = "tozero"),
           legend = list(orientation = "h", x = 0, y = 1.12), margin = list(l = 10, t = 10))
  }
}                                         # Grouped bar chart comparing fatal vs non-fatal distributions

rate_bar <- function(df, col, min_n = 5) {
  g <- compare_tbl(df, col); if (is.null(g)) return(NULL)
  w <- g |> mutate(cat = as.character(cat)) |> group_by(cat) |>
    summarise(fatal = sum(n[severity == "Fatal"]), total = sum(n), .groups = "drop") |>
    filter(total >= min_n) |> mutate(rate = round(100 * fatal / total, 1))
  if (!nrow(w)) return(NULL)
  lv <- levels(g$cat); lv <- lv[lv %in% w$cat]
  w$cat <- factor(w$cat, levels = lv)
  overall <- round(100 * sum(w$fatal) / sum(w$total), 1)
  w$hov <- paste0(w$fatal, " fatal of ", w$total, " crashes")
  ttl <- paste0("Fatal share of crashes (%) - dashed line = overall ", overall, "%")
  if (col %in% ORDERED) {
    plot_ly(w, x = ~cat, y = ~rate, type = "bar", marker = list(color = sev_fill[["Fatal"]], line = list(color = sev_line[["Fatal"]], width = 1)),
            text = ~paste0(rate, "%"), textposition = "outside", cliponaxis = FALSE,
            customdata = ~hov, hovertemplate = "%{x}: %{y}% (%{customdata})<extra></extra>") |>
      tidy(xaxis = list(title = "", categoryorder = "array", categoryarray = lv),
           yaxis = list(title = ttl, gridcolor = GRID, rangemode = "tozero"), margin = list(t = 20),
           shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = overall, y1 = overall,
                              line = list(color = "#555555", dash = "dash", width = 1))))
  } else {
    plot_ly(w, x = ~rate, y = ~cat, type = "bar", orientation = "h",
            marker = list(color = sev_fill[["Fatal"]], line = list(color = sev_line[["Fatal"]], width = 1)),
            text = ~paste0(rate, "%"), textposition = "outside", cliponaxis = FALSE,
            customdata = ~hov, hovertemplate = "%{y}: %{x}% (%{customdata})<extra></extra>") |>
      tidy(yaxis = list(title = "", categoryorder = "array", categoryarray = rev(lv)),
           xaxis = list(title = ttl, gridcolor = GRID, rangemode = "tozero"), margin = list(l = 10, r = 40),
           shapes = list(list(type = "line", yref = "paper", y0 = 0, y1 = 1, x0 = overall, x1 = overall,
                              line = list(color = "#555555", dash = "dash", width = 1))))
  }
}                                         # Lethality rate bar chart with overall average benchmark

hour_compare <- function(df, mode = "share") {
  h <- df |> filter(!is.na(hour)) |> count(severity, hour)
  p <- plot_ly(type = "scatter", mode = "lines+markers")
  for (s in names(sev_fill)) {
    full <- data.frame(hour = 0:23) |> left_join(filter(h, severity == s), by = "hour")
    full$n[is.na(full$n)] <- 0
    full$share <- if (sum(full$n) > 0) round(100 * full$n / sum(full$n), 1) else 0
    p <- add_trace(p, x = full$hour, y = if (mode == "share") full$share else full$n, name = s,
                   customdata = full$n,
                   line = list(color = sev_fill[[s]], width = 3), marker = list(color = sev_fill[[s]], size = 6),
                   hovertemplate = "%{x}:00 - %{y} (%{customdata} crashes)<extra>%{fullData.name}</extra>")
  }
  p |> tidy(xaxis = list(title = "Hour of day", tickmode = "linear", dtick = 2, gridcolor = GRID),
            yaxis = list(title = if (mode == "share") "% of that severity's crashes" else "Count of crashes",
                         rangemode = "tozero", gridcolor = GRID),
            legend = list(orientation = "h", x = 0, y = 1.12), margin = list(t = 10))
}                                         # 24-hour line profile comparing fatal and non-fatal timing

# Leaflet map markers, legends, and cluster builders
diamond_svg <- sprintf("<svg xmlns='http://www.w3.org/2000/svg' width='18' height='18'><polygon points='9,1 17,9 9,17 1,9' fill='%s' stroke='%s' stroke-width='1.5'/></svg>",
                       sev_fill[["Fatal"]], sev_line[["Fatal"]])
fatal_icon <- makeIcon(paste0("data:image/svg+xml;charset=UTF-8,", URLencode(diamond_svg, reserved = TRUE)), iconWidth = 15, iconHeight = 15)
STOP_FILL <- "#8C8C8C"; STOP_LINE <- "#3F3F3F"
legend_html <- function(stops = FALSE) paste0("<div style='background:#fff;padding:6px 10px;border-radius:4px;line-height:1.6'><b>Severity</b><br>",
                                              "<span style='color:", sev_fill[["Fatal"]], "'>&#9670;</span> Fatal<br>",
                                              "<span style='color:", sev_fill[["Non-fatal"]], "'>&#9679;</span> Non-fatal",
                                              if (stops) paste0("<br><span style='color:", STOP_LINE, "'>&#9679;</span> Bus stop"), "</div>")

base_map <- function(legend = TRUE, stops = FALSE) {
  m <- leaflet() |>
    addProviderTiles(providers$Esri.WorldGrayCanvas) |>
    setView(lng = 88.35, lat = 22.57, zoom = 12)
  if (legend) m <- addControl(m, HTML(legend_html(stops)), position = "bottomright")
  m
}                                         # Initialize Leaflet base map centered on Kolkata

popup_txt <- function(x) paste0("<b>", x$severity, "</b> (", x$year, ")<br>Victim: ", x$victim,
                                "<br>Vehicle: ", x$offender, "<br>",
                                htmlEscape(ifelse(is.na(x$place), "", x$place)))

add_points <- function(map, df) {
  pts <- filter(df, !is.na(lat)); nf <- filter(pts, severity == "Non-fatal"); fa <- filter(pts, severity == "Fatal")
  if (nrow(nf)) map <- addCircleMarkers(map, data = nf, lng = ~lon, lat = ~lat, radius = 3, weight = 0.5,
                                        color = sev_line[["Non-fatal"]], fillColor = sev_fill[["Non-fatal"]],
                                        fillOpacity = 0.35, popup = popup_txt(nf))
  if (nrow(fa)) map <- addMarkers(map, data = fa, lng = ~lon, lat = ~lat, icon = fatal_icon, popup = popup_txt(fa))
  map
}                                         # Add individual crash markers to map

cluster_icon <- function(fill, border, txt) {
  JS(sprintf("function(cluster) {
    var n = cluster.getChildCount();
    var s = n < 10 ? 32 : (n < 100 ? 40 : 50);
    return L.divIcon({
      html: '<div style=\"background:%s;border:2px solid %s;color:%s;border-radius:50%%;width:' + s + 'px;height:' + s +
            'px;line-height:' + (s - 4) + 'px;text-align:center;font-weight:700;opacity:0.92\">' + n + '</div>',
      className: '', iconSize: L.point(s, s) });
  }", fill, border, txt))
}                                         # Custom JS cluster badge generator

add_clusters <- function(map, df) {
  pts <- filter(df, !is.na(lat)); nf <- filter(pts, severity == "Non-fatal"); fa <- filter(pts, severity == "Fatal")
  if (nrow(nf)) map <- addCircleMarkers(map, data = nf, lng = ~lon, lat = ~lat, radius = 5, weight = 1,
                                        color = sev_line[["Non-fatal"]], fillColor = sev_fill[["Non-fatal"]],
                                        fillOpacity = 0.6, popup = popup_txt(nf),
                                        clusterOptions = markerClusterOptions(
                                          showCoverageOnHover = FALSE, maxClusterRadius = 45,
                                          iconCreateFunction = cluster_icon(sev_fill[["Non-fatal"]], sev_line[["Non-fatal"]], sev_line[["Non-fatal"]])))
  if (nrow(fa)) map <- addMarkers(map, data = fa, lng = ~lon, lat = ~lat, icon = fatal_icon, popup = popup_txt(fa),
                                  clusterOptions = markerClusterOptions(
                                    showCoverageOnHover = FALSE, maxClusterRadius = 45,
                                    iconCreateFunction = cluster_icon(sev_fill[["Fatal"]], sev_line[["Fatal"]], "#ffffff")))
  map
}                                         # Add clustered crash markers separated by severity

# Bus stops data, proximity calculation, and stop charts
STOPS <- if (exists("load_stops") && file.exists("data/bus_stops.xlsx")) load_stops("data/bus_stops.xlsx") else NULL
HAS_STOPS <- !is.null(STOPS) && nrow(STOPS) > 0
STOP_BANDS <- c("0-50 m", "50-100 m", "100-200 m", "200-500 m", "500+ m")

nearest_stop <- function(lat, lon, stops) {
  n <- length(lat); dist <- rep(NA_real_, n); idx <- rep(NA_integer_, n)
  if (is.null(stops) || !nrow(stops)) return(list(dist = dist, idx = idx))
  ok <- which(!is.na(lat) & !is.na(lon) & lat > 22.35 & lat < 22.80 & lon > 88.15 & lon < 88.55)
  kx <- 111320 * cos(22.57 * pi / 180); ky <- 110574
  sx <- stops$stop_lon * kx; sy <- stops$stop_lat * ky
  for (i in ok) {
    dd <- (sx - lon[i] * kx)^2 + (sy - lat[i] * ky)^2
    j <- which.min(dd); dist[i] <- sqrt(dd[j]); idx[i] <- j
  }
  list(dist = dist, idx = idx)
}                                         # Compute straight-line distance (meters) to nearest bus stop

dist_curve <- function(df, rad) {
  x <- df |> filter(!is.na(stop_dist))
  if (!nrow(x)) return(NULL)
  grid <- seq(0, 500, by = 10)
  p <- plot_ly(type = "scatter", mode = "lines")
  for (s in names(sev_fill)) {
    ds <- x$stop_dist[x$severity == s]
    if (!length(ds)) next
    p <- add_trace(p, x = grid, y = vapply(grid, function(g) round(100 * mean(ds <= g), 1), numeric(1)), name = s,
                   customdata = vapply(grid, function(g) sum(ds <= g), numeric(1)),
                   line = list(color = sev_fill[[s]], width = 3),
                   hovertemplate = "Within %{x} m: %{y}% (%{customdata} crashes)<extra>%{fullData.name}</extra>")
  }
  p |> tidy(xaxis = list(title = "Distance to nearest bus stop (m)", gridcolor = GRID),
            yaxis = list(title = "% of crashes within this distance", range = c(0, 100), gridcolor = GRID),
            legend = list(orientation = "h", x = 0, y = 1.12), margin = list(t = 10),
            shapes = list(list(type = "line", x0 = rad, x1 = rad, yref = "paper", y0 = 0, y1 = 1,
                               line = list(color = "#555555", dash = "dash", width = 1))))
}                                         # Cumulative share of crashes within distance to bus stop

top_stops_bar <- function(df, rad, top = 15) {
  x <- df |> filter(!is.na(stop_dist), stop_dist <= rad, !is.na(stop_idx))
  if (!nrow(x)) return(NULL)
  g <- x |> count(stop_idx, stop_name, stop_kind, severity, name = "n")
  tot <- g |> group_by(stop_idx) |> summarise(total = sum(n), .groups = "drop") |> arrange(desc(total)) |> head(top)
  g <- g |> filter(stop_idx %in% tot$stop_idx)
  lab <- g |> distinct(stop_idx, stop_name, stop_kind)
  lab <- lab[match(tot$stop_idx, lab$stop_idx), ]
  lab$label <- make.unique(paste0(lab$stop_name, " (", tolower(lab$stop_kind), ")"), sep = " #")
  g <- left_join(g, lab[, c("stop_idx", "label")], by = "stop_idx")
  g$label <- factor(g$label, levels = rev(lab$label))
  g$severity <- factor(g$severity, levels = names(sev_fill))
  plot_ly(g, x = ~n, y = ~label, color = ~severity, colors = sev_fill, type = "bar", orientation = "h",
          hovertemplate = "%{y}: %{x} crashes<extra>%{fullData.name}</extra>") |>
    tidy(barmode = "stack", xaxis = list(title = paste0("Crashes within ", rad, " m of the stop"), gridcolor = GRID, rangemode = "tozero"),
         yaxis = list(title = "", automargin = TRUE), legend = list(orientation = "h", x = 0, y = 1.08), margin = list(l = 10, t = 10))
}                                         # Stacked bar chart of stops with highest nearby crash counts

# Derived columns and UI dropdown options
d$rid <- seq_len(nrow(d))
if (!"time_band" %in% names(d)) d$time_band <- time_band(d$hour)
if ("age" %in% names(d) && !"age_group" %in% names(d))
  d$age_group <- cut(suppressWarnings(as.numeric(d$age)), breaks = c(-1, 17, 29, 44, 59, 150),
                     labels = c("0-17", "18-29", "30-44", "45-59", "60+"))

OPT <- c(sex = "Sex", age_group = "Age group", helmet = "Helmet", accident_type = "Accident type",
         month = "Month", ps = "Police station", tp_guard = "TP guard")
OPT <- OPT[names(OPT) %in% names(d)]
OPT <- OPT[vapply(names(OPT), function(v) any(!is.na(d[[v]])), logical(1))]

BY_CHOICES <- c("Victim road user" = "victim", "Offending vehicle" = "offender", "Time band" = "time_band",
                "Hour of day" = "hour", "Day / Night" = "period", "Year" = "year", setNames(names(OPT), OPT))
if (HAS_STOPS) BY_CHOICES <- c(BY_CHOICES, "Near a bus stop (your radius)" = "near_stop", "Distance to nearest stop" = "stop_band",
                               "Nearest stop type" = "stop_kind", "Nearest stop is a junction" = "stop_junction")

sorted_off <- sort(unique(as.character(d$offender))) # Sorted offending vehicle levels
sorted_vic <- sort(unique(as.character(d$victim)))   # Sorted victim road user levels
yrs <- sort(unique(d$year))                          # Available dataset years

# Custom CSS for Explore layout, pill checkboxes, and loading overlays
explore_extra_css <- "
  .insight { font-size:17px; margin:2px 0 14px; }
  .tblwrap { max-height:340px; overflow:auto; }
  .tblwrap table { font-size:13px; }
  .tblwrap th { border-bottom:2px solid @MAROON@ !important; }
  .accordion-button { font-weight:700; color:@MAROON@; }
  /* tighter spacing around the segmented toggles in the sidebar */
  .seg { margin-bottom:10px; }
  .seg .shiny-input-container { display:block; width:100%; max-width:100%; margin:0; }
  .seg .shiny-label-null { display:none; }
  .accordion-button:not(.collapsed) { background:#F3EAF0; color:@MAROON@; box-shadow:none; }

  .recalculating { opacity:.35; }
  .panel .shiny-bound-output.recalculating { position:relative; }
  .panel .shiny-bound-output.recalculating::after { content:'Loading...'; position:absolute; top:45%; left:50%;
    transform:translateX(-50%); color:@MAROON@; font-weight:700; background:#fff; padding:2px 10px; border-radius:10px; }

  .pill-group .shiny-options-group { display:flex; flex-wrap:wrap; gap:6px; }
  .pill-group .checkbox, .pill-group .form-check { margin:0; padding:0; }
  .pill-group input[type=checkbox] { position:absolute; opacity:0; }
  .pill-group label { cursor:pointer; margin:0; }
  .pill-group label span { display:inline-block; padding:3px 12px; border-radius:16px; background:#F3EAF0; border:1px solid #D9CBD3; font-size:13px; }
  .pill-group input:checked + span { background:@MAROON@; border-color:#3D0F28; color:#fff; font-weight:700; }
  .pill-group input:focus-visible + span { outline:2px solid @YELLOW@; outline-offset:1px; }
"
explore_extra_css <- gsub("@MAROON@", MAROON, gsub("@YELLOW@", YELLOW, explore_extra_css, fixed = TRUE), fixed = TRUE)
explore_css <- paste0(app_css, explore_extra_css)

# UI component helpers (pill groups, optional filters, KPI cards)
pills <- function(id, label, choices) div(class = "pill-group", checkboxGroupInput(id, label, choices, selected = NULL))

opt_filters <- if (length(OPT)) {
  tagList(lapply(names(OPT), function(v)
    pills(paste0("ex_", v), OPT[[v]], as.character(cat_levels(d[[v]], v)))))
} else {
  helpText("More filters appear here automatically once those columns exist.")
}

kpi_box <- function(title, value, sub = NULL, small = FALSE)
  div(class = "kpi", div(class = "t", title),
      div(class = "v", style = if (small) "font-size:22px;", value),
      if (!is.null(sub)) div(class = "t", sub))

# UI layout
ui <- page_fluid(
  theme = app_theme,
  padding = 0,
  
  # Inject CSS and first-visit tour trigger
  tags$head(
    tags$style(HTML(explore_css)),
    tags$script(HTML("$(document).on('shiny:connected', function() {
      if (!localStorage.getItem('kolTourSeenExplore')) Shiny.setInputValue('first_visit', true, {priority: 'event'});
    });"))
  ),
  
  # Top header bar
  div(class = "hdr",
      div(div(class = "over", "Explore · Fatal vs non-fatal"),
          h1("Kolkata Road Crashes 2021–2023"),
          div(class = "sub", sprintf("Kolkata Traffic Police records · %s cases", fmt(nrow(d))))),
      div(actionButton("how", "? How to use", class = "btn-hdr"),
          actionButton("ex_reset", "Reset all", class = "btn-hdr"))),
  
  # Main sidebar and dashboard content
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 340, bg = "#fff",
      helpText("Every filter applies to both fatal and non-fatal crashes, so you can see how they differ."),
      div(class = "sidebar-label", "Compare"),
      selectInput("ex_by", "Compare by", choices = BY_CHOICES, selected = "victim"),
      tags$label("Show as"),
      div(class = "seg", radioButtons("ex_mode", NULL, c("Share within severity (%)" = "share", "Count" = "n"), inline = TRUE)),
      div(class = "sidebar-label", style = "margin-top:10px;", "Map"),
      tags$label("Show on map"),
      div(class = "seg", checkboxGroupInput("ex_show", NULL, choices = names(sev_fill), selected = names(sev_fill), inline = TRUE)),
      tags$label("Map view"),
      div(class = "seg", radioButtons("ex_map_mode", NULL, c("Clusters" = "cluster", "Individual points" = "points"), inline = TRUE)),
      div(class = "sidebar-label", style = "margin-top:10px;", "Filters"),
      tags$label("Year"),
      div(class = "seg", checkboxGroupInput("ex_year", NULL, yrs, inline = TRUE)),
      tags$label("Period"),
      div(class = "seg", radioButtons("ex_period", NULL, c("All", "Day", "Night"), inline = TRUE)),
      sliderInput("ex_hours", "Hour of day", min = 0, max = 23, value = c(0, 23), step = 1, sep = ""),
      tags$label("Time band"),
      div(class = "seg seg-wrap", checkboxGroupInput("ex_band", NULL, TIME_BANDS, inline = TRUE)),
      do.call(accordion, c(list(open = "Victim & vehicle"), Filter(Negate(is.null), list(
        accordion_panel("Victim & vehicle",
                        pills("ex_vic", "Victim Type", sorted_vic),
                        pills("ex_off", "Offending Vehicle", sorted_off)),
        accordion_panel("More variables", opt_filters),
        if (HAS_STOPS) accordion_panel("Bus stops & assumptions",
                                       sliderInput("ex_radius", "A crash is 'near a stop' if within", min = 25, max = 500, value = 100, step = 25, post = " m"),
                                       radioButtons("ex_prox", "Show crashes", c("All crashes" = "all", "Only near a stop" = "near", "Only away from stops" = "far")),
                                       checkboxGroupInput("ex_stop_types", "Stop types to count", choices = sort(unique(STOPS$stop_type)), selected = sort(unique(STOPS$stop_type))),
                                       radioButtons("ex_stop_junction", "Which stops", c("All stops" = "all", "Junction stops only" = "Y", "Non-junction stops only" = "N")),
                                       checkboxInput("ex_show_stops", "Show bus stops on the map", value = TRUE),
                                       helpText("Assumption: each crash is matched to its nearest selected stop by straight-line distance."))
      )))),
      uiOutput("ex_countbox"),
      div(style = "margin-top:12px;", actionLink("about", "ⓘ About the data & source", class = "about"))
    ),
    
    # Dashboard main panel
    div(style = "padding:14px 22px 20px;",
        div(class = "story", "Fatal and non-fatal crashes, side by side"),
        div(class = "lede", "Pick what to compare in the sidebar. Every chart, the map and the table follow the same filters."),
        uiOutput("ex_kpis"),
        uiOutput("ex_insight"),
        
        # Spatial map and primary comparison bar chart
        div(class = "panel", h4("Where crashes happen: fatal and non-fatal together"),
            leafletOutput("ex_map", height = 520)),
        div(class = "panel", textOutput("ex_cmp_title", container = h4), plotlyOutput("ex_cmp", height = 420)),
        
        # Middle row: lethality rate and hourly profile
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          div(class = "panel", h4("How deadly is each category?"), plotlyOutput("ex_rate", height = 360)),
          div(class = "panel", h4("When: hour-of-day profile"), plotlyOutput("ex_hour", height = 360))
        ),
        
        # Bottom row: multi-year trend and summary table
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          div(class = "panel", h4("Change over time"), plotlyOutput("ex_trend", height = 340)),
          div(class = "panel", h4("Side-by-side table"), div(class = "tblwrap", tableOutput("ex_table")))
        ),
        
        # Conditional bus stops proximity analysis section
        if (HAS_STOPS) tagList(
          div(class = "story2", "Bus stops: are crashes happening near stops?"),
          uiOutput("ex_stop_kpis"),
          layout_columns(
            col_widths = c(6, 6), fill = FALSE,
            div(class = "panel", h4("How close are crashes to a bus stop?"), plotlyOutput("ex_dist", height = 360)),
            div(class = "panel", h4("Bus stops with the most crashes nearby"), plotlyOutput("ex_topstops", height = 460))
          )
        )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  empty_msg <- "No crashes match these filters."
  
  pick <- function(x, col, sel) if (length(sel)) x[x[[col]] %in% sel, ] else x # Helper to filter column if values selected
  
  # Filter active bus stops and compute nearest stop per crash
  stops_sel <- reactive({
    if (!HAS_STOPS) return(NULL)
    s <- STOPS
    if (length(input$ex_stop_types)) s <- s[s$stop_type %in% input$ex_stop_types, ]
    if (!identical(input$ex_stop_junction, "all") && !is.null(input$ex_stop_junction)) s <- s[s$is_junction == (input$ex_stop_junction == "Y"), ]
    s
  })
  
  crash_stop <- reactive({
    s <- stops_sel(); validate(need(!is.null(s) && nrow(s) > 0, "No bus stops match these settings."))
    nearest_stop(d$lat, d$lon, s)
  })
  
  # Filter dataset based on sidebar selections and proximity rules
  ex_data <- reactive({
    x <- d |>
      pick("year", input$ex_year) |>
      pick("offender", input$ex_off) |>
      pick("victim", input$ex_vic) |>
      pick("time_band", input$ex_band)
    for (v in names(OPT)) x <- pick(x, v, input[[paste0("ex_", v)]])
    if (input$ex_period != "All") x <- filter(x, period == input$ex_period)
    hr <- input$ex_hours
    if (!identical(as.numeric(hr), c(0, 23))) x <- filter(x, !is.na(hour), hour >= hr[1], hour <= hr[2])
    
    if (HAS_STOPS) {
      cs <- crash_stop(); s <- stops_sel(); rad <- input$ex_radius
      x <- x |> mutate(
        stop_dist = cs$dist[rid], stop_idx = cs$idx[rid],
        stop_name = if (!is.null(s) && nrow(s) > 0) s$stop_name[stop_idx] else NA_character_,
        stop_kind = if (!is.null(s) && nrow(s) > 0) s$stop_type[stop_idx] else NA_character_,
        stop_junction = ifelse(is.na(stop_idx), NA_character_, ifelse(s$is_junction[stop_idx], "Junction stop", "Non-junction stop")),
        near_stop = ifelse(is.na(stop_dist), NA_character_, ifelse(stop_dist <= rad, paste0("Within ", rad, " m of a stop"), paste0("Beyond ", rad, " m"))),
        stop_band = cut(stop_dist, breaks = c(-Inf, 50, 100, 200, 500, Inf), labels = STOP_BANDS))
      if (identical(input$ex_prox, "near")) x <- filter(x, !is.na(stop_dist), stop_dist <= rad)
      if (identical(input$ex_prox, "far"))  x <- filter(x, !is.na(stop_dist), stop_dist > rad)
    }
    x
  })
  
  by_label <- reactive(names(BY_CHOICES)[BY_CHOICES == input$ex_by][1]) # Active comparison variable label
  
  # Top KPI cards and dynamic insight headline
  output$ex_kpis <- renderUI({
    x <- ex_data()
    nf <- sum(x$severity == "Fatal"); nn <- sum(x$severity == "Non-fatal")
    f <- top_share(x$victim[x$severity == "Fatal"]); n <- top_share(x$victim[x$severity == "Non-fatal"])
    layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE,
      kpi_box("Fatal crashes", fmt(nf), "matching filters"),
      kpi_box("Non-fatal crashes", fmt(nn), "matching filters"),
      kpi_box("Fatal share of crashes", if (nf + nn == 0) "n/a" else paste0(pct(nf, nf + nn), "%"), "of all matching crashes"),
      kpi_box("Top victim group", ifelse(is.na(f$label), "n/a", f$label),
              sprintf("Fatal %s%% | non-fatal top: %s (%s%%)", f$share, ifelse(is.na(n$label), "n/a", n$label), n$share), small = TRUE)
    )
  })
  
  ex_insight_html <- reactive({
    x <- ex_data(); g <- compare_tbl(x, input$ex_by)
    if (is.null(g)) return(empty_msg)
    w <- g |> mutate(cat = as.character(cat)) |>
      group_by(cat) |>
      summarise(f = share[severity == "Fatal"], nfs = share[severity == "Non-fatal"], .groups = "drop") |>
      mutate(gap = f - nfs) |> arrange(desc(abs(gap)))
    r <- w[1, ]
    sprintf("Biggest difference by <b>%s</b>: <b>%s</b> makes up <b>%s%%</b> of fatal crashes but <b>%s%%</b> of non-fatal ones.",
            by_label(), htmlEscape(r$cat), r$f, r$nfs)
  })
  output$ex_insight <- renderUI(div(class = "insight", HTML(ex_insight_html())))
  
  # Interactive Leaflet map setup and proxy updates
  output$ex_map <- renderLeaflet(base_map(stops = HAS_STOPS))
  map_ok <- reactiveVal(FALSE)
  observeEvent(input$ex_map_bounds, map_ok(TRUE), once = TRUE)
  
  observe({
    req(map_ok())
    x <- filter(ex_data(), severity %in% input$ex_show)
    m <- leafletProxy("ex_map") |> clearMarkers() |> clearMarkerClusters()
    if (HAS_STOPS && isTRUE(input$ex_show_stops)) {
      s <- stops_sel()
      if (!is.null(s) && nrow(s) > 0) m <- addCircleMarkers(m, data = s, lng = ~stop_lon, lat = ~stop_lat, radius = 3, weight = 1,
                                                            color = STOP_LINE, fillColor = STOP_FILL, fillOpacity = 0.85,
                                                            popup = ~paste0("<b>", htmlEscape(stop_name), "</b><br>", stop_type,
                                                                            ifelse(is_junction, " (junction)", "")))
    }
    if (identical(input$ex_map_mode, "cluster")) add_clusters(m, x) else add_points(m, x)
  })
  
  # Main comparison, lethality rate, hourly profile, and trend charts
  output$ex_cmp_title <- renderText(paste(if (input$ex_mode == "share") "Share of crashes by" else "Number of crashes by", tolower(by_label())))
  output$ex_cmp   <- renderPlotly({ x <- ex_data(); validate(need(nrow(x) > 0, empty_msg)); p <- compare_bar(x, input$ex_by, input$ex_mode); validate(need(!is.null(p), empty_msg)); p })
  output$ex_rate  <- renderPlotly({ x <- ex_data(); p <- rate_bar(x, input$ex_by); validate(need(!is.null(p), "Not enough crashes in each category to compare (needs 5 or more).")); p })
  output$ex_hour  <- renderPlotly({ x <- ex_data(); validate(need(sum(!is.na(x$hour)) > 0, empty_msg)); hour_compare(x, input$ex_mode) })
  output$ex_trend <- renderPlotly({ x <- ex_data(); validate(need(nrow(x) > 0, empty_msg)); trend_plot(x) })
  
  # Bus stop proximity KPIs and charts
  output$ex_stop_kpis <- renderUI({
    x <- ex_data(); rad <- input$ex_radius
    xs <- filter(x, !is.na(stop_dist))
    one <- function(sev) {
      y <- xs[xs$severity == sev, ]
      list(n = nrow(y), near = sum(y$stop_dist <= rad), med = if (nrow(y)) round(median(y$stop_dist)) else NA)
    }
    f <- one("Fatal"); n <- one("Non-fatal")
    shown <- function(o) if (o$n == 0) "n/a" else paste0(pct(o$near, o$n), "%")
    layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE,
      kpi_box("Fatal crashes near a stop", shown(f), sprintf("%s of %s mapped crashes within %s m", fmt(f$near), fmt(f$n), rad)),
      kpi_box("Non-fatal crashes near a stop", shown(n), sprintf("%s of %s mapped crashes within %s m", fmt(n$near), fmt(n$n), rad)),
      kpi_box("Median distance to a stop", paste0(ifelse(is.na(f$med), "n/a", f$med), " m | ", ifelse(is.na(n$med), "n/a", n$med), " m"), "fatal | non-fatal"),
      kpi_box("Stops with a crash nearby", fmt(length(unique(xs$stop_idx[xs$stop_dist <= rad]))),
              paste0("of ", fmt(nrow(stops_sel())), " selected stops"))
    )
  })
  
  output$ex_dist     <- renderPlotly({ p <- dist_curve(ex_data(), input$ex_radius); validate(need(!is.null(p), "No crashes with usable coordinates.")); p })
  output$ex_topstops <- renderPlotly({ p <- top_stops_bar(ex_data(), input$ex_radius); validate(need(!is.null(p), "No crashes within this radius of a stop.")); p })
  
  # Side-by-side summary table and sidebar match counter
  ex_tab <- reactive({
    x <- ex_data(); g <- compare_tbl(x, input$ex_by); req(g)
    f <- g |> filter(severity == "Fatal") |> transmute(cat = as.character(cat), fn = as.integer(n), fp = share)
    n <- g |> filter(severity == "Non-fatal") |> transmute(cat = as.character(cat), nn = as.integer(n), np = share)
    tab <- left_join(f, n, by = "cat") |>
      mutate(gap = round(fp - np, 1), rate = ifelse(fn + nn > 0, round(100 * fn / (fn + nn), 1), NA_real_))
    tab <- tab[match(levels(g$cat), tab$cat), ]
    tab <- tab[!is.na(tab$cat), ]
    setNames(tab[, c("cat", "fn", "fp", "nn", "np", "gap", "rate")],
             c(by_label(), "Fatal", "Fatal %", "Non-fatal", "Non-fatal %", "Gap (pp)", "Fatal share of crashes %"))
  })
  output$ex_table <- renderTable(ex_tab(), striped = TRUE, digits = 1, na = "-")
  
  output$ex_countbox <- renderUI({
    x <- ex_data(); n <- nrow(x)
    p <- if (n > 0) round(100 * mean(!is.na(x$lat)), 1) else 0
    div(class = "countbox", HTML(sprintf("Showing <b>%s</b> of %s crashes.<br><b>%s%%</b> have a map location.",
                                         fmt(n), fmt(nrow(d)), p)))
  })
  
  # Reset all filters to default values
  observeEvent(input$ex_reset, {
    for (id in c("ex_year", "ex_off", "ex_vic", "ex_band", paste0("ex_", names(OPT))))
      updateCheckboxGroupInput(session, id, selected = character(0))
    updateSelectInput(session, "ex_by", selected = "victim")
    updateRadioButtons(session, "ex_mode", selected = "share")
    updateRadioButtons(session, "ex_map_mode", selected = "cluster")
    updateRadioButtons(session, "ex_period", selected = "All")
    updateCheckboxGroupInput(session, "ex_show", selected = names(sev_fill))
    updateSliderInput(session, "ex_hours", value = c(0, 23))
    if (HAS_STOPS) {
      updateSliderInput(session, "ex_radius", value = 100)
      updateRadioButtons(session, "ex_prox", selected = "all")
      updateCheckboxGroupInput(session, "ex_stop_types", selected = sort(unique(STOPS$stop_type)))
      updateRadioButtons(session, "ex_stop_junction", selected = "all")
      updateCheckboxInput(session, "ex_show_stops", value = TRUE)
    }
  })
  
  # Help tour and data source modals
  tour <- function() modalDialog(
    title = "How to use this dashboard", easyClose = TRUE,
    tags$ol(
      tags$li(tags$b("The filter sidebar"), " controls every chart on the page. Each filter applies to both fatal and non-fatal crashes."),
      tags$li(tags$b("Compare by"), " changes what the bar chart, the table and the headline sentence compare."),
      tags$li(tags$b("The map"), " shows fatal crashes as maroon diamonds and non-fatal ones as yellow dots."),
      tags$li(tags$b("Reset all"), " puts every filter back to its starting position.")),
    footer = tagList(tags$button(type = "button", class = "btn btn-primary", `data-bs-dismiss` = "modal",
                                 onclick = "localStorage.setItem('kolTourSeenExplore', '1');", "Got it")))
  observeEvent(input$how, showModal(tour()))
  observeEvent(input$first_visit, showModal(tour()))
  
  observeEvent(input$about, showModal(modalDialog(
    title = "About the data and where it comes from", easyClose = TRUE, footer = modalButton("Close"),
    p("Kolkata Police road accident records for 2021-2023 (fatal and non-fatal sheets), shared by the Kolkata Police Department."),
    tags$ul(
      tags$li(sprintf("Crashes: %s fatal and %s non-fatal.", fmt(sum(d$severity == "Fatal")), fmt(sum(d$severity == "Non-fatal")))),
      tags$li(sprintf("Location: %s%% of crashes have a usable position inside Kolkata.", round(100 * mean(!is.na(d$lat))))),
      if (HAS_STOPS) tags$li(sprintf("Bus stops: %s stops inside Kolkata.", fmt(nrow(STOPS)))),
      tags$li("Counts are crashes recorded by the police.")))))
}

shinyApp(ui, server)