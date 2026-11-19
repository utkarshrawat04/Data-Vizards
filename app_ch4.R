library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(plotly)
library(leaflet)

source("R/clean_data.R")    
source("R/clean_fatal.R")   
source("R/bus_stops.R")     
source("R/shared_theme.R")  # Load helper scripts and centralized theme

pct <- function(x, d = 0) paste0(formatC(100 * x, format = "f", digits = d), "%") # Format proportion as percentage string

if (!exists("load_all_crashes")) {
  load_all_crashes <- function(path) {
    bind_rows(load_nonfatal(path), mutate(load_fatal(path), persons_killed = persons)) |>
      mutate(crash_id = row_number())
  }
}     # Fallback loader combining fatal and non-fatal sheets if missing from R/ files

XLSX  <- "data/data.xlsx"
STOPS <- "data/bus_stops.xlsx"

for (f in c(XLSX, STOPS)) if (!file.exists(f))
  stop("Cannot find ", f, ". Put data.xlsx and bus_stops.xlsx inside the 'data' folder, then run the app again.")
CACHE <- "data/ch4_crashes_clean.rds"
if (file.exists(CACHE) && file.mtime(CACHE) >= max(file.mtime(c(XLSX, STOPS)))) {
  cached  <- readRDS(CACHE)
  crashes <- cached$crashes
  stops   <- cached$stops
} else {
  message("Cleaning the data (about 20 seconds, first run only)...")
  stops   <- load_stops(STOPS)
  crashes <- add_stop_distance(suppressWarnings(load_all_crashes(XLSX)), stops)
  saveRDS(list(crashes = crashes, stops = stops), CACHE)
}     # Load cached crash and stop data or rebuild from Excel files if outdated

N_TOTAL   <- nrow(crashes)                                            # Total crash count
N_BUS     <- sum(crashes$bus == "Bus")                                # Total crashes involving a bus
N_LOCATED <- sum(!is.na(crashes$dist_stop_m))                         # Total crashes with valid coordinates
YEARS     <- sort(unique(crashes$year))                               # Available dataset years

RU_LVLS  <- c("All road users", "Pedestrian", "Rider", "Pillion rider", "Cyclist",
              "Passenger", "Driver / Owner", "Other", "Unknown")
RU_LVLS  <- RU_LVLS[RU_LVLS == "All road users" | RU_LVLS %in% crashes$road_user]
STN_LVLS <- names(sort(table(crashes$station), decreasing = TRUE))    # Police stations sorted by frequency
MAX_D    <- 500                                                       # Distance chart upper bound (meters)

ui <- page_fluid(
  theme = app_theme,
  padding = 0,
  tags$head(
    tags$style(HTML(app_css)),
    tags$style(HTML(".legend-dot { display:inline-block; width:10px; height:10px; border-radius:50%; margin:0 4px 0 10px; vertical-align:middle; }")),
    tags$script(HTML("$(document).on('shiny:connected', function() {
      if (!localStorage.getItem('kolTourSeenCh4')) Shiny.setInputValue('first_visit', true, {priority: 'event'});
    });"))
  ),  # Inject custom CSS and first-visit tour trigger
  
  div(class = "hdr",
      div(div(class = "over", "Chapter 4 of 4 · The main culprit: the bus"),
          h1("Kolkata Road Crashes 2021–2023"),
          div(class = "sub", sprintf("Kolkata Traffic Police records and WBTC bus stops · %s crashes, %s involving a bus · %s bus stops",
                                     fmt(N_TOTAL), fmt(N_BUS), fmt(nrow(stops))))),
      div(actionButton("how", "? How to use", class = "btn-hdr"),
          actionButton("reset_top", "Reset all", class = "btn-hdr"))), # Top header bar layout
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 300, bg = "#fff",
      div(class = "sidebar-label", "Filters"),
      checkboxGroupInput("year", "Year", choices = YEARS, selected = YEARS, inline = TRUE),
      tags$label("Severity"),
      div(class = "seg", radioButtons("severity", NULL, inline = TRUE, selected = "All", choices = c("All" = "All", "Fatal" = "Fatal", "Non-fatal" = "Non-fatal"))),
      sliderInput("radius", "Near a stop = within", min = 25, max = 300, value = 100, step = 25, post = " m"),
      selectInput("road_user", "Road user (victim)", choices = RU_LVLS),
      sliderInput("hours", "Hour of day", min = 0, max = 23, value = c(0, 23), step = 1, post = ":00", sep = ""),
      selectizeInput("station", "Police station", choices = STN_LVLS, multiple = TRUE,
                     options = list(placeholder = "Search...")),
      checkboxGroupInput("layers", "Show on the map",
                         choices = c("Bus crashes", "Other crashes", "Bus stops"),
                         selected = c("Bus crashes", "Bus stops")),
      uiOutput("countbox"),
      uiOutput("chips"),
      div(downloadButton("dl", "Download CSV")),
      div(style = "margin-top:12px;", actionLink("about", "ⓘ About the data & source", class = "about"))
    ), # Sidebar filter controls
    
    div(style = "padding:14px 22px 20px;",
        div(class = "story", uiOutput("story", inline = TRUE)),
        div(class = "lede", uiOutput("lede", inline = TRUE)),
        uiOutput("empty_banner"),
        uiOutput("kpis"),
        layout_columns(
          col_widths = c(7, 5), fill = FALSE,
          div(class = "panel", h4("Where bus crashes happen, and where the stops are"),
              uiOutput("map_hint"),
              div(role = "img", `aria-label` = "Map of Kolkata showing bus crashes, other crashes and bus stops", leafletOutput("map", height = 420))),
          div(class = "panel", h4("How close to a stop?"),
              div(class = "hint", "Share of crashes that happened within each distance of the nearest bus stop. Click the chart to set the distance."),
              chart("dist_plot", 400, "Line chart of the share of bus crashes and other crashes within each distance of a bus stop"))
        ), # Top row: spatial map and cumulative distance curve
        uiOutput("callout"),
        layout_columns(
          col_widths = c(7, 5), fill = FALSE,
          div(class = "panel", h4("Stops with the most bus crashes nearby"),
              uiOutput("stops_hint"),
              chart("stops_plot", 340, "Bar chart of the bus stops with the most bus crashes within the chosen distance")),
          div(class = "panel victim", h4("Who bus crashes hurt"),
              uiOutput("victim"))
        ), # Middle row: high-incident bus stops and victim demographics
        div(class = "panel", h4("Buses in context: fatal crashes by vehicle"),
            div(class = "hint", "Buses are a big part of the story, but not the biggest: trucks and goods vehicles are in more fatal crashes. That is why this chapter looks at where bus crashes happen, not only how many there are."),
            chart("context_plot", 230, "Bar chart of fatal crashes by vehicle group with buses highlighted"))
    ) # Main dashboard content panels
  )
) 
server <- function(input, output, session) {
  
  picked_stop <- reactiveVal("") # Track currently selected bus stop from map or bar chart
  
  filtered <- reactive({
    d <- crashes
    d <- d[d$year %in% as.integer(input$year), , drop = FALSE]
    if (input$severity != "All")             d <- d[d$severity == input$severity, , drop = FALSE]
    if (input$road_user != "All road users") d <- d[d$road_user == input$road_user, , drop = FALSE]
    if (length(input$station) > 0)           d <- d[!is.na(d$station) & d$station %in% input$station, , drop = FALSE]
    if (!identical(as.integer(input$hours), c(0L, 23L)))
      d <- d[!is.na(d$hour) & d$hour >= input$hours[1] & d$hour <= input$hours[2], , drop = FALSE]
    if (picked_stop() != "")
      d <- d[!is.na(d$nearest_stop) & d$nearest_stop == picked_stop() & d$dist_stop_m <= input$radius, , drop = FALSE]
    d
  }) # Filter crash dataset based on sidebar inputs and selected stop
  
  located <- reactive({ d <- filtered(); d[!is.na(d$dist_stop_m), , drop = FALSE] }) # Subset crashes with valid spatial distance to stops
  
  near_share <- reactive({
    g <- located(); r <- input$radius
    s <- function(grp) { x <- g$dist_stop_m[g$bus == grp]; if (length(x)) mean(x <= r) else NA }
    list(bus = s("Bus"), other = s("Other vehicles"),
         n_bus = sum(g$bus == "Bus"), n_other = sum(g$bus != "Bus"))
  }) # Calculate proximity shares within chosen radius for buses vs other vehicles
  
  reset_all <- function() {
    updateCheckboxGroupInput(session, "year", selected = YEARS)
    updateRadioButtons(session, "severity", selected = "All")
    updateSliderInput(session, "radius", value = 100)
    updateSelectInput(session, "road_user", selected = "All road users")
    updateSliderInput(session, "hours", value = c(0, 23))
    updateSelectizeInput(session, "station", selected = character(0))
    updateCheckboxGroupInput(session, "layers", selected = c("Bus crashes", "Bus stops"))
    picked_stop("")
  } # Reset all sidebar filters and map selections to default
  observeEvent(input$reset_top,    reset_all())   # Reset trigger from top header bar button
  observeEvent(input$reset_inline, reset_all())   # Reset trigger from empty state banner link
  
  output$countbox <- renderUI({
    d <- filtered(); g <- located()
    div(class = "countbox", HTML(sprintf(
      "Showing <b>%s</b> of %s crashes.<br><b>%s</b> involved a bus. <b>%s</b> have a location and are on the map.",
      fmt(nrow(d)), fmt(N_TOTAL), fmt(sum(d$bus == "Bus")), fmt(nrow(g)))))
  }) # Sidebar match count summary box
  
  output$chips <- renderUI({
    chip <- function(key, label)
      tags$a(class = "chip", href = "#", `data-k` = key,
             onclick = "Shiny.setInputValue('chip_remove', this.dataset.k, {priority: 'event'}); return false;",
             paste0(label, " ×"))
    items <- list()
    if (length(input$year) != length(YEARS)) items <- c(items, list(chip("year", paste(sort(input$year), collapse = ", "))))
    if (input$severity != "All")             items <- c(items, list(chip("severity", input$severity)))
    if (input$radius != 100)                 items <- c(items, list(chip("radius", sprintf("Within %d m", input$radius))))
    if (input$road_user != "All road users") items <- c(items, list(chip("road_user", input$road_user)))
    if (!identical(as.integer(input$hours), c(0L, 23L)))
      items <- c(items, list(chip("hours", sprintf("%02d:00–%02d:59", input$hours[1], input$hours[2]))))
    for (s in input$station) items <- c(items, list(chip(paste0("station::", s), s)))
    if (picked_stop() != "") items <- c(items, list(chip("stop", paste("Stop:", picked_stop()))))
    if (length(items)) div(style = "margin-bottom:8px;", items)
  }) # Render active filter chips below sidebar
  
  observeEvent(input$chip_remove, {
    k <- input$chip_remove
    if (k == "year")      updateCheckboxGroupInput(session, "year", selected = YEARS)
    if (k == "severity")  updateRadioButtons(session, "severity", selected = "All")
    if (k == "radius")    updateSliderInput(session, "radius", value = 100)
    if (k == "road_user") updateSelectInput(session, "road_user", selected = "All road users")
    if (k == "hours")     updateSliderInput(session, "hours", value = c(0, 23))
    if (k == "stop")      picked_stop("")
    if (startsWith(k, "station::"))
      updateSelectizeInput(session, "station", selected = setdiff(input$station, sub("^station::", "", k)))
  }) # Remove individual filter when an active chip is clicked
  
  output$empty_banner <- renderUI({
    if (nrow(filtered()) > 0) return(NULL)
    div(class = "empty", "No crashes match these filters. ", actionLink("reset_inline", "Reset all"))
  }) # Empty state warning banner when no records match
  
  output$story <- renderUI({
    ns <- near_share()
    if (is.na(ns$bus) || ns$n_bus < 20) return("Buses crash where they stop")
    sprintf("%s of bus crashes happen within %d m of a stop", pct(ns$bus), input$radius)
  }) # Dynamic narrative headline text
  
  output$lede <- renderUI({
    f <- filtered(); f <- f[f$severity == "Fatal", ]
    if (nrow(f) == 0) return(HTML("Buses are second only to trucks in fatal crashes. Choose filters that include fatal crashes to see the numbers."))
    HTML(sprintf(paste0("A bus was involved in <b>%s</b> of fatal crashes, second only to trucks and goods vehicles (%s). ",
                        "This chapter asks where bus crashes happen: are they clustered around the places buses stop?"),
                 pct(mean(f$vehicle == "Bus")), pct(mean(f$vehicle == "Truck / Goods"))))
  }) # Descriptive lede text introducing bus fatal shares
  
  output$kpis <- renderUI({
    d <- filtered(); b <- d[d$bus == "Bus", ]; ns <- near_share()
    killed <- sum(b$persons_killed[b$severity == "Fatal"], na.rm = TRUE)
    kpi <- function(v, t) column(3, div(class = "kpi", div(class = "v", v), div(class = "t", t)))
    fluidRow(style = "margin-bottom:16px;",
             kpi(fmt(nrow(b)), sprintf("bus crashes shown, of %s crashes", fmt(nrow(d)))),
             kpi(fmt(killed), "people killed in bus crashes"),
             kpi(if (is.na(ns$bus)) "–" else pct(ns$bus), sprintf("of bus crashes within %d m of a stop", input$radius)),
             kpi(if (is.na(ns$other)) "–" else pct(ns$other), sprintf("of other crashes within %d m of a stop", input$radius)))
  }) # Top summary KPI cards
  
  output$map_hint <- renderUI({
    div(class = "hint", HTML(paste0(
      "Only crashes with a recorded location are shown. Click a grey bus stop (or a bar below) to zoom in and draw its ",
      input$radius, " m zone. Click a crash for its details.")))
  }) # Map interaction instructions
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      setView(lng = 88.36, lat = 22.555, zoom = 12) |>
      addLegend(position = "bottomright", opacity = 0.9,
                colors = c(MAROON, "#C9A3B8", YELLOW, "#8C8389"),
                labels = c("Bus crash near a stop", "Bus crash further away", "Other crash", "Bus stop"))
  }) # Initialize Leaflet base map
  
  observe({
    g <- located(); r <- input$radius; lay <- input$layers; ps <- picked_stop()
    popup <- function(x) sprintf("<b>%s</b> · %s<br>%s hit a %s<br>%s<br>%.0f m from <b>%s</b>",
                                 format(x$date, "%d %b %Y"), x$severity, x$vehicle, tolower(x$road_user),
                                 coalesce(x$station, "station not recorded"), x$dist_stop_m, x$nearest_stop)
    proxy <- leafletProxy("map") |> clearMarkers() |> clearShapes()
    
    if ("Bus stops" %in% lay)
      proxy |> addCircleMarkers(data = stops, lng = ~stop_lon, lat = ~stop_lat, layerId = ~paste0("stop_", stop_id),
                                radius = 3, stroke = FALSE, fillColor = "#8C8389", fillOpacity = 0.75,
                                label = ~paste0(stop_name, " (", tolower(stop_type), ")"))
    if (ps != "" && any(stops$stop_name == ps)) {
      s <- stops[stops$stop_name == ps, ][1, ]
      proxy |> addCircles(lng = s$stop_lon, lat = s$stop_lat, radius = r, color = MAROON, weight = 2,
                          fillColor = MAROON, fillOpacity = 0.08, label = sprintf("%s: %d m zone", ps, r))
    }
    if (nrow(g) == 0) return()
    if ("Other crashes" %in% lay) {
      o <- g[g$bus != "Bus", ]
      if (nrow(o)) proxy |> addCircleMarkers(data = o, lng = ~lon, lat = ~lat, radius = 4, color = "#ffffff", weight = 1,
                                             fillColor = YELLOW, fillOpacity = 0.75, popup = popup(o))
    }
    if ("Bus crashes" %in% lay) {
      far  <- g[g$bus == "Bus" & g$dist_stop_m >  r, ]
      near <- g[g$bus == "Bus" & g$dist_stop_m <= r, ]
      if (nrow(far))  proxy |> addCircleMarkers(data = far, lng = ~lon, lat = ~lat, radius = 5, color = "#ffffff", weight = 1,
                                                fillColor = "#C9A3B8", fillOpacity = 0.9, popup = popup(far))
      if (nrow(near)) proxy |> addCircleMarkers(data = near, lng = ~lon, lat = ~lat, radius = 6, color = "#ffffff", weight = 1,
                                                fillColor = MAROON, fillOpacity = 0.9, popup = popup(near))
    }
  }) # Dynamically update map markers, stop layers, and buffer circles
  
  observeEvent(picked_stop(), {
    ps <- picked_stop()
    if (ps != "" && any(stops$stop_name == ps)) {
      s <- stops[stops$stop_name == ps, ][1, ]
      leafletProxy("map") |> setView(lng = s$stop_lon, lat = s$stop_lat, zoom = 16)
    } else {
      leafletProxy("map") |> setView(lng = 88.36, lat = 22.555, zoom = 12)
    }
  }, ignoreInit = TRUE) # Zoom map to selected bus stop or reset view
  
  observeEvent(input$map_marker_click, {
    id <- input$map_marker_click$id
    if (is.null(id) || !startsWith(id, "stop_")) return()
    name <- stops$stop_name[paste0("stop_", stops$stop_id) == id][1]
    if (is.na(name)) return()
    picked_stop(if (identical(picked_stop(), name)) "" else name)
  }) # Toggle bus stop selection when clicked on map
  
  output$dist_plot <- renderPlotly({
    g <- located()
    validate(need(nrow(g) > 0, "No crashes with a location match these filters."))
    xs <- seq(0, MAX_D, by = 5)
    curve <- function(grp) {
      x <- g$dist_stop_m[g$bus == grp]
      if (!length(x)) return(NULL)
      data.frame(d = xs, share = vapply(xs, function(v) 100 * mean(x <= v), numeric(1)), grp = grp, n = length(x))
    }
    cv <- bind_rows(curve("Bus"), curve("Other vehicles"))
    p <- plot_ly(source = "dist")
    for (grp in unique(cv$grp)) {
      c1 <- cv[cv$grp == grp, ]
      col <- if (grp == "Bus") MAROON else YELLOW
      p <- p |> add_lines(data = c1, x = ~d, y = ~share, name = sprintf("%s (%s)", grp, fmt(c1$n[1])),
                          line = list(color = col, width = 3),
                          text = sprintf("%s: %.0f%% within %d m", grp, c1$share, c1$d), hoverinfo = "text")
    }
    p |> tidy(margin = list(l = 50, r = 15, t = 40, b = 45), hovermode = "x",
              xaxis = list(title = "Distance to the nearest bus stop", ticksuffix = " m", range = c(0, MAX_D),
                           gridcolor = GRID, zeroline = FALSE),
              yaxis = list(title = "", ticksuffix = "%", range = c(0, 100), gridcolor = GRID, zeroline = FALSE),
              legend = list(orientation = "h", x = 0, y = 1.16, entrywidth = 170),
              shapes = list(list(type = "line", x0 = input$radius, x1 = input$radius, y0 = 0, y1 = 100,
                                 line = list(color = INK, dash = "dot", width = 1.5))),
              annotations = list(list(x = input$radius, y = 4, text = sprintf(" %d m", input$radius), showarrow = FALSE,
                                      xanchor = "left", font = list(size = 12, color = INK)))) |>
      event_register("plotly_click")
  }) # Cumulative distance distribution line chart
  
  observeEvent(event_data("plotly_click", source = "dist", priority = "event"), {
    x <- event_data("plotly_click", source = "dist", priority = "event")$x
    if (is.null(x) || !is.numeric(x)) return()
    updateSliderInput(session, "radius", value = min(300, max(25, round(x / 25) * 25)))
  }) # Update distance radius slider on distance chart click
  
  output$callout <- renderUI({
    ns <- near_share()
    if (is.na(ns$bus) || is.na(ns$other) || ns$n_bus < 20 || ns$other == 0) return(NULL)
    ratio <- ns$bus / ns$other
    div(class = "callout", HTML(sprintf(
      "Within <b>%d m</b> of a bus stop: <b>%s</b> of bus crashes, against %s of crashes with other vehicles. A bus crash is <b>%s times</b> as likely to happen next to a stop.",
      input$radius, pct(ns$bus), pct(ns$other), formatC(ratio, format = "f", digits = 1))))
  }) # Comparative risk callout banner
  
  output$stops_hint <- renderUI({
    div(class = "hint", sprintf("Bus crashes within %d m, counted at the nearest stop. Click a bar to zoom the map to that stop, draw its zone and filter the page to it; click again to undo.", input$radius))
  }) # Subtitle instructions for top bus stops chart
  
  output$stops_plot <- renderPlotly({
    g <- located()
    b <- g[g$bus == "Bus" & g$dist_stop_m <= input$radius, ]
    validate(need(nrow(b) > 0, "No bus crashes near a stop match these filters."))
    top <- b |> count(nearest_stop, name = "n") |> arrange(desc(n), nearest_stop) |> head(10) |> arrange(n)
    fat <- b |> filter(severity == "Fatal") |> count(nearest_stop, name = "fatal")
    top <- left_join(top, fat, by = "nearest_stop") |> mutate(fatal = coalesce(fatal, 0L))
    top$nearest_stop <- factor(top$nearest_stop, levels = top$nearest_stop)
    ps <- picked_stop()
    col <- ifelse(ps == "" | top$nearest_stop == ps, MAROON, QUIET)
    plot_ly(top, x = ~n, y = ~nearest_stop, type = "bar", orientation = "h", source = "stops",
            marker = list(color = col), textposition = "none",
            text = sprintf("%s<br>%d bus crashes within %d m (%d fatal)", top$nearest_stop, top$n, input$radius, top$fatal),
            hoverinfo = "text") |>
      tidy(margin = list(l = 190, r = 15, t = 10, b = 45),
           xaxis = list(title = "Bus crashes", rangemode = "tozero", gridcolor = GRID, zeroline = FALSE),
           yaxis = list(title = "", ticksuffix = "  ")) |>
      event_register("plotly_click")
  }) # Top 10 bus stops bar chart
  
  observeEvent(event_data("plotly_click", source = "stops", priority = "event"), {
    s <- event_data("plotly_click", source = "stops", priority = "event")$y
    if (is.null(s) || !is.character(s)) return()
    picked_stop(if (identical(picked_stop(), s)) "" else s)
  }) # Toggle stop selection when clicking a bar in top stops chart
  
  output$victim <- renderUI({
    b <- filtered(); b <- b[b$bus == "Bus", ]
    if (nrow(b) == 0) return(div(class = "hint", "No bus crashes match these filters."))
    ru <- b$road_user[!is.na(b$road_user) & b$road_user != "Unknown"]
    if (!length(ru)) return(div(class = "hint", "The victim is not recorded for these bus crashes."))
    tab <- sort(table(ru), decreasing = TRUE)
    rows <- head(names(tab), 4)
    f <- b[b$severity == "Fatal", ]
    tagList(
      div(class = "big", sprintf("Most often a %s.", tolower(rows[1]))),
      lapply(rows, function(r) div(class = "row2", span(r), span(pct(tab[[r]] / length(ru))))),
      div(class = "row2", span("Bus crashes that killed someone"), span(pct(nrow(f) / nrow(b)))),
      div(class = "hint", style = "margin-top:8px;",
          sprintf("Victim shares are based on %s bus crashes where the victim is recorded.", fmt(length(ru))))
    )
  }) # Victim demographics breakdown panel
  
  output$context_plot <- renderPlotly({
    f <- filtered(); f <- f[f$severity == "Fatal", ]
    validate(need(nrow(f) > 0, "No fatal crashes match these filters."))
    v <- f |> count(vehicle) |> mutate(share = 100 * n / sum(n)) |> arrange(n)
    v$vehicle <- factor(v$vehicle, levels = v$vehicle)
    plot_ly(v, x = ~share, y = ~vehicle, type = "bar", orientation = "h", textposition = "none",
            marker = list(color = ifelse(v$vehicle == "Bus", MAROON, QUIET)),
            text = sprintf("%s: %d fatal crashes (%.0f%%)", v$vehicle, v$n, v$share), hoverinfo = "text") |>
      tidy(margin = list(l = 110, r = 15, t = 5, b = 40),
           xaxis = list(title = "Share of fatal crashes", ticksuffix = "%", rangemode = "tozero", gridcolor = GRID, zeroline = FALSE),
           yaxis = list(title = "", ticksuffix = "  "))
  }) # Vehicle fatal context horizontal bar chart
  
  tour <- function() modalDialog(
    title = "How to use this dashboard", easyClose = TRUE,
    tags$ol(
      tags$li(tags$b("The filter sidebar"), " controls every chart on the page. Year, severity, distance, road user, hour and police station all combine."),
      tags$li(tags$b("The distance slider"), " sets what counts as 'near a stop'. You can also click the distance chart to set it."),
      tags$li(tags$b("Click a stop"), " on the map (a grey dot), or a bar in 'Stops with the most bus crashes nearby', to zoom in on it. Click it again to undo."),
      tags$li(tags$b("Reset all"), " puts every filter back to its starting position.")),
    footer = tagList(tags$button(type = "button", class = "btn btn-primary", `data-bs-dismiss` = "modal",
                                 onclick = "localStorage.setItem('kolTourSeenCh4', '1');", "Got it"))) # Help tour modal dialog builder
  
  observeEvent(input$how, showModal(tour()))         # Open tour on help button click
  observeEvent(input$first_visit, showModal(tour())) # Open tour on initial visit
  
  observeEvent(input$about, showModal(modalDialog(
    title = "About the data and where it comes from", easyClose = TRUE, footer = modalButton("Close"),
    p("Kolkata Police road accident records for 2021–2023 (all six sheets: fatal and non-fatal), shared by the Kolkata Police Department,",
      "and the bus stop list shared by the West Bengal Transport Corporation (WBTC). Both were cleaned with the same rules as Chapters 1 and 2."),
    tags$ul(
      tags$li(sprintf("%s crashes, %s of them involving a bus. %s crashes (%s) have a usable location; only these can be measured against bus stops.",
                      fmt(N_TOTAL), fmt(N_BUS), fmt(N_LOCATED), pct(N_LOCATED / N_TOTAL))),
      tags$li(sprintf("Bus stops: %s of the 1,856 listed stops are inside the Kolkata area with usable coordinates. The rest (outside the city, missing or wrong coordinates, duplicates) were left out.",
                      fmt(nrow(stops)))),
      tags$li("Distance = straight-line distance from the crash to the nearest bus stop, junction or terminal. It is not the distance along the road."),
      tags$li("Kolkata has many stops, so many crashes of any kind happen close to one. That is why bus crashes are always compared with crashes involving other vehicles."),
      tags$li("'Bus' = the offending vehicle recorded by the police was a bus. Counts are crashes recorded by the police, not rates: the data has no bus kilometres or traffic volumes."))))) # About data & methodology modal
  
  output$dl <- downloadHandler(
    filename = function() "kolkata_crashes_chapter4_filtered.csv",
    content = function(file) {
      out <- filtered()[, c("date", "year", "month", "hour", "severity", "station", "place", "vehicle", "road_user",
                            "persons_killed", "lat", "lon", "nearest_stop", "nearest_stop_type", "dist_stop_m")]
      out$dist_stop_m <- round(out$dist_stop_m)
      write.csv(out, file, row.names = FALSE, na = "")
    }) # CSV export download handler
}

shinyApp(ui, server) 