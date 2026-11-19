library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(plotly)
library(leaflet)


source("R/clean_data.R") 
source("R/clean_fatal.R")                 # Load helper scripts and shared theme
source("R/shared_theme.R")

crashes <- if (file.exists("data/crashes_ch2.rds")) {
  readRDS("data/crashes_ch2.rds")
} else {
  df <- bind_rows(load_nonfatal("data/data.xlsx"), load_fatal("data/data.xlsx")) |>
    mutate(crash_id = row_number())
  dir.create("data", showWarnings = FALSE)                    
  saveRDS(df, "data/crashes_ch2.rds")
  df
}                                         # Load cached crash data or rebuild from Excel if missing

# Constants and dropdown options
DEFAULT_SEV <- "nonfatal"                               # Default severity filter
N_TOTAL  <- nrow(crashes)                               # Total crash count
YEARS    <- sort(unique(crashes$year))                  # Available years
HOURS    <- 0:23                                        # 24-hour range
hour_lbl <- function(h) sprintf("%02d:00", h)          # Format hour labels

VEH_LVLS <- c("All vehicles", "Car / Taxi", "Two-wheeler", "Bus", "Truck / Goods","Auto / Rickshaw", "Bicycle", "Other", "Unknown")
VEH_LVLS <- VEH_LVLS[VEH_LVLS == "All vehicles" | VEH_LVLS %in% crashes$vehicle]  

RU_LVLS  <- c("All road users", "Pedestrian", "Rider", "Pillion rider", "Cyclist","Passenger", "Driver / Owner", "Other", "Unknown")
RU_LVLS  <- RU_LVLS[RU_LVLS == "All road users" | RU_LVLS %in% crashes$road_user]

STN_LVLS <- names(sort(table(crashes$station), decreasing = TRUE))          
HEAT <- c("#FBF3E4", "#F3CB7E", "#C2557A", "#45102D")   # Color scale for heatmap

# Custom CSS for segmented radio buttons
css_seg <- "
  .seg { margin-bottom:8px; }
  .seg .shiny-options-group { display:flex; }
  .seg .shiny-options-group > * { flex:1; margin:0 !important; padding:0 !important; }
  .seg .form-check { padding-left:0; }
  .seg label { display:block; margin:0; }
  .seg input[type=radio] { position:absolute; opacity:0; pointer-events:none; }
  .seg label span { display:block; text-align:center; padding:5px 6px; border:1px solid @MAROON@; margin-left:-1px;
    font-size:13px; cursor:pointer; color:@MAROON@; background:#fff; }
  .seg input[type=radio]:checked + span { background:@MAROON@; color:#fff; font-weight:700; }
  .seg input[type=radio]:focus-visible + span { outline:2px solid @YELLOW@; outline-offset:1px; }
"

css <- paste0(app_css, gsub("@YELLOW@", YELLOW, gsub("@MAROON@", MAROON, css_seg, fixed = TRUE), fixed = TRUE))

# UI layout
ui <- page_fluid(
  theme = bs_theme(version = 5, bg = CREAM, fg = INK, primary = MAROON,
                   base_font = font_google("Source Sans 3", local = FALSE),
                   heading_font = font_google("Libre Baskerville", local = FALSE)),
  padding = 0,
  
  # Inject CSS and first-visit tour trigger
  tags$head(
    tags$style(HTML(css)),
    tags$script(HTML("$(document).on('shiny:connected', function() {
      if (!localStorage.getItem('kolTourSeen')) Shiny.setInputValue('first_visit', true, {priority: 'event'});
    });"))
  ),
  
  # Top header bar
  div(class = "hdr",
      div(div(class = "over", "Chapter 2 of 4 · The Volume Problem"), h1("Kolkata Road Crashes 2021–2023"),
          div(class = "sub", sprintf("Kolkata Traffic Police records · %s cases", fmt(N_TOTAL)))),
      div(actionButton("how", "? How to use", class = "btn-hdr"), actionButton("reset_top", "Reset all", class = "btn-hdr"))),
  
  # Main sidebar and dashboard content
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 300, bg = "#fff",
      div(class = "sidebar-label", "Filters"),
      checkboxGroupInput("year", "Year", choices = YEARS, selected = YEARS, inline = TRUE),
      tags$label("Severity"),
      div(class = "seg", radioButtons("severity", NULL, inline = TRUE, selected = DEFAULT_SEV, choices = c("All" = "all", "Fatal" = "fatal", "Non-Fatal" = "nonfatal"))),
      selectInput("vehicle", "Vehicle group (offending)", choices = VEH_LVLS),
      selectInput("road_user", "Road user (victim)", choices = RU_LVLS),
      sliderInput("hours", "Hour of day", min = 0, max = 23, value = c(0, 23), step = 1, post = ":00", sep = ""),
      selectizeInput("station", "Police station", choices = STN_LVLS, multiple = TRUE, options = list(placeholder = "Search...")),
      uiOutput("countbox"),
      uiOutput("chips"),
      div(downloadButton("dl", "Download CSV")),
      div(style = "margin-top:12px;", actionLink("about", "ⓘ About the data & source", class = "about"))
    ),
    
    # Dashboard main panel
    div(style = "padding:14px 22px 20px;",
        uiOutput("story"),
        div(class = "lede", "Click a bar, a heatmap cell or a map circle to filter everything else."),
        uiOutput("empty_banner"),
        uiOutput("kpis"),
        
        # Top row: hourly and monthly charts
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          div(class = "panel", h4("Crashes by hour of day"), uiOutput("hour_hint"),
              plotlyOutput("hour_plot", height = 300)),
          div(class = "panel", h4("Crashes per month"), uiOutput("month_hint"),
              plotlyOutput("month_plot", height = 300))
        ),
        
        # Bottom row: station heatmap and map
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          div(class = "panel", h4("Top 12 police stations by hour"),
              div(class = "hint", "Darker cell = more crashes. Click a cell to filter to that station."),
              plotlyOutput("heat_plot", height = 400)),
          div(class = "panel", h4("Where crashes recur"),
              radioButtons("map_mode", NULL, inline = TRUE,
                           choices = c("Areas (police stations)" = "areas", "Points" = "points")),
              uiOutput("map_hint"),
              leafletOutput("map", height = 350))
        )
    )
  )
)

# Server logic
server <- function(input, output, session) {
  
  # Filter dataset based on sidebar selections
  filtered <- reactive({
    d <- crashes
    d <- d[d$year %in% as.integer(input$year), , drop = FALSE]
    if (identical(input$severity, "fatal"))    d <- d[d$severity == "Fatal", , drop = FALSE]
    if (identical(input$severity, "nonfatal")) d <- d[d$severity == "Non-fatal", , drop = FALSE]
    if (input$vehicle   != "All vehicles")   d <- d[d$vehicle   == input$vehicle, , drop = FALSE]
    if (input$road_user != "All road users") d <- d[d$road_user == input$road_user, , drop = FALSE]
    if (length(input$station) > 0)           d <- d[d$station %in% input$station, , drop = FALSE]
    if (!identical(as.integer(input$hours), c(0L, 23L)))
      d <- d[!is.na(d$hour) & d$hour >= input$hours[1] & d$hour <= input$hours[2], , drop = FALSE]
    d
  })
  
  # Reset all filters to default values
  reset_all <- function() {
    updateCheckboxGroupInput(session, "year", selected = YEARS)
    updateRadioButtons(session, "severity", selected = DEFAULT_SEV)
    updateSelectInput(session, "vehicle", selected = "All vehicles")
    updateSelectInput(session, "road_user", selected = "All road users")
    updateSliderInput(session, "hours", value = c(0, 23))
    updateSelectizeInput(session, "station", selected = character(0))
    updateRadioButtons(session, "map_mode", selected = "areas")
  }
  observeEvent(input$reset_top, reset_all(), ignoreInit = TRUE)
  
  # Sidebar match count and active filter chips
  output$countbox <- renderUI({
    d <- filtered(); n <- nrow(d)
    pct <- if (n > 0) round(100 * mean(!is.na(d$lat)), 1) else 0
    div(class = "countbox", HTML(sprintf("Showing <b>%s</b> of %s crashes.<br><b>%s%%</b> have a map location.",
                                         fmt(n), fmt(N_TOTAL), pct)))
  })
  
  output$chips <- renderUI({
    chip <- function(key, label)
      tags$a(class = "chip", href = "#", `data-k` = key,
             onclick = "Shiny.setInputValue('chip_remove', this.dataset.k, {priority: 'event'}); return false;",
             paste0(label, " ×"))
    items <- list()
    if (length(input$year) != length(YEARS)) items <- c(items, list(chip("year", paste(sort(input$year), collapse = ", "))))
    if (!identical(input$severity, DEFAULT_SEV))
      items <- c(items, list(chip("severity", c(all = "All severities", fatal = "Fatal", nonfatal = "Non-Fatal")[[input$severity]])))
    if (input$vehicle != "All vehicles")     items <- c(items, list(chip("vehicle", input$vehicle)))
    if (input$road_user != "All road users") items <- c(items, list(chip("road_user", input$road_user)))
    if (!identical(as.integer(input$hours), c(0L, 23L)))
      items <- c(items, list(chip("hours", sprintf("%02d:00-%02d:59", input$hours[1], input$hours[2]))))
    for (s in input$station) items <- c(items, list(chip(paste0("station::", s), s)))
    if (length(items)) div(style = "margin-bottom:8px;", items)
  })
  
  # Remove individual filter when chip is clicked
  observeEvent(input$chip_remove, {
    k <- input$chip_remove
    if (k == "year")      updateCheckboxGroupInput(session, "year", selected = YEARS)
    if (k == "severity")  updateRadioButtons(session, "severity", selected = DEFAULT_SEV)
    if (k == "vehicle")   updateSelectInput(session, "vehicle", selected = "All vehicles")
    if (k == "road_user") updateSelectInput(session, "road_user", selected = "All road users")
    if (k == "hours")     updateSliderInput(session, "hours", value = c(0, 23))
    if (startsWith(k, "station::"))
      updateSelectizeInput(session, "station", selected = setdiff(input$station, sub("^station::", "", k)))
  })
  
  # Empty state warning banner
  output$empty_banner <- renderUI({
    if (nrow(filtered()) > 0) return(NULL)
    div(class = "empty", "No crashes match these filters. Use “Reset all” at the top right to start again.")
  })
  
  # Dynamic headline and KPI cards
  output$story <- renderUI({
    txt <- switch(input$severity,
                  nonfatal = "Minor Crashes Run from 10 am to Midnight, Every Day",
                  fatal    = "Fatal Crashes: When and Where They Happen",
                  "When and Where Crashes Happen, Fatal and Non-Fatal Together")
    div(class = "story", txt)
  })
  
  output$kpis <- renderUI({
    d <- filtered(); n <- nrow(d)
    h <- d$hour[!is.na(d$hour)]
    share <- if (length(h) > 0) paste0(round(100 * mean(h >= 10)), "%") else "-"
    dd <- d[!is.na(d$date), ]
    ratio <- "-"
    if (nrow(dd) >= 50) {
      we <- format(dd$date, "%u") %in% c("6", "7")
      ratio <- paste0(round(100 * (sum(we) / 2) / (sum(!we) / 5)), "%")
    }
    kpi <- function(v, t) column(4, div(class = "kpi", div(class = "v", v), div(class = "t", t)))
    fluidRow(style = "margin-bottom:16px;",
             kpi(fmt(n), sprintf("crashes shown, of %s in the records", fmt(N_TOTAL))),
             kpi(share, "happen between 10:00 and midnight"),
             kpi(ratio, "weekend crashes per day compared with a weekday"))
  })
  
  # Hourly crash distribution chart and click handler
  hourly <- reactive({
    d <- filtered()
    validate(need(nrow(d) > 0, "No crashes match these filters."))
    d <- d[!is.na(d$hour), ]
    validate(need(nrow(d) > 0, "No crashes with a recorded time match these filters."))
    tibble(hour = HOURS) |>
      left_join(count(d, hour), by = "hour") |>
      mutate(n = coalesce(n, 0L), window = hour >= 10)
  })
  
  output$hour_hint <- renderUI({
    h <- tryCatch(hourly(), error = function(e) NULL)
    if (is.null(h)) return(div(class = "hint", HTML("&nbsp;")))
    div(class = "hint", HTML(sprintf(
      "<b>%d%%</b> of crashes with a recorded time fall between 10:00 and midnight (dark bars). Click a bar to see only that hour.",
      round(100 * sum(h$n[h$window]) / sum(h$n)))))
  })
  
  output$hour_plot <- renderPlotly({
    h <- hourly()
    tip <- sprintf("%s to %02d:59<br>%s crashes (%s%%)", hour_lbl(h$hour), h$hour, fmt(h$n),
                   round(100 * h$n / sum(h$n), 1))
    plot_ly(h, x = ~hour, y = ~n, type = "bar", source = "hour", text = tip, hoverinfo = "text",
            marker = list(color = ifelse(h$window, MAROON, QUIET))) |>
      tidy(xaxis = list(title = "Hour of day", tickvals = seq(0, 23, 3), ticktext = hour_lbl(seq(0, 23, 3)),
                        range = c(-0.6, 23.6)),
           yaxis = list(title = "Crashes", rangemode = "tozero", gridcolor = GRID, zeroline = FALSE),
           shapes = list(list(type = "line", x0 = 9.5, x1 = 9.5, y0 = 0, y1 = 1, yref = "paper",
                              line = list(color = "#777777", dash = "dash", width = 1))),
           annotations = list(list(x = 9.7, y = 1, yref = "paper", text = "10:00", showarrow = FALSE,
                                   xanchor = "left", yanchor = "top", font = list(size = 11, color = "#555555")))) |>
      event_register("plotly_click")
  })
  
  observeEvent(event_data("plotly_click", source = "hour", priority = "event"), {
    h <- event_data("plotly_click", source = "hour", priority = "event")$x
    if (is.null(h)) return()
    h <- as.integer(round(h))
    if (identical(as.integer(input$hours), c(h, h))) updateSliderInput(session, "hours", value = c(0, 23))
    else updateSliderInput(session, "hours", value = c(h, h))
  })
  
  # Monthly trend line chart with Puja and COVID markers
  monthly <- reactive({
    d <- filtered()
    validate(need(nrow(d) > 0, "No crashes match these filters."))
    yrs <- sort(as.integer(input$year))
    validate(need(length(yrs) > 0, "Select at least one year."))
    grid <- tibble(month_start = seq(as.Date(sprintf("%d-01-01", min(yrs))),
                                     as.Date(sprintf("%d-12-01", max(yrs))), by = "month"))
    grid <- grid[as.integer(format(grid$month_start, "%Y")) %in% yrs, ]
    grid |> left_join(count(d, month_start), by = "month_start") |> mutate(n = coalesce(n, 0L))
  })
  
  output$month_hint <- renderUI({
    m <- tryCatch(monthly(), error = function(e) NULL)
    if (is.null(m) || sum(m$n) == 0) return(div(class = "hint", HTML("&nbsp;")))
    hi <- m[which.max(m$n), ]; lo <- m[which.min(m$n), ]
    div(class = "hint", HTML(sprintf(
      "Busiest month <b>%s</b> (%d crashes), quietest <b>%s</b> (%d). Grey band = COVID restrictions, May-June 2021.",
      format(hi$month_start, "%b %Y"), hi$n, format(lo$month_start, "%b %Y"), lo$n)))
  })
  
  output$month_plot <- renderPlotly({
    m <- monthly()
    puja <- as.Date(sprintf("%d-10-01", unique(as.integer(format(m$month_start, "%Y")))))
    shapes <- lapply(puja, function(x) list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
                                            line = list(color = YELLOW, dash = "dot", width = 2)))
    notes <- lapply(puja, function(x) list(x = x, y = 1, yref = "paper", text = "Puja", showarrow = FALSE,
                                           yanchor = "bottom", font = list(size = 11, color = "#8a6200")))
    if (as.Date("2021-05-01") %in% m$month_start) {
      shapes <- c(list(list(type = "rect", x0 = "2021-05-01", x1 = "2021-06-30", y0 = 0, y1 = 1, yref = "paper",
                            fillcolor = "rgba(150,140,148,0.25)", line = list(width = 0), layer = "below")), shapes)
      notes <- c(notes, list(list(x = "2021-06-01", y = 0.04, yref = "paper", text = "COVID", showarrow = FALSE,
                                  font = list(size = 11, color = "#555555"))))
    }
    plot_ly(m, x = ~month_start, y = ~n, type = "scatter", mode = "lines+markers",
            line = list(color = MAROON, width = 2.5), marker = list(color = MAROON, size = 6),
            text = ~sprintf("%s<br>%d crashes", format(month_start, "%B %Y"), n), hoverinfo = "text") |>
      tidy(margin = list(l = 50, r = 15, t = 22, b = 45),
           xaxis = list(title = "", type = "date", tickformat = "%b %Y", dtick = "M6", gridcolor = GRID),
           yaxis = list(title = "Crashes per month", rangemode = "tozero", gridcolor = GRID, zeroline = FALSE),
           shapes = shapes, annotations = notes)
  })
  
  # Police station by hour heatmap and click handler
  output$heat_plot <- renderPlotly({
    d <- filtered()
    validate(need(nrow(d) > 0, "No crashes match these filters."))
    d <- d[!is.na(d$hour) & !is.na(d$station), ]
    validate(need(nrow(d) > 0, "No crashes with a recorded time match these filters."))
    top <- count(d, station, sort = TRUE) |> slice_head(n = 12)
    g <- d |> filter(station %in% top$station) |> count(station, hour) |>
      complete(station = top$station, hour = HOURS, fill = list(n = 0L))
    tip <- sprintf("%s, %s: %d crashes", g$station, hour_lbl(g$hour), g$n)
    plot_ly(g, x = ~hour, y = ~station, z = ~n, type = "heatmap", source = "heat", showscale = FALSE,
            colorscale = lapply(seq_along(HEAT), function(i) list((i - 1) / (length(HEAT) - 1), HEAT[i])),
            xgap = 1, ygap = 1, text = tip, hoverinfo = "text") |>
      add_trace(type = "scatter", mode = "text", x = g$hour, y = g$station, z = NULL,
                text = ifelse(g$n > 0, g$n, ""), hoverinfo = "none",
                textfont = list(size = 9, color = ifelse(g$n > max(g$n) * 0.5, "#FFFFFF", INK))) |>
      tidy(margin = list(l = 130, r = 10, t = 10, b = 45), showlegend = FALSE,
           xaxis = list(title = "Hour of day", tickvals = seq(0, 23, 3), ticktext = hour_lbl(seq(0, 23, 3)),
                        range = c(-0.5, 23.5)),
           yaxis = list(title = "", categoryorder = "array", categoryarray = rev(top$station))) |>
      event_register("plotly_click")
  })
  
  observeEvent(event_data("plotly_click", source = "heat", priority = "event"), {
    st <- event_data("plotly_click", source = "heat", priority = "event")$y
    if (is.null(st) || !is.character(st)) return()
    now <- input$station
    updateSelectizeInput(session, "station", selected = if (st %in% now) setdiff(now, st) else c(now, st))
  })
  
  # Interactive Leaflet map setup and layer updates
  output$map_hint <- renderUI({
    d <- filtered()
    div(class = "hint", sprintf("%s of %s filtered crashes have coordinates and appear here. %s",
                                fmt(sum(!is.na(d$lat))), fmt(nrow(d)),
                                if (identical(input$map_mode, "areas")) "Click a circle to filter to that station." else "Zoom in to split the clusters. Yellow ring = fatal crash."))
  })
  
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      setView(lng = 88.35, lat = 22.55, zoom = 11)
  })
  
  cluster_icon <- htmlwidgets::JS(sprintf("function(cluster) {
      var n = cluster.getChildCount(); var s = n < 10 ? 30 : (n < 100 ? 38 : 46);
      return L.divIcon({html: '<div style=\"background:%s;border:2px solid #fff;color:#fff;border-radius:50%%;width:' + s +
        'px;height:' + s + 'px;line-height:' + (s - 4) + 'px;text-align:center;font-weight:700\">' + n + '</div>',
        className: '', iconSize: L.point(s, s)});
    }", MAROON))
  
  observe({
    d <- filtered(); d <- d[!is.na(d$lat), ]
    proxy <- leafletProxy("map") |> clearMarkers() |> clearMarkerClusters() |> clearShapes() |> clearControls()
    if (nrow(d) == 0) return()
    if (input$map_mode == "areas") {
      a <- d |> group_by(station) |> summarise(n = n(), lat = median(lat), lon = median(lon), .groups = "drop")
      pal <- colorNumeric(HEAT, domain = a$n)
      proxy |>
        addCircleMarkers(data = a, lng = ~lon, lat = ~lat, layerId = ~station,
                         radius = ~pmax(6, sqrt(n) * 2.2), fillColor = ~pal(n), fillOpacity = 0.85,
                         color = MAROON, weight = 1,
                         popup = ~sprintf("<b>%s</b><br>%d crashes with a location", station, n),
                         label = ~sprintf("%s: %d", station, n)) |>
        addLegend(position = "bottomright", pal = pal, values = a$n, title = "Crashes")
    } else {
      proxy |>
        addCircleMarkers(data = d, lng = ~lon, lat = ~lat, radius = ~ifelse(severity == "Fatal", 7, 5),
                         fillColor = MAROON, fillOpacity = 0.7,
                         color = ~ifelse(severity == "Fatal", YELLOW, "#ffffff"),
                         weight = ~ifelse(severity == "Fatal", 3, 1),
                         clusterOptions = markerClusterOptions(showCoverageOnHover = FALSE, maxClusterRadius = 45,
                                                               iconCreateFunction = cluster_icon),
                         popup = ~sprintf("<b>%s</b> &middot; %s<br>%s %d, %s<br>Offending: %s<br>Victim: %s<br>%s",
                                          station, sub("Non-fatal", "Non-Fatal", severity), ifelse(is.na(month_start), "", format(month_start, "%b")), year,
                                          ifelse(is.na(hour), "time n/a", hour_lbl(hour)),
                                          vehicle, road_user, ifelse(is.na(place), "", place)))
    }
  })
  
  observeEvent(input$map_marker_click, {
    st <- input$map_marker_click$id
    if (is.null(st) || !identical(input$map_mode, "areas")) return()
    now <- input$station
    updateSelectizeInput(session, "station", selected = if (st %in% now) setdiff(now, st) else c(now, st))
  })
  
  # Help tour and data source modals
  tour <- function() modalDialog(
    title = "How to use this dashboard", easyClose = TRUE,
    tags$ol(
      tags$li(tags$b("The filter sidebar"), " controls every chart on the page. Year, severity, vehicle, road user, hour and police station all combine."),
      tags$li(tags$b("Click a bar, a heatmap cell or a map circle"), " to filter to that hour or station. Click again to undo."),
      tags$li(tags$b("Reset all"), " puts every filter back to its starting position.")),
    footer = tagList(tags$button(type = "button", class = "btn btn-primary", `data-bs-dismiss` = "modal",
                                 onclick = "localStorage.setItem('kolTourSeen', '1');", "Got it")))
  
  observeEvent(input$how, showModal(tour()))
  observeEvent(input$first_visit, showModal(tour()))
  
  observeEvent(input$about, showModal(modalDialog(
    title = "About the data and where it comes from", easyClose = TRUE, footer = modalButton("Close"),
    p("Kolkata Police road accident records for 2021-2023 (fatal and non-fatal sheets), shared by the Kolkata Police Department.",
      "The sheets were typed by hand at many police stations, so every field was standardised before charting."),
    tags$ul(
      tags$li(sprintf("Time: %s of %s crashes have no readable time and are left out of the hour charts.",
                      fmt(sum(is.na(crashes$hour))), fmt(N_TOTAL))),
      tags$li(sprintf("Location: coordinates came in about ten formats. %s%% have a usable position inside Kolkata, so the map shows a subset.",
                      round(100 * mean(!is.na(crashes$lat)), 1))),
      tags$li(sprintf("Vehicles and victims were grouped into eight categories each; raw police station labels were merged into %d stations.",
                      sum(!is.na(unique(crashes$station))))),
      tags$li("Month and year come from the accident date when it falls in the sheet year, otherwise from the sheet's month column."),
      tags$li("The fatal sheets have no station column, so a fatal case's police station is read from its case reference (for example Bowbazar P.S. Case No. 116)."),
      tags$li("Counts are crashes recorded by the police. They are not rates, because there are no traffic volumes in the data.")))))
  
  # CSV export handler
  output$dl <- downloadHandler(
    filename = function() "kolkata_crashes_filtered.csv",
    content = function(file) {
      out <- filtered()[, c("severity", "date", "year", "month", "hour", "station", "place", "vehicle", "road_user", "lat", "lon")]
      write.csv(out, file, row.names = FALSE, na = "")
    })
}

shinyApp(ui, server)