needed  <- c("shiny", "bslib", "dplyr", "tidyr", "plotly", "readxl", "stringr", "tibble")  # Install any missing package the first time 
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing, repos = "https://cloud.r-project.org")
}

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(plotly)

source("R/clean_data.R")    # Cleaning helpers 
source("R/clean_fatal.R")   # adds the fatal sheets using the same helpers
source("R/shared_theme.R")  # colours, fonts, tidy(), fmt() and the stylesheet shared by every chapter

# Fallback in case load_all_crashes() is not in your R folder
if (!exists("load_all_crashes")) {
  load_all_crashes <- function(path) {
    fa <- load_fatal(path)
    fa$persons_killed <- fa$persons
    fa$sex <- c(M = "Male", F = "Female")[fa$sex]
    bind_rows(load_nonfatal(path), fa) |> mutate(crash_id = row_number())
  }
}

XLSX <- "data/data.xlsx"
if (!file.exists(XLSX)) {
  found <- list.files("data", pattern = "\\.xlsx$", full.names = TRUE)
  if (length(found) == 0)
    stop("Cannot find the data. Put data.xlsx inside the 'data' folder of pranav_chap1, then run the app again.")
  XLSX <- found[1]
}
CACHE <- "data/all_crashes_clean.rds"  # The cleaned result is cached in data/all_crashes_clean.rds so later starts are instant.
crashes <- if (file.exists(CACHE) && file.mtime(CACHE) >= file.mtime(XLSX)) {
  readRDS(CACHE)
} else {
  message("Cleaning the data (about 20 seconds, first run only)...")
  df <- suppressWarnings(load_all_crashes(XLSX))
  saveRDS(df, CACHE)
  df
} # If data.xlsx is newer than the cache, it is rebuilt.


sx <- toupper(substr(as.character(crashes$sex), 1, 1))
crashes$sex <- ifelse(sx == "M", "Male", ifelse(sx %in% c("F", "W"), "Female", NA_character_))  # The fatal sheets store sex as a single letter (M / F); spell it out so the text reads "a male pedestrian"

N_TOTAL <- nrow(crashes)                                # Total crash records loaded
N_FATAL <- sum(crashes$severity == "Fatal")             # Total fatal crash records
YEARS   <- sort(unique(crashes$year))                   # Sorted years available in dataset
pct     <- function(x) paste0(round(100 * x), "%")      # Format proportion as rounded percentage string

VEH_ALL  <- c("Car / Taxi", "Two-wheeler", "Bus", "Truck / Goods",
              "Auto / Rickshaw", "Bicycle", "Other", "Unknown")
VEH_ALL  <- VEH_ALL[VEH_ALL %in% crashes$vehicle]       # Keep vehicle categories present in data
VEH_LVLS <- c("All vehicles", VEH_ALL)                  # Vehicle filter options including "All vehicles"
RU_LVLS  <- c("All road users", "Pedestrian", "Rider", "Pillion rider", "Cyclist",
              "Passenger", "Driver / Owner", "Other", "Unknown")
RU_LVLS  <- RU_LVLS[RU_LVLS == "All road users" | RU_LVLS %in% crashes$road_user] # Valid road user filter options
STN_LVLS <- names(sort(table(crashes$station), decreasing = TRUE)) # Police stations ordered by crash count


# Colours, fonts, tidy() and the stylesheet (app_css) come from R/shared_theme.R

css <- paste0(app_css, sprintf("
  .callout b { color:%s; }
  .victim .row2 span:last-child { font-weight:700; font-variant-numeric:tabular-nums; }
  .radio-inline-sm .shiny-options-group { margin-top:-2px; }
", MAROON))

ui <- page_fluid(
  theme = app_theme,
  padding = 0,
  tags$head(
    tags$style(HTML(css)),
    tags$script(HTML("$(document).on('shiny:connected', function() {
      if (!localStorage.getItem('kolTourSeenCh1')) Shiny.setInputValue('first_visit', true, {priority: 'event'});
    });")) # Trigger first-visit tour modal once per browser
  ),
  
  div(class = "hdr",
      div(div(class = "over", "Chapter 1 of 4 · Cars crash, heavy vehicles kill"),
          h1("Kolkata Road Crashes 2021–2023"),
          div(class = "sub", sprintf("Kolkata Traffic Police records · %s crashes, %s of them fatal",
                                     fmt(N_TOTAL), fmt(N_FATAL)))),
      div(actionButton("how", "? How to use", class = "btn-hdr"),
          actionButton("reset_top", "Reset all", class = "btn-hdr"))), # Top header bar with help and reset buttons
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 300, bg = "#fff",
      div(class = "sidebar-label", "Filters"),
      checkboxGroupInput("year", "Year", choices = YEARS, selected = YEARS, inline = TRUE),
      tags$label("Severity"),
      div(class = "seg", radioButtons("severity", NULL, inline = TRUE, choices = c("All" = "All", "Fatal" = "Fatal", "Non-Fatal" = "Non-fatal"), selected = "All")),
      selectInput("vehicle", "Vehicle group (offending)", choices = VEH_LVLS),
      selectInput("road_user", "Road user (victim)", choices = RU_LVLS),
      sliderInput("hours", "Hour of day", min = 0, max = 23, value = c(0, 23), step = 1, post = ":00", sep = ""),
      selectizeInput("station", "Police station", choices = STN_LVLS, multiple = TRUE,
                     options = list(placeholder = "Search...")),
      uiOutput("countbox"),
      uiOutput("chips"),
      div(downloadButton("dl", "Download CSV")),
      div(style = "margin-top:12px;", actionLink("about", "ⓘ About the data & source", class = "about"))
    ), # Sidebar filter controls, match counter, active chips, export, and about link
    
    div(style = "padding:14px 22px 20px;",
        div(class = "story", "Cars Crash Most; Trucks and Buses Kill Most"),
        div(class = "lede", uiOutput("scenario", inline = TRUE)),
        uiOutput("empty_banner"),
        uiOutput("kpis"),
        layout_columns(
          col_widths = c(7, 5), fill = FALSE,
          div(class = "panel", h4("Share of all crashes vs share of fatal crashes"),
              div(class = "radio-inline-sm",
                  radioButtons("measure", NULL, inline = TRUE,
                               choices = c("Share %" = "share", "Count" = "count"))),
              uiOutput("dumbbell_hint"),
              div(role = "img", `aria-label` = "Dot chart comparing each vehicle group's share of all crashes with its share of fatal crashes", plotlyOutput("dumbbell_plot", height = 330))),
          div(class = "panel", h4("How deadly is a crash with each vehicle?"),
              uiOutput("deadly_hint"),
              div(role = "img", `aria-label` = "Bar chart of fatal crashes per 100 crashes for each vehicle group, with 95% ranges", plotlyOutput("deadly_plot", height = 360)))
        ), # Top row: vehicle share/count dumbbell chart and fatality rate chart
        layout_columns(
          col_widths = c(7, 5), fill = FALSE,
          div(class = "panel", uiOutput("year_title"),
              div(class = "hint", "The two charts have their own scales: there are about ten times more non-fatal crashes. A rise in recorded minor crashes may partly reflect better reporting."),
              div(role = "img", `aria-label` = "Line charts of fatal and non-fatal crashes per year", plotlyOutput("year_plot", height = 280))),
          div(class = "panel victim", h4("Typical victim"),
              uiOutput("victim"))
        ) # Bottom row: yearly fatal vs non-fatal trend and typical fatal victim summary
    )
  )
)

server <- function(input, output, session) {
  
  # Every filter except severity and vehicle.
  base <- reactive({
    d <- crashes
    d <- d[d$year %in% as.integer(input$year), , drop = FALSE]
    if (input$road_user != "All road users") d <- d[d$road_user == input$road_user, , drop = FALSE]
    if (length(input$station) > 0)           d <- d[!is.na(d$station) & d$station %in% input$station, , drop = FALSE]
    if (!identical(as.integer(input$hours), c(0L, 23L)))
      d <- d[!is.na(d$hour) & d$hour >= input$hours[1] & d$hour <= input$hours[2], , drop = FALSE]
    d
  })   #  The vehicle charts use this one, because they compare vehicles and severities with each other.
  
  filtered <- reactive({
    d <- base()
    if (input$severity != "All")         d <- d[d$severity == input$severity, , drop = FALSE]
    if (input$vehicle != "All vehicles") d <- d[d$vehicle == input$vehicle, , drop = FALSE]
    d
  })  # Every filter (used by the KPIs, the year chart, the victim card and the download)
  
  # reset, from the header, the sidebar or the empty-result banner
  reset_all <- function() {
    updateCheckboxGroupInput(session, "year", selected = YEARS)
    updateRadioButtons(session, "severity", selected = "All")
    updateSelectInput(session, "vehicle", selected = "All vehicles")
    updateSelectInput(session, "road_user", selected = "All road users")
    updateSliderInput(session, "hours", value = c(0, 23))
    updateSelectizeInput(session, "station", selected = character(0))
    updateRadioButtons(session, "measure", selected = "share")
  }
  # One observer per button.
  observeEvent(input$reset_top,    reset_all())
  observeEvent(input$reset_inline, reset_all())
  
  output$countbox <- renderUI({
    d <- filtered(); n <- nrow(d)
    div(class = "countbox", HTML(sprintf("Showing <b>%s</b> of %s crashes.<br><b>%s</b> of them fatal.",
                                         fmt(n), fmt(N_TOTAL), fmt(sum(d$severity == "Fatal")))))
  }) # Render sidebar record and fatal crash count summary
  
  output$chips <- renderUI({
    chip <- function(key, label)
      tags$a(class = "chip", href = "#", `data-k` = key,
             onclick = "Shiny.setInputValue('chip_remove', this.dataset.k, {priority: 'event'}); return false;",
             paste0(label, " ×"))
    items <- list()
    if (length(input$year) != length(YEARS)) items <- c(items, list(chip("year", paste(sort(input$year), collapse = ", "))))
    if (input$severity != "All")             items <- c(items, list(chip("severity", sub("Non-fatal", "Non-Fatal", input$severity))))
    if (input$vehicle != "All vehicles")     items <- c(items, list(chip("vehicle", input$vehicle)))
    if (input$road_user != "All road users") items <- c(items, list(chip("road_user", input$road_user)))
    if (!identical(as.integer(input$hours), c(0L, 23L)))
      items <- c(items, list(chip("hours", sprintf("%02d:00–%02d:59", input$hours[1], input$hours[2]))))
    for (s in input$station) items <- c(items, list(chip(paste0("station::", s), s)))
    if (length(items)) div(style = "margin-bottom:8px;", items)
  }) # Render removable filter chips for active sidebar selections
  
  observeEvent(input$chip_remove, {
    k <- input$chip_remove
    if (k == "year")      updateCheckboxGroupInput(session, "year", selected = YEARS)
    if (k == "severity")  updateRadioButtons(session, "severity", selected = "All")
    if (k == "vehicle")   updateSelectInput(session, "vehicle", selected = "All vehicles")
    if (k == "road_user") updateSelectInput(session, "road_user", selected = "All road users")
    if (k == "hours")     updateSliderInput(session, "hours", value = c(0, 23))
    if (startsWith(k, "station::"))
      updateSelectizeInput(session, "station", selected = setdiff(input$station, sub("^station::", "", k)))
  }) # Clear an individual filter when its chip is clicked
  
  output$empty_banner <- renderUI({
    if (nrow(filtered()) > 0) return(NULL)
    div(class = "empty", "No crashes match these filters. ", actionLink("reset_inline", "Reset all"))
  }) # Show empty-state reset prompt when no crashes match filters
  
  # The most common fatal victim
  output$scenario <- renderUI({
    f <- crashes[crashes$severity == "Fatal" & crashes$road_user != "Unknown" & !is.na(crashes$sex) & !is.na(crashes$age), ]
    f$age_band <- cut(f$age, c(0, 17, 29, 44, 59, 120), labels = c("under 18", "18–29", "30–44", "45–59", "60 or older"))
    top <- count(f, road_user, sex, age_band, sort = TRUE)[1, ]
    HTML(sprintf("Out of everyone killed on Kolkata's roads in 2021–2023, the most common victim is a <b>%s %s aged %s</b>.",
                 tolower(top$sex), tolower(top$road_user), top$age_band))
  })
  
  # This shows the headline numbers
  output$kpis <- renderUI({
    d <- filtered(); f <- d[d$severity == "Fatal", ]
    killed <- sum(f$persons_killed, na.rm = TRUE)
    ped <- if (nrow(f) > 0) pct(mean(f$road_user == "Pedestrian")) else "–"
    heavy <- if (nrow(f) > 0) pct(mean(f$vehicle %in% c("Truck / Goods", "Bus"))) else "–"
    kpi <- function(v, t) column(4, div(class = "kpi", div(class = "v", v), div(class = "t", t)))
    fluidRow(style = "margin-bottom:16px;",
             kpi(fmt(nrow(d)), sprintf("crashes shown, of %s", fmt(N_TOTAL))),
             kpi(ped, "of fatal crashes killed a pedestrian"),
             kpi(heavy, "of fatal crashes involved a truck or bus"))
  })
  
  # Share (or count) of all crashes vs fatal crashes 
  by_vehicle <- reactive({
    d <- base()
    validate(need(nrow(d) > 0, "No crashes match these filters."))
    v <- d |> count(vehicle, severity) |>
      complete(vehicle = VEH_ALL, severity = c("Fatal", "Non-fatal"), fill = list(n = 0L)) |>
      pivot_wider(names_from = severity, values_from = n) |>
      mutate(all_n = Fatal + `Non-fatal`,
             share_all = 100 * all_n / max(sum(all_n), 1),
             share_fatal = 100 * Fatal / max(sum(Fatal), 1),
             per100 = ifelse(all_n > 0, 100 * Fatal / all_n, NA))
    
    # 95% Wilson interval for the fatal rate: honest about small groups 
    z <- 1.96; n <- pmax(v$all_n, 1); p <- v$Fatal / n
    centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
    half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
    v$lo100 <- 100 * pmax(0, centre - half)
    v$hi100 <- 100 * pmin(1, centre + half)
    v
  }) # Aggregate crash counts, severity shares, fatality rates, and Wilson CIs by vehicle
  
  output$dumbbell_hint <- renderUI({
    v <- tryCatch(by_vehicle(), error = function(e) NULL)
    if (is.null(v) || sum(v$Fatal) == 0) return(div(class = "hint", HTML("&nbsp;")))
    top <- v[which.max(v$share_fatal - v$share_all), ]
    div(class = "hint", HTML(sprintf(
      "Hollow dot = all crashes, filled dot = fatal crashes. <b>%s</b> has the biggest jump: %.0f%% of crashes but %.0f%% of fatal ones. Click a row to filter by that vehicle.",
      top$vehicle, top$share_all, top$share_fatal)))
  }) # Dynamic subtitle highlighting largest fatal-vs-all share gap
  
  output$dumbbell_plot <- renderPlotly({
    v <- by_vehicle()
    share <- identical(input$measure, "share")
    v <- v |> mutate(x_all = if (share) share_all else all_n,
                     x_fat = if (share) share_fatal else Fatal) |>
      arrange(x_fat)
    v$vehicle <- factor(v$vehicle, levels = v$vehicle)
    sel <- input$vehicle
    on  <- sel == "All vehicles" | v$vehicle == sel
    unit <- if (share) "%" else ""
    tip_all <- sprintf("%s<br>%s crashes (%.1f%% of all)", v$vehicle, fmt(v$all_n), v$share_all)
    tip_fat <- sprintf("%s<br>%s fatal crashes (%.1f%% of fatal)", v$vehicle, fmt(v$Fatal), v$share_fatal)
    plot_ly(v, source = "veh") |>
      add_segments(x = ~x_all, xend = ~x_fat, y = ~vehicle, yend = ~vehicle, hoverinfo = "none",
                   line = list(color = QUIET, width = 4), showlegend = FALSE) |>
      add_markers(x = ~x_all, y = ~vehicle, name = "All crashes", text = tip_all, hoverinfo = "text",
                  marker = list(color = "#FFFFFF", size = 13,
                                line = list(color = ifelse(on, YELLOW, QUIET), width = 3))) |>
      add_markers(x = ~x_fat, y = ~vehicle, name = "Fatal crashes", text = tip_fat, hoverinfo = "text",
                  marker = list(color = ifelse(on, MAROON, QUIET), size = 13)) |>
      tidy(margin = list(l = 110, r = 15, t = 30, b = 45),
           xaxis = list(title = if (share) "Share of crashes" else "Number of crashes",
                        ticksuffix = unit, rangemode = "tozero", gridcolor = GRID, zeroline = FALSE),
           yaxis = list(title = "", ticksuffix = "  "),
           legend = list(orientation = "h", x = 0, y = 1.12)) |>
      event_register("plotly_click")
  }) # Render dumbbell chart comparing all vs fatal vehicle distributions
  
  observeEvent(event_data("plotly_click", source = "veh", priority = "event"), {
    veh <- event_data("plotly_click", source = "veh", priority = "event")$y
    if (is.null(veh) || !is.character(veh)) return()
    updateSelectInput(session, "vehicle", selected = if (identical(input$vehicle, veh)) "All vehicles" else veh)
  }) # Toggle vehicle filter when clicking a row on the dumbbell chart
  
  # Fatal crashes per 100 crashes 
  output$deadly_hint <- renderUI({
    div(class = "hint", "Fatal crashes for every 100 crashes involving that vehicle. The thin line is the 95% range: a long line means few crashes, so the rate is uncertain. Vehicles with fewer than 20 crashes are greyed out.")
  })
  
  output$deadly_plot <- renderPlotly({
    v <- by_vehicle() |> filter(all_n > 0) |> arrange(per100)
    v$vehicle <- factor(v$vehicle, levels = v$vehicle)
    sel <- input$vehicle
    col <- ifelse(v$all_n < 20, QUIET,
                  ifelse(sel == "All vehicles" | v$vehicle == sel, MAROON, QUIET))
    tip <- sprintf("%s<br>%.1f fatal per 100 crashes<br>95%% range: %.1f to %.1f<br>(%s fatal of %s)",
                   v$vehicle, v$per100, v$lo100, v$hi100, fmt(v$Fatal), fmt(v$all_n))
    plot_ly(v, x = ~per100, y = ~vehicle, type = "bar", orientation = "h", source = "deadly",
            text = tip, hoverinfo = "text", textposition = "none", marker = list(color = col),
            error_x = list(type = "data", symmetric = FALSE,   # no range for greyed-out small groups
                           array = ifelse(v$all_n < 20, 0, v$hi100 - v$per100),
                           arrayminus = ifelse(v$all_n < 20, 0, v$per100 - v$lo100), color = INK, thickness = 1.2, width = 4)) |>
      tidy(margin = list(l = 110, r = 15, t = 10, b = 45),
           xaxis = list(title = "Fatal crashes per 100 crashes", rangemode = "tozero", gridcolor = GRID, zeroline = FALSE),
           yaxis = list(title = "", ticksuffix = "  ")) |>
      event_register("plotly_click")
  }) # Render horizontal bar chart of fatal crash rate per 100 crashes with error bars
  
  observeEvent(event_data("plotly_click", source = "deadly", priority = "event"), {
    veh <- event_data("plotly_click", source = "deadly", priority = "event")$y
    if (is.null(veh) || !is.character(veh)) return()
    updateSelectInput(session, "vehicle", selected = if (identical(input$vehicle, veh)) "All vehicles" else veh)
  }) # Toggle vehicle filter when clicking a bar on the deadliness chart
  
  # Fatal vs Non-Fatal per year
  # Heading written from the data shown, so it stays true whatever the filters are
  output$year_title <- renderUI({
    d <- base()
    if (input$vehicle != "All vehicles") d <- d[d$vehicle == input$vehicle, , drop = FALSE]
    yrs <- sort(as.integer(input$year))
    if (nrow(d) == 0 || length(yrs) < 2) return(h4("Fatal and non-fatal crashes by year"))
    trend <- function(sev) {
      a <- sum(d$severity == sev & d$year == min(yrs)); b <- sum(d$severity == sev & d$year == max(yrs))
      if (b < a * 0.95) "fell" else if (b > a * 1.05) "rose" else "held steady"
    }
    h4(sprintf("Fatal crashes %s while non-fatal crashes %s, %d–%d", trend("Fatal"), trend("Non-fatal"), min(yrs), max(yrs)))
  })
  
  output$year_plot <- renderPlotly({
    d <- base()                                          # ignores severity: it compares the two
    if (input$vehicle != "All vehicles") d <- d[d$vehicle == input$vehicle, , drop = FALSE]
    validate(need(nrow(d) > 0, "No crashes match these filters."))
    yrs <- sort(as.integer(input$year))
    y <- d |> count(year, severity) |>
      complete(year = yrs, severity = c("Fatal", "Non-fatal"), fill = list(n = 0L))
    line_for <- function(sev, colour) {
      plot_ly(y[y$severity == sev, ], x = ~year, y = ~n, type = "scatter", mode = "lines+markers", name = sev,
              line = list(color = colour, width = 2.5), marker = list(color = colour, size = 8),
              text = ~sprintf("%d: %s %s crashes", year, fmt(n), tolower(sev)), hoverinfo = "text")
    }
    subplot(line_for("Fatal", MAROON), line_for("Non-fatal", YELLOW), nrows = 1, margin = 0.08) |>
      tidy(margin = list(l = 50, r = 15, t = 30, b = 35), showlegend = FALSE,
           xaxis  = list(tickvals = YEARS, gridcolor = GRID),
           xaxis2 = list(tickvals = YEARS, gridcolor = GRID),
           yaxis  = list(title = "Crashes", rangemode = "tozero", gridcolor = GRID, zeroline = FALSE),
           yaxis2 = list(rangemode = "tozero", gridcolor = GRID, zeroline = FALSE),
           annotations = list(
             list(text = "<b>Fatal</b>", x = 0, y = 1.08, xref = "paper", yref = "paper", xanchor = "left",
                  showarrow = FALSE, font = list(color = MAROON, size = 13)),
             list(text = "<b>Non-Fatal</b>", x = 0.54, y = 1.08, xref = "paper", yref = "paper", xanchor = "left",
                  showarrow = FALSE, font = list(color = "#8a6200", size = 13))))
  }) # Render side-by-side annual trend plots for fatal and non-fatal crashes
  
  # Typical victim of the fatal crashes in the current filter
  output$victim <- renderUI({
    f <- filtered(); f <- f[f$severity == "Fatal", ]
    if (nrow(f) == 0) return(div(class = "hint", "No fatal crashes match these filters."))
    mode_of <- function(x) { x <- x[!is.na(x) & x != "Unknown"]; if (!length(x)) "not recorded" else names(sort(table(x), decreasing = TRUE))[1] }
    share_of <- function(x, val) pct(mean(x == val, na.rm = TRUE))
    band <- cut(f$age, c(0, 17, 29, 44, 59, 120), labels = c("under 18", "18–29", "30–44", "45–59", "60+"))
    night <- ifelse(is.na(f$hour), NA, ifelse(f$hour >= 18 | f$hour < 6, "after dark (6 pm–6 am)", "in daytime (6 am–6 pm)"))
    ru <- mode_of(f$road_user); sx <- mode_of(f$sex); ag <- mode_of(as.character(band)); tm <- mode_of(night)
    tagList(
      div(class = "big", sprintf("A %s %s, aged %s, hit %s.", tolower(sx), tolower(ru), ag, tm)),
      div(class = "row2", span(sprintf("Victims who were %s", tolower(ru))), span(share_of(f$road_user, ru))),
      div(class = "row2", span(sprintf("Victims who were %s", tolower(sx))), span(share_of(f$sex, sx))),
      div(class = "row2", span(sprintf("Victims aged %s", ag)), span(share_of(as.character(band), ag))),
      div(class = "row2", span(sprintf("Crashes %s", tm)), span(share_of(night, tm))),
      div(class = "hint", style = "margin-top:8px;", sprintf("Based on %s fatal crashes in the current filters.", fmt(nrow(f))))
    )
  })
  
  # help, about and download 
  tour <- function() modalDialog(
    title = "How to use this dashboard", easyClose = TRUE,
    tags$ol(
      tags$li(tags$b("The filter sidebar"), " controls every chart on the page. Year, severity, vehicle, road user, hour and police station all combine."),
      tags$li(tags$b("Click a vehicle"), " on either vehicle chart to filter the page to it. Click it again to undo."),
      tags$li(tags$b("Reset all"), " puts every filter back to its starting position.")),
    footer = tagList(tags$button(type = "button", class = "btn btn-primary", `data-bs-dismiss` = "modal",
                                 onclick = "localStorage.setItem('kolTourSeenCh1', '1');", "Got it"))) # Build onboarding tour modal dialog
  
  observeEvent(input$how, showModal(tour()))         # Open tour modal when help button is clicked
  observeEvent(input$first_visit, showModal(tour())) # Open tour modal on first visit
  
  observeEvent(input$about, showModal(modalDialog(
    title = "About the data and where it comes from", easyClose = TRUE, footer = modalButton("Close"),
    p("Kolkata Police road accident records for 2021–2023 (all six sheets: fatal and non-fatal), shared by the Kolkata Police Department.",
      "The sheets were typed by hand at many police stations, so every field was standardised before charting, with the same rules as Chapter 2."),
    tags$ul(
      tags$li(sprintf("Fatal crashes: %s cases in which %s people died. Non-fatal crashes: %s.",
                      fmt(N_FATAL), fmt(sum(crashes$persons_killed, na.rm = TRUE)), fmt(N_TOTAL - N_FATAL))),
      tags$li("Vehicle = the offending vehicle named by the police, grouped into eight categories. 'Unknown' means the record does not say (often a hit-and-run)."),
      tags$li("The two vehicle charts ignore the severity and vehicle filters, because they compare vehicles and severities with each other."),
      tags$li("Victim age, sex and helmet use are recorded for fatal crashes only, so the Typical victim card uses fatal crashes."),
      tags$li("The thin lines on the deadliness chart are 95% Wilson intervals: the range the true rate probably lies in, given how few crashes some vehicles have."),
      tags$li("Counts are crashes recorded by the police. They are not rates, because the data has no traffic volumes or vehicle numbers."))))) # Open data & methodology modal dialog
  
  output$dl <- downloadHandler(
    filename = function() "kolkata_crashes_chapter1_filtered.csv",
    content = function(file) {
      out <- filtered()[, c("date", "year", "month", "hour", "severity", "station", "place",
                            "vehicle", "road_user", "sex", "age", "persons_killed", "lat", "lon")]
      write.csv(out, file, row.names = FALSE, na = "")
    }) # Export currently filtered crash records to CSV
}

shinyApp(ui, server)