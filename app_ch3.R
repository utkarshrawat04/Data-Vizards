library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(stringr)
library(htmltools)
library(shinyjs)
library(bslib)


source("R/shared_theme.R")             # Load shared theme and fatal accident data cleaning script
source("R/clean_fatal.R")


app_dir <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) ".")                  # Resolve application directory path for local data loading
dpath <- function(f) if (file.exists(file.path("data", f))) file.path("data", f) else file.path(app_dir, "data", f)


VEH_LEV  <- c("Bus", "Truck / lorry", "Car", "Taxi", "Motorcycle", "Auto-rickshaw", "Unknown", "Other")
USER_LEV <- c("Pedestrian", "Cyclist", "Rider (2-wheeler)", "Pillion rider", "Passenger / occupant", "Other")              # Define category level mappings for vehicles, road users, and age brackets
AGE_LEV  <- c("Under 30", "30s", "40s", "50s", "60s", "70s", "80+")
recode_veh <- function(x) { x[x == "Truck"] <- "Truck / lorry"; factor(x, levels = VEH_LEV) }


fatal <- read.csv(dpath("fatal_clean.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA")) |>
  mutate(vehicle   = recode_veh(vehicle),
         road_user = factor(road_user, levels = USER_LEV),
         age_band  = factor(age_band, levels = c(AGE_LEV, "Not recorded")),
         date      = as.Date(date),
         has_loc   = !is.na(lat))                                                                                 # Load and clean fatal and non-fatal crash datasets from CSV files
nonfatal <- read.csv(dpath("nonfatal_slim.csv"), stringsAsFactors = FALSE, na.strings = c("", "NA")) |>
  mutate(vehicle = recode_veh(vehicle))
N_ALL <- nrow(fatal)
STATIONS <- sort(unique(fatal$station[fatal$station != "Not recorded"]))                         


USER_COL <- c("Pedestrian" = MAROON, "Cyclist" = YELLOW, "Rider (2-wheeler)" = TEAL,
              "Pillion rider" = "#8a9a5b", "Passenger / occupant" = "#6d7bb0", "Other" = "#9b9298")                # Color palettes and pluralization helper lookup tables

pct  <- function(a, b) if (b > 0) 100 * a / b else NA_real_
fmt_pct <- function(x, d = 0) ifelse(is.na(x), "–", paste0(formatC(x, format = "f", digits = d), "%"))

veh_plural <- c("Bus" = "buses", "Truck / lorry" = "lorries", "Car" = "cars", "Taxi" = "taxis",
                "Motorcycle" = "motorcycles", "Auto-rickshaw" = "auto-rickshaws", "Other" = "other vehicles")
user_plural <- c("Pedestrian" = "Pedestrians", "Cyclist" = "Cyclists", "Rider (2-wheeler)" = "Two-wheeler riders",
                 "Pillion rider" = "Pillion passengers", "Passenger / occupant" = "Passengers and vehicle occupants", "Other" = "Other road users")


empty_plot <- function(msg = "No crashes match these filters") {
  plot_ly(type = "scatter", mode = "markers", x = 0, y = 0, hoverinfo = "none", marker = list(opacity = 0)) |>
    layout(xaxis = list(visible = FALSE), yaxis = list(visible = FALSE), font = list(family = FONT, size = 12, color = INK),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           annotations = list(list(text = msg, showarrow = FALSE, font = list(size = 14, color = "#5b5258"),
                                   x = 0, y = 0)),
           margin = list(l = 10, r = 10, t = 10, b = 10)) |>
    config(displayModeBar = FALSE)
}                                                                                         # Fallback blank plot builder for empty data subsets

card <- function(title, sub = NULL, ..., class = "") {
  div(class = paste("panel", class), h4(title), if (!is.null(sub)) div(class = "sub", sub), ...)
}                                                                               # Reusable container card wrapper function

# Custom CSS styling rules for Chapter 3
css_seg_single <- sprintf("
  .seg { margin-bottom:8px; }
  .seg .shiny-options-group { display:flex; }
  .seg .shiny-options-group > * { flex:1; margin:0 !important; padding:0 !important; }
  .seg .form-check { padding-left:0; }
  .seg label { display:block; margin:0; }
  .seg input[type=radio] { position:absolute; opacity:0; pointer-events:none; }
  .seg label span { display:block; text-align:center; padding:5px 6px; border:1px solid %s;
    font-size:13px; cursor:default; color:#fff; background:%s; font-weight:700; }
  .legend span { margin-right: 18px; display: inline-flex; align-items: center; }
  .legend span i { width: 12px; height: 12px; display: inline-block; margin-right: 5px; }
", MAROON, MAROON)

chapter3_css <- paste0(app_css, css_seg_single)

# User interface 
ui <- page_fluid(
  title = "Kolkata Road Crashes 2021-2023 | Chapter 3: The Severity Problem",
  theme = app_theme,
  padding = 0,
  useShinyjs(),
  tags$head(tags$style(HTML(chapter3_css)), tags$meta(name = "viewport", content = "width=device-width, initial-scale=1")),
  
  # Top header bar layout
  div(class = "hdr",
      div(div(class = "over", "CHAPTER 3 OF 4 · THE SEVERITY PROBLEM"),
          h1("Kolkata Road Crashes 2021–2023"),
          div(class = "sub", sprintf("Kolkata Traffic Police records · %s fatal crashes", format(N_ALL, big.mark = ",")))),
      div(actionButton("howto", "? How to use", class = "btn-hdr"),
          actionButton("reset_all", "Reset all", class = "btn-hdr"))),
  
  # Sidebar filter controls and dashboard layout
  layout_sidebar(
    fillable = FALSE,
    sidebar = sidebar(
      width = 300, bg = "#fff",
      div(class = "sidebar-label", "Filters"),
      checkboxGroupInput("year", "Year", choices = c("2021", "2022", "2023"), selected = c("2021", "2022", "2023"), inline = TRUE),
      tags$label("Severity"),
      div(class = "seg", radioButtons("severity_fatal", NULL, inline = TRUE, selected = "fatal", choices = c("Fatal" = "fatal"))),
      selectInput("veh", "Vehicle group (offending)", choices = c("All vehicles", VEH_LEV), selected = "All vehicles"),
      selectInput("user", "Road user (victim)", choices = c("All road users", USER_LEV), selected = "All road users"),
      sliderInput("hours", "Hour of day", min = 0, max = 23, value = c(0, 23), step = 1, post = ":00", sep = ""),
      selectizeInput("station", "Police station", choices = STATIONS, selected = NULL, multiple = TRUE,
                     options = list(placeholder = "Search...")),
      uiOutput("showing"),
      uiOutput("chips"),
      div(downloadButton("dl", "Download CSV")),
      div(style = "margin-top:12px;", actionLink("about", "ⓘ About the data & source", class = "about"))
    ),
    
    # Main content panels and analytics displays
    div(style = "padding:14px 22px 20px;",
        div(class = "story", textOutput("headline", inline = TRUE)),
        div(class = "lede", "Click a bar, a grid cell, or a square to filter everything else."),
        uiOutput("kpis"),
        
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          card("Who is dying? Victims by age and sex", uiOutput("pyr_sub"),
               plotlyOutput("pyr", height = "330px")),
          card("Who hits whom", "Deaths by victim type (rows) and offending vehicle (columns). Darker cell = more deaths.",
               plotlyOutput("grid", height = "330px"))
        ),
        
        layout_columns(
          col_widths = c(6, 6), fill = FALSE,
          card("Day vs night", uiOutput("dn_sub"),
               radioButtons("dn_mode", NULL, choices = c("Deaths" = "n", "% of crashes that are fatal" = "rate"), inline = TRUE),
               plotlyOutput("dn", height = "285px"),
               div(class = "note", HTML("Yellow bars = day (6am–6pm), maroon bars = night (6pm–6am). Rates use Chapter 2’s non-fatal records for the same year, hour and vehicle filters; groups with fewer than 5 deaths are noisy."))),
          card("Helmet status of two-wheeler deaths", uiOutput("hel_sub"),
               plotlyOutput("waffle", height = "250px"),
               div(class = "legend", HTML(sprintf("<span><i style='background:%s'></i>Helmet worn</span><span><i style='background:%s'></i>No helmet</span><span><i style='background:#cfc8cc'></i>Not recorded</span>", MAROON, YELLOW))),
               div(class = "note", "One square = one rider or pillion passenger who died. These are victims only: the records do not say how many riders wear helmets overall, so this cannot show whether helmets lower the risk of dying."))
        ),
        
        div(class = "foot", uiOutput("footnote"))
    )
  )
)

# Server
server <- function(input, output, session) {
  
  
  xf <- reactiveValues(age = NULL, sex = NULL, gu = NULL, gv = NULL, dv = NULL, dp = NULL, helmet = NULL, stn = NULL)
  clear_x <- function() { for (k in c("age", "sex", "gu", "gv", "dv", "dp", "helmet", "stn")) xf[[k]] <- NULL }               # Initialize cross-filtering reactive values
  unclick <- function(src) runjs(sprintf("Shiny.setInputValue('plotly_click-%s', null, {priority:'event'});", src))
  

  base <- reactive({
    d <- fatal |> filter(year %in% as.integer(input$year))
    if (!identical(input$veh, "All vehicles")) d <- d |> filter(vehicle == input$veh)
    if (!identical(input$user, "All road users")) d <- d |> filter(road_user == input$user)
    full_day <- identical(as.integer(input$hours), c(0L, 23L))
    d <- d |> filter((is.na(hour) & full_day) | (!is.na(hour) & hour >= input$hours[1] & hour <= input$hours[2]))
    if (length(input$station)) d <- d |> filter(station %in% input$station)
    d
  })                                             # Base reactive filter set for all data subsets
  
  
  fdat <- function(skip = "") {
    d <- base()
    if (skip != "pyr")    { if (!is.null(xf$age)) d <- d |> filter(age_band == xf$age, sex == xf$sex) }
    if (skip != "grid")   { if (!is.null(xf$gu)) d <- d |> filter(road_user == xf$gu, vehicle == xf$gv) }
    if (skip != "dn")     { if (!is.null(xf$dv)) d <- d |> filter(vehicle == xf$dv, period == xf$dp) }
    if (skip != "waffle") { if (!is.null(xf$helmet)) d <- d |> filter(helmet == xf$helmet) }
    if (skip != "stn")    { if (!is.null(xf$stn)) d <- d |> filter(station == xf$stn) }
    d
  }                                             # Filter data by chart-specific selections
  cur <- reactive(fdat(""))
  
  
  do_reset <- function() {
    clear_x()
    updateCheckboxGroupInput(session, "year", selected = c("2021", "2022", "2023"))
    updateSelectInput(session, "veh", selected = "All vehicles")
    updateSelectInput(session, "user", selected = "All road users")
    updateSliderInput(session, "hours", value = c(0, 23))
    updateSelectizeInput(session, "station", selected = character(0))
    updateRadioButtons(session, "dn_mode", selected = "n")
  }                                              # Reset all filters and active selections
  observeEvent(input$reset_all, do_reset())
  
  
  observeEvent(input$howto, showModal(modalDialog(
    title = "How to use this chapter", easyClose = TRUE, footer = modalButton("Got it"),
    tags$ol(
      tags$li(tags$b("Filters (left)"), " apply to every chart: year, offending vehicle, victim type, hour of day and police station."),
      tags$li(tags$b("Click any bar, grid cell, or waffle square"), " to filter the other charts. Click it again to undo."),
      tags$li(tags$b("Chips"), " under the filter box show which chart selections are active; click one to remove it."),
      tags$li(tags$b("Reset all"), " (top right) undoes everything. ", tags$b("Download CSV"), " saves the cases you are looking at.")))))      # Modal dialog for user instructions
  
  
  observeEvent(input$about, showModal(modalDialog(
    title = "About the data & source", easyClose = TRUE, footer = modalButton("Close"),
    p("Source: Kolkata Traffic Police fatal-accident records, 2021–2023 (DATA.xlsx), shared with the West Bengal Transport Corporation. Each row is one fatal case with one victim."),
    tags$ul(
      tags$li("Vehicle names were grouped (e.g. Pvt. Bus, WBTC Bus, Mini bus → Bus). ‘Unknown’ means a hit-and-run with no vehicle identified."),
      tags$li("Day = 6am–6pm, night = 6pm–6am, using the recorded time of the crash. Cases with no time are left out of day/night views."),
      tags$li("Helmet status is recorded only for two-wheeler riders and pillion passengers."),
      tags$li("Fatality rates divide deaths by deaths plus non-fatal crashes (Chapter 2 data) in the same group.")))))                  # Modal dialog for data source and methodology notes
  
  # Active filter indicator chips UI renderer
  output$chips <- renderUI({
    ch <- list()
    add <- function(key, txt) ch[[length(ch) + 1]] <<- span(class = "chip", paste0(txt, " ×"),
                                                            onclick = sprintf("Shiny.setInputValue('rm_chip','%s',{priority:'event'})", key))
    if (!is.null(xf$age))    add("pyr", paste0(xf$sex, " · ", xf$age))
    if (!is.null(xf$gu))     add("grid", paste0(xf$gu, " × ", xf$gv))
    if (!is.null(xf$dv))     add("dn", paste0(xf$dv, " · ", str_extract(xf$dp, "^\\w+")))
    if (!is.null(xf$helmet)) add("waffle", xf$helmet)
    if (!is.null(xf$stn))    add("stn", xf$stn)
    if (length(ch)) div(class = "chips", ch)
  })
  observeEvent(input$rm_chip, {
    switch(input$rm_chip, pyr = { xf$age <- NULL; xf$sex <- NULL }, grid = { xf$gu <- NULL; xf$gv <- NULL },
           dn = { xf$dv <- NULL; xf$dp <- NULL }, waffle = xf$helmet <- NULL, stn = xf$stn <- NULL)
  })
  
  output$showing <- renderUI({
    d <- cur()
    div(class = "countbox", HTML(sprintf("Showing <b>%s</b> of %s deaths.", format(nrow(d), big.mark = ","), format(N_ALL, big.mark = ","))))
  })                                   # Sidebar match count box renderer
  
  
  output$headline <- renderText({
    d <- cur()
    if (nrow(d) < 10) return("Too few deaths to summarise: widen the filters")
    tab <- sort(table(d$road_user), decreasing = TRUE)
    top <- names(tab)[1]; share <- round(100 * tab[1] / nrow(d))
    if (top == "Pedestrian") {
      v <- d |> filter(road_user == "Pedestrian", !vehicle %in% c("Unknown")) |> count(vehicle, sort = TRUE) |> head(2)
      who <- if (nrow(v) >= 2) paste0(veh_plural[as.character(v$vehicle[1])], " and ", veh_plural[as.character(v$vehicle[2])]) else veh_plural[as.character(v$vehicle[1])]
      sprintf("Pedestrians are %d%% of deaths; most are hit by %s", share, who)
    } else sprintf("%s are the largest group of victims, at %d%% of deaths", user_plural[[as.character(top)]], share)
  })                                    # Dynamic story headline text generator
  
  
  rate_parts <- reactive({
    f <- fatal |> filter(year %in% as.integer(input$year), !is.na(hour), hour >= input$hours[1], hour <= input$hours[2])
    n <- nonfatal |> filter(year %in% as.integer(input$year), !is.na(hour), hour >= input$hours[1], hour <= input$hours[2])
    if (!identical(input$veh, "All vehicles")) { f <- f |> filter(vehicle == input$veh); n <- n |> filter(vehicle == input$veh) }
    list(f = f, n = n)
  })                                    # Reactive calculator for fatality rate denominator components
  
  
  output$kpis <- renderUI({
    d <- cur(); n <- nrow(d)
    ped <- pct(sum(d$road_user == "Pedestrian"), n)
    aged <- d |> filter(road_user == "Pedestrian", !is.na(age)); old <- pct(sum(aged$age60), nrow(aged))
    rp <- rate_parts()
    rate <- function(p) { fd <- sum(rp$f$period == p); nd <- sum(rp$n$period == p, na.rm = TRUE); pct(fd, fd + nd) }
    rd <- rate("Day (6am-6pm)"); rn <- rate("Night (6pm-6am)")
    ratio <- if (!is.na(rd) && !is.na(rn) && rd > 0) sprintf("%.1f×", rn / rd) else "–"
    fluidRow(style = "margin-bottom:16px;",
             column(3, div(class = "kpi", div(class = "v", format(n, big.mark = ",")), div(class = "t", sprintf("deaths shown, of %s fatal cases", format(N_ALL, big.mark = ","))))),
             column(3, div(class = "kpi", div(class = "v", fmt_pct(ped)), div(class = "t", "of victims were pedestrians"))),
             column(3, div(class = "kpi", div(class = "v", fmt_pct(old)), div(class = "t", sprintf("of pedestrian victims aged 60+ (of %s with recorded age)", nrow(aged))))),
             column(3, div(class = "kpi", div(class = "v", ratio), div(class = "t", HTML(sprintf("night-time risk relative to day (%s vs %s)", fmt_pct(rn, 1), fmt_pct(rd, 1))))))
    )
  })                                     # Top summary KPI statistics cards renderer
  
  
  output$pyr_sub <- renderUI({
    d <- fdat("pyr"); nr <- sum(d$age_band == "Not recorded" | d$sex == "Not recorded")
    HTML(sprintf("Men on the left, women on the right. Click a bar to filter.%s",
                 if (nr > 0) sprintf(" <i>%d with age/sex not recorded left out.</i>", nr) else ""))
  })                                     # Subtitle for age-sex pyramid chart
  
  # Age-sex pyramid Plotly chart renderer and click handler
  output$pyr <- renderPlotly({
    d <- fdat("pyr") |> filter(age_band != "Not recorded", sex %in% c("Men", "Women"))
    if (nrow(d) == 0) return(empty_plot())
    df <- d |> count(age_band, sex, .drop = FALSE) |> complete(age_band = factor(AGE_LEV, levels = AGE_LEV), sex = c("Men", "Women"), fill = list(n = 0)) |>
      mutate(age_band = factor(age_band, levels = AGE_LEV), key = paste(age_band, sex, sep = "|"),
             sel = if (is.null(xf$age)) TRUE else (age_band == xf$age & sex == xf$sex))
    m <- max(df$n) * 1.25 + 1
    p <- plot_ly(source = "pyr")
    for (s in c("Men", "Women")) {
      x <- df |> filter(sex == s); sg <- if (s == "Men") -1 else 1
      p <- add_bars(p, data = x, y = ~age_band, x = sg * x$n, key = ~key, orientation = "h", name = s,
                    marker = list(color = if (s == "Men") MAROON else YELLOW, opacity = ifelse(x$sel, 1, .28)),
                    text = ifelse(x$n > 0, x$n, ""), textposition = "outside", cliponaxis = FALSE,
                    hovertemplate = paste0("<b>", s, ", %{y}</b><br>", "%{text} deaths<extra></extra>"))
    }
    p |> layout(barmode = "overlay", font = list(family = FONT, size = 12, color = INK), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                xaxis = list(range = c(-m, m), tickvals = pretty(c(-m, m), 6), ticktext = abs(pretty(c(-m, m), 6)), zeroline = TRUE,
                             zerolinecolor = "#8c8288", gridcolor = GRID, title = "Deaths"),
                yaxis = list(categoryorder = "array", categoryarray = AGE_LEV, title = "", fixedrange = TRUE),
                legend = list(orientation = "h", x = .5, xanchor = "center", y = 1.08),
                margin = list(l = 60, r = 10, t = 10, b = 40)) |>
      config(displayModeBar = FALSE) |> event_register("plotly_click")
  })
  observeEvent(event_data("plotly_click", source = "pyr"), {
    k <- event_data("plotly_click", source = "pyr")$key
    if (length(k) == 1) {
      parts <- strsplit(k, "|", fixed = TRUE)[[1]]
      if (!is.null(xf$age) && xf$age == parts[1] && xf$sex == parts[2]) { xf$age <- NULL; xf$sex <- NULL }
      else { xf$age <- parts[1]; xf$sex <- parts[2] }
    }
    unclick("pyr")
  })
  
  
  output$grid <- renderPlotly({
    d <- fdat("grid")
    if (nrow(d) == 0) return(empty_plot())
    tb <- d |> count(road_user, vehicle, .drop = FALSE) |> complete(road_user = factor(USER_LEV, levels = USER_LEV), vehicle = factor(VEH_LEV, levels = VEH_LEV), fill = list(n = 0))
    z <- matrix(0, length(USER_LEV), length(VEH_LEV), dimnames = list(USER_LEV, VEH_LEV))
    for (i in seq_len(nrow(tb))) z[as.character(tb$road_user[i]), as.character(tb$vehicle[i])] <- tb$n[i]
    zz <- z; zz[zz == 0] <- NA
    mx <- max(z)
    ann <- list(); shp <- list()
    for (i in seq_along(USER_LEV)) for (j in seq_along(VEH_LEV)) if (z[i, j] > 0)
      ann[[length(ann) + 1]] <- list(x = VEH_LEV[j], y = USER_LEV[i], text = z[i, j], showarrow = FALSE,
                                     font = list(size = 12, color = if (z[i, j] / mx > .6) "#fff" else INK))
    if (!is.null(xf$gu)) {
      j <- match(xf$gv, VEH_LEV) - 1; i <- match(xf$gu, USER_LEV) - 1
      shp <- list(list(type = "rect", x0 = j - .5, x1 = j + .5, y0 = i - .5, y1 = i + .5, line = list(color = YELLOW, width = 3)))
    }
    plot_ly(source = "grid", x = VEH_LEV, y = USER_LEV, z = zz, type = "heatmap", xgap = 3, ygap = 3,
            colorscale = lapply(seq_along(HEAT), function(i) list((i - 1) / (length(HEAT) - 1), HEAT[i])), showscale = FALSE,
            hovertemplate = "<b>%{y}</b> killed by <b>%{x}</b><br>%{z} deaths<extra></extra>") |>
      layout(font = list(family = FONT, size = 12, color = INK), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)", annotations = ann, shapes = shp,
             xaxis = list(side = "top", tickangle = -30, title = "", fixedrange = TRUE, categoryorder = "array", categoryarray = VEH_LEV),
             yaxis = list(autorange = "reversed", title = "", fixedrange = TRUE, categoryorder = "array", categoryarray = USER_LEV),
             margin = list(l = 130, r = 10, t = 70, b = 10)) |>
      config(displayModeBar = FALSE) |> event_register("plotly_click")
  })
  observeEvent(event_data("plotly_click", source = "grid"), {
    e <- event_data("plotly_click", source = "grid")
    if (!is.null(e$x) && !is.null(e$y)) {
      v <- as.character(e$x[[1]]); u <- as.character(e$y[[1]])
      if (!is.null(xf$gu) && xf$gu == u && xf$gv == v) { xf$gu <- NULL; xf$gv <- NULL } else { xf$gu <- u; xf$gv <- v }
    }
    unclick("grid")
  })                    # Victim vs offending vehicle heatmap Plotly chart renderer and click handler
  
  output$dn_sub <- renderUI({
    d <- fdat("dn"); nt <- sum(is.na(d$hour))
    HTML(sprintf("Deaths by offending vehicle and time of day.%s", if (nt > 0) sprintf(" <i>%d with no time left out.</i>", nt) else ""))
  })                    # Subtitle for Day vs Night chart
  
  
  output$dn <- renderPlotly({
    d <- fdat("dn") |> filter(!is.na(period))
    if (nrow(d) == 0) return(empty_plot())
    df <- d |> count(vehicle, period, .drop = FALSE) |> complete(vehicle = factor(VEH_LEV, levels = VEH_LEV), period = c("Day (6am-6pm)", "Night (6pm-6am)"), fill = list(n = 0))
    rp <- rate_parts()
    nfc <- rp$n |> filter(!is.na(period)) |> count(vehicle, period, .drop = FALSE) |> rename(nf = n)
    ftc <- rp$f |> filter(!is.na(period)) |> count(vehicle, period, .drop = FALSE) |> rename(fd = n)
    df <- df |> left_join(nfc, by = c("vehicle", "period")) |> left_join(ftc, by = c("vehicle", "period")) |>
      mutate(nf = coalesce(nf, 0L), fd = coalesce(fd, 0L), rate = ifelse(fd + nf > 0, 100 * fd / (fd + nf), NA_real_),
             key = paste(vehicle, period, sep = "|"),
             sel = if (is.null(xf$dv)) TRUE else (vehicle == xf$dv & period == xf$dp))
    rate_mode <- identical(input$dn_mode, "rate")
    p <- plot_ly(source = "dn")
    for (pp in c("Day (6am-6pm)", "Night (6pm-6am)")) {
      x <- df |> filter(period == pp); night <- grepl("^Night", pp)
      yv <- if (rate_mode) x$rate else x$n
      col <- if (night) MAROON else YELLOW
      pat <- list(shape = "")
      p <- add_bars(p, x = x$vehicle, y = yv, key = x$key, name = sub(" \\(.*", "", pp),
                    marker = list(color = col, pattern = pat, opacity = ifelse(x$sel, 1, .3), line = list(color = col, width = 1)),
                    text = if (rate_mode) ifelse(is.na(yv), "", sprintf("%.1f%%", yv)) else ifelse(yv > 0, yv, ""),
                    textposition = "outside", cliponaxis = FALSE,
                    customdata = if (rate_mode) paste0(x$fd, " deaths of ", x$fd + x$nf, " crashes") else paste0(x$n, " deaths"),
                    hovertemplate = paste0("<b>%{x}</b> · ", sub(" \\(.*", "", pp), "<br>%{customdata}<extra></extra>"))
    }
    p |> layout(barmode = "group", font = list(family = FONT, size = 12, color = INK), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                xaxis = list(title = "", fixedrange = TRUE, categoryorder = "array", categoryarray = VEH_LEV, tickangle = -25),
                yaxis = list(title = if (rate_mode) "% of crashes that are fatal" else "Deaths", gridcolor = GRID, fixedrange = TRUE,
                             ticksuffix = if (rate_mode) "%" else ""),
                legend = list(orientation = "h", x = .5, xanchor = "center", y = 1.12),
                margin = list(l = 55, r = 10, t = 25, b = 60)) |>
      config(displayModeBar = FALSE) |> event_register("plotly_click")
  })               # Day vs Night Plotly bar chart renderer and click handler
  observeEvent(event_data("plotly_click", source = "dn"), {
    k <- event_data("plotly_click", source = "dn")$key
    if (length(k) == 1) {
      parts <- strsplit(k, "|", fixed = TRUE)[[1]]
      if (!is.null(xf$dv) && xf$dv == parts[1] && xf$dp == parts[2]) { xf$dv <- NULL; xf$dp <- NULL }
      else { xf$dv <- parts[1]; xf$dp <- parts[2] }
    }
    unclick("dn")
  })            
  
  # Subtitle for helmet waffle chart
  HEL <- c("Helmet worn", "No helmet", "Not recorded"); WCOL <- 18
  output$hel_sub <- renderUI({
    d <- fdat("waffle") |> filter(road_user %in% c("Rider (2-wheeler)", "Pillion rider"))
    n <- nrow(d)
    if (n == 0) return(HTML("Riders and pillion passengers who died."))
    w <- sum(d$helmet == "Helmet worn"); nh <- sum(d$helmet == "No helmet")
    HTML(sprintf("Of <b>%d</b> two-wheeler victims, <b>%d</b> wore a helmet and <b>%d</b> did not.", n, w, nh))
  })                      # Subtitle for helmet waffle chart
  
  output$waffle <- renderPlotly({
    d <- fdat("waffle") |> filter(road_user %in% c("Rider (2-wheeler)", "Pillion rider"))
    if (nrow(d) == 0) return(empty_plot("No two-wheeler deaths match filters"))
    codes <- unlist(lapply(seq_along(HEL), function(i) rep(i, sum(d$helmet == HEL[i]))))
    ncol <- WCOL; nrow_ <- ceiling(length(codes) / ncol)
    z <- matrix(c(codes, rep(NA, nrow_ * ncol - length(codes))), nrow_, ncol, byrow = TRUE)
    txt <- matrix(ifelse(is.na(z), "", HEL[z]), nrow_, ncol)
    cols <- c(MAROON, YELLOW, "#cfc8cc")
    cs <- list(c(0, cols[1]), c(1/3, cols[1]), c(1/3, cols[2]), c(2/3, cols[2]), c(2/3, cols[3]), c(1, cols[3]))
    p <- plot_ly(source = "waffle", z = z, x = seq_len(ncol), y = seq_len(nrow_), type = "heatmap", zmin = .5, zmax = 3.5,
                 colorscale = cs, showscale = FALSE, xgap = 3, ygap = 3, text = txt, hoverinfo = "text", opacity = 1)
    if (!is.null(xf$helmet)) {
      zz <- z; zz[!is.na(zz) & zz != match(xf$helmet, HEL)] <- NA
      p <- plot_ly(source = "waffle") |>
        add_heatmap(z = z, x = seq_len(ncol), y = seq_len(nrow_), colorscale = list(c(0, "#e7e1e4"), c(1, "#e7e1e4")), showscale = FALSE, xgap = 3, ygap = 3, hoverinfo = "none", zmin = .5, zmax = 3.5) |>
        add_heatmap(z = zz, x = seq_len(ncol), y = seq_len(nrow_), colorscale = cs, showscale = FALSE, xgap = 3, ygap = 3, zmin = .5, zmax = 3.5, text = txt, hoverinfo = "text")
    }
    p |> layout(font = list(family = FONT, size = 12, color = INK), paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
                xaxis = list(visible = FALSE, fixedrange = TRUE), yaxis = list(visible = FALSE, autorange = "reversed", fixedrange = TRUE, scaleanchor = "x"),
                margin = list(l = 5, r = 5, t = 5, b = 5)) |>
      config(displayModeBar = FALSE) |> event_register("plotly_click")
  })             # Helmet waffle heatmap Plotly chart renderer and click handler   
  observeEvent(event_data("plotly_click", source = "waffle"), {
    e <- event_data("plotly_click", source = "waffle")
    if (!is.null(e$x)) {
      d <- fdat("waffle") |> filter(road_user %in% c("Rider (2-wheeler)", "Pillion rider"))
      codes <- unlist(lapply(seq_along(HEL), function(i) rep(HEL[i], sum(d$helmet == HEL[i]))))
      idx <- (as.integer(e$y[[1]]) - 1) * WCOL + as.integer(e$x[[1]])
      if (idx >= 1 && idx <= length(codes)) {
        h <- codes[idx]
        xf$helmet <- if (!is.null(xf$helmet) && xf$helmet == h) NULL else h
      }
    }
    unclick("waffle")
  })
  
  output$footnote <- renderUI({
    d <- cur()
    HTML(sprintf("<b>Reading notes.</b> Each record is one fatal case (%d cases). Source: Kolkata Traffic Police records 2021–2023.", nrow(d)))
  })             # Footer reading notes text renderer
  
  output$dl <- downloadHandler(
    filename = function() paste0("kolkata_fatal_crashes_", Sys.Date(), ".csv"),
    content = function(file) write.csv(cur(), file, row.names = FALSE, na = ""))
}    # CSV download handler

shinyApp(ui, server)