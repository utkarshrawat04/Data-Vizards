CREAM  <- "#FBF7EF"
INK    <- "#2B2B2B"
MAROON <- "#6B1A47"
YELLOW <- "#F2B01E"
QUIET  <- "#D9D1D6"
GRID   <- "#ECE6EA"
FONT   <- "Source Sans 3, Arial, sans-serif"
TEAL   <- "#2f7f86"                                        # distinct accent (night bars, "Rider" in Ch3)
HEAT   <- c("#FBF3E4", "#F3CB7E", "#C2557A", "#45102D")   # heatmap scale, light to dark

# One bootstrap theme for every chapter
app_theme <- bslib::bs_theme(version = 5, bg = CREAM, fg = INK, primary = MAROON,
                             base_font = bslib::font_google("Source Sans 3", local = FALSE),
                             heading_font = bslib::font_google("Libre Baskerville", local = FALSE))

strip_item <- function(txt, on = FALSE)
  span(class = if (on) "on" else "off", title = if (!on) "Another part of the dashboard", txt) # Top progress strip, shared by every chapter

chart <- function(id, height, label)
  div(role = "img", `aria-label` = label, plotly::plotlyOutput(id, height = height))    # Plotly chart wrapped with an accessible text label (used by Chapter 4)

fmt <- function(x) format(x, big.mark = ",")

tidy <- function(p, margin = list(l = 50, r = 15, t = 10, b = 45), ...) {
  p |>
    layout(font = list(family = FONT, size = 12, color = INK),
           paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
           margin = margin, ...) |>
    config(displayModeBar = FALSE)
}

base_css <- sprintf("
  body { background:%s; font-family:%s; color:%s; }
  .hdr { background:%s; color:#fff; padding:16px 26px; display:flex; justify-content:space-between; align-items:flex-end; }
  .hdr h1 { font-family:'Libre Baskerville', Georgia, serif; font-size:26px; margin:2px 0 4px; color:#fff; }
  .hdr .over { font-size:12px; letter-spacing:.08em; text-transform:uppercase; color:%s; font-weight:700; }
  .hdr .sub { font-size:14px; opacity:.9; }
  .btn-hdr { background:transparent; border:1px solid rgba(255,255,255,.7); color:#fff; border-radius:3px; margin-left:8px; font-size:14px; }
  .btn-hdr:hover { background:#fff; color:%s; }
  
  .strip { background:#fff; border-bottom:1px solid #E4DCE1; padding:9px 26px; font-size:13px; letter-spacing:.04em; }
  .strip span { margin-right:26px; padding-bottom:7px; color:#9A8F96; }
  .strip .on { color:%s; font-weight:700; border-bottom:3px solid %s; }
  
  .story { font-family:'Libre Baskerville', Georgia, serif; font-size:24px; color:%s; margin:4px 0 2px; }
  .lede { color:#5b5258; margin-bottom:14px; }
  
  .kpi { background:#fff; border:1px solid #E4DCE1; border-top:3px solid %s; padding:10px 14px; margin-bottom:16px; }
  .kpi .v { font-size:28px; font-weight:700; color:%s; font-variant-numeric:tabular-nums; line-height:1.15; }
  .kpi .t { font-size:13px; color:#5b5258; }
  
  .panel { background:#fff; border:1px solid #E4DCE1; padding:12px 14px 10px; margin-bottom:16px; position:relative; }
  .panel h4 { font-size:16px; font-weight:700; margin:0 0 6px; color:%s; }
  
  .hint { font-size:13px; color:#5b5258; min-height:36px; margin-bottom:4px; }
  .countbox { font-size:13px; background:#F3EAF0; padding:8px 10px; margin:6px 0 8px; border-left:3px solid %s; }
  
  .chip { display:inline-block; background:#fff; border:1px solid %s; color:%s; padding:1px 9px; margin:0 4px 4px 0; font-size:12px; text-decoration:none; border-radius:12px; }
  .chip:hover { background:%s; color:#fff; }
  
  .empty { background:#FFF6DC; border-left:3px solid %s; padding:8px 12px; margin-bottom:12px; }
  .sidebar-label { font-size:12px; letter-spacing:.06em; text-transform:uppercase; color:%s; font-weight:700; }
  
  .recalculating { opacity:.35; }
  .shiny-output-error-validation { color:#6b6168; font-style:italic; padding:50px 10px; text-align:center; }
  a.about { font-size:13px; color:%s; }
", CREAM, FONT, INK, MAROON, YELLOW, MAROON, MAROON, YELLOW, MAROON, MAROON, MAROON, MAROON, MAROON, MAROON, MAROON, MAROON, MAROON, MAROON, MAROON)

seg_css <- "
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
  .seg input[type=checkbox] { position:absolute; opacity:0; pointer-events:none; }
  .seg input[type=checkbox]:checked + span { background:@MAROON@; color:#fff; font-weight:700; }
  .seg input[type=checkbox]:focus-visible + span { outline:2px solid @YELLOW@; outline-offset:1px; }
  .seg-wrap .shiny-options-group { display:grid !important; grid-template-columns:1fr 1fr; }
  .seg-wrap label span { margin-top:-1px; }
"
seg_css_compiled <- gsub("@YELLOW@", YELLOW, gsub("@MAROON@", MAROON, seg_css, fixed = TRUE), fixed = TRUE)


extra_css <- "
  .panel .sub { font-size:13px; color:#5b5258; min-height:36px; margin-bottom:4px; }
  .note  { font-size:12px; color:#5b5258; margin-top:6px; }
  .foot  { font-size:13px; color:#5b5258; border-top:1px solid #E4DCE1; padding-top:10px; }
  .chips { margin-bottom:8px; }
  .chip  { cursor:pointer; }
  .story2 { font-family:'Libre Baskerville', Georgia, serif; font-size:20px; color:@MAROON@; margin:18px 0 8px; }
  /* Chapter 4 */
  .callout { background:#F3EAF0; border-left:4px solid @MAROON@; padding:12px 16px; margin-bottom:16px; font-size:16px; }
  .victim .big { font-family:'Libre Baskerville', Georgia, serif; font-size:20px; color:@MAROON@; margin:6px 0 10px; }
  .row2 { display:flex; justify-content:space-between; padding:5px 0; border-bottom:1px solid #ECE6EA; font-size:14px; }
"  
extra_css <- gsub("@MAROON@", MAROON, extra_css, fixed = TRUE)           # Panel sub-titles, notes, footers and chips (plain string: no sprintf, so % is safe)

app_css <- paste0(base_css, seg_css_compiled, extra_css)