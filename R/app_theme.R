# Observatory theme: Bootstrap 5 via bslib plus static assets in www/.

ASSET_VERSION <- "14"

axis_bs_theme <- function() {
  bslib::bs_theme(
    version      = 5,
    bg           = AXIS$parchment,
    fg           = AXIS$ink,
    primary      = AXIS$ink,
    secondary    = AXIS$sand,
    success      = AXIS$gain,
    danger       = AXIS$loss,
    warning      = AXIS$brass,
    info         = AXIS$slate,
    base_font    = "Inter, system-ui, -apple-system, 'Segoe UI', sans-serif",
    heading_font = "Fraunces, 'Iowan Old Style', Georgia, serif",
    code_font    = "'JetBrains Mono', ui-monospace, Menlo, monospace",
    "border-radius" = "10px",
    "link-color" = AXIS$brass_deep
  )
}

axis_head <- function() {
  asset <- function(path) paste0(path, "?v=", ASSET_VERSION)
  tags$head(
    tags$meta(name = "theme-color", content = AXIS$parchment),
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
    tags$link(
      rel  = "stylesheet",
      href = paste0(
        "https://fonts.googleapis.com/css2?",
        "family=Fraunces:ital,opsz,wght,SOFT@0,9..144,300..700,0..100;1,9..144,300..700,0..100&",
        "family=Inter:wght@400;500;600&",
        "family=JetBrains+Mono:wght@400;500&display=swap"
      )
    ),
    useBusyIndicators(spinners = TRUE, pulse = TRUE, fade = TRUE),
    busyIndicatorOptions(
      spinner_type     = "www/img/axis-spinner.svg",
      spinner_color    = AXIS$brass,
      spinner_size     = "44px",
      spinner_delay    = "150ms",
      spinner_selector = "html",
      pulse_background = paste0("linear-gradient(90deg, transparent, ", AXIS$brass, ", transparent)"),
      pulse_height     = "2px",
      fade_opacity     = 0.45
    ),
    tags$link(rel = "stylesheet", href = asset("css/tokens.css")),
    tags$link(rel = "stylesheet", href = asset("css/app.css")),
    tags$link(rel = "stylesheet", href = asset("css/landing.css")),
    tags$script(src = asset("js/app.js")),
    tags$script(src = asset("js/landing.js"), defer = NA)
  )
}

axis_brand <- function() {
  tags$span(class = "brand-mark", "axis", tags$span(class = "brand-dot", ".", .noWS = "outside"))
}