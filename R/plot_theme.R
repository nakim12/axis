# Observatory chart theme. Hex values mirror www/css/tokens.css; keep the two in sync.

AXIS <- list(
  parchment  = "#f1ebdf",
  vellum     = "#f8f4ec",
  sand       = "#e7decb",
  ink        = "#1d1a15",
  ink_soft   = "#4a4339",
  muted      = "#857b6a",
  rule       = "#b9ae9b",
  grid       = "#e4dccd",
  brass      = "#a0763a",
  brass_deep = "#6f4f1f",
  slate      = "#4f6b80",
  stone      = "#8a8170",
  gain       = "#4e7a56",
  loss       = "#a5463a"
)

AXIS$portfolio <- AXIS$brass
AXIS$benchmark <- AXIS$slate

# Oxide sits last so a holding is rarely painted in the "loss" color.
axis_ramp <- c(
  AXIS$brass, AXIS$slate, "#6f8f63", "#c9a35b",
  "#6e5470", "#3f7a78", AXIS$stone, AXIS$loss
)

register_axis_fonts <- function(dir = "fonts") {
  ok <- function(f) file.exists(file.path(dir, f))
  fams <- list(sans = "sans", mono = "mono")
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(fams)
  if (ok("Inter-Regular.ttf") && ok("Inter-SemiBold.ttf")) {
    try({
      systemfonts::register_font(
        "Axis Sans",
        plain = file.path(dir, "Inter-Regular.ttf"),
        bold  = file.path(dir, "Inter-SemiBold.ttf")
      )
      fams$sans <- "Axis Sans"
    }, silent = TRUE)
  }
  if (ok("JetBrainsMono-Regular.ttf")) {
    try({
      systemfonts::register_font("Axis Mono", plain = file.path(dir, "JetBrainsMono-Regular.ttf"))
      fams$mono <- "Axis Mono"
    }, silent = TRUE)
  }
  fams
}

AXIS_FONT <- register_axis_fonts()

axis_palette <- function(n) {
  if (n <= length(axis_ramp)) return(axis_ramp[seq_len(n)])
  grDevices::colorRampPalette(axis_ramp)(n)
}

theme_axis <- function(base_size = 13) {
  ggplot2::theme_minimal(base_size = base_size, base_family = AXIS_FONT$sans) +
    ggplot2::theme(
      plot.background    = ggplot2::element_rect(fill = AXIS$vellum, color = NA),
      panel.background   = ggplot2::element_rect(fill = AXIS$vellum, color = NA),
      panel.border       = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = AXIS$grid, linewidth = 0.35),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      axis.line.x        = ggplot2::element_line(color = AXIS$rule, linewidth = 0.4),
      axis.ticks         = ggplot2::element_blank(),
      axis.text          = ggplot2::element_text(color = AXIS$muted, size = ggplot2::rel(0.74), family = AXIS_FONT$mono),
      axis.title         = ggplot2::element_text(color = AXIS$muted, size = ggplot2::rel(0.82)),
      plot.title         = ggplot2::element_text(
        color = AXIS$ink, face = "bold", size = ggplot2::rel(1.05),
        margin = ggplot2::margin(b = 8)
      ),
      plot.title.position = "plot",
      plot.subtitle      = ggplot2::element_text(color = AXIS$muted, margin = ggplot2::margin(b = 10)),
      plot.caption       = ggplot2::element_text(color = AXIS$muted, size = ggplot2::rel(0.75)),
      plot.caption.position = "plot",
      legend.text        = ggplot2::element_text(color = AXIS$ink_soft, size = ggplot2::rel(0.82)),
      legend.title       = ggplot2::element_text(color = AXIS$muted, size = ggplot2::rel(0.8)),
      legend.key         = ggplot2::element_blank(),
      strip.text         = ggplot2::element_text(color = AXIS$ink, face = "bold", hjust = 0),
      plot.margin        = ggplot2::margin(14, 16, 12, 12)
    )
}

theme_axis_void <- function(base_size = 13) {
  ggplot2::theme_void(base_size = base_size, base_family = AXIS_FONT$sans) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(fill = AXIS$vellum, color = NA),
      plot.title      = ggplot2::element_text(color = AXIS$ink, face = "bold", margin = ggplot2::margin(b = 8)),
      legend.text     = ggplot2::element_text(color = AXIS$ink_soft),
      legend.title    = ggplot2::element_text(color = AXIS$muted),
      plot.margin     = ggplot2::margin(14, 16, 12, 12)
    )
}

renderAxisPlot <- function(expr, ..., bg = AXIS$vellum, env = parent.frame(), quoted = FALSE) {
  if (!quoted) expr <- substitute(expr)
  shiny::renderPlot(expr, ..., bg = bg, env = env, quoted = TRUE)
}

plot_message <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg,
      color = AXIS$muted, size = 4.6, lineheight = 1.25, family = AXIS_FONT$sans) +
    ggplot2::xlim(-1, 1) + ggplot2::ylim(-1, 1) +
    theme_axis_void()
}

ggplot2::theme_set(theme_axis())

# Bundled fonts only render through ragg; the explicit call also lets
# rsconnect's dependency scan include ragg in the deploy manifest.
options(shiny.useragg = requireNamespace("ragg", quietly = TRUE))

options(
  ggplot2.discrete.colour = function(...) ggplot2::discrete_scale("colour", palette = axis_palette, ...),
  ggplot2.discrete.fill   = function(...) ggplot2::discrete_scale("fill", palette = axis_palette, ...)
)

ggplot2::update_geom_defaults("line",  list(colour = AXIS$brass))
ggplot2::update_geom_defaults("point", list(colour = AXIS$brass))
ggplot2::update_geom_defaults("col",   list(fill = AXIS$brass))
ggplot2::update_geom_defaults("area",  list(fill = AXIS$brass))
ggplot2::update_geom_defaults("text",  list(colour = AXIS$ink_soft, family = AXIS_FONT$sans))
