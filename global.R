# Shiny loads this file before app.R so R/ modules are available on any host.

r_mods <- c(
  "config.R",
  "helpers.R",
  "plot_theme.R",
  "app_theme.R",
  "page_ui.R",
  "portfolio.R",
  "snapshots.R",
  "scenarios.R",
  "allocation.R",
  "landing_ui.R",
  "methodology_ui.R",
  "app_ui.R",
  "app_server.R"
)
for (m in r_mods) {
  f <- file.path("R", m)
  if (file.exists(f)) source(f, local = FALSE)
}
