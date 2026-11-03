# Shared page template for the inner app pages: header, control rail, numbered
# sections, KPI strip, and the empty state. Styles live in www/css/app.css.

page_header <- function(num, label, title, lead = NULL) {
  tags$header(
    class = "page-head",
    tags$div(class = "page-eyebrow",
      if (!is.null(num)) tags$span(class = "page-num", num),
      if (!is.null(num)) tags$span(class = "page-eyebrow-sep", "\u2014"),
      label
    ),
    tags$h1(class = "page-title", HTML(title)),
    if (!is.null(lead)) tags$p(class = "page-lead", lead)
  )
}

page_layout <- function(rail, ...) {
  tags$div(
    class = "page-layout",
    tags$aside(class = "page-rail", rail),
    tags$div(class = "page-main", ...)
  )
}

rail_group <- function(label, ...) {
  tags$div(
    class = "rail-group",
    tags$div(class = "rail-label", label),
    ...
  )
}

rail_note <- function(...) tags$p(class = "rail-note", ...)

page_section <- function(num, title, ..., lead = NULL) {
  tags$section(
    class = "page-section",
    tags$div(
      class = "page-section-head",
      tags$span(class = "page-section-num", num),
      tags$h2(class = "page-section-title", title),
      tags$span(class = "page-section-line")
    ),
    if (!is.null(lead)) tags$p(class = "page-section-lead", lead),
    ...
  )
}

kpi_card <- function(label, value, cls = "kpi-neutral") {
  tags$div(
    class = "kpi-card",
    tags$div(class = paste("kpi-value", cls), value),
    tags$div(class = "kpi-label", label)
  )
}

kpi_strip <- function(...) tags$div(class = "kpi-strip", ...)

# Horizontal 100% bar with a legend underneath. `df` needs label, pct (0-1), color.
stack_bar <- function(title, df) {
  seg <- lapply(seq_len(nrow(df)), function(i) {
    tags$span(class = "stack-seg",
      style = sprintf("flex-basis:%.3f%%;background:%s", 100 * df$pct[i], df$color[i]),
      title = sprintf("%s %.1f%%", df$label[i], 100 * df$pct[i]))
  })
  key <- lapply(seq_len(nrow(df)), function(i) {
    tags$li(
      tags$span(class = "stack-dot", style = sprintf("background:%s", df$color[i])),
      tags$span(class = "stack-name", df$label[i]),
      tags$span(class = "stack-pct", sprintf("%.1f%%", 100 * df$pct[i]))
    )
  })
  tags$div(class = "stack",
    tags$div(class = "stack-title", title),
    tags$div(class = "stack-bar", seg),
    tags$ul(class = "stack-key", key)
  )
}

# Single "Export" dropdown. Each item is list(id, label, ext) for a downloadHandler.
export_menu <- function(...) {
  items <- list(...)
  tags$div(class = "dropdown export-menu",
    tags$button(type = "button", class = "btn btn-default btn-sm dropdown-toggle",
      `data-bs-toggle` = "dropdown", `aria-expanded` = "false", "Export"),
    tags$ul(class = "dropdown-menu dropdown-menu-end",
      lapply(items, function(it) {
        tags$li(
          tags$a(id = it$id, class = "dropdown-item shiny-download-link",
            href = "", target = "_blank", download = NA,
            tags$span(it$label), tags$span(class = "export-ext", it$ext))
        )
      })
    )
  )
}

plot_plate <- function(id, height = "360px") {
  plotOutput(id, height = height)
}

empty_ring_svg <- function() {
  HTML(paste0(
    '<svg class="empty-ring" viewBox="0 0 120 120" aria-hidden="true">',
    '<circle cx="60" cy="60" r="46" class="er-outer"/>',
    '<ellipse cx="60" cy="60" rx="46" ry="15" class="er-ring"/>',
    '<ellipse cx="60" cy="60" rx="46" ry="15" class="er-ring er-tilt"/>',
    '<ellipse cx="60" cy="60" rx="15" ry="46" class="er-ring er-faint"/>',
    '<line x1="28" y1="96" x2="92" y2="24" class="er-axis"/>',
    '<circle cx="60" cy="60" r="6" class="er-core"/>',
    "</svg>"
  ))
}

empty_state <- function(title, body, cta_id = NULL,
                        cta_label = "Analyze a sample portfolio \u2192") {
  tags$div(
    class = "empty-state",
    empty_ring_svg(),
    tags$h4(title),
    tags$p(body),
    if (!is.null(cta_id)) {
      tags$div(class = "empty-actions",
        actionButton(cta_id, cta_label, class = "btn-primary"),
        actionLink(paste0(cta_id, "_build"), "or build your own")
      )
    }
  )
}
