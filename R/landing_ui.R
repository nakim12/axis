# Landing page (Home). Styles: www/css/landing.css. Sphere + scroll motion: www/js/landing.js.

landing_section_head <- function(num, label, title, lead = NULL) {
  tags$div(
    class = "landing-section-head", `data-reveal` = NA,
    tags$div(
      class = "landing-rule",
      tags$span(class = "landing-rule-num", num),
      tags$span(class = "landing-rule-line"),
      tags$span(class = "landing-rule-label", label)
    ),
    tags$h2(HTML(title)),
    if (!is.null(lead)) tags$p(class = "landing-section-lead", lead)
  )
}

# Small engraved-style sketches for the three steps (viewBox 240 x 96).
step_sketch_build <- function() {
  rows <- list(c("AAPL", 25), c("MSFT", 25), c("JPM", 20), c("JNJ", 15), c("XOM", 15))
  bars <- vapply(seq_along(rows), function(i) {
    y <- 8 + (i - 1) * 18
    w <- as.numeric(rows[[i]][2]) * 6
    sprintf(
      paste0(
        '<text x="0" y="%d" class="sk-label">%s</text>',
        '<line x1="44" y1="%d" x2="212" y2="%d" class="sk-track"/>',
        '<line x1="44" y1="%d" x2="%d" y2="%d" class="sk-bar"/>',
        '<text x="240" y="%d" class="sk-label sk-end">%s</text>'
      ),
      y + 4, rows[[i]][1], y, y, y, 44 + w, y, y + 4, rows[[i]][2]
    )
  }, character(1))
  HTML(paste0('<svg viewBox="0 0 240 96" class="step-sketch" aria-hidden="true">',
              paste(bars, collapse = ""), "</svg>"))
}

preview_growth_sketch <- function() {
  to_path <- function(v) {
    x <- seq(0, 360, length.out = length(v))
    y <- 92 - (v - 1) * 280
    paste0("M", paste(sprintf("%.1f %.1f", x, y), collapse = " L"))
  }
  port <- c(1, 1.01, 1.03, 1.02, 1.05, 1.07, 1.06, 1.04, 1.08, 1.11, 1.13, 1.12,
            1.15, 1.17, 1.16, 1.19, 1.21, 1.2, 1.23, 1.25)
  bench <- c(1, 1.005, 1.02, 1.01, 1.03, 1.04, 1.02, 0.995, 1.02, 1.04, 1.05, 1.045,
             1.06, 1.07, 1.065, 1.08, 1.1, 1.09, 1.11, 1.13)
  HTML(paste0(
    '<figure class="preview-growth"><figcaption>',
    '<span>Growth of $1</span>',
    '<span><i class="key key-port"></i>Portfolio <i class="key key-bench"></i>S&amp;P 500</span>',
    '</figcaption>',
    '<svg viewBox="0 0 360 100" preserveAspectRatio="none" aria-hidden="true">',
    '<line x1="0" y1="92" x2="360" y2="92" class="sk-rule"/>',
    '<path class="pg-bench" d="', to_path(bench), '"/>',
    '<path class="pg-port" d="', to_path(port), '"/>',
    "</svg></figure>"
  ))
}

step_sketch_diagnose <- function() {
  HTML(paste0(
    '<svg viewBox="0 0 240 96" class="step-sketch" aria-hidden="true">',
    '<line x1="0" y1="14" x2="240" y2="14" class="sk-rule"/>',
    '<path class="sk-dd-area" d="M0 14 L18 16 L30 24 L44 19 L58 14 L72 14 L86 30 L98 52 L110 78 ',
    'L120 60 L132 40 L146 22 L160 14 L174 14 L188 26 L198 34 L210 18 L224 14 L240 14 Z"/>',
    '<path class="sk-dd-line" d="M0 14 L18 16 L30 24 L44 19 L58 14 L72 14 L86 30 L98 52 L110 78 ',
    'L120 60 L132 40 L146 22 L160 14 L174 14 L188 26 L198 34 L210 18 L224 14 L240 14"/>',
    '<circle cx="110" cy="78" r="3" class="sk-dot"/>',
    '<text x="118" y="88" class="sk-label">max drawdown</text>',
    "</svg>"
  ))
}

step_sketch_decide <- function() {
  pairs <- list(c("AAPL", 25, 14), c("MSFT", 25, 16), c("JPM", 20, 22), c("JNJ", 15, 26), c("XOM", 15, 22))
  cols <- vapply(seq_along(pairs), function(i) {
    x <- 18 + (i - 1) * 46
    cur <- as.numeric(pairs[[i]][2]) * 2.6
    pro <- as.numeric(pairs[[i]][3]) * 2.6
    sprintf(
      paste0(
        '<rect x="%d" y="%.1f" width="9" height="%.1f" class="sk-cur"/>',
        '<rect x="%d" y="%.1f" width="9" height="%.1f" class="sk-pro"/>',
        '<text x="%d" y="94" class="sk-label sk-mid">%s</text>'
      ),
      x, 80 - cur, cur, x + 12, 80 - pro, pro, x + 10, pairs[[i]][1]
    )
  }, character(1))
  HTML(paste0('<svg viewBox="0 0 240 96" class="step-sketch" aria-hidden="true">',
              '<line x1="0" y1="80" x2="240" y2="80" class="sk-rule"/>',
              paste(cols, collapse = ""), "</svg>"))
}

landing_step <- function(num, label, title, body, sketch) {
  tags$div(
    class = "landing-step", `data-reveal` = NA,
    tags$div(class = "landing-step-num", paste0(num, " \u2014 ", label)),
    tags$div(class = "landing-step-sketch", sketch),
    tags$h3(title),
    tags$p(body)
  )
}

landing_feature <- function(num, title, body) {
  tags$div(
    class = "landing-feature", `data-reveal` = NA,
    tags$span(class = "landing-feature-num", num),
    tags$h4(title),
    tags$p(body)
  )
}

preview_kpi <- function(label, value, tone = "neutral") {
  tags$div(
    class = "preview-kpi",
    tags$div(class = paste0("preview-kpi-value tone-", tone), value),
    tags$div(class = "preview-kpi-label", label)
  )
}

preview_insight <- function(tag, text) {
  tags$div(
    class = "preview-insight",
    tags$span(class = "preview-insight-tag", tag),
    tags$p(text)
  )
}

landing_panel <- function() {
  tags$div(
    class = "landing-root",
    tags$div(class = "landing-progress", tags$span()),

    # ── Hero ────────────────────────────────────────────────────────────────
    tags$section(
      class = "landing-hero",
      tags$div(
        class = "landing-sphere-wrap", `aria-hidden` = "true",
        tags$canvas(id = "armillary", class = "landing-sphere")
      ),
      tags$div(
        class = "landing-hero-copy",
        tags$p(class = "landing-eyebrow", "portfolio intelligence \u00b7 risk \u00b7 allocation"),
        tags$h1(class = "landing-wordmark", "axis",
          tags$span(class = "brand-dot", ".", .noWS = "outside")),
        tags$p(class = "landing-tagline", "Decisions first."),
        tags$p(
          class = "landing-lead",
          "Build a weighted portfolio, benchmark it, and read its risk the way a careful ",
          "investor would: concentration, drawdowns, scenarios, and plain-language insight."
        ),
        tags$div(
          class = "landing-cta-row",
          actionButton("landing_sample", "Try a sample portfolio \u2192",
            class = "btn landing-btn-primary"),
          actionButton("landing_start", "Build your own",
            class = "btn landing-btn-ghost")
        ),
        tags$div(
          class = "landing-stats",
          tags$span(tags$b("30"), " tickers"),
          tags$span(tags$b("5"), " sectors"),
          tags$span(tags$b("4"), " stress episodes"),
          tags$span(tags$b(length(allocation_methods)), " allocation models")
        )
      ),
      tags$a(class = "landing-scroll-cue", href = "#landing-gap",
        tags$span("scroll"), tags$i())
    ),

    tags$div(
      class = "landing-body",

      # ── 01 The gap ──────────────────────────────────────────────────────────
      tags$section(
        class = "landing-section", id = "landing-gap",
        landing_section_head("01", "The gap",
          "You see returns.<br><em>You don\u2019t see risk.</em>"),
        tags$div(
          class = "landing-gap",
          tags$div(
            class = "gap-card", `data-reveal` = NA,
            tags$div(class = "gap-card-tag", "What a brokerage shows you"),
            tags$h3("A price chart and a number."),
            tags$p("Total return, a pie of holdings, maybe a day\u2019s change. It tells you where you ended up, ",
                   "not how exposed you were getting there or what could go wrong next.")
          ),
          tags$div(
            class = "gap-card gap-card--axis", `data-reveal` = NA,
            tags$div(class = "gap-card-tag", "What Axis shows you"),
            tags$h3("Where the risk actually sits."),
            tags$p("Which names drive variance, how the mix behaved in past sell-offs, how it compares with ",
                   "simply holding the index, and what a better-balanced allocation would look like.")
          )
        )
      ),

      # ── 02 How it works ─────────────────────────────────────────────────────
      tags$section(
        class = "landing-section",
        landing_section_head("02", "How it works", "Three steps. One workflow."),
        tags$div(
          class = "landing-steps",
          landing_step("01", "Build", "Define the portfolio",
            paste("Enter tickers and weights or load a template, then pick a benchmark",
                  "and a history window."),
            step_sketch_build()),
          landing_step("02", "Diagnose", "See risk in context",
            paste("Volatility, Sharpe, drawdowns, beta, contribution to risk, and rolling",
                  "metrics against your benchmark."),
            step_sketch_diagnose()),
          landing_step("03", "Decide", "Test the alternatives",
            paste("Replay stress episodes on your weights and compare your mix with",
                  "min-variance, inverse-vol, and max-Sharpe models."),
            step_sketch_decide())
        )
      ),

      # ── 03 Diagnosis preview ────────────────────────────────────────────────
      tags$section(
        class = "landing-section",
        landing_section_head("03", "Diagnosis", "Numbers, then what they mean.",
          "Every analysis ends in plain language, so the metrics come with an interpretation instead of a puzzle."),
        tags$div(
          class = "preview-card", `data-reveal` = NA,
          tags$div(
            class = "preview-head",
            tags$span(class = "preview-title", "Balanced Growth"),
            tags$span(class = "preview-meta", "AAPL \u00b7 MSFT \u00b7 JPM \u00b7 JNJ \u00b7 XOM  \u2014  vs S&P 500  \u2014  1 year")
          ),
          tags$div(
            class = "preview-grid",
            tags$div(
              class = "preview-main",
              tags$div(
                class = "preview-kpis",
                preview_kpi("Annual return", "25.0%", "gain"),
                preview_kpi("Volatility", "12.2%"),
                preview_kpi("Sharpe", "2.04", "gain"),
                preview_kpi("Max drawdown", "\u22126.4%", "loss"),
                preview_kpi("Beta", "0.40"),
                preview_kpi("Diversification", "2.10")
              ),
              preview_growth_sketch()
            ),
            tags$div(
              class = "preview-insights",
              preview_insight("concentration",
                "MSFT and AAPL together account for 73% of portfolio risk."),
              preview_insight("benchmark",
                "Outperformed the S&P 500 by 11.4% a year with a beta of 0.40."),
              preview_insight("sectors",
                "Technology is 50% of the portfolio, the largest single exposure.")
            )
          ),
          tags$p(class = "preview-foot", "Illustrative output from the Balanced Growth sample. Your numbers will differ.")
        )
      ),

      # ── 04 Capabilities ─────────────────────────────────────────────────────
      tags$section(
        class = "landing-section",
        landing_section_head("04", "Capabilities", "Built for investors who read numbers."),
        tags$div(
          class = "landing-features",
          landing_feature("i", "Portfolio-native metrics",
            "Weighted returns and covariance-aware risk, not isolated per-symbol summaries."),
          landing_feature("ii", "Benchmark grounding",
            "Cumulative paths, excess return, beta, and tracking error against SPY, QQQ, and peers."),
          landing_feature("iii", "Concentration signals",
            "Sector weights, HHI-style concentration, and the names that drive variance."),
          landing_feature("iv", "Scenarios & stress",
            "Replay known rough patches on your weights, then layer a daily shock for a mechanical what-if."),
          landing_feature("v", "Allocation",
            "Long-only model portfolios compared with your current mix, then walk-forward backtested with costs to see if they would have helped."),
          landing_feature("vi", "Exports",
            "Portfolio snapshots as JSON, metrics as CSV, and a bundled report you can keep or share.")
        )
      ),

      # ── 05 Trust ────────────────────────────────────────────────────────────
      tags$section(
        class = "landing-section",
        landing_section_head("05", "Method", "Careful about what it claims."),
        tags$div(
          class = "landing-trust", `data-reveal` = NA,
          tags$div(class = "trust-item",
            tags$h4("Public data"),
            tags$p("Daily adjusted closes from Yahoo Finance. Nothing proprietary, nothing hidden.")),
          tags$div(class = "trust-item",
            tags$h4("Every formula written down"),
            tags$p("Volatility, Sharpe, VaR, beta, risk contribution: each one is defined on the Methodology page.")),
          tags$div(class = "trust-item",
            tags$h4("Limits stated plainly"),
            tags$p("Confidence intervals on backtest results, and forecasts scored against a random walk, which they did not beat.")),
          tags$div(class = "trust-item",
            tags$h4("Education, not advice"),
            tags$p("Axis helps you reason about a portfolio. The decision stays yours."))
        ),
        tags$div(
          class = "landing-trust-link", `data-reveal` = NA,
          actionButton("landing_methodology", "Read the methodology \u2192",
            class = "btn landing-btn-ghost")
        )
      ),

      # ── Final CTA ───────────────────────────────────────────────────────────
      tags$section(
        class = "landing-final", `data-reveal` = NA,
        tags$p(class = "landing-eyebrow", "ready when you are"),
        tags$h2(HTML("Put your portfolio<br><em>under the instrument.</em>")),
        tags$p(class = "landing-final-lead",
          "Load a sample in one click, or bring your own tickers and weights."),
        tags$div(
          class = "landing-cta-row",
          actionButton("landing_cta_sample", "Try a sample portfolio \u2192",
            class = "btn landing-btn-primary"),
          actionButton("landing_cta_start", "Build your own",
            class = "btn landing-btn-ghost")
        )
      ),

      # ── Footer ──────────────────────────────────────────────────────────────
      tags$footer(
        class = "landing-footer",
        tags$div(
          class = "landing-footer-brand",
          tags$span(class = "landing-footer-mark", "axis",
            tags$span(class = "brand-dot", ".", .noWS = "outside")),
          tags$p(
            class = "landing-disclaimer",
            "Educational use only. Axis does not provide personalized investment, tax, or legal advice. ",
            "Past performance and backtests are not guarantees of future results."
          )
        ),
        tags$div(
          class = "landing-footer-links",
          actionLink("landing_scenarios", "Stress"),
          tags$a(href = "https://github.com/nakim12/investment-performance-tracker",
            target = "_blank", rel = "noopener", "GitHub")
        )
      )
    )
  )
}
