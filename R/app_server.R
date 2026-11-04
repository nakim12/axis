# Main Shiny server logic.

app_server <- function(input, output, session) {

  # Home tab flag drives landing-only chrome (transparent nav, progress bar, sphere loop).
  observe({
    v <- input$main_nav
    home <- is.null(v) || identical(v, "home")
    session$sendCustomMessage("setHomeTab", list(home = home))
  })

  # ══════════════════════════════════════════════════════════════════════════

  # PORTFOLIO BUILDER
  # ══════════════════════════════════════════════════════════════════════════

  # Classify insight copy for card styling (warn / ok / neutral).
  insight_card_class <- function(text) {
    tl <- tolower(text)
    warn_pat <- c(
      "underperform", "negative sharpe", "limited diversification",
      "significant downside", "heavily concentrated", "dominant risk",
      "diverge meaningfully", "highly correlated"
    )
    ok_pat <- c(
      "outperformed", "strong risk-adjusted", "strong diversification",
      "moderate diversification benefit"
    )
    if (any(vapply(warn_pat, function(p) grepl(p, tl, fixed = TRUE), logical(1)))) {
      return("insight-card insight-card--warn")
    }
    if (any(vapply(ok_pat, function(p) grepl(p, tl, fixed = TRUE), logical(1)))) {
      return("insight-card insight-card--ok")
    }
    "insight-card insight-card--neutral"
  }

  # Weight-only facts are exact; risk estimates firm up within about a year of
  # daily data; mean returns stay noisy for much longer.
  insight_confidence <- function(text, n_days) {
    tl <- tolower(text)
    has <- function(pats) any(vapply(pats, function(p) grepl(p, tl, fixed = TRUE), logical(1)))
    if (has(c("sector", "concentrated"))) return("high")
    if (!has("tracking error") && has(c("return", "outperform", "underperform", "sharpe", "declined"))) {
      return(if (n_days >= 750) "medium" else "low")
    }
    if (n_days >= 500) "high" else if (n_days >= 120) "medium" else "low"
  }

  confidence_note <- c(
    high   = "Follows from the weights, or from a long enough sample to be stable.",
    medium = "A reasonable estimate on this window; it can shift with a different period.",
    low    = "Based on a short or noisy sample. Treat it as a hint, not a finding."
  )

  insight_card <- function(text, n_days) {
    conf <- insight_confidence(text, n_days)
    tags$div(class = insight_card_class(text),
      tags$span(class = paste0("insight-conf insight-conf--", conf),
        title = confidence_note[[conf]], paste(conf, "confidence")),
      tags$p(text)
    )
  }

  portfolio <- reactiveValues(
    holdings          = tibble(ticker = character(), weight = numeric(), sector = character()),
    analyzed          = FALSE,
    price_data        = NULL,
    returns           = NULL,
    benchmark_data    = NULL,
    benchmark_returns = NULL,
    benchmark_name    = "SPY",
    metrics           = NULL,
    weights_frac      = NULL,
    sector_weights    = NULL,
    drawdown          = NULL,
    risk_contrib      = NULL,
    diversification   = NULL,
    holding_cor       = NULL,
    return_attrib     = NULL,
    sector_return_attrib = NULL
  )

  diagnosis_pins <- reactiveValues(a = NULL, b = NULL)

  # -- Landing page CTAs ----------------------------------------------------

  go_to <- function(page) updateNavbarPage(session, inputId = "main_nav", selected = page)

  load_default_sample <- function() {
    sample_name <- names(sample_portfolios)[[1]]
    sample      <- sample_portfolios[[sample_name]]
    portfolio$holdings <- tibble(
      ticker = sample$tickers,
      weight = sample$weights,
      sector = sapply(sample$tickers, get_sector_for_ticker)
    )
    portfolio$analyzed <- FALSE
    updateSelectInput(session, "sample_portfolio", selected = sample_name)
    sample_name
  }

  observeEvent(list(input$landing_start, input$landing_cta_start), {
    go_to("portfolio")
  }, ignoreInit = TRUE)

  observeEvent(list(input$landing_sample, input$landing_cta_sample), {
    sample_name <- load_default_sample()
    go_to("portfolio")
    showNotification(paste("Loaded sample:", sample_name), type = "message")
  }, ignoreInit = TRUE)

  observeEvent(input$landing_methodology, go_to("methodology"), ignoreInit = TRUE)
  observeEvent(input$landing_scenarios,   go_to("stress"),      ignoreInit = TRUE)
  observeEvent(input$goto_diagnosis,      go_to("diagnosis"),   ignoreInit = TRUE)

  # Empty-state CTAs on pages that need an analyzed portfolio.
  empty_cta_ids <- c("empty_sample_diag", "empty_sample_stress", "empty_sample_alloc")

  lapply(empty_cta_ids, function(id) {
    observeEvent(input[[id]], {
      load_default_sample()
      run_analysis()
    })
    observeEvent(input[[paste0(id, "_build")]], go_to("portfolio"))
  })

  # -- Add holding ----------------------------------------------------------

  observeEvent(input$add_holding, {
    req(input$portfolio_ticker_input)
    ticker <- toupper(trimws(input$portfolio_ticker_input))
    weight <- input$portfolio_weight_input

    if (nchar(ticker) == 0) {
      showNotification("Please enter a ticker symbol.", type = "error")
      return()
    }
    if (is.na(weight) || weight <= 0) {
      showNotification("Weight must be greater than 0.", type = "error")
      return()
    }

    if (ticker %in% portfolio$holdings$ticker) {
      portfolio$holdings$weight[portfolio$holdings$ticker == ticker] <- weight
      showNotification(paste(ticker, "weight updated to", weight, "%"), type = "message")
    } else {
      sector <- get_sector_for_ticker(ticker)
      portfolio$holdings <- bind_rows(
        portfolio$holdings,
        tibble(ticker = ticker, weight = weight, sector = sector)
      )
      showNotification(paste(ticker, "added at", weight, "%"), type = "message")
    }

    updateSelectizeInput(session, "portfolio_ticker_input", selected = "")
    portfolio$analyzed <- FALSE
  })

  # -- Load sample ----------------------------------------------------------

  observeEvent(input$load_sample, {
    req(input$sample_portfolio, input$sample_portfolio != "")
    sample <- sample_portfolios[[input$sample_portfolio]]
    sectors <- sapply(sample$tickers, get_sector_for_ticker)
    portfolio$holdings <- tibble(
      ticker = sample$tickers,
      weight = sample$weights,
      sector = sectors
    )
    portfolio$analyzed <- FALSE
    showNotification(paste("Loaded:", input$sample_portfolio), type = "message")
  })

  # -- Clear all holdings ---------------------------------------------------

  observeEvent(input$clear_portfolio, {
    portfolio$holdings  <- tibble(ticker = character(), weight = numeric(), sector = character())
    portfolio$analyzed  <- FALSE
    portfolio$returns   <- NULL
    portfolio$metrics   <- NULL
    portfolio$return_attrib <- NULL
    portfolio$sector_return_attrib <- NULL
  })

  # -- Portfolio snapshot (.json) export / import ---------------------------

  output$download_portfolio_snapshot <- downloadHandler(
    filename = function() {
      lab <- input$portfolio_snapshot_label
      slug <- if (is.null(lab) || !nzchar(trimws(lab))) {
        format(Sys.Date(), "%Y-%m-%d")
      } else {
        gsub("[^A-Za-z0-9._-]+", "-", trimws(lab))
      }
      sprintf("portfolio-snapshot-%s.json", slug)
    },
    content = function(file) {
      req(nrow(portfolio$holdings) > 0)
      ms <- if (isTRUE(portfolio$analyzed)) {
        metrics_summary_for_json(portfolio$metrics)
      } else {
        NULL
      }
      snap <- build_portfolio_snapshot_list(
        portfolio$holdings,
        input$benchmark_ticker,
        input$portfolio_date_range[1],
        input$portfolio_date_range[2],
        label = input$portfolio_snapshot_label,
        metrics = ms
      )
      writeLines(
        jsonlite::toJSON(snap, pretty = TRUE, auto_unbox = TRUE),
        file,
        useBytes = TRUE
      )
    }
  )

  observeEvent(input$portfolio_snapshot_upload, {
    up <- input$portfolio_snapshot_upload
    if (is.null(up) || !is.data.frame(up) || nrow(up) < 1L) {
      return(invisible(NULL))
    }
    raw <- readBin(up$datapath[1], "raw", file.info(up$datapath[1])$size)
    txt <- rawToChar(raw)
    Encoding(txt) <- "UTF-8"
    obj <- tryCatch(
      parse_portfolio_snapshot(txt),
      error = function(e) {
        showNotification(conditionMessage(e), type = "error", duration = 8)
        return(NULL)
      }
    )
    req(obj)

    portfolio$holdings <- snapshot_holdings_tibble(obj)
    bench <- obj$benchmark_ticker
    if (!bench %in% unname(benchmark_choices)) {
      showNotification(
        paste("Unknown benchmark in file:", bench, "— using SPY."),
        type = "warning", duration = 5)
      bench <- "SPY"
    }
    updateSelectInput(session, "benchmark_ticker", selected = bench)
    dr <- obj$date_range
    if (is.list(dr) && !is.null(dr$start) && !is.null(dr$end)) {
      updateDateRangeInput(session, "portfolio_date_range",
        start = as.Date(dr$start),
        end   = as.Date(dr$end))
    } else if (is.character(dr) && length(dr) >= 2L) {
      updateDateRangeInput(session, "portfolio_date_range",
        start = as.Date(dr[[1]]),
        end   = as.Date(dr[[2]]))
    }

    portfolio$analyzed <- FALSE
    portfolio$returns   <- NULL
    portfolio$metrics   <- NULL
    portfolio$return_attrib <- NULL
    portfolio$sector_return_attrib <- NULL

    lab <- obj$label
    showNotification(
      paste0(
        "Loaded snapshot",
        if (!is.null(lab) && nzchar(as.character(lab))) paste0(": ", lab) else ""
      ),
      type = "message")
  }, ignoreNULL = TRUE, ignoreInit = TRUE)

  # -- Holdings ledger ------------------------------------------------------

  observeEvent(input$remove_holding_ticker, {
    tk <- input$remove_holding_ticker
    req(tk %in% portfolio$holdings$ticker)
    portfolio$holdings <- portfolio$holdings %>% filter(ticker != tk)
    portfolio$analyzed <- FALSE
  })

  output$holdings_list <- renderUI({
    h <- portfolio$holdings
    if (nrow(h) == 0) {
      return(tags$div(class = "empty-inline",
        "No holdings yet. Add a ticker from the panel, or load a sample portfolio."))
    }
    cols  <- unname(holding_colors()[h$ticker])
    scale <- max(h$weight)
    rows <- lapply(seq_len(nrow(h)), function(i) {
      tk <- h$ticker[i]
      tags$li(class = "holding-row",
        tags$span(class = "holding-swatch", style = sprintf("background:%s", cols[i])),
        tags$span(class = "holding-ticker", tk),
        tags$span(class = "holding-sector", h$sector[i]),
        tags$span(class = "holding-bar",
          tags$span(style = sprintf("width:%.1f%%;background:%s", 100 * h$weight[i] / scale, cols[i]))
        ),
        tags$span(class = "holding-weight", sprintf("%.1f%%", h$weight[i])),
        tags$button(
          type = "button", class = "holding-remove",
          title = paste("Remove", tk), `aria-label` = paste("Remove", tk),
          onclick = sprintf(
            "Shiny.setInputValue('remove_holding_ticker', %s, {priority: 'event'})",
            jsonlite::toJSON(tk, auto_unbox = TRUE)
          ),
          HTML("&times;")
        )
      )
    })
    tags$ul(class = "holdings-list", rows)
  })

  # -- Summary stats --------------------------------------------------------

  output$total_weight_display <- renderUI({
    total <- sum(portfolio$holdings$weight)
    if (nrow(portfolio$holdings) == 0) {
      return(tags$div(class = "kpi-value kpi-neutral", "--"))
    }
    ok <- abs(total - 100) < 0.5
    tags$div(
      class = paste("kpi-value", if (ok) "weight-ok" else "weight-warn"),
      sprintf("%.1f%%", total),
      if (!ok) tags$span(class = "kpi-note",
        if (total > 100) "over-allocated" else "under-allocated")
    )
  })

  output$num_holdings_display <- renderText({
    nrow(portfolio$holdings)
  })

  output$concentration_display <- renderText({
    if (nrow(portfolio$holdings) == 0) return("--")
    score <- compute_concentration_score(portfolio$holdings$weight)
    sprintf("%.2f", score)
  })

  # One color per holding and per sector, shared by the ledger, the allocation
  # bars, and every Diagnosis chart.
  holding_colors <- reactive({
    h <- portfolio$holdings
    setNames(axis_palette(nrow(h)), h$ticker)
  })

  sector_colors <- reactive({
    h <- portfolio$holdings
    if (nrow(h) == 0) return(character())
    sw <- compute_sector_weights(h$ticker, h$weight) %>% arrange(desc(weight))
    setNames(axis_palette(nrow(sw)), sw$sector)
  })

  # -- Allocation bars ------------------------------------------------------

  output$allocation_bars <- renderUI({
    h <- portfolio$holdings
    if (nrow(h) == 0) {
      return(tags$div(class = "empty-inline",
        "The split by holding and by sector appears here once you add holdings."))
    }
    by_holding <- tibble(label = h$ticker, pct = h$weight / sum(h$weight),
      color = unname(holding_colors()[h$ticker]))
    sw <- compute_sector_weights(h$ticker, h$weight) %>%
      mutate(pct = weight / sum(weight)) %>%
      arrange(desc(pct))
    by_sector <- tibble(label = sw$sector, pct = sw$pct, color = unname(sector_colors()[sw$sector]))
    tagList(
      stack_bar("By holding", by_holding),
      stack_bar("By sector", by_sector)
    )
  })

  # -- Analyze portfolio ----------------------------------------------------

  observeEvent(input$analyze_portfolio, run_analysis())

  run_analysis <- function() {
    holdings <- portfolio$holdings
    if (nrow(holdings) == 0) {
      showNotification("Add at least one holding first.", type = "error")
      return()
    }

    weights <- holdings$weight
    names(weights) <- holdings$ticker
    weights_frac <- weights / sum(weights)

    withProgress(message = "Analyzing portfolio...", value = 0, {

      incProgress(0.15, detail = "Fetching holdings data...")
      price_data <- fetch_stock_data(
        holdings$ticker,
        from_date = input$portfolio_date_range[1],
        to_date   = input$portfolio_date_range[2])

      if (is.null(price_data)) {
        showNotification("Could not fetch price data. Check ticker symbols.", type = "error")
        return()
      }

      failed <- setdiff(holdings$ticker, unique(price_data$ticker))
      if (length(failed) > 0) {
        showNotification(
          paste("No data for:", paste(failed, collapse = ", ")),
          type = "warning", duration = 6)
      }

      incProgress(0.15, detail = "Fetching benchmark data...")
      benchmark_data <- fetch_stock_data(
        input$benchmark_ticker,
        from_date = input$portfolio_date_range[1],
        to_date   = input$portfolio_date_range[2])

      incProgress(0.2, detail = "Computing portfolio returns...")
      port_returns <- compute_portfolio_returns(price_data, weights_frac)

      if (is.null(port_returns)) {
        showNotification("Could not compute portfolio returns.", type = "error")
        return()
      }

      bench_returns <- NULL
      if (!is.null(benchmark_data)) {
        bw <- c(1)
        names(bw) <- input$benchmark_ticker
        bench_returns <- compute_portfolio_returns(benchmark_data, bw)
      }

      incProgress(0.2, detail = "Computing metrics...")
      metrics <- compute_portfolio_metrics(port_returns, bench_returns)

      sector_w <- compute_sector_weights(holdings$ticker, holdings$weight)

      incProgress(0.1, detail = "Computing diagnostics...")
      dd_series <- compute_drawdown_series(port_returns)
      risk_c    <- compute_risk_contribution(price_data, weights_frac)
      div_ratio <- compute_diversification_ratio(price_data, weights_frac)
      hold_cor  <- compute_holding_correlations(price_data)
      ret_attr  <- compute_return_attribution(price_data, weights_frac)
      sec_attr  <- if (!is.null(ret_attr)) aggregate_return_attrib_by_sector(ret_attr) else NULL

      portfolio$price_data        <- price_data
      portfolio$returns           <- port_returns
      portfolio$benchmark_data    <- benchmark_data
      portfolio$benchmark_returns <- bench_returns
      portfolio$benchmark_name    <- names(benchmark_choices)[benchmark_choices == input$benchmark_ticker]
      portfolio$metrics           <- metrics
      portfolio$weights_frac      <- weights_frac
      portfolio$sector_weights    <- sector_w
      portfolio$drawdown          <- dd_series
      portfolio$risk_contrib      <- risk_c
      portfolio$diversification   <- div_ratio
      portfolio$holding_cor       <- hold_cor
      portfolio$return_attrib     <- ret_attr
      portfolio$sector_return_attrib <- sec_attr
      portfolio$analyzed          <- TRUE

      incProgress(0.3, detail = "Done!")
    })

    showNotification("Portfolio analysis complete!", type = "message", duration = 3)
  }

  # -- Results section (dynamic UI) -----------------------------------------

  output$portfolio_results_section <- renderUI({
    if (!portfolio$analyzed) {
      return(page_section("03", "Results",
        tags$div(class = "empty-inline",
          "Add holdings, then ", tags$b("Analyze portfolio"), " to see results here.")
      ))
    }

    m <- portfolio$metrics

    ret_cls   <- if (m$annual_return >= 0) "kpi-positive" else "kpi-negative"
    sharp_cls <- if (m$sharpe_ratio > 0.5) "kpi-positive" else
                 if (m$sharpe_ratio >= 0)  "kpi-neutral"  else "kpi-negative"
    beta_val  <- if (!is.null(m$beta) && !is.na(m$beta)) sprintf("%.2f", m$beta) else "N/A"

    page_section("03", "Results",
      kpi_strip(
        kpi_card("Annual return", sprintf("%.1f%%", m$annual_return * 100), ret_cls),
        kpi_card("Volatility",    sprintf("%.1f%%", m$annual_volatility * 100)),
        kpi_card("Sharpe",        sprintf("%.2f", m$sharpe_ratio), sharp_cls),
        kpi_card("Max drawdown",  sprintf("%.1f%%", m$max_drawdown * 100), "kpi-negative"),
        kpi_card("Beta",          beta_val)
      ),
      plot_plate("portfolio_perf_chart", "400px"),
      tags$div(class = "section-cta",
        actionButton("goto_diagnosis", "Open the full diagnosis \u2192", class = "btn-default")
      )
    )
  })

  # -- Performance chart ----------------------------------------------------

  output$portfolio_perf_chart <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$returns)

    port_df <- portfolio$returns %>%
      transmute(date, cumulative_return, series = "Portfolio")

    if (!is.null(portfolio$benchmark_returns)) {
      bench_df <- portfolio$benchmark_returns %>%
        transmute(date, cumulative_return, series = portfolio$benchmark_name)
      combined <- bind_rows(port_df, bench_df)
    } else {
      combined <- port_df
    }

    ggplot(combined, aes(x = date, y = cumulative_return, color = series)) +
      geom_area(data = port_df, aes(x = date, y = cumulative_return),
        inherit.aes = FALSE, fill = AXIS$brass, alpha = 0.09) +
      geom_line(linewidth = 0.75) +
      geom_hline(yintercept = 0, linetype = "dotted", color = AXIS$rule) +
      labs(
        title = "Cumulative return",
        x = NULL, y = NULL, color = ""
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c("Portfolio" = AXIS$portfolio,
                                    setNames(AXIS$benchmark, portfolio$benchmark_name))) +
      theme_axis(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # ══════════════════════════════════════════════════════════════════════════
  # DIAGNOSIS TAB
  # ══════════════════════════════════════════════════════════════════════════

  # -- Rolling metrics reactive (recomputes when window slider changes) -----

  diag_rolling <- reactive({
    req(portfolio$analyzed, portfolio$returns)
    window <- if (!is.null(input$diag_rolling_window)) input$diag_rolling_window else 30
    compute_rolling_metrics(portfolio$returns, portfolio$benchmark_returns, window)
  })

  # -- Diagnosis content (dynamic UI) ---------------------------------------

  output$diagnosis_content <- renderUI({
    if (!portfolio$analyzed) {
      return(empty_state(
        "Nothing under the instrument yet.",
        "Analyze a portfolio to see its return, risk, benchmark context, and plain-language insights.",
        cta_id = "empty_sample_diag"
      ))
    }

    m  <- portfolio$metrics
    sw <- portfolio$sector_weights
    rc <- portfolio$risk_contrib
    dr <- portfolio$diversification

    ret_cls     <- if (m$annual_return >= 0) "kpi-positive" else "kpi-negative"
    sharp_cls   <- if (m$sharpe_ratio > 0.5) "kpi-positive" else
                   if (m$sharpe_ratio >= 0)  "kpi-neutral"  else "kpi-negative"
    sortino_val <- if (!is.na(m$sortino_ratio)) sprintf("%.2f", m$sortino_ratio) else "N/A"
    sortino_cls <- if (!is.na(m$sortino_ratio) && m$sortino_ratio > 0.5) "kpi-positive" else
                   if (!is.na(m$sortino_ratio) && m$sortino_ratio >= 0)  "kpi-neutral"  else "kpi-negative"
    beta_val    <- if (!is.null(m$beta) && !is.na(m$beta)) sprintf("%.2f", m$beta) else "N/A"

    insights <- generate_diagnosis_insights(m, sw, rc, dr, portfolio$holding_cor)
    actions  <- generate_action_suggestions(
      m, portfolio$return_attrib, rc, dr, portfolio$holding_cor)

    dr_cls <- if (!is.na(dr) && dr > 1.3) "kpi-positive" else
              if (!is.na(dr) && dr > 1.0) "kpi-neutral"  else "kpi-negative"

    tagList(
      tags$div(class = "page-toolbar",
        tags$p(class = "page-context",
          sprintf("%d holdings \u00b7 vs %s \u00b7 %s to %s",
                  nrow(portfolio$holdings), portfolio$benchmark_name,
                  format(min(portfolio$returns$date), "%b %Y"),
                  format(max(portfolio$returns$date), "%b %Y"))),
        export_menu(
          list(id = "download_diagnosis_bundle",   label = "Full report",          ext = ".zip"),
          list(id = "download_diagnosis",          label = "Diagnosis summary",    ext = ".txt"),
          list(id = "download_metrics_csv",        label = "Metrics",              ext = ".csv"),
          list(id = "download_holding_attrib_csv", label = "Holding attribution",  ext = ".csv"),
          list(id = "download_sector_attrib_csv",  label = "Sector attribution",   ext = ".csv")
        )
      ),

      page_section("01", "Headline",
        tags$div(class = "kpi-strip kpi-strip--hero",
          kpi_card("Annual return", sprintf("%.1f%%", m$annual_return * 100), ret_cls),
          kpi_card("Volatility",    sprintf("%.1f%%", m$annual_volatility * 100)),
          kpi_card("Sharpe",        sprintf("%.2f", m$sharpe_ratio), sharp_cls),
          kpi_card("Sortino",       sortino_val, sortino_cls),
          kpi_card("Max drawdown",  sprintf("%.1f%%", m$max_drawdown * 100), "kpi-negative"),
          kpi_card("Beta",          beta_val)
        ),
        if (length(insights) > 0) {
          tags$div(class = "insight-grid",
            lapply(insights, insight_card, n_days = nrow(portfolio$returns))
          )
        },
        if (length(actions) > 0) {
          tagList(
            tags$h3(class = "subhead", "What to consider next"),
            tags$p(class = "page-section-lead",
              "Ideas to stress-test your allocation, not recommendations."),
            tags$div(class = "insight-grid",
              lapply(actions, function(txt) {
                div(class = "insight-card insight-card--action", p(txt))
              })
            )
          )
        }
      ),

      page_section("02", "Performance vs benchmark",
        tags$div(class = "plot-grid plot-grid--7-5",
          plot_plate("diag_perf_chart", "360px"),
          plot_plate("diag_drawdown_chart", "360px")
        ),
        plot_plate("perf_port_excess", "300px"),
        tags$h3(class = "subhead", "Period summary"),
        tableOutput("perf_port_summary")
      ),

      page_section("03", "Rolling metrics",
        tags$div(class = "inline-controls",
          sliderInput("diag_rolling_window", "Rolling window (days)",
            min = 10, max = 60, value = 30, step = 5)
        ),
        tags$div(class = "plot-grid plot-grid--2",
          plot_plate("diag_rolling_vol_chart", "330px"),
          plot_plate("diag_rolling_sharpe_chart", "330px")
        )
      ),

      page_section("04", "Risk & diversification",
        tags$div(class = "plot-grid plot-grid--2",
          plot_plate("diag_risk_contrib_chart", "360px"),
          plot_plate("diag_corr_heatmap", "360px")
        ),
        tags$div(class = "plot-grid plot-grid--2",
          tags$div(
            plot_plate("diag_sector_chart", "300px"),
            tags$h3(class = "subhead", "Sector weights"),
            tableOutput("diag_sector_table")
          ),
          tags$div(class = "score-card",
            kpi_card("Diversification ratio",
              if (!is.na(dr)) sprintf("%.2f", dr) else "N/A", dr_cls),
            tags$p(
              "Values above 1.0 indicate a diversification benefit. Higher values mean holdings ",
              "are less correlated, pulling portfolio risk below the weighted average of individual risks."
            )
          )
        )
      ),

      page_section("05", "Return attribution",
        lead = paste(
          "Each holding's share of the sum of daily portfolio returns (fixed weights).",
          "Read alongside risk contribution; the names can differ when correlations matter."),
        tags$div(class = "plot-grid plot-grid--2",
          tableOutput("diag_return_attrib_table"),
          plot_plate("diag_return_attrib_chart", "320px")
        )
      ),

      page_section("06", "Sector attribution",
        lead = paste(
          "Sector weights sum holdings; linear share sums each holding\u2019s attribution within the sector.",
          "Weighted period return blends holding period returns inside that sector."),
        tags$div(class = "plot-grid plot-grid--2",
          tableOutput("diag_sector_return_table"),
          plot_plate("diag_sector_return_chart", "320px")
        )
      ),

      page_section("07", "Detailed metrics",
        tableOutput("diag_metrics_table")
      ),

      page_section("08", "Compare saved runs",
        lead = paste(
          "Pins capture headline metrics for the current analysis.",
          "They stay in this browser session until you clear them or refresh."),
        tags$div(class = "pin-controls",
          textInput("diagnosis_pin_a_label", "Pin A label", value = "Scenario A"),
          actionButton("diagnosis_pin_save_a", "Save current as A", class = "btn-default btn-sm"),
          textInput("diagnosis_pin_b_label", "Pin B label", value = "Scenario B"),
          actionButton("diagnosis_pin_save_b", "Save current as B", class = "btn-default btn-sm"),
          actionLink("diagnosis_pins_clear", "Clear pins", class = "rail-link")
        ),
        tableOutput("diagnosis_compare_table")
      )
    )
  })

  # -- Diagnosis: Cumulative performance chart ------------------------------

  output$diag_perf_chart <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$returns)

    port_df <- portfolio$returns %>%
      transmute(date, cumulative_return, series = "Portfolio")

    if (!is.null(portfolio$benchmark_returns)) {
      bench_df <- portfolio$benchmark_returns %>%
        transmute(date, cumulative_return, series = portfolio$benchmark_name)
      combined <- bind_rows(port_df, bench_df)
    } else {
      combined <- port_df
    }

    ggplot(combined, aes(x = date, y = cumulative_return, color = series)) +
      geom_area(data = port_df, aes(x = date, y = cumulative_return),
        inherit.aes = FALSE, fill = AXIS$brass, alpha = 0.09) +
      geom_line(linewidth = 0.75) +
      geom_hline(yintercept = 0, linetype = "dotted", color = AXIS$rule) +
      labs(title = "Cumulative return", x = NULL, y = NULL, color = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c("Portfolio" = AXIS$portfolio,
                                    setNames(AXIS$benchmark, portfolio$benchmark_name))) +
      theme_axis(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # -- Diagnosis: Drawdown chart --------------------------------------------

  output$diag_drawdown_chart <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$drawdown)

    ggplot(portfolio$drawdown, aes(x = date, y = drawdown)) +
      geom_area(fill = AXIS$loss, alpha = 0.16) +
      geom_line(color = AXIS$loss, linewidth = 0.45) +
      geom_hline(yintercept = 0, linewidth = 0.3) +
      labs(title = "Drawdown from peak", x = NULL, y = NULL) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_axis(base_size = 13)
  })

  # -- Diagnosis: Rolling volatility ----------------------------------------

  output$diag_rolling_vol_chart <- renderAxisPlot({
    rolling <- diag_rolling()
    req(rolling)

    plot_df <- tibble(date = rolling$date, Portfolio = rolling$rolling_vol)
    if ("bench_rolling_vol" %in% names(rolling)) {
      plot_df[[portfolio$benchmark_name]] <- rolling$bench_rolling_vol
    }

    plot_long <- plot_df %>%
      pivot_longer(-date, names_to = "series", values_to = "volatility") %>%
      filter(!is.na(volatility))

    window <- if (!is.null(input$diag_rolling_window)) input$diag_rolling_window else 30

    ggplot(plot_long, aes(x = date, y = volatility, color = series)) +
      geom_line(linewidth = 0.75) +
      labs(title = paste0("Rolling ", window, "-day volatility, annualized"),
           x = NULL, y = NULL, color = NULL) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c("Portfolio" = AXIS$portfolio,
                                    setNames(AXIS$benchmark, portfolio$benchmark_name))) +
      theme_axis(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # -- Diagnosis: Rolling Sharpe --------------------------------------------

  output$diag_rolling_sharpe_chart <- renderAxisPlot({
    rolling <- diag_rolling()
    req(rolling)

    plot_df <- tibble(date = rolling$date, Portfolio = rolling$rolling_sharpe)
    if ("bench_rolling_sharpe" %in% names(rolling)) {
      plot_df[[portfolio$benchmark_name]] <- rolling$bench_rolling_sharpe
    }

    plot_long <- plot_df %>%
      pivot_longer(-date, names_to = "series", values_to = "sharpe") %>%
      filter(!is.na(sharpe))

    window <- if (!is.null(input$diag_rolling_window)) input$diag_rolling_window else 30

    ggplot(plot_long, aes(x = date, y = sharpe, color = series)) +
      geom_line(linewidth = 0.75) +
      geom_hline(yintercept = 0, linetype = "dashed", color = AXIS$rule, alpha = 0.5) +
      labs(title = paste0("Rolling ", window, "-day Sharpe ratio, annualized"),
           x = NULL, y = NULL, color = NULL) +
      scale_color_manual(values = c("Portfolio" = AXIS$portfolio,
                                    setNames(AXIS$benchmark, portfolio$benchmark_name))) +
      theme_axis(base_size = 13) +
      theme(legend.position = "bottom")
  })

  # -- Diagnosis: Risk contribution -----------------------------------------

  output$diag_risk_contrib_chart <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$risk_contrib)
    rc <- portfolio$risk_contrib

    ggplot(rc, aes(x = reorder(ticker, pct_contribution), y = pct_contribution,
                   fill = ticker)) +
      geom_col(width = 0.6) +
      coord_flip() +
      labs(title = "Share of portfolio risk", x = NULL, y = NULL) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = holding_colors()) +
      theme_axis(base_size = 13) +
      theme(legend.position = "none")
  })

  # -- Diagnosis: Correlation heatmap ---------------------------------------

  output$diag_corr_heatmap <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$holding_cor)
    cor_matrix <- portfolio$holding_cor
    melted     <- melt(cor_matrix)

    ggplot(melted, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = AXIS$vellum, linewidth = 0.8) +
      geom_text(aes(label = sprintf("%.2f", value)), color = AXIS$ink, size = 3.5) +
      scale_fill_gradient2(low = AXIS$slate, high = AXIS$loss, mid = AXIS$vellum,
                           midpoint = 0, limit = c(-1, 1), name = "Correlation") +
      labs(title = "Correlation of daily returns") +
      theme_axis(base_size = 12) +
      theme(axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title   = element_blank(),
            panel.grid   = element_blank()) +
      coord_fixed()
  })

  # -- Diagnosis: Sector concentration chart --------------------------------

  output$diag_sector_chart <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$sector_weights)
    sw <- portfolio$sector_weights %>%
      mutate(pct = weight / sum(weight))

    eq_weight <- 1 / nrow(sw)

    ggplot(sw, aes(x = reorder(sector, pct), y = pct, fill = sector)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = eq_weight, linetype = "dashed",
                 color = AXIS$rule, alpha = 0.7) +
      coord_flip() +
      labs(title = "Sector weights", x = NULL, y = NULL,
           caption = "Dashed line = equal sector weight") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = sector_colors()) +
      theme_axis(base_size = 13) +
      theme(legend.position = "none")
  })

  # -- Diagnosis: Detailed metrics table ------------------------------------

  output$diag_metrics_table <- renderTable({
    req(portfolio$analyzed, portfolio$metrics)
    build_diagnosis_metrics_tibble(portfolio$metrics)
  }, striped = TRUE, hover = TRUE, align = "lr", width = "100%")

  output$diag_sector_table <- renderTable({
    req(portfolio$analyzed, portfolio$sector_weights)
    sw <- portfolio$sector_weights
    tot <- sum(sw$weight)
    sw %>%
      mutate(`Weight (%)` = sprintf("%.1f%%", weight / tot * 100)) %>%
      select(Sector = sector, `Weight (%)`)
  }, striped = TRUE, align = "lr", width = "100%")

  output$diag_return_attrib_table <- renderTable({
    req(portfolio$analyzed, portfolio$return_attrib)
    portfolio$return_attrib %>%
      transmute(
        Ticker = ticker,
        `Weight (%)` = sprintf("%.1f%%", weight * 100),
        `Period return` = sprintf("%.1f%%", period_total_return * 100),
        `Linear share` = ifelse(is.na(linear_share), "N/A",
          sprintf("%.0f%%", linear_share * 100))
      )
  }, striped = TRUE, align = "lrrr", width = "100%")

  output$diag_return_attrib_chart <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$return_attrib)
    ra <- portfolio$return_attrib %>% filter(!is.na(linear_share))
    req(nrow(ra) > 0)
    ggplot(ra, aes(x = reorder(ticker, linear_share), y = linear_share, fill = ticker)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = 0, color = AXIS$rule, linewidth = 0.3) +
      coord_flip() +
      labs(title = "Share of summed daily returns", x = NULL, y = NULL, fill = "") +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = holding_colors()) +
      theme_axis(base_size = 13) +
      theme(legend.position = "none")
  })

  output$diag_sector_return_table <- renderTable({
    if (!portfolio$analyzed) return(NULL)
    sr <- portfolio$sector_return_attrib
    if (is.null(sr) || nrow(sr) == 0) {
      return(data.frame(Note = "No sector attribution for this run.", check.names = FALSE))
    }
    tot_w <- sum(sr$weight_in_portfolio)
    sr %>%
      transmute(
        Sector = sector,
        `Weight (%)` = sprintf("%.1f%%", weight_in_portfolio / tot_w * 100),
        `Linear share` = ifelse(is.na(linear_share_sector), "N/A",
          sprintf("%.0f%%", linear_share_sector * 100)),
        `Weighted period return` = sprintf("%.1f%%", weighted_period_return * 100)
      )
  }, striped = TRUE, align = "lrrr", width = "100%")

  output$diag_sector_return_chart <- renderAxisPlot({
    if (!portfolio$analyzed) return(invisible(NULL))
    sr <- portfolio$sector_return_attrib
    if (is.null(sr) || nrow(sr) == 0) {
      return(plot_message("No sector attribution for this run."))
    }
    if (all(is.na(sr$linear_share_sector))) {
      plot_df <- sr %>%
        mutate(
          plot_y = weight_in_portfolio / sum(weight_in_portfolio),
          metric = "Portfolio weight share"
        )
      sub <- "Portfolio weight share (linear share unavailable)"
    } else {
      plot_df <- sr %>%
        mutate(
          plot_y = linear_share_sector,
          metric = "Linear return share"
        )
      sub <- "Sum of holding linear shares within each sector"
    }
    ggplot(plot_df, aes(x = reorder(sector, plot_y), y = plot_y, fill = sector)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = 0, color = AXIS$rule, linewidth = 0.3) +
      coord_flip() +
      labs(title = "Sector attribution", subtitle = sub, x = NULL, y = NULL) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_manual(values = sector_colors()) +
      theme_axis(base_size = 13) +
      theme(legend.position = "none")
  })

  portfolio_as_diagnosis_list <- reactive({
    req(portfolio$analyzed)
    list(
      metrics              = portfolio$metrics,
      sector_weights       = portfolio$sector_weights,
      risk_contrib         = portfolio$risk_contrib,
      diversification      = portfolio$diversification,
      holding_cor          = portfolio$holding_cor,
      return_attrib        = portfolio$return_attrib,
      sector_return_attrib = portfolio$sector_return_attrib,
      holdings             = portfolio$holdings,
      benchmark_name       = portfolio$benchmark_name
    )
  })

  output$download_diagnosis <- downloadHandler(
    filename = function() {
      sprintf("portfolio-diagnosis-%s.txt", Sys.Date())
    },
    content = function(file) {
      writeLines(diagnosis_export_lines(portfolio_as_diagnosis_list()), file)
    }
  )

  output$download_diagnosis_bundle <- downloadHandler(
    filename = function() sprintf("portfolio-full-report-%s.zip", Sys.Date()),
    content = function(file) {
      req(portfolio$analyzed)
      d <- tempfile("pil-report")
      dir.create(d)
      on.exit(unlink(d, recursive = TRUE), add = TRUE)

      p <- portfolio_as_diagnosis_list()
      writeLines(diagnosis_export_lines(p), file.path(d, "diagnosis.txt"))
      utils::write.csv(
        build_diagnosis_metrics_tibble(portfolio$metrics),
        file.path(d, "metrics.csv"),
        row.names = FALSE
      )
      ra <- portfolio$return_attrib
      if (!is.null(ra) && nrow(ra) > 0) {
        utils::write.csv(ra, file.path(d, "holding_attribution.csv"), row.names = FALSE)
      }
      sr <- portfolio$sector_return_attrib
      if (!is.null(sr) && nrow(sr) > 0) {
        utils::write.csv(sr, file.path(d, "sector_attribution.csv"), row.names = FALSE)
      }
      snap <- build_portfolio_snapshot_list(
        portfolio$holdings,
        input$benchmark_ticker,
        input$portfolio_date_range[1],
        input$portfolio_date_range[2],
        label = "full-report-bundle",
        metrics = metrics_summary_for_json(portfolio$metrics)
      )
      writeLines(
        jsonlite::toJSON(snap, pretty = TRUE, auto_unbox = TRUE),
        file.path(d, "portfolio_snapshot.json"),
        useBytes = TRUE
      )

      owd <- setwd(d)
      on.exit(setwd(owd), add = TRUE)
      fl <- list.files()
      if (!length(fl)) {
        stop("Report bundle: no files to zip.", call. = FALSE)
      }
      utils::zip(zipfile = file, files = fl)
      if (!file.exists(file)) {
        stop("Report bundle: zip failed (is 'zip' installed on this system?).", call. = FALSE)
      }
    }
  )

  output$download_metrics_csv <- downloadHandler(
    filename = function() sprintf("portfolio-metrics-%s.csv", Sys.Date()),
    content = function(file) {
      req(portfolio$analyzed, portfolio$metrics)
      utils::write.csv(
        build_diagnosis_metrics_tibble(portfolio$metrics),
        file,
        row.names = FALSE
      )
    }
  )

  output$download_holding_attrib_csv <- downloadHandler(
    filename = function() sprintf("holding-attribution-%s.csv", Sys.Date()),
    content = function(file) {
      req(portfolio$analyzed)
      ra <- portfolio$return_attrib
      if (is.null(ra) || nrow(ra) == 0) {
        utils::write.csv(
          data.frame(note = "No holding attribution for this run."),
          file,
          row.names = FALSE
        )
      } else {
        utils::write.csv(ra, file, row.names = FALSE)
      }
    }
  )

  output$download_sector_attrib_csv <- downloadHandler(
    filename = function() sprintf("sector-attribution-%s.csv", Sys.Date()),
    content = function(file) {
      req(portfolio$analyzed)
      sr <- portfolio$sector_return_attrib
      if (is.null(sr) || nrow(sr) == 0) {
        utils::write.csv(
          data.frame(note = "No sector attribution for this run."),
          file,
          row.names = FALSE
        )
      } else {
        utils::write.csv(sr, file, row.names = FALSE)
      }
    }
  )

  observeEvent(input$diagnosis_pin_save_a, {
    req(portfolio$analyzed)
    diagnosis_pins$a <- build_diagnosis_pin(
      input$diagnosis_pin_a_label,
      portfolio$metrics,
      portfolio$holdings,
      portfolio$benchmark_name
    )
    showNotification("Saved diagnosis pin A.", type = "message")
  })

  observeEvent(input$diagnosis_pin_save_b, {
    req(portfolio$analyzed)
    diagnosis_pins$b <- build_diagnosis_pin(
      input$diagnosis_pin_b_label,
      portfolio$metrics,
      portfolio$holdings,
      portfolio$benchmark_name
    )
    showNotification("Saved diagnosis pin B.", type = "message")
  })

  observeEvent(input$diagnosis_pins_clear, {
    diagnosis_pins$a <- NULL
    diagnosis_pins$b <- NULL
    showNotification("Cleared diagnosis pins.", type = "message")
  })

  output$diagnosis_compare_table <- renderTable({
    req(portfolio$analyzed)
    compare_diagnosis_pins_table(
      build_diagnosis_metrics_tibble(portfolio$metrics),
      diagnosis_pins$a,
      diagnosis_pins$b
    )
  }, striped = TRUE, align = "l", width = "100%")

  # ══════════════════════════════════════════════════════════════════════════
  # DIAGNOSIS — excess return and period summary vs benchmark
  # ══════════════════════════════════════════════════════════════════════════

  output$perf_port_excess <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$returns)
    if (is.null(portfolio$benchmark_returns)) {
      return(plot_message("No benchmark data for this window,\nso excess return is unavailable."))
    }
    p_df <- portfolio$returns %>% select(date, r_p = portfolio_return)
    b_df <- portfolio$benchmark_returns %>% select(date, r_b = portfolio_return)
    joined <- inner_join(p_df, b_df, by = "date") %>%
      arrange(date) %>%
      mutate(
        excess        = r_p - r_b,
        cum_excess    = cumprod(1 + excess) - 1
      )
    req(nrow(joined) > 2)
    ggplot(joined, aes(x = date, y = cum_excess)) +
      geom_hline(yintercept = 0, linetype = "dotted", color = AXIS$rule) +
      geom_line(linewidth = 0.75, color = AXIS$brass) +
      labs(
        title = paste0("Cumulative excess return vs ", portfolio$benchmark_name),
        x = NULL,
        y = NULL
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_axis(base_size = 13)
  })

  output$perf_port_summary <- renderTable({
    req(portfolio$analyzed, portfolio$metrics, portfolio$returns)
    m <- portfolio$metrics
    ptr <- portfolio$returns
    port_tr <- dplyr::last(ptr$cumulative_return)
    bench_tr <- NA_real_
    bench_vol <- NA_character_
    bench_sharpe <- NA_character_
    bench_dd <- NA_character_
    if (!is.null(portfolio$benchmark_returns)) {
      btr <- portfolio$benchmark_returns
      bench_tr <- dplyr::last(btr$cumulative_return)
      br <- btr$portfolio_return
      bench_vol <- sprintf("%.1f%%", ann_vol(br) * 100)
      bs <- sharpe_ratio(br)
      bench_sharpe <- if (is.finite(bs)) sprintf("%.2f", bs) else "N/A"
      bench_dd <- sprintf("%.1f%%", max_drawdown(br) * 100)
    }
    has_bench <- !is.null(portfolio$benchmark_returns)
    tibble(
      Series = c("Portfolio", portfolio$benchmark_name),
      `Period total return` = c(
        sprintf("%.1f%%", port_tr * 100),
        if (!has_bench || is.na(bench_tr)) "N/A" else sprintf("%.1f%%", bench_tr * 100)
      ),
      `Ann. volatility` = c(
        sprintf("%.1f%%", m$annual_volatility * 100),
        if (has_bench) bench_vol else "N/A"
      ),
      `Sharpe` = c(
        sprintf("%.2f", m$sharpe_ratio),
        if (has_bench) bench_sharpe else "N/A"
      ),
      `Max drawdown` = c(
        sprintf("%.1f%%", m$max_drawdown * 100),
        if (has_bench) bench_dd else "N/A"
      ),
      `Beta` = c(
        if (!is.null(m$beta) && !is.na(m$beta)) sprintf("%.2f", m$beta) else "N/A",
        if (has_bench) "1.00" else "N/A"
      ),
      `Excess return (ann.)` = c(
        if (!is.null(m$excess_return)) sprintf("%.1f%%", m$excess_return * 100) else "N/A",
        if (has_bench) "—" else "N/A"
      ),
      `Tracking error (ann.)` = c(
        if (!is.null(m$tracking_error)) sprintf("%.1f%%", m$tracking_error * 100) else "N/A",
        if (has_bench) "—" else "N/A"
      )
    )
  }, align = "c", width = "100%")

  # ══════════════════════════════════════════════════════════════════════════
  # SCENARIOS TAB
  # ══════════════════════════════════════════════════════════════════════════

  observeEvent(input$stress_preset, {
    req(input$stress_preset)
    if (input$stress_preset != "custom") {
      p <- stress_presets[[input$stress_preset]]
      if (!is.null(p)) {
        updateDateRangeInput(session, "stress_custom_range",
          start = p$start, end = p$end)
      }
    }
  }, ignoreInit = TRUE)

  observe({
    hp <- portfolio$holdings$ticker
    if (length(hp) == 0) return()
    secs <- names(stock_sectors)[vapply(names(stock_sectors), function(s) {
      any(hp %in% stock_sectors[[s]])
    }, logical(1))]
    if (length(secs) == 0) return()
    cur <- input$stress_sector_pick
    sel <- if (!is.null(cur) && cur %in% secs) cur else secs[[1]]
    updateSelectInput(session, "stress_sector_pick", choices = secs, selected = sel)
  })

  stress_window_dates <- reactive({
    req(input$stress_custom_range)
    list(start = input$stress_custom_range[1], end = input$stress_custom_range[2])
  })

  output$stress_hist_plot <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$returns)
    dates <- stress_window_dates()
    ps <- slice_returns_by_date(portfolio$returns, dates$start, dates$end)
    validate(need(!is.null(ps) && nrow(ps) >= 2,
      "No overlapping portfolio data in this date range. Widen the range or re-analyze."))

    p_re <- rebase_cumulative_from_slice(ps)
    if (is.null(p_re) || nrow(p_re) < 2) {
      return(plot_message("Insufficient data in window."))
    }

    if (!is.null(portfolio$benchmark_returns)) {
      bs <- slice_returns_by_date(portfolio$benchmark_returns, dates$start, dates$end)
      if (!is.null(bs) && nrow(bs) >= 2) {
        b_re <- rebase_cumulative_from_slice(bs)
        if (!is.null(b_re) && nrow(b_re) >= 2) {
          combined <- bind_rows(
            p_re %>% transmute(date, cumulative_return, series = "Portfolio"),
            b_re %>% transmute(date, cumulative_return, series = portfolio$benchmark_name)
          )
          return(
            ggplot(combined, aes(x = date, y = cumulative_return, color = series)) +
              geom_line(linewidth = 0.75) +
              geom_hline(yintercept = 0, linetype = "dotted", color = AXIS$rule) +
              labs(title = "Cumulative return in window (rebased)", x = NULL, y = "", color = "") +
              scale_y_continuous(labels = scales::percent_format()) +
              scale_color_manual(values = c(
                "Portfolio" = AXIS$portfolio,
                setNames(AXIS$benchmark, portfolio$benchmark_name)
              )) +
              theme_axis(base_size = 13) +
              theme(legend.position = "bottom")
          )
        }
      }
    }

    ggplot(p_re, aes(x = date, y = cumulative_return)) +
      geom_line(linewidth = 0.75, color = AXIS$portfolio) +
      geom_hline(yintercept = 0, linetype = "dotted", color = AXIS$rule) +
      labs(title = "Cumulative return in window (rebased, portfolio only)",
           x = NULL, y = "Cumulative return") +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_axis(base_size = 13)
  })

  output$stress_hist_table <- renderTable({
    req(portfolio$analyzed, portfolio$returns)
    dates <- stress_window_dates()
    ps <- slice_returns_by_date(portfolio$returns, dates$start, dates$end)
    validate(need(!is.null(ps) && nrow(ps) >= 2, "Adjust the date range."))

    sm_p <- summarize_return_slice(ps)
    sm_b <- NULL
    if (!is.null(portfolio$benchmark_returns)) {
      bs <- slice_returns_by_date(portfolio$benchmark_returns, dates$start, dates$end)
      if (!is.null(bs) && nrow(bs) >= 2) sm_b <- summarize_return_slice(bs)
    }

    fmt_pct <- function(x) if (is.null(x)) "N/A" else sprintf("%.2f%%", x * 100)
    fmt_pct2 <- function(x) if (is.null(x)) "N/A" else sprintf("%.2f%%", x * 100)

    tibble(
      Metric = c(
        "Trading days", "Total return", "Ann. volatility", "Max drawdown",
        "Worst day", "Best day"
      ),
      Portfolio = c(
        as.character(sm_p$n_days),
        fmt_pct(sm_p$total_return),
        fmt_pct2(sm_p$ann_vol),
        fmt_pct(sm_p$max_dd),
        fmt_pct2(sm_p$worst_day),
        fmt_pct2(sm_p$best_day)
      ),
      Benchmark = c(
        if (!is.null(sm_b)) as.character(sm_b$n_days) else "N/A",
        if (!is.null(sm_b)) fmt_pct(sm_b$total_return) else "N/A",
        if (!is.null(sm_b)) fmt_pct2(sm_b$ann_vol) else "N/A",
        if (!is.null(sm_b)) fmt_pct(sm_b$max_dd) else "N/A",
        if (!is.null(sm_b)) fmt_pct2(sm_b$worst_day) else "N/A",
        if (!is.null(sm_b)) fmt_pct2(sm_b$best_day) else "N/A"
      )
    )
  }, striped = TRUE, align = "lrr", width = "100%")

  output$stress_shock_plot <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$price_data, portfolio$weights_frac)
    rw <- returns_wide_from_price_data(portfolio$price_data)
    w  <- portfolio$weights_frac
    base <- portfolio_returns_from_wide(rw, w)
    validate(need(!is.null(base) && nrow(base) >= 3, "Could not build return series for stress."))

    bps <- if (is.null(input$stress_bps)) 0 else input$stress_bps
    tickers_shock <- names(w)
    if (!is.null(input$stress_scope) && input$stress_scope == "sector" &&
          !is.null(input$stress_sector_pick) && nzchar(input$stress_sector_pick)) {
      tickers_shock <- holdings_tickers_in_sector(names(w), input$stress_sector_pick)
      if (length(tickers_shock) == 0) tickers_shock <- names(w)
    }

    rw2 <- if (bps == 0) {
      rw
    } else {
      apply_daily_return_shock_wide(rw, tickers_shock, bps / 10000)
    }
    shocked <- portfolio_returns_from_wide(rw2, w)
    validate(need(!is.null(shocked) && nrow(shocked) >= 3, "Stressed path could not be computed."))

    aligned <- inner_join(
      base %>% select(date, baseline = cumulative_return),
      shocked %>% select(date, stressed = cumulative_return),
      by = "date"
    )
    validate(need(nrow(aligned) >= 3, "No overlapping dates between baseline and stressed series."))

    long_df <- aligned %>%
      pivot_longer(c(baseline, stressed), names_to = "series", values_to = "cumulative_return") %>%
      mutate(series = ifelse(series == "baseline", "Baseline", "Stressed"))

    sub <- if (bps == 0) {
      "No daily bps shock (paths should coincide)."
    } else {
      paste0(
        bps, " bps/day to ",
        if (!is.null(input$stress_scope) && input$stress_scope == "sector" &&
              !is.null(input$stress_sector_pick)) input$stress_sector_pick else "all holdings"
      )
    }

    ggplot(long_df, aes(x = date, y = cumulative_return, color = series)) +
      geom_line(linewidth = 0.75) +
      geom_hline(yintercept = 0, linetype = "dotted", color = AXIS$rule) +
      labs(
        title = "Full-sample cumulative return: baseline vs stressed",
        subtitle = sub,
        x = NULL, y = "Cumulative return", color = ""
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_color_manual(values = c("Baseline" = AXIS$stone, "Stressed" = AXIS$brass)) +
      theme_axis(base_size = 13) +
      theme(legend.position = "bottom")
  })

  output$stress_fan_plot <- renderAxisPlot({
    req(portfolio$analyzed, portfolio$returns)
    h <- input$stress_fan_horizon
    ns <- input$stress_fan_n_sims
    sd <- input$stress_fan_seed
    fan <- bootstrap_fan_from_returns(
      portfolio$returns$portfolio_return, h, ns, sd)
    validate(need(!is.null(fan), "Need a longer return history for the bootstrap fan (at least ~10 days)."))

    sm <- fan$sample_matrix
    df_line <- bind_rows(lapply(seq_len(nrow(sm)), function(i) {
      tibble(day = fan$days, cum = sm[i, ], sim = i)
    }))
    ribbon <- tibble(
      day = fan$days,
      ymin = fan$p05,
      ymax = fan$p95,
      mid = fan$p50
    )

    ggplot() +
      geom_line(
        data = df_line, aes(x = day, y = cum, group = sim),
        alpha = 0.07, color = AXIS$stone, linewidth = 0.35
      ) +
      geom_ribbon(
        data = ribbon, aes(x = day, ymin = ymin, ymax = ymax),
        fill = AXIS$brass, alpha = 0.18
      ) +
      geom_line(
        data = ribbon, aes(x = day, y = mid),
        color = AXIS$brass_deep, linewidth = 1.05
      ) +
      labs(
        title = "Bootstrap cumulative return fan (portfolio, fixed weights)",
        subtitle = paste("Median path (teal); 5–95% band;", ns, "simulations ×", h, "days ahead"),
        x = "Trading days ahead",
        y = "Cumulative return"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_axis(base_size = 13)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # ALLOCATION LAB
  # ══════════════════════════════════════════════════════════════════════════

  output$alloc_lab_need_analyze <- renderUI({
    if (isTRUE(portfolio$analyzed)) return(NULL)
    empty_state(
      "No weights to compare yet.",
      "Analyze a portfolio to compare your weights with min-variance, inverse-vol, equal-weight, and max-Sharpe models.",
      cta_id = "empty_sample_alloc"
    )
  })

  output$stress_need_analyze <- renderUI({
    if (isTRUE(portfolio$analyzed)) return(NULL)
    empty_state(
      "No portfolio to stress yet.",
      "Analyze a portfolio to replay historical episodes, apply a daily shock, and simulate forward paths.",
      cta_id = "empty_sample_stress"
    )
  })

  alloc_lab_data <- reactive({
    req(isTRUE(portfolio$analyzed))
    req(!is.null(portfolio$price_data), !is.null(portfolio$weights_frac))
    ms <- compute_mu_sigma_annual(portfolio$price_data, names(portfolio$weights_frac))
    validate(need(
      !is.null(ms),
      "Not enough overlapping daily returns to estimate mean and covariance. Widen the analysis window."
    ))
    tick <- colnames(ms$Sigma)
    w0 <- align_weights(portfolio$weights_frac, tick)
    validate(need(sum(w0) > 1e-12 && length(w0) >= 1L, "Could not align weights to the return sample."))

    method <- input$alloc_method
    mw <- if (!is.null(input$alloc_max_w_pct)) input$alloc_max_w_pct / 100 else 1
    n <- length(w0)
    if (mw < 1 / n - 1e-9) {
      validate(need(
        FALSE,
        paste0(
          "Max weight must be at least ",
          sprintf("%.1f%%", 100 / n),
          " so ", n, " positive weights can sum to 100%."
        )
      ))
    }

    w1 <- propose_allocation(method, ms, max_w = mw)
    fallback <- attr(w1, "fallback")
    w1 <- w1[tick]
    w1[is.na(w1)] <- 0
    if (sum(w1) < 1e-12) {
      w1 <- rep(1 / length(tick), length(tick))
    } else {
      w1 <- w1 / sum(w1)
    }
    names(w1) <- tick

    list(
      ms = ms,
      w0 = w0,
      w1 = w1,
      fallback = fallback,
      m0 = portfolio_ann_metrics(w0, ms$mu, ms$Sigma),
      m1 = portfolio_ann_metrics(w1, ms$mu, ms$Sigma)
    )
  })

  output$alloc_tradeoff <- renderUI({
    req(isTRUE(portfolio$analyzed))
    d <- alloc_lab_data()
    card <- function(label, a, b, fmt, unit, better) {
      delta <- b - a
      good  <- if (is.na(delta) || abs(delta) < 1e-9) NA else if (better == "up") delta > 0 else delta < 0
      cls   <- if (is.na(good)) "delta-flat" else if (good) "delta-good" else "delta-bad"
      tags$div(class = "tradeoff-card",
        tags$div(class = "kpi-label", label),
        tags$div(class = "tradeoff-values",
          tags$span(class = "tradeoff-from", fmt(a)),
          tags$span(class = "tradeoff-arrow", "\u2192"),
          tags$span(class = "tradeoff-to", fmt(b))
        ),
        tags$div(class = paste("tradeoff-delta", cls),
          if (is.na(delta)) "\u2014" else if (unit == "pp") sprintf("%+.1f pp", 100 * delta) else sprintf("%+.2f", delta))
      )
    }
    f_pct <- function(x) sprintf("%.1f%%", 100 * x)
    f_sh  <- function(x) if (is.na(x)) "\u2014" else sprintf("%.2f", x)
    tags$div(class = "tradeoff-grid",
      card("Annual return", d$m0$ann_return, d$m1$ann_return, f_pct, "pp", "up"),
      card("Volatility",    d$m0$ann_vol,    d$m1$ann_vol,    f_pct, "pp", "down"),
      card("Sharpe (RF = 0)", d$m0$sharpe,   d$m1$sharpe,     f_sh,  "",   "up")
    )
  })

  output$alloc_weight_pairs <- renderUI({
    req(isTRUE(portfolio$analyzed))
    d <- alloc_lab_data()
    tick  <- names(d$w0)
    w0    <- d$w0[tick]
    w1    <- d$w1[tick]
    ord   <- order(-pmax(w0, w1))
    scale <- max(c(w0, w1))
    rows <- lapply(ord, function(i) {
      dl <- 100 * (w1[i] - w0[i])
      tags$li(class = "pair-row",
        tags$span(class = "pair-ticker", tick[i]),
        tags$span(class = "pair-bars",
          tags$span(class = "pair-bar pair-bar--current",
            tags$span(style = sprintf("width:%.1f%%", 100 * w0[i] / scale))),
          tags$span(class = "pair-bar pair-bar--proposed",
            tags$span(style = sprintf("width:%.1f%%", 100 * w1[i] / scale)))
        ),
        tags$span(class = "pair-values",
          tags$span(class = "pair-current", sprintf("%.1f%%", 100 * w0[i])),
          tags$span(class = "pair-proposed", sprintf("%.1f%%", 100 * w1[i]))
        ),
        tags$span(class = paste("pair-delta",
            if (abs(dl) < 0.05) "delta-flat" else if (dl > 0) "delta-up" else "delta-down"),
          if (abs(dl) < 0.05) "\u00b10.0" else sprintf("%+.1f", dl))
      )
    })
    tagList(
      tags$div(class = "pair-legend",
        tags$span(class = "pair-key pair-key--current", "Current"),
        tags$span(class = "pair-key pair-key--proposed", "Proposed"),
        tags$span(class = "pair-key-note", "Change in percentage points")
      ),
      tags$ul(class = "pair-list", rows),
      if (!is.null(d$fallback)) {
        tags$p(class = "page-footnote", tags$b("Fallback used: "), d$fallback, ".")
      }
    )
  })

  # ══════════════════════════════════════════════════════════════════════════
  # EXPLORE — single names (prices & returns, forecast, risk)
  # ══════════════════════════════════════════════════════════════════════════

  # -- Sector filter observers (Explore) -----------------------------------

  observe({
    selected_sector <- input$sector_filter
    if (selected_sector == "all") {
      updateSelectInput(session, "tickers",
        choices  = all_stocks,
        selected = input$tickers)
    } else {
      sector_stocks <- stock_sectors[[selected_sector]]
      updateSelectInput(session, "tickers",
        choices  = sector_stocks,
        selected = sector_stocks[1:min(2, length(sector_stocks))])
    }
  })

  observe({
    selected_sector <- input$forecast_sector
    if (selected_sector == "all") {
      updateSelectInput(session, "forecast_ticker", choices = all_stocks)
    } else {
      updateSelectInput(session, "forecast_ticker",
        choices = stock_sectors[[selected_sector]])
    }
  })

  observe({
    selected_sector <- input$risk_sector
    if (selected_sector == "all") {
      updateSelectInput(session, "risk_tickers",
        choices  = all_stocks,
        selected = input$risk_tickers)
    } else {
      sector_stocks <- stock_sectors[[selected_sector]]
      updateSelectInput(session, "risk_tickers",
        choices  = sector_stocks,
        selected = sector_stocks[1:min(2, length(sector_stocks))])
    }
  })

  observeEvent(input$select_all, {
    sector <- input$sector_filter
    choices <- if (sector == "all") all_stocks else stock_sectors[[sector]]
    updateSelectInput(session, "tickers", selected = choices)
  })

  observeEvent(input$clear_all, {
    updateSelectInput(session, "tickers", selected = character(0))
  })

  # -- Stock data reactive (Explore → Prices & returns) --------------------

  stock_data <- reactive({
    if (is.null(input$tickers) || length(input$tickers) == 0) return(NULL)

    withProgress(message = "Loading stock data...", value = 0, {
      data_list <- lapply(seq_along(input$tickers), function(i) {
        ticker <- input$tickers[i]
        incProgress(1 / length(input$tickers), detail = paste("Loading", ticker))
        tryCatch({
          df <- getSymbols(ticker, src = "yahoo",
                           from = Sys.Date() - 365,
                           to   = Sys.Date(),
                           auto.assign = FALSE)
          if (is.null(df) || nrow(df) == 0) return(NULL)
          df <- fortify.zoo(df) %>% as_tibble()
          colnames(df)[1] <- "date"
          if (ncol(df) >= 6) {
            names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
          }
          df$ticker <- ticker
          if (sum(!is.na(df$adjusted)) < 10) return(NULL)
          df
        }, error = function(e) NULL)
      })
      data_list <- data_list[!sapply(data_list, is.null)]
      if (length(data_list) == 0) return(NULL)
      bind_rows(data_list)
    })
  })

  # -- Price Plot -----------------------------------------------------------

  output$pricePlot <- renderAxisPlot({
    df <- stock_data()
    if (is.null(df) || nrow(df) == 0) {
      return(plot_message("Select at least one ticker."))
    }
    validate_data(df, "stock price data")
    ma_window <- input$ma_window

    df <- df %>%
      arrange(date) %>%
      group_by(ticker) %>%
      mutate(ma = if (n() >= ma_window)
        rollmean(adjusted, k = ma_window, fill = NA, align = "right") else NA) %>%
      ungroup()

    insufficient <- df %>% group_by(ticker) %>%
      summarise(has_ma = any(!is.na(ma))) %>% filter(!has_ma)
    if (nrow(insufficient) > 0)
      showNotification(paste("Insufficient data for", ma_window, "day MA for:",
        paste(insufficient$ticker, collapse = ", ")), type = "warning")

    ggplot(df, aes(x = date, y = adjusted, color = ticker)) +
      geom_line(alpha = 0.7, linewidth = 0.75) +
      geom_line(aes(y = ma), linetype = "dashed", linewidth = 0.8, alpha = 0.8) +
      labs(title = paste0("Adjusted price with ", ma_window, "-day moving average"),
           x = NULL, y = "Adjusted price", color = NULL) +
      scale_y_continuous(labels = scales::dollar_format()) +
      theme_axis(base_size = 12) +
      theme(legend.position = "bottom")
  })

  # -- Daily Returns --------------------------------------------------------

  output$returnsPlot <- renderAxisPlot({
    df <- stock_data()
    validate_data(df, "returns data")
    df <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      ungroup()
    if (sum(!is.na(df$daily_return)) == 0) validate("No return data available")

    ggplot(df, aes(x = date, y = daily_return, color = ticker)) +
      geom_line(alpha = 0.6) +
      geom_hline(yintercept = 0, linetype = "dotted", color = AXIS$rule) +
      labs(title = "Daily returns", x = NULL, y = "Daily return",
           color = NULL) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
      theme_axis(base_size = 12) +
      theme(legend.position = "bottom")
  })

  # -- Return Distribution --------------------------------------------------

  output$returnDistribution <- renderAxisPlot({
    df <- stock_data()
    validate_data(df, "returns data")
    df <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      ungroup()

    ggplot(df, aes(x = daily_return, fill = ticker)) +
      geom_histogram(alpha = 0.6, bins = 50, position = "identity") +
      facet_wrap(~ticker, scales = "free_y") +
      labs(title = "Distribution of daily returns",
           x = "Daily return", y = "Frequency", fill = NULL) +
      scale_x_continuous(labels = scales::percent_format()) +
      theme_axis(base_size = 12)
  })

  # -- Cumulative Returns ---------------------------------------------------

  output$cumulativePlot <- renderAxisPlot({
    df <- stock_data()
    validate_data(df, "cumulative returns data")
    df <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(
        daily_return      = adjusted / dplyr::lag(adjusted) - 1,
        cumulative_return = cumprod(1 + replace_na(daily_return, 0)) - 1
      ) %>%
      filter(!is.na(cumulative_return)) %>%
      ungroup()

    ggplot(df, aes(x = date, y = cumulative_return, color = ticker)) +
      geom_line(linewidth = 0.75, alpha = 0.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = AXIS$rule) +
      labs(title = "Growth of $1",
           x = NULL, y = NULL, color = NULL) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_axis(base_size = 14) +
      theme(legend.position = "bottom")
  })

  # -- Performance Summary --------------------------------------------------

  output$performanceSummary <- renderTable({
    df <- stock_data()
    validate_data(df, "performance data")
    df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      summarise(
        `Total Return`       = sprintf("%.2f%%",  (last(adjusted) / first(adjusted) - 1) * 100),
        `Volatility (Annual)` = sprintf("%.2f%%", ann_vol(daily_return) * 100),
        `Sharpe Ratio`       = sprintf("%.2f",    sharpe_ratio(daily_return)),
        `Best Day`           = sprintf("%.2f%%",  max(daily_return) * 100),
        `Worst Day`          = sprintf("%.2f%%",  min(daily_return) * 100),
        `Current Price`      = sprintf("$%.2f",   last(adjusted))
      ) %>%
      rename(Stock = ticker)
  }, align = "c")

  # -- Forecast data reactive -----------------------------------------------

  forecast_data <- reactive({
    if (is.null(input$forecast_ticker) || input$forecast_ticker == "") {
      showNotification("Please select a ticker for forecasting", type = "error", duration = 5)
      return(tibble())
    }
    withProgress(message = paste("Loading data for", input$forecast_ticker), value = 0.5, {
      tryCatch({
        df <- getSymbols(input$forecast_ticker, src = "yahoo",
                         from = Sys.Date() - 365,
                         to   = Sys.Date(),
                         auto.assign = FALSE)
        if (is.null(df) || nrow(df) == 0) {
          showNotification(paste("No data found for", input$forecast_ticker), type = "error")
          return(tibble())
        }
        df <- fortify.zoo(df) %>% as_tibble()
        colnames(df)[1] <- "date"
        if (ncol(df) >= 6) {
          names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
        }
        df <- df %>% arrange(date)
        if (sum(!is.na(df$adjusted)) < 30)
          showNotification("Insufficient data for forecasting", type = "warning")
        return(df)
      }, error = function(e) {
        showNotification(paste("Failed to load data for forecast:", e$message), type = "error")
        return(tibble())
      })
    })
  })

  output$dataAvailability <- renderText({
    df <- forecast_data()
    if (nrow(df) == 0) return("No data available")
    days_available <- nrow(df)
    min_for_forecast <- if (input$forecast_mode == "conservative") {
      max(50, input$horizon * 2)
    } else {
      max(30, input$horizon * 0.5)
    }
    min_for_backtest <- input$horizon + 60

    if (days_available < min_for_forecast) {
      paste("Insufficient:", days_available, "days available,", min_for_forecast, "needed")
    } else if (days_available < min_for_backtest) {
      paste("Limited:", days_available, "days (can forecast but not backtest)")
    } else {
      paste("Good:", days_available, "days of data available")
    }
  })

  # -- Forecast Plot --------------------------------------------------------

  output$forecastPlot <- renderAxisPlot({
    df <- forecast_data()
    if (nrow(df) == 0) {
      return(plot_message("Select a ticker to forecast."))
    }
    validate_data(df, "forecast data")

    min_required <- if (input$forecast_mode == "conservative") {
      max(50, input$horizon * 2)
    } else {
      max(30, input$horizon * 0.5)
    }

    if (nrow(df) < min_required) {
      showNotification(paste("Insufficient data: Have", nrow(df), "days, need", min_required),
                       type = "warning", duration = 5)
      return(plot_message(paste0(
        "Need at least ", min_required, " days of data for a ", input$horizon,
        "-day forecast.\nCurrent data: ", nrow(df), " days.\n\n",
        "Try aggressive mode or a shorter horizon."
      )))
    }

    withProgress(message = "Generating forecast...", value = 0.5, {
      ts_data <- ts(df$adjusted, frequency = 252)
      tryCatch({
        model <- auto.arima(ts_data)
        forecasted <- forecast(model, h = input$horizon)
        autoplot(forecasted) +
          labs(title = paste0(input$forecast_ticker, " price forecast, next ",
                              input$horizon, " days"),
               x = "Trading days", y = "Adjusted price ($)") +
          theme_axis(base_size = 12)
      }, error = function(e) {
        showNotification(paste("Forecast error:", e$message), type = "error")
        validate("Unable to generate forecast. Please try a different ticker or horizon.")
      })
    })
  })

  # -- Forecast Evaluation --------------------------------------------------

  output$forecastMetrics <- renderPrint({
    df <- forecast_data()
    if (nrow(df) == 0) { cat("No data available for evaluation\n"); return() }
    horizon      <- input$horizon
    min_required <- horizon + 60

    if (nrow(df) <= min_required) {
      cat("Insufficient Data for Backtesting\n")
      cat("================================\n")
      cat("To evaluate forecast accuracy, we need:\n")
      cat("  - Historical data for training\n")
      cat("  - Actual data to compare against forecast\n\n")
      cat("Current data points:", nrow(df), "\n")
      cat("Required data points:", min_required, "\n")
      cat("  (", horizon, "days forecast +", 60, "days minimum training)\n\n")
      cat("Try reducing the forecast horizon or wait for more data.\n")
      return()
    }

    withProgress(message = "Evaluating forecast accuracy...", value = 0.5, {
      df    <- df %>% arrange(date)
      train <- head(df$adjusted, -horizon)
      test  <- tail(df$adjusted, horizon)
      if (length(train) < 30) { cat("Insufficient training data for evaluation\n"); return() }
      ts_train <- ts(train, frequency = 252)

      tryCatch({
        model <- auto.arima(ts_train)
        fc    <- forecast(model, h = horizon)
        rmse  <- sqrt(mean((fc$mean - test)^2, na.rm = TRUE))
        mae   <- mean(abs(fc$mean - test), na.rm = TRUE)
        mape  <- mean(abs((fc$mean - test) / test), na.rm = TRUE) * 100

        cat("Forecast Model: ARIMA",
            paste0("(", paste(model$arma[c(1, 6, 2)], collapse = ","), ")\n"))
        cat("----------------\n")
        cat("  Forecast Horizon:", horizon, "days\n")
        cat("  Training samples:", length(train), "\n")
        cat("  RMSE:", sprintf("$%.2f", rmse), "\n")
        cat("  MAE :", sprintf("$%.2f", mae), "\n")
        cat("  MAPE:", sprintf("%.2f%%", mape), "\n")
      }, error = function(e) {
        cat("Error in forecast evaluation:", e$message, "\n")
      })
    })
  })

  # -- Forecast Accuracy Plot -----------------------------------------------

  output$forecastAccuracyPlot <- renderAxisPlot({
    df <- forecast_data()
    if (nrow(df) == 0) return()
    horizon      <- input$horizon
    min_required <- horizon + 60
    if (nrow(df) <= min_required) {
      return(plot_message(paste0(
        "Need ", min_required - nrow(df), " more days of data\n",
        "to compare forecast against actual."
      )))
    }

    df         <- df %>% arrange(date)
    train      <- head(df$adjusted, -horizon)
    test       <- tail(df$adjusted, horizon)
    test_dates <- tail(df$date, horizon)
    if (length(train) < 30) validate("Insufficient training data for accuracy plot")

    withProgress(message = "Creating accuracy plot...", value = 0.5, {
      ts_train <- ts(train, frequency = 252)
      tryCatch({
        model <- auto.arima(ts_train)
        fc    <- forecast(model, h = horizon)
        forecast_df <- data.frame(
          date     = test_dates,
          actual   = as.numeric(test),
          forecast = as.numeric(fc$mean),
          lower_80 = fc$lower[, 1], upper_80 = fc$upper[, 1],
          lower_95 = fc$lower[, 2], upper_95 = fc$upper[, 2]
        )
        ggplot(forecast_df, aes(x = date)) +
          geom_ribbon(aes(ymin = lower_95, ymax = upper_95),
                      fill = AXIS$slate, alpha = 0.3) +
          geom_ribbon(aes(ymin = lower_80, ymax = upper_80),
                      fill = AXIS$slate, alpha = 0.5) +
          geom_line(aes(y = actual),   color = AXIS$ink,  linewidth = 1.2) +
          geom_line(aes(y = forecast), color = AXIS$brass, linetype = "dashed", linewidth = 1) +
          geom_point(aes(y = actual),   color = AXIS$ink,  size = 2, alpha = 0.5) +
          geom_point(aes(y = forecast), color = AXIS$brass, size = 2, alpha = 0.5) +
          labs(title = paste0(input$forecast_ticker,
                              " forecast vs actual, last ", horizon, " days"),
               x = NULL, y = "Adjusted price ($)",
               caption = "Ink = actual · brass = forecast · shaded = confidence interval") +
          scale_y_continuous(labels = scales::dollar_format()) +
          theme_axis(base_size = 12)
      }, error = function(e) {
        showNotification(paste("Error creating accuracy plot:", e$message), type = "error")
        validate("Unable to create accuracy plot")
      })
    })
  })

  # -- Risk Analysis data reactive ------------------------------------------

  risk_data <- reactive({
    req(input$risk_tickers)
    if (length(input$risk_tickers) == 0) {
      showNotification("Please select at least one ticker for risk analysis", type = "error")
      return(tibble())
    }
    withProgress(message = "Loading risk analysis data...", value = 0, {
      data_list <- lapply(seq_along(input$risk_tickers), function(i) {
        ticker <- input$risk_tickers[i]
        incProgress(1 / length(input$risk_tickers), detail = paste("Loading", ticker))
        tryCatch({
          df <- getSymbols(ticker, src = "yahoo",
                           from = input$risk_date_range[1],
                           to   = input$risk_date_range[2],
                           auto.assign = FALSE)
          if (is.null(df) || nrow(df) == 0) return(NULL)
          df <- fortify.zoo(df) %>% as_tibble()
          colnames(df)[1] <- "date"
          if (ncol(df) >= 6) {
            names(df)[2:7] <- c("open", "high", "low", "close", "volume", "adjusted")
          }
          df$ticker <- ticker
          df
        }, error = function(e) NULL)
      })
    })
    data_list <- data_list[!sapply(data_list, is.null)]
    if (length(data_list) == 0) return(tibble())
    bind_rows(data_list)
  })

  # -- Risk Metrics Table ---------------------------------------------------

  output$riskMetricsTable <- renderTable({
    df <- risk_data()
    validate_data(df, "risk analysis")
    df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      summarise(
        `Annual Return`  = ann_return(daily_return),
        `Volatility`     = ann_vol(daily_return),
        `Sharpe Ratio`   = sharpe_ratio(daily_return),
        `Max Drawdown`   = max_drawdown(daily_return),
        `VaR (95%)`      = quantile(daily_return, 0.05, na.rm = TRUE),
        `Best Day`       = max(daily_return, na.rm = TRUE),
        `Worst Day`      = min(daily_return, na.rm = TRUE),
        `Positive Days`  = sum(daily_return > 0, na.rm = TRUE) / n()
      ) %>%
      mutate(
        across(c(`Annual Return`, `Volatility`, `Max Drawdown`, `VaR (95%)`,
                  `Best Day`, `Worst Day`, `Positive Days`),
               ~ sprintf("%.2f%%", . * 100)),
        `Sharpe Ratio` = sprintf("%.2f", `Sharpe Ratio`)
      ) %>%
      rename(Stock = ticker)
  }, align = "c", striped = TRUE, hover = TRUE)

  # -- Correlation Heatmap --------------------------------------------------

  output$correlationHeatmap <- renderAxisPlot({
    df <- risk_data()
    validate_data(df, "correlation analysis")
    returns_wide <- df %>%
      select(date, ticker, adjusted) %>%
      arrange(date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      select(date, ticker, daily_return) %>%
      pivot_wider(names_from = ticker, values_from = daily_return)

    if (ncol(returns_wide) < 3) {
      return(plot_message("Select at least 2 stocks for correlation analysis."))
    }

    cor_matrix <- cor(returns_wide[, -1], use = "complete.obs")
    melted_cor <- melt(cor_matrix)

    ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(color = AXIS$vellum, linewidth = 0.8) +
      geom_text(aes(label = sprintf("%.2f", value)), color = AXIS$ink, size = 4) +
      scale_fill_gradient2(low = AXIS$slate, high = AXIS$loss, mid = AXIS$vellum,
                           midpoint = 0, limit = c(-1, 1), space = "Lab",
                           name = "Correlation") +
      theme_axis(base_size = 12) +
      theme(axis.text.x  = element_text(angle = 45, vjust = 1, hjust = 1),
            axis.title   = element_blank(),
            panel.grid   = element_blank()) +
      labs(title = "Correlation of daily returns") +
      coord_fixed()
  })

  # -- Risk-Return Plot -----------------------------------------------------

  output$riskReturnPlot <- renderAxisPlot({
    df <- risk_data()
    validate_data(df, "risk-return analysis")
    risk_return <- df %>%
      arrange(ticker, date) %>%
      group_by(ticker) %>%
      mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
      filter(!is.na(daily_return)) %>%
      summarise(
        annual_return = ann_return(daily_return),
        volatility    = ann_vol(daily_return)
      )

    ggplot(risk_return, aes(x = volatility, y = annual_return)) +
      geom_point(aes(color = ticker), size = 5, alpha = 0.7) +
      geom_text(aes(label = ticker), vjust = -1.5, size = 4) +
      geom_hline(yintercept = 0, linetype = "dashed", color = AXIS$rule, alpha = 0.5) +
      labs(title = "Risk vs return",
           x = "Annual volatility", y = "Annual return", color = NULL) +
      scale_x_continuous(labels = scales::percent_format()) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_axis(base_size = 12) +
      theme(legend.position = "none") +
      annotate("text", x = max(risk_return$volatility) * 0.9,
               y = max(risk_return$annual_return) * 0.9,
               label = "Higher return,\nHigher risk",
               size = 3, color = AXIS$muted, hjust = 1) +
      annotate("text", x = min(risk_return$volatility) * 1.1,
               y = min(risk_return$annual_return) * 1.1,
               label = "Lower return,\nLower risk",
               size = 3, color = AXIS$muted, hjust = 0)
  })
}
