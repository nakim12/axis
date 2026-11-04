# "Does it hold up?": the walk-forward backtest section of the Allocation page.

bt_reference <- local({
  f <- file.path("data", "eval", "backtest_results.rds")
  if (!file.exists(f)) return(NULL)
  res <- readRDS(f)
  list(
    params = res$params,
    cases  = lapply(res$cases[c("asset_class", "sector")], function(cs) {
      cs[c("label", "universe", "benchmark", "period", "summary", "boot",
           "in_sample_max_sharpe", "walk_forward_max_sharpe_gross")]
    })
  )
})

bt_or <- function(a, b) if (is.null(a)) b else a

bt_fmt_num <- function(x, d = 2) ifelse(is.finite(x), formatC(x, format = "f", digits = d), "n/a")
bt_fmt_pct <- function(x, d = 1) ifelse(is.finite(x), paste0(formatC(100 * x, format = "f", digits = d), "%"), "n/a")
bt_fmt_ci  <- function(lo, hi) paste0("[", bt_fmt_num(lo), ", ", bt_fmt_num(hi), "]")
bt_fmt_diff <- function(d, lo, hi) paste(sprintf("%+.2f", d), bt_fmt_ci(lo, hi))

backtest_rail_ui <- function() {
  rail_group("Backtest",
    selectInput("bt_years", "History",
      choices = c("5 years" = 5, "10 years" = 10, "15 years" = 15, "20 years" = 20),
      selected = 10),
    selectInput("bt_lookback", "Estimation window",
      choices = c("6 months" = 126, "1 year" = 252, "2 years" = 504),
      selected = 252),
    numericInput("bt_cost", "Trading cost (bps per unit of turnover)",
      value = 10, min = 0, max = 100, step = 1),
    actionButton("bt_run", "Run backtest", class = "btn-primary w-100"),
    rail_note("Rebalances monthly and uses the weight cap above. Downloads the extra history it needs.")
  )
}

backtest_sections_ui <- function() {
  tagList(
    page_section("03", "Does it hold up?",
      lead = paste(
        "A walk-forward backtest. At each month-end, every method re-estimates from the trailing window only,",
        "trades at the close, and holds until the next month-end. Nothing it could not have known is used."
      ),
      uiOutput("bt_body")
    ),
    page_section("04", "Reference results",
      lead = "The same test on two fixed ETF universes chosen in advance, so nobody picked the winners.",
      uiOutput("bt_reference")
    )
  )
}

bt_insight <- function(text, conf, cls = "insight-card--neutral") {
  tags$div(class = paste("insight-card", cls),
    tags$span(class = paste0("insight-conf insight-conf--", conf), paste(conf, "confidence")),
    tags$p(text)
  )
}

# The headline: does the best method beat equal weight by more than noise?
bt_headline <- function(summary, boot, candidates) {
  b <- boot[boot$strategy %in% setdiff(candidates, "Equal weight"), ]
  best <- b[which.max(b$sharpe), ]
  ew <- boot$sharpe[boot$strategy == "Equal weight"]
  gap <- sprintf("95%% interval for the gap: %s to %s", bt_fmt_num(best$diff_lo), bt_fmt_num(best$diff_hi))
  if (best$diff_lo > 0) {
    bt_insight(sprintf("%s beat equal weight after costs: a Sharpe of %s vs %s, and the %s, which excludes zero.",
                       best$strategy, bt_fmt_num(best$sharpe), bt_fmt_num(ew), gap),
               "high", "insight-card--ok")
  } else if (best$diff > 0.2) {
    bt_insight(sprintf("%s had the best Sharpe after costs (%s vs %s for equal weight), but the %s includes zero, so the edge may be luck.",
                       best$strategy, bt_fmt_num(best$sharpe), bt_fmt_num(ew), gap),
               "medium")
  } else if (best$diff > 0) {
    bt_insight(sprintf("No method clearly beat equal weight. The best, %s, had a Sharpe of %s vs %s, a gap well within noise (%s).",
                       best$strategy, bt_fmt_num(best$sharpe), bt_fmt_num(ew), gap),
               "low")
  } else {
    bt_insight(sprintf("Nothing beat plain equal weight (Sharpe %s) after costs on this history.", bt_fmt_num(ew)),
               "low", "insight-card--warn")
  }
}

bt_laggards <- function(boot, candidates) {
  b <- boot[boot$strategy %in% setdiff(candidates, "Equal weight") & boot$diff_hi < 0, ]
  n <- nrow(b)
  if (!n) return(NULL)
  who <- if (n == 1) b$strategy else paste(paste(b$strategy[-n], collapse = ", "), "and", b$strategy[n])
  bt_insight(sprintf("%s did worse than equal weight by more than noise: %s 95%% interval for the gap is entirely below zero.",
                     who, if (n == 1) "its" else "each one's"),
             "high", "insight-card--warn")
}

bt_results_table <- function(summary, boot) {
  b <- boot[match(summary$strategy, boot$strategy), ]
  data.frame(
    Strategy = summary$strategy,
    CAGR = bt_fmt_pct(summary$cagr),
    Volatility = bt_fmt_pct(summary$volatility),
    `Sharpe (95% CI)` = paste(bt_fmt_num(summary$sharpe), bt_fmt_ci(b$sharpe_lo, b$sharpe_hi)),
    `vs equal weight` = ifelse(summary$strategy == "Equal weight", "baseline",
                               bt_fmt_diff(b$diff, b$diff_lo, b$diff_hi)),
    `Max drawdown` = bt_fmt_pct(summary$max_drawdown),
    `Turnover / yr` = bt_fmt_pct(summary$turnover_annual, 0),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
}

backtest_server <- function(input, output, session, portfolio) {
  model_strategies <- lapply(
    stats::setNames(unname(allocation_methods), names(allocation_methods)),
    function(m) list(method = m)
  )

  bt_result <- reactive({
    req(isTRUE(portfolio$analyzed), !is.null(portfolio$weights_frac))
    weights <- portfolio$weights_frac
    tickers <- names(weights)
    validate(need(length(tickers) >= 2, "The backtest compares allocation methods, so it needs at least two holdings."))
    bench <- bt_or(portfolio$benchmark_ticker, "SPY")
    years <- as.numeric(input$bt_years)
    lookback <- as.integer(input$bt_lookback)
    cost <- max(0, as.numeric(bt_or(input$bt_cost, 10)), na.rm = TRUE)
    cap <- bt_or(input$alloc_max_w_pct, 100) / 100

    withProgress(message = "Running walk-forward backtest", value = 0.1, {
      from <- Sys.Date() - ceiling(years * 365.25 + lookback * 1.5)
      prices <- fetch_stock_data(unique(c(tickers, bench)), from_date = from, to_date = Sys.Date())
      validate(need(!is.null(prices), "Could not download price history for the backtest."))
      missing <- setdiff(unique(c(tickers, bench)), unique(prices$ticker))
      validate(need(!length(intersect(missing, tickers)),
                    paste("No price history for:", paste(intersect(missing, tickers), collapse = ", "))))
      incProgress(0.4, detail = "Risk-free rate")
      irx <- fetch_stock_data("^IRX", from_date = from, to_date = Sys.Date())
      rm <- returns_matrix_from_prices(prices, unique(c(tickers, bench)))
      rf <- if (!is.null(irx)) {
        daily_riskfree(rm$dates, data.frame(date = irx$date, irx_pct = irx$adjusted))
      } else 0

      strategies <- c(
        model_strategies,
        list("Your weights" = list(weights = weights)),
        if (!bench %in% missing) stats::setNames(
          list(list(weights = stats::setNames(1, bench), costs = FALSE)),
          paste0("Benchmark: ", portfolio$benchmark_name))
      )
      incProgress(0.2, detail = "Re-estimating every month")
      bt <- tryCatch(
        run_walk_forward(rm$R, rm$dates, strategies, rf = rf, lookback = lookback,
                         cost_bps = cost, max_w = cap, universe = tickers),
        error = function(e) NULL
      )
      validate(need(!is.null(bt), "Not enough shared history for even one rebalance. Try a shorter estimation window."))
      incProgress(0.2, detail = "Bootstrapping confidence intervals")
      list(
        bt = bt,
        summary = bt_summary(bt),
        boot = bt_bootstrap_sharpe(bt, baseline = "Equal weight", n_boot = 1000L),
        in_sample = bt_in_sample_max_sharpe(rm$R, rm$dates, bt, cap, tickers),
        tickers = tickers,
        rf_source = if (is.numeric(rf) && length(rf) > 1) "13-week T-bill" else "zero (T-bill download failed)",
        params = list(years = years, lookback = lookback, cost = cost, cap = cap)
      )
    })
  }) %>%
    bindCache(sort(names(portfolio$weights_frac)), round(portfolio$weights_frac[sort(names(portfolio$weights_frac))], 6),
              portfolio$benchmark_ticker, input$bt_years, input$bt_lookback, input$bt_cost,
              input$alloc_max_w_pct, Sys.Date()) %>%
    bindEvent(input$bt_run)

  output$bt_body <- renderUI({
    if (!isTRUE(portfolio$analyzed)) return(NULL)
    if (is.null(input$bt_run) || input$bt_run == 0) {
      return(tags$p(class = "page-footnote",
        "Choose the history and settings in the rail, then run the backtest. ",
        "It downloads up to 20 years of prices and takes a few seconds."))
    }
    r <- bt_result()
    s <- r$summary
    candidates <- c(names(allocation_methods), "Your weights")
    ms_wf <- s$sharpe_gross[s$strategy == "Max Sharpe"]
    fallbacks <- s$fallbacks[s$strategy == "Max Sharpe"]
    ew_w <- r$bt$strategies[["Equal weight"]]$weights
    excluded <- vapply(r$tickers, function(tk) sum(ew_w[[tk]] == 0), numeric(1))
    excluded <- excluded[excluded > 0]
    period <- range(r$bt$dates)

    tagList(
      tags$p(class = "page-footnote",
        sprintf("Out of sample %s to %s (%d monthly rebalances). %s-day estimation window, %s bps per unit of turnover, %s cap. Sharpe uses the %s rate.",
                format(period[1], "%b %Y"), format(period[2], "%b %Y"), length(r$bt$rebalances),
                r$params$lookback, r$params$cost,
                if (r$params$cap >= 1) "no" else bt_fmt_pct(r$params$cap, 0), r$rf_source)),
      tags$div(class = "insight-grid",
        bt_headline(s, r$boot, candidates),
        bt_laggards(r$boot, candidates),
        bt_insight(
          sprintf("Max Sharpe looked like %s with hindsight (fitted once on the whole period) but delivered %s when it could only see the past. Its turnover was %s a year.",
                  bt_fmt_num(r$in_sample$sharpe), bt_fmt_num(ms_wf),
                  bt_fmt_pct(s$turnover_annual[s$strategy == "Max Sharpe"], 0)),
          "medium")
      ),
      plot_plate("bt_growth_plot", "360px"),
      tableOutput("bt_table"),
      tags$div(class = "bt-weights-head",
        selectInput("bt_weights_strategy", "Weights over time",
          choices = names(allocation_methods), selected = "Min variance", width = "280px")
      ),
      plot_plate("bt_weights_plot", "300px"),
      tags$ul(class = "page-footnote bt-notes",
        if (fallbacks > 0) tags$li(sprintf(
          "Max Sharpe fell back to min variance at %d of %d rebalances because no capped mix beat the T-bill rate.",
          fallbacks, length(r$bt$rebalances))),
        if (length(excluded)) tags$li(sprintf(
          "Excluded until they had a full estimation window: %s.",
          paste(sprintf("%s (%d rebalances)", names(excluded), excluded), collapse = ", "))),
        tags$li("Your weights and the model methods pay trading costs; the benchmark is held without them.")
      )
    )
  })

  output$bt_table <- renderTable({
    r <- bt_result()
    bt_results_table(r$summary, r$boot)
  }, align = "l", striped = FALSE)

  output$bt_growth_plot <- renderAxisPlot({
    r <- bt_result()
    df <- do.call(rbind, lapply(names(r$bt$strategies), function(nm) {
      x <- r$bt$strategies[[nm]]$returns
      data.frame(date = x$date, strategy = nm, wealth = cumprod(1 + x$net))
    }))
    lvls <- names(r$bt$strategies)
    df$strategy <- factor(df$strategy, levels = lvls)
    is_bench <- grepl("^Benchmark", lvls)
    cols <- stats::setNames(axis_palette(length(lvls)), lvls)
    cols["Your weights"] <- AXIS$brass
    cols[is_bench] <- AXIS$ink_soft
    others <- setdiff(lvls, c("Your weights", lvls[is_bench]))
    cols[others] <- axis_palette(length(others) + 1)[-1]
    sizes <- stats::setNames(ifelse(lvls == "Your weights", 1.1, 0.55), lvls)
    types <- stats::setNames(ifelse(is_bench, "22", "solid"), lvls)
    ggplot(df, aes(date, wealth, color = strategy, linewidth = strategy, linetype = strategy)) +
      geom_line() +
      scale_color_manual(values = cols) +
      scale_linewidth_manual(values = sizes) +
      scale_linetype_manual(values = types) +
      scale_y_log10(labels = scales::dollar_format(accuracy = 0.01)) +
      labs(title = "Growth of $1, after costs", x = NULL, y = NULL,
           color = NULL, linewidth = NULL, linetype = NULL, caption = "Log scale") +
      theme_axis(base_size = 12) +
      theme(legend.position = "bottom") +
      guides(color = guide_legend(nrow = 2))
  })

  output$bt_weights_plot <- renderAxisPlot({
    r <- bt_result()
    nm <- bt_or(input$bt_weights_strategy, "Min variance")
    W <- r$bt$strategies[[nm]]$weights
    req(!is.null(W))
    long <- tidyr::pivot_longer(W[, c("date", r$tickers)], -date, names_to = "ticker", values_to = "weight")
    ggplot(long, aes(date, weight, fill = ticker)) +
      geom_area(position = "stack", color = AXIS$vellum, linewidth = 0.15) +
      scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
      labs(title = paste0(nm, ": target weights at each rebalance"), x = NULL, y = NULL, fill = NULL) +
      theme_axis(base_size = 12) +
      theme(legend.position = "bottom")
  })

  output$bt_reference <- renderUI({
    ref <- bt_reference
    if (is.null(ref)) return(tags$p(class = "page-footnote", "Reference results are not bundled with this build."))
    rows <- names(allocation_methods)
    cell <- function(cs, nm) {
      b <- cs$boot[cs$boot$strategy == nm, ]
      if (!nrow(b)) return("")
      if (nm == "Equal weight") return(paste0(bt_fmt_num(b$sharpe), " (baseline)"))
      paste0(bt_fmt_num(b$sharpe), " (vs equal weight ",
             bt_fmt_diff(b$diff, b$diff_lo, b$diff_hi), ")")
    }
    ac <- ref$cases$asset_class
    sc <- ref$cases$sector
    tbl <- tags$table(class = "shiny-table table bt-ref-table",
      tags$thead(tags$tr(tags$th("Strategy"), tags$th(ac$label), tags$th(sc$label))),
      tags$tbody(lapply(rows, function(nm) tags$tr(tags$td(nm), tags$td(cell(ac, nm)), tags$td(cell(sc, nm)))))
    )
    methods <- setdiff(rows, "Equal weight")
    beat <- unlist(lapply(list(ac, sc), function(cs) {
      b <- cs$boot[cs$boot$strategy %in% methods, ]
      if (any(b$diff_lo > 0)) paste0(b$strategy[b$diff_lo > 0], " (", cs$label, ")")
    }))
    headline <- if (!length(beat)) {
      sprintf("On both universes, no method's Sharpe ratio beat equal weight by more than noise: every 95%% interval for the gap includes zero, %s to %s.",
              format(ac$period[1], "%B %Y"), format(ac$period[2], "%B %Y"))
    } else {
      sprintf("Beat equal weight beyond noise: %s. Every other method's gap was within noise.",
              paste(beat, collapse = "; "))
    }
    tagList(
      tags$div(class = "insight-grid",
        bt_insight(headline, "high"),
        bt_insight(sprintf(
          "Min variance's real payoff was smaller losses: a worst drawdown of %s vs %s for equal weight on asset-class ETFs.",
          bt_fmt_pct(ac$summary$max_drawdown[ac$summary$strategy == "Min variance"]),
          bt_fmt_pct(ac$summary$max_drawdown[ac$summary$strategy == "Equal weight"])), "high", "insight-card--ok")
      ),
      tags$div(class = "shiny-html-output", tbl),
      tags$p(class = "page-footnote",
        sprintf("Sharpe after costs, %s to %s. Universes: %s; %s. %d-day window, %s bps costs, %s%% cap. Full tables in docs/results.md on GitHub.",
                format(ac$period[1], "%b %Y"), format(ac$period[2], "%b %Y"),
                paste(ac$universe, collapse = ", "), paste(sc$universe, collapse = ", "),
                ref$params$lookback, ref$params$cost_bps, 100 * ref$params$max_w))
    )
  })
}
