# Explore -> Forecast: a live ARIMA forecast plus its precomputed track record
# against a random walk (scripts/run_forecast_eval.R).

fc_reference <- local({
  f <- file.path("data", "eval", "forecast_eval.rds")
  if (!file.exists(f)) return(NULL)
  readRDS(f)[c("config", "summary", "pooled")]
})

fc_train_days <- 504L

forecast_rail_ui <- function() {
  tagList(
    rail_group("Ticker",
      selectInput("forecast_sector", "Sector",
        choices = sector_filter_choices(), selected = "all"),
      selectInput("forecast_ticker", "Ticker", choices = NULL)
    ),
    rail_group("Model",
      sliderInput("horizon", "Horizon (trading days)", min = 1, max = 63, value = 21),
      rail_note("ARIMA on the last two years of daily log prices, the same setup as the accuracy test.")
    )
  )
}

forecast_sections_ui <- function() {
  tagList(
    page_section("01", "Forecast",
      lead = "ARIMA on log prices, with 80% and 95% intervals. Exploratory only, not a trading signal.",
      plot_plate("forecastPlot", "400px")),
    page_section("02", "How accurate has it been?",
      lead = paste(
        "Tested against a random walk, which assumes tomorrow's price equals today's,",
        "at weekly origins since 2015 on a frozen copy of the data."
      ),
      uiOutput("fc_accuracy"),
      plot_plate("fc_relmae_plot", "300px"))
  )
}

fc_future_dates <- function(last, h) {
  d <- seq(last + 1, by = "day", length.out = ceiling(h * 7 / 5) + 7)
  head(d[!format(d, "%u") %in% c("6", "7")], h)
}

fc_model_label <- function(fit) {
  o <- forecast::arimaorder(fit)
  paste0("ARIMA(", paste(o[1:3], collapse = ","), ")",
         if ("drift" %in% names(stats::coef(fit))) " with drift" else "")
}

fc_eval_horizon <- function(h, horizons) horizons[which.min(abs(horizons - h))]

fc_fmt_p <- function(p) ifelse(is.na(p), "n/a", ifelse(p < 0.001, "< 0.001", formatC(p, format = "f", digits = 3)))

fc_headline <- function(tk, r) {
  stats <- sprintf("RelMAE %s, corrected p = %s", bt_fmt_num(r$relmae, 3), fc_fmt_p(r$dm_p_bh))
  text <- if (r$dm_p_bh < 0.05 && r$relmae < 1) {
    sprintf("ARIMA beat a random walk for %s at the %d-day horizon (%s).", tk, r$h, stats)
  } else if (r$dm_p_bh < 0.05) {
    sprintf("ARIMA was less accurate than a random walk for %s at the %d-day horizon (%s). Assuming no change would have done better.",
            tk, r$h, stats)
  } else {
    sprintf("ARIMA did not meaningfully beat a random walk for %s at the %d-day horizon (%s); treat the forecast line as a rough guide.",
            tk, r$h, stats)
  }
  cls <- switch(r$confidence, high = "insight-card--ok", medium = "insight-card--neutral", "insight-card--warn")
  bt_insight(text, r$confidence, cls)
}

fc_accuracy_table <- function(s) {
  data.frame(
    Horizon = paste(s$h, ifelse(s$h == 1, "day", "days")),
    `ARIMA error` = bt_fmt_pct(s$mae_arima, 2),
    `Random walk error` = bt_fmt_pct(s$mae_rw, 2),
    RelMAE = bt_fmt_num(s$relmae, 3),
    `p (corrected)` = fc_fmt_p(s$dm_p_bh),
    `80% coverage` = bt_fmt_pct(s$cov80),
    `95% coverage` = bt_fmt_pct(s$cov95),
    `95% coverage, volatile weeks` = bt_fmt_pct(s$cov95_high_vol),
    check.names = FALSE, stringsAsFactors = FALSE
  )
}

forecast_server <- function(input, output, session) {
  forecast_data <- reactive({
    tk <- input$forecast_ticker
    req(tk)
    withProgress(message = paste("Loading", tk), value = 0.5, {
      df <- fetch_stock_data(tk, from_date = Sys.Date() - 800, to_date = Sys.Date())
    })
    validate(need(!is.null(df) && nrow(df) > 0, paste("No price history found for", tk)))
    df <- df[!is.na(df$adjusted), ]
    tail(df[order(df$date), c("date", "adjusted")], fc_train_days)
  }) %>% bindCache(input$forecast_ticker, Sys.Date())

  forecast_fit <- reactive({
    df <- forecast_data()
    validate(need(nrow(df) >= 120, "Need at least six months of history to fit a forecast."))
    fit <- tryCatch(forecast::auto.arima(stats::ts(log(df$adjusted), frequency = 1), seasonal = FALSE),
                    error = function(e) NULL)
    validate(need(!is.null(fit), "Could not fit a model to this price history."))
    list(df = df, fit = fit)
  })

  output$forecastPlot <- renderAxisPlot({
    ff <- forecast_fit()
    h <- input$horizon
    f <- forecast::forecast(ff$fit, h = h, level = c(80, 95))
    hist <- tail(ff$df, 126)
    fut <- data.frame(
      date = fc_future_dates(max(ff$df$date), h),
      mid = exp(as.numeric(f$mean)),
      lo80 = exp(f$lower[, 1]), hi80 = exp(f$upper[, 1]),
      lo95 = exp(f$lower[, 2]), hi95 = exp(f$upper[, 2])
    )
    o <- forecast::arimaorder(ff$fit)
    flat <- fc_is_random_walk(list(order = o[1:3], drift = "drift" %in% names(stats::coef(ff$fit))))
    ggplot() +
      geom_ribbon(data = fut, aes(date, ymin = lo95, ymax = hi95), fill = AXIS$slate, alpha = 0.16) +
      geom_ribbon(data = fut, aes(date, ymin = lo80, ymax = hi80), fill = AXIS$slate, alpha = 0.28) +
      geom_line(data = hist, aes(date, adjusted), color = AXIS$ink, linewidth = 0.7) +
      geom_line(data = rbind(data.frame(date = max(hist$date), mid = tail(hist$adjusted, 1)), fut[c("date", "mid")]),
                aes(date, mid), color = AXIS$brass, linewidth = 0.9, linetype = "dashed") +
      scale_y_continuous(labels = scales::dollar_format()) +
      labs(
        title = paste0(input$forecast_ticker, ", next ", h, " trading days"),
        subtitle = paste0(fc_model_label(ff$fit), " on log prices, fitted to the last ", nrow(ff$df), " trading days",
                          if (flat) ". It chose a random walk, so the forecast is flat." else "."),
        x = NULL, y = NULL,
        caption = "Ink = last six months · brass = median forecast · shaded = 80% and 95% intervals"
      ) +
      theme_axis(base_size = 12)
  })

  output$fc_accuracy <- renderUI({
    tk <- input$forecast_ticker
    req(tk)
    if (is.null(fc_reference)) return(tags$p(class = "page-footnote", "Accuracy results are not bundled with this build."))
    s <- fc_reference$summary[fc_reference$summary$ticker == tk, ]
    if (!nrow(s)) return(tags$p(class = "page-footnote", paste("No precomputed accuracy results for", tk, ".")))
    s <- s[order(s$h), ]
    cfg <- fc_reference$config
    all <- fc_reference$summary
    he <- fc_eval_horizon(input$horizon, cfg$horizons)
    r <- s[s$h == he, ]
    tbl <- fc_accuracy_table(s)
    tagList(
      tags$div(class = "insight-grid",
        fc_headline(tk, r),
        bt_insight(sprintf(
          "At %d days, its 95%% intervals held the actual price %s of the time, but %s in the most volatile third of weeks.",
          he, bt_fmt_pct(r$cov95, 0), bt_fmt_pct(r$cov95_high_vol, 0)),
          "high", if (r$cov95_high_vol < 0.9) "insight-card--warn" else "insight-card--neutral"),
        bt_insight(sprintf(
          "Across all %d tickers tested at %s days, ARIMA beat the random walk after correction in %d of %d cases and lost in %d.",
          length(unique(all$ticker)), paste(cfg$horizons, collapse = ", "),
          sum(all$dm_p_bh < 0.05 & all$relmae < 1), nrow(all), sum(all$dm_p_bh < 0.05 & all$relmae > 1)),
          "high")
      ),
      tags$div(class = "shiny-html-output", tags$table(class = "shiny-table table fc-table",
        tags$thead(tags$tr(lapply(names(tbl), tags$th))),
        tags$tbody(lapply(seq_len(nrow(tbl)), function(i) {
          tags$tr(class = if (s$h[i] == he) "fc-row-active", lapply(unlist(tbl[i, ]), tags$td))
        }))
      )),
      tags$p(class = "page-footnote", sprintf(
        "%d weekly origins, %s to %s. Error is the mean absolute log-price error, roughly a percentage move. RelMAE below 1 means ARIMA was more accurate. p is a two-sided Diebold-Mariano test, corrected (Benjamini-Hochberg) across all %d ticker and horizon tests. The highlighted row is the tested horizon closest to yours.",
        max(s$n), format(s$first_origin[1], "%b %Y"), format(s$last_origin[1], "%b %Y"), nrow(all)))
    )
  })

  output$fc_relmae_plot <- renderAxisPlot({
    tk <- input$forecast_ticker
    req(tk, fc_reference)
    all <- fc_reference$summary
    all$hl <- factor(paste(all$h, ifelse(all$h == 1, "day", "days")),
                     levels = paste(sort(unique(all$h)), ifelse(sort(unique(all$h)) == 1, "day", "days")))
    me <- all[all$ticker == tk, ]
    set.seed(1)
    ggplot(all, aes(hl, relmae)) +
      geom_hline(yintercept = 1, color = AXIS$rule, linetype = "dashed") +
      geom_jitter(width = 0.15, height = 0, color = AXIS$stone, alpha = 0.55, size = 2) +
      geom_point(data = me, color = AXIS$brass, size = 4) +
      annotate("text", x = 0.55, y = 1, label = "Random walk", hjust = 0, vjust = -0.6,
               color = AXIS$muted, size = 3.2, family = AXIS_FONT$sans) +
      labs(title = paste0("RelMAE for all ", length(unique(all$ticker)), " tested tickers, ", tk, " in brass"),
           subtitle = "Below the line, ARIMA was more accurate than the random walk; above it, less.",
           x = NULL, y = "RelMAE") +
      theme_axis(base_size = 12)
  })
}
