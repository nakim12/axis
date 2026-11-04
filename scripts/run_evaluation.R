#!/usr/bin/env Rscript
# Walk-forward backtest on the frozen dataset. Run from the repo root.
# Writes data/eval/backtest_results.rds and docs/results.md. Every reported
# backtest number comes from here.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
for (f in c("config.R", "eval_config.R", "metrics.R", "scenarios.R",
            "allocation.R", "backtest.R")) {
  suppressPackageStartupMessages(source(file.path("R", f)))
}

prices   <- readRDS(file.path(eval_data_dir, "prices.rds"))
riskfree <- readRDS(file.path(eval_data_dir, "riskfree.rds"))
p <- eval_backtest_defaults

model_strategies <- lapply(
  stats::setNames(unname(allocation_methods), names(allocation_methods)),
  function(m) list(method = m)
)

run_case <- function(label, universe, benchmark, extra = list()) {
  message("Running ", label)
  tickers <- unique(c(universe, names(benchmark$weights), unlist(lapply(extra, function(s) names(s$weights)))))
  rm <- returns_matrix_from_prices(prices, tickers)
  rf <- daily_riskfree(rm$dates, riskfree)
  strategies <- c(
    model_strategies,
    extra,
    stats::setNames(list(list(weights = benchmark$weights, costs = FALSE)),
                    paste0("Benchmark: ", benchmark$label))
  )
  bt <- run_walk_forward(rm$R, rm$dates, strategies, rf = rf,
                         lookback = p$lookback, cost_bps = p$cost_bps,
                         max_w = p$max_w, universe = universe)
  summary <- bt_summary(bt)
  boot <- bt_bootstrap_sharpe(bt, baseline = "Equal weight")
  ins <- bt_in_sample_max_sharpe(rm$R, rm$dates, bt, p$max_w, universe)
  shrink <- vapply(bt_rebalance_index(rm$dates, p$lookback), function(i) {
    w <- rm$R[(i - p$lookback + 1):i, intersect(bt_eligible(rm$R[(i - p$lookback + 1):i, , drop = FALSE]), universe), drop = FALSE]
    attr(ledoit_wolf_cov(w), "shrinkage")
  }, numeric(1))
  list(
    label = label, universe = universe, benchmark = benchmark$label,
    period = range(bt$dates), n_days = length(bt$dates),
    n_rebalances = length(bt$rebalances),
    summary = summary, boot = boot,
    in_sample_max_sharpe = ins,
    walk_forward_max_sharpe_gross = summary$sharpe_gross[summary$strategy == "Max Sharpe"],
    mean_shrinkage = mean(shrink),
    notes = lapply(bt$strategies, `[[`, "notes"),
    returns = lapply(bt$strategies, function(s) s$returns[, c("date", "net")]),
    weights = lapply(bt$strategies[names(model_strategies)], `[[`, "weights")
  )
}

cases <- list(
  asset_class = run_case(eval_universes$asset_class$label,
                         eval_universes$asset_class$tickers,
                         eval_universes$asset_class$benchmark),
  sector      = run_case(eval_universes$sector$label,
                         eval_universes$sector$tickers,
                         eval_universes$sector$benchmark)
)

spy <- list(label = "S&P 500 (SPY)", weights = c(SPY = 1))
for (nm in names(sample_portfolios)) {
  sp <- sample_portfolios[[nm]]
  w <- stats::setNames(sp$weights / sum(sp$weights), sp$tickers)
  cases[[paste0("sample_", nm)]] <- run_case(
    paste0("Sample: ", nm), sp$tickers, spy,
    extra = list("Sample weights" = list(weights = w))
  )
}

saveRDS(
  list(generated = Sys.time(), params = p, data_manifest = file.path(eval_data_dir, "MANIFEST.md"),
       cases = cases),
  file.path(eval_data_dir, "backtest_results.rds"), compress = "xz"
)

# ---- Markdown report --------------------------------------------------------

pct <- function(x, d = 1) ifelse(is.na(x), "n/a", sprintf(paste0("%.", d, "f%%"), 100 * x))
num <- function(x, d = 2) ifelse(is.na(x), "n/a", sprintf(paste0("%.", d, "f"), x))
ci  <- function(lo, hi) sprintf("[%s, %s]", num(lo), num(hi))

case_table <- function(cs) {
  s <- cs$summary
  b <- cs$boot[match(s$strategy, cs$boot$strategy), ]
  c(
    "| Strategy | CAGR | Volatility | Sharpe (95% CI) | Sharpe before costs | Sharpe vs equal weight (95% CI) | Max drawdown | Turnover / yr | Fallbacks |",
    "|---|---|---|---|---|---|---|---|---|",
    sprintf("| %s | %s | %s | %s %s | %s | %s | %s | %s | %d |",
            s$strategy, pct(s$cagr), pct(s$volatility),
            num(s$sharpe), ci(b$sharpe_lo, b$sharpe_hi),
            num(s$sharpe_gross),
            ifelse(s$strategy == "Equal weight", "baseline",
                   paste(sprintf("%+.2f", b$diff), ci(b$diff_lo, b$diff_hi))),
            pct(s$max_drawdown), pct(s$turnover_annual, 0), s$fallbacks)
  )
}

case_section <- function(cs, heading_level = "##") {
  ins <- cs$in_sample_max_sharpe
  c(
    paste(heading_level, cs$label),
    "",
    sprintf("Universe: %s. Benchmark: %s. Out-of-sample period: %s to %s (%d trading days, %d monthly rebalances).",
            paste(cs$universe, collapse = ", "), cs$benchmark,
            cs$period[1], cs$period[2], cs$n_days, cs$n_rebalances),
    "",
    case_table(cs),
    "",
    sprintf("Max Sharpe, in sample vs walk-forward (before costs): **%s** when fitted once on the whole period with hindsight, **%s** when re-estimated monthly from trailing data only.",
            num(ins$sharpe), num(cs$walk_forward_max_sharpe_gross)),
    "",
    sprintf("Average Ledoit-Wolf shrinkage intensity: %.2f.", cs$mean_shrinkage),
    ""
  )
}

report <- c(
  "# Axis evaluation results",
  "",
  "Generated by `scripts/run_evaluation.R` from the frozen dataset in `data/eval/` (see its MANIFEST). Do not edit by hand.",
  "",
  sprintf("Settings, fixed before any results were computed: %d-day lookback, monthly rebalance on the last trading day, %s bps per unit of turnover, %s%% weight cap (raised to 1/n when fewer names are eligible). Sharpe and Sortino use the 13-week T-bill rate. Confidence intervals come from a stationary block bootstrap (mean block 21 days, 2,000 resamples) that resamples the same days for every strategy.",
          p$lookback, p$cost_bps, 100 * p$max_w),
  "",
  "Sharpe ratios are after costs unless labeled. A difference from equal weight whose interval includes zero is not distinguishable from noise.",
  "",
  case_section(cases$asset_class),
  case_section(cases$sector),
  "## Sample portfolios (demo only)",
  "",
  "These are five or six of today's large-cap stocks, so they carry survivorship and hindsight bias. They show the engine working on the app's samples, not evidence about strategies.",
  "",
  unlist(lapply(cases[grepl("^sample_", names(cases))], case_section, heading_level = "###"))
)

forecast_file <- file.path(eval_data_dir, "forecast_eval.rds")
if (file.exists(forecast_file)) {
  fe <- readRDS(forecast_file)
  fc <- fe$config
  pl <- fe$pooled
  s  <- fe$summary
  report <- c(report,
    "## Forecasts: ARIMA vs a random walk",
    "",
    sprintf("Generated by `scripts/run_forecast_eval.R`. Rolling origins every %d trading days from %s (or two years after listing), a %d-day training window of log prices, horizons of %s trading days. `auto.arima` re-selects the order every %d origins; coefficients are re-estimated at every origin. The baseline is a random walk (tomorrow's price equals today's). Errors are in log-price space.",
            fc$origin_step, fc$start, fc$train_window, paste(fc$horizons, collapse = ", "), fc$reselect_every),
    "",
    "RelMAE is ARIMA's mean absolute error divided by the random walk's on the same points; below 1 means ARIMA was more accurate. Significance uses a two-sided Diebold-Mariano test on absolute errors with the Harvey-Leybourne-Newbold correction, then a Benjamini-Hochberg correction across all tickers and horizons.",
    "",
    "| Group | Horizon | Tickers | RelMAE median [range] | ARIMA better | Significant (better / worse) | After BH (better / worse) | 80% coverage | 95% coverage | 95% coverage, low / mid / high volatility |",
    "|---|---|---|---|---|---|---|---|---|---|",
    sprintf("| %s | %d | %d | %s [%s, %s] | %d | %d / %d | %d / %d | %s | %s | %s / %s / %s |",
            pl$group, pl$h, pl$tickers, num(pl$relmae_median, 3), num(pl$relmae_min, 3), num(pl$relmae_max, 3),
            pl$arima_better, pl$sig_better, pl$sig_worse, pl$bh_better, pl$bh_worse,
            pct(pl$cov80), pct(pl$cov95), pct(pl$cov95_low_vol), pct(pl$cov95_mid_vol), pct(pl$cov95_high_vol)),
    "",
    sprintf("Across all %d tickers and %d horizons (%d tests), ARIMA beat the random walk after the Benjamini-Hochberg correction in %d cases and lost in %d. At the order re-selections, `auto.arima` picked a plain random walk %s of the time.",
            length(unique(s$ticker)), length(fc$horizons), nrow(s),
            sum(s$dm_p_bh < 0.05 & s$relmae < 1), sum(s$dm_p_bh < 0.05 & s$relmae > 1),
            pct(mean(s$rw_share[s$h == fc$horizons[1]]), 0)),
    "",
    "Volatility thirds are set per ticker by trailing 21-day realized volatility at each origin.",
    ""
  )
}
writeLines(report, "docs/results.md")
message("Wrote docs/results.md and ", file.path(eval_data_dir, "backtest_results.rds"))
