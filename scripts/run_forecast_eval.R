#!/usr/bin/env Rscript
# Rolling-origin ARIMA vs random walk on the frozen dataset. Run from the repo
# root. Writes data/eval/forecast_eval.rds, then regenerates docs/results.md
# through scripts/run_evaluation.R. Takes a few minutes.

suppressPackageStartupMessages({
  for (f in c("config.R", "eval_config.R", "metrics.R", "forecast_eval.R")) source(file.path("R", f))
})

prices <- readRDS(file.path(eval_data_dir, "prices.rds"))
cfg <- eval_forecast

groups <- c(
  stats::setNames(rep("App stocks", length(all_stocks)), unname(all_stocks)),
  stats::setNames(rep(eval_universes$asset_class$label, length(eval_universes$asset_class$tickers)),
                  eval_universes$asset_class$tickers),
  stats::setNames(rep(eval_universes$sector$label, length(eval_universes$sector$tickers)),
                  eval_universes$sector$tickers)
)
groups <- groups[!duplicated(names(groups))]

summary <- do.call(rbind, lapply(names(groups), function(tk) {
  message("Evaluating ", tk)
  x <- prices[prices$ticker == tk & !is.na(prices$adjusted), ]
  x <- x[order(x$date), ]
  rows <- fc_rolling_forecasts(log(x$adjusted), x$date, cfg$horizons, cfg$train_window,
                               cfg$origin_step, cfg$reselect_every, cfg$start)
  s <- fc_summarize_ticker(rows, cfg$origin_step)
  cbind(ticker = tk, group = groups[[tk]], first_origin = min(rows$origin),
        last_origin = max(rows$origin), s)
}))
rownames(summary) <- NULL
summary$dm_p_bh <- stats::p.adjust(summary$dm_p, method = "BH")
summary$confidence <- mapply(fc_confidence, summary$relmae, summary$dm_p_bh)

pooled <- do.call(rbind, lapply(split(summary, list(summary$group, summary$h)), function(s) {
  data.frame(
    group = s$group[1], h = s$h[1], tickers = nrow(s),
    relmae_median = stats::median(s$relmae), relmae_min = min(s$relmae), relmae_max = max(s$relmae),
    arima_better = sum(s$relmae < 1),
    sig_better = sum(s$dm_p < 0.05 & s$relmae < 1), sig_worse = sum(s$dm_p < 0.05 & s$relmae > 1),
    bh_better = sum(s$dm_p_bh < 0.05 & s$relmae < 1), bh_worse = sum(s$dm_p_bh < 0.05 & s$relmae > 1),
    cov80 = stats::weighted.mean(s$cov80, s$n), cov95 = stats::weighted.mean(s$cov95, s$n),
    cov95_low_vol = mean(s$cov95_low_vol), cov95_mid_vol = mean(s$cov95_mid_vol),
    cov95_high_vol = mean(s$cov95_high_vol),
    rw_share = mean(s$rw_share)
  )
}))
pooled <- pooled[order(match(pooled$group, unique(groups)), pooled$h), ]
rownames(pooled) <- NULL

saveRDS(
  list(generated = Sys.time(), config = cfg, groups = unique(groups),
       summary = summary, pooled = pooled),
  file.path(eval_data_dir, "forecast_eval.rds"), compress = "xz"
)
message("Wrote ", file.path(eval_data_dir, "forecast_eval.rds"))

source("scripts/run_evaluation.R", local = new.env())
