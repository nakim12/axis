# Rolling-origin evaluation of ARIMA price forecasts against a random walk.
# Everything works on log prices: a log-price forecast error is a log-return
# error, so it is comparable across tickers, and interval coverage is the same
# in log and price space.

fc_select_spec <- function(y) {
  fit <- forecast::auto.arima(y, seasonal = FALSE)
  cf <- names(stats::coef(fit))
  list(order = unname(forecast::arimaorder(fit)[1:3]),
       drift = "drift" %in% cf,
       mean  = "intercept" %in% cf)
}

fc_is_random_walk <- function(spec) {
  identical(as.numeric(spec$order), c(0, 1, 0)) && !spec$drift
}

# Re-estimates the coefficients for a fixed order. Falls back to the random
# walk (keeping drift) if the fixed-order fit fails on this window.
fc_refit <- function(y, spec) {
  tryCatch(
    forecast::Arima(y, order = spec$order, include.mean = spec$mean,
                    include.drift = spec$drift && spec$order[2] == 1),
    error = function(e) forecast::Arima(y, order = c(0, 1, 0), include.drift = spec$drift)
  )
}

fc_origins <- function(dates, train_window, origin_step, start, min_h = 1L) {
  first <- max(train_window, which(dates >= start)[1])
  if (is.na(first) || first > length(dates) - min_h) return(integer(0))
  seq.int(first, length(dates) - min_h, by = origin_step)
}

# One row per origin and horizon. The fit at origin t sees y[(t - W + 1):t]
# only; the order is re-selected every `reselect_every` origins.
fc_rolling_forecasts <- function(y, dates, horizons = c(1L, 5L, 21L),
                                 train_window = 504L, origin_step = 5L,
                                 reselect_every = 13L, start = dates[1]) {
  origins <- fc_origins(dates, train_window, origin_step, start, min(horizons))
  H <- max(horizons)
  n <- length(y)
  spec <- NULL
  out <- vector("list", length(origins))
  for (k in seq_along(origins)) {
    t <- origins[k]
    train <- stats::ts(y[(t - train_window + 1):t], frequency = 1)
    reselect <- (k - 1) %% reselect_every == 0
    if (reselect) spec <- fc_select_spec(train)
    f <- forecast::forecast(fc_refit(train, spec), h = H, level = c(80, 95))
    ret <- diff(y[max(1, t - 21):t])
    hh <- horizons
    out[[k]] <- data.frame(
      origin = dates[t], h = hh,
      arima = as.numeric(f$mean)[hh],
      lo80 = f$lower[hh, 1], hi80 = f$upper[hh, 1],
      lo95 = f$lower[hh, 2], hi95 = f$upper[hh, 2],
      rw = y[t],
      actual = ifelse(t + hh <= n, y[pmin(t + hh, n)], NA_real_),
      vol21 = stats::sd(ret) * sqrt(TRADING_DAYS),
      reselected = reselect,
      random_walk = fc_is_random_walk(spec)
    )
  }
  rows <- do.call(rbind, out)
  rows[!is.na(rows$actual), , drop = FALSE]
}

# Diebold-Mariano on absolute errors (matches RelMAE). Origins are
# `origin_step` days apart, so h-day errors overlap across ceiling(h / step)
# origins; that sets the autocovariance lag. Identical errors give p = 1.
fc_dm_pvalue <- function(e_model, e_base, h, origin_step) {
  if (length(e_model) < 10 || isTRUE(all.equal(abs(e_model), abs(e_base)))) return(1)
  tryCatch(
    forecast::dm.test(e_model, e_base, alternative = "two.sided",
                      h = ceiling(h / origin_step), power = 1)$p.value,
    error = function(e) NA_real_
  )
}

fc_vol_tercile <- function(v) {
  q <- stats::quantile(v, c(1, 2) / 3, na.rm = TRUE, names = FALSE)
  factor(findInterval(v, q, rightmost.closed = TRUE) + 1L, levels = 1:3,
         labels = c("Low", "Mid", "High"))
}

fc_coverage <- function(rows) {
  c(cov80 = mean(rows$actual >= rows$lo80 & rows$actual <= rows$hi80),
    cov95 = mean(rows$actual >= rows$lo95 & rows$actual <= rows$hi95))
}

fc_summarize_ticker <- function(rows, origin_step) {
  rows$vol_tercile <- fc_vol_tercile(rows$vol21)
  do.call(rbind, lapply(split(rows, rows$h), function(r) {
    e_a <- r$actual - r$arima
    e_r <- r$actual - r$rw
    cv <- fc_coverage(r)
    by_vol <- vapply(split(r, r$vol_tercile), function(x) fc_coverage(x)[["cov95"]], numeric(1))
    data.frame(
      h = r$h[1], n = nrow(r),
      mae_arima = mean(abs(e_a)), mae_rw = mean(abs(e_r)),
      rmse_arima = sqrt(mean(e_a^2)), rmse_rw = sqrt(mean(e_r^2)),
      relmae = mean(abs(e_a)) / mean(abs(e_r)),
      dm_p = fc_dm_pvalue(e_a, e_r, r$h[1], origin_step),
      cov80 = cv[["cov80"]], cov95 = cv[["cov95"]],
      cov95_low_vol = by_vol[["Low"]], cov95_mid_vol = by_vol[["Mid"]],
      cov95_high_vol = by_vol[["High"]],
      rw_share = mean(r$random_walk[r$reselected])
    )
  }))
}

# Confidence tag for the app's forecast line.
fc_confidence <- function(relmae, p_bh) {
  if (is.na(relmae)) return("low")
  if (relmae < 0.95 && !is.na(p_bh) && p_bh < 0.05) "high"
  else if (relmae < 1) "medium"
  else "low"
}
