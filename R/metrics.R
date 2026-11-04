# Return and risk metrics on daily simple returns.
# `rf` is a daily risk-free rate: a scalar or a vector aligned with `r`.
# Ratios return NA (not Inf or an error) when their denominator is zero.

TRADING_DAYS <- 252

.finite_pairs <- function(r, rf = 0) {
  rf <- rep_len(rf, length(r))
  ok <- is.finite(r) & is.finite(rf)
  list(r = r[ok], rf = rf[ok])
}

ann_return <- function(r) {
  r <- r[is.finite(r)]
  if (!length(r)) return(NA_real_)
  mean(r) * TRADING_DAYS
}

cagr <- function(r) {
  r <- r[is.finite(r)]
  if (!length(r)) return(NA_real_)
  prod(1 + r)^(TRADING_DAYS / length(r)) - 1
}

ann_vol <- function(r) {
  r <- r[is.finite(r)]
  if (length(r) < 2L) return(NA_real_)
  stats::sd(r) * sqrt(TRADING_DAYS)
}

sharpe_ratio <- function(r, rf = 0) {
  p <- .finite_pairs(r, rf)
  x <- p$r - p$rf
  if (length(x) < 2L) return(NA_real_)
  s <- stats::sd(x)
  if (!is.finite(s) || s < 1e-15) return(NA_real_)
  sqrt(TRADING_DAYS) * mean(x) / s
}

# Downside deviation uses every day, with gains counted as zero.
sortino_ratio <- function(r, rf = 0) {
  p <- .finite_pairs(r, rf)
  x <- p$r - p$rf
  if (!length(x)) return(NA_real_)
  dd <- sqrt(mean(pmin(x, 0)^2))
  if (dd < 1e-15) return(NA_real_)
  sqrt(TRADING_DAYS) * mean(x) / dd
}

# Wealth starts at 1 before the first return, so a first-day loss counts.
drawdown_path <- function(r) {
  r[!is.finite(r)] <- 0
  w <- cumprod(1 + r)
  w / pmax(cummax(w), 1) - 1
}

max_drawdown <- function(r) {
  if (!length(r)) return(NA_real_)
  min(0, drawdown_path(r))
}

beta_to <- function(r, b) {
  ok <- is.finite(r) & is.finite(b)
  if (sum(ok) < 2L) return(NA_real_)
  vb <- stats::var(b[ok])
  if (vb < 1e-20) return(NA_real_)
  stats::cov(r[ok], b[ok]) / vb
}

tracking_error <- function(r, b) {
  ok <- is.finite(r) & is.finite(b)
  if (sum(ok) < 2L) return(NA_real_)
  stats::sd(r[ok] - b[ok]) * sqrt(TRADING_DAYS)
}

# ^IRX quotes the 13-week T-bill yield in annual percent. Missing days carry
# the previous value forward.
daily_riskfree <- function(dates, riskfree_tbl) {
  rf <- riskfree_tbl[order(riskfree_tbl$date), ]
  idx <- findInterval(as.numeric(dates), as.numeric(rf$date))
  out <- rep(NA_real_, length(dates))
  out[idx > 0] <- rf$irx_pct[idx[idx > 0]] / 100 / TRADING_DAYS
  out
}
