# Historical windows and simple additive return shocks for the Stress page.

stress_presets <- list(
  custom = NULL,
  covid  = list(
    label = "COVID crash (Feb 19 – Mar 23, 2020)",
    start = as.Date("2020-02-19"),
    end   = as.Date("2020-03-23")
  ),
  y2022 = list(
    label = "2022 sell-off (Jan – Sep 2022)",
    start = as.Date("2022-01-01"),
    end   = as.Date("2022-09-30")
  ),
  q4_2018 = list(
    label = "Q4 2018 drawdown",
    start = as.Date("2018-10-01"),
    end   = as.Date("2018-12-31")
  ),
  recovery_2020 = list(
    label = "Post-COVID rally (Apr – Dec 2020)",
    start = as.Date("2020-04-01"),
    end   = as.Date("2020-12-31")
  )
)

stress_preset_choices <- function() {
  nm <- names(stress_presets)
  labs <- character(length(nm))
  for (i in seq_along(nm)) {
    x <- stress_presets[[nm[i]]]
    labs[i] <- if (is.null(x)) "Custom date range" else x$label
  }
  stats::setNames(nm, labs)
}

returns_wide_from_price_data <- function(price_data) {
  price_data %>%
    arrange(ticker, date) %>%
    group_by(ticker) %>%
    mutate(daily_return = (adjusted / dplyr::lag(adjusted)) - 1) %>%
    filter(!is.na(daily_return)) %>%
    select(date, ticker, daily_return) %>%
    pivot_wider(names_from = ticker, values_from = daily_return) %>%
    arrange(date) %>%
    drop_na()
}

portfolio_returns_from_wide <- function(returns_wide, weights) {
  nm <- setdiff(names(returns_wide), "date")
  available <- intersect(names(weights), nm)
  if (length(available) == 0) return(NULL)

  w <- weights[available]
  w <- w / sum(w)

  X <- as.matrix(returns_wide[, available, drop = FALSE])
  tibble(
    date              = returns_wide$date,
    portfolio_return  = as.numeric(X %*% w),
    cumulative_return = cumprod(1 + as.numeric(X %*% w)) - 1
  )
}

slice_returns_by_date <- function(tbl, start, end) {
  if (is.null(tbl) || nrow(tbl) == 0) return(NULL)
  tbl %>% filter(date >= start, date <= end) %>% arrange(date)
}

rebase_cumulative_from_slice <- function(port_slice) {
  if (is.null(port_slice) || nrow(port_slice) == 0) return(NULL)
  r <- port_slice$portfolio_return
  tibble(
    date              = port_slice$date,
    portfolio_return  = r,
    cumulative_return = cumprod(1 + r) - 1
  )
}

summarize_return_slice <- function(port_slice) {
  if (is.null(port_slice) || nrow(port_slice) < 2) {
    return(NULL)
  }
  r   <- port_slice$portfolio_return
  cum <- cumprod(1 + r)
  peak <- cummax(cum)
  dd   <- (cum - peak) / peak
  list(
    n_days       = nrow(port_slice),
    total_return = as.numeric(cum[length(cum)] / cum[1] - 1),
    ann_vol      = sd(r, na.rm = TRUE) * sqrt(252),
    max_dd       = min(dd),
    worst_day    = min(r, na.rm = TRUE),
    best_day     = max(r, na.rm = TRUE)
  )
}

apply_daily_return_shock_wide <- function(returns_wide, tickers, shock) {
  rw <- returns_wide
  for (t in tickers) {
    if (t %in% names(rw)) rw[[t]] <- rw[[t]] + shock
  }
  rw
}

holdings_tickers_in_sector <- function(holding_tickers, sector_name) {
  if (identical(sector_name, "__all__")) return(holding_tickers)
  st <- stock_sectors[[sector_name]]
  intersect(holding_tickers, st)
}

# Bootstrap resample of portfolio daily returns (fixed weights); descriptive fan only.
bootstrap_fan_from_returns <- function(r_daily, horizon, n_sims, seed = 1L) {
  r <- as.numeric(r_daily)
  r <- r[is.finite(r)]
  if (length(r) < 10L) return(NULL)
  h <- as.integer(horizon)
  ns <- as.integer(n_sims)
  if (is.na(h) || h < 1L) return(NULL)
  if (is.na(ns) || ns < 20L) return(NULL)
  set.seed(as.integer(seed))
  out <- matrix(0, nrow = ns, ncol = h + 1L)
  for (i in seq_len(ns)) {
    idx <- sample.int(length(r), h, replace = TRUE)
    rd <- r[idx]
    out[i, ] <- cumprod(c(1, 1 + rd)) - 1
  }
  days <- 0:h
  qs <- apply(out, 2, stats::quantile,
    probs = c(0.05, 0.5, 0.95), na.rm = TRUE, type = 7)
  n_show <- min(80L, ns)
  list(
    days = days,
    p05 = as.numeric(qs[1, ]),
    p50 = as.numeric(qs[2, ]),
    p95 = as.numeric(qs[3, ]),
    sample_matrix = out[seq_len(n_show), , drop = FALSE]
  )
}
