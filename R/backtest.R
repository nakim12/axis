# Walk-forward backtest. At each month-end rebalance date t, weights are
# estimated from the `lookback` daily returns ending at t (nothing after t),
# traded at t's close, and earn returns from t + 1. Weights drift with prices
# between rebalances. Costs are charged on rebalance days as turnover x rate,
# where turnover is half the sum of absolute weight changes; the initial
# purchase is not charged.

# Wide matrix of daily simple returns (rows = dates, cols = tickers) from long
# prices. Days before a ticker lists are NA.
returns_matrix_from_prices <- function(prices, tickers) {
  p <- prices[prices$ticker %in% tickers, c("date", "ticker", "adjusted")]
  dates <- sort(unique(p$date))
  P <- matrix(NA_real_, length(dates), length(tickers),
              dimnames = list(as.character(dates), tickers))
  P[cbind(match(p$date, dates), match(p$ticker, tickers))] <- p$adjusted
  R <- P[-1, , drop = FALSE] / P[-nrow(P), , drop = FALSE] - 1
  list(dates = dates[-1], R = R)
}

# Row indices of the last trading day of each month that has `lookback` rows
# of history and at least one day after it.
bt_rebalance_index <- function(dates, lookback) {
  ym <- format(dates, "%Y-%m")
  i <- which(ym[-length(ym)] != ym[-1])
  i[i >= lookback]
}

# Tickers with a complete return history over the window.
bt_eligible <- function(window) colnames(window)[colSums(is.na(window)) == 0]

bt_fixed_weights <- function(target, eligible) {
  w <- target[intersect(names(target), eligible)]
  if (!length(w) || sum(w) <= 0) return(NULL)
  w / sum(w)
}

# One strategy's target weights at a rebalance, over the full ticker set.
bt_target <- function(strategy, window, rf_window, max_w, tickers, universe = tickers) {
  eligible <- intersect(bt_eligible(window), universe)
  out <- stats::setNames(rep(0, length(tickers)), tickers)
  note <- NULL

  if (!is.null(strategy$weights)) {
    w <- bt_fixed_weights(strategy$weights, bt_eligible(window))
    if (is.null(w)) return(list(w = NULL, note = "no eligible holdings"))
    out[names(w)] <- w
    return(list(w = out, note = NULL))
  }

  n <- length(eligible)
  if (n == 0) return(list(w = NULL, note = "no eligible tickers"))
  cap <- max(max_w, 1 / n)
  if (cap > max_w + 1e-12) note <- sprintf("cap raised to %.1f%% (%d eligible)", 100 * cap, n)

  if (n == 1) {
    out[eligible] <- 1
    return(list(w = out, note = note))
  }
  inputs <- estimate_inputs(window[, eligible, drop = FALSE])
  rf_ann <- mean(rf_window) * TRADING_DAYS
  w <- propose_allocation(strategy$method, inputs, max_w = cap, rf = rf_ann)
  fb <- attr(w, "fallback")
  out[names(w)] <- as.numeric(w)
  list(w = out, note = c(note, if (!is.null(fb)) paste("fallback:", fb)))
}

# strategies: named list; each element is list(method = "<allocation id>") or
# list(weights = c(TICKER = w, ...), costs = TRUE/FALSE) for fixed targets.
# rf: daily risk-free rate aligned with rows of R (scalar 0 is fine).
# universe: the columns model strategies may hold (benchmarks can use others).
run_walk_forward <- function(R, dates, strategies, rf = 0,
                             lookback = 252L, cost_bps = 10, max_w = 1,
                             universe = colnames(R)) {
  stopifnot(nrow(R) == length(dates))
  rf <- rep_len(rf, nrow(R))
  rf[is.na(rf)] <- 0
  tickers <- colnames(R)
  rb <- bt_rebalance_index(dates, lookback)
  if (!length(rb)) stop("Not enough history for a single rebalance.", call. = FALSE)
  start <- rb[1]
  days  <- (start + 1):nrow(R)
  cost_rate <- cost_bps / 1e4

  # Targets depend only on rows up to each rebalance date.
  targets <- lapply(strategies, function(s) {
    lapply(rb, function(i) {
      rows <- (i - lookback + 1):i
      bt_target(s, R[rows, , drop = FALSE], rf[rows], max_w, tickers, universe)
    })
  })

  Rz <- R
  Rz[is.na(Rz)] <- 0

  results <- lapply(names(strategies), function(nm) {
    s <- strategies[[nm]]
    charge <- !identical(s$costs, FALSE)
    tg <- targets[[nm]]
    w <- tg[[1]]$w
    gross <- net <- numeric(length(days))
    turnover <- numeric(length(rb))
    W <- matrix(NA_real_, length(rb), length(tickers), dimnames = list(NULL, tickers))
    W[1, ] <- w
    k <- 1L
    for (j in seq_along(days)) {
      t <- days[j]
      r <- Rz[t, ]
      rp <- sum(w * r)
      gross[j] <- rp
      w <- w * (1 + r) / (1 + rp)
      if (k < length(rb) && t == rb[k + 1L]) {
        k <- k + 1L
        new_w <- tg[[k]]$w
        if (!is.null(new_w)) {
          turnover[k] <- 0.5 * sum(abs(new_w - w))
          w <- new_w
        }
        W[k, ] <- w
      }
      net[j] <- gross[j] - if (charge && t == rb[k] && k > 1L) turnover[k] * cost_rate else 0
    }
    notes <- unlist(lapply(seq_along(tg), function(i) {
      if (length(tg[[i]]$note)) paste0(dates[rb[i]], ": ", tg[[i]]$note)
    }))
    list(
      returns  = data.frame(date = dates[days], gross = gross, net = net),
      weights  = data.frame(date = dates[rb], W, check.names = FALSE),
      turnover = data.frame(date = dates[rb], turnover = turnover),
      notes    = notes
    )
  })
  names(results) <- names(strategies)

  list(
    strategies = results,
    rf         = rf[days],
    dates      = dates[days],
    rebalances = dates[rb],
    params     = list(lookback = lookback, cost_bps = cost_bps, max_w = max_w)
  )
}

bt_summary <- function(bt) {
  rf <- bt$rf
  years <- length(bt$dates) / TRADING_DAYS
  rows <- lapply(names(bt$strategies), function(nm) {
    s <- bt$strategies[[nm]]
    fallbacks <- sum(grepl("fallback:", s$notes))
    data.frame(
      strategy        = nm,
      cagr            = cagr(s$returns$net),
      volatility      = ann_vol(s$returns$net),
      sharpe          = sharpe_ratio(s$returns$net, rf),
      sharpe_gross    = sharpe_ratio(s$returns$gross, rf),
      sortino         = sortino_ratio(s$returns$net, rf),
      max_drawdown    = max_drawdown(s$returns$net),
      turnover_annual = sum(s$turnover$turnover) / years,
      fallbacks       = fallbacks,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

# Politis & Romano (1994) stationary bootstrap: blocks start at random days and
# have geometric lengths with the given mean, wrapping around the end.
stationary_bootstrap_index <- function(n, mean_block = 21) {
  m <- ceiling(2 * n / mean_block) + 10L
  repeat {
    lens <- stats::rgeom(m, 1 / mean_block) + 1L
    if (sum(lens) >= n) break
    m <- 2L * m
  }
  starts <- sample.int(n, m, replace = TRUE)
  idx <- (rep(starts, lens) - 1L + sequence(lens) - 1L) %% n + 1L
  idx[seq_len(n)]
}

# 95% intervals for each strategy's Sharpe and for its difference from
# `baseline`, using the same resampled days for every strategy.
bt_bootstrap_sharpe <- function(bt, baseline = "Equal weight", n_boot = 2000L,
                                mean_block = 21, seed = 20261103L) {
  X <- sapply(bt$strategies, function(s) s$returns$net - bt$rf)
  n <- nrow(X)
  sh <- function(M) sqrt(TRADING_DAYS) * colMeans(M) / apply(M, 2, stats::sd)
  point <- sh(X)
  old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
  on.exit(if (is.null(old_seed)) rm(".Random.seed", envir = globalenv())
          else assign(".Random.seed", old_seed, envir = globalenv()))
  set.seed(seed)
  draws <- t(vapply(seq_len(n_boot), function(b) {
    sh(X[stationary_bootstrap_index(n, mean_block), , drop = FALSE])
  }, numeric(ncol(X))))
  colnames(draws) <- colnames(X)
  ci <- function(v) stats::quantile(v, c(0.025, 0.975), names = FALSE)
  out <- data.frame(
    strategy  = colnames(X),
    sharpe    = point,
    sharpe_lo = apply(draws, 2, ci)[1, ],
    sharpe_hi = apply(draws, 2, ci)[2, ],
    stringsAsFactors = FALSE
  )
  if (baseline %in% colnames(X)) {
    d <- draws - draws[, baseline]
    out$diff    <- point - point[[baseline]]
    out$diff_lo <- apply(d, 2, ci)[1, ]
    out$diff_hi <- apply(d, 2, ci)[2, ]
  }
  rownames(out) <- NULL
  out
}

# Max Sharpe fitted once on the whole out-of-sample period (in-sample, with
# hindsight), held at those weights and rebalanced monthly without costs.
bt_in_sample_max_sharpe <- function(R, dates, bt, max_w, universe = colnames(R)) {
  rows <- match(bt$dates, dates)
  window <- R[rows, , drop = FALSE]
  eligible <- intersect(bt_eligible(window), universe)
  inputs <- estimate_inputs(window[, eligible, drop = FALSE])
  rf_ann <- mean(bt$rf) * TRADING_DAYS
  w <- propose_allocation("maxsharpe", inputs, max_w = max(max_w, 1 / length(eligible)), rf = rf_ann)
  fixed <- run_walk_forward(R, dates, list(fit = list(weights = w, costs = FALSE)),
                            rf = 0, lookback = bt$params$lookback, cost_bps = 0)
  r <- fixed$strategies$fit$returns$gross
  list(weights = w, sharpe = sharpe_ratio(r, bt$rf))
}
