# Long-only, fully invested model portfolios. Every method accepts a per-name
# cap `max_w` in (0, 1]; 1 means no cap. A cap below 1/n is infeasible.

regularize_cov <- function(Sigma) {
  S <- (Sigma + t(Sigma)) / 2
  n <- nrow(S)
  ev <- eigen(S, symmetric = TRUE)$values
  eps <- max(1e-10, 1e-4 * max(abs(ev), 1e-12))
  if (length(ev) && min(ev) < eps) {
    S <- S + diag(n) * (eps - min(ev))
  }
  S
}

.asset_names <- function(Sigma) {
  nm <- colnames(Sigma)
  if (is.null(nm)) nm <- rownames(Sigma)
  if (is.null(nm)) nm <- paste0("x", seq_len(nrow(Sigma)))
  nm
}

check_weight_cap <- function(max_w, n) {
  if (!is.finite(max_w) || max_w <= 0 || max_w > 1) {
    stop("max_w must be in (0, 1].", call. = FALSE)
  }
  if (max_w * n < 1 - 1e-9) {
    stop(sprintf("A %.1f%% cap is infeasible for %d names; it must be at least %.1f%%.",
                 100 * max_w, n, 100 / n), call. = FALSE)
  }
  invisible(TRUE)
}

# Scale to the cap and hand the excess to uncapped names in proportion to
# their base weights, repeating until nothing exceeds the cap.
cap_weights <- function(w, max_w = 1) {
  check_weight_cap(max_w, length(w))
  w <- w / sum(w)
  if (max_w >= 1 - 1e-12) return(w)
  capped <- rep(FALSE, length(w))
  repeat {
    free <- !capped
    room <- 1 - max_w * sum(capped)
    w[free] <- w[free] / sum(w[free]) * room
    w[capped] <- max_w
    over <- free & w > max_w + 1e-12
    if (!any(over)) break
    capped <- capped | over
  }
  w
}

alloc_equal_weight <- function(tickers, max_w = 1) {
  n <- length(tickers)
  check_weight_cap(max_w, n)
  stats::setNames(rep(1 / n, n), tickers)
}

alloc_inverse_volatility <- function(Sigma, max_w = 1) {
  v <- sqrt(pmax(diag(Sigma), 1e-16))
  stats::setNames(cap_weights(1 / v, max_w), .asset_names(Sigma))
}

alloc_min_variance <- function(Sigma, max_w = 1) {
  n  <- nrow(Sigma)
  nm <- .asset_names(Sigma)
  check_weight_cap(max_w, n)
  if (n <= 1L) return(stats::setNames(1, nm[[1]]))

  S <- regularize_cov(Sigma)
  Amat <- cbind(matrix(1, n, 1), diag(n))
  bvec <- c(1, rep(0, n))
  if (max_w < 1 - 1e-8) {
    Amat <- cbind(Amat, -diag(n))
    bvec <- c(bvec, rep(-max_w, n))
  }

  fit <- tryCatch(
    quadprog::solve.QP(2 * S, rep(0, n), Amat, bvec, meq = 1L),
    error = function(e) NULL
  )
  if (is.null(fit)) {
    w <- rep(1 / n, n)
    attr(w, "fallback") <- "equal weight (min-variance solver failed)"
  } else {
    w <- pmax(fit$solution, 0)
    w <- w / sum(w)
  }
  out <- stats::setNames(as.numeric(w), nm)
  attr(out, "fallback") <- attr(w, "fallback")
  out
}

# Long-only max Sharpe via the homogeneous reformulation: minimize y' S y
# subject to (mu - rf)' y = 1 and y >= 0, then w = y / sum(y). The cap becomes
# max_w * sum(y) - y_i >= 0. Infeasible when no capped mix beats rf, in which
# case this falls back to min variance with the same cap.
alloc_max_sharpe <- function(Sigma, mu, rf = 0, max_w = 1) {
  n  <- nrow(Sigma)
  nm <- .asset_names(Sigma)
  check_weight_cap(max_w, n)
  if (n <= 1L) return(stats::setNames(1, nm[[1]]))

  excess <- as.numeric(mu[nm]) - rf
  fallback <- function(reason) {
    w <- alloc_min_variance(Sigma, max_w)
    attr(w, "fallback") <- paste0("min variance (", reason, ")")
    w
  }
  if (!any(excess > 0)) return(fallback("no asset beats the risk-free rate"))

  S <- regularize_cov(Sigma)
  Amat <- cbind(excess, diag(n))
  bvec <- c(1, rep(0, n))
  if (max_w < 1 - 1e-8) {
    Amat <- cbind(Amat, max_w * matrix(1, n, n) - diag(n))
    bvec <- c(bvec, rep(0, n))
  }

  fit <- tryCatch(
    quadprog::solve.QP(2 * S, rep(0, n), Amat, bvec, meq = 1L),
    error = function(e) NULL
  )
  if (is.null(fit)) return(fallback("no capped mix beats the risk-free rate"))
  y <- pmax(fit$solution, 0)
  if (sum(y) < 1e-12) return(fallback("solver returned zero weights"))
  stats::setNames(y / sum(y), nm)
}

# Ledoit & Wolf (2004), "Honey, I shrunk the sample covariance matrix":
# shrink toward a constant-correlation target with the closed-form intensity.
# X is a T x N matrix of returns. Returns the shrunk covariance (same units as
# X) with the intensity in attr(, "shrinkage").
ledoit_wolf_cov <- function(X) {
  X <- as.matrix(X)
  t_n <- nrow(X)
  n   <- ncol(X)
  Y <- sweep(X, 2, colMeans(X))
  S <- crossprod(Y) / t_n
  if (n == 1L) {
    attr(S, "shrinkage") <- 0
    return(S)
  }

  s_d  <- sqrt(diag(S))
  corr <- S / outer(s_d, s_d)
  rbar <- (sum(corr) - n) / (n * (n - 1))
  F <- rbar * outer(s_d, s_d)
  diag(F) <- diag(S)

  pi_mat <- crossprod(Y^2) / t_n - S^2
  pi_hat <- sum(pi_mat)

  theta <- crossprod(Y^3, Y) / t_n - diag(S) * S
  ratio <- outer(1 / s_d, s_d)
  off   <- row(theta) != col(theta)
  rho_hat <- sum(diag(pi_mat)) + rbar * sum((ratio * theta)[off])

  gamma_hat <- sum((F - S)^2)
  delta <- if (gamma_hat > 0) {
    max(0, min(1, (pi_hat - rho_hat) / gamma_hat / t_n))
  } else 1

  out <- delta * F + (1 - delta) * S
  dimnames(out) <- list(colnames(X), colnames(X))
  attr(out, "shrinkage") <- delta
  out
}

returns_matrix_for <- function(price_data, tickers) {
  tickers <- unique(tickers)
  pd <- price_data %>% filter(ticker %in% tickers)
  rw <- returns_wide_from_price_data(pd)
  nm <- intersect(tickers, setdiff(names(rw), "date"))
  if (length(nm) < 1L) return(NULL)
  rw <- rw %>% select(date, dplyr::all_of(nm)) %>% drop_na()
  if (nrow(rw) < 5L) return(NULL)
  as.matrix(rw[, nm, drop = FALSE])
}

estimate_inputs <- function(X) {
  nm <- colnames(X)
  mu <- stats::setNames(colMeans(X) * TRADING_DAYS, nm)
  Sigma <- stats::cov(X) * TRADING_DAYS
  dimnames(Sigma) <- list(nm, nm)
  Sigma_lw <- ledoit_wolf_cov(X) * TRADING_DAYS
  list(mu = mu, Sigma = Sigma, Sigma_lw = Sigma_lw, n_days = nrow(X))
}

compute_mu_sigma_annual <- function(price_data, tickers) {
  X <- returns_matrix_for(price_data, tickers)
  if (is.null(X)) return(NULL)
  estimate_inputs(X)
}

align_weights <- function(w, tickers) {
  w <- w[tickers]
  w[is.na(w)] <- 0
  w / sum(w)
}

portfolio_ann_metrics <- function(w, mu, Sigma) {
  nm <- names(w)
  w <- as.numeric(w)
  mu_v <- as.numeric(mu[nm])
  Sig <- Sigma[nm, nm, drop = FALSE]
  er <- sum(w * mu_v)
  vol <- sqrt(max(0, as.numeric(t(w) %*% Sig %*% w)))
  sharpe <- if (vol > 1e-12) er / vol else NA_real_
  list(ann_return = er, ann_vol = vol, sharpe = sharpe)
}

allocation_methods <- c(
  "Min variance"                  = "minvar",
  "Min variance (shrunk covariance)" = "minvar_lw",
  "Inverse volatility"            = "invvol",
  "Equal weight"                  = "equal",
  "Max Sharpe"                    = "maxsharpe"
)

# `inputs` is the list from estimate_inputs(). rf is annual.
propose_allocation <- function(method, inputs, max_w = 1, rf = 0) {
  Sigma   <- inputs$Sigma
  tickers <- .asset_names(Sigma)
  switch(
    method,
    minvar    = alloc_min_variance(Sigma, max_w),
    minvar_lw = alloc_min_variance(inputs$Sigma_lw, max_w),
    invvol    = alloc_inverse_volatility(Sigma, max_w),
    equal     = alloc_equal_weight(tickers, max_w),
    maxsharpe = alloc_max_sharpe(Sigma, inputs$mu, rf = rf, max_w = max_w),
    stop("Unknown allocation method: ", method, call. = FALSE)
  )
}
