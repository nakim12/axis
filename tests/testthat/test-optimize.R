methods <- unname(allocation_methods)

test_that("every method is long-only and fully invested", {
  for (m in methods) {
    w <- propose_allocation(m, fixture_inputs())
    expect_equal(sum(w), 1, tolerance = 1e-10, label = m)
    expect_true(all(w >= -1e-12), label = m)
    expect_named(w, c("A", "B", "C"))
  }
})

test_that("every method respects a 40% cap", {
  for (m in methods) {
    w <- propose_allocation(m, fixture_inputs(), max_w = 0.4)
    expect_true(all(w <= 0.4 + 1e-8), label = m)
    expect_equal(sum(w), 1, tolerance = 1e-10, label = m)
  }
})

test_that("a cap below 1/n is rejected", {
  for (m in methods) {
    expect_error(propose_allocation(m, fixture_inputs(), max_w = 0.3), "infeasible")
  }
})

test_that("min variance has no more variance than equal weight", {
  w_mv <- alloc_min_variance(fixture_sigma)
  w_eq <- alloc_equal_weight(colnames(fixture_sigma))
  expect_lte(port_var(w_mv, fixture_sigma), port_var(w_eq, fixture_sigma) + 1e-10)
})

test_that("two uncorrelated equal-volatility assets split 50/50", {
  S <- diag(c(0.04, 0.04), 2)
  dimnames(S) <- list(c("X", "Y"), c("X", "Y"))
  expect_equal(unname(alloc_min_variance(S)), c(0.5, 0.5), tolerance = 1e-8)
})

test_that("inverse volatility matches hand weights, with and without a cap", {
  # 1/vol = 10, 5, 3.333 -> shares 6/11, 3/11, 2/11.
  expect_equal(unname(alloc_inverse_volatility(fixture_sigma)), c(6, 3, 2) / 11)
  # Cap A at 0.4; B and C split the remaining 0.6 in a 3:2 ratio.
  expect_equal(unname(alloc_inverse_volatility(fixture_sigma, max_w = 0.4)),
               c(0.4, 0.36, 0.24))
})

test_that("max Sharpe matches the analytic tangency when it is long-only", {
  mu <- c(A = 0.04, B = 0.07, C = 0.10)
  tangency <- solve(fixture_sigma, mu)
  expect_true(all(tangency > 0))
  expect_equal(alloc_max_sharpe(fixture_sigma, mu),
               tangency / sum(tangency), tolerance = 1e-6)
})

# Every long-only mix on a 0.5% grid over the 3-asset simplex.
simplex_grid <- function(step = 0.005) {
  g <- expand.grid(A = seq(0, 1, step), B = seq(0, 1, step))
  g <- g[g$A + g$B <= 1 + 1e-12, ]
  cbind(A = g$A, B = g$B, C = 1 - g$A - g$B)
}

grid_best_sharpe <- function(mu, rf, max_w = 1) {
  g <- simplex_grid()
  g <- g[apply(g, 1, max) <= max_w + 1e-12, , drop = FALSE]
  ex <- as.numeric(g %*% (mu - rf))
  vol <- sqrt(rowSums((g %*% fixture_sigma) * g))
  max(ex / vol)
}

sharpe_of <- function(w, mu, rf) sum(w * (mu - rf)) / sqrt(port_var(w, fixture_sigma))

test_that("max Sharpe beats every long-only mix on a fine grid", {
  # B has a negative excess return, so the true optimum sits on the boundary.
  mu <- c(A = 0.05, B = 0.01, C = 0.09)
  rf <- 0.02
  w <- alloc_max_sharpe(fixture_sigma, mu, rf = rf)
  expect_equal(unname(w[["B"]]), 0, tolerance = 1e-8)
  expect_gte(sharpe_of(w, mu, rf), grid_best_sharpe(mu, rf) - 1e-9)
})

test_that("capped max Sharpe beats every capped mix on a fine grid", {
  mu <- c(A = 0.04, B = 0.07, C = 0.12)
  w <- alloc_max_sharpe(fixture_sigma, mu, max_w = 0.4)
  expect_true(all(w <= 0.4 + 1e-8))
  expect_gte(sharpe_of(w, mu, 0), grid_best_sharpe(mu, 0, max_w = 0.4) - 1e-9)
})

test_that("max Sharpe falls back to min variance when nothing beats cash", {
  mu <- c(A = 0.01, B = 0.015, C = 0.02)
  w <- alloc_max_sharpe(fixture_sigma, mu, rf = 0.03)
  expect_match(attr(w, "fallback"), "min variance")
  expect_equal(as.numeric(w), as.numeric(alloc_min_variance(fixture_sigma)))
})

# Literal transcription of Ledoit & Wolf (2004), Appendix B, with explicit loops.
ledoit_wolf_reference <- function(X) {
  t_n <- nrow(X); n <- ncol(X)
  Y <- sweep(X, 2, colMeans(X))
  S <- matrix(0, n, n)
  for (i in 1:n) for (j in 1:n) S[i, j] <- sum(Y[, i] * Y[, j]) / t_n
  r <- S / sqrt(outer(diag(S), diag(S)))
  rbar <- 0
  for (i in 1:(n - 1)) for (j in (i + 1):n) rbar <- rbar + r[i, j]
  rbar <- 2 * rbar / (n * (n - 1))
  F <- matrix(0, n, n)
  for (i in 1:n) for (j in 1:n) {
    F[i, j] <- if (i == j) S[i, i] else rbar * sqrt(S[i, i] * S[j, j])
  }
  pi_ij <- function(i, j) sum((Y[, i] * Y[, j] - S[i, j])^2) / t_n
  theta <- function(k, i, j) sum((Y[, k]^2 - S[k, k]) * (Y[, i] * Y[, j] - S[i, j])) / t_n
  pi_hat <- 0
  for (i in 1:n) for (j in 1:n) pi_hat <- pi_hat + pi_ij(i, j)
  rho_hat <- 0
  for (i in 1:n) rho_hat <- rho_hat + pi_ij(i, i)
  for (i in 1:n) for (j in 1:n) if (i != j) {
    rho_hat <- rho_hat + rbar / 2 * (sqrt(S[j, j] / S[i, i]) * theta(i, i, j) +
                                     sqrt(S[i, i] / S[j, j]) * theta(j, i, j))
  }
  gamma_hat <- sum((F - S)^2)
  delta <- max(0, min(1, (pi_hat - rho_hat) / gamma_hat / t_n))
  list(cov = delta * F + (1 - delta) * S, delta = delta)
}

test_that("Ledoit-Wolf matches a loop-by-loop transcription of the paper", {
  set.seed(42)
  X <- matrix(rnorm(80 * 4, sd = 0.01), 80, 4) %*% chol(matrix(0.3, 4, 4) + diag(0.7, 4))
  colnames(X) <- paste0("x", 1:4)
  ref <- ledoit_wolf_reference(X)
  got <- ledoit_wolf_cov(X)
  expect_equal(attr(got, "shrinkage"), ref$delta, tolerance = 1e-10)
  expect_equal(unname(unclass(got)[, ]), ref$cov, tolerance = 1e-12, ignore_attr = TRUE)
})

test_that("Ledoit-Wolf shrinkage is in [0, 1], positive definite, and fades with more data", {
  set.seed(7)
  sim <- function(t_n) {
    X <- matrix(rnorm(t_n * 6, sd = 0.01), t_n, 6)
    colnames(X) <- paste0("x", 1:6)
    ledoit_wolf_cov(X)
  }
  short <- sim(40)
  long  <- sim(4000)
  for (S in list(short, long)) {
    d <- attr(S, "shrinkage")
    expect_gte(d, 0)
    expect_lte(d, 1)
    expect_gt(min(eigen(unclass(S), symmetric = TRUE)$values), 0)
  }
  expect_lt(attr(long, "shrinkage"), attr(short, "shrinkage"))
})
