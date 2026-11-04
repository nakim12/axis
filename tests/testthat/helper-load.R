root <- normalizePath(file.path("..", ".."))

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

for (f in c("config.R", "eval_config.R", "metrics.R", "scenarios.R",
            "portfolio.R", "allocation.R", "backtest.R", "forecast_eval.R")) {
  suppressPackageStartupMessages(source(file.path(root, "R", f), local = FALSE))
}

# Ten daily returns with mean 0.003 and sum of squared deviations 0.00131.
fixture_r <- c(0.01, -0.02, 0.015, 0.005, -0.01, 0.02, 0, -0.005, 0.01, 0.005)

# Three assets: vols 10/20/30%, correlations 0.2 (1,2), 0.3 (1,3), 0.4 (2,3).
fixture_sigma <- local({
  v <- c(0.1, 0.2, 0.3)
  R <- matrix(c(1, 0.2, 0.3,
                0.2, 1, 0.4,
                0.3, 0.4, 1), 3, byrow = TRUE)
  S <- diag(v) %*% R %*% diag(v)
  dimnames(S) <- list(c("A", "B", "C"), c("A", "B", "C"))
  S
})

fixture_inputs <- function(mu = c(A = 0.04, B = 0.07, C = 0.10)) {
  list(mu = mu, Sigma = fixture_sigma, Sigma_lw = fixture_sigma, n_days = 252)
}

port_var <- function(w, S) as.numeric(t(w) %*% S %*% w)

# Long-format prices for three tickers built from known daily returns.
fixture_prices <- function() {
  rets <- list(
    A = c(0.01, -0.02, 0.015, 0.005, -0.01, 0.02),
    B = c(-0.005, 0.01, 0.0, 0.02, -0.015, 0.005),
    C = c(0.02, 0.01, -0.03, 0.01, 0.0, -0.01)
  )
  dates <- as.Date("2024-01-01") + 0:6
  bind_rows(lapply(names(rets), function(tk) {
    tibble(date = dates, ticker = tk, adjusted = 100 * cumprod(c(1, 1 + rets[[tk]])))
  }))
}
