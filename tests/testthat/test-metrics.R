test_that("annual return and volatility match hand calculations", {
  expect_equal(ann_return(fixture_r), 0.003 * 252)
  expect_equal(ann_vol(fixture_r), sqrt(0.00131 / 9) * sqrt(252))
})

test_that("CAGR compounds to an annual rate", {
  expect_equal(cagr(c(rep(0, 251), 0.10)), 0.10)
  expect_equal(cagr(rep(0.001, 504)), 1.001^252 - 1)
})

test_that("Sharpe is sqrt(252) * mean excess / sd excess", {
  expect_equal(sharpe_ratio(fixture_r), sqrt(252) * 0.003 / sqrt(0.00131 / 9))
  # A constant risk-free rate shifts the mean but not the sd.
  expect_equal(sharpe_ratio(fixture_r, rf = 0.001),
               sqrt(252) * 0.002 / sqrt(0.00131 / 9))
})

test_that("Sharpe is NA, not Inf, for a constant series", {
  expect_true(is.na(sharpe_ratio(rep(0.001, 20))))
})

test_that("Sortino uses downside deviation over all days", {
  # Losing days -0.02, -0.01, -0.005: squares sum to 0.000525 over 10 days.
  expect_equal(sortino_ratio(fixture_r), sqrt(252) * 0.003 / sqrt(0.000525 / 10))
})

test_that("Sortino is NA when there are no losing days", {
  expect_true(is.na(sortino_ratio(c(0.01, 0.02, 0, 0.005))))
})

test_that("max drawdown measures the fall from the running peak", {
  expect_equal(max_drawdown(c(0.25, -0.20, 0.10)), -0.20)
  expect_equal(max_drawdown(c(0.10, 0.10, -0.5, 0.5, -0.5)), -0.625)
  expect_equal(max_drawdown(c(0.01, 0.02)), 0)
})

test_that("a first-day loss counts as drawdown", {
  expect_equal(max_drawdown(c(-0.10, 0.05)), -0.10)
  expect_equal(drawdown_path(c(-0.10, 0.05)), c(-0.10, 0.9 * 1.05 - 1))
})

test_that("beta and tracking error behave at the known extremes", {
  b <- c(0.01, -0.02, 0.015, 0.005, -0.01, 0.02, -0.003)
  expect_equal(beta_to(b, b), 1)
  expect_equal(beta_to(2 * b, b), 2)
  expect_equal(beta_to(b + 0.001, b), 1)
  expect_equal(tracking_error(b, b), 0)
  expect_true(is.na(beta_to(b, rep(0.001, length(b)))))
})

test_that("daily risk-free converts percent yields and fills gaps forward", {
  rf <- tibble(date = as.Date(c("2024-01-02", "2024-01-04")), irx_pct = c(5.04, 2.52))
  out <- daily_riskfree(as.Date(c("2024-01-01", "2024-01-02", "2024-01-03", "2024-01-04")), rf)
  expect_equal(out, c(NA, 0.0504 / 252, 0.0504 / 252, 0.0252 / 252))
})

test_that("compute_portfolio_metrics uses the shared definitions", {
  # Benchmark statistics need more than 10 overlapping days.
  r <- rep(fixture_r, 2)
  dates <- as.Date("2024-01-01") + seq_along(r)
  pr <- tibble(date = dates, portfolio_return = r)
  br <- tibble(date = dates, portfolio_return = r / 2)
  m <- compute_portfolio_metrics(pr, br)
  expect_equal(m$sharpe_ratio, sharpe_ratio(r))
  expect_equal(m$sortino_ratio, sortino_ratio(r))
  expect_equal(m$max_drawdown, max_drawdown(r))
  expect_equal(m$beta, 2)
  expect_equal(m$tracking_error, ann_vol(r / 2))
})

test_that("stress episode totals include the first day", {
  s <- summarize_return_slice(tibble(date = as.Date("2020-02-19") + 0:1,
                                     portfolio_return = c(-0.10, 0.10)))
  expect_equal(s$total_return, 0.9 * 1.1 - 1)
  expect_equal(s$max_dd, -0.10)
})
