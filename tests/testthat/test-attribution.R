weights <- c(A = 0.5, B = 0.3, C = 0.2)

test_that("risk contributions add up to portfolio volatility", {
  rc <- compute_risk_contribution(fixture_prices(), weights)
  X <- returns_matrix_for(fixture_prices(), c("A", "B", "C"))
  w <- weights[colnames(X)]
  vol <- sqrt(port_var(w, stats::cov(X))) * sqrt(252)
  expect_equal(sum(rc$pct_contribution), 1)
  expect_equal(sum(rc$component_ctr), vol)
})

test_that("holding return contributions add up to the portfolio return", {
  ra <- compute_return_attribution(fixture_prices(), weights)
  pr <- compute_portfolio_returns(fixture_prices(), weights)
  expect_equal(sum(ra$linear_contribution), sum(pr$portfolio_return))
  expect_equal(sum(ra$linear_share), 1)
})

test_that("portfolio returns are the weighted sum of holding returns", {
  pr <- compute_portfolio_returns(fixture_prices(), weights)
  # Day 1: 0.5 * 0.01 + 0.3 * -0.005 + 0.2 * 0.02
  expect_equal(pr$portfolio_return[1], 0.0075)
})
