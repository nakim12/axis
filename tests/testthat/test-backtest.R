# Six days across three months, two assets. With a 2-day lookback the
# rebalances are Jan 31 (row 2) and Feb 29 (row 5); Mar 1 is the last row, so
# it can't be a rebalance.
hand_dates <- as.Date(c("2024-01-30", "2024-01-31", "2024-02-01",
                        "2024-02-02", "2024-02-29", "2024-03-01"))
hand_R <- cbind(
  A = c(0.01, -0.01, 0.10, 0.00, 0.10, 0.02),
  B = c(-0.01, 0.01, 0.00, 0.10, -0.10, 0.04)
)

test_that("rebalances fall on month ends with enough history", {
  expect_equal(bt_rebalance_index(hand_dates, lookback = 2), c(2L, 5L))
  expect_equal(bt_rebalance_index(hand_dates, lookback = 3), 5L)
})

test_that("equal weight matches a hand-computed path with drift and costs", {
  bt <- run_walk_forward(hand_R, hand_dates, list(eq = list(method = "equal")),
                         lookback = 2, cost_bps = 10)
  s <- bt$strategies$eq
  # Day 3: 50/50 earns 5%; weights drift to 0.55/1.05 and 0.50/1.05.
  # Day 4: B gains 10% on 0.50/1.05, i.e. 1/21; weights drift back to 50/50.
  # Day 5: +10% / -10% nets to 0; weights drift to 55/45, and rebalancing to
  #        50/50 is 5% turnover, costing 0.05 x 10 bps.
  # Day 6: 50/50 earns the average of 2% and 4%.
  expect_equal(s$returns$gross, c(0.05, 1 / 21, 0, 0.03))
  expect_equal(s$returns$net,   c(0.05, 1 / 21, -0.05 * 0.001, 0.03))
  expect_equal(s$turnover$turnover, c(0, 0.05))
  expect_equal(bt$dates, hand_dates[3:6])
})

test_that("costs can be switched off for a fixed benchmark", {
  bt <- run_walk_forward(hand_R, hand_dates,
                         list(bench = list(weights = c(A = 0.5, B = 0.5), costs = FALSE)),
                         lookback = 2, cost_bps = 10)
  expect_equal(bt$strategies$bench$returns$net, c(0.05, 1 / 21, 0, 0.03))
})

sim_returns <- function(n = 900, k = 4, seed = 11) {
  set.seed(seed)
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = n * 1.45)
  dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")][seq_len(n)]
  R <- matrix(rnorm(n * k, 0.0004, 0.01), n, k,
              dimnames = list(NULL, paste0("T", seq_len(k))))
  list(R = R, dates = dates)
}

all_strategies <- lapply(stats::setNames(unname(allocation_methods), names(allocation_methods)),
                         function(m) list(method = m))

test_that("weights at a rebalance never depend on later returns", {
  d <- sim_returns()
  base <- run_walk_forward(d$R, d$dates, all_strategies, lookback = 126, max_w = 0.5)
  rb <- bt_rebalance_index(d$dates, 126)
  cut <- rb[6]
  R2 <- d$R
  R2[(cut + 1):nrow(R2), ] <- R2[(cut + 1):nrow(R2), ] * -3 + 0.01
  changed <- run_walk_forward(R2, d$dates, all_strategies, lookback = 126, max_w = 0.5)
  for (nm in names(all_strategies)) {
    expect_equal(changed$strategies[[nm]]$weights[1:6, ],
                 base$strategies[[nm]]$weights[1:6, ], label = nm)
    # Equal weight ignores the data, so only the estimating strategies must react.
    if (nm != "Equal weight") {
      expect_false(isTRUE(all.equal(changed$strategies[[nm]]$weights[7:10, ],
                                    base$strategies[[nm]]$weights[7:10, ])), label = nm)
    }
  }
})

test_that("every rebalance is long-only, fully invested, and capped", {
  d <- sim_returns()
  bt <- run_walk_forward(d$R, d$dates, all_strategies, lookback = 126, max_w = 0.4)
  for (nm in names(all_strategies)) {
    W <- as.matrix(bt$strategies[[nm]]$weights[, -1])
    expect_equal(unname(rowSums(W)), rep(1, nrow(W)), tolerance = 1e-8, label = nm)
    expect_true(all(W >= -1e-10 & W <= 0.4 + 1e-8), label = nm)
  }
})

test_that("a ticker is excluded until it has a full lookback of history", {
  d <- sim_returns()
  d$R[1:300, "T4"] <- NA
  bt <- run_walk_forward(d$R, d$dates, all_strategies["Min variance"], lookback = 126)
  W <- bt$strategies[["Min variance"]]$weights
  first_ok <- min(which(bt_rebalance_index(d$dates, 126) - 126 + 1 > 300))
  expect_true(all(W$T4[seq_len(first_ok - 1)] == 0))
  expect_gt(W$T4[first_ok], 0)
})

test_that("the bootstrap is reproducible and brackets the point estimate", {
  d <- sim_returns(n = 700)
  bt <- run_walk_forward(d$R, d$dates, all_strategies[c("Equal weight", "Min variance")],
                         lookback = 126)
  a <- bt_bootstrap_sharpe(bt, n_boot = 200)
  b <- bt_bootstrap_sharpe(bt, n_boot = 200)
  expect_identical(a, b)
  expect_true(all(a$sharpe_lo <= a$sharpe & a$sharpe <= a$sharpe_hi))
  expect_equal(a$diff[a$strategy == "Equal weight"], 0)
})

test_that("stationary bootstrap indices are valid and mostly contiguous", {
  set.seed(3)
  idx <- stationary_bootstrap_index(5000, mean_block = 20)
  expect_length(idx, 5000)
  expect_true(all(idx >= 1 & idx <= 5000))
  steps <- diff(idx) == 1 | (idx[-1] == 1 & idx[-5000] == 5000)
  expect_equal(mean(steps), 1 - 1 / 20, tolerance = 0.02)
})
