fc_fixture <- function(n = 160, seed = 7) {
  set.seed(seed)
  list(y = log(100) + cumsum(c(0, stats::arima.sim(list(ar = 0.3), n - 1, sd = 0.01))),
       dates = as.Date("2020-01-01") + seq_len(n) - 1)
}

fc_run <- function(y, dates) {
  fc_rolling_forecasts(y, dates, horizons = c(1L, 5L), train_window = 100L,
                       origin_step = 5L, reselect_every = 4L)
}

test_that("forecasts at each origin use only data up to that origin", {
  f <- fc_fixture()
  base <- fc_run(f$y, f$dates)
  cut <- 130
  y2 <- f$y
  y2[(cut + 1):length(y2)] <- y2[(cut + 1):length(y2)] + 0.5
  alt <- fc_run(y2, f$dates)
  keep <- base$origin <= f$dates[cut]
  cols <- c("origin", "h", "arima", "lo80", "hi80", "lo95", "hi95", "rw", "vol21")
  expect_true(any(keep))
  expect_equal(alt[alt$origin <= f$dates[cut], cols], base[keep, cols])
})

test_that("origins, baseline and actuals line up", {
  f <- fc_fixture()
  rows <- fc_run(f$y, f$dates)
  idx <- match(rows$origin, f$dates)
  expect_equal(unique(diff(unique(idx))), 5L)
  expect_equal(min(idx), 100L)
  expect_equal(rows$rw, f$y[idx])
  expect_equal(rows$actual, f$y[idx + rows$h])
  expect_true(all(idx + rows$h <= length(f$y)))
  expect_true(all(rows$lo95 <= rows$lo80 & rows$lo80 <= rows$arima &
                  rows$arima <= rows$hi80 & rows$hi80 <= rows$hi95))
})

test_that("summary metrics match hand calculations", {
  rows <- data.frame(
    h = 1L, actual = c(0, 0.1, -0.1, 0.2), arima = c(0.05, 0.05, 0, 0.1), rw = c(0.1, 0, 0, 0),
    lo80 = -0.05, hi80 = 0.05, lo95 = -0.15, hi95 = 0.15,
    vol21 = c(0.1, 0.2, 0.3, 0.4), reselected = c(TRUE, FALSE, TRUE, FALSE),
    random_walk = c(TRUE, TRUE, FALSE, FALSE)
  )
  s <- fc_summarize_ticker(rows, origin_step = 5L)
  expect_equal(s$mae_arima, mean(c(0.05, 0.05, 0.1, 0.1)))
  expect_equal(s$mae_rw, mean(c(0.1, 0.1, 0.1, 0.2)))
  expect_equal(s$relmae, 0.075 / 0.125)
  expect_equal(s$cov80, 0.25)
  expect_equal(s$cov95, 0.75)
  expect_equal(s$rw_share, 0.5)
})

test_that("identical forecasts get a DM p-value of 1", {
  e <- c(0.01, -0.02, 0.03, 0.01, -0.01, 0.02, 0.00, -0.03, 0.01, 0.02)
  expect_equal(fc_dm_pvalue(e, e, 1, 5), 1)
  expect_lt(fc_dm_pvalue(e * 0.2, e * 3 + 0.05, 1, 5), 0.05)
})

test_that("confidence tags follow RelMAE and corrected significance", {
  expect_equal(fc_confidence(0.9, 0.01), "high")
  expect_equal(fc_confidence(0.9, 0.2), "medium")
  expect_equal(fc_confidence(0.97, 0.01), "medium")
  expect_equal(fc_confidence(1.02, 0.01), "low")
})
