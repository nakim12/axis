# Fixed evaluation setup for reported backtest and forecast results.
# Chosen before any results were computed; do not tune after the fact.

eval_universes <- list(
  asset_class = list(
    label     = "Asset-class ETFs",
    tickers   = c("SPY", "EFA", "EEM", "TLT", "IEF", "GLD", "VNQ", "DBC"),
    benchmark = list(label = "60/40 SPY/IEF", weights = c(SPY = 0.6, IEF = 0.4))
  ),
  sector = list(
    label     = "Sector ETFs",
    tickers   = c("XLB", "XLE", "XLF", "XLI", "XLK", "XLP", "XLU", "XLV", "XLY"),
    benchmark = list(label = "S&P 500 (SPY)", weights = c(SPY = 1))
  )
)

eval_window <- list(
  start = as.Date("2006-03-01"),
  end   = as.Date("2026-10-30")
)

eval_riskfree_ticker <- "^IRX"

eval_backtest_defaults <- list(
  lookback  = 252L,
  rebalance = "monthly",
  cost_bps  = 10,
  max_w     = 0.35
)

eval_forecast <- list(
  horizons      = c(1L, 5L, 21L),
  train_window  = 504L,
  origin_step   = 5L,
  reselect_every = 13L,
  start         = as.Date("2015-01-01")
)

eval_data_dir <- "data/eval"
