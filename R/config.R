library(shiny)
library(quantmod)
library(zoo)
library(forecast)
library(tidyr)
library(scales)
library(reshape2)
library(dplyr)
library(ggplot2)

options(xts.warn_dplyr_breaks_lag = FALSE)

stock_sectors <- list(
  "Technology" = c("AAPL", "MSFT", "GOOGL", "META", "NVDA", "AMZN"),
  "Financial"  = c("JPM", "BAC", "WFC", "GS", "MS", "V"),
  "Healthcare" = c("JNJ", "PFE", "UNH", "CVS", "ABBV", "MRK"),
  "Consumer"   = c("WMT", "HD", "NKE", "MCD", "SBUX", "TGT"),
  "Energy"     = c("XOM", "CVX", "COP", "SLB", "OXY", "PSX")
)

all_stocks <- unlist(stock_sectors)
names(all_stocks) <- all_stocks

sample_portfolios <- list(
  "Balanced Growth" = list(
    tickers = c("AAPL", "MSFT", "JPM", "JNJ", "XOM"),
    weights = c(25, 25, 20, 15, 15)
  ),
  "Tech Heavy" = list(
    tickers = c("AAPL", "MSFT", "GOOGL", "NVDA", "AMZN"),
    weights = c(30, 25, 20, 15, 10)
  ),
  "Conservative" = list(
    tickers = c("JNJ", "PFE", "WMT", "V", "XOM", "CVX"),
    weights = c(20, 15, 20, 15, 15, 15)
  ),
  "Equal Weight Large Cap" = list(
    tickers = c("AAPL", "MSFT", "AMZN", "JPM", "JNJ", "XOM"),
    weights = rep(100 / 6, 6)
  )
)

benchmark_choices <- c(
  "S&P 500 (SPY)"    = "SPY",
  "Nasdaq 100 (QQQ)" = "QQQ",
  "Total Market (VTI)" = "VTI",
  "Dow Jones (DIA)"  = "DIA"
)
