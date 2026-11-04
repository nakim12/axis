# Static methodology / limitations copy for the Methodology tab.

methodology_panel <- function() {
  tagList(
    page_header(NULL, "Methodology", "How the numbers are <em>made</em>.",
      "How metrics are computed in this app, and what not to infer from them."),

    tags$div(class = "method-grid",
    wellPanel(
      h4("Data"),
      tags$ul(
        tags$li("Daily adjusted close prices from Yahoo Finance via ", tags$code("quantmod"), "."),
        tags$li("Your analysis window is the date range chosen on ", tags$b("Portfolio"), "."),
        tags$li("Per-ticker charts under ", tags$b("Explore"), " \u2192 ", tags$b("Prices & returns"),
                " use the tickers selected there and about the last year of data (separate from the portfolio window).")
      )
    ),

    wellPanel(
      h4("Portfolio return"),
      tags$ul(
        tags$li("Weights are fixed over time: each day, portfolio return is the ",
                tags$b("weighted sum"), " of holding daily simple returns on aligned dates."),
        tags$li("Cumulative return is ", tags$code("cumprod(1 + r) - 1"), " on that series."),
        tags$li("This is ", tags$b("not"), " a full rebalance or cash-flow model; it is a standard back-of-the-envelope backtest.")
      )
    ),

    wellPanel(
      h4("Risk & ratios"),
      tags$ul(
        tags$li(tags$b("Annualized volatility:"), " sample standard deviation of daily returns × √252."),
        tags$li(tags$b("Sharpe:"), " (annualized mean return − risk-free) / annualized vol. Risk-free is assumed 0 here."),
        tags$li(tags$b("Sortino:"), " annualized mean return / annualized downside deviation, where downside deviation is ",
                "√(mean of min(r, 0)²) over ", tags$em("all"), " days, so gains count as zero rather than being dropped. ",
                "Shown as N/A when there are no losing days."),
        tags$li(tags$b("Max drawdown:"), " worst peak-to-trough on the cumulative wealth index, which starts at 1 before the first day, so a first-day loss counts."),
        tags$li(tags$b("VaR / CVaR (95%):"), " historical 5th percentile of daily returns and mean of returns on days at or below that threshold.")
      )
    ),

    wellPanel(
      h4("Benchmark-relative"),
      tags$ul(
        tags$li("Metrics use ", tags$b("overlapping trading days"), " only."),
        tags$li(tags$b("Beta:"), " Cov(portfolio, benchmark) / Var(benchmark) on daily returns."),
        tags$li(tags$b("Alpha (shown):"), " annualized portfolio return − beta × annualized benchmark return (simplified CAPM-style decomposition)."),
        tags$li(tags$b("Excess return:"), " mean daily difference × 252."),
        tags$li(tags$b("Tracking error:"), " annualized std dev of daily excess returns."),
        tags$li(tags$b("Information ratio:"), " annualized excess return / tracking error.")
      )
    ),

    wellPanel(
      h4("Concentration & diversification"),
      tags$ul(
        tags$li(tags$b("HHI-style concentration"), " on weights: normalized Herfindahl so 0 ≈ equal-weight, 1 ≈ single name."),
        tags$li(tags$b("Diversification ratio:"), " weighted average of individual annual volatilities / portfolio annual volatility (from the same return window)."),
        tags$li(tags$b("Risk contribution:"), " Euler / marginal contribution using the covariance matrix of daily returns and normalized weights."),
        tags$li(tags$b("Linear return attribution:"),
                " each name’s sum over days of (weight × daily return), as a share of the sum of daily portfolio returns. ",
                "It matches the fixed-weight daily model but is ", tags$b("not"), " the same as each name’s simple period return.")
      )
    ),

    wellPanel(
      h4("Sectors"),
      tags$p("Sector labels come from a fixed in-app universe (Technology, Financial, etc.). ",
             "Tickers outside that list are grouped as ", tags$b("Other"), ".")
    ),

    wellPanel(
      h4("Explore \u2192 Forecast"),
      tags$ul(
        tags$li(tags$b("Model:"), " ", tags$code("auto.arima"), " on the last two years of daily ", tags$b("log"), " prices, with no seasonal terms. ",
                "Forecasts and 80% / 95% intervals are converted back to dollars, so the line is the median path. It is exploratory, not a trading signal."),
        tags$li(tags$b("Rolling-origin test:"), " on a frozen copy of the data, every 5 trading days from January 2015, ARIMA is fitted to the previous 504 days only ",
                "and forecasts 1, 5 and 21 days ahead. The model order is re-selected every 13 origins (about a quarter) and the coefficients are re-estimated at every origin. ",
                "A unit test changes all prices after an origin and checks that the forecasts made up to it stay the same."),
        tags$li(tags$b("Baseline:"), " a random walk, which forecasts that the price stays where it is. Beating it is the minimum bar for a price forecast."),
        tags$li(tags$b("RelMAE:"), " ARIMA's mean absolute log-price error divided by the random walk's on the same forecasts. Below 1 means ARIMA was more accurate."),
        tags$li(tags$b("Significance:"), " a two-sided Diebold–Mariano test on absolute errors (with the Harvey–Leybourne–Newbold small-sample correction). ",
                "Because 21-day forecasts made a week apart overlap, the test allows for correlated errors across ", tags$em("h"), " / 5 origins. ",
                "With 47 tickers and 3 horizons, some p-values fall below 0.05 by chance, so they are corrected with Benjamini–Hochberg across all 141 tests."),
        tags$li(tags$b("Coverage:"), " the share of actual prices that landed inside each interval. A well-calibrated 95% interval holds about 95% of them. ",
                "Coverage is also split into thirds by trailing 21-day volatility, because intervals fitted on calm data tend to be too narrow in turbulent weeks."),
        tags$li(tags$b("Confidence tag:"), " high if RelMAE is below 0.95 and significant after correction, medium if RelMAE is below 1, otherwise low.")
      )
    ),

    wellPanel(
      h4("Stress"),
      tags$ul(
        tags$li(tags$b("Historical episodes:"), " slice your portfolio (and benchmark) to a calendar window and rebase cumulative return to the first day in that window. ",
                "Summary stats are descriptive only."),
        tags$li(tags$b("Additive daily shock:"), " the same fixed ", tags$b("basis-point"), " amount is added to each selected holding’s ",
                "daily simple return on every day in your analysis sample; the portfolio is recomputed with fixed weights. ",
                "This is a mechanical sensitivity tool, not a model of how markets behave under stress."),
        tags$li(tags$b("Bootstrap fan:"), " independent resamples of the portfolio’s realized daily returns (same fixed weights) are compounded forward for a chosen horizon. ",
                "Bands show simulation percentiles; this is ", tags$b("not"), " a structural forecast and ignores drift, autocorrelation, and regime change.")
      )
    ),

    wellPanel(
      h4("Allocation"),
      tags$ul(
        tags$li(tags$b("Inputs:"), " sample mean and covariance of daily simple returns on your analysis window, annualized (×252 and ×252 for cov). ",
                "Your current weights are compared to alternative rules on that same window only."),
        tags$li(tags$b("Weight cap:"), " every method is long-only and fully invested, and respects the per-name cap. ",
                "A cap below 1/", tags$em("n"), " is rejected because the weights could not sum to 100%."),
        tags$li(tags$b("Min variance:"), " the portfolio with the lowest estimated variance, solved exactly with quadratic programming."),
        tags$li(tags$b("Min variance (shrunk covariance):"), " the same solver on a Ledoit–Wolf (2004) covariance estimate, ",
                "which pulls the noisy sample covariance toward a constant-correlation target by a data-driven amount. ",
                "It matters most when there are many names relative to the length of the window; with a handful of names and a year of data, it changes little."),
        tags$li(tags$b("Inverse volatility:"), " weights proportional to 1 / annualized volatility; names over the cap are clipped and the excess goes to the rest in proportion."),
        tags$li(tags$b("Equal weight:"), " 1/", tags$em("n"), " on each name in the covariance sample."),
        tags$li(tags$b("Max Sharpe:"), " the long-only portfolio with the highest estimated Sharpe ratio, solved exactly as a quadratic program ",
                "(minimize ", tags$em("y′Σy"), " subject to ", tags$em("μ′y = 1"), ", ", tags$em("y ≥ 0"), ", then rescale ", tags$em("y"), " to sum to 1). ",
                "If no capped mix has a positive expected return, it falls back to min variance and says so.")
      )
    ),

    wellPanel(
      h4("Walk-forward backtest"),
      tags$ul(
        tags$li(tags$b("No look-ahead:"), " on the last trading day of each month, every method re-estimates from the trailing window only ",
                "(6 months, 1 year, or 2 years of daily returns), trades at that day's close, and earns returns from the next day. ",
                "Using later data to choose earlier weights is look-ahead bias; it flatters every optimizer, max Sharpe most of all. ",
                "A unit test changes all returns after a rebalance date and checks that the weights chosen on that date stay the same."),
        tags$li(tags$b("Drift:"), " between rebalances, weights move with prices, as in a real account."),
        tags$li(tags$b("Turnover and costs:"), " turnover at a rebalance is half the sum of absolute weight changes; cost is turnover × the bps rate, ",
                "taken from that day's return. The initial purchase is not charged, and the benchmark is held without costs."),
        tags$li(tags$b("Short histories:"), " a ticker without a full estimation window is left out until it has one, and the page lists it."),
        tags$li(tags$b("Sharpe and its uncertainty:"), " Sharpe uses excess returns over the 13-week T-bill rate. ",
                "95% intervals come from a stationary block bootstrap (Politis & Romano, 1994; mean block 21 days), resampling the same days for every method, ",
                "so the interval for the difference from equal weight is a paired comparison. If it includes zero, the gap is not distinguishable from noise."),
        tags$li(tags$b("In sample vs walk-forward:"), " max Sharpe is also fitted once on the whole period with hindsight. ",
                "The gap between that and its walk-forward Sharpe shows how much of an optimizer's apparent edge comes from knowing the answer."),
        tags$li(tags$b("Reference results:"), " the same test on two ETF universes and settings chosen before any results were computed, ",
                "run on a frozen copy of the data so the numbers can be reproduced exactly."),
        tags$li(tags$b("Limits:"), " one historical path; your tickers are ones you chose today (survivorship and hindsight bias); ",
                "no taxes, slippage, or bid-ask spreads beyond the flat cost.")
      )
    ),

    wellPanel(
      h4("Insight confidence"),
      tags$ul(
        tags$li(tags$b("High:"), " follows directly from the weights (sector share, concentration), ",
                "or is a risk estimate on roughly two years or more of daily data."),
        tags$li(tags$b("Medium:"), " risk estimates (volatility, beta, correlation, risk contribution) on about six months to two years, ",
                "or return-based statements on three years or more."),
        tags$li(tags$b("Low:"), " return-based statements (return, excess return, Sharpe) on under three years, or anything on a very short window. ",
                "Mean returns are noisy; a single year says little about the next.")
      )
    ),

    wellPanel(
      h4("Snapshots, pins, and bundled export"),
      tags$ul(
        tags$li(tags$b("Portfolio JSON"), " stores weights, benchmark ticker, analysis dates, and (if you already analyzed) a compact numeric metrics summary. ",
                "You can reload it from ", tags$b("Portfolio"), " or share the file with yourself on another machine."),
        tags$li(tags$b("Diagnosis pins"), " capture the headline metrics table for the ", tags$b("current"), " run so you can compare after changing weights. ",
                "They live in this browser session only and disappear on refresh."),
        tags$li(tags$b("Full report (.zip)"), " bundles the diagnosis text export, key CSVs, and the portfolio JSON snapshot.")
      )
    ),

    wellPanel(
      h4("Disclaimer"),
      tags$p(style = "font-weight: 500;",
        "Educational software only — not investment, tax, or legal advice. ",
        "Past results do not guarantee future performance. You are responsible for your own decisions."
      )
    )
    )
  )
}
