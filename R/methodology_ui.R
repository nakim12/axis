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
      tags$p(tags$b("ARIMA"), " on prices is ", tags$b("exploratory"), ": useful for methodology demos, not a trading signal. ",
             "Uncertainty and model error are real; treat forecasts as one input among many.")
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
                "It usually gives steadier weights."),
        tags$li(tags$b("Inverse volatility:"), " weights proportional to 1 / annualized volatility; names over the cap are clipped and the excess goes to the rest in proportion."),
        tags$li(tags$b("Equal weight:"), " 1/", tags$em("n"), " on each name in the covariance sample."),
        tags$li(tags$b("Max Sharpe:"), " the long-only portfolio with the highest estimated Sharpe ratio, solved exactly as a quadratic program ",
                "(minimize ", tags$em("y′Σy"), " subject to ", tags$em("μ′y = 1"), ", ", tags$em("y ≥ 0"), ", then rescale ", tags$em("y"), " to sum to 1). ",
                "If no capped mix has a positive expected return, it falls back to min variance and says so.")
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
