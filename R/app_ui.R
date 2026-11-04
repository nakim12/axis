# Main Shiny UI. Five primary pages plus Home (via the wordmark) and
# Methodology (via the "?" pill). Styles live in www/css; theme in R/app_theme.R.

sector_filter_choices <- function() c("All Sectors" = "all", names(stock_sectors))

explore_default_tickers <- c("AAPL", "MSFT", "JPM")

app_ui <- function() {
  navbarPage(
  title       = axis_brand(),
  windowTitle = "Axis — portfolio intelligence",
  id          = "main_nav",
  theme       = axis_bs_theme(),
  header      = axis_head(),
  collapsible = TRUE,

  tabPanel("Home", value = "home", landing_panel()),

  # ── Portfolio ──────────────────────────────────────────────────────────────

  tabPanel("Portfolio", value = "portfolio",
    page_header("01", "Portfolio", "Build the <em>portfolio</em>.",
      "Define holdings and weights, choose a benchmark and window, then analyze."),
    page_layout(
      rail = tagList(
        rail_group("Holdings",
          selectizeInput("portfolio_ticker_input", "Ticker",
            choices  = stock_sectors,
            options  = list(create = TRUE,
                            placeholder = "Type or select a ticker...")
          ),
          numericInput("portfolio_weight_input", "Weight (%)",
            value = 20, min = 0.1, max = 100, step = 0.5),
          actionButton("add_holding", "Add holding", class = "btn-default btn-sm btn-block")
        ),
        rail_group("Quick start",
          selectInput("sample_portfolio", "Sample portfolio",
            choices = c("Choose..." = "", names(sample_portfolios))),
          actionButton("load_sample", "Load sample", class = "btn-default btn-sm btn-block")
        ),
        rail_group("Benchmark & window",
          selectInput("benchmark_ticker", "Compare against",
            choices = benchmark_choices, selected = "SPY"),
          dateRangeInput("portfolio_date_range", "Analysis period",
            start = Sys.Date() - 365,
            end   = Sys.Date(),
            max   = Sys.Date())
        ),
        tags$div(class = "rail-cta",
          actionButton("analyze_portfolio", "Analyze portfolio \u2192",
            class = "btn-primary btn-lg btn-block"),
          actionLink("clear_portfolio", "Clear all holdings", class = "rail-link")
        ),
        rail_group("Save & share",
          textInput("portfolio_snapshot_label", "Snapshot label",
            placeholder = "e.g. Taxable account"),
          downloadButton("download_portfolio_snapshot", "Download .json",
            class = "btn-default btn-sm btn-block"),
          rail_note("Exports weights, benchmark, and date range."),
          fileInput("portfolio_snapshot_upload", "Load snapshot",
            accept = c("application/json", ".json"),
            buttonLabel = "Choose JSON…", placeholder = "No file selected",
            width = "100%"),
          rail_note("After loading, analyze again to refresh metrics.")
        )
      ),

      page_section("01", "Holdings",
        uiOutput("holdings_list"),
        kpi_strip(
          tags$div(class = "kpi-card",
            uiOutput("total_weight_display"),
            tags$div(class = "kpi-label", "Total weight")
          ),
          tags$div(class = "kpi-card",
            tags$div(class = "kpi-value", textOutput("num_holdings_display", inline = TRUE)),
            tags$div(class = "kpi-label", "Holdings")
          ),
          tags$div(class = "kpi-card",
            tags$div(class = "kpi-value", textOutput("concentration_display", inline = TRUE)),
            tags$div(class = "kpi-label", "Concentration (HHI)")
          )
        )
      ),

      page_section("02", "Allocation",
        uiOutput("allocation_bars")
      ),

      uiOutput("portfolio_results_section")
    )
  ),

  # ── Diagnosis ──────────────────────────────────────────────────────────────

  tabPanel("Diagnosis", value = "diagnosis",
    page_header("02", "Diagnosis", "Read the <em>risk</em>.",
      "Return, risk, and benchmark context for the analyzed portfolio, in numbers and in plain language."),
    uiOutput("diagnosis_content")
  ),

  # ── Stress ─────────────────────────────────────────────────────────────────

  tabPanel("Stress", value = "stress",
    page_header("03", "Stress", "Test it <em>under pressure</em>.",
      "Replay historical episodes, add a mechanical daily shock, or simulate a spread of forward paths."),
    page_layout(
      rail = tagList(
        rail_group("Historical episode",
          selectInput("stress_preset", "Preset or custom",
            choices  = stress_preset_choices(),
            selected = "custom"),
          dateRangeInput("stress_custom_range", "Date range",
            start = Sys.Date() - 365, end = Sys.Date(), max = Sys.Date()),
          rail_note("Choosing a preset fills these dates; you can edit them anytime.")
        ),
        rail_group("Additive shock",
          radioButtons("stress_scope", "Apply extra daily return to",
            choices = c("All holdings" = "all", "One sector" = "sector"),
            selected = "all"),
          conditionalPanel(
            condition = "input.stress_scope == 'sector'",
            selectInput("stress_sector_pick", "Sector",
              choices = names(stock_sectors), selected = names(stock_sectors)[[1]])
          ),
          sliderInput("stress_bps", "Extra daily return (bps)",
            min = -100, max = 100, value = 0, step = 5),
          rail_note("The same bps is added to each affected holding\u2019s daily return. 0 bps is the baseline.")
        ),
        rail_group("Bootstrap fan",
          sliderInput("stress_fan_horizon", "Horizon (trading days)",
            min = 5, max = 252, value = 63, step = 1),
          sliderInput("stress_fan_n_sims", "Simulated paths",
            min = 100, max = 3000, value = 800, step = 50),
          numericInput("stress_fan_seed", "Random seed", value = 1, min = 1, max = 99999, step = 1),
          rail_note("Resamples realized daily returns with fixed weights. Illustrative dispersion, not a forecast.")
        )
      ),
      uiOutput("stress_need_analyze"),
      page_section("01", "Historical episode",
        lead = "Portfolio and benchmark rebased to the first day of the window.",
        plot_plate("stress_hist_plot", "300px"),
        tableOutput("stress_hist_table")
      ),
      page_section("02", "Stressed vs baseline",
        lead = "Baseline is the analyzed portfolio. The stressed path applies the bps adjustment only to the scope you selected.",
        plot_plate("stress_shock_plot", "340px")
      ),
      page_section("03", "Bootstrap fan",
        lead = "Each path stacks independent draws from realized daily returns; the band is the 5th\u201395th percentile.",
        plot_plate("stress_fan_plot", "320px")
      )
    )
  ),

  # ── Allocation ─────────────────────────────────────────────────────────────

  tabPanel("Allocation", value = "allocation",
    page_header("04", "Allocation", "Weigh the <em>alternatives</em>.",
      "Compare your current weights with model portfolios built on the same price history."),
    page_layout(
      rail = tagList(
        rail_group("Model portfolio",
          selectInput("alloc_method", "Construction method",
            choices = allocation_methods,
            selected = "minvar"),
          sliderInput("alloc_max_w_pct", "Max weight per name (%)",
            min = 5, max = 100, value = 100, step = 1),
          rail_note("100% means no cap. Every method stays long-only and fully invested."),
          rail_note("Sections 01 and 02 use the price history from your last analysis run.")
        ),
        backtest_rail_ui()
      ),
      uiOutput("alloc_lab_need_analyze"),
      page_section("01", "The trade-off",
        lead = "Annualized estimates for both mixes on the same return sample.",
        uiOutput("alloc_tradeoff")
      ),
      page_section("02", "Weights, current vs proposed",
        uiOutput("alloc_weight_pairs"),
        conditionalPanel(
          condition = "input.alloc_method == 'maxsharpe'",
          tags$p(class = "page-footnote",
            tags$b("Max Sharpe"), " leans entirely on estimated mean returns, which are noisy over short windows. ",
            "Expect its weights to swing when the analysis period changes."
          )
        )
      ),
      backtest_sections_ui()
    )
  ),

  # ── Explore (single names) ─────────────────────────────────────────────────

  tabPanel("Explore", value = "explore",
    page_header("05", "Explore", "Look at <em>single names</em>.",
      "Price trends, per-ticker returns, exploratory forecasts, and risk, outside the portfolio context."),
    tabsetPanel(
      id   = "explore_tabs",
      type = "pills",

      tabPanel("Prices & returns", value = "prices",
        page_layout(
          rail = tagList(
            rail_group("Tickers",
              selectInput("sector_filter", "Sector",
                choices = sector_filter_choices(), selected = "all"),
              selectInput("tickers", "Compare",
                choices = unname(all_stocks), selected = explore_default_tickers,
                multiple = TRUE),
              tags$div(class = "btn-pair",
                actionButton("select_all", "Select all", class = "btn-default btn-sm"),
                actionButton("clear_all",  "Clear",      class = "btn-default btn-sm")
              )
            ),
            rail_group("Trend",
              sliderInput("ma_window", "Moving average (days)",
                min = 5, max = 100, value = 20)
            ),
            rail_note("Returns use roughly the last year of data for the selected tickers.")
          ),
          page_section("01", "Price trend", plot_plate("pricePlot", "400px")),
          page_section("02", "Cumulative performance",
            lead = "Growth of $1 invested over the loaded period.",
            plot_plate("cumulativePlot", "380px")),
          page_section("03", "Daily returns", plot_plate("returnsPlot", "360px")),
          page_section("04", "Return distributions", plot_plate("returnDistribution", "380px")),
          page_section("05", "Summary",
            tableOutput("performanceSummary"))
        )
      ),

      tabPanel("Forecast", value = "forecast",
        page_layout(
          rail = tagList(
            rail_group("Ticker",
              selectInput("forecast_sector", "Sector",
                choices = sector_filter_choices(), selected = "all"),
              selectInput("forecast_ticker", "Ticker", choices = NULL)
            ),
            rail_group("Model",
              sliderInput("horizon", "Horizon (days)", min = 5, max = 90, value = 30),
              radioButtons("forecast_mode", "Mode",
                choices  = c("Conservative (recommended)" = "conservative",
                             "Aggressive (less reliable)"  = "aggressive"),
                selected = "conservative"),
              rail_note("Conservative mode requires more history for a fit.")
            ),
            tags$div(class = "rail-note", textOutput("dataAvailability"))
          ),
          page_section("01", "Forecast",
            lead = "ARIMA on prices. Exploratory only, not a trading signal.",
            plot_plate("forecastPlot", "400px")),
          page_section("02", "Model evaluation",
            verbatimTextOutput("forecastMetrics"),
            plot_plate("forecastAccuracyPlot", "340px"))
        )
      ),

      tabPanel("Risk", value = "risk",
        page_layout(
          rail = tagList(
            rail_group("Tickers",
              selectInput("risk_sector", "Sector",
                choices = sector_filter_choices(), selected = "all"),
              selectInput("risk_tickers", "Analyze",
                choices = unname(all_stocks), selected = explore_default_tickers,
                multiple = TRUE),
              dateRangeInput("risk_date_range", "Analysis period",
                start = Sys.Date() - 365,
                end   = Sys.Date(),
                max   = Sys.Date())
            ),
            rail_group("Definitions",
              tags$dl(class = "rail-defs",
                tags$dt("Volatility"),   tags$dd("Annualized standard deviation of returns."),
                tags$dt("Sharpe"),       tags$dd("Risk-adjusted return; higher is better."),
                tags$dt("Max drawdown"), tags$dd("Largest peak-to-trough decline."),
                tags$dt("VaR (95%)"),    tags$dd("Daily loss exceeded on 5% of days.")
              )
            )
          ),
          page_section("01", "Risk metrics",
            tableOutput("riskMetricsTable")),
          page_section("02", "Correlation", plot_plate("correlationHeatmap", "400px")),
          page_section("03", "Risk vs return", plot_plate("riskReturnPlot", "400px"))
        )
      )
    )
  ),

  # ── Methodology ("?" pill) ─────────────────────────────────────────────────

  tabPanel(
    title = tags$span(class = "nav-help", title = "Methodology", `aria-label` = "Methodology", "?"),
    value = "methodology",
    methodology_panel()
  )
  )
}
