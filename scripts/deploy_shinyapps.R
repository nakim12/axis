#!/usr/bin/env Rscript
# Deploy app.R (and R/ helpers) to shinyapps.io. Run from repository root.
# Requires env: SHINYAPPS_ACCOUNT, SHINYAPPS_TOKEN, SHINYAPPS_SECRET
# Optional: SHINYAPPS_APP_NAME (default: investment-performance-tracker)

account <- Sys.getenv("SHINYAPPS_ACCOUNT", unset = "")
token   <- Sys.getenv("SHINYAPPS_TOKEN", unset = "")
secret  <- Sys.getenv("SHINYAPPS_SECRET", unset = "")

if (account == "" || token == "" || secret == "") {
  stop(
    "Missing shinyapps.io credentials. Set SHINYAPPS_ACCOUNT, ",
    "SHINYAPPS_TOKEN, and SHINYAPPS_SECRET (GitHub Actions secrets).",
    call. = FALSE
  )
}

app_name <- Sys.getenv("SHINYAPPS_APP_NAME", unset = "")
if (app_name == "") app_name <- "investment-performance-tracker"

rsconnect::setAccountInfo(
  name   = account,
  token  = token,
  secret = secret
)

# shinyapps.io downloads package *sources* from URLs in the bundle manifest.
# - Named repos c(CRAN = "https://...") can still serialize as scheme "CRAN/..." on server.
# - Linux/binary installs sometimes record Version like "15.2.4-1" but CRAN tarballs are
#   "15.2.4", so fetches 404 / wrong path.
cran_root <- "https://cloud.r-project.org"
options(repos = cran_root)

if (nzchar(Sys.getenv("CI", ""))) {
  message("CI: reinstalling Rcpp stack from CRAN source for rsconnect/shinyapps manifest...")
  utils::install.packages(
    c("Rcpp", "cpp11", "RcppArmadillo"),
    repos = cran_root,
    type  = "source",
    quiet = TRUE
  )
}

# Only bundle the Shiny app. If the whole repo is bundled, rsconnect's renv
# snapshot pulls in knitr/rmarkdown/tseries from the Rmd and fails CI.
r_files <- if (dir.exists("R")) {
  list.files("R", pattern = "\\.[Rr]$", full.names = TRUE)
} else {
  character()
}
www_files <- if (dir.exists("www")) {
  list.files("www", recursive = TRUE, full.names = TRUE)
} else {
  character()
}
font_files <- if (dir.exists("fonts")) {
  list.files("fonts", pattern = "\\.ttf$", full.names = TRUE)
} else {
  character()
}
app_files <- c(
  "app.R",
  if (file.exists("global.R")) "global.R",
  sort(r_files),
  sort(www_files),
  sort(font_files)
)

deploy_args <- list(
  appDir        = ".",
  appPrimaryDoc = "app.R",
  appName       = app_name,
  appFiles      = app_files,
  lint          = FALSE,
  forceUpdate   = TRUE
)

# shinyapps.io sometimes returns HTTP 409 while another deploy is still in flight.
max_attempts <- as.integer(Sys.getenv("DEPLOY_MAX_ATTEMPTS", "5"))
if (is.na(max_attempts) || max_attempts < 1L) max_attempts <- 5L
base_wait <- as.integer(Sys.getenv("DEPLOY_RETRY_WAIT_SEC", "45"))
if (is.na(base_wait) || base_wait < 5L) base_wait <- 45L

is_transient_deploy_error <- function(e) {
  msg <- paste(conditionMessage(e), collapse = " ")
  grepl(
    "409|already in progress|Unable to dispatch task|tasks already in progress",
    msg,
    ignore.case = TRUE
  )
}

for (attempt in seq_len(max_attempts)) {
  outcome <- tryCatch(
    {
      do.call(rsconnect::deployApp, deploy_args)
      list(ok = TRUE)
    },
    error = function(e) {
      list(ok = FALSE, err = e, transient = is_transient_deploy_error(e))
    }
  )

  if (isTRUE(outcome$ok)) {
    break
  }

  if (!outcome$transient || attempt >= max_attempts) {
    stop(conditionMessage(outcome$err), call. = FALSE)
  }

  wait_sec <- min(base_wait * (2^(attempt - 1L)), 300L)
  message(
    "Transient shinyapps.io deploy conflict (HTTP 409 or similar). ",
    "Attempt ", attempt, "/", max_attempts, "; waiting ", wait_sec,
    " s before retry..."
  )
  Sys.sleep(wait_sec)
}
