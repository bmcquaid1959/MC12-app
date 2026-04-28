# ============================================================
# scripts/build_data.R
# Run this locally in RStudio to build data/df_all.rds
# ============================================================

library(RODBC)
library(here)
library(readr)

source(here::here("R", "prep_helpers.R"))

# -----------------------------
# Paths
# -----------------------------
company_catalog_path       <- here::here("data", "company_catalog.csv")
metric_catalog_path        <- here::here("data", "metric_catalog.csv")
report_layout_catalog_path <- here::here("data", "report_layout_catalog.csv")
output_rds_path            <- here::here("data", "df_all.rds")

# -----------------------------
# Helper: open and validate ODBC connection
# -----------------------------
open_mcbook_connection <- function(dsn_name = "MCBOOK_Current") {
  RODBC::odbcCloseAll()
  
  conn <- RODBC::odbcConnect(dsn_name)
  
  is_open <- tryCatch({
    info <- RODBC::odbcGetInfo(conn)
    !is.null(info)
  }, error = function(e) {
    FALSE
  })
  
  if (!is_open) {
    stop(
      paste0(
        "Unable to open ODBC connection for DSN '", dsn_name, "'.\n",
        "Check that:\n",
        "1. The DSN exists and is named exactly '", dsn_name, "'.\n",
        "2. The correct 32-bit/64-bit ODBC driver is installed.\n",
        "3. RStudio and the DSN use the same architecture.\n",
        "4. The DSN can be opened outside this script."
      ),
      call. = FALSE
    )
  }
  
  conn
}

# -----------------------------
# Main build function
# -----------------------------
build_mcbook_data <- function() {
  company_catalog       <- read_company_catalog(company_catalog_path)
  metric_catalog        <- read_metric_catalog(metric_catalog_path)
  report_layout_catalog <- read_report_layout_catalog(report_layout_catalog_path)
  
  conn <- open_mcbook_connection("MCBOOK_Current")
  on.exit(RODBC::odbcClose(conn), add = TRUE)
  
  df_raw <- RODBC::sqlQuery(
    channel = conn,
    query = "SELECT * FROM [BOOK DATA]",
    stringsAsFactors = FALSE
  )
  
  if (is.character(df_raw)) {
    stop(
      paste("ODBC query failed:", paste(df_raw, collapse = " | ")),
      call. = FALSE
    )
  }
  
  if (!is.data.frame(df_raw) || nrow(df_raw) == 0) {
    stop("The query returned no rows from [BOOK DATA].", call. = FALSE)
  }
  
  df_all <- build_df_all(
    df_raw = df_raw,
    company_catalog = company_catalog,
    metric_catalog = metric_catalog,
    report_layout_catalog = report_layout_catalog
  )
  
  saveRDS(df_all, output_rds_path)
  
  message("Build complete: ", output_rds_path)
  message("Rows: ", nrow(df_all))
  message("Cols: ", ncol(df_all))
  message("Date range: ", min(df_all$date, na.rm = TRUE), " to ", max(df_all$date, na.rm = TRUE))
  
  invisible(df_all)
}

# -----------------------------
# Execute build
# -----------------------------
build_mcbook_data()