# ============================================================
# R/prep_helpers.R
# Helper functions for building data/df_all.rds
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(janitor)
library(purrr)
library(rlang)

# -----------------------------
# Catalog readers
# -----------------------------
read_company_catalog <- function(path = "data/company_catalog.csv") {
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    mutate(
      company_id   = as.character(company_id),
      display_name = as.character(display_name),
      color_hex    = stringr::str_replace_all(color_hex, '"', ""),
      plot_order   = as.integer(plot_order),
      is_benchmark = dplyr::coalesce(as.logical(is_benchmark), FALSE),
      show_in_app  = dplyr::coalesce(as.logical(show_in_app), TRUE)
    ) |>
    arrange(plot_order)
}

read_metric_catalog <- function(path = "data/metric_catalog.csv") {
  mc <- readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
  
  defaults <- list(
    old_metric = NA_character_,
    mcbook_variable_name = NA_character_,
    metric = NA_character_,
    formula = NA_character_,
    display_label_long = NA_character_,
    format_type = "number",
    aggregation_rule = "sum",
    time_period_support = "monthly|ytd|quarterly|annual",
    higher_is_better = NA,
    show_in_table_default = NA,
    show_in_chart_default = NA,
    can_drill = NA,
    chart_family = NA_character_,
    report_sort_group = NA_character_,
    denominator_metric = NA_character_
  )
  
  for (nm in names(defaults)) {
    if (!nm %in% names(mc)) mc[[nm]] <- defaults[[nm]]
  }
  
  mc |>
    mutate(
      across(
        c(
          old_metric, mcbook_variable_name, metric, formula, display_label_long,
          format_type, aggregation_rule, time_period_support, chart_family,
          report_sort_group, denominator_metric
        ),
        ~ na_if(trimws(as.character(.x)), "")
      ),
      metric = as.character(metric),
      display_label_long = coalesce(display_label_long, metric),
      format_type = coalesce(format_type, "number"),
      aggregation_rule = str_to_lower(coalesce(aggregation_rule, "sum")),
      higher_is_better = suppressWarnings(as.logical(higher_is_better)),
      show_in_table_default = suppressWarnings(as.logical(show_in_table_default)),
      show_in_chart_default = suppressWarnings(as.logical(show_in_chart_default)),
      can_drill = suppressWarnings(as.logical(can_drill))
    )
}

read_report_layout_catalog <- function(path = "data/report_layout_catalog.csv") {
  rl <- readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
  
  if ("layout_row_id_1" %in% names(rl)) {
    rl <- rl |> select(-layout_row_id_1)
  }
  
  defaults <- list(
    metric = NA_character_,
    display_label_long = NA_character_,
    report_view = NA_character_,
    layout_row_id = NA_character_,
    section_order = NA_real_,
    metric_order_wi_section = NA_real_,
    row_kind = NA_character_,
    include_in_default_view = TRUE,
    drilldown_parent_metric = NA_character_,
    indent_level = 0,
    font = NA_character_
  )
  
  for (nm in names(defaults)) {
    if (!nm %in% names(rl)) rl[[nm]] <- defaults[[nm]]
  }
  
  rl |>
    mutate(
      across(
        c(metric, display_label_long, report_view, layout_row_id,
          row_kind, drilldown_parent_metric, font),
        ~ na_if(trimws(as.character(.x)), "")
      ),
      section_order = suppressWarnings(as.numeric(section_order)),
      metric_order_wi_section = suppressWarnings(as.numeric(metric_order_wi_section)),
      include_in_default_view = coalesce(as.logical(include_in_default_view), TRUE),
      indent_level = coalesce(as.numeric(indent_level), 0),
      row_kind = str_replace_all(str_to_lower(coalesce(row_kind, "detail")), "-", "_"),
      font = str_to_lower(font)
    ) |>
    arrange(report_view, section_order, metric_order_wi_section)
}

# -----------------------------
# Validation helpers
# -----------------------------
extract_formula_dependencies <- function(formula_text, valid_metrics) {
  if (is.na(formula_text) || trimws(formula_text) == "") {
    return(character(0))
  }
  
  tokens <- stringr::str_extract_all(
    formula_text,
    "\\b[A-Za-z_][A-Za-z0-9_]*\\b"
  )[[1]]
  
  reserved <- c("TRUE", "FALSE", "NA", "NULL", "Inf", "NaN")
  tokens <- setdiff(tokens, reserved)
  
  intersect(unique(tokens), valid_metrics)
}

topological_sort_metrics <- function(metric_catalog) {
  formula_tbl <- metric_catalog |>
    filter(!is.na(formula), formula != "") |>
    distinct(metric, formula)
  
  metric_names <- formula_tbl$metric
  valid_metrics <- metric_catalog$metric
  
  deps <- purrr::map(
    formula_tbl$formula,
    extract_formula_dependencies,
    valid_metrics = valid_metrics
  )
  names(deps) <- metric_names
  
  temp_mark <- character(0)
  perm_mark <- character(0)
  ordered <- character(0)
  
  visit <- function(node) {
    if (node %in% perm_mark) return(invisible(NULL))
    if (node %in% temp_mark) {
      stop(
        paste0("Circular formula dependency detected involving metric: ", node),
        call. = FALSE
      )
    }
    
    temp_mark <<- c(temp_mark, node)
    
    for (dep in deps[[node]]) {
      if (dep %in% names(deps)) {
        visit(dep)
      }
    }
    
    temp_mark <<- setdiff(temp_mark, node)
    perm_mark <<- c(perm_mark, node)
    ordered <<- c(ordered, node)
    invisible(NULL)
  }
  
  for (m in metric_names) visit(m)
  ordered
}

validate_metric_catalog <- function(metric_catalog) {
  metric_names <- metric_catalog$metric
  
  if (any(is.na(metric_names) | metric_names == "")) {
    stop("metric_catalog.csv contains blank metric names.", call. = FALSE)
  }
  
  dup_metrics <- metric_catalog |>
    count(metric, name = "n") |>
    filter(n > 1)
  
  if (nrow(dup_metrics) > 0) {
    stop(
      paste0(
        "Duplicate metric names in metric_catalog.csv: ",
        paste(dup_metrics$metric, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  formula_check <- metric_catalog |>
    filter(!is.na(formula), formula != "") |>
    rowwise() |>
    mutate(
      formula_refs = list(extract_formula_dependencies(formula, metric_names)),
      self_reference = metric %in% formula_refs,
      unresolved_refs = list(setdiff(formula_refs, metric_names))
    ) |>
    ungroup()
  
  self_ref_rows <- formula_check |>
    filter(self_reference)
  
  if (nrow(self_ref_rows) > 0) {
    stop(
      paste0(
        "Self-referential formulas found in metric_catalog.csv: ",
        paste(self_ref_rows$metric, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  
  unresolved <- formula_check |>
    filter(lengths(unresolved_refs) > 0)
  
  if (nrow(unresolved) > 0) {
    msg <- unresolved |>
      transmute(txt = paste0(metric, " -> ", map_chr(unresolved_refs, ~ paste(.x, collapse = ", ")))) |>
      pull(txt) |>
      paste(collapse = " | ")
    stop(paste("Unresolved formula references:", msg), call. = FALSE)
  }
  
  invisible(topological_sort_metrics(metric_catalog))
}

validate_report_layout_catalog <- function(report_layout_catalog, metric_catalog) {
  layout_metrics <- report_layout_catalog |>
    filter(!is.na(metric), metric != "") |>
    distinct(metric) |>
    pull(metric)
  
  missing_metrics <- setdiff(layout_metrics, metric_catalog$metric)
  
  if (length(missing_metrics) > 0) {
    stop(
      paste0(
        "report_layout_catalog.csv contains metrics not found in metric_catalog.csv: ",
        paste(missing_metrics, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

# -----------------------------
# Raw data helpers
# -----------------------------
standardize_raw_columns <- function(df_raw) {
  janitor::clean_names(df_raw)
}

map_mcbook_columns_to_metrics <- function(df_raw, metric_catalog) {
  df <- standardize_raw_columns(df_raw)
  
  source_map <- metric_catalog |>
    filter(!is.na(mcbook_variable_name), mcbook_variable_name != "") |>
    transmute(
      source_clean = janitor::make_clean_names(mcbook_variable_name),
      metric = metric
    ) |>
    distinct()
  
  matched_source_map <- source_map |>
    filter(source_clean %in% names(df))
  
  # CHANGED: base pipe cannot use `{}` this way, so build rename_map explicitly
  rename_map <- stats::setNames(matched_source_map$source_clean, matched_source_map$metric)
  
  if (length(rename_map) > 0) {
    df <- df |>
      rename(!!!rename_map)
  }
  
  missing_raw_metrics <- setdiff(source_map$metric, names(df))
  if (length(missing_raw_metrics) > 0) {
    for (m in missing_raw_metrics) df[[m]] <- NA
  }
  
  df
}

coerce_metric_columns <- function(df, metric_catalog) {
  metrics_in_data <- intersect(metric_catalog$metric, names(df))
  
  date_like <- metric_catalog |>
    filter(metric %in% metrics_in_data, format_type %in% c("date", "string")) |>
    pull(metric)
  
  numeric_like <- setdiff(metrics_in_data, date_like)
  
  for (nm in numeric_like) {
    if (!is.numeric(df[[nm]])) {
      df[[nm]] <- suppressWarnings(as.numeric(df[[nm]]))
    }
  }
  
  df
}

attach_company_id <- function(df, company_catalog) {
  company_lookup <- company_catalog |>
    filter(!is.na(co_number)) |>
    transmute(co_num = as.numeric(co_number), co_id = as.character(company_id))
  
  out <- df |>
    mutate(co_num = suppressWarnings(as.numeric(co_num))) |>
    left_join(company_lookup, by = "co_num")
  
  if (!"co_id" %in% names(out)) {
    out$co_id <- NA_character_
  }
  
  out |>
    filter(!is.na(co_id))
}

apply_company_filters <- function(df) {
  df |>
    filter(!(co_id == "cla" & yr < 2024)) |>
    filter(!(co_id == "zik" & yr < 2025))
}

add_time_fields <- function(df) {
  df |>
    mutate(
      yr = suppressWarnings(as.integer(yr)),
      mo = suppressWarnings(as.integer(mo)),
      date = lubridate::make_date(yr, mo, 1) |> lubridate::ceiling_date("month") - lubridate::days(1),
      qtr = lubridate::quarter(date)
    ) |>
    filter(!is.na(date), yr >= 2016)
}

fill_numeric_nas <- function(df, metric_catalog) {
  metrics_in_data <- intersect(metric_catalog$metric, names(df))
  numeric_metrics <- metric_catalog |>
    filter(metric %in% metrics_in_data, !format_type %in% c("date", "string")) |>
    pull(metric)
  
  df |>
    mutate(across(any_of(numeric_metrics), ~ tidyr::replace_na(.x, 0)))
}

# -----------------------------
# Formula engine
# -----------------------------
evaluate_formula_metric <- function(df, metric_name, formula_text) {
  eval_env <- rlang::env(parent = baseenv())
  for (nm in names(df)) {
    rlang::env_bind(eval_env, !!nm := df[[nm]])
  }
  
  result <- eval(parse(text = formula_text), envir = eval_env)
  
  if (length(result) == 1 && nrow(df) > 1) {
    result <- rep(result, nrow(df))
  }
  
  df[[metric_name]] <- result
  df
}

compute_all_formula_metrics <- function(df, metric_catalog) {
  formula_order <- topological_sort_metrics(metric_catalog)
  
  formula_lookup <- metric_catalog |>
    filter(metric %in% formula_order) |>
    distinct(metric, formula)
  
  for (m in formula_order) {
    formula_text <- formula_lookup$formula[match(m, formula_lookup$metric)]
    df <- evaluate_formula_metric(df, m, formula_text)
  }
  
  df
}

# -----------------------------
# Benchmark rows
# -----------------------------
add_benchmark_rows <- function(df, metric_catalog, benchmark_id = "med") {
  candidate_metrics <- intersect(metric_catalog$metric, names(df))
  numeric_metrics <- metric_catalog |>
    filter(metric %in% candidate_metrics, !format_type %in% c("date", "string")) |>
    pull(metric)
  
  bench_rows <- df |>
    group_by(date, yr, mo, qtr) |>
    summarise(
      across(any_of(numeric_metrics), ~ median(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    mutate(
      co_id = benchmark_id,
      co_num = NA_real_
    )
  
  for (nm in setdiff(names(df), names(bench_rows))) {
    bench_rows[[nm]] <- NA
  }
  bench_rows <- bench_rows[, names(df)]
  
  bind_rows(df, bench_rows)
}

# -----------------------------
# Column ordering
# -----------------------------
reorder_columns_from_catalog <- function(df, metric_catalog) {
  metric_order <- metric_catalog$metric
  keep_first <- intersect(metric_order, names(df))
  keep_rest  <- setdiff(names(df), keep_first)
  df |> select(all_of(c(keep_first, keep_rest)))
}

# -----------------------------
# Main build function
# -----------------------------
build_df_all <- function(df_raw, company_catalog, metric_catalog, report_layout_catalog) {
  validate_metric_catalog(metric_catalog)
  validate_report_layout_catalog(report_layout_catalog, metric_catalog)
  
  df_raw |>
    map_mcbook_columns_to_metrics(metric_catalog = metric_catalog) |>
    coerce_metric_columns(metric_catalog = metric_catalog) |>
    attach_company_id(company_catalog = company_catalog) |>
    apply_company_filters() |>
    add_time_fields() |>
    fill_numeric_nas(metric_catalog = metric_catalog) |>
    compute_all_formula_metrics(metric_catalog = metric_catalog) |>
    add_benchmark_rows(metric_catalog = metric_catalog, benchmark_id = "med") |>
    reorder_columns_from_catalog(metric_catalog = metric_catalog) |>
    arrange(desc(date), co_id)
}
