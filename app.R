# ============================================================
# app.R
# MCBOOK Analysis Explorer
# Metadata-driven report viewer
# ============================================================

library(shiny)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(lubridate)
library(ggplot2)
library(scales)
library(ggrepel)
library(janitor)
library(here)
library(purrr)

# ============================================================
# Catalog readers
# ============================================================
read_company_catalog <- function(path = here::here("data", "company_catalog.csv")) {
  readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names() |>
    mutate(
      co_id = as.character(company_id),
      display_name = as.character(display_name),
      color_hex = stringr::str_replace_all(color_hex, '"', ""),
      plot_order = as.integer(plot_order),
      is_benchmark = dplyr::coalesce(as.logical(is_benchmark), FALSE),
      show_in_app = dplyr::coalesce(as.logical(show_in_app), TRUE)
    ) |>
    arrange(plot_order)
}

read_metric_catalog <- function(path = here::here("data", "metric_catalog.csv")) {
  mc <- readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
  
  defaults <- list(
    metric = NA_character_,
    display_label_long = NA_character_,
    format_type = "number",
    aggregation_rule = "sum",
    higher_is_better = NA,
    show_in_table_default = NA,
    show_in_chart_default = NA,
    can_drill = NA,
    chart_family = NA_character_,
    report_sort_group = NA_character_,
    denominator_metric = NA_character_
  )
  
  for (nm in names(defaults)) {
    if (!nm %in% names(mc)) {
      mc[[nm]] <- defaults[[nm]]
    }
  }
  
  mc |>
    mutate(
      across(
        c(
          metric, display_label_long, format_type, aggregation_rule,
          chart_family, report_sort_group, denominator_metric
        ),
        ~ na_if(trimws(as.character(.x)), "")
      ),
      display_label_long = coalesce(display_label_long, metric),
      aggregation_rule = str_to_lower(coalesce(aggregation_rule, "sum")),
      higher_is_better = suppressWarnings(as.logical(higher_is_better)),
      show_in_table_default = suppressWarnings(as.logical(show_in_table_default)),
      show_in_chart_default = suppressWarnings(as.logical(show_in_chart_default)),
      can_drill = suppressWarnings(as.logical(can_drill))
    )
}

read_report_layout_catalog <- function(path = here::here("data", "report_layout_catalog.csv")) {
  rl <- readr::read_csv(path, show_col_types = FALSE) |>
    janitor::clean_names()
  
  dup_cols <- intersect(c("layout_row_id_1", "layout_row_id_4", "layout_row_id_12"), names(rl))
  if (length(dup_cols) > 0) {
    rl <- rl |> select(-all_of(dup_cols))
  }
  
  defaults <- list(
    metric = NA_character_,
    display_label_long = NA_character_,
    report_view = NA_character_,
    layout_row_id = NA_character_,
    section_order = NA_real_,
    metric_order_wi_section = NA_real_,
    row_kind = "detail",
    include_in_default_view = TRUE,
    drilldown_parent_metric = NA_character_,
    indent_level = 0,
    font = NA_character_,
    row_border_top = NA_character_,
    display_override_dollars = NA_character_,
    display_override_percent = NA_character_,
    display_override_rank = NA_character_
  )
  
  for (nm in names(defaults)) {
    if (!nm %in% names(rl)) {
      rl[[nm]] <- defaults[[nm]]
    }
  }
  
  rl |>
    mutate(
      across(
        c(
          metric, display_label_long, report_view, layout_row_id,
          row_kind, drilldown_parent_metric, font, row_border_top,
          display_override_dollars, display_override_percent, display_override_rank
        ),
        ~ na_if(trimws(as.character(.x)), "")
      ),
      section_order = suppressWarnings(as.numeric(section_order)),
      metric_order_wi_section = suppressWarnings(as.numeric(metric_order_wi_section)),
      row_kind = str_replace_all(str_to_lower(coalesce(row_kind, "detail")), "-", "_"),
      include_in_default_view = coalesce(as.logical(include_in_default_view), TRUE),
      indent_level = coalesce(as.numeric(indent_level), 0),
      font = str_to_lower(font),
      row_border_top = str_to_lower(row_border_top),
      display_override_dollars = str_to_lower(display_override_dollars),
      display_override_percent = str_to_lower(display_override_percent),
      display_override_rank = str_to_lower(display_override_rank)
    ) |>
    arrange(report_view, section_order, metric_order_wi_section)
}

# ============================================================
# General helpers
# ============================================================
aggregate_value_for_metric <- function(df, metric_name, metric_catalog) {
  if (is.na(metric_name) || metric_name == "" || !metric_name %in% names(df) || nrow(df) == 0) {
    return(NA_real_)
  }
  
  rule <- metric_catalog |>
    filter(metric == metric_name) |>
    pull(aggregation_rule)
  
  if (length(rule) == 0 || is.na(rule[[1]]) || rule[[1]] == "") {
    rule <- "sum"
  } else {
    rule <- tolower(rule[[1]])
  }
  
  x <- df[[metric_name]]
  
  if (rule %in% c("last", "ending", "end")) {
    return(dplyr::last(x))
  }
  if (rule %in% c("mean", "avg", "average")) {
    return(mean(x, na.rm = TRUE))
  }
  if (rule %in% c("median")) {
    return(median(x, na.rm = TRUE))
  }
  
  sum(x, na.rm = TRUE)
}

metric_display_label <- function(metric_name, metric_catalog) {
  out <- metric_catalog |>
    filter(metric == metric_name) |>
    pull(display_label_long)
  
  if (length(out) == 0 || is.na(out[[1]]) || out[[1]] == "") metric_name else out[[1]]
}

metric_format_type <- function(metric_name, metric_catalog) {
  out <- metric_catalog |>
    filter(metric == metric_name) |>
    pull(format_type)
  
  if (length(out) == 0 || is.na(out[[1]]) || out[[1]] == "") "number" else out[[1]]
}

metric_higher_is_better <- function(metric_name, metric_catalog) {
  out <- metric_catalog |>
    filter(metric == metric_name) |>
    pull(higher_is_better)
  
  if (length(out) == 0 || is.na(out[[1]])) TRUE else isTRUE(out[[1]])
}

metric_denominator_metric <- function(metric_name, metric_catalog) {
  out <- metric_catalog |>
    filter(metric == metric_name) |>
    pull(denominator_metric)
  
  if (length(out) == 0 || is.na(out[[1]]) || out[[1]] == "") NA_character_ else out[[1]]
}

format_statement_value <- function(value, format_type, display_mode = c("value", "percent", "rank")) {
  display_mode <- match.arg(display_mode)
  
  if (is.na(value)) return("")
  if (display_mode == "rank") return(as.character(as.integer(value)))
  if (display_mode == "percent") return(scales::percent(value, accuracy = 0.1))
  
  if (format_type %in% c("dollar", "dollars")) {
    if (value < 0) return(paste0("(", scales::dollar(abs(round(value, 0)), accuracy = 1), ")"))
    return(scales::dollar(round(value, 0), accuracy = 1))
  }
  if (format_type == "dollar2dec") {
    if (value < 0) return(paste0("(", scales::dollar(abs(value), accuracy = 0.01), ")"))
    return(scales::dollar(value, accuracy = 0.01))
  }
  if (format_type %in% c("pct1dec", "percent")) return(scales::percent(value, accuracy = 0.1))
  if (format_type == "pct2dec") return(scales::percent(value, accuracy = 0.01))
  if (format_type == "integer") return(format(round(value, 0), big.mark = ","))
  if (format_type == "num1dec") return(format(round(value, 1), nsmall = 1, big.mark = ","))
  if (format_type == "num2dec") return(format(round(value, 2), nsmall = 2, big.mark = ","))
  format(round(value, 1), big.mark = ",")
}

value_labeler <- function(format_type) {
  switch(
    format_type,
    "dollar" = scales::label_dollar(),
    "dollars" = scales::label_dollar(),
    "dollar2dec" = scales::label_dollar(accuracy = 0.01),
    "pct1dec" = scales::label_percent(accuracy = 0.1),
    "pct2dec" = scales::label_percent(accuracy = 0.01),
    "percent" = scales::label_percent(accuracy = 0.1),
    "integer" = scales::label_number(accuracy = 1, big.mark = ","),
    "num1dec" = scales::label_number(accuracy = 0.1, big.mark = ","),
    "num2dec" = scales::label_number(accuracy = 0.01, big.mark = ","),
    scales::label_number(big.mark = ",")
  )
}

compute_display_percent <- function(value, metric_name, company_slice, metric_catalog, report_view) {
  if (is.na(value)) return(NA_real_)
  
  fmt <- metric_format_type(metric_name, metric_catalog)
  denom_metric <- metric_denominator_metric(metric_name, metric_catalog)
  
  if (fmt %in% c("pct1dec", "pct2dec", "percent")) {
    return(value)
  }
  
  if (!is.na(denom_metric) && denom_metric %in% names(company_slice)) {
    denom_value <- aggregate_value_for_metric(company_slice, denom_metric, metric_catalog)
    if (is.na(denom_value) || denom_value == 0) return(NA_real_)
    return(value / denom_value)
  }
  
  if (report_view == "Profit & Loss Statement" && "rev_net_sales" %in% names(company_slice)) {
    denom_value <- aggregate_value_for_metric(company_slice, "rev_net_sales", metric_catalog)
    if (is.na(denom_value) || denom_value == 0) return(NA_real_)
    return(value / denom_value)
  }
  
  NA_real_
}

period_floor <- function(date, period) {
  dplyr::case_when(
    period == "month" ~ lubridate::floor_date(date, "month"),
    period == "quarter" ~ lubridate::floor_date(date, "quarter"),
    period == "year" ~ lubridate::floor_date(date, "year"),
    TRUE ~ lubridate::floor_date(date, "month")
  )
}

compute_y_limits <- function(df, pad_pct = 0.05) {
  if (nrow(df) == 0) return(c(0, 1))
  
  ymin <- suppressWarnings(min(df$value, na.rm = TRUE))
  ymax <- suppressWarnings(max(df$value, na.rm = TRUE))
  if (!is.finite(ymin) || !is.finite(ymax)) return(c(0, 1))
  
  pad <- (ymax - ymin) * pad_pct
  if (!is.finite(pad) || pad == 0) pad <- 1
  
  c(ymin - pad, ymax + pad)
}

date_scale_settings <- function(period, start_date, end_date) {
  lim <- c(
    as.Date(paste0(format(start_date, "%Y"), "-01-01")),
    as.Date(paste0(format(end_date, "%Y"), "-12-31"))
  )
  
  if (period == "month") {
    return(list(
      limits = lim,
      breaks = seq(lim[1], lim[2], by = "1 year"),
      minor_breaks = seq(lim[1], lim[2], by = "3 months"),
      labels = scales::label_date_short()
    ))
  }
  
  if (period == "quarter") {
    return(list(
      limits = lim,
      breaks = seq(lim[1], lim[2], by = "1 year"),
      minor_breaks = seq(lim[1], lim[2], by = "3 months"),
      labels = function(x) paste0("Q", lubridate::quarter(x), "\n", lubridate::year(x))
    ))
  }
  
  list(
    limits = lim,
    breaks = seq(lim[1], lim[2], by = "1 year"),
    minor_breaks = NULL,
    labels = function(x) format(x, "%Y")
  )
}

build_plot_theme <- function() {
  theme_gray() +
    theme(
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.title.x = element_text(size = 14, face = "bold"),
      axis.title.y = element_text(size = 14, face = "bold"),
      legend.position = "bottom",
      legend.title = element_text(size = 13, face = "bold"),
      legend.text = element_text(size = 11),
      plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 11),
      plot.margin = margin(10, 60, 10, 10)
    )
}

# ============================================================
# Metadata-driven layout helpers
# ============================================================
build_company_metadata <- function(df_all, company_catalog) {
  missing_ids <- setdiff(unique(df_all$co_id), company_catalog$co_id)
  
  if (length(missing_ids) > 0) {
    next_order <- if (all(is.na(company_catalog$plot_order))) 1 else max(company_catalog$plot_order, na.rm = TRUE) + 1
    
    add_rows <- tibble(
      co_id = missing_ids,
      display_name = toupper(missing_ids),
      plot_order = next_order + seq_along(missing_ids) - 1,
      color_hex = NA_character_,
      is_benchmark = missing_ids == "med",
      show_in_app = TRUE
    )
    
    company_catalog <- bind_rows(company_catalog, add_rows)
  }
  
  company_catalog <- company_catalog |>
    arrange(plot_order)
  
  missing_color_idx <- which(is.na(company_catalog$color_hex) | company_catalog$color_hex == "")
  if (length(missing_color_idx) > 0) {
    fallback_cols <- scales::hue_pal()(max(length(missing_color_idx), 3))
    company_catalog$color_hex[missing_color_idx] <- fallback_cols[seq_along(missing_color_idx)]
  }
  
  app_company_df <- company_catalog |>
    filter(show_in_app, !is_benchmark)
  
  list(
    catalog = company_catalog,
    co_order = company_catalog$co_id,
    co_colors = stats::setNames(company_catalog$color_hex, company_catalog$co_id),
    app_company_ids = app_company_df$co_id,
    app_company_choices = stats::setNames(app_company_df$co_id, app_company_df$display_name)
  )
}

report_views <- function(report_layout_catalog) {
  report_layout_catalog |>
    filter(!is.na(report_view), report_view != "") |>
    distinct(report_view) |>
    pull(report_view)
}

report_layout_for_view <- function(report_view_name, report_layout_catalog, metric_catalog) {
  report_layout_catalog |>
    filter(report_view == report_view_name) |>
    left_join(
      metric_catalog |>
        select(
          metric,
          mc_display_label_long = display_label_long,
          format_type,
          aggregation_rule,
          higher_is_better,
          denominator_metric,
          report_sort_group,
          chart_family
        ),
      by = "metric"
    ) |>
    mutate(
      display_label = coalesce(display_label_long, mc_display_label_long, metric, ""),
      format_type = coalesce(format_type, "number"),
      aggregation_rule = coalesce(aggregation_rule, "sum"),
      row_kind = str_replace_all(coalesce(row_kind, "detail"), "-", "_"),
      indent_level = coalesce(indent_level, 0),
      include_in_default_view = coalesce(include_in_default_view, TRUE),
      row_border_top = coalesce(row_border_top, "")
      ) |>
    arrange(section_order, metric_order_wi_section, layout_row_id)
}

report_metric_choices <- function(report_view_name, report_layout_catalog, metric_catalog, df_all) {
  report_metrics <- report_layout_catalog |>
    filter(report_view == report_view_name, !is.na(metric), metric != "") |>
    distinct(metric)
  
  out <- metric_catalog |>
    semi_join(report_metrics, by = "metric") |>
    filter(metric %in% names(df_all)) |>
    mutate(chart_default_rank = ifelse(isTRUE(show_in_chart_default), 0L, 1L)) |>
    arrange(chart_default_rank, report_sort_group, display_label_long)
  
  stats::setNames(out$metric, out$display_label_long)
}

report_stack_group_choices <- function(report_view_name, report_layout_catalog, metric_catalog, df_all) {
  report_metrics <- report_layout_catalog |>
    filter(report_view == report_view_name, !is.na(metric), metric != "") |>
    distinct(metric)
  
  out <- metric_catalog |>
    semi_join(report_metrics, by = "metric") |>
    filter(metric %in% names(df_all), chart_family == "stack_component", !is.na(report_sort_group)) |>
    count(report_sort_group, name = "n") |>
    filter(n > 1) |>
    arrange(report_sort_group)
  
  stats::setNames(out$report_sort_group, out$report_sort_group)
}

metrics_for_stack_group <- function(report_view_name, stack_group, report_layout_catalog, metric_catalog, df_all) {
  report_metrics <- report_layout_catalog |>
    filter(report_view == report_view_name, !is.na(metric), metric != "") |>
    distinct(metric)
  
  metric_catalog |>
    semi_join(report_metrics, by = "metric") |>
    filter(metric %in% names(df_all), chart_family == "stack_component", report_sort_group == stack_group) |>
    arrange(display_label_long) |>
    select(metric, display_label_long, report_sort_group)
}

aggregate_single_metric <- function(data, metric_name, period, metric_catalog) {
  stopifnot(metric_name %in% names(data))
  
  rule <- metric_catalog |>
    filter(metric == metric_name) |>
    pull(aggregation_rule)
  
  if (length(rule) == 0 || is.na(rule[[1]]) || rule[[1]] == "") {
    rule <- "sum"
  } else {
    rule <- tolower(rule[[1]])
  }
  
  data |>
    mutate(plot_date = period_floor(date, period)) |>
    arrange(co_id, date) |>
    group_by(plot_date, co_id) |>
    summarise(
      value = dplyr::case_when(
        rule %in% c("last", "ending", "end") ~ dplyr::last(.data[[metric_name]]),
        rule %in% c("mean", "avg", "average") ~ mean(.data[[metric_name]], na.rm = TRUE),
        rule %in% c("median") ~ median(.data[[metric_name]], na.rm = TRUE),
        TRUE ~ sum(.data[[metric_name]], na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

aggregate_stack_group <- function(data, report_view_name, stack_group, period, report_layout_catalog, metric_catalog) {
  stack_metrics <- metrics_for_stack_group(report_view_name, stack_group, report_layout_catalog, metric_catalog, data)
  
  if (nrow(stack_metrics) == 0) return(tibble())
  
  data |>
    mutate(plot_date = period_floor(date, period)) |>
    select(date, plot_date, co_id, any_of(stack_metrics$metric)) |>
    pivot_longer(
      cols = any_of(stack_metrics$metric),
      names_to = "metric",
      values_to = "value"
    ) |>
    left_join(stack_metrics, by = "metric") |>
    group_by(plot_date, co_id, metric, display_label_long, report_sort_group) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop")
}

resolve_display_override <- function(metric_name, row_override, display_mode) {
  metric_name <- dplyr::coalesce(as.character(metric_name), "")
  row_override <- dplyr::coalesce(as.character(row_override), "")
  display_mode <- dplyr::coalesce(as.character(display_mode), "dollars")
  
  # Normalize UI mode to renderer mode
  normalized_mode <- dplyr::case_when(
    display_mode == "dollars" ~ "value",
    display_mode == "percent" ~ "percent",
    display_mode == "rank" ~ "rank",
    TRUE ~ "value"
  )
  
  # Catalog-driven override wins
  if (row_override %in% c("value", "percent", "rank", "blank")) {
    return(row_override)
  }
  
  # Fallbacks until catalog is fully populated
  if (metric_name == "num_weeks") {
    if (normalized_mode == "rank") return("blank")
    return("value")
  }
  
  if (metric_name == "rev_net_sales") {
    if (normalized_mode == "percent") return("value")
    if (normalized_mode == "rank") return("blank")
  }
  
  normalized_mode
}

build_statement_table_data <- function(data, report_layout_df, metric_catalog, company_ids, rank_company_ids, report_date, basis, report_view_name) {
  report_date <- as.Date(report_date)
  
  if (basis == "mtd") {
    period_data_selected <- data |>
      filter(floor_date(date, "month") == floor_date(report_date, "month"), co_id %in% company_ids)
    
    period_data_rank <- data |>
      filter(floor_date(date, "month") == floor_date(report_date, "month"), co_id %in% rank_company_ids)
  } else {
    period_data_selected <- data |>
      filter(year(date) == year(report_date), date <= report_date, co_id %in% company_ids)
    
    period_data_rank <- data |>
      filter(year(date) == year(report_date), date <= report_date, co_id %in% rank_company_ids)
  }
  
  result <- report_layout_df
  
  for (cid in company_ids) {
    company_slice <- period_data_selected |>
      filter(co_id == cid)
    
    values <- purrr::map_dbl(seq_len(nrow(report_layout_df)), function(i) {
      metric_name <- report_layout_df$metric[[i]]
      aggregate_value_for_metric(company_slice, metric_name, metric_catalog)
    })
    
    percents <- purrr::map2_dbl(values, report_layout_df$metric, function(v, metric_name) {
      compute_display_percent(v, metric_name, company_slice, metric_catalog, report_view_name)
    })
    
    result[[paste0(cid, "__value")]] <- values
    result[[paste0(cid, "__pct")]] <- percents
    result[[paste0(cid, "__rank")]] <- NA_real_
  }
  
  for (j in seq_len(nrow(report_layout_df))) {
    metric_j <- report_layout_df$metric[j]
    if (is.na(metric_j) || metric_j == "") next
    
    metric_vals <- purrr::map_dbl(rank_company_ids, function(cid) {
      company_slice <- period_data_rank |>
        filter(co_id == cid)
      
      value_j <- aggregate_value_for_metric(company_slice, metric_j, metric_catalog)
      compute_display_percent(value_j, metric_j, company_slice, metric_catalog, report_view_name)
    })
    
    higher_is_better <- metric_higher_is_better(metric_j, metric_catalog)
    
    rank_tbl <- tibble(
      co_id = rank_company_ids,
      rank_value = if (higher_is_better) dplyr::dense_rank(dplyr::desc(metric_vals)) else dplyr::dense_rank(metric_vals)
    )
    
    for (cid in company_ids) {
      result[[paste0(cid, "__rank")]][j] <- rank_tbl$rank_value[match(cid, rank_tbl$co_id)]
    }
  }
  
  result
}

company_fill_css <- function(color_hex, alpha = 0.08) {
  grDevices::adjustcolor(color_hex, alpha.f = alpha)
}

rank_fill_css <- function(rank_value, n_companies) {
  if (is.na(rank_value) || is.na(n_companies) || n_companies < 2) return("")
  
  top_cut <- ceiling(n_companies * 0.25)
  bottom_cut <- floor(n_companies * 0.75) + 1
  
  if (rank_value <= top_cut) return("background:#cfe8cf;")
  if (rank_value >= bottom_cut) return("background:#f4cccc;")
  ""
}

border_top_css <- function(row_border_top) {
  row_border_top <- dplyr::coalesce(as.character(row_border_top), "")
  
  if (row_border_top == "thin") {
    return(" border-top:2px solid #444;")
  }
  if (row_border_top == "thick") {
    return(" border-top:4px solid #222;")
  }
  if (row_border_top == "double") {
    return(" border-top:3px double #222;")
  }
  
  ""
}

build_left_style <- function(indent_level, font_style, row_kind) {
  indent_level <- dplyr::coalesce(as.numeric(indent_level), 0)
  font_style <- dplyr::coalesce(as.character(font_style), "")
  row_kind <- dplyr::coalesce(as.character(row_kind), "detail")
  
  out <- paste0(
    "text-align:left; position:sticky; left:0; background:white; z-index:1;",
    "padding-left:", 8 + 18 * indent_level, "px;"
  )
  
  if (font_style == "bold" || row_kind %in% c("section_total", "sub_total", "total")) {
    out <- paste0(out, " font-weight:700;")
  }
  if (font_style == "italics") {
    out <- paste0(out, " font-style:italic;")
  }
  if (row_kind == "section_header") {
    out <- paste0(out, " background:#e9ecef;")
  }
  
  out
}

build_cell_style <- function(row_kind, bg_color, font_style, val = NA_real_, display_mode = "dollars", rank_val = NA_real_, pct = NA_real_, n_rank_companies = 0) {
  row_kind <- dplyr::coalesce(as.character(row_kind), "detail")
  font_style <- dplyr::coalesce(as.character(font_style), "")
  bg_color <- dplyr::coalesce(as.character(bg_color), "#ffffff")
  
  if (display_mode == "percent") {
    out <- paste0("text-align:right;", rank_fill_css(rank_val, n_rank_companies))
    if (!is.na(pct) && pct < 0) out <- paste0(out, " color:#c00000;")
    if (!is.na(rank_val) && rank_val == 1) out <- paste0(out, " font-weight:700;")
    return(out)
  }
  
  if (display_mode == "rank") {
    out <- paste0("text-align:right;", rank_fill_css(rank_val, n_rank_companies))
    if (!is.na(rank_val) && rank_val == 1) out <- paste0(out, " font-weight:700;")
    return(out)
  }
  
  out <- "text-align:right;"
  if (row_kind == "section_header") {
    out <- paste0(out, " background:#e9ecef;")
  } else {
    out <- paste0(out, " background:", bg_color, ";")
  }
  if (font_style == "bold" || row_kind %in% c("section_total", "sub_total", "total")) {
    out <- paste0(out, " font-weight:700;")
  }
  if (!is.na(val) && val < 0) {
    out <- paste0(out, " color:#c00000;")
  }
  
  out
}

render_statement_table_html <- function(statement_df, company_catalog, company_ids, display_mode, basis, report_date, co_colors, n_rank_companies, report_view_name) {
  if (nrow(statement_df) == 0) {
    return(tags$div("No statement data available for the selected options."))
  }
  
  display_names <- company_catalog$display_name[match(company_ids, company_catalog$co_id)]
  
  co_bg <- stats::setNames(
    vapply(company_ids, function(cid) company_fill_css(co_colors[[cid]], alpha = 0.08), character(1)),
    company_ids
  )
  co_bg_header <- stats::setNames(
    vapply(company_ids, function(cid) company_fill_css(co_colors[[cid]], alpha = 0.16), character(1)),
    company_ids
  )
  
  parent_metrics <- unique(statement_df$drilldown_parent_metric[!is.na(statement_df$drilldown_parent_metric)])
  parent_metrics <- setdiff(parent_metrics, "")
  
  body_rows <- lapply(seq_len(nrow(statement_df)), function(i) {
    rowi <- statement_df[i, ]
    
    row_kind <- dplyr::coalesce(as.character(rowi$row_kind[[1]]), "detail")
    metric_name <- rowi$metric[[1]]
    label <- rowi$display_label[[1]]
    indent_level <- rowi$indent_level[[1]]
    format_type <- rowi$format_type[[1]]
    font_style <- dplyr::coalesce(as.character(rowi$font[[1]]), "")
    show_default <- isTRUE(rowi$include_in_default_view[[1]])
    drill_parent <- rowi$drilldown_parent_metric[[1]]
    row_border_top <- dplyr::coalesce(as.character(rowi$row_border_top[[1]]), "")
    
    is_parent_toggle <- !is.na(metric_name) && metric_name %in% parent_metrics
    toggle_group <- if (is_parent_toggle) metric_name else drill_parent
    link_id <- if (!is.na(toggle_group) && toggle_group != "") paste0("toggle_", toggle_group) else NA_character_
    
    line_content <- if (is_parent_toggle) {
      tags$span(
        id = link_id,
        `data-label` = label,
        onclick = sprintf("toggleStatementGroup('%s', '%s')", toggle_group, link_id),
        style = "cursor:pointer; user-select:none;",
        HTML(paste0("&#9656; ", label))
      )
    } else {
      label
    }
    
    left_style <- paste0(
      build_left_style(indent_level, font_style, row_kind),
      border_top_css(row_border_top)
    )
    cells <- list(tags$td(line_content, style = left_style))
    
    for (cid in company_ids) {
      val <- rowi[[paste0(cid, "__value")]]
      pct <- rowi[[paste0(cid, "__pct")]]
      rank_val <- rowi[[paste0(cid, "__rank")]]
      
      row_override <- if (display_mode == "dollars") {
        if ("display_override_dollars" %in% names(rowi)) rowi$display_override_dollars[[1]] else NA_character_   # CHANGED
      } else if (display_mode == "percent") {
        if ("display_override_percent" %in% names(rowi)) rowi$display_override_percent[[1]] else NA_character_   # CHANGED
      } else {
        if ("display_override_rank" %in% names(rowi)) rowi$display_override_rank[[1]] else NA_character_         # CHANGED
      }
      
      resolved_mode <- resolve_display_override(metric_name, row_override, display_mode)
      
      style_mode <- if (resolved_mode == "blank") "dollars" else resolved_mode
      
      cell_style <- paste0(
        build_cell_style(
          row_kind = row_kind,
          bg_color = co_bg[[cid]],
          font_style = font_style,
          val = val,
          display_mode = style_mode,
          rank_val = rank_val,
          pct = pct,
          n_rank_companies = n_rank_companies
        ),
        border_top_css(row_border_top)
      )
      
      if (row_kind == "section_header" || resolved_mode == "blank") {
        cell_value <- ""
      } else if (resolved_mode == "value") {
        cell_value <- format_statement_value(val, format_type, "value")
      } else if (resolved_mode == "percent") {
        cell_value <- format_statement_value(pct, format_type, "percent")
      } else {
        cell_value <- format_statement_value(rank_val, format_type, "rank")
      }
      
      cells <- append(cells, list(tags$td(cell_value, style = cell_style)))
    }
    
    row_class <- if (!is.na(drill_parent) && drill_parent != "" && !show_default) paste0("detail-row-", drill_parent) else NULL
    row_style <- if (!is.na(drill_parent) && drill_parent != "" && !show_default) "display:none;" else NULL
    
    tags$tr(cells, class = row_class, style = row_style)
  })
  
  header_row_1 <- tags$tr(
    c(
      list(tags$td(report_view_name, style = "font-weight:700; background:#e9ecef; text-align:left; position:sticky; left:0; z-index:2;")),
      lapply(seq_along(company_ids), function(i) {
        cid <- company_ids[i]
        tags$td(display_names[i], style = paste0("font-weight:700; text-align:center; background:", co_bg_header[[cid]], ";"))
      })
    )
  )
  
  header_row_2 <- tags$tr(
    c(
      list(tags$td("", style = "background:#f8f9fa; position:sticky; left:0; z-index:2;")),
      lapply(company_ids, function(cid) {
        sub_label <- if (display_mode == "dollars") {
          ifelse(basis == "mtd", "MTD $", "YTD $")
        } else if (display_mode == "percent") {
          ifelse(basis == "mtd", "MTD %", "YTD %")
        } else {
          "Rank"
        }
        
        tags$td(sub_label, style = paste0("text-align:right; background:", co_bg_header[[cid]], ";"))
      })
    )
  )
  
  tags$div(
    style = "overflow-x:auto;",
    tags$div(
      style = "margin-bottom:8px; display:flex; align-items:center; gap:10px;",
      tags$div(
        style = "font-size:14px;",
        tags$b("Report month: "), format(as.Date(report_date), "%B %Y")
      ),
      tags$button(
        id = "expand_all_rows",
        type = "button",
        class = "btn btn-default btn-sm",
        "Expand all"
      ),
      tags$button(
        id = "collapse_all_rows",
        type = "button",
        class = "btn btn-default btn-sm",
        "Collapse all"
      )
    ),
    if (display_mode %in% c("percent", "rank")) {
      tags$div(
        style = "margin-bottom:8px; font-size:12px;",
        tags$span(
          style = "display:inline-block; padding:2px 8px; margin-right:8px; background:#cfe8cf; border:1px solid #b6d7a8;",
          "Top quartile"
        ),
        tags$span(
          style = "display:inline-block; padding:2px 8px; background:#f4cccc; border:1px solid #e6b8b7;",
          "Bottom quartile"
        )
      )
    },
    tags$table(
      class = "table table-sm table-bordered",
      style = "white-space:nowrap;",
      tags$tbody(header_row_1, header_row_2, body_rows)
    )
  )
}

# ============================================================
# Load data
# ============================================================
df_all <- readRDS(here::here("data", "df_all.rds"))

if (!inherits(df_all$date, "Date")) {
  df_all <- df_all |>
    mutate(date = as.Date(date))
}

company_catalog <- read_company_catalog()
metric_catalog <- read_metric_catalog()
report_layout_catalog <- read_report_layout_catalog()

company_meta <- build_company_metadata(df_all, company_catalog)

company_catalog <- company_meta$catalog
co_order <- company_meta$co_order
co_colors <- company_meta$co_colors
app_company_ids <- company_meta$app_company_ids
app_company_choices <- company_meta$app_company_choices
available_report_views <- report_views(report_layout_catalog)

default_report_view <- if ("Profit & Loss Statement" %in% available_report_views) "Profit & Loss Statement" else available_report_views[[1]]

available_years <- sort(unique(df_all$yr))
available_report_dates <- df_all |>
  filter(co_id != "med") |>
  distinct(date) |>
  arrange(date) |>
  pull(date)

default_year_range <- c(min(available_years), max(available_years))
default_report_date <- max(available_report_dates)
default_report_year <- lubridate::year(default_report_date)
default_report_month <- lubridate::month(default_report_date)

plot_theme <- build_plot_theme()

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  tags$head(
    tags$script(HTML("
      window.toggleStatementGroup = function(groupId, linkId) {
        var rows = document.querySelectorAll('.detail-row-' + groupId);
        var link = document.getElementById(linkId);
        var isHidden = true;

        rows.forEach(function(r) {
          if (window.getComputedStyle(r).display !== 'none') {
            isHidden = false;
          }
        });

        rows.forEach(function(r) {
          r.style.display = isHidden ? 'table-row' : 'none';
        });

        if (link) {
          var baseLabel = link.getAttribute('data-label');
          link.innerHTML = (isHidden ? '&#9662; ' : '&#9656; ') + baseLabel;
        }
      };

      window.expandAllStatementGroups = function() {
        var links = document.querySelectorAll('[id^=\"toggle_\"]');
        links.forEach(function(link) {
          var groupId = link.id.replace('toggle_', '');
          var rows = document.querySelectorAll('.detail-row-' + groupId);
          rows.forEach(function(r) { r.style.display = 'table-row'; });
          var baseLabel = link.getAttribute('data-label');
          link.innerHTML = '&#9662; ' + baseLabel;
        });
      };

      window.collapseAllStatementGroups = function() {
        var links = document.querySelectorAll('[id^=\"toggle_\"]');
        links.forEach(function(link) {
          var groupId = link.id.replace('toggle_', '');
          var rows = document.querySelectorAll('.detail-row-' + groupId);
          rows.forEach(function(r) { r.style.display = 'none'; });
          var baseLabel = link.getAttribute('data-label');
          link.innerHTML = '&#9656; ' + baseLabel;
        });
      };

      document.addEventListener('click', function(e) {
        if (e.target && e.target.id === 'expand_all_rows') {
          window.expandAllStatementGroups();
        }
        if (e.target && e.target.id === 'collapse_all_rows') {
          window.collapseAllStatementGroups();
        }
      });
    ")),
    tags$style(HTML("
      body, .table, .btn, .form-control {
        font-family: 'Segoe UI', Tahoma, Arial, sans-serif;
      }
    
      .table td, .table th {
        font-weight: 500;
      }
    
      .table td strong, .table th strong {
        font-weight: 700;
      }
    
      .btn-group-container-sw .btn-group .btn {
        padding: 3px 6px;
        font-size: 12px;
        line-height: 1.15;
        min-width: 34px;
      }
    
      .btn-group-container-sw label.btn {
        margin-bottom: 2px;
      }
    
      .shiny-input-container {
        margin-bottom: 10px;
      }
    "))
  ),
  
  titlePanel("MCBOOK Analysis Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      pickerInput(
        inputId = "report_view",
        label = "Report view",
        choices = available_report_views,
        selected = default_report_view
      ),
      
      pickerInput(
        inputId = "analysis_mode",
        label = "Display",
        choices = c(
          "Statement table" = "statement_table",
          "Metric trend" = "metric_trend",
          "Component stack" = "component_stack"
        ),
        selected = "statement_table"
      ),
      
      conditionalPanel(
        condition = "input.analysis_mode != 'statement_table'",
        sliderInput(
          inputId = "year_range",
          label = "Year range",
          min = min(available_years),
          max = max(available_years),
          value = default_year_range,
          step = 1,
          sep = ""
        ),
        radioButtons(
          inputId = "time_agg",
          label = "Time aggregation",
          choices = c("Monthly" = "month", "Quarterly" = "quarter", "Annual" = "year"),
          selected = "quarter",
          inline = TRUE
        )
      ),
      
      checkboxInput(
        inputId = "include_benchmark",
        label = "Include benchmark median",
        value = FALSE
      ),
      
      pickerInput(
        inputId = "companies",
        label = "Companies",
        choices = app_company_choices,
        selected = app_company_ids,
        multiple = TRUE,
        options = list(
          `actions-box` = TRUE,
          `live-search` = TRUE,
          size = 10,
          `selected-text-format` = "count > 3"
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis_mode == 'metric_trend'",
        pickerInput(
          inputId = "metric_name",
          label = "Metric",
          choices = NULL,
          selected = NULL,
          options = list(`live-search` = TRUE)
        ),
        radioButtons(
          inputId = "y_scale_basis",
          label = "Set y-axis range using",
          choices = c("All filtered companies" = "all", "Selected companies only" = "selected"),
          selected = "all"
        ),
        prettySwitch(
          inputId = "show_labels",
          label = "Show end labels",
          value = FALSE,
          status = "primary"
        ),
        prettySwitch(
          inputId = "show_trendlines",
          label = "Show trendlines",
          value = TRUE,
          status = "primary"
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis_mode == 'component_stack'",
        pickerInput(
          inputId = "stack_group",
          label = "Component group",
          choices = NULL,
          selected = NULL
        ),
        radioButtons(
          inputId = "stack_position",
          label = "Display mode",
          choices = c("Stacked values" = "stack", "100% stacked share" = "fill"),
          selected = "stack",
          inline = TRUE
        )
      ),
      
      conditionalPanel(
        condition = "input.analysis_mode == 'statement_table'",
        fluidRow(
          column(
            width = 6,
            radioGroupButtons(
              inputId = "report_year",
              label = "Report year",
              choices = sort(unique(lubridate::year(available_report_dates))),
              selected = default_report_year,
              justified = FALSE,
              size = "sm",
              direction = "vertical",
              status = "default",
              checkIcon = list(yes = icon("check"))
            )
          ),
          column(
            width = 6,
            radioGroupButtons(
              inputId = "report_month",
              label = "Report month",
              choices = stats::setNames(1:12, month.abb),
              selected = default_report_month,
              justified = FALSE,
              size = "sm",
              direction = "vertical",
              status = "default",
              checkIcon = list(yes = icon("check"))
            )
          )
        ),
        
        radioButtons(
          inputId = "statement_basis",
          label = "Basis",
          choices = c("Month-to-date" = "mtd", "Year-to-date" = "ytd"),
          selected = "mtd",
          inline = TRUE
        ),
        
        conditionalPanel(
          condition = "input.report_view == 'Profit & Loss Statement'",
          radioButtons(
            inputId = "statement_display_mode",
            label = "Display",
            choices = c("Dollars" = "dollars", "% of Net Sales" = "percent", "Peer Rank (%NS)" = "rank"),
            selected = "percent",
            inline = TRUE
          )
        ),
        
        conditionalPanel(
          condition = "input.report_view != 'Profit & Loss Statement'",
          radioButtons(
            inputId = "statement_display_mode_other",
            label = "Display",
            choices = c("Values" = "dollars"),
            selected = "dollars",
            inline = TRUE
          )
        )
      )
    ),
    
    mainPanel(
      width = 9,
      conditionalPanel(
        condition = "input.analysis_mode != 'statement_table'",
        plotOutput("main_plot", height = "700px")
      ),
      conditionalPanel(
        condition = "input.analysis_mode == 'statement_table'",
        uiOutput("statement_table_ui")
      )
    )
  )
)

# ============================================================
# Server
# ============================================================
server <- function(input, output, session) {
  
  observeEvent(input$report_view, {
    metric_choices_now <- report_metric_choices(input$report_view, report_layout_catalog, metric_catalog, df_all)
    default_metric <- if (length(metric_choices_now) > 0) unname(metric_choices_now)[1] else NULL
    updatePickerInput(session, "metric_name", choices = metric_choices_now, selected = default_metric)
    
    stack_choices_now <- report_stack_group_choices(input$report_view, report_layout_catalog, metric_catalog, df_all)
    default_stack <- if (length(stack_choices_now) > 0) unname(stack_choices_now)[1] else NULL
    updatePickerInput(session, "stack_group", choices = stack_choices_now, selected = default_stack)
  }, ignoreInit = FALSE)
  
  start_date <- reactive({
    as.Date(paste0(input$year_range[1], "-01-01"))
  })
  
  end_date <- reactive({
    as.Date(paste0(input$year_range[2], "-12-31"))
  })
  
  selected_company_ids <- reactive({
    ids <- input$companies
    if (isTRUE(input$include_benchmark) && "med" %in% co_order) {
      ids <- unique(c("med", ids))
    }
    ids
  })
  
  base_filtered_data <- reactive({
    df_all |>
      filter(date >= start_date(), date <= end_date())
  })
  
  selected_filtered_data <- reactive({
    base_filtered_data() |>
      filter(co_id %in% selected_company_ids())
  })
  
  current_report_layout <- reactive({
    report_layout_for_view(input$report_view, report_layout_catalog, metric_catalog)
  })
  
  metric_plot_data <- reactive({
    req(input$metric_name)
    req(input$metric_name %in% names(df_all))
    
    aggregate_single_metric(
      data = selected_filtered_data(),
      metric_name = input$metric_name,
      period = input$time_agg,
      metric_catalog = metric_catalog
    ) |>
      mutate(co_id = factor(co_id, levels = co_order))
  }) |>
    bindCache(input$report_view, input$metric_name, input$time_agg, input$year_range, input$companies, input$include_benchmark)
  
  all_metric_plot_data <- reactive({
    req(input$metric_name)
    req(input$metric_name %in% names(df_all))
    
    all_ids <- company_catalog |>
      filter(show_in_app) |>
      pull(co_id)
    
    aggregate_single_metric(
      data = base_filtered_data() |> filter(co_id %in% all_ids),
      metric_name = input$metric_name,
      period = input$time_agg,
      metric_catalog = metric_catalog
    )
  }) |>
    bindCache(input$report_view, input$metric_name, input$time_agg, input$year_range)
  
  stack_plot_data <- reactive({
    req(input$stack_group)
    
    aggregate_stack_group(
      data = selected_filtered_data(),
      report_view_name = input$report_view,
      stack_group = input$stack_group,
      period = input$time_agg,
      report_layout_catalog = report_layout_catalog,
      metric_catalog = metric_catalog
    ) |>
      mutate(co_id = factor(co_id, levels = co_order))
  }) |>
    bindCache(input$report_view, input$stack_group, input$time_agg, input$year_range, input$companies, input$include_benchmark)
  
  current_y_limits <- reactive({
    if (input$analysis_mode != "metric_trend") return(NULL)
    if (input$y_scale_basis == "selected") return(compute_y_limits(metric_plot_data()))
    compute_y_limits(all_metric_plot_data())
  })
  
  xset <- reactive({
    date_scale_settings(input$time_agg, start_date(), end_date())
  })
  
  label_data <- reactive({
    metric_plot_data() |>
      arrange(co_id, plot_date) |>
      group_by(co_id) |>
      slice_tail(n = 1) |>
      ungroup()
  })
  
  statement_date_company_ids <- reactive({
    ids <- intersect(input$companies, app_company_ids)
    if (length(ids) == 0) ids <- app_company_ids
    
    ids_with_data <- df_all |>
      filter(co_id %in% ids) |>
      distinct(co_id) |>
      pull(co_id)
    
    if (length(ids_with_data) == 0) ids_with_data <- app_company_ids
    ids_with_data
  })
  
  available_statement_dates <- reactive({
    dates <- df_all |>
      filter(co_id %in% statement_date_company_ids()) |>
      distinct(date) |>
      arrange(date) |>
      pull(date)
    
    if (length(dates) == 0) dates <- available_report_dates
    dates
  })
  
  available_statement_years <- reactive({
    yrs <- sort(unique(lubridate::year(available_statement_dates())))
    if (length(yrs) == 0) yrs <- sort(unique(lubridate::year(available_report_dates)))
    yrs
  })
  
  observeEvent(list(input$companies, input$analysis_mode), {
    req(input$analysis_mode == "statement_table")
    
    valid_years <- available_statement_years()
    req(length(valid_years) > 0)
    
    selected_year <- suppressWarnings(as.integer(input$report_year))
    if (is.na(selected_year) || !selected_year %in% valid_years) {
      selected_year <- max(valid_years)
    }
    
    updateRadioGroupButtons(
      session = session,
      inputId = "report_year",
      choices = as.character(valid_years),
      selected = as.character(selected_year)
    )
  }, ignoreInit = FALSE)
  
  observeEvent(list(input$report_year, input$companies, input$analysis_mode), {
    req(input$analysis_mode == "statement_table")
    req(input$report_year)
    
    year_num <- suppressWarnings(as.integer(input$report_year))
    req(!is.na(year_num))
    
    valid_months <- available_statement_dates()[
      lubridate::year(available_statement_dates()) == year_num
    ] |>
      lubridate::month() |>
      unique() |>
      sort()
    
    if (length(valid_months) == 0) {
      valid_months <- sort(unique(lubridate::month(available_report_dates)))
    }
    
    selected_month <- suppressWarnings(as.integer(input$report_month))
    if (is.na(selected_month) || !selected_month %in% valid_months) {
      selected_month <- max(valid_months)
    }
    
    updateRadioGroupButtons(
      session = session,
      inputId = "report_month",
      choices = stats::setNames(as.character(valid_months), month.abb[valid_months]),
      selected = as.character(selected_month)
    )
  }, ignoreInit = FALSE)
  
  report_date_selected <- reactive({
    req(input$report_year, input$report_month)
    
    report_year_num <- suppressWarnings(as.integer(input$report_year))
    report_month_num <- suppressWarnings(as.integer(input$report_month))
    
    validate(
      need(!is.na(report_year_num), "Invalid report year."),
      need(!is.na(report_month_num), "Invalid report month.")
    )
    
    candidate <- as.Date(sprintf("%04d-%02d-01", report_year_num, report_month_num)) |>
      lubridate::ceiling_date("month") - lubridate::days(1)
    
    available_in_month <- available_statement_dates()[
      lubridate::year(available_statement_dates()) == report_year_num &
        lubridate::month(available_statement_dates()) == report_month_num
    ]
    
    if (length(available_in_month) > 0) return(max(available_in_month))
    candidate
  })
  
  statement_display_mode_current <- reactive({
    if (input$report_view == "Profit & Loss Statement") {
      return(input$statement_display_mode)
    }
    "dollars"
  })
  
  statement_table_data <- reactive({
    req(input$report_year, input$report_month)
    req(length(selected_company_ids()) > 0)
    
    validate(
      need(
        nrow(df_all |> filter(co_id %in% selected_company_ids(), date == report_date_selected())) > 0,
        "No data for the selected companies in this period."
      )
    )
    
    build_statement_table_data(
      data = df_all,
      report_layout_df = current_report_layout(),
      metric_catalog = metric_catalog,
      company_ids = selected_company_ids(),
      rank_company_ids = app_company_ids,
      report_date = report_date_selected(),
      basis = input$statement_basis,
      report_view_name = input$report_view
    )
  })
  
  output$statement_table_ui <- renderUI({
    validate(need(length(selected_company_ids()) > 0, "Select at least one company."))
    
    render_statement_table_html(
      statement_df = statement_table_data(),
      company_catalog = company_catalog,
      company_ids = selected_company_ids(),
      display_mode = statement_display_mode_current(),
      basis = input$statement_basis,
      report_date = report_date_selected(),
      co_colors = co_colors,
      n_rank_companies = length(app_company_ids),
      report_view_name = input$report_view
    )
  })
  
  output$main_plot <- renderPlot({
    if (input$analysis_mode == "metric_trend") {
      pdat <- metric_plot_data()
      validate(need(nrow(pdat) > 0, "No data available for the selected options."))
      
      current_metric_label <- metric_display_label(input$metric_name, metric_catalog)
      current_format_type <- metric_format_type(input$metric_name, metric_catalog)
      
      p_nonbenchmark <- pdat |> filter(co_id != "med")
      p_benchmark <- pdat |> filter(co_id == "med")
      
      p <- ggplot() +
        geom_point(data = p_nonbenchmark, aes(x = plot_date, y = value, color = co_id), size = 2) +
        geom_line(data = p_nonbenchmark, aes(x = plot_date, y = value, color = co_id, group = co_id), linewidth = 1) +
        geom_point(data = p_benchmark, aes(x = plot_date, y = value, color = co_id), size = 3) +
        geom_line(data = p_benchmark, aes(x = plot_date, y = value, color = co_id, group = co_id), linewidth = 1.5) +
        scale_color_manual(
          values = co_colors,
          breaks = co_order,
          labels = company_catalog$display_name[match(co_order, company_catalog$co_id)],
          drop = TRUE
        ) +
        scale_x_date(
          limits = xset()$limits,
          breaks = xset()$breaks,
          minor_breaks = xset()$minor_breaks,
          labels = xset()$labels
        ) +
        scale_y_continuous(labels = value_labeler(current_format_type)) +
        labs(
          title = current_metric_label,
          subtitle = paste(tools::toTitleCase(input$time_agg), "view"),
          x = NULL,
          y = current_metric_label,
          color = "Company"
        ) +
        plot_theme
      
      if (isTRUE(input$show_trendlines)) {
        if (nrow(p_nonbenchmark) > 1) {
          p <- p + geom_smooth(
            data = p_nonbenchmark,
            aes(x = plot_date, y = value, color = co_id, group = co_id),
            method = "lm",
            formula = y ~ x,
            se = FALSE,
            linetype = "dotted",
            linewidth = 0.9
          )
        }
        if (nrow(p_benchmark) > 1) {
          p <- p + geom_smooth(
            data = p_benchmark,
            aes(x = plot_date, y = value, color = co_id, group = co_id),
            method = "lm",
            formula = y ~ x,
            se = FALSE,
            linetype = "dotted",
            linewidth = 1.1
          )
        }
      }
      
      if (isTRUE(input$show_labels) && nrow(label_data()) > 0) {
        p <- p +
          geom_label_repel(
            data = label_data(),
            aes(x = plot_date, y = value, label = co_id, color = co_id),
            size = 3.2,
            direction = "y",
            hjust = 0,
            nudge_x = 30,
            box.padding = 0.35,
            point.padding = 0.2,
            label.size = 0.2,
            show.legend = FALSE,
            max.overlaps = Inf
          ) +
          coord_cartesian(
            ylim = current_y_limits(),
            xlim = c(xset()$limits[1], xset()$limits[2] + 60),
            clip = "off"
          )
      } else {
        p <- p + coord_cartesian(ylim = current_y_limits())
      }
      
      print(p)
    }
    
    if (input$analysis_mode == "component_stack") {
      pdat <- stack_plot_data()
      validate(need(nrow(pdat) > 0, "No data available for the selected options."))
      
      y_lab <- if (input$stack_position == "fill") "Share of total" else "Value"
      
      p <- ggplot(pdat, aes(x = plot_date, y = value, fill = display_label_long)) +
        geom_col(position = input$stack_position, width = 25) +
        facet_wrap(~ co_id, ncol = 2, scales = "free_y") +
        scale_x_date(
          limits = xset()$limits,
          breaks = xset()$breaks,
          minor_breaks = xset()$minor_breaks,
          labels = xset()$labels
        ) +
        labs(
          title = paste(input$report_view, "-", input$stack_group),
          subtitle = paste(tools::toTitleCase(input$time_agg), "view"),
          x = NULL,
          y = y_lab,
          fill = "Component"
        ) +
        plot_theme
      
      if (input$stack_position == "fill") {
        p <- p + scale_y_continuous(labels = scales::label_percent())
      } else {
        p <- p + scale_y_continuous(labels = scales::label_dollar())
      }
      
      print(p)
    }
  }, res = 110)
}

shinyApp(ui, server)
