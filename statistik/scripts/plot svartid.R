#!/usr/bin/env Rscript

# plot svartider timeplot.R
#
# This script is sourced from the Shiny server with all toggles
# (date range, timeGranularity, regionToggle, svartypeFilterToggle, etc.)
# defined in the local environment.
#
# It must create a ggplot object named: p

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)
  library(here)
  library(lubridate)
  library(tibble)
  library(grid)
})

message("✓ Pakker til 'svartider'-plot er indlæst")

# -----------------------------------------------------------------------------
# 0. Fallbacks (safe defaults if not set by Shiny)
# -----------------------------------------------------------------------------

if (!exists("start_year.timePlot"))  start_year.timePlot  <- 2023
if (!exists("start_month.timePlot")) start_month.timePlot <- 1
if (!exists("start_day.timePlot"))   start_day.timePlot   <- 1

if (!exists("end_year.timePlot"))    end_year.timePlot    <- as.integer(format(Sys.Date(), "%Y"))
if (!exists("end_month.timePlot"))   end_month.timePlot   <- as.integer(format(Sys.Date(), "%m"))
if (!exists("end_day.timePlot"))     end_day.timePlot     <- as.integer(format(Sys.Date(), "%d"))

if (!exists("timeGranularity.timePlot")) timeGranularity.timePlot <- "Måned"
if (!exists("regionToggle.timePlot"))    regionToggle.timePlot    <- "Begge"
if (!exists("svartypeFilterToggle.timePlot")) svartypeFilterToggle.timePlot <- "Alle"

if (!exists("svartidTypeToggle")) svartidTypeToggle <- "Hverdage u. helligdage og weekend"

if (!exists("graf1_percentile")) graf1_percentile <- 90
if (!exists("graf2_percentile")) graf2_percentile <- 50
if (!exists("graf3_percentile")) graf3_percentile <- "Ingen"

# New toggle: show mean line
if (!exists("showMeanToggle")) showMeanToggle <- FALSE

# Mellow but fixed mappings (black/blue/orange families)
graf1_color <- "#2F2F2F"  # mellow charcoal
graf2_color <- "#4F6FA6"  # mellow blue
graf3_color <- "#C97A2B"  # mellow orange

# New: mellow red for mean
mean_color  <- "#B35C5C"  # mellow red

# -----------------------------------------------------------------------------
# 1. Load data (same pattern as other scripts)
# -----------------------------------------------------------------------------

source(file.path(here("statistik", "scripts"), "load downloaded data.R"))

message("Checking for data...")

if (!exists("data.lmraad_filtered")) {
  stop("data.lmraad_filtered not found in environment. Please run 'download, prepare, save.R' first.")
}

message("✓ Data found: ", nrow(data.lmraad_filtered), " rows")

# -----------------------------------------------------------------------------
# 2. Helpers
# -----------------------------------------------------------------------------

allowed_percentiles <- c(20, 40, 50, 75, 80, 90, 100)

percentile_label <- function(pct) {
  if (isTRUE(identical(pct, 50))) "Median" else paste0(pct, "-percentil")
}

resolve_svartid_col <- function(toggle) {
  if (identical(toggle, "Alle dage")) {
    "svartid.raw"
  } else if (identical(toggle, "Hverdage m. helligdage")) {
    "svartid.NoWeekend"
  } else if (identical(toggle, "Hverdage u. helligdage og weekend")) {
    "svartid.NoWeekendNoHolidays"
  } else {
    stop("Invalid svartidTypeToggle. Allowed: 'Alle dage', 'Hverdage m. helligdage', 'Hverdage u. helligdage og weekend'.")
  }
}

validate_percentile_choice <- function(x) {
  if (identical(x, "Ingen")) return(TRUE)
  if (is.numeric(x) && length(x) == 1 && x %in% allowed_percentiles) return(TRUE)
  stop("Invalid percentile choice. Allowed: 'Ingen' or one of: ", paste(allowed_percentiles, collapse = ", "))
}

validate_percentile_choice(graf1_percentile)
validate_percentile_choice(graf2_percentile)
validate_percentile_choice(graf3_percentile)

svartid_col <- resolve_svartid_col(svartidTypeToggle)
if (!svartid_col %in% names(data.lmraad_filtered)) {
  stop("Chosen svartid column not found in data: ", svartid_col)
}

# -----------------------------------------------------------------------------
# 3. Filtering (regionToggle.timePlot and svartypeFilterToggle.timePlot)
# -----------------------------------------------------------------------------

filtered_data <- data.lmraad_filtered

if (!identical(regionToggle.timePlot, "Begge")) {
  filtered_data <- filtered_data %>%
    dplyr::filter(`Region (*)` == regionToggle.timePlot)
}

if (!identical(svartypeFilterToggle.timePlot, "Alle")) {
  if (identical(svartypeFilterToggle.timePlot, "Klinisk rådgivning")) {
    filtered_data <- filtered_data %>%
      dplyr::filter(`Svartype (*)` %in% c(
        "Almindeligt svar",
        "Kortsvar",
        "Generel forespørgsel"
      ))
  } else {
    filtered_data <- filtered_data %>%
      dplyr::filter(`Svartype (*)` == svartypeFilterToggle.timePlot)
  }
}

# -----------------------------------------------------------------------------
# 4. Weekly base (focus on AdjustedDate)
# -----------------------------------------------------------------------------

weekly_base <- filtered_data %>%
  mutate(
    AdjustedDate = as.Date(AdjustedDate),

    year_adj  = suppressWarnings(as.integer(Year)),
    month_adj = factor(
      suppressWarnings(as.integer(Month)),
      levels = 1:12,
      labels = c("Jan", "Feb", "Mar", "Apr", "Maj", "Jun",
                 "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")
    ),
    month_num = suppressWarnings(as.integer(Month)),
    quarter_num = case_when(
      month_num <= 3 ~ 1L,
      month_num <= 6 ~ 2L,
      month_num <= 9 ~ 3L,
      TRUE ~ 4L
    ),
    quarter_adj = factor(quarter_num, levels = 1:4, labels = c("K1", "K2", "K3", "K4"))
  ) %>%
  filter(!is.na(AdjustedDate))

# -----------------------------------------------------------------------------
# 5. Time range filtering (based on AdjustedDate)
# -----------------------------------------------------------------------------

start_date <- as.Date(paste(start_year.timePlot, start_month.timePlot, start_day.timePlot, sep = "-"))
end_date   <- as.Date(paste(end_year.timePlot,   end_month.timePlot,   end_day.timePlot,   sep = "-"))

weekly_filtered <- weekly_base %>%
  filter(
    !is.na(AdjustedDate),
    AdjustedDate >= start_date,
    AdjustedDate <= end_date
  )

# -----------------------------------------------------------------------------
# 6. Period grid (same logic as reference)
# -----------------------------------------------------------------------------

period_grid <- NULL

if (timeGranularity.timePlot == "Uge") {
  week_seq <- seq.Date(
    from = floor_date(start_date, "week", week_start = 1),
    to   = floor_date(end_date,   "week", week_start = 1),
    by   = "week"
  )

  iso_year_seq <- lubridate::isoyear(week_seq)
  iso_week_seq <- lubridate::isoweek(week_seq)

  period_grid <- tibble::tibble(
    iso_year       = iso_year_seq,
    iso_week       = iso_week_seq,
    year_adj       = iso_year_seq,
    period_label   = sprintf("%d-W%02d", iso_year_seq, iso_week_seq),
    period_display = sprintf("%02d", iso_week_seq)
  ) %>%
    dplyr::distinct(iso_year, iso_week, .keep_all = TRUE)

} else if (timeGranularity.timePlot == "Måned") {
  month_seq <- seq.Date(
    from = floor_date(start_date, "month"),
    to   = floor_date(end_date,   "month"),
    by   = "month"
  )

  month_num_seq <- lubridate::month(month_seq)
  month_adj_seq <- factor(
    month_num_seq,
    levels = 1:12,
    labels = c("Jan", "Feb", "Mar", "Apr", "Maj", "Jun",
               "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")
  )

  period_grid <- tibble::tibble(
    year_adj       = lubridate::year(month_seq),
    month_adj      = month_adj_seq,
    period_label   = sprintf("%d-%s", year_adj, month_adj),
    period_display = as.character(month_adj)
  )

} else if (timeGranularity.timePlot == "Kvartal") {
  start_quarter_date <- as.Date(paste(start_year.timePlot, ((start_month.timePlot - 1) %/% 3) * 3 + 1, 1, sep = "-"))
  end_quarter_date   <- as.Date(paste(end_year.timePlot,   ((end_month.timePlot  - 1) %/% 3) * 3 + 1, 1, sep = "-"))

  quarter_seq <- seq.Date(start_quarter_date, end_quarter_date, by = "quarter")

  quarter_num_seq <- lubridate::quarter(quarter_seq)
  quarter_adj_seq <- factor(quarter_num_seq, levels = 1:4, labels = c("K1", "K2", "K3", "K4"))

  period_grid <- tibble::tibble(
    year_adj       = lubridate::year(quarter_seq),
    quarter_num    = quarter_num_seq,
    quarter_adj    = quarter_adj_seq,
    period_label   = sprintf("%d-%s", year_adj, quarter_adj),
    period_display = as.character(quarter_adj)
  )

} else if (timeGranularity.timePlot == "År") {
  year_seq <- seq(from = start_year.timePlot, to = end_year.timePlot, by = 1)

  period_grid <- tibble::tibble(
    year_adj       = year_seq,
    period_label   = as.character(year_seq),
    period_display = as.character(year_seq)
  )

} else {
  stop("Invalid timeGranularity.timePlot. Allowed: 'Uge', 'Måned', 'Kvartal', 'År'.")
}

period_levels <- period_grid$period_label

period_data <- period_grid %>%
  mutate(
    period_label = factor(period_label, levels = period_levels),
    period_index = as.numeric(period_label)
  ) %>%
  distinct(period_label, .keep_all = TRUE) %>%
  arrange(period_index)

n_periods <- length(period_levels)

if (n_periods <= 50L) {
  x_labels_map <- setNames(period_data$period_display, period_data$period_label)
} else {
  step    <- ceiling(n_periods / 50L)
  show_ix <- seq(1L, n_periods, by = step)
  lbl_vec <- rep("", n_periods)
  lbl_vec[show_ix] <- period_data$period_display[show_ix]
  x_labels_map <- setNames(lbl_vec, period_data$period_label)
}

# Always show small x-ticks
axis_ticks_x <- element_line(colour = "black", linewidth = 0.2)

# Year highlighting/labels (same logic as reference)
if (timeGranularity.timePlot != "År") {
  year_data <- period_data %>%
    group_by(year_adj) %>%
    summarise(
      xmin    = min(period_index) - 0.43,
      xmax    = max(period_index) + 0.43,
      xcenter = mean(period_index),
      .groups = "drop"
    )
} else {
  year_data <- NULL
}

# -----------------------------------------------------------------------------
# 7. Bin observations to periods
# -----------------------------------------------------------------------------

binned <- weekly_filtered

if (timeGranularity.timePlot == "Uge") {
  binned <- binned %>%
    mutate(
      iso_year     = lubridate::isoyear(AdjustedDate),
      iso_week     = lubridate::isoweek(AdjustedDate),
      period_label = sprintf("%d-W%02d", iso_year, iso_week)
    )
} else if (timeGranularity.timePlot == "Måned") {
  binned <- binned %>%
    mutate(period_label = sprintf("%d-%s", year_adj, month_adj))
} else if (timeGranularity.timePlot == "Kvartal") {
  binned <- binned %>%
    mutate(period_label = sprintf("%d-%s", year_adj, quarter_adj))
} else if (timeGranularity.timePlot == "År") {
  binned <- binned %>%
    mutate(period_label = as.character(year_adj))
}

binned <- binned %>%
  mutate(period_label = factor(period_label, levels = period_levels))

# -----------------------------------------------------------------------------
# 8. Percentile lines (Graf 1-3) + optional mean line
# -----------------------------------------------------------------------------

series_specs <- tibble::tibble(
  graf = c("Graf 1", "Graf 2", "Graf 3"),
  p    = list(graf1_percentile, graf2_percentile, graf3_percentile),
  col  = c(graf1_color, graf2_color, graf3_color)
) %>%
  mutate(
    p_val = vapply(p, function(x) if (identical(x, "Ingen")) NA_real_ else as.numeric(x), numeric(1)),
    label = vapply(p, function(x) if (identical(x, "Ingen")) NA_character_ else percentile_label(as.numeric(x)), character(1))
  ) %>%
  filter(!is.na(p_val), !is.na(label)) %>%
  mutate(label = factor(label, levels = label)) # preserve Graf 1 -> Graf 3 order

base_period_df <- period_data %>%
  select(period_label, period_display, year_adj, period_index)

line_data <- tibble::tibble()

# Percentiles
if (nrow(series_specs) > 0) {
  for (i in seq_len(nrow(series_specs))) {
    p_here     <- series_specs$p_val[i]
    label_here <- as.character(series_specs$label[i])

    tmp <- binned %>%
      mutate(val = .data[[svartid_col]]) %>%
      filter(!is.na(period_label)) %>%
      filter(!is.na(val)) %>%  # NA drop before percentile calc
      group_by(period_label) %>%
      summarise(
        value = as.numeric(stats::quantile(val, probs = p_here / 100, na.rm = TRUE, names = FALSE, type = 7)),
        .groups = "drop"
      ) %>%
      mutate(series_label = label_here)

    tmp_full <- base_period_df %>%
      left_join(tmp, by = "period_label") %>%
      mutate(series_label = label_here)

    line_data <- bind_rows(line_data, tmp_full)
  }
}

# Mean
if (isTRUE(showMeanToggle)) {
  mean_tmp <- binned %>%
    mutate(val = .data[[svartid_col]]) %>%
    filter(!is.na(period_label)) %>%
    filter(!is.na(val)) %>%
    group_by(period_label) %>%
    summarise(
      value = mean(val, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      value = ifelse(is.finite(value), value, NA_real_),
      series_label = "Gennemsnit"
    )

  mean_full <- base_period_df %>%
    left_join(mean_tmp, by = "period_label") %>%
    mutate(series_label = "Gennemsnit")

  line_data <- bind_rows(line_data, mean_full)
}

if (nrow(line_data) > 0) {
  # Order: percentiles in Graf1->Graf3 order, then mean last (if present)
  lvl <- character(0)
  if (nrow(series_specs) > 0) lvl <- c(lvl, as.character(series_specs$label))
  if (isTRUE(showMeanToggle)) lvl <- c(lvl, "Gennemsnit")

  lvl <- unique(lvl)

  line_data <- line_data %>%
    mutate(series_label = factor(series_label, levels = lvl))
} else {
  line_data <- base_period_df %>%
    mutate(series_label = factor(character(0)), value = numeric(0))
}

# NEW: Only add markers when some time bins are missing (i.e. NA after join)
has_missing_bins <- (nrow(line_data) > 0) && any(is.na(line_data$value))
line_data_for_geom <- if (isTRUE(has_missing_bins)) {
  line_data %>% filter(!is.na(value))
} else {
  line_data
}

# -----------------------------------------------------------------------------
# 9. Y-axis limits (min 5 + n*5)
# -----------------------------------------------------------------------------

max_val <- suppressWarnings(max(line_data$value, na.rm = TRUE))
if (!is.finite(max_val)) max_val <- 0

y_upper <- if (max_val <= 5) {
  5
} else {
  5 + ceiling((max_val - 5) / 5) * 5
}

break_interval <- 5

if (timeGranularity.timePlot != "År" && !is.null(year_data)) {
  year_labels_df <- year_data %>%
    mutate(
      y     = -y_upper * 0.10,
      label = as.character(year_adj)
    )
} else {
  year_labels_df <- NULL
}

# -----------------------------------------------------------------------------
# 10. Subtitle (same logic/structure as reference, without speciale)
# -----------------------------------------------------------------------------

subtitle_filters <- character(0)

if (!identical(regionToggle.timePlot, "Begge")) {
  subtitle_filters <- c(subtitle_filters, regionToggle.timePlot)
}

if (!identical(svartypeFilterToggle.timePlot, "Alle") &&
    svartypeFilterToggle.timePlot %in% c("Medicingennemgang", "Generel forespørgsel", "Ibrugtagningssag")) {

  if (identical(regionToggle.timePlot, "Nordjylland")) {
    base_location <- "Nordjylland"

  } else if (identical(regionToggle.timePlot, "Begge")) {

    if (svartypeFilterToggle.timePlot %in% c("Medicingennemgang", "Ibrugtagningssag")) {
      base_location <- "Midtjylland"
    } else {
      base_location <- "Aarhus og Aalborg"
    }

  } else {
    base_location <- "Midtjylland"
  }

} else {
  base_location <- "Aarhus og Aalborg"
}

subtitle_text <- paste(unique(c(base_location, subtitle_filters)), collapse = " | ")

# -----------------------------------------------------------------------------
# 11. Plot
# -----------------------------------------------------------------------------

# Color + linetype mappings (to ensure legend and dashed mean are correct)
series_colors <- c()
series_linetypes <- c()

if (nrow(series_specs) > 0) {
  series_colors <- setNames(series_specs$col, as.character(series_specs$label))
  series_linetypes <- setNames(rep("solid", length(series_colors)), names(series_colors))
}

if (isTRUE(showMeanToggle)) {
  series_colors <- c(series_colors, setNames(mean_color, "Gennemsnit"))
  series_linetypes <- c(series_linetypes, setNames("dashed", "Gennemsnit"))
}

p <- ggplot()

if (timeGranularity.timePlot != "År" && !is.null(year_data) && nrow(year_data) > 0) {
  p <- p +
    geom_rect(
      data  = year_data,
      aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf),
      fill  = "grey95",
      alpha = 0.5
    ) +
    geom_rect(
      data = year_labels_df,
      aes(
        xmin = xmin,
        xmax = xmax,
        ymin = y - y_upper * 0.03,
        ymax = y + y_upper * 0.03
      ),
      inherit.aes = FALSE,
      fill        = "grey95",
      colour      = NA,
      linewidth   = 0,
      alpha       = 0.5
    )
}

p <- p +
  geom_hline(
    yintercept = y_upper * -0.001,
    color      = "grey50",
    linewidth  = 0.8
  ) +
  geom_line(
    data = line_data_for_geom,
    aes(
      x        = period_label,
      y        = value,
      color    = series_label,
      linetype = series_label,
      group    = series_label
    ),
    linewidth = 1.2,
    alpha     = 0.9,
    lineend   = "round",
    linejoin  = "round",
    na.rm     = TRUE
  )

# NEW: Markers only when there are missing bins
if (isTRUE(has_missing_bins)) {
  p <- p +
    geom_point(
      data = line_data_for_geom,
      aes(
        x     = period_label,
        y     = value,
        color = series_label,
        group = series_label
      ),
      size  = 2.5,
      alpha = 0.9,
      stroke = 0,
      na.rm = TRUE
    )
}

if (timeGranularity.timePlot != "År" && !is.null(year_labels_df) && nrow(year_labels_df) > 0) {
  p <- p +
    geom_label(
      data   = year_labels_df,
      aes(x = xcenter, y = y, label = label),
      size   = 5,
      fontface = "bold",
      vjust  = 0.5,
      color  = "black",
      fill   = NA,
      label.r = grid::unit(0, "pt"),
      label.padding = grid::unit(2, "pt"),
      linewidth = 0
    )
}

p <- p +
  scale_color_manual(
    values = series_colors,
    breaks = names(series_colors),
    drop   = TRUE,
    name   = NULL
  ) +
  scale_linetype_manual(
    values = series_linetypes,
    breaks = names(series_linetypes),
    drop   = TRUE,
    name   = NULL
  ) +
  scale_y_continuous(
    breaks       = seq(0, y_upper, by = break_interval),
    minor_breaks = seq(0, y_upper, by = break_interval / 5),
    expand       = c(0, 0)
  ) +
  scale_x_discrete(
    limits = period_levels,
    drop   = FALSE,
    labels = x_labels_map
  ) +
  labs(
    x        = "",
    y        = "Svartid (dage)",
    title    = "Svartider i Lægemiddelrådgivningen",
    subtitle = subtitle_text
  ) +
  coord_cartesian(clip = "off", ylim = c(0, y_upper)) +
  theme_classic() +
  theme(
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.2),
    panel.grid.minor.y = element_line(colour = "grey92", linewidth = 0.2),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position   = "right",
    legend.margin     = margin(0, 0, 0, 0),
    legend.spacing.x  = unit(0, "pt"),
    legend.spacing.y  = unit(0, "pt"),
    legend.key.width  = unit(6, "pt"),
    legend.key.height = unit(6, "pt"),
    legend.text       = element_text(size = 10),
    legend.title      = element_text(face = "bold"),

    axis.text.x = element_text(
      size  = 10,
      angle = 90,
      hjust = 1,
      vjust = 0.5
    ),

    axis.title.x = element_text(
      size   = 12,
      face   = "bold",
      margin = margin(t = 40, unit = "pt")
    ),

    axis.title.y = element_text(
      size = 12,
      face = "bold"
    ),

    axis.ticks.length = unit(2, "pt"),
    axis.line.x       = element_blank(),
    axis.ticks.x      = axis_ticks_x,
    axis.ticks        = element_line(colour = "black", linewidth = 0.2),

    plot.title    = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5),
    plot.margin   = margin(t = 2, r = 2, b = -5, l = 2, unit = "pt")
  )

# p is returned to Shiny via the sourcing environment
