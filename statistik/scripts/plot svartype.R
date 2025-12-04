#!/usr/bin/env Rscript

# This script is sourced from the Shiny server with all toggles
# (showStackCount, showTotalCount, timeGranularity, etc.) already
# defined in the local environment.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)
  library(here)
  library(lubridate)
  library(ggchicklet)
  library(tibble)
  library(grid)
})

# Fallback if toggle is not set from Shiny
if (!exists("showCountInLegend")) {
  showCountInLegend <- FALSE
}

message("✓ Pakker til 'svartype'-plot er indlæst")

# -------------------------------------------------------------------------
# 1. Load data
# -------------------------------------------------------------------------

source(file.path(here("statistik", "scripts"), "load downloaded data.R"))

message("Checking for data...")

if (!exists("data.lmraad_filtered")) {
  stop("data.lmraad_filtered not found in environment. Please run 'download, prepare, save.R' first.")
}

message("✓ Data found: ", nrow(data.lmraad_filtered), " rows")

# -------------------------------------------------------------------------
# 2. DATA PREPARATION
# -------------------------------------------------------------------------

message("Preparing data for plotting…")

effective_groupSvarToggle <- groupSvarToggle &&
  (identical(svartypeFilterToggle, "Alle") ||
     identical(svartypeFilterToggle, "Klinisk rådgivning"))

vibrant_palette <- c(
  "Almindeligt svar"        = "#082A54",
  "Kortsvar"                = "#E02B35",
  "Generel forespørgsel"    = "#59A89C",
  "Klinisk forespørgsel"    = "#0083B8",
  "Medicingennemgang"       = "#F0C571",
  "Ibrugtagningssag"        = "#2E8B57",
  "Psykiatrikonference"     = "#A559AA",
  "Andre"                   = "#808080",
  "Bivirkningsindberetning" = "#C96A2B"
)

if (any(duplicated(unname(vibrant_palette)))) {
  stop("vibrant_palette contains duplicated colours - adjust hex codes so all categories are unique.")
}

if (effective_groupSvarToggle) {
  if (psykiatrikonf_AndreToggle) {
    svar_levels_plot <- c(
      "Ibrugtagningssag",
      "Medicingennemgang",
      "Generel forespørgsel",
      "Klinisk forespørgsel",
      "Psykiatrikonference",
      "Andre"
    )
    svar_levels_legend <- c(
      "Klinisk forespørgsel",
      "Generel forespørgsel",
      "Medicingennemgang",
      "Ibrugtagningssag",
      "Psykiatrikonference",
      "Andre"
    )
  } else {
    svar_levels_plot <- c(
      "Ibrugtagningssag",
      "Medicingennemgang",
      "Generel forespørgsel",
      "Klinisk forespørgsel"
    )
    svar_levels_legend <- c(
      "Klinisk forespørgsel",
      "Generel forespørgsel",
      "Medicingennemgang",
      "Ibrugtagningssag"
    )
  }
} else {
  if (psykiatrikonf_AndreToggle) {
    svar_levels_plot <- c(
      "Ibrugtagningssag",
      "Medicingennemgang",
      "Generel forespørgsel",
      "Almindeligt svar",
      "Kortsvar",
      "Psykiatrikonference",
      "Andre"
    )
    svar_levels_legend <- rev(svar_levels_plot)
  } else {
    svar_levels_plot <- c(
      "Ibrugtagningssag",
      "Medicingennemgang",
      "Generel forespørgsel",
      "Almindeligt svar",
      "Kortsvar"
    )
    svar_levels_legend <- rev(svar_levels_plot)
  }
}

# -------------------------------------------------------------------------
# Legend levels: restrict when Svartype-filter is active
# -------------------------------------------------------------------------
legend_levels_effective <- svar_levels_legend

if (!identical(svartypeFilterToggle, "Alle")) {
  if (identical(svartypeFilterToggle, "Klinisk rådgivning")) {

    if (effective_groupSvarToggle) {
      # Grouped: only one category in the legend
      legend_levels_effective <- intersect(svar_levels_legend, "Klinisk forespørgsel")
    } else {
      # Ungrouped: show the three underlying types
      klinisk_levels <- c("Kortsvar", "Almindeligt svar", "Generel forespørgsel")
      legend_levels_effective <- intersect(svar_levels_legend, klinisk_levels)
    }

  } else {
    # Any other specific Svartype -> single entry in legend
    legend_levels_effective <- intersect(svar_levels_legend, svartypeFilterToggle)
  }
}

# -------------------------------------------------------------------------
# Filtering: start from full dataset
# -------------------------------------------------------------------------

filtered_data <- data.lmraad_filtered

# 1) Speciale / Hospital logic
#    - If Speciale == "Hospital": filter on sektor == "Hospital"
#    - Else if Speciale != "Alle": filter on the chosen speciality
if (identical(specialeToggle, "Hospital")) {
  filtered_data <- filtered_data %>%
    dplyr::filter(sektor == "Hospital")
} else if (!identical(specialeToggle, "Alle")) {
  filtered_data <- filtered_data %>%
    dplyr::filter(specialeCorrected == specialeToggle)
}

# 2) Region filter
if (!identical(regionToggle, "Begge")) {
  filtered_data <- filtered_data %>%
    dplyr::filter(`Region (*)` == regionToggle)
}

# 3) Svartype filter
if (!identical(svartypeFilterToggle, "Alle")) {
  if (identical(svartypeFilterToggle, "Klinisk rådgivning")) {
    filtered_data <- filtered_data %>%
      dplyr::filter(`Svartype (*)` %in% c(
        "Almindeligt svar",
        "Kortsvar",
        "Generel forespørgsel"
      ))
  } else {
    filtered_data <- filtered_data %>%
      dplyr::filter(`Svartype (*)` == svartypeFilterToggle)
  }
}

# -------------------------------------------------------------------------
# 3. WEEKLY BASE
# -------------------------------------------------------------------------

weekly_base <- filtered_data %>%
  mutate(
    AdjustedDate = as.Date(AdjustedDate),
    WeekNumber   = as.integer(WeekNumber),
    year_adj     = as.integer(Year),
    month_adj    = factor(
      Month,
      levels = 1:12,
      labels = c("Jan", "Feb", "Mar", "Apr", "Maj", "Jun",
                 "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")
    ),
    month_num = as.integer(Month),
    quarter_num = case_when(
      month_num <= 3 ~ 1L,
      month_num <= 6 ~ 2L,
      month_num <= 9 ~ 3L,
      TRUE ~ 4L
    ),
    quarter_adj = factor(quarter_num, levels = 1:4, labels = c("K1", "K2", "K3", "K4")),
    svar_kategori = case_when(
      !is.na(svar_kategori)                    ~ svar_kategori,
      `Svartype (*)` == "Medicingennemgang"    ~ "Medicingennemgang",
      `Svartype (*)` == "Ibrugtagningssag"     ~ "Ibrugtagningssag",
      `Svartype (*)` == "Kortsvar"             ~ "Kortsvar",
      `Svartype (*)` == "Generel forespørgsel" ~ "Generel forespørgsel",
      `Svartype (*)` == "Almindeligt svar"     ~ "Almindeligt svar",
      TRUE                                     ~ NA_character_
    )
  ) %>%
  filter(
    !is.na(AdjustedDate),
    !is.na(svar_kategori)
  )

if (effective_groupSvarToggle) {
  weekly_base <- weekly_base %>%
    mutate(
      svar_kategori = case_when(
        # When we have specifically filtered to "Klinisk rådgivning",
        # group all three underlying types to a single category:
        identical(svartypeFilterToggle, "Klinisk rådgivning") &
          svar_kategori %in% c("Kortsvar", "Almindeligt svar", "Generel forespørgsel") ~
          "Klinisk forespørgsel",

        # Otherwise (e.g. Svartype == "Alle"), keep the original grouping logic:
        svar_kategori %in% c("Kortsvar", "Almindeligt svar") ~ "Klinisk forespørgsel",

        TRUE ~ svar_kategori
      )
    )
}

if (!psykiatrikonf_AndreToggle) {
  weekly_base <- weekly_base %>%
    filter(!svar_kategori %in% c("Psykiatrikonference", "Andre"))
}

# -------------------------------------------------------------------------
# 4. TIME GRID AND COUNTS
# -------------------------------------------------------------------------

start_date <- as.Date(paste(start_year, start_month, start_day, sep = "-"))
end_date   <- as.Date(paste(end_year,   end_month,   end_day,   sep = "-"))

if (timeGranularity == "Uge") {
  weekly_filtered <- weekly_base %>%
    filter(
      AdjustedDate >= start_date,
      AdjustedDate <= end_date
    )
} else if (timeGranularity == "Måned") {
  weekly_filtered <- weekly_base %>%
    filter(
      AdjustedDate >= start_date,
      AdjustedDate <= end_date
    )
} else if (timeGranularity == "Kvartal") {
  start_quarter_date <- as.Date(paste(start_year, ((start_month - 1) %/% 3) * 3 + 1, 1, sep = "-"))
  end_quarter_date   <- as.Date(paste(end_year,   ((end_month  - 1) %/% 3) * 3 + 1, 1, sep = "-"))

  weekly_filtered <- weekly_base %>%
    mutate(
      quarter_start_date = as.Date(paste(
        year_adj,
        ((month_num - 1) %/% 3) * 3 + 1,
        1,
        sep = "-"
      ))
    ) %>%
    filter(
      quarter_start_date >= start_quarter_date,
      quarter_start_date <= end_quarter_date
    )
}

period_grid <- NULL
plot_counts <- NULL

if (timeGranularity == "Uge") {
  message("Preparing WEEKLY data...")

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

  counts <- weekly_filtered %>%
    mutate(
      iso_year = lubridate::isoyear(AdjustedDate),
      iso_week = lubridate::isoweek(AdjustedDate)
    ) %>%
    count(iso_year, iso_week, svar_kategori, name = "n")

  plot_counts <- period_grid %>%
    left_join(counts, by = c("iso_year", "iso_week")) %>%
    mutate(
      n = tidyr::replace_na(n, 0L)
    )

} else if (timeGranularity == "Måned") {
  message("Preparing MONTHLY data...")

  month_seq <- seq.Date(
    from = floor_date(start_date, "month"),
    to   = floor_date(end_date,   "month"),
    by   = "month"
  )

  month_num <- lubridate::month(month_seq)
  month_adj <- factor(
    month_num,
    levels = 1:12,
    labels = c("Jan", "Feb", "Mar", "Apr", "Maj", "Jun",
               "Jul", "Aug", "Sep", "Okt", "Nov", "Dec")
  )

  period_grid <- tibble::tibble(
    year_adj       = lubridate::year(month_seq),
    month_adj      = month_adj,
    period_label   = sprintf("%d-%s", year_adj, month_adj),
    period_display = as.character(month_adj)
  )

  counts <- weekly_filtered %>%
    count(year_adj, month_adj, svar_kategori, name = "n")

  plot_counts <- period_grid %>%
    left_join(counts, by = c("year_adj", "month_adj")) %>%
    mutate(
      n = tidyr::replace_na(n, 0L)
    )

} else if (timeGranularity == "Kvartal") {
  message("Preparing QUARTERLY data...")

  start_quarter_date <- as.Date(paste(start_year, ((start_month - 1) %/% 3) * 3 + 1, 1, sep = "-"))
  end_quarter_date   <- as.Date(paste(end_year,   ((end_month  - 1) %/% 3) * 3 + 1, 1, sep = "-"))

  quarter_seq <- seq.Date(start_quarter_date, end_quarter_date, by = "quarter")

  quarter_num <- lubridate::quarter(quarter_seq)
  quarter_adj <- factor(
    quarter_num,
    levels = 1:4,
    labels = c("K1", "K2", "K3", "K4")
  )

  period_grid <- tibble::tibble(
    year_adj       = lubridate::year(quarter_seq),
    quarter_num    = quarter_num,
    quarter_adj    = quarter_adj,
    period_label   = sprintf("%d-%s", year_adj, quarter_adj),
    period_display = as.character(quarter_adj)
  )

  counts <- weekly_filtered %>%
    count(year_adj, quarter_adj, quarter_num, svar_kategori, name = "n")

  plot_counts <- period_grid %>%
    left_join(counts, by = c("year_adj", "quarter_adj", "quarter_num")) %>%
    mutate(
      n = tidyr::replace_na(n, 0L)
    )
}

period_levels <- period_grid$period_label

plot_counts <- plot_counts %>%
  mutate(
    period_label  = factor(period_label, levels = period_levels),
    svar_kategori = factor(svar_kategori, levels = svar_levels_plot)
  ) %>%
  group_by(period_label, period_display, year_adj) %>%
  complete(
    svar_kategori = factor(svar_levels_plot, levels = svar_levels_plot),
    fill = list(n = 0L)
  ) %>%
  ungroup()

# --- totals per period (for bars & trendline) ---
total_counts <- plot_counts %>%
  group_by(period_label) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  mutate(period_index = as.numeric(period_label))

# --- totals per svar_kategori for legend labels ---
category_totals <- plot_counts %>%
  group_by(svar_kategori) %>%
  summarise(cat_total = sum(n), .groups = "drop")

# Robust axis when all totals are 0 or no data
y_top <- max(total_counts$total, na.rm = TRUE)
if (!is.finite(y_top) || y_top <= 0) {
  y_top <- 1  # minimal positive height
}

y_break_percent <- y_top * 0.20

possible_breaks <- c(2, 5, 10, 15, 20, 25, 30, 40, 50, 75,
                     100, 125, 150, 200, 250, 500)
break_interval  <- possible_breaks[which.min(abs(possible_breaks - y_break_percent))]

y_top5 <- ceiling(y_top / break_interval) * break_interval

pad <- max(0.3, 0.02 * y_top5)

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

axis_ticks_x <- if (n_periods > 50L) {
  element_line(colour = "black", linewidth = 0.2)
} else {
  element_blank()
}

min_radius_pts <- 0.5
max_radius_pts <- 5.0
raw_radius_pts <- 5 - n_periods / 20
bar_radius_pts <- max(min_radius_pts, min(max_radius_pts, raw_radius_pts))
bar_radius     <- grid::unit(bar_radius_pts, "pt")

year_data <- period_data %>%
  group_by(year_adj) %>%
  summarise(
    xmin    = min(period_index) - 0.43,
    xmax    = max(period_index) + 0.43,
    xcenter = mean(period_index),
    .groups = "drop"
  )

year_labels_df <- year_data %>%
  mutate(
    y     = -y_top5 * 0.10,
    label = as.character(year_adj)
  )

legend_df <- tibble::tibble(
  period_label  = factor(rep(period_levels[1], length(legend_levels_effective)),
                         levels = period_levels),
  n             = 1L,
  svar_kategori = factor(legend_levels_effective,
                         levels = legend_levels_effective)
)

bars_df <- plot_counts %>%
  filter(n > 0) %>%
  mutate(
    period_label  = factor(period_label, levels = period_levels),
    svar_kategori = factor(svar_kategori, levels = svar_levels_plot)
  )

has_bars <- nrow(bars_df) > 0

if (showTrendlineToggle) {
  trend_model        <- lm(total ~ period_index, data = total_counts)
  total_counts$trend <- predict(trend_model, total_counts)
}

x_title <- ""

# -------------------------------------------------------------------------
# Subtitle: "Aarhus og Aalborg" + Region + Speciale (no Sektor, no Svartype)
# -------------------------------------------------------------------------

subtitle_filters <- character(0)

# Add Region (without "Region:")
if (!identical(regionToggle, "Begge")) {
  subtitle_filters <- c(subtitle_filters, regionToggle)
}

# Add Speciale (without "Speciale:")
if (!identical(specialeToggle, "Alle")) {
  subtitle_filters <- c(subtitle_filters, specialeToggle)
}

subtitle_text <- paste(
  c("Aarhus og Aalborg", subtitle_filters),
  collapse = " | "
)

# -------------------------------------------------------------------------
# Legend labels: possibly include counts
# -------------------------------------------------------------------------

# Map svar_kategori -> total count
cat_totals_vec <- setNames(
  category_totals$cat_total,
  as.character(category_totals$svar_kategori)
)

# Labels with "(n)" suffix
legend_labels_with_n <- vapply(
  legend_levels_effective,
  FUN = function(lbl) {
    n_val <- unname(cat_totals_vec[[lbl]])
    if (is.null(n_val) || is.na(n_val)) n_val <- 0L
    paste0(lbl, " (", n_val, ")")
  },
  FUN.VALUE = character(1)
)

# Choose labels and title based on showCountInLegend
legend_labels <- if (isTRUE(showCountInLegend)) legend_labels_with_n else legend_levels_effective
legend_title  <- if (isTRUE(showCountInLegend)) "Type aktivitet (n)" else "Type aktivitet"

message("Creating plot…")

y_lower <- -y_top5 * 0.12
y_upper <- y_top5 * 1.02

# -------------------------------------------------------------------------
# 5. PLOT
# -------------------------------------------------------------------------

p <- ggplot() +
  ggchicklet::geom_chicklet(
    data = legend_df,
    aes(x = period_label, y = n, fill = svar_kategori),
    width        = 0.85,
    radius       = bar_radius,
    alpha        = 0,
    colour       = NA,
    size         = 0.5,
    inherit.aes  = FALSE,
    show.legend  = TRUE
  ) +
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
      ymin = y - y_top5 * 0.03,
      ymax = y + y_top5 * 0.03
    ),
    inherit.aes = FALSE,
    fill        = "grey95",
    colour      = NA,
    linewidth   = 0,
    alpha       = .5
  ) +
  # Trendline with linetype mapped for legend
  (if (showTrendlineToggle) {
    geom_line(
      data = total_counts,
      aes(
        x        = period_label,
        y        = trend,
        group    = 1,
        linetype = "Trendlinje"
      ),
      color     = "black",
      linewidth = 1.2,
      alpha     = .6,
      lineend   = "round",
      linejoin  = "round"
    )
  } else {
    NULL
  }) +
  geom_hline(
    yintercept = y_top * -0.001,
    color      = "grey50",
    linewidth  = 0.8
  ) +
  # Bars only if there is at least one non-zero bar
  (if (has_bars) {
    ggchicklet::geom_chicklet(
      data    = bars_df,
      aes(x = period_label, y = n, fill = svar_kategori),
      width   = 0.85,
      radius  = bar_radius,
      colour  = "white",
      size    = 0.5,
      position = position_stack(reverse = TRUE)
    )
  } else {
    NULL
  }) +
  # Stack counts only if requested AND there are bars
  (if (showStackCount && has_bars) {
    geom_text(
      data     = bars_df,
      aes(x = period_label, y = n, label = n),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      color    = "white",
      size     = 3.0,
      fontface = "bold"
    )
  } else {
    NULL
  }) +
  # Total counts only if requested AND there is any non-zero total
  (if (showTotalCount && any(total_counts$total > 0)) {
    geom_text(
      data     = total_counts,
      aes(x = period_label, y = total, label = total),
      vjust    = -0.5,
      size     = 2.5,
      fontface = "plain",
      color    = "black"
    )
  } else {
    NULL
  }) +
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
  ) +
  # Fill legend with optional (n)
  scale_fill_manual(
    values = vibrant_palette[legend_levels_effective],
    limits = legend_levels_effective,
    breaks = legend_levels_effective,
    labels = legend_labels,
    drop   = FALSE,
    name   = legend_title
  ) +
  # Linetype legend for trendline
  scale_linetype_manual(
    name   = NULL,
    values = c("Trendlinje" = "solid"),
    breaks = "Trendlinje"
  ) +
  guides(
    fill     = guide_legend(override.aes = list(alpha = 1), order = 1),
    linetype = guide_legend(order = 2, override.aes = list(color = "black"))
  ) +
  scale_y_continuous(
    breaks       = seq(0, y_top5, by = break_interval),
    minor_breaks = NULL,
    expand       = c(0, 0)
  ) +
  scale_x_discrete(
    limits = period_levels,
    drop   = FALSE,
    labels = x_labels_map
  ) +
  labs(
    x        = x_title,
    y        = "Forespørgsler",
    title    = "Aktiviteter i Lægemiddelrådgivningen",
    subtitle = subtitle_text
  ) +
  coord_cartesian(clip = "off", ylim = c(0, y_upper)) +
  theme_classic() +
  theme(
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
    plot.margin   = margin(t = 3, r = 0, b = 90, l = 0, unit = "pt")
  )

print(p)
