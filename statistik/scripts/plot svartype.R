#!/usr/bin/env Rscript


# =============================================================================
# 0A. TOGGLES & DYNAMIC TIME RANGE (TOP OF SCRIPT)
# =============================================================================

# Toggle for showing numbers on each stack
showStackCount <- F

# Toggle for showing total count above bars
showTotalCount <- T

# Toggle for time granularity: "Uge", "Måned", or "Kvartal"
timeGranularity <- "Kvartal"

# Toggle for showing trendline
showTrendlineToggle <- F

# Toggle for grouping response types
groupSvarToggle <- F

# Toggle for showing Psykiatrikonference and Andre categories
psykiatrikonf_AndreToggle <- FALSE  # New toggle

# Toggle for filtering by sektor: "Alle", "Almen praksis", "Hospital"
sektorToggle <- "Alle"

# Toggle for filtering by specialeCorrected: "Alle", "Almen praksis", "Neurologi", ...
specialeToggle <- "Alle"

# Toggle for filtering by Region (*): "Begge" (no filter), "Nordjylland", "Midtjylland"
regionToggle <- "Begge"

# Toggle for filtering by Svartype (*):
# "Alle",
# "Medicingennemgang", "Ibrugtagningssag",
# "Klinisk rådgivning" (Almindeligt svar + Kortsvar + Generel forespørgsel),
# "Almindeligt svar", "Kortsvar", "Generel forespørgsel"
svartypeFilterToggle <- "Klinisk rådgivning"

# Set the common start and end dates here (used for all granularities)
start_year  <- 2022L
start_month <- 1L
start_day   <- 1L
end_year    <- 2025L
end_month   <- 10L
end_day     <- 31L

# =============================================================================
# 0B. PACKAGE LOADING SECTION
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)
  library(here)
  library(lubridate)
  library(ggchicklet)
})

message("✓ Pakker til uge-aktivitetsplot er indlæst")

# =============================================================================
# 1. Load data
# =============================================================================

source(file.path(here("statistik", "scripts"), "load downloaded data.R"))

# =============================================================================
# 2. DATA CHECK
# =============================================================================

message("Checking for data...")

if (!exists("data.lmraad_filtered")) {
  stop("data.lmraad_filtered not found in environment. Please run script 2 first to load the data.")
}

message("✓ Data found: ", nrow(data.lmraad_filtered), " rows")

# =============================================================================
# 3. DATA PREPARATION
# =============================================================================

message("Preparing data for plotting…")

# Effective grouping toggle: groupSvarToggle must NOT be applied when Svartype-filter is not "Alle"
effective_groupSvarToggle <- groupSvarToggle && identical(svartypeFilterToggle, "Alle")

# Palette + levels
vibrant_palette <- c(
  # Klinisk rådgivning
  "Almindeligt svar"      = "#082A54",
  "Kortsvar"              = "#E02B35",
  "Generel forespørgsel"  = "#59A89C",
  "Klinisk forespørgsel"  = "#0083B8",  # grouped category

  # Andre typer
  "Medicingennemgang"     = "#F0C571",
  "Ibrugtagningssag"      = "#2E8B57",
  "Psykiatrikonference"   = "#A559AA",
  "Andre"                 = "#808080",

  # Hvis du bruger bivirkningsdata som egen kategori
  "Bivirkningsindberetning" = "#C96A2B"
)

# Sikkerhed: tjek at alle farver er unikke
if (any(duplicated(unname(vibrant_palette)))) {
  stop("vibrant_palette contains duplicated colours – adjust hex codes so all categories are unique.")
}

# Handle grouping toggle and psykiatrikonf_AndreToggle
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

# ---- Apply filters based on toggles to data.lmraad_filtered
filtered_data <- data.lmraad_filtered

# 1) Filter by sektor
if (!identical(sektorToggle, "Alle")) {
  filtered_data <- filtered_data %>%
    filter(sektor == sektorToggle)
}

# 2) Filter by specialeCorrected
if (!identical(specialeToggle, "Alle")) {
  filtered_data <- filtered_data %>%
    filter(specialeCorrected == specialeToggle)
}

# 3) Filter by Region (*)
if (!identical(regionToggle, "Begge")) {
  filtered_data <- filtered_data %>%
    filter(`Region (*)` == regionToggle)
}

# 4) Filter by Svartype (*)
if (!identical(svartypeFilterToggle, "Alle")) {
  if (identical(svartypeFilterToggle, "Klinisk rådgivning")) {
    # Klinisk rådgivning = Almindeligt svar + Kortsvar + Generel forespørgsel
    filtered_data <- filtered_data %>%
      filter(`Svartype (*)` %in% c(
        "Almindeligt svar",
        "Kortsvar",
        "Generel forespørgsel"
      ))
  } else {
    filtered_data <- filtered_data %>%
      filter(`Svartype (*)` == svartypeFilterToggle)
  }
}

# ---- Base dataset using existing Month and Year columns
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
    month_num    = as.integer(Month),
    # Calculate quarter
    quarter_num  = case_when(
      month_num <= 3 ~ 1L,
      month_num <= 6 ~ 2L,
      month_num <= 9 ~ 3L,
      TRUE ~ 4L
    ),
    quarter_adj  = factor(quarter_num, levels = 1:4, labels = c("K1", "K2", "K3", "K4")),
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

# Apply grouping if effective_groupSvarToggle is TRUE
if (effective_groupSvarToggle) {
  weekly_base <- weekly_base %>%
    mutate(
      svar_kategori = case_when(
        svar_kategori %in% c("Kortsvar", "Almindeligt svar") ~ "Klinisk forespørgsel",
        TRUE ~ svar_kategori
      )
    )
}

# Filter out Psykiatrikonference and Andre if toggle is FALSE
if (!psykiatrikonf_AndreToggle) {
  weekly_base <- weekly_base %>%
    filter(!svar_kategori %in% c("Psykiatrikonference", "Andre"))
}

# Create common start and end dates
start_date <- as.Date(paste(start_year, start_month, start_day, sep = "-"))
end_date   <- as.Date(paste(end_year,   end_month,   end_day,   sep = "-"))

# Filter data based on time granularity
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

if (nrow(weekly_filtered) == 0L) {
  stop("No data in selected time interval. Adjust start/end parameters.")
}

# =============================================================================
# 4. PREPARE DATA BASED ON TIME GRANULARITY (with full period grid)
# =============================================================================

period_grid <- NULL
plot_counts <- NULL

if (timeGranularity == "Uge") {
  message("Preparing WEEKLY data...")

  # Full sequence of Mondays in the selected date range
  week_seq <- seq.Date(
    from = floor_date(start_date, "week", week_start = 1),
    to   = floor_date(end_date,   "week", week_start = 1),
    by   = "week"
  )

  # ISO year/week for the period grid
  iso_year_seq <- lubridate::isoyear(week_seq)
  iso_week_seq <- lubridate::isoweek(week_seq)

  period_grid <- tibble::tibble(
    iso_year       = iso_year_seq,
    iso_week       = iso_week_seq,
    year_adj       = iso_year_seq,                       # used downstream for year bands
    period_label   = sprintf("%d-W%02d", iso_year_seq, iso_week_seq),
    period_display = sprintf("%02d", iso_week_seq)
  ) %>%
    dplyr::distinct(iso_year, iso_week, .keep_all = TRUE)

  # Compute ISO year/week for the data as well (so it matches the grid)
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

# Use full sequence of periods for the axis
period_levels <- period_grid$period_label

# ---- Complete svar_kategori so all categories exist in each period
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

# Calculate total counts for each period (for showTotalCount and trendline)
total_counts <- plot_counts %>%
  group_by(period_label) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  mutate(period_index = as.numeric(period_label))

# ---- Y-axis scaling with dynamic breaks (20% of max value)
y_top <- max(total_counts$total, na.rm = TRUE)

# Calculate 20% of max value for break interval
y_break_percent <- y_top * 0.20

# Define possible break intervals and find the closest one
possible_breaks <- c(2, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 125, 150, 200, 250, 500)
break_interval  <- possible_breaks[which.min(abs(possible_breaks - y_break_percent))]

# Calculate y_top5 as next multiple of break_interval above y_top
y_top5 <- ceiling(y_top / break_interval) * break_interval

pad <- max(0.3, 0.02 * y_top5)

# =============================================================================
# 5. CREATE YEAR LABELS WITH GREY BOXES
# =============================================================================

# Period data directly from period_grid
period_data <- period_grid %>%
  mutate(
    period_label = factor(period_label, levels = period_levels),
    period_index = as.numeric(period_label)
  ) %>%
  distinct(period_label, .keep_all = TRUE) %>%
  arrange(period_index)

# Number of periods (x-values)
n_periods <- length(period_levels)

# Build x-axis label map with at most 50 non-empty labels
if (n_periods <= 50L) {
  # Show all labels
  x_labels_map <- setNames(period_data$period_display, period_data$period_label)
} else {
  # Show at most 50 labels: first and then every 'step'-th
  step <- ceiling(n_periods / 50L)
  show_idx <- seq(1L, n_periods, by = step)

  lbl_vec <- rep("", n_periods)
  lbl_vec[show_idx] <- period_data$period_display[show_idx]

  x_labels_map <- setNames(lbl_vec, period_data$period_label)
}

# Tick marks: only show x-ticks when we have more than 50 periods
axis_ticks_x <- if (n_periods > 50L) {
  element_line(colour = "black", linewidth = 0.2)
} else {
  element_blank()
}

# Bar corner rounding: fewer periods -> more rounding, more periods -> less
# Define radius bounds (in pt)
min_radius_pts <- 0.5   # smallest rounding
max_radius_pts <- 5.0   # largest rounding

# Compute raw radius as a function of number of periods
raw_radius_pts <- 5 - n_periods / 20

# Clamp raw radius between min_radius_pts and max_radius_pts
bar_radius_pts <- max(
  min_radius_pts,
  min(max_radius_pts, raw_radius_pts)
)

bar_radius <- grid::unit(bar_radius_pts, "pt")
# Calculate year positions and labels with gaps between years
year_data <- period_data %>%
  group_by(year_adj) %>%
  summarise(
    xmin    = min(period_index) - 0.43,
    xmax    = max(period_index) + 0.43,
    xcenter = mean(period_index),
    .groups = "drop"
  )

# Create a separate data frame for year labels (position identical to your reference)
year_labels_df <- year_data %>%
  mutate(
    y     = -y_top5 * 0.10,
    label = as.character(year_adj)
  )

# =============================================================================
# 6. LEGEND + CHICKLET DATA + TRENDLINE
# =============================================================================

# Create legend_df based on current svar_levels_legend
legend_df <- tibble::tibble(
  period_label  = factor(rep(period_levels[1], length(svar_levels_legend)), levels = period_levels),
  n             = 1L,
  svar_kategori = factor(svar_levels_legend, levels = svar_levels_legend)
)

bars_df <- plot_counts %>%
  filter(n > 0) %>%   # periods with zero total are still kept in total_counts / scale_x
  mutate(
    period_label  = factor(period_label, levels = period_levels),
    svar_kategori = factor(svar_kategori, levels = svar_levels_plot)
  )

# Create trendline data if toggle is TRUE
if (showTrendlineToggle) {
  # Simple linear regression for trend
  trend_model        <- lm(total ~ period_index, data = total_counts)
  total_counts$trend <- predict(trend_model, total_counts)
}

# Set x-axis title based on time granularity
x_title <- case_when(
  timeGranularity == "Uge"     ~ "",
  timeGranularity == "Kvartal" ~ "",
  TRUE                         ~ ""  # Empty for Måned
)

# Dynamic subtitle including selected speciale when not "Alle"
subtitle_text <- if (identical(specialeToggle, "Alle")) {
  "Aarhus og Aalborg"
} else {
  paste0("Aarhus og Aalborg - Speciale: ", specialeToggle)
}

# =============================================================================
# 7. PLOTTING SECTION
# =============================================================================

message("Creating plot…")

# We keep your original y_lower/y_upper calculations,
# but only use y_upper in coord_cartesian (no y-limits on the scale).
y_lower <- -y_top5 * 0.12
y_upper <- y_top5 * 1.02

p <- ggplot() +
  # Invisible chicklet layer to ensure all legend entries (even when some categories have no data)
ggchicklet::geom_chicklet(
  data = legend_df,
  aes(x = period_label, y = n, fill = svar_kategori),
  width  = 0.85,
  radius = bar_radius,
  alpha  = 0,
  colour = NA,
  size   = 0.5,
  inherit.aes = FALSE,
  show.legend  = TRUE
) +
  # Year background rectangles in main chart area (central panel)
  geom_rect(
    data  = year_data,
    aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = Inf),
    fill  = "grey95",
    alpha = 0.5
  ) +
  # Grey band for year labels BELOW the chart, same width as above
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
  # Trendline if enabled (placed BEHIND bars and labels)
  (if (showTrendlineToggle) {
    geom_line(
      data      = total_counts,
      aes(x = period_label, y = trend, group = 1),
      color     = "#FF0000",
      linewidth = 1.5,
      alpha     = 1,
      lineend   = "round",
      linejoin  = "round"
    )
  } else {
    NULL
  }) +
  # X-axis line (horizontal grey thin line)
  geom_hline(
    yintercept = y_top * -0.001,
    color      = "grey50",
    linewidth  = 0.8
  ) +
  # Main bars
ggchicklet::geom_chicklet(
  data    = bars_df,
  aes(x = period_label, y = n, fill = svar_kategori),
  width   = 0.85,
  radius  = bar_radius,
  colour  = "white",
  size    = 0.5,
  position = position_stack(reverse = TRUE)
) +
  # Stack numbers if enabled
  (if (showStackCount) {
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
  # Total count above bars if enabled
  (if (showTotalCount) {
    geom_text(
      data    = total_counts,
      aes(x = period_label, y = total, label = total),
      vjust   = -0.5,
      size    = 2.5,
      fontface = "plain",
      color   = "black"
    )
  } else {
    NULL
  }) +
  # Year labels (text) centred in the grey band (position unchanged vs reference)
  geom_label(
    data   = year_labels_df,
    aes(x = xcenter, y = y, label = label),
    size   = 5,
    fontface = "bold",
    vjust  = 0.5,
    color  = "black",
    fill   = NA,                     # let the grey band show through
    label.r = grid::unit(0, "pt"),
    label.padding = grid::unit(2, "pt"),
    linewidth = 0                    # no extra border from geom_label
  ) +
scale_fill_manual(
  values = vibrant_palette[svar_levels_legend],
  limits = svar_levels_legend,
  breaks = svar_levels_legend,
  drop   = FALSE,
  name   = "Type aktivitet"
) +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  # IMPORTANT: no y-limits here, so we can safely draw outside and
  # use coord_cartesian to zoom without killing the grey band.
  scale_y_continuous(
    breaks       = seq(0, y_top5, by = break_interval),
    minor_breaks = NULL,
    expand       = c(0, 0)
  ) +
  # Show *all* periods (weeks / months / quarters), even if total = 0
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
  # Same as your reference: zoom 0..y_upper, but allow drawing outside panel
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

# Print the plot
print(p)

# =============================================================================
# 8. SAVE PLOT
# =============================================================================

plots_dir <- here("statistik", "output", "weekly activity")
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir, recursive = TRUE)
  message("Created directory: ", plots_dir)
}

plot_filename <- paste0(
  tolower(timeGranularity),
  "_activity_plot_",
  sprintf("Y%dM%dD%d_to_Y%dM%dD%d",
          start_year, start_month, start_day,
          end_year, end_month, end_day),
  ifelse(groupSvarToggle, "_grouped", ""),
  ifelse(psykiatrikonf_AndreToggle, "_withPA", "_noPA"),
  ifelse(showTrendlineToggle, "_trend", ""),
  ".png"
)
plot_path <- here(plots_dir, plot_filename)

ggsave(plot_path, p, width = 10, height = 6.5, dpi = 500)
message("✓ Plot saved: ", plot_path)

message("✓ Plot creation completed successfully!")
