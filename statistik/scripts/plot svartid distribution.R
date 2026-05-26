#!/usr/bin/env Rscript

# plot svartid distribution.R
#
# Sourced from the Shiny server with variables defined in the local environment.
# Produces overlapping histograms of svartid for up to 4 user-defined date ranges.
#
# Required variables (set by Shiny):
#   svartidTypeToggle.timePlot    - which svartid column to use (shared with udvikling plot)
#   svartypeFilterToggle.timePlot - svartype filter (shared)
#   regionToggle.timePlot         - region filter (shared)
#   dist_vis_type                 - "pct" or "abs"
#   dist_n_ranges                 - integer 1-4
#   dist_from_1 .. dist_from_4   - Date: start of each period
#   dist_to_1   .. dist_to_4     - Date: end of each period
#
# Must create a ggplot object named: p

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(ggtext)
  library(here)
  library(lubridate)
  library(tibble)
})

message("\u2713 Pakker til 'svartider distribution'-plot er indl\u00e6st")

# -----------------------------------------------------------------------------
# 0. Fallbacks
# -----------------------------------------------------------------------------

if (!exists("svartidTypeToggle.timePlot"))    svartidTypeToggle.timePlot    <- "Hverdage u. helligdage og weekend"
if (!exists("svartypeFilterToggle.timePlot")) svartypeFilterToggle.timePlot <- "Alle"
if (!exists("regionToggle.timePlot"))         regionToggle.timePlot         <- "Begge"
if (!exists("dist_vis_type"))       dist_vis_type       <- "pct"
if (!exists("dist_n_ranges"))       dist_n_ranges       <- 3L
if (!exists("dist_show_n_title"))   dist_show_n_title   <- TRUE
if (!exists("dist_bar_label"))      dist_bar_label      <- "Nej"
if (!exists("dist_from_1"))    dist_from_1 <- floor_date(Sys.Date(), "month") %m-% months(6)
if (!exists("dist_to_1"))      dist_to_1   <- floor_date(Sys.Date(), "month") - days(1)
if (!exists("dist_title_1"))   dist_title_1 <- ""
if (!exists("dist_from_2"))    dist_from_2 <- floor_date(Sys.Date(), "month") %m-% years(1)
if (!exists("dist_to_2"))      dist_to_2   <- floor_date(Sys.Date(), "month") - days(1)
if (!exists("dist_title_2"))   dist_title_2 <- ""
if (!exists("dist_from_3"))    dist_from_3 <- floor_date(Sys.Date(), "month") %m-% years(2)
if (!exists("dist_to_3"))      dist_to_3   <- floor_date(Sys.Date(), "month") - days(1)
if (!exists("dist_title_3"))   dist_title_3 <- ""
if (!exists("dist_from_4"))    dist_from_4 <- floor_date(Sys.Date(), "month") %m-% months(30)
if (!exists("dist_to_4"))      dist_to_4   <- floor_date(Sys.Date(), "month") - days(1)
if (!exists("dist_title_4"))   dist_title_4 <- ""

# -----------------------------------------------------------------------------
# 1. Load data
# -----------------------------------------------------------------------------

source(file.path(here("statistik", "scripts"), "load downloaded data.R"))

if (!exists("data.lmraad_filtered")) {
  stop("data.lmraad_filtered not found. Please run 'download, prepare, save.R' first.")
}

message("\u2713 Data fundet: ", nrow(data.lmraad_filtered), " r\u00e6kker")

# -----------------------------------------------------------------------------
# 2. Column selection
# -----------------------------------------------------------------------------

resolve_svartid_col <- function(toggle) {
  if (identical(toggle, "Alle dage")) {
    "svartid.raw"
  } else if (identical(toggle, "Hverdage m. helligdage")) {
    "svartid.NoWeekend"
  } else if (identical(toggle, "Hverdage u. helligdage og weekend")) {
    "svartid.NoWeekendNoHolidays"
  } else {
    stop("Ugyldig svartidTypeToggle: ", toggle)
  }
}

x_axis_label <- switch(
  svartidTypeToggle.timePlot,
  "Alle dage"                         = "Svartid (dage)",
  "Hverdage m. helligdage"            = "Svartid (arbejdsdage m. helligdage)",
  "Hverdage u. helligdage og weekend" = "Svartid (arbejdsdage u. helligdage)",
  "Svartid (dage)"
)

svartid_col <- resolve_svartid_col(svartidTypeToggle.timePlot)

if (!(svartid_col %in% names(data.lmraad_filtered))) {
  stop("Svartid-kolonne '", svartid_col, "' ikke fundet i data. K\u00f8r 'download, prepare, save.R' igen.")
}

as_days_numeric <- function(x) {
  if (inherits(x, "difftime")) return(as.numeric(x, units = "days"))
  as.numeric(x)
}

data.lmraad_filtered <- data.lmraad_filtered %>%
  mutate(across(all_of(svartid_col), as_days_numeric))

# -----------------------------------------------------------------------------
# 3. Common filters
# -----------------------------------------------------------------------------

filtered_data <- data.lmraad_filtered %>%
  mutate(AdjustedDate = as.Date(AdjustedDate)) %>%
  filter(!is.na(AdjustedDate))

if (!identical(regionToggle.timePlot, "Begge")) {
  filtered_data <- filtered_data %>%
    dplyr::filter(`Region (*)` == regionToggle.timePlot)
}

if (!identical(svartypeFilterToggle.timePlot, "Alle")) {
  if (identical(svartypeFilterToggle.timePlot, "Klinisk r\u00e5dgivning")) {
    filtered_data <- filtered_data %>%
      dplyr::filter(`Svartype (*)` %in% c(
        "Almindeligt svar", "Kortsvar", "Generel foresp\u00f8rgsel"
      ))
  } else {
    filtered_data <- filtered_data %>%
      dplyr::filter(`Svartype (*)` == svartypeFilterToggle.timePlot)
  }
}

# -----------------------------------------------------------------------------
# 4. Build active ranges
# -----------------------------------------------------------------------------

all_range_inputs <- list(
  list(from = dist_from_1, to = dist_to_1, user_title = dist_title_1),
  list(from = dist_from_2, to = dist_to_2, user_title = dist_title_2),
  list(from = dist_from_3, to = dist_to_3, user_title = dist_title_3),
  list(from = dist_from_4, to = dist_to_4, user_title = dist_title_4)
)

n_active <- min(as.integer(dist_n_ranges), 4L)
active_ranges <- all_range_inputs[seq_len(n_active)]

# Label: use user title if provided, otherwise fall back to date range
active_ranges <- lapply(active_ranges, function(r) {
  date_label <- paste0(format(r$from, "%d.%m.%Y"), " \u2013 ", format(r$to, "%d.%m.%Y"))
  r$label <- if (nchar(r$user_title) > 0) r$user_title else date_label
  r
})

period_labels_ordered <- sapply(active_ranges, `[[`, "label")

# -----------------------------------------------------------------------------
# 5. Determine x-axis range from data
# -----------------------------------------------------------------------------

all_vals_combined <- unlist(lapply(active_ranges, function(r) {
  d <- filtered_data %>%
    filter(AdjustedDate >= r$from, AdjustedDate <= r$to) %>%
    pull(.data[[svartid_col]])
  d[!is.na(d) & is.finite(d) & d >= 0]
}))

if (length(all_vals_combined) == 0) {
  stop("Ingen data fundet for de valgte perioder og filtre.")
}

# Breaks must cover the full data range (otherwise hist() errors on out-of-range values)
x_max_data <- ceiling(max(all_vals_combined, na.rm = TRUE))
x_max_data <- max(x_max_data, 5L)

# Display is capped at 98th percentile so extreme outliers don't compress the plot
x_max <- ceiling(quantile(all_vals_combined, 0.98, na.rm = TRUE))
x_max <- max(x_max, 5L)

bin_width <- 1L
breaks    <- seq(-0.5, x_max_data + 0.5, by = bin_width)

message("x_max_data: ", x_max_data, " dage (faktisk max); x_max display: ", x_max, " dage (98. percentil)")

# -----------------------------------------------------------------------------
# 6. Bin data per period
# -----------------------------------------------------------------------------

binned_list <- lapply(seq_along(active_ranges), function(i) {
  r <- active_ranges[[i]]

  grp_vals <- filtered_data %>%
    filter(AdjustedDate >= r$from, AdjustedDate <= r$to) %>%
    pull(.data[[svartid_col]])
  grp_vals <- grp_vals[!is.na(grp_vals) & is.finite(grp_vals) & grp_vals >= 0]

  n_total <- length(grp_vals)
  med_val <- if (n_total > 0) median(grp_vals, na.rm = TRUE) else NA_real_

  h <- hist(grp_vals, breaks = breaks, plot = FALSE)

  tibble(
    x_mid        = h$mids,
    count        = h$counts,
    pct          = if (n_total > 0) h$counts / n_total * 100 else rep(0, length(h$mids)),
    period_label = r$label,
    median_val   = med_val,
    n_obs        = n_total
  )
})

binned_df <- bind_rows(binned_list) %>%
  mutate(period_label = factor(period_label, levels = period_labels_ordered)) %>%
  filter(x_mid <= x_max)

# -----------------------------------------------------------------------------
# 7. Median and duration warning
# -----------------------------------------------------------------------------

median_df <- binned_df %>%
  group_by(period_label) %>%
  summarise(
    median_val = first(median_val),
    n_obs      = first(n_obs),
    .groups    = "drop"
  )

durations <- sapply(active_ranges, function(r) as.numeric(r$to - r$from))

show_duration_warning <- FALSE
if (identical(dist_vis_type, "abs") && length(durations) > 1 && min(durations) > 0) {
  if ((max(durations) - min(durations)) / min(durations) > 0.05) {
    show_duration_warning <- TRUE
    message("! Periodevarighed varierer >5% - advarsel tilf\u00f8jes til plot")
  }
}

# -----------------------------------------------------------------------------
# 8. Subtitle
# -----------------------------------------------------------------------------

subtitle_filters <- character(0)
if (!identical(regionToggle.timePlot, "Begge")) {
  subtitle_filters <- c(subtitle_filters, regionToggle.timePlot)
}

needs_kfe_note <- identical(svartypeFilterToggle.timePlot, "Alle") ||
  identical(svartypeFilterToggle.timePlot, "Klinisk r\u00e5dgivning") ||
  identical(svartypeFilterToggle.timePlot, "Almindeligt svar") ||
  identical(svartypeFilterToggle.timePlot, "Kortsvar")

base_location <- if (isTRUE(needs_kfe_note)) "Region Midtjylland*" else "Region Midtjylland"

subtitle_text_base <- paste(
  unique(c(base_location, subtitle_filters)),
  collapse = " | "
)

if (isTRUE(needs_kfe_note)) {
  subtitle_text <- paste0(
    subtitle_text_base,
    "<br><span style='font-size:9pt'>",
    "*Almindeligt svar og Kortsvar besvares ogs\u00e5 af Klinisk Farmakologisk Enhed, Aalborg",
    "</span>"
  )
} else {
  subtitle_text <- subtitle_text_base
}

# -----------------------------------------------------------------------------
# 9. Prepare facet labels (period title + n) and y-axis
# -----------------------------------------------------------------------------

y_var   <- if (identical(dist_vis_type, "pct")) "pct" else "count"
y_label <- if (identical(dist_vis_type, "pct")) "Andel i perioden (%)" else "Antal i perioden"

# Build augmented strip labels: "Periode titel  (n = 123)" or just "Periode titel"
n_by_period <- setNames(median_df$n_obs, as.character(median_df$period_label))
augmented_labels <- if (isTRUE(dist_show_n_title)) {
  paste0(
    period_labels_ordered,
    "  (n\u00a0=\u00a0",
    n_by_period[period_labels_ordered],
    ")"
  )
} else {
  period_labels_ordered
}

binned_df <- binned_df %>%
  mutate(strip_label = factor(
    augmented_labels[as.integer(period_label)],
    levels = augmented_labels
  ))

median_df <- median_df %>%
  mutate(strip_label = factor(augmented_labels, levels = augmented_labels))

# -----------------------------------------------------------------------------
# 10. Build plot (faceted, stacked panels, shared x-axis)
# -----------------------------------------------------------------------------

hist_color   <- "#2166AC"
median_color <- "#0D2645"  # darker shade of the same blue for contrast

caption_text <- if (show_duration_warning) {
  "! Perioder i hvert plot varierer >5% i varighed"
} else {
  NULL
}

# Bar label layer (optional)
bar_label_layer <- NULL
if (!identical(dist_bar_label, "Nej")) {
  bar_label_df <- binned_df %>%
    filter(count > 0) %>%
    mutate(.label = if (identical(dist_bar_label, "n")) {
      as.character(count)
    } else {
      sprintf("%.1f%%", pct)
    })
  bar_label_layer <- geom_text(
    data    = bar_label_df,
    mapping = aes(label = .label),
    vjust   = -0.3,
    size    = 2.5,
    color   = "#333333"
  )
}

p <- ggplot(binned_df, aes(x = x_mid, y = .data[[y_var]])) +
  geom_col(
    fill  = hist_color,
    color = NA,
    alpha = 0.75,
    width = bin_width * 0.95
  ) +
  geom_vline(
    data      = median_df %>% filter(!is.na(median_val)),
    aes(xintercept = median_val),
    linetype  = "dashed",
    linewidth = 0.9,
    color     = median_color
  ) +
  bar_label_layer +
  facet_wrap(
    ~ strip_label,
    nrow   = n_active,
    scales = "fixed"
  ) +
  scale_x_continuous(
    breaks = seq(0, x_max, by = 1),
    expand = c(0.01, 0)
  ) +
  coord_cartesian(xlim = c(-0.5, x_max + 0.5)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title    = "Fordeling af svartider",
    subtitle = subtitle_text,
    x        = x_axis_label,
    y        = y_label,
    caption  = caption_text
  ) +
  theme_classic() +
  theme(
    panel.grid.major.y  = element_line(colour = "grey85", linewidth = 0.2),
    panel.grid.major.x  = element_line(colour = "grey85", linewidth = 0.2),
    panel.grid.minor.x  = element_blank(),
    strip.text          = element_text(size = 11, face = "bold", hjust = 0),
    strip.background    = element_rect(fill = "grey97", colour = "grey80"),
    legend.position     = "none",
    axis.text.x         = element_text(size = 9, angle = 0, hjust = 0.5, vjust = 1),
    axis.text.y         = element_text(size = 10),
    axis.title.x        = element_text(size = 12, face = "bold"),
    axis.title.y        = element_text(size = 12, face = "bold"),
    plot.title          = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle       = ggtext::element_markdown(size = 12, hjust = 0.5),
    plot.caption        = element_text(size = 10, color = "#CC0000", face = "bold", hjust = 0),
    plot.margin         = margin(t = 5, r = 10, b = 5, l = 5, unit = "pt"),
    panel.spacing.y     = unit(10, "pt")
  )

# p is returned to Shiny via the sourcing environment
