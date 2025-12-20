#!/usr/bin/env Rscript

# plot speciale piechart.R
#
# Standalone script (outside Shiny) to validate functionality.
# Based on: data.lmraad_filtered
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
  library(forcats)
  library(scales)
  library(RColorBrewer)
  library(viridisLite)
})

message("✓ Pakker til 'speciale'-piechart er indlæst")

# -----------------------------------------------------------------------------
# TOGGLES - Set your preferred settings here
# -----------------------------------------------------------------------------

# COLOR TOGGLE
# Options: "Set3", "Paired", "Set1", "Set2", "Dark2", "Accent",
#          "viridis", "plasma", "magma", "inferno", "cividis"
if (!exists("colorToggle.specialePlot")) {
  colorToggle <- "Set3"
} else {
  colorToggle <- as.character(colorToggle.specialePlot)
}

# ANDRE SPECIALER GREY TOGGLE
if (!exists("AndreAlwaysGreyToggle.specialePlot")) {
  AndreAlwaysGreyToggle <- TRUE  # Default: "Andre specialer" is grey
} else {
  AndreAlwaysGreyToggle <- as.logical(AndreAlwaysGreyToggle.specialePlot)
}

# SHOW NA SPECIALITY TOGGLE
if (!exists("showNASpecialityToggle.specialePlot")) {
  showNASpecialityToggle <- FALSE
} else {
  showNASpecialityToggle <- as.logical(showNASpecialityToggle.specialePlot)
}


# Color palette functions
get_color_palette <- function(palette_name, n) {
  if (palette_name %in% c("Set3", "Paired", "Set1", "Set2", "Dark2", "Accent")) {
    # RColorBrewer qualitative palettes (max 12 colors)
    max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
    if (n <= max_colors) {
      return(RColorBrewer::brewer.pal(n, palette_name))
    } else {
      # Generate additional colors using colorRampPalette
      base_colors <- RColorBrewer::brewer.pal(max_colors, palette_name)
      color_func <- colorRampPalette(base_colors)
      return(color_func(n))
    }
  } else if (palette_name %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
    # Viridis palettes (continuous, but work well for discrete with many categories)
    return(viridisLite::viridis(n, option = palette_name))
  } else {
    # Fallback to hue_pal if requested palette not found
    warning("Color palette '", palette_name, "' not found. Using default hue palette.")
    return(scales::hue_pal()(n))
  }
}

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

as_filter_vec <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))

  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]

  # If user selects "Alle" together with others, drop "Alle"
  if ("Alle" %in% x && length(x) > 1) {
    x <- setdiff(x, "Alle")
  }

  # If only "Alle" remains, treat as no filtering
  if (length(x) == 1 && identical(x, "Alle")) return(character(0))

  x
}

resolve_date_col <- function(df) {
  if ("AdjustedDate" %in% names(df)) {
    d <- suppressWarnings(as.Date(df$AdjustedDate))
    if (sum(!is.na(d)) > 0) return(d)
  }

  if (all(c("Year", "Month") %in% names(df))) {
    y <- suppressWarnings(as.integer(df$Year))
    m <- suppressWarnings(as.integer(df$Month))
    d <- suppressWarnings(as.Date(sprintf("%04d-%02d-01", y, m)))
    return(d)
  }

  return(rep(as.Date(NA), nrow(df)))
}

validate_show_numbers <- function(x) {
  allowed <- c("%", "Absolut", "Ingen")
  if (!is.character(x) || length(x) != 1 || !(x %in% allowed)) {
    stop("Invalid showNumbersToggle.specialePlot. Allowed: ", paste(allowed, collapse = ", "))
  }
  TRUE
}

validate_legend_position <- function(x) {
  allowed <- c("I diagram", "Udenfor diagram")
  if (!is.character(x) || length(x) != 1 || !(x %in% allowed)) {
    stop("Invalid legendPositionToggle.specialePlot. Allowed: ", paste(allowed, collapse = ", "))
  }
  TRUE
}

validate_show_numbers(showNumbersToggle.specialePlot)
validate_legend_position(legendPositionToggle.specialePlot)

# -----------------------------------------------------------------------------
# 3. Filtering (time + Region + Svartype + Spørgsmålskategori)
# -----------------------------------------------------------------------------

start_date <- as.Date(paste(start_year.specialePlot, start_month.specialePlot, start_day.specialePlot, sep = "-"))
end_date   <- as.Date(paste(end_year.specialePlot,   end_month.specialePlot,   end_day.specialePlot,   sep = "-"))

date_vec <- resolve_date_col(data.lmraad_filtered)

filtered_data <- data.lmraad_filtered %>%
  mutate(.date_for_filter = date_vec) %>%
  filter(!is.na(.date_for_filter), .date_for_filter >= start_date, .date_for_filter <= end_date)

message("✓ Rows after date filter: ", nrow(filtered_data))

# Always exclude "Bivirkningsindberetning" entirely (never included in this plot)
if ("Spørgsmålskategori (*)" %in% names(filtered_data)) {
  filtered_data <- filtered_data %>%
    filter(is.na(`Spørgsmålskategori (*)`) | `Spørgsmålskategori (*)` != "Bivirkningsindberetning")
  message("✓ Rows after excluding 'Bivirkningsindberetning': ", nrow(filtered_data))
}

# Region
if (!identical(regionToggle.specialePlot, "Begge")) {
  filtered_data <- filtered_data %>%
    filter(`Region (*)` == regionToggle.specialePlot)
  message("✓ Rows after region filter: ", nrow(filtered_data))
}

# Svartype (incl. Klinisk rådgivning grouping)
if (!identical(svartypeFilterToggle.specialePlot, "Alle")) {
  if (identical(svartypeFilterToggle.specialePlot, "Klinisk rådgivning")) {
    filtered_data <- filtered_data %>%
      filter(`Svartype (*)` %in% c("Almindeligt svar", "Kortsvar", "Generel forespørgsel"))
  } else {
    filtered_data <- filtered_data %>%
      filter(`Svartype (*)` == svartypeFilterToggle.specialePlot)
  }
  message("✓ Rows after svartype filter: ", nrow(filtered_data))
}

# Spørgsmålskategori (multiple allowed)
spm_filter <- as_filter_vec(spmKategoriFilterToggle.specialePlot)
spm_filter <- setdiff(spm_filter, "Bivirkningsindberetning")

if (length(spm_filter) > 0) {
  filtered_data <- filtered_data %>%
    filter(`Spørgsmålskategori (*)` %in% spm_filter)
  message("✓ Rows after spørgsmålskategori filter: ", nrow(filtered_data))
}

# -----------------------------------------------------------------------------
# 4. Prepare data (match provided pie chart logic)
# -----------------------------------------------------------------------------
# Speciale missing handling (robust: NA + empty + "NA"/"null" text)
# Create a local 'specialeCorrected' for plotting, based on raw "Speciale (*)" (preferred)
speciale_src_col <- dplyr::case_when(
  "Speciale (*)" %in% names(filtered_data) ~ "Speciale (*)",
  "Speciale"     %in% names(filtered_data) ~ "Speciale",
  TRUE ~ NA_character_
)

if (is.na(speciale_src_col)) {
  stop("Mangler kolonne til speciale. Forventede 'Speciale (*)' (eller alternativt 'Speciale').")
}

filtered_data <- filtered_data %>%
  mutate(
    specialeCorrected = as.character(.data[[speciale_src_col]]),

    # Optional: keep old label convention without touching sector logic
    specialeCorrected = if_else(specialeCorrected == "Almen medicin", "Almen praksis", specialeCorrected),

    .speciale_missing =
      is.na(specialeCorrected) |
      trimws(specialeCorrected) == "" |
      tolower(trimws(specialeCorrected)) %in% c("na", "n/a", "null")
  )

# Count missing BEFORE filtering/replacing (your previous code counted after filtering, which can hide the issue)
n_missing <- sum(filtered_data$.speciale_missing, na.rm = TRUE)

if (isTRUE(showNASpecialityToggle)) {
  filtered_data <- filtered_data %>%
    mutate(specialeCorrected = if_else(.speciale_missing, "Ikke angivet", specialeCorrected))
} else {
  filtered_data <- filtered_data %>%
    filter(!.speciale_missing)
}

filtered_data <- filtered_data %>% select(-.speciale_missing)

specialerAtPlotte <- as.character(specialerAtPlotte.specialePlot)
specialerAtPlotte <- specialerAtPlotte[!is.na(specialerAtPlotte) & nzchar(specialerAtPlotte)]
specialerAtPlotte <- unique(specialerAtPlotte)
if (!("Andre specialer" %in% specialerAtPlotte)) {
  specialerAtPlotte <- c("Andre specialer", specialerAtPlotte)
}

if (isTRUE(showNASpecialityToggle) && !("Ikke angivet" %in% specialerAtPlotte)) {
  specialerAtPlotte <- c(specialerAtPlotte, "Ikke angivet")
}

yearly_counts_speciale <- filtered_data %>%
  group_by(specialeCorrected) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(
    specialeCorrected = if_else(
      specialeCorrected %in% specialerAtPlotte,
      specialeCorrected,
      "Andre specialer"
    )
  ) %>%
  group_by(specialeCorrected) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  mutate(specialeCorrected = factor(specialeCorrected, levels = specialerAtPlotte)) %>%
  complete(specialeCorrected, fill = list(Count = 0)) %>%
  arrange(specialeCorrected)

total_count <- sum(yearly_counts_speciale$Count, na.rm = TRUE)

# Check if we have data to plot
if (total_count == 0) {
  message("ⓘ Ingen data tilgængelig med de valgte filtre. Viser 'Ingen data' beskeder.")

  # Create an empty plot with "Ingen data" message
  p <- ggplot() +
    annotate("text", x = 0.5, y = 0.5, label = "Ingen data",
             size = 24, fontface = "bold", color = "darkred") +
    labs(
      title = "Forespørgsler per speciale",
      subtitle = paste(
        if (!identical(regionToggle.specialePlot, "Begge")) regionToggle.specialePlot else "Aarhus og Aalborg",
        if (!identical(svartypeFilterToggle.specialePlot, "Alle")) svartypeFilterToggle.specialePlot,
        if (length(spm_filter) > 0) paste(spm_filter, collapse = ", "),
        sep = " | "
      )
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      panel.background = element_rect(fill = "white", color = NA),
      plot.margin = margin(t = 10, r = 2, b = 2, l = 2, unit = "pt")
    )

  plot(p)
  # p is returned to the caller
} else {
  # FIRST: Move "Andre specialer" to the end of the levels
  current_levels <- levels(yearly_counts_speciale$specialeCorrected)
  andre_index <- which(current_levels == "Andre specialer")
  if (length(andre_index) > 0) {
    new_levels <- c(current_levels[-andre_index], "Andre specialer")
    yearly_counts_speciale$specialeCorrected <- factor(
      yearly_counts_speciale$specialeCorrected,
      levels = new_levels
    )
  }
  # SECOND: Move "Ikke angivet" to the very end (after "Andre specialer")
current_levels <- levels(yearly_counts_speciale$specialeCorrected)
if ("Ikke angivet" %in% current_levels) {
  new_levels <- setdiff(current_levels, "Ikke angivet")
  new_levels <- c(new_levels, "Ikke angivet")
  yearly_counts_speciale$specialeCorrected <- factor(
    yearly_counts_speciale$specialeCorrected,
    levels = new_levels
  )
}

  # Prepare data for pie chart - now "Andre specialer" will be last
  pie_data <- yearly_counts_speciale %>%
    arrange(desc(specialeCorrected)) %>%
    mutate(
      fraction = if (total_count > 0) Count / total_count else 0,
      percent  = paste0(round(fraction * 100, 0), "%"),
      csum     = rev(cumsum(rev(Count))),
      pos      = Count / 2 + lead(csum, 1),
      pos      = if_else(is.na(pos), Count / 2, pos),

      # Label inside only when legend is "I diagram"
      label_in = dplyr::case_when(
        identical(legendPositionToggle.specialePlot, "Udenfor diagram") ~ "",
        identical(showNumbersToggle.specialePlot, "%")                  ~ percent,
        identical(showNumbersToggle.specialePlot, "Absolut")            ~ as.character(Count),
        TRUE                                                       ~ ""
      ),

      # Create a factor that maintains the original order for pie chart
      pie_order = fct_inorder(specialeCorrected)
    )

  # Generate colors with support for:
# - optional fixed color for "Ikke angivet"
# - optional fixed grey for "Andre specialer"

n_categories <- nlevels(pie_data$pie_order)
category_levels <- levels(pie_data$pie_order)

has_ikke_angivet <- "Ikke angivet" %in% category_levels
use_grey_for_andre <- isTRUE(AndreAlwaysGreyToggle) && ("Andre specialer" %in% category_levels)

# Categories that should receive palette colors
palette_categories <- setdiff(
  category_levels,
  c(
    if (has_ikke_angivet) "Ikke angivet" else character(0),
    if (use_grey_for_andre) "Andre specialer" else character(0)
  )
)

n_palette <- length(palette_categories)
main_colors <- if (n_palette > 0) get_color_palette(colorToggle, n_palette) else character(0)

# Named color vector for all categories
color_values <- setNames(rep(NA_character_, length(category_levels)), category_levels)

# Fixed colors
if (has_ikke_angivet) {
  color_values["Ikke angivet"] <- "#1A1A1A"  # almost black
}
if (use_grey_for_andre) {
  color_values["Andre specialer"] <- "grey70"
}

# Assign palette colors in user order where possible
user_order <- specialerAtPlotte
user_order_matched <- user_order[user_order %in% palette_categories]

if (length(user_order_matched) > 0) {
  color_values[user_order_matched] <- main_colors[seq_along(user_order_matched)]

  remaining_categories <- setdiff(palette_categories, user_order_matched)
  if (length(remaining_categories) > 0) {
    start_idx <- length(user_order_matched) + 1
    color_values[remaining_categories] <- main_colors[start_idx:(start_idx + length(remaining_categories) - 1)]
  }
} else {
  if (length(palette_categories) > 0) {
    color_values[palette_categories] <- main_colors[seq_along(palette_categories)]
  }
}

andre_color_message <- if (use_grey_for_andre) {
  "'Andre specialer' is colored grey"
} else {
  "'Andre specialer' gets a color from the palette (last position, if present)"
}

  # Create legend data - sorted by proportion (highest at top) but "Andre specialer" at bottom
  legend_data <- pie_data %>%
    # Calculate sort order for legend
    mutate(
      sort_order = dplyr::case_when(
  specialeCorrected == "Ikke angivet"   ~ 2,
  specialeCorrected == "Andre specialer" ~ 1,
  TRUE                                   ~ 0
)
    ) %>%
    arrange(sort_order, desc(Count)) %>%
    mutate(
      legend_label = dplyr::case_when(
        identical(showNumbersToggle.specialePlot, "%")       ~ paste0(as.character(specialeCorrected), " (", percent, ")"),
        identical(showNumbersToggle.specialePlot, "Absolut") ~ paste0(as.character(specialeCorrected), " (", Count, ")"),
        TRUE                                           ~ as.character(specialeCorrected)
      )
    ) %>%
    select(pie_order, specialeCorrected, legend_label)

  # Create legend labels vector in the desired order
  legend_labels_vec <- setNames(
    legend_data$legend_label,
    as.character(legend_data$pie_order)
  )

  # Create breaks for legend in the desired order
  legend_breaks <- as.character(legend_data$pie_order)

  message("✓ Using color palette: '", colorToggle, "' with ", n_categories, " categories")
  message("✓ AndreAlwaysGreyToggle is: ", AndreAlwaysGreyToggle)
  message("✓ ", andre_color_message)
  message("✓ Pie chart order: Main categories start at 12 o'clock, 'Andre specialer' at the end")

  # -----------------------------------------------------------------------------
  # 5. Subtitle (same style/logic pattern as prior plots)
  # -----------------------------------------------------------------------------

  subtitle_filters <- character(0)

  if (!identical(regionToggle.specialePlot, "Begge")) {
    subtitle_filters <- c(subtitle_filters, regionToggle.specialePlot)
  }

  if (!identical(svartypeFilterToggle.specialePlot, "Alle")) {
    subtitle_filters <- c(subtitle_filters, svartypeFilterToggle.specialePlot)
  }

  if (length(spm_filter) > 0) {
    subtitle_filters <- c(subtitle_filters, paste(spm_filter, collapse = ", "))
  }

  base_location <- if (!identical(regionToggle.specialePlot, "Begge")) regionToggle.specialePlot else "Aarhus og Aalborg"
  subtitle_text <- paste(unique(c(base_location, subtitle_filters)), collapse = " | ")

  # -----------------------------------------------------------------------------
  # 6. Plot (match provided plot, + title/subtitle, + legend toggle)
  # -----------------------------------------------------------------------------

  axis_text_theme <- if (identical(legendPositionToggle.specialePlot, "Udenfor diagram")) {
    element_blank()
  } else {
    element_text(size = 10)
  }

  y_scale_and_guides <- if (identical(legendPositionToggle.specialePlot, "Udenfor diagram")) {
    list(scale_y_continuous(breaks = NULL))
  } else {
    list(
      scale_y_continuous(
        breaks = pie_data$pos,
        labels = pie_data$specialeCorrected
      ),
      geom_segment(aes(x = 1.5, xend = 1.55, y = pos, yend = pos), color = "black", linewidth = 0.5)
    )
  }

  p <- ggplot(pie_data, aes(x = "", y = Count, fill = pie_order)) +
    geom_col(width = 1, color = "white") +
    coord_polar(theta = "y", clip = "off") +
    y_scale_and_guides +
    scale_x_discrete(expand = expansion(add = c(0, 0.5))) +
    scale_fill_manual(
      values = color_values,
      breaks = legend_breaks,
      labels = legend_labels_vec,
      name    = if (identical(legendPositionToggle.specialePlot, "Udenfor diagram")) "Speciale" else NULL,
      guide = guide_legend(reverse = FALSE)  # Don't reverse, we already have the order we want
    ) +
    geom_text(
      aes(label = label_in, x = 1.4, y = pos),
      color = "black",
      size = 4
    ) +
    labs(
      title    = "Forespørgsler per speciale",
      subtitle = subtitle_text
    ) +
    theme_void() +
    theme(
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      axis.text  = axis_text_theme,

      legend.position = if (identical(legendPositionToggle.specialePlot, "Udenfor diagram")) "right" else "none",
      legend.title    = element_text(face = "bold"),
      legend.text     = element_text(size = 10),

      panel.background = element_rect(fill = "white", color = NA),
      panel.border = element_blank(),

      plot.title    = element_text(size = 16, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5),
      plot.margin   = margin(t = 10, r = 2, b = 2, l = 2, unit = "pt")
    )

  plot(p)
}
# p is returned to the caller (and later to Shiny) via the sourcing environment
