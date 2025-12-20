#!/usr/bin/env Rscript

# plot spørgsmålstype piechart.R
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

message("✓ Pakker til 'spørgsmålstype'-piechart er indlæst")

# -----------------------------------------------------------------------------
# TOGGLES - Set your preferred settings here
# -----------------------------------------------------------------------------

# COLOR TOGGLE
# Options: "Set3", "Paired", "Set1", "Set2", "Dark2", "Accent",
#          "viridis", "plasma", "magma", "inferno", "cividis"
if (!exists("colorToggle.spmPlot")) {
	colorToggle <- "Set2"
} else {
	colorToggle <- as.character(colorToggle.spmPlot)
}

# ANDRE SPØRGSMÅL GREY TOGGLE
# When TRUE: "Andre spørgsmål" is always grey
# When FALSE: "Andre spørgsmål" gets the last color in the palette
if (!exists("AndreAlwaysGreyToggle.spmPlot")) {
	AndreAlwaysGreyToggle <- TRUE  # Default: "Andre spørgsmål" is grey
} else {
	AndreAlwaysGreyToggle <- as.logical(AndreAlwaysGreyToggle.spmPlot)
}

# Optional fallbacks for truly-standalone runs (Shiny sets these)
if (!exists("showNumbersToggle.spmPlot")) showNumbersToggle.spmPlot <- "%"
if (!exists("legendPositionToggle.spmPlot")) legendPositionToggle.spmPlot <- "I diagram"

# Color palette functions
get_color_palette <- function(palette_name, n) {
	if (palette_name %in% c("Set3", "Paired", "Set1", "Set2", "Dark2", "Accent")) {
		max_colors <- RColorBrewer::brewer.pal.info[palette_name, "maxcolors"]
		if (n <= max_colors) {
			return(RColorBrewer::brewer.pal(n, palette_name))
		} else {
			base_colors <- RColorBrewer::brewer.pal(max_colors, palette_name)
			color_func <- colorRampPalette(base_colors)
			return(color_func(n))
		}
	} else if (palette_name %in% c("viridis", "plasma", "magma", "inferno", "cividis")) {
		return(viridisLite::viridis(n, option = palette_name))
	} else {
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

	if ("Alle" %in% x && length(x) > 1) {
		x <- setdiff(x, "Alle")
	}

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
		stop("Invalid showNumbersToggle.spmPlot. Allowed: ", paste(allowed, collapse = ", "))
	}
	TRUE
}

validate_legend_position <- function(x) {
	allowed <- c("I diagram", "Udenfor diagram")
	if (!is.character(x) || length(x) != 1 || !(x %in% allowed)) {
		stop("Invalid legendPositionToggle.spmPlot. Allowed: ", paste(allowed, collapse = ", "))
	}
	TRUE
}

validate_show_numbers(showNumbersToggle.spmPlot)
validate_legend_position(legendPositionToggle.spmPlot)

# -----------------------------------------------------------------------------
# 3. Filtering (time + Region + Svartype)
# -----------------------------------------------------------------------------

start_date <- as.Date(paste(start_year.spmPlot, start_month.spmPlot, start_day.spmPlot, sep = "-"))
end_date   <- as.Date(paste(end_year.spmPlot,   end_month.spmPlot,   end_day.spmPlot,   sep = "-"))

date_vec <- resolve_date_col(data.lmraad_filtered)

filtered_data <- data.lmraad_filtered %>%
	mutate(.date_for_filter = date_vec) %>%
	filter(!is.na(.date_for_filter), .date_for_filter >= start_date, .date_for_filter <= end_date)

message("✓ Rows after date filter: ", nrow(filtered_data))

# Region
if (!identical(regionToggle.spmPlot, "Begge")) {
	filtered_data <- filtered_data %>%
		filter(`Region (*)` == regionToggle.spmPlot)
	message("✓ Rows after region filter: ", nrow(filtered_data))
}

# Svartype (incl. Klinisk rådgivning grouping)
if (!identical(svartypeFilterToggle.spmPlot, "Alle")) {
	if (identical(svartypeFilterToggle.spmPlot, "Klinisk rådgivning")) {
		filtered_data <- filtered_data %>%
			filter(`Svartype (*)` %in% c("Almindeligt svar", "Kortsvar", "Generel forespørgsel"))
	} else {
		filtered_data <- filtered_data %>%
			filter(`Svartype (*)` == svartypeFilterToggle.spmPlot)
	}
	message("✓ Rows after svartype filter: ", nrow(filtered_data))
}

# Exclude activity types that should not be part of spørgsmaalskategori piechart
filtered_data <- filtered_data %>%
  dplyr::filter(!(`Svartype (*)` %in% c("Bivirkningsindberetning")))

# Speciale (multi select; "Alle" = no filtering)
spec_filter <- character(0)
if (exists("specialeFilterToggle.spmPlot")) {
  spec_filter <- as_filter_vec(specialeFilterToggle.spmPlot)
}

if (length(spec_filter) > 0) {
  if (!("Speciale (*)" %in% names(filtered_data))) {
    stop("Column 'Speciale (*)' not found in filtered_data, but specialeFilterToggle.spmPlot is set.")
  }

  filtered_data <- filtered_data %>%
    filter(`Speciale (*)` %in% spec_filter)

  message("✓ Rows after speciale filter: ", nrow(filtered_data))
}

# -----------------------------------------------------------------------------
# 4. Prepare data (match provided pie chart logic, but for Spørgsmålskategori)
# -----------------------------------------------------------------------------

# Selection (which categories to show explicitly). Everything else goes to "Andre spørgsmål".
spmTyperAtPlotte <- as.character(spmTyperAtPlotte.spmPlot)
spmTyperAtPlotte <- spmTyperAtPlotte[!is.na(spmTyperAtPlotte) & nzchar(spmTyperAtPlotte)]
spmTyperAtPlotte <- unique(spmTyperAtPlotte)
# Selection (which categories to show explicitly). Everything else goes to "Andre spørgsmål".
spmTyperAtPlotte <- as.character(spmTyperAtPlotte.spmPlot)
spmTyperAtPlotte <- spmTyperAtPlotte[!is.na(spmTyperAtPlotte) & nzchar(spmTyperAtPlotte)]
spmTyperAtPlotte <- unique(spmTyperAtPlotte)

# Ensure "Andre spørgsmål" is never treated as a selectable category
spmTyperAtPlotte <- setdiff(spmTyperAtPlotte, "Andre spørgsmål")

# Factor levels must still include the residual, or it will become NA later
levels_to_plot <- c(spmTyperAtPlotte, "Andre spørgsmål")

if (!("Spørgsmålskategori (*)" %in% names(filtered_data))) {
	stop("Column 'Spørgsmålskategori (*)' not found in data.lmraad_filtered.")
}

yearly_counts_spm <- filtered_data %>%
	mutate(spm_kat = as.character(`Spørgsmålskategori (*)`)) %>%
	group_by(spm_kat) %>%
	summarise(Count = n(), .groups = "drop") %>%
	mutate(
		spm_kat = dplyr::case_when(
			is.na(spm_kat) | !nzchar(spm_kat) ~ "Andre spørgsmål",
			spm_kat %in% spmTyperAtPlotte     ~ spm_kat,
			TRUE                              ~ "Andre spørgsmål"
		)
	) %>%
	group_by(spm_kat) %>%
	summarise(Count = sum(Count), .groups = "drop") %>%
	mutate(spm_kat = factor(spm_kat, levels = levels_to_plot)) %>%
	complete(spm_kat, fill = list(Count = 0)) %>%
	arrange(spm_kat)

total_count <- sum(yearly_counts_spm$Count, na.rm = TRUE)

if (total_count == 0) {
	message("ⓘ Ingen data tilgængelig med de valgte filtre. Viser 'Ingen data' beskeder.")

	p <- ggplot() +
		annotate("text", x = 0.5, y = 0.5, label = "Ingen data",
			    size = 24, fontface = "bold", color = "darkred") +
		labs(
			title = "Forespørgsler per spørgsmålstype",
			subtitle = paste(
				if (!identical(regionToggle.spmPlot, "Begge")) regionToggle.spmPlot else "Aarhus og Aalborg",
				if (!identical(svartypeFilterToggle.spmPlot, "Alle")) svartypeFilterToggle.spmPlot,
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
} else {
	# Move "Andre spørgsmål" to the end of the levels
	current_levels <- levels(yearly_counts_spm$spm_kat)
	andre_index <- which(current_levels == "Andre spørgsmål")
	if (length(andre_index) > 0) {
		new_levels <- c(current_levels[-andre_index], "Andre spørgsmål")
		yearly_counts_spm$spm_kat <- factor(yearly_counts_spm$spm_kat, levels = new_levels)
	}

	pie_data <- yearly_counts_spm %>%
		arrange(desc(spm_kat)) %>%
		mutate(
			fraction = if (total_count > 0) Count / total_count else 0,
			percent  = paste0(round(fraction * 100, 0), "%"),
			csum     = rev(cumsum(rev(Count))),
			pos      = Count / 2 + lead(csum, 1),
			pos      = if_else(is.na(pos), Count / 2, pos),

			label_in = dplyr::case_when(
				identical(legendPositionToggle.spmPlot, "Udenfor diagram") ~ "",
				identical(showNumbersToggle.spmPlot, "%")                  ~ percent,
				identical(showNumbersToggle.spmPlot, "Absolut")            ~ as.character(Count),
				TRUE                                                       ~ ""
			),

			pie_order = fct_inorder(spm_kat)
		)

	n_categories <- nlevels(pie_data$pie_order)
	category_levels <- levels(pie_data$pie_order)

	if (AndreAlwaysGreyToggle) {
		n_main_categories <- n_categories - 1
	} else {
		n_main_categories <- n_categories
	}

	main_colors <- get_color_palette(colorToggle, n_main_categories)

	color_values <- vector("character", n_categories)
	names(color_values) <- category_levels

	other_categories <- setdiff(category_levels, "Andre spørgsmål")

	if (AndreAlwaysGreyToggle) {
		color_values["Andre spørgsmål"] <- "grey70"

		user_order <- setdiff(spmTyperAtPlotte, "Andre spørgsmål")
		user_order_matched <- user_order[user_order %in% other_categories]

		if (length(user_order_matched) > 0) {
			color_values[user_order_matched] <- main_colors[1:length(user_order_matched)]
			remaining_categories <- setdiff(other_categories, user_order_matched)
			if (length(remaining_categories) > 0) {
				start_idx <- length(user_order_matched) + 1
				color_values[remaining_categories] <- main_colors[start_idx:(start_idx + length(remaining_categories) - 1)]
			}
		} else {
			if (length(other_categories) > 0) {
				color_values[other_categories] <- main_colors[1:length(other_categories)]
			}
		}

		andre_color_message <- "'Andre spørgsmål' is colored grey"
	} else {
		user_order <- spmTyperAtPlotte
		user_order_matched <- user_order[user_order %in% category_levels]

		if (length(user_order_matched) > 0) {
			color_values[user_order_matched] <- main_colors[1:length(user_order_matched)]
			remaining_categories <- setdiff(category_levels, user_order_matched)
			if (length(remaining_categories) > 0) {
				start_idx <- length(user_order_matched) + 1
				color_values[remaining_categories] <- main_colors[start_idx:(start_idx + length(remaining_categories) - 1)]
			}
		} else {
			color_values[category_levels] <- main_colors[1:n_categories]
		}

		andre_color_message <- "'Andre spørgsmål' gets a color from the palette (last position)"
	}

	legend_data <- pie_data %>%
		mutate(sort_order = ifelse(spm_kat == "Andre spørgsmål", Inf, -Count)) %>%
		arrange(sort_order) %>%
		mutate(
			legend_label = dplyr::case_when(
				identical(showNumbersToggle.spmPlot, "%")       ~ paste0(as.character(spm_kat), " (", percent, ")"),
				identical(showNumbersToggle.spmPlot, "Absolut") ~ paste0(as.character(spm_kat), " (", Count, ")"),
				TRUE                                           ~ as.character(spm_kat)
			)
		) %>%
		select(pie_order, spm_kat, legend_label)

	legend_labels_vec <- setNames(legend_data$legend_label, as.character(legend_data$pie_order))
	legend_breaks <- as.character(legend_data$pie_order)

	message("✓ Using color palette: '", colorToggle, "' with ", n_categories, " categories")
	message("✓ AndreAlwaysGreyToggle is: ", AndreAlwaysGreyToggle)
	message("✓ ", andre_color_message)
	message("✓ Pie chart order: Main categories start at 12 o'clock, 'Andre spørgsmål' at the end")

	# -----------------------------------------------------------------------------
	# 5. Subtitle (same style/logic pattern as prior plots)
	# -----------------------------------------------------------------------------

	subtitle_filters <- character(0)

	if (!identical(regionToggle.spmPlot, "Begge")) {
		subtitle_filters <- c(subtitle_filters, regionToggle.spmPlot)
	}

	if (!identical(svartypeFilterToggle.spmPlot, "Alle")) {
		subtitle_filters <- c(subtitle_filters, svartypeFilterToggle.spmPlot)
	}

	base_location <- if (!identical(regionToggle.spmPlot, "Begge")) regionToggle.spmPlot else "Aarhus og Aalborg"
	subtitle_text <- paste(unique(c(base_location, subtitle_filters)), collapse = " | ")

	# -----------------------------------------------------------------------------
	# 6. Plot (match provided plot, + title/subtitle, + legend toggle)
	# -----------------------------------------------------------------------------

	axis_text_theme <- if (identical(legendPositionToggle.spmPlot, "Udenfor diagram")) {
		element_blank()
	} else {
		element_text(size = 10)
	}

	y_scale_and_guides <- if (identical(legendPositionToggle.spmPlot, "Udenfor diagram")) {
		list(scale_y_continuous(breaks = NULL))
	} else {
		list(
			scale_y_continuous(
				breaks = pie_data$pos,
				labels = pie_data$spm_kat
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
			name   = if (identical(legendPositionToggle.spmPlot, "Udenfor diagram")) "Spørgsmålstype" else NULL,
			guide  = guide_legend(reverse = FALSE)
		) +
		geom_text(
			aes(label = label_in, x = 1.4, y = pos),
			color = "black",
			size  = 4
		) +
		labs(
			title    = "Forespørgsler per spørgsmålstype",
			subtitle = subtitle_text
		) +
		theme_void() +
		theme(
			axis.ticks = element_blank(),
			axis.title = element_blank(),
			axis.text  = axis_text_theme,

			legend.position = if (identical(legendPositionToggle.spmPlot, "Udenfor diagram")) "right" else "none",
			legend.title    = element_text(face = "bold"),
			legend.text     = element_text(size = 10),

			panel.background = element_rect(fill = "white", color = NA),
			panel.border     = element_blank(),

			plot.title    = element_text(size = 16, hjust = 0.5, face = "bold"),
			plot.subtitle = element_text(size = 12, hjust = 0.5),
			plot.margin   = margin(t = 10, r = 2, b = 2, l = 2, unit = "pt")
		)

	plot(p)
}
# p is returned to the caller (and later to Shiny) via the sourcing environment
