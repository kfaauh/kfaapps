#!/usr/bin/env Rscript
# Script to create activity plots from prepared data
# Requires: data.lmraad_filtered to be loaded in environment (run script 2 first)

# =============================================================================
# 0. PACKAGE LOADING SECTION
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(ggtext)
  library(patchwork)
  library(here)
  library(lubridate)
  library(ggchicklet)
})

message("✓ Pakker til aktivitetsplot er indlæst")


# load_pkg <- function(pkg) {
#   if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
#     install.packages(pkg, dependencies = TRUE)
#     library(pkg, character.only = TRUE)
#   }
#   message("✓ Loaded: ", pkg)
# }
#
# # Load standard CRAN packages
# load_pkg("dplyr")
# load_pkg("tidyr")
# load_pkg("ggplot2")
# load_pkg("ggtext")
# load_pkg("patchwork")
# load_pkg("here")
# load_pkg("lubridate")
#
# # Special handling for ggchicklet (GitHub package)
# if (!require("ggchicklet", quietly = TRUE)) {
#   if (!require("remotes", quietly = TRUE)) {
#     install.packages("remotes")
#     library("remotes")
#   }
#   remotes::install_github("hrbrmstr/ggchicklet")
#   library("ggchicklet")
# }
# # message("✓ Loaded: ggchicklet")

# =============================================================================
# 1. Load data
# =============================================================================
# Use script designed to load data
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
# 3. DATA PREPARATION FOR PLOTTING
# =============================================================================

message("Preparing data for plotting...")

vibrant_palette <- c(
  "Almindeligt svar"      = "#082a54",
  "Kortsvar"              = "#e02b35",
  "Generel forespørgsel"  = "#59a89c",
  "Psykiatrikonference"   = "#a559aa",
  "Medicingennemgang"     = "#f0c571",
  "Andre"                 = "#808080",
  "Ibrugtagningssag"      = "#2E8B57"
)
# CHANGED: Reversed the order to bring Ibrugtagningssag to bottom and Kortsvar to top
svar_levels_plot <- c("Ibrugtagningssag","Medicingennemgang","Generel forespørgsel",
                      "Almindeligt svar","Kortsvar")
svar_levels_legend <- rev(svar_levels_plot)

ref_date_cph <- as_date(date.loadedFile)
as_of_days <- seq(ref_date_cph - days(7), ref_date_cph, by = "day") |>
  (\(x) x[wday(x, week_start = 1) <= 5])() |>
  tail(5)

# -----------------------------------------------------------------------------
# Danish weekday labels for x-axis (locale-free)
# -----------------------------------------------------------------------------

# English -> Danish weekday lookup
weekday_da <- c(
  "Monday"    = "Mandag",
  "Tuesday"   = "Tirsdag",
  "Wednesday" = "Onsdag",
  "Thursday"  = "Torsdag",
  "Friday"    = "Fredag",
  "Saturday"  = "Lørdag",
  "Sunday"    = "Søndag"
)

# Get weekday names in default (English) and translate
wd_en <- lubridate::wday(
  as_of_days,
  label      = TRUE,
  abbr       = FALSE,
  week_start = 1
)
wd_en <- as.character(wd_en)

wd <- unname(weekday_da[wd_en])

# Keep your capitalization logic (safe, already capitalized)
wd_cap <- paste0(toupper(substr(wd, 1, 1)), substr(wd, 2, nchar(wd)))

axis_labs <- setNames(
  sprintf(
    "<b>%s</b><br><span style='font-size:8pt'><i>%s</i></span>",
    wd_cap,
    format(as_of_days, "%d.%m")
  ),
  as.character(as_of_days)
)

activity_plot_data <- data.lmraad_filtered %>%
  mutate(
    AdjustedDate = as_date(AdjustedDate),
    FaerdigDate  = as_date(`Færdig (*)`),
    svar_kategori = case_when(
      `Svartype (*)` == "Medicingennemgang" ~ "Medicingennemgang",
      `Svartype (*)` == "Ibrugtagningssag"  ~ "Ibrugtagningssag",
      `Svartype (*)` == "Kortsvar"  ~ "Kortsvar",
      `Svartype (*)` == "Generel forespørgsel"  ~ "Generel forespørgsel",
      `Svartype (*)` == "Almindeligt svar"  ~ "Almindeligt svar",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(AdjustedDate),
         !is.na(svar_kategori),
         KFE_KFA == "KFA" |
           (is.na(KFE_KFA) & (svar_kategori == "Medicingennemgang" | svar_kategori == "Ibrugtagningssag")))

message("✓ Plot data prepared successfully!")

# =============================================================================
# 4. PLOTTING SECTION
# =============================================================================

message("Creating plots...")

backlog_1_42_bySvar <- activity_plot_data %>%
  crossing(as_of = as_of_days) %>%
  filter(
    if_else(
      as_of == max(as_of_days),
      is.na(FaerdigDate),
      is.na(FaerdigDate) | (FaerdigDate >= as_of & AdjustedDate < as_of)),
  ) %>%
  mutate(age = as.integer(as_of - AdjustedDate)) %>%
  filter(
    if_else(
      as_of == max(as_of_days),
      dplyr::between(age, 0, 42),
      dplyr::between(age, 1, 42)
    ),
    !is.na(svar_kategori)
  ) %>%
  mutate(svar_kategori = factor(svar_kategori, levels = svar_levels_plot)) %>%
  count(as_of, svar_kategori, name = "n") %>%
  complete(as_of = as_of_days,
           svar_kategori = factor(svar_levels_plot, levels = svar_levels_plot),
           fill = list(n = 0L)) %>%
  mutate(as_of_f = factor(as_of, levels = as_of_days))

y_top <- backlog_1_42_bySvar %>%
  group_by(as_of_f) %>%
  summarise(t = sum(n), .groups = "drop") %>%
  summarise(mx = max(t, na.rm = TRUE)) %>%
  pull(mx)
y_top6 <- max(6, ceiling(y_top / 6) * 6)
pad <- max(0.3, 0.02 * y_top6)

bars_df <- backlog_1_42_bySvar %>% filter(n > 0) %>%
  mutate(
    day_index = as.integer(factor(as_of, levels = as_of_days)),
    max_index = max(day_index),
    position_from_right = max_index - day_index,
    alpha_val = case_when(
      position_from_right == 0 ~ 1.0,
      position_from_right == 1 ~ 0.8,
      position_from_right >= 2 ~ 0.8 - (position_from_right - 1) * 0.10
    )
  ) %>%
  mutate(alpha_val = pmax(alpha_val, 0))

legend_df_backlog <- tibble::tibble(
  as_of_f = factor(as_of_days[1], levels = as_of_days),
  n = 1L,
  svar_kategori = factor(svar_levels_plot, levels = svar_levels_plot)
)

p_backlog <- ggplot() +
  annotate("segment", x = -Inf, xend = Inf, y = 0, yend = 0, colour = "grey75", linewidth = 0.6) +
  ggchicklet::geom_chicklet(
    data = legend_df_backlog,
    aes(x = as_of_f, y = n, fill = svar_kategori),
    width = 0.72, radius = grid::unit(8, "pt"),
    colour = "white", size = .1, alpha = 0,
    inherit.aes = FALSE, position = position_stack(reverse = TRUE)
  ) +
  ggchicklet::geom_chicklet(
    data = bars_df,
    aes(x = as_of_f, y = n, fill = svar_kategori, alpha = alpha_val),
    width = 0.72, radius = grid::unit(7, "pt"),
    colour = "white", size = .5,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = bars_df,
    aes(x = as_of_f, y = n, label = n),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    color = "white", size = 3.2, fontface = "bold"
  ) +
  scale_fill_manual(values = vibrant_palette[svar_levels_plot],
                    limits = svar_levels_legend, breaks = svar_levels_legend,
                    drop = FALSE, name = "Type aktivitet") +
  scale_alpha_identity() +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  scale_y_continuous(limits = c(-pad, y_top6),
                     breaks = seq(0, y_top6, by = 3),
                     minor_breaks = seq(0, y_top6, by = 1),
                     expand = c(0, 0)) +
  scale_x_discrete(labels = axis_labs) +
  labs(x = NULL, y = "Forespørgsler", title = NULL,
       subtitle = NULL) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    legend.position = "right",
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(0, "pt"),
    legend.spacing.y = unit(0, "pt"),
    legend.key.width = unit(6, "pt"),
    legend.key.height = unit(6, "pt"),
    axis.text.x = ggtext::element_markdown(size = 9, lineheight = 1.05),
    axis.ticks.length = unit(2, "pt"),
    axis.line.x = element_blank(),
    axis.ticks.length.x = unit(0, "pt"),
    axis.ticks = element_line(colour = "black", linewidth = 0.2),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 15)),
    plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
  )

p_backlog_noleg <- p_backlog + theme(legend.position = "none",
                                     plot.margin = margin(t = -5, r = 2, b = -10, l = 1, "pt"))
p_backlog_legend <- ggplot() +
  ggchicklet::geom_chicklet(
    data = legend_df_backlog,
    aes(x = as_of_f, y = n, fill = svar_kategori),
    width = 0.72, radius = grid::unit(8, "pt"),
    colour = "white", size = .1, alpha = 0,
    inherit.aes = FALSE, position = position_stack(reverse = TRUE)
  ) +
  scale_fill_manual(values = vibrant_palette[svar_levels_plot],
                    limits = svar_levels_legend, breaks = svar_levels_legend,
                    drop = FALSE, name = "Type aktivitet") +
  guides(fill = guide_legend(override.aes = list(alpha = 1))) +
  theme_void() +
  theme(
    legend.position = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.x = unit(0, "pt"),
    legend.spacing.y = unit(0, "pt"),
    plot.margin = margin(t = 3, r = 0, b = 0, l = 0, "pt")
  )

as_of <- max(as_of_days)
# CHANGED: Replaced Kortsvar with Ibrugtagningssag and reordered as requested
y_levels <- c("Ibrugtagningssag", "Medicingennemgang", "Generel forespørgsel", "Almindeligt svar")
status_levels <- c("Modtaget", "Fordelt", "Startet", "Ved bagvagt")
status_cols <- c("Modtaget" = "#444444", "Fordelt" = "#808080", "Startet" = "#E6D27F", "Ved bagvagt" = "#EFBDA7")

scatter_df <- activity_plot_data %>%
  filter(is.na(FaerdigDate) | FaerdigDate >= as_of) %>%
  mutate(
    age_adjusted = as.integer(as_of - AdjustedDate),
    status_norm = case_when(
      Status %in% c("Modtaget", "Fordelt", "Startet", "Ved bagvagt") ~ Status,
      TRUE ~ NA_character_
    ),
    x_age = if_else(age_adjusted <= 21, age_adjusted, 23L)
  ) %>%
  filter(!is.na(svar_kategori), !is.na(status_norm),
         dplyr::between(age_adjusted, 0, 44)) %>%
  mutate(
    svar_kategori = factor(svar_kategori),
    status_norm   = factor(status_norm, levels = status_levels)
  ) %>%
  add_count(x_age, svar_kategori, name = "n_grp") %>%
  group_by(x_age, svar_kategori) %>%
  mutate(
    idx_in_grp = row_number(),
    y_base     = as.integer(factor(svar_kategori, levels = y_levels)),
    y_offset   = ifelse(n_grp == 1, 0,
                        scales::rescale(idx_in_grp, to = c(-0.2, 0.2))),
    y_jit      = y_base + y_offset
  ) %>%
  ungroup()

legend_df_status <- tibble::tibble(
  x_age = 0L,
  svar_kategori = factor(rep(y_levels[1], length(status_levels)), levels = y_levels),
  status_norm = factor(status_levels, levels = status_levels)
) %>%
  mutate(y_idx = as.integer(svar_kategori))

p_age_bottom <- ggplot() +
  geom_point(
    data = legend_df_status,
    aes(x = x_age, y = y_idx, fill = status_norm),
    shape = 21, size = 2.8,
    stroke = 0.6, colour = "black",
    alpha = 0,
    show.legend = TRUE
  ) +
  # Fordelt, Startet, Ved bagvagt - filled circles
  geom_point(
    data = scatter_df %>% filter(status_norm %in% c("Fordelt", "Startet", "Ved bagvagt")),
    aes(x = x_age, y = y_jit, fill = status_norm),
    shape = 21, size = 2.8, stroke = 0.6,
    colour = "black", alpha = 0.75
  ) +
  # Modtaget - circle with cross
  geom_point(
    data = scatter_df %>% filter(status_norm == "Modtaget"),
    aes(x = x_age, y = y_jit),
    shape = 13, size = 2.8,
    colour = "black", alpha = 0.75
  ) +
  geom_hline(
    yintercept = seq_along(y_levels) + 0.5,
    color = "grey", linewidth = 0.1, linetype = "solid"
  ) +
  scale_fill_manual(values = status_cols, limits = status_levels, drop = FALSE, name = "Status") +
  guides(fill = guide_legend(
    ncol = 1, byrow = TRUE,
    override.aes = list(
      alpha = 0.75,
      shape = c(13, 21, 21, 21),
      size = 3,
      colour = "black",
      stroke = 0.6,
      fill = c(NA, status_cols["Fordelt"], status_cols["Startet"], status_cols["Ved bagvagt"])
    ),
    keyheight = unit(6, "pt")
  )) +
  scale_y_continuous(limits = c(0.5, length(y_levels) + 0.5),
                     breaks = seq_along(y_levels),
                     labels = y_levels,
                     expand = c(0, 0)) +
  scale_x_continuous(
    breaks  = c(0, 7, 14, 21, 23),
    labels  = c("0", "7", "14", "21", "21+"),
    limits  = c(0, 23),
    minor_breaks = setdiff(0:23, c(0, 7, 14, 21, 23)),
    expand  = c(0.02, 0)
  ) +
  guides(
    x = guide_axis(minor.ticks = TRUE)
  ) +
  labs(x = "Tid siden modtaget (dage)",
       y = NULL,
       title = paste0("Liggetid fordelt på svartype per d. ", format(max(as_of_days), "%d.%m"))) +
  coord_cartesian(clip = "off") +
  theme_classic() +
  theme(
    panel.grid = element_blank(),
    axis.ticks.length.x = unit(0.1, "cm"),
    legend.position = c(0.98, 0.98),
    legend.justification = c(1, 1),
    legend.background = element_rect(fill = scales::alpha("white", 0.25),
                                     colour = "black", linewidth = 0.2),
    legend.margin = margin(2, 3, 2, 3),
    legend.key.height = unit(6, "pt"),
    legend.key.width = unit(8, "pt"),
    legend.title = element_text(face = "bold", size = 8),
    legend.text = element_text(size = 7.5),
    axis.text.y = element_text(angle = 0, hjust = 1),
    axis.ticks = element_line(colour = "black", linewidth = 0.2),
    axis.minor.ticks.length = rel(0.7),        # minor = 50% of major
    axis.title.x = element_text(margin = margin(t = 5, b = 5)),
    plot.title = element_text(size = 13, hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(size = 9, hjust = 0.5, face = "plain"),
    plot.title.position = "plot",
    plot.margin = margin(t = -5, r = 0, b = 0, l = 6, "pt")
  )

horizontal_sep <- ggplot() +
  annotate("segment", x = 0, xend = 1, y = 0.5, yend = 0.5,
           colour = "black", linewidth = 0.2) +
  theme_void() +
  theme(plot.margin = margin(t = 0, r = 0, b = 5, l = 0, "pt"))

# =============================================================================
# 4b. SUBTITLE: MISSING / CONFLICTING AFFILIATIONS
# =============================================================================
# Now covers:
#   - is.na(KFE_KFA)
#   - is.na(Forvagt_affiliation)
#   - is.na(Bagvagt_affiliation)
# and conflicting:
#   - Forvagt_affiliation != Bagvagt_affiliation

affiliation_issues <- data.lmraad_filtered %>%
  mutate(
    AdjustedDate = as_date(AdjustedDate),
    FaerdigDate = as_date(`Færdig (*)`)
  ) %>%
  filter(
    AdjustedDate <= max(as_of_days),
    (is.na(FaerdigDate) | FaerdigDate >= max(as_of_days)),
    `Svartype (*)` %in% c("Almindeligt svar", "Kortsvar"),
    dplyr::between(as.integer(max(as_of_days) - AdjustedDate), 0, 42)
  ) %>%
  mutate(
    missing_kfe_kfa        = is.na(KFE_KFA),
    missing_forvagt_affil  = is.na(Forvagt_affiliation),
    missing_bagvagt_affil  = is.na(Bagvagt_affiliation),
    conflicting_affil      = !is.na(Forvagt_affiliation) &
                             !is.na(Bagvagt_affiliation) &
                             Forvagt_affiliation != Bagvagt_affiliation
  )

# ----- Missing affiliation names (Forvagt + Bagvagt) -----
missing_forvagt_names <- affiliation_issues %>%
  filter(missing_kfe_kfa | missing_forvagt_affil) %>%
  distinct(`Forvagt*`) %>%
  filter(!is.na(`Forvagt*`)) %>%
  pull(`Forvagt*`)

missing_bagvagt_names <- affiliation_issues %>%
  filter(missing_kfe_kfa | missing_bagvagt_affil) %>%
  distinct(`Bagvagt*`) %>%
  filter(!is.na(`Bagvagt*`)) %>%
  pull(`Bagvagt*`)

missing_names <- unique(c(missing_forvagt_names, missing_bagvagt_names))

subtitle_parts <- character(0)

if (length(missing_names) > 0) {
  if (length(missing_names) > 5) {
    missing_names_text <- paste0(paste(missing_names[1:5], collapse = ", "), ", ...")
  } else {
    missing_names_text <- paste(missing_names, collapse = ", ")
  }

  missing_text <- paste0(
    "Manglende KFE/KFA tilhørsforhold:\n",
    "| ", missing_names_text, " | Opdater tilhørsforhold"
  )

  subtitle_parts <- c(subtitle_parts, missing_text)
}

# ----- Conflicting affiliations (Forvagt_affiliation != Bagvagt_affiliation) -----
conflicting_pairs <- affiliation_issues %>%
  filter(conflicting_affil) %>%
  distinct(`Forvagt*`, `Bagvagt*`) %>%
  filter(!is.na(`Forvagt*`), !is.na(`Bagvagt*`))

if (nrow(conflicting_pairs) > 0) {
  pair_labels <- paste(conflicting_pairs$`Forvagt*`, "&", conflicting_pairs$`Bagvagt*`)

  if (length(pair_labels) > 5) {
    pair_text <- paste0(paste(pair_labels[1:5], collapse = ", "), ", ...")
  } else {
    pair_text <- paste(pair_labels, collapse = ", ")
  }

  conflict_text <- paste0(
    "Modstridende tilhørsforhold:\n",
    "| ", pair_text, " | Opdater tilhørsforhold"
  )

  subtitle_parts <- c(subtitle_parts, conflict_text)
}

# ----- Apply subtitle (if any issues) -----
if (length(subtitle_parts) > 0) {
  subtitle_text <- paste(subtitle_parts, collapse = "\n\n")

  p_age_bottom <- p_age_bottom +
    labs(subtitle = subtitle_text) +
    theme(plot.subtitle = element_text(color = "red", size = 9, hjust = 0.5))
}

areas <- c(
  area(t = 1, l = 1,  b = 1, r = 17),
  area(t = 1, l = 18, b = 1, r = 19),
  area(t = 2, l = 1,  b = 2, r = 18),
  area(t = 3, l = 2,  b = 3, r = 18)
)

combined <- p_backlog_noleg + p_backlog_legend + horizontal_sep + p_age_bottom +
  plot_layout(
    design = areas,
    widths = c(rep(1, 17), 1, 1),
    heights = c(5, 0.35, 3)
  ) + plot_annotation(
    title = "Igangværende aktiviteter",
    subtitle = "Klinisk Farmakologisk Afdeling",
    theme = theme(
      plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(size = 12, hjust = 0.5)
      #plot.margin = margin(t = 5, r = 0, b = 0, l = 0, "pt")
    ))

combined_grob <- patchwork::patchworkGrob(combined)

# =============================================================================
# 5. SAVE PLOT
# =============================================================================

message("Saving plot...")

plots_dir <- here("statistik", "output", "current activities")
ensure_directory <- function(dir_path) {
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }
}
ensure_directory(plots_dir)

plot_filename <- paste0("activity_plot_", format(ref_date_cph, "%Y-%m-%d"), ".png")
plot_path <- here(plots_dir, plot_filename)

ggsave(plot_path, combined_grob, width = 8, height = 5, dpi = 300)
message("✓ Plot saved: ", plot_path)

message("Displaying plot...")
grid::grid.newpage()
grid::grid.draw(combined_grob)

message("✓ Plot creation completed successfully!")
