#!/usr/bin/env Rscript
# Script to load data from Azure and Excel, then save prepared data
# Output folder: "../Azure download/"

# =============================================================================
# 1. PACKAGE LOADING
# =============================================================================

# # Function to load packages
# load_pkg <- function(pkg) {
#   if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
#     install.packages(pkg, dependencies = TRUE)
#     library(pkg, character.only = TRUE)
#   }
#   message("✓ Loaded: ", pkg)
# }
#
# # Load required packages
# load_pkg("Microsoft365R")
# load_pkg("dplyr")
# load_pkg("readr")
# load_pkg("lubridate")
# load_pkg("readxl")
# load_pkg("here")
# load_pkg("tidyr")

message("Start of script")

# Simple approach for server
library(Microsoft365R)
library(dplyr)
library(readr)
library(lubridate)
library(readxl)
library(here)
library(tidyr)

# =============================================================================
# 2. SETUP AND UTILITY FUNCTIONS
# =============================================================================

# Function to ensure directory exists
ensure_directory <- function(dir_path) {
  if(!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }
}

# Get today's date for file naming
creation_date_string <- format(Sys.Date(), "%d-%m-%y")


# =============================================================================
# 3. AZURE DATA LOADING
# =============================================================================

message("\nLoading data from Azure...")

# Load site and document library
site_list <- list_sharepoint_sites()
site <- site_list[[1]]
doc_lib <- site$get_list("Dokumenter")

# Define display names
cols_to_analyze <- c(
  "Id",
  "Svartype (*)",
  "Forvagt*",
  "Bagvagt*",
  "Modtaget (*)",
  "Færdig (*)",
  "Speciale (*)",
  "Speciale",
  "Hospital (*)",
  "Region (*)",
  "Status",
  "Forvagt_x002a_",
  "Bagvagt_x002a_"
)

# Map display names to internal names, fixing "ID" mismatch
colinfo <- doc_lib$get_column_info()
needed_info <- subset(colinfo, displayName %in% cols_to_analyze)
needed_info$name[needed_info$name == "ID"] <- "id"

# Retrieve selected columns
internal_names <- c(needed_info$name, "FileRef")
all_items <- doc_lib$list_items(select = internal_names, n = Inf)

# Rename internal names to display names
items_df <- all_items
match_idx <- match(needed_info$name, names(items_df))
names(items_df)[match_idx] <- needed_info$displayName

# Filter for each folder
data.lmraad <- subset(
  items_df,
  startsWith(FileRef, "/sites/Lgemiddelrdgivning/Delte dokumenter/Lægemiddelrådgivning")
)

# Remove rows where all specified columns are NA
data.lmraad <- data.lmraad %>%
  filter(
    rowSums(
      is.na(select(., `Svartype (*)`, `Forvagt*`, `Bagvagt*`, `Modtaget (*)`, `Færdig (*)`, `Speciale (*)`, `Hospital (*)`, `Region (*)`))
    ) < 8  # 8 is the number of columns being checked
  )

data.medicingennemgang <- subset(
  items_df,
  startsWith(FileRef, "/sites/Lgemiddelrdgivning/Delte dokumenter/Medicingennemgang")
)
# Only show rows where svartype is not empty
data.medicingennemgang <- data.medicingennemgang[!is.na(data.medicingennemgang$`Svartype (*)`),]

# Make medicingennemgang SDCA etc. also appear as "Medicingennemgang
data.medicingennemgang$`Svartype (*)` <- "Medicingennemgang"

data.ibrugtagning <- subset(
  items_df,
  startsWith(FileRef, "/sites/Lgemiddelrdgivning/Delte dokumenter/Ibrugtagningssag/")
)
# Exclude document enteries by removing rows with NA hospital
data.ibrugtagning <- data.ibrugtagning[!is.na(data.ibrugtagning$`Hospital (*)`),]

# Ensure
data.medicingennemgang$`Svartype (*)` <- "Medicingennemgang"

format_dates <- function(data) {
  data %>%
    mutate(
      `Modtaget (*)` = if_else(
        is.na(`Modtaget (*)`),
        # if missing, stay missing (as a Date)
        as.Date(NA),
        # else parse, shift to CEST, then drop the time
        as_date(
          with_tz(
            ymd_hms(`Modtaget (*)`, tz = "UTC"),
            "Europe/Copenhagen"
          )
        )
      ),
      `Færdig (*)` = if_else(
        is.na(`Færdig (*)`),
        as.Date(NA),
        as_date(
          with_tz(
            ymd_hms(`Færdig (*)`, tz = "UTC"),
            "Europe/Copenhagen"
          )
        )
      )
    )
}

# Apply function
data.lmraad <- format_dates(data.lmraad)
data.medicingennemgang <- format_dates(data.medicingennemgang)
data.ibrugtagning <- format_dates(data.ibrugtagning)

message("✓ Azure data loaded successfully")


# =============================================================================
# 4. EXCEL FILE LOADING
# =============================================================================

message("\nLoading newest Excel file...")

# Function to get the newest Excel file in a directory
get_newest_excel_file <- function(directory_path) {
  # Get all Excel files in the directory
  excel_files <- list.files(
    path = directory_path,
    pattern = "\\.xlsx?$",  # Matches .xlsx and .xls files
    full.names = TRUE
  )

  # If no files found, return NULL
  if (length(excel_files) == 0) {
    return(NULL)
  }

  # Get file info and find the newest file
  file_info <- file.info(excel_files)
  newest_file <- excel_files[which.max(file_info$mtime)]

  return(newest_file)
}

# Main file loading logic
main_folder <- here("statistik", "data", "KFE KFA")
original_folder <- here("statistik", "data", "KFE KFA", "Original")

# Try to get newest file from main folder
newest_file <- get_newest_excel_file(main_folder)

# If no file found in main folder, try the Original subfolder
if (is.null(newest_file)) {
  newest_file <- get_newest_excel_file(original_folder)

  if (!is.null(newest_file)) {
    message("LOADING ORIGINAL FILE")
    file_source <- "Original folder"
  } else {
    stop("No Excel files found in either the main folder or Original subfolder")
  }
} else {
  file_source <- "Main folder"
}

# Print which file is being loaded
message("Loading: ", basename(newest_file))
message("Source: ", file_source)

# Read the Excel file into R
KFE_KFA_affiliation <- read_excel(newest_file)

# Reshape the data
KFE_KFA_affiliation.long <- KFE_KFA_affiliation %>%
  pivot_longer(
    cols = c(KFA, KFE),
    names_to = "afdeling",
    values_to = "navn",
    values_drop_na = TRUE
  )

message("✓ Excel affiliation data loaded successfully")


# =============================================================================
# 5. DATA PREPARATION
# =============================================================================

message("\nPreparing and transforming data...")

# Map English weekday names to Danish
eng_to_dk <- c(
  "Monday"    = "Mandag",
  "Tuesday"   = "Tirsdag",
  "Wednesday" = "Onsdag",
  "Thursday"  = "Torsdag",
  "Friday"    = "Fredag",
  "Saturday"  = "Lørdag",
  "Sunday"    = "Søndag"
)

# Define the function
transform_data <- function(data) {

  # Merge `Speciale` into `Speciale (*)` and set `Speciale (*)` to NA when `Speciale` is not NA
  data <- data %>%
    mutate(
      `Speciale (*)` = if_else(
        !is.na(Speciale),  # If `Speciale` is not NA
        NA_character_,     # Set `Speciale (*)` to NA
        `Speciale (*)`     # Otherwise, keep `Speciale (*)` as is
      ),
      `Speciale (*)` = coalesce(`Speciale (*)`, Speciale)  # Merge `Speciale` into `Speciale (*)`
    )

  # Update `Modtaget (*)` where previous and next values are the same and not NA
  data <- data %>%
    mutate(Id = as.numeric(Id)) %>%
    arrange(Id) %>%  # Ensure data is sorted by Id
    mutate(
      prev_modtaget = lag(`Modtaget (*)`),  # Previous row's Modtaget (*)
      next_modtaget = lead(`Modtaget (*)`),  # Next row's Modtaget (*)
      `Modtaget (*)` = if_else(
         is.na(`Modtaget (*)`) & !is.na(prev_modtaget) & !is.na(next_modtaget) & prev_modtaget == next_modtaget,
        prev_modtaget,
        `Modtaget (*)`
      )
    ) %>%
    select(-prev_modtaget, -next_modtaget)

  # Additional transformations
  data <- data %>%
    mutate(
      # 1) Basic columns
      Year      = year(`Modtaget (*)`),
      Month     = month(`Modtaget (*)`),
      MonthDate = floor_date(`Modtaget (*)`, unit = "month"),
      `Svartype (*)` = case_when(
      `Svartype (*)` %in% c("Medicingennemgang SDCA", "SDCA MGG MADS") ~ "Medicingennemgang",
      TRUE ~ `Svartype (*)`
    ),

      # 2) Replace missing values
      `Svartype (*)` = replace_na(`Svartype (*)`, "Andre"),
      `Region (*)`   = replace_na(`Region (*)`, "Andre"),

      # 3) English weekday to be adjusted
      ugedag_modtaget = weekdays(as.Date(`Modtaget (*)`)),
      ugedag_svaret = weekdays(as.Date(`Færdig (*)`))
    ) %>%
    # 4) Adjust weekend days to Monday of the following week
    mutate(
      AdjustedDate = case_when(
        ugedag_modtaget == "Saturday" ~ as.Date(`Modtaget (*)`) + days(2),
        ugedag_modtaget == "Sunday"   ~ as.Date(`Modtaget (*)`) + days(1),
        TRUE ~ as.Date(`Modtaget (*)`)
      ),
      ugedag_modtaget = weekdays(AdjustedDate), # Weekdays based on adjusted date
      WeekNumber = isoweek(AdjustedDate),
      svartid = as.Date(`Færdig (*)`) - AdjustedDate
    ) %>%
    # 5) Convert English day names to Danish (and remove names attribute)
    mutate(
      ugedag_modtaget = unname(eng_to_dk[ugedag_modtaget]),
      ugedag_svaret = unname(eng_to_dk[ugedag_svaret]),

      # 6) Define sektor
      sektor = case_when(
        `Speciale (*)` == "Almen medicin" ~ "Almen praksis",
        is.na(`Hospital (*)`) ~ "Almen praksis",
        is.na(`Hospital (*)`) & is.na(`Speciale (*)`) ~ "Almen praksis",
        TRUE ~ "Hospital"
      ),

      # 7) Define corrected specialties
      specialeCorrected = case_when(
        `Speciale (*)` == "Almen medicin" ~ "Almen praksis",
        is.na(`Hospital (*)`) ~ "Almen praksis",
        is.na(`Hospital (*)`) & is.na(`Speciale (*)`) ~ "Almen praksis",
        !is.na(`Speciale (*)`) ~ `Speciale (*)`,
        TRUE ~ "Andre"
      ),

      # 8) NEW: Add svar_kategori and ensure proper date formats
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
    )

  return(data)
}

# Apply the function to all three dataframes
data.lmraad <- transform_data(data.lmraad)
data.medicingennemgang <- transform_data(data.medicingennemgang)
data.ibrugtagning <- transform_data(data.ibrugtagning)
data.ibrugtagning$`Svartype (*)` <- "Ibrugtagningssag"

# Make data frame with all contacts
data.lmraad_filtered <- rbind(data.lmraad, data.medicingennemgang, data.ibrugtagning)

# Define current year/month if needed later
CurrentYear <- year(Sys.Date())
CurrentMonth <- month(Sys.Date())

# Today's date
today_cph <- today(tzone = "Europe/Copenhagen")

# Add age col to state the age
data.lmraad_filtered <- data.lmraad_filtered %>%
  mutate(
    # integer days since AdjustedDate
    age_adjusted = as.integer(today_cph - AdjustedDate)
  )

# Remove and reorder cols
data.lmraad_filtered <- data.lmraad_filtered %>%
  select(
    Id,
    everything(),
    -`@odata.etag`
  ) %>%
  relocate(FileRef, .after = last_col())

# Record min and max dates used
start_date <- min(data.lmraad_filtered$`Modtaget (*)`, na.rm = TRUE)
end_date   <- max(data.lmraad_filtered$`Modtaget (*)`, na.rm = TRUE)
start_date_string <- format(as.Date(start_date), "%d-%m-%Y")

message("✓ Data transformation completed")


# =============================================================================
# 6. AFFILIATION MERGING
# =============================================================================

message("\nMerging affiliation data...")

# This is the cleanest and most readable approach
data.lmraad_filtered <- data.lmraad_filtered %>%
  # First try to match Forvagt
  left_join(KFE_KFA_affiliation.long %>%
              select(navn, afdeling) %>%
              rename(Forvagt_affiliation = afdeling),
            by = c("Forvagt*" = "navn")) %>%
  # Then try to match Bagvagt
  left_join(KFE_KFA_affiliation.long %>%
              select(navn, afdeling) %>%
              rename(Bagvagt_affiliation = afdeling),
            by = c("Bagvagt*" = "navn")) %>%

   # Apply the priority logic
  mutate(
    KFE_KFA = case_when(
      !is.na(Forvagt_affiliation) ~ Forvagt_affiliation,
      !is.na(Bagvagt_affiliation) ~ Bagvagt_affiliation,
      TRUE ~ NA_character_
    )
  )

message("✓ Affiliation data merged successfully")


# =============================================================================
# 7. REORGANISE STATUS
# =============================================================================

message("\nReorganising status values...")

data.lmraad_filtered <- data.lmraad_filtered %>%
  mutate(
    Status = case_when(
      is.na(Status) & `Svartype (*)` == "Ibrugtagningssag" ~ "Modtaget",
      TRUE                                                 ~ Status
    )
  ) %>%
  mutate(
    Status = case_when(
      Status == "Modtaget" & !is.na(`Forvagt*`) ~ "Fordelt",
      TRUE                                      ~ Status
    )
  )

message("✓ Status values reorganised successfully")


# =============================================================================
# 8. DATA SAVING AND FILE CLEANING
# =============================================================================

# 8.a DATA SAVING
message("\nSaving prepared data...")

# Set up output directory using project root: Data/Azure data
output_dir <- here("statistik", "Data", "Azure data")
ensure_directory(output_dir)

message("Output directory: ", output_dir)

# Generate filename with today's date
today_date <- format(Sys.Date(), "%Y-%m-%d")
file_name <- paste0("azure_", today_date, ".rds")
file_path <- here(output_dir, file_name)

# Remove existing file for today if it exists
if (file.exists(file_path)) {
  file.remove(file_path)
  message("✓ Replaced existing file for today")
}

# Save main dataset as RDS
saveRDS(data.lmraad_filtered, file_path)
message("✓ Saved: ", file_name)

# 8.b FILE CLEANING
message("\nCleaning up old files...")

# Clean up old files - keep only latest 5
cleanup_old_files <- function(directory, pattern = "azure_.*\\.rds", keep_count = 5) {
  # Get all azure rds files
  files <- list.files(directory, pattern = pattern, full.names = TRUE)

  if (length(files) > keep_count) {
    # Get file info and sort by modification time (newest first)
    file_info <- file.info(files)
    file_info$file <- files

    # Sort by modification time (newest first)
    sorted_files <- file_info[order(file_info$mtime, decreasing = TRUE), ]

    # Files to keep (newest ones)
    files_to_keep <- sorted_files$file[1:keep_count]

    # Files to delete (oldest ones)
    files_to_delete <- setdiff(sorted_files$file, files_to_keep)

    # Delete oldest files
    if (length(files_to_delete) > 0) {
      file.remove(files_to_delete)
      message("✓ Cleaned up ", length(files_to_delete), " old file(s)")
    }
  }
}

# Run cleanup
cleanup_old_files(output_dir)

# Summary
current_files <- list.files(output_dir, pattern = "azure_.*\\.rds")
message("\nCurrent files in directory (", length(current_files), "):")
for (file in current_files) {
  message("  - ", file)
}


# =============================================================================
# 9. COMPLETION SUMMARY
# =============================================================================

message("\n", rep("=", 50))
message("DATA PREPARATION COMPLETED SUCCESSFULLY!")
message(rep("=", 50))
message("Main dataset: ", nrow(data.lmraad_filtered), " rows")
message("Creation date: ", creation_date_string)
message("Data range: ", start_date, " to ", end_date)
message("Output location: ", normalizePath(output_dir))
message("Files created:")
message("  - prepared_data.rds (main dataset)")
message("  - prepared_data.csv (main dataset)")
message("  - metadata.rds (processing metadata)")
message(rep("=", 50))
