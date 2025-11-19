#!/usr/bin/env Rscript
# Script to load the newest prepared data file

# =============================================================================
# 1. PACKAGE LOADING SECTION
# =============================================================================

# Lightweight function to load packages
load_pkg <- function(pkg) {
  if(!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
  message("✓ Loaded: ", pkg)
}

# Load required packages
load_pkg("here")

# =============================================================================
# 2. DATA LOADING SECTION
# =============================================================================

message("Loading newest prepared data file...")

# Function to get the newest RDS file in a directory
get_newest_rds_file <- function(directory_path) {
  # Get all RDS files in the directory
  rds_files <- list.files(
    path = directory_path,
    pattern = "^azure_.*\\.rds$",  # Matches azure_YYYY-MM-DD.rds files
    full.names = TRUE
  )

  # If no files found, return NULL
  if (length(rds_files) == 0) {
    return(NULL)
  }

  # Get file info and find the newest file
  file_info <- file.info(rds_files)
  newest_file <- rds_files[which.max(file_info$mtime)]

  return(newest_file)
}

# Set up data directory
data_dir <- here("statistik", "Data", "Azure data")

# Check if directory exists
if (!dir.exists(data_dir)) {
  stop("Data directory does not exist: ", data_dir,
       "\nPlease run 01_azure_data_load_prep.R first")
}

# Get newest file
newest_file <- get_newest_rds_file(data_dir)

if (is.null(newest_file)) {
  stop("No RDS files found in: ", data_dir,
       "\nPlease run 01_azure_data_load_prep.R first")
}

# Print which file is being loaded
message("Loading: ", basename(newest_file))
message("File date: ", file.info(newest_file)$mtime)

# Load the data
data.lmraad_filtered <- readRDS(newest_file)

message("✓ Data loaded successfully!")
message("Data is available as: data.lmraad_filtered")
