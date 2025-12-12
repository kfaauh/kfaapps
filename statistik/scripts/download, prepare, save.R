#!/usr/bin/env Rscript
# Script to load data from Azure and Excel, then save prepared data
# Output folder: "../Azure download/"

# =============================================================================
# 1. PACKAGE LOADING
# =============================================================================

suppressPackageStartupMessages({
  library(Microsoft365R)
  library(AzureAuth)
  library(AzureGraph)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(readxl)
  library(here)
  library(tidyr)
  library(bizdays)
  library(timeDate)
})

# =============================================================================
# 2. AZURE CONFIG (APP-ONLY AUTH)
# =============================================================================

tenant_id      <- "61fd1d36-fecb-47ca-b7d7-d0df0370a198"
client_id      <- "9ad9b61a-dacb-4d2f-8489-3e2edfda66e1"
sharepoint_URL <- "https://aarhusuniversitet.sharepoint.com/sites/Lgemiddelrdgivning"

secret_path   <- here("statistik", "secret_for_sharepoint.txt")
client_secret <- readLines(secret_path, warn = FALSE)[1]

if (is.na(client_secret) || client_secret == "") {
  stop(
    "INVALID_SHAREPOINT_SECRET: SharePoint client secret mangler eller er tom. Opdater filen secret_for_sharepoint.txt på serveren.",
    call. = FALSE
  )
}

message("\nAuthenticating to Azure AD using app-only credentials...")

token <- tryCatch(
  {
    AzureAuth::get_azure_token(
      resource  = "https://graph.microsoft.com/.default",
      tenant    = tenant_id,
      app       = client_id,
      password  = client_secret,
      auth_type = "client_credentials",
      version   = 2
    )
  },
  error = function(e) {
    stop(
      paste0(
        "INVALID_SHAREPOINT_SECRET: Azure AD kunne ikke godkende klienthemmeligheden. ",
        "Opdater secret_for_sharepoint.txt på serveren. Teknisk fejl: ",
        e$message
      ),
      call. = FALSE
    )
  }
)

message("✓ Azure AD token acquired")
message("Connecting to SharePoint site...")

site <- get_sharepoint_site(
  site_url = sharepoint_URL,
  token    = token
)

message("✓ Connected to SharePoint site")

doc_lib <- site$get_list("Dokumenter")
message("✓ Retrieved document library 'Dokumenter'")

# =============================================================================
# 3. SETUP AND UTILITY FUNCTIONS
# =============================================================================

ensure_directory <- function(dir_path) {
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE)
    message("Created directory: ", dir_path)
  }
}

creation_date_string <- format(Sys.Date(), "%d-%m-%y")

# =============================================================================
# 4. LOAD BIVIRKNINGSINDERETNING DATA
# =============================================================================

message("\nLoading data from Bivirkningsindberetning list...")

biv_list <- site$get_list("Bivirkningsindberetning")

biv_cols <- c(
  "Lægemiddel (ATC)",
  "Primære Bivirkning",
  "Øvrige bivirkninger",
  "Matrikel",
  "Afdeling",
  "Alvorsgrad",
  "Oprettet",
  "Oprettet af"
)

biv_colinfo <- biv_list$get_column_info()
biv_needed <- subset(biv_colinfo, displayName %in% biv_cols)
biv_internal_names <- biv_needed$name

# Add id and Created columns - note lowercase "id" based on earlier outputs
biv_internal_names <- c("id", biv_internal_names, "Created")

biv_items_raw <- biv_list$list_items(select = biv_internal_names, n = Inf)
biv_items_df <- biv_items_raw

# Standardize column names: rename "id" to "Id" for consistency
if ("id" %in% names(biv_items_df)) {
  names(biv_items_df)[names(biv_items_df) == "id"] <- "Id"
  message("✓ Renamed 'id' column to 'Id'")
}

# Rename other columns to display names - safely handle missing columns
for (i in 1:nrow(biv_needed)) {
  col_name <- biv_needed$name[i]
  display_name <- biv_needed$displayName[i]

  if (col_name %in% names(biv_items_df)) {
    names(biv_items_df)[names(biv_items_df) == col_name] <- display_name
  } else {
    message("Warning: Column '", col_name, "' not found in biv_items_df")
  }
}

message("✓ Bivirkningsindberetning data loaded: ", nrow(biv_items_df), " records")

# =============================================================================
# 5. TRANSFORM BIVIRKNINGSINDERETNING DATA
# =============================================================================

message("Transforming Bivirkningsindberetning data...")

biv_items_df <- biv_items_df %>%
  rename("Modtaget (*)" = Oprettet) %>%
  mutate(
    "Modtaget (*)" = as_date(with_tz(
      ymd_hms(`Modtaget (*)`, tz = "UTC"),
      "Europe/Copenhagen"
    ))
  )

biv_items_df <- biv_items_df %>%
  mutate(
    AdjustedDate = `Modtaget (*)`,
    "Færdig (*)" = as.Date(NA)
  )

biv_items_df <- biv_items_df %>%
  mutate(
    "Svartype (*)" = "Bivirkningsindberetning",
    Status = NA_character_,
    "Speciale (*)" = NA_character_,
    "Hospital (*)" = NA_character_,
    "Region (*)" = NA_character_,
    "Forvagt*" = NA_character_,
    "Bagvagt*" = NA_character_,
    "Forvagt_x002a_" = NA_character_,
    "Bagvagt_x002a_" = NA_character_,
    Speciale = NA_character_,
    FileRef = NA_character_
  )

biv_items_df <- biv_items_df %>%
  mutate(
    Year = year(AdjustedDate),
    Month = month(AdjustedDate),
    MonthDate = floor_date(AdjustedDate, unit = "month"),
    WeekNumber = isoweek(AdjustedDate),
    ugedag_modtaget = NA_character_,
    ugedag_svaret = NA_character_,
    sektor = "Hospital",
    specialeCorrected = NA_character_,
    svar_kategori = "Bivirkningsindberetning",
    FaerdigDate = as.Date(NA)
  )

biv_items_df <- biv_items_df %>%
  mutate(
    age_adjusted = NA_integer_
  )

biv_items_df <- biv_items_df %>%
  select(
    any_of("Id"),
    any_of("Svartype (*)"),
    any_of("Forvagt*"),
    any_of("Bagvagt*"),
    any_of("Modtaget (*)"),
    any_of("Færdig (*)"),
    any_of("Speciale (*)"),
    any_of("Speciale"),
    any_of("Hospital (*)"),
    any_of("Region (*)"),
    any_of("Status"),
    any_of("Forvagt_x002a_"),
    any_of("Bagvagt_x002a_"),
    any_of("Year"),
    any_of("Month"),
    any_of("MonthDate"),
    any_of("ugedag_modtaget"),
    any_of("ugedag_svaret"),
    any_of("AdjustedDate"),
    any_of("WeekNumber"),
    any_of("sektor"),
    any_of("specialeCorrected"),
    any_of("svar_kategori"),
    any_of("FaerdigDate"),
    any_of("age_adjusted"),
    any_of("Lægemiddel (ATC)"),
    any_of("Primære Bivirkning"),
    any_of("Øvrige bivirkninger"),
    any_of("Matrikel"),
    any_of("Afdeling"),
    any_of("Alvorsgrad"),
    any_of("Oprettet af"),
    any_of("FileRef")
  )

data.bivirkninger <- biv_items_df
message("✓ Bivirkningsindberetning data transformed: ", nrow(data.bivirkninger), " records")

# =============================================================================
# 6. LOAD ORIGINAL LÆGEMIDDELRÅDGIVNING DATA
# =============================================================================

message("\nLoading original Lægemiddelrådgivning data...")

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

colinfo <- doc_lib$get_column_info()
needed_info <- subset(colinfo, displayName %in% cols_to_analyze)

# Handle ID column mapping to "Id"
if ("ID" %in% needed_info$name) {
  needed_info$name[needed_info$name == "ID"] <- "Id"
  message("✓ Standardized 'ID' to 'Id' in column mapping")
} else if ("id" %in% needed_info$name) {
  needed_info$name[needed_info$name == "id"] <- "Id"
  message("✓ Standardized 'id' to 'Id' in column mapping")
}

internal_names <- needed_info$name
message("Requesting columns: ", paste(internal_names, collapse = ", "))

# Add FileRef column
internal_names <- c(internal_names, "FileRef")

all_items <- doc_lib$list_items(select = internal_names, n = Inf)
items_df <- all_items

message("Actual columns retrieved: ", paste(names(items_df), collapse = ", "))

for (i in 1:nrow(needed_info)) {
  col_name <- needed_info$name[i]
  display_name <- needed_info$displayName[i]

  if (col_name %in% names(items_df)) {
    names(items_df)[names(items_df) == col_name] <- display_name
  } else {
    message("Warning: Column '", col_name, "' not found in items_df")
  }
}

# Ensure Id column exists with correct case
if ("id" %in% names(items_df)) {
  names(items_df)[names(items_df) == "id"] <- "Id"
  message("✓ Standardized 'id' to 'Id' in main data")
} else if ("ID" %in% names(items_df)) {
  names(items_df)[names(items_df) == "ID"] <- "Id"
  message("✓ Standardized 'ID' to 'Id' in main data")
}

message("Final columns in items_df: ", paste(names(items_df), collapse = ", "))

data.lmraad <- subset(
  items_df,
  startsWith(FileRef, "/sites/Lgemiddelrdgivning/Delte dokumenter/Lægemiddelrådgivning")
)

data.lmraad <- data.lmraad %>%
  filter(
    rowSums(
      is.na(select(., any_of(c(
        "Svartype (*)", "Forvagt*", "Bagvagt*", "Modtaget (*)",
        "Færdig (*)", "Speciale (*)", "Hospital (*)", "Region (*)"
      ))))
    ) < 8
  )

data.medicingennemgang <- subset(
  items_df,
  startsWith(FileRef, "/sites/Lgemiddelrdgivning/Delte dokumenter/Medicingennemgang")
)

data.medicingennemgang <- data.medicingennemgang[!is.na(data.medicingennemgang$`Svartype (*)`),]
data.medicingennemgang$`Svartype (*)` <- "Medicingennemgang"

data.ibrugtagning <- subset(
  items_df,
  startsWith(FileRef, "/sites/Lgemiddelrdgivning/Delte dokumenter/Ibrugtagningssag/")
)

data.ibrugtagning <- data.ibrugtagning[!is.na(data.ibrugtagning$`Hospital (*)`),]
data.medicingennemgang$`Svartype (*)` <- "Medicingennemgang"

format_dates <- function(data) {
  if (!"Modtaget (*)" %in% names(data)) {
    return(data)
  }

  data %>%
    mutate(
      `Modtaget (*)` = if_else(
        is.na(`Modtaget (*)`),
        as.Date(NA),
        as_date(with_tz(ymd_hms(`Modtaget (*)`, tz = "UTC"), "Europe/Copenhagen"))
      ),
      `Færdig (*)` = if_else(
        is.na(`Færdig (*)`),
        as.Date(NA),
        as_date(with_tz(ymd_hms(`Færdig (*)`, tz = "UTC"), "Europe/Copenhagen"))
      )
    )
}

data.lmraad <- format_dates(data.lmraad)
data.medicingennemgang <- format_dates(data.medicingennemgang)
data.ibrugtagning <- format_dates(data.ibrugtagning)

message("✓ Original Azure data loaded successfully")

# =============================================================================
# 7. EXCEL FILE LOADING
# =============================================================================

message("\nLoading newest Excel file...")

get_newest_excel_file <- function(directory_path) {
  excel_files <- list.files(
    path = directory_path,
    pattern = "\\.xlsx?$",
    full.names = TRUE
  )

  if (length(excel_files) == 0) {
    return(NULL)
  }

  file_info <- file.info(excel_files)
  excel_files[which.max(file_info$mtime)]
}

main_folder <- here("statistik", "data", "tilknytning, KFE_KFA")
original_folder <- here("statistik", "data", "tilknytning, KFE_KFA", "Original")

newest_file <- get_newest_excel_file(main_folder)

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

message("Loading: ", basename(newest_file))
message("Source: ", file_source)

KFE_KFA_affiliation <- read_excel(newest_file)

KFE_KFA_affiliation.long <- KFE_KFA_affiliation %>%
  pivot_longer(
    cols = c(KFA, KFE),
    names_to = "afdeling",
    values_to = "navn",
    values_drop_na = TRUE
  )

message("✓ Excel affiliation data loaded successfully")

# =============================================================================
# 8. DATA PREPARATION
# =============================================================================

message("\nPreparing and transforming data...")

eng_to_dk <- c(
  "Monday"    = "Mandag",
  "Tuesday"   = "Tirsdag",
  "Wednesday" = "Onsdag",
  "Thursday"  = "Torsdag",
  "Friday"    = "Fredag",
  "Saturday"  = "Lørdag",
  "Sunday"    = "Søndag"
)

dk_helligdage <- function(years) {
  years <- unique(years)
  easter <- as.Date(timeDate::Easter(years))

  c(
    as.Date(paste0(years, "-01-01")),
    as.Date(paste0(years, "-12-25")),
    as.Date(paste0(years, "-12-26")),
    as.Date(paste0(years, "-06-05")),
    as.Date(paste0(years, "-12-24")),
    as.Date(paste0(years, "-12-31")),
    easter - 3,
    easter - 2,
    easter + 1,
    easter + 39,
    easter + 50
  )
}

all_dates <- c(
  data.lmraad$`Modtaget (*)`,
  data.medicingennemgang$`Modtaget (*)`,
  data.ibrugtagning$`Modtaget (*)`,
  data.bivirkninger$`Modtaget (*)`,
  Sys.Date()
)

min_year <- min(lubridate::year(all_dates), na.rm = TRUE)
max_year <- max(lubridate::year(all_dates), na.rm = TRUE) + 5

years_range <- min_year:max_year
dk_hols <- dk_helligdage(years_range)

bizdays::create.calendar(
  name        = "DK_weekends_only",
  weekdays    = c("saturday", "sunday"),
  holidays    = c(),
  adjust.from = bizdays::adjust.next,
  adjust.to   = bizdays::adjust.previous
)

bizdays::create.calendar(
  name        = "DK_helligdage",
  weekdays    = c("saturday", "sunday"),
  holidays    = dk_hols,
  adjust.from = bizdays::adjust.next,
  adjust.to   = bizdays::adjust.previous
)

transform_data <- function(data) {
  if (!"Id" %in% names(data)) {
    message(
      "Warning: Id column not found in data. Available columns: ",
      paste(names(data), collapse = ", ")
    )
    if ("ID" %in% names(data)) {
      data <- data %>% rename(Id = ID)
      message("Renamed ID to Id")
    } else if ("id" %in% names(data)) {
      data <- data %>% rename(Id = id)
      message("Renamed id to Id")
    } else {
      stop("No Id column found and cannot create one")
    }
  }

  if ("Speciale" %in% names(data) && "Speciale (*)" %in% names(data)) {
    data <- data %>%
      mutate(
        `Speciale (*)` = if_else(!is.na(Speciale), NA_character_, `Speciale (*)`),
        `Speciale (*)` = coalesce(`Speciale (*)`, Speciale)
      )
  }

  if ("Modtaget (*)" %in% names(data)) {
    data <- data %>%
      mutate(Id = as.numeric(Id)) %>%
      arrange(Id) %>%
      mutate(
        prev_modtaget = lag(`Modtaget (*)`),
        next_modtaget = lead(`Modtaget (*)`),
        `Modtaget (*)` = if_else(
          is.na(`Modtaget (*)`) &
            !is.na(prev_modtaget) &
            !is.na(next_modtaget) &
            prev_modtaget == next_modtaget,
          prev_modtaget,
          `Modtaget (*)`
        )
      ) %>%
      select(-any_of(c("prev_modtaget", "next_modtaget")))
  }

  data <- data %>%
    mutate(
      Year      = year(`Modtaget (*)`),
      Month     = month(`Modtaget (*)`),
      MonthDate = floor_date(`Modtaget (*)`, unit = "month"),
      `Svartype (*)` = case_when(
        `Svartype (*)` %in% c("Medicingennemgang SDCA", "SDCA MGG MADS") ~ "Medicingennemgang",
        TRUE ~ `Svartype (*)`
      ),
      `Svartype (*)` = replace_na(`Svartype (*)`, "Andre"),
      `Region (*)`   = replace_na(`Region (*)`, "Andre"),
      ugedag_modtaget = weekdays(as.Date(`Modtaget (*)`)),
      ugedag_svaret   = weekdays(as.Date(`Færdig (*)`))
    ) %>%
    mutate(
      AdjustedDate = case_when(
        ugedag_modtaget == "Saturday" ~ as.Date(`Modtaget (*)`) + days(2),
        ugedag_modtaget == "Sunday"   ~ as.Date(`Modtaget (*)`) + days(1),
        TRUE ~ as.Date(`Modtaget (*)`)
      ),
      ugedag_modtaget = weekdays(AdjustedDate),
      WeekNumber      = isoweek(AdjustedDate),
      svartid.raw = as.Date(`Færdig (*)`) - AdjustedDate,
      svartid.NoWeekend = bizdays::bizdays(
        AdjustedDate,
        as.Date(`Færdig (*)`),
        cal = "DK_weekends_only"
      ),
      svartid.NoWeekendNoHolidays = bizdays::bizdays(
        AdjustedDate,
        as.Date(`Færdig (*)`),
        cal = "DK_helligdage"
      )
    ) %>%
    mutate(
      ugedag_modtaget = unname(eng_to_dk[ugedag_modtaget]),
      ugedag_svaret   = unname(eng_to_dk[ugedag_svaret]),
      sektor = case_when(
        `Speciale (*)` == "Almen medicin" ~ "Almen praksis",
        is.na(`Hospital (*)`) ~ "Almen praksis",
        is.na(`Hospital (*)`) & is.na(`Speciale (*)`) ~ "Almen praksis",
        TRUE ~ "Hospital"
      ),
      specialeCorrected = case_when(
        `Speciale (*)` == "Almen medicin" ~ "Almen praksis",
        is.na(`Hospital (*)`) ~ "Almen praksis",
        is.na(`Hospital (*)`) & is.na(`Speciale (*)`) ~ "Almen praksis",
        !is.na(`Speciale (*)`) ~ `Speciale (*)`,
        TRUE ~ "Andre"
      ),
      AdjustedDate = as_date(AdjustedDate),
      FaerdigDate  = as_date(`Færdig (*)`),
      svar_kategori = case_when(
        `Svartype (*)` == "Medicingennemgang"       ~ "Medicingennemgang",
        `Svartype (*)` == "Ibrugtagningssag"        ~ "Ibrugtagningssag",
        `Svartype (*)` == "Kortsvar"                ~ "Kortsvar",
        `Svartype (*)` == "Generel forespørgsel"    ~ "Generel forespørgsel",
        `Svartype (*)` == "Almindeligt svar"        ~ "Almindeligt svar",
        `Svartype (*)` == "Bivirkningsindberetning" ~ "Bivirkningsindberetning",
        TRUE ~ NA_character_
      )
    )

  data
}

data.lmraad <- transform_data(data.lmraad)
data.medicingennemgang <- transform_data(data.medicingennemgang)
data.ibrugtagning <- transform_data(data.ibrugtagning)

data.ibrugtagning$`Svartype (*)` <- "Ibrugtagningssag"
data.ibrugtagning$`Region (*)` <- "Midtjylland"

data.lmraad_filtered <- rbind(data.lmraad, data.medicingennemgang, data.ibrugtagning)

today_cph <- today(tzone = "Europe/Copenhagen")

data.lmraad_filtered <- data.lmraad_filtered %>%
  mutate(
    age_adjusted = as.integer(today_cph - AdjustedDate)
  )

data.lmraad_filtered <- data.lmraad_filtered %>%
  select(
    any_of("Id"),
    everything(),
    -any_of("@odata.etag")
  ) %>%
  relocate(any_of("FileRef"), .after = last_col())

start_date <- min(data.lmraad_filtered$`Modtaget (*)`, na.rm = TRUE)
end_date   <- max(data.lmraad_filtered$`Modtaget (*)`, na.rm = TRUE)
start_date_string <- format(as.Date(start_date), "%d-%m-%Y")

message("✓ Data transformation completed")

# =============================================================================
# 9. AFFILIATION MERGING
# =============================================================================

message("\nMerging affiliation data...")

data.lmraad_filtered <- data.lmraad_filtered %>%
  left_join(
    KFE_KFA_affiliation.long %>%
      select(navn, afdeling) %>%
      rename(Forvagt_affiliation = afdeling),
    by = c("Forvagt*" = "navn")
  ) %>%
  left_join(
    KFE_KFA_affiliation.long %>%
      select(navn, afdeling) %>%
      rename(Bagvagt_affiliation = afdeling),
    by = c("Bagvagt*" = "navn")
  ) %>%
  mutate(
    KFE_KFA = case_when(
      !is.na(Forvagt_affiliation) ~ Forvagt_affiliation,
      !is.na(Bagvagt_affiliation) ~ Bagvagt_affiliation,
      TRUE ~ NA_character_
    )
  )

message("✓ Affiliation data merged successfully")

# =============================================================================
# 10. NUMBER.UNIFORMISE CERTAIN INPUTS
# =============================================================================

data.lmraad_filtered <- data.lmraad_filtered %>%
  mutate(
    specialeCorrected = case_when(
      specialeCorrected == "Rheumatologi" ~ "Reumatologi",
      TRUE ~ specialeCorrected
    )
  )

# =============================================================================
# 11. REORGANISE STATUS
# =============================================================================

message("\nReorganising status values...")

data.lmraad_filtered <- data.lmraad_filtered %>%
  mutate(
    Status = case_when(
      `Svartype (*)` == "Ibrugtagningssag" & !is.na(`Færdig (*)`) ~ "Sendt",
      `Svartype (*)` == "Ibrugtagningssag" & is.na(Status) & is.na(`Færdig (*)`) ~ "Modtaget",
      TRUE ~ Status
    )
  ) %>%
  mutate(
    Status = case_when(
      Status == "Sendt" & is.na(`Færdig (*)`) ~ "Sendt (dato mangler)",
      Status == "Modtaget" & !is.na(`Forvagt*`) ~ "Fordelt",
      TRUE ~ Status
    )
  )

message("✓ Status values reorganised successfully")

# =============================================================================
# 12. DATA SAVING AND FILE CLEANING
# =============================================================================

message("\nSaving prepared data...")

output_dir <- here("statistik", "Data", "Azure data")
ensure_directory(output_dir)

message("Output directory: ", output_dir)

today_date <- format(Sys.Date(), "%Y-%m-%d")
file_name <- paste0("azure_", today_date, ".rds")
file_path <- here(output_dir, file_name)

if (file.exists(file_path)) {
  file.remove(file_path)
  message("✓ Replaced existing file for today")
}

saveRDS(data.lmraad_filtered, file_path)
message("✓ Saved main data: ", file_name)

message("\nSaving Bivirkningsindberetning data...")

biv_dir <- here("statistik", "Data", "Azure data", "bivirkninger")
ensure_directory(biv_dir)

message("Bivirkninger directory: ", biv_dir)

biv_file_name <- paste0("azure.bivirkninger_", today_date, ".rds")
biv_file_path <- here(biv_dir, biv_file_name)

if (file.exists(biv_file_path)) {
  file.remove(biv_file_path)
  message("✓ Replaced existing bivirkninger file for today")
}

saveRDS(data.bivirkninger, biv_file_path)
message("✓ Saved bivirkninger data: ", biv_file_name)

message("\nCleaning up old main data files...")

cleanup_old_files <- function(directory, pattern = "azure_.*\\.rds", keep_count = 5) {
  files <- list.files(directory, pattern = pattern, full.names = TRUE)

  if (length(files) > keep_count) {
    file_info <- file.info(files)
    file_info$file <- files
    sorted_files <- file_info[order(file_info$mtime, decreasing = TRUE), ]
    files_to_keep <- sorted_files$file[1:keep_count]
    files_to_delete <- setdiff(sorted_files$file, files_to_keep)

    if (length(files_to_delete) > 0) {
      file.remove(files_to_delete)
      message("✓ Cleaned up ", length(files_to_delete), " old file(s)")
    }
  }
}

cleanup_old_files(output_dir)

message("\nCleaning up old bivirkninger files...")

cleanup_bivirkninger_files <- function(directory, pattern = "azure\\.bivirkninger_.*\\.rds", keep_count = 5) {
  files <- list.files(directory, pattern = pattern, full.names = TRUE)

  if (length(files) > keep_count) {
    file_info <- file.info(files)
    file_info$file <- files
    sorted_files <- file_info[order(file_info$mtime, decreasing = TRUE), ]
    files_to_keep <- sorted_files$file[1:keep_count]
    files_to_delete <- setdiff(sorted_files$file, files_to_keep)

    if (length(files_to_delete) > 0) {
      file.remove(files_to_delete)
      message("✓ Cleaned up ", length(files_to_delete), " old bivirkninger file(s)")
    }
  }
}

cleanup_bivirkninger_files(biv_dir)

current_files <- list.files(output_dir, pattern = "azure_.*\\.rds")
current_biv_files <- list.files(biv_dir, pattern = "azure\\.bivirkninger_.*\\.rds")

message("\nCurrent files in main directory (", length(current_files), "):")
for (file in current_files) {
  message("  - ", file)
}

message("\nCurrent files in bivirkninger directory (", length(current_biv_files), "):")
for (file in current_biv_files) {
  message("  - ", file)
}

# =============================================================================
# 13. COMPLETION SUMMARY
# =============================================================================

message("\n", rep("=", 50))
message("DATA PREPARATION COMPLETED SUCCESSFULLY!")
message(rep("=", 50))
message("Main dataset: ", nrow(data.lmraad_filtered), " rows")
message("Bivirkninger dataset: ", nrow(data.bivirkninger), " rows")
message("Creation date: ", creation_date_string)
message("Data range (main): ", start_date, " to ", end_date)
message(
  "Data range (bivirkninger): ",
  min(data.bivirkninger$`Modtaget (*)`, na.rm = TRUE), " to ",
  max(data.bivirkninger$`Modtaget (*)`, na.rm = TRUE)
)
message("Output locations:")
message("  - Main data: ", normalizePath(output_dir))
message("  - Bivirkninger data: ", normalizePath(biv_dir))
message("Files created:")
message("  - ", file_name, " (main dataset)")
message("  - ", biv_file_name, " (bivirkninger dataset)")
message(rep("=", 50))
