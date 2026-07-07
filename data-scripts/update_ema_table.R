# update_ema_table.R
# Builds a local EMA medicines table for the source-first document finder.
#
# Server cron:
#   Rscript /srv/shiny-server/kfaapps/data-scripts/update_ema_table.R \
#     /srv/shiny-server/kfaapps/spc/data
#
# Local:
#   Rscript data-scripts/update_ema_table.R spc/data
#
# Source in R/RStudio:
#   source("data-scripts/update_ema_table.R")
#
# Output:
#   <output_dir>/ema_medicines.rds
#   <output_dir>/ema_medicines.csv
#   <output_dir>/raw/ema_medicines_YYYYMMDD.xlsx
#
# The app uses this table to find EMA medicine pages and then parses only EMA
# Product Information links on demand. Public assessment reports and medicine
# overview/summary documents are deliberately not part of the main workflow.

required_packages <- c("readxl", "stringr")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ", paste(missing_packages, collapse = ", "),
    ". Install with install.packages(c(", paste(sprintf('"%s"', missing_packages), collapse = ", "), ")).",
    call. = FALSE
  )
}

get_script_dir <- function() {
  cmd <- commandArgs(trailingOnly = FALSE)
  file_arg <- "--file="
  idx <- grep(paste0("^", file_arg), cmd)
  if (length(idx) > 0) {
    script_path <- sub(file_arg, "", cmd[idx[1]], fixed = TRUE)
    return(dirname(normalizePath(script_path, winslash = "/", mustWork = FALSE)))
  }
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    rstudio_path <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
    if (!is.null(rstudio_path) && length(rstudio_path) == 1 && nzchar(rstudio_path)) {
      return(dirname(normalizePath(rstudio_path, winslash = "/", mustWork = FALSE)))
    }
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

get_arg <- function(args, index, default = NULL) {
  if (length(args) >= index && !is.na(args[index]) && nzchar(args[index])) return(args[index])
  default
}

clean_text <- function(x) {
  if (inherits(x, "POSIXt")) x <- as.Date(x)
  if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d") else x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  x <- stringr::str_squish(x)
  x[x %in% c("NA", "N/A", "n/a", "Not applicable", "not applicable")] <- ""
  x
}

normalise_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "æ", "ae")
  x <- stringr::str_replace_all(x, "ø", "oe")
  x <- stringr::str_replace_all(x, "å", "aa")
  x2 <- suppressWarnings(iconv(x, from = "", to = "ASCII//TRANSLIT", sub = ""))
  x <- ifelse(is.na(x2), x, x2)
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  stringr::str_squish(x)
}

clean_header <- function(x) normalise_key(clean_text(x))

script_dir <- get_script_dir()
project_root <- if (basename(script_dir) %in% c("data-scripts", "scripts", "R")) dirname(script_dir) else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
args <- commandArgs(trailingOnly = TRUE)

output_dir <- get_arg(args, 1, Sys.getenv("EMA_OUTPUT_DIR", unset = file.path(project_root, "spc", "data")))
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
raw_dir <- file.path(output_dir, "raw")
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(raw_dir, showWarnings = FALSE, recursive = TRUE)

if (length(args) > 1) {
  message("Ignoring extra command-line argument(s): ", paste(args[-1], collapse = " | "), ". update_ema_table.R only accepts one optional argument: output_dir.")
}

message("Script directory: ", script_dir)
message("Project root:     ", project_root)
message("Output directory: ", output_dir)

DEFAULT_EMA_XLSX_URL <- "https://www.ema.europa.eu/en/documents/report/medicines-output-medicines-report_en.xlsx"
EMA_XLSX_URL <- Sys.getenv("EMA_MEDICINES_XLSX_URL", unset = DEFAULT_EMA_XLSX_URL)

is_xlsx_file <- function(path) {
  if (!file.exists(path) || is.na(file.info(path)$size) || file.info(path)$size < 4) return(FALSE)
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  sig <- readBin(con, what = "raw", n = 4)
  length(sig) >= 2 && all(sig[1:2] == charToRaw("PK"))
}

download_with_base_r <- function(url, destfile) {
  args_download <- list(url = url, destfile = destfile, mode = "wb", quiet = FALSE, method = "libcurl")
  if ("headers" %in% names(formals(utils::download.file))) {
    args_download$headers <- c("User-Agent" = "Mozilla/5.0 R regulatory-doc-viewer")
  }
  tryCatch(do.call(utils::download.file, args_download), error = function(e) {
    message("download.file(method = 'libcurl') failed; retrying with default method.")
    utils::download.file(url = url, destfile = destfile, mode = "wb", quiet = FALSE)
  })
}

download_with_httr2 <- function(url, destfile) {
  req <- httr2::request(url)
  req <- httr2::req_user_agent(req, "Mozilla/5.0 R regulatory-doc-viewer")
  req <- httr2::req_timeout(req, 120)
  resp <- httr2::req_perform(req)
  httr2::resp_check_status(resp)
  writeBin(httr2::resp_body_raw(resp), destfile)
}

download_ema_xlsx <- function(urls, destfile) {
  urls <- unique(urls[nzchar(urls)])
  last_error <- NULL
  for (url in urls) {
    tmp <- paste0(destfile, ".tmp")
    if (file.exists(tmp)) unlink(tmp)
    message("Downloading EMA workbook: ", url)
    ok <- tryCatch({
      if (requireNamespace("httr2", quietly = TRUE)) download_with_httr2(url, tmp) else download_with_base_r(url, tmp)
      TRUE
    }, error = function(e) {
      last_error <<- conditionMessage(e)
      message("Download failed: ", last_error)
      FALSE
    })
    if (ok && is_xlsx_file(tmp)) {
      if (file.exists(destfile)) unlink(destfile)
      file.rename(tmp, destfile)
      return(url)
    }
    if (ok && file.exists(tmp)) {
      preview <- tryCatch(paste(readLines(tmp, n = 5, warn = FALSE), collapse = "\n"), error = function(e) "[Could not read invalid download preview]")
      message("Downloaded file was not a valid XLSX file. Preview:\n", preview)
    }
    if (file.exists(tmp)) unlink(tmp)
  }
  stop("Could not download a valid XLSX file from EMA. Last error: ", ifelse(is.null(last_error), "none", last_error), call. = FALSE)
}

row_to_text <- function(df, i) {
  vals <- unlist(df[i, ], use.names = FALSE)
  vals <- as.character(vals)
  vals[is.na(vals)] <- ""
  clean_header(paste(vals, collapse = " "))
}

score_header_row <- function(row_text) {
  patterns <- c(
    "category", "name of medicine", "ema product number", "medicine status",
    "international non proprietary name", "active substance", "atc code", "medicine url"
  )
  sum(vapply(patterns, function(p) grepl(p, row_text, fixed = TRUE), logical(1)))
}

find_header_row <- function(path, sheet = 1, n_max = 80) {
  preview <- readxl::read_excel(path, sheet = sheet, col_names = FALSE, n_max = n_max, .name_repair = "minimal")
  if (nrow(preview) == 0) stop("The workbook appears to be empty.", call. = FALSE)
  row_texts <- vapply(seq_len(nrow(preview)), function(i) row_to_text(preview, i), character(1))
  scores <- vapply(row_texts, score_header_row, integer(1))
  header_row <- which.max(scores)
  if (length(header_row) == 0 || is.na(header_row) || max(scores, na.rm = TRUE) < 3) {
    stop("Could not identify the EMA header row.", call. = FALSE)
  }
  header_row
}

find_col <- function(df, patterns) {
  nm <- clean_header(names(df))
  hits <- vapply(nm, function(one_name) any(vapply(patterns, function(p) grepl(p, one_name, ignore.case = TRUE, perl = TRUE), logical(1))), logical(1))
  idx <- which(hits)
  if (length(idx) == 0) return(NA_character_)
  names(df)[idx[1]]
}

get_col <- function(df, col_name) {
  if (is.na(col_name) || !nzchar(col_name) || !(col_name %in% names(df))) return(rep("", nrow(df)))
  clean_text(df[[col_name]])
}

ema_slug_from_url <- function(url) {
  url <- clean_text(url)
  m <- regmatches(url, regexpr("/EPAR/[^/?#]+", url, ignore.case = TRUE))
  if (!length(m) || !nzchar(m)) return("")
  sub(".*/EPAR/", "", m)
}

raw_xlsx <- file.path(raw_dir, sprintf("ema_medicines_%s.xlsx", format(Sys.Date(), "%Y%m%d")))
used_source <- download_ema_xlsx(unique(c(EMA_XLSX_URL, DEFAULT_EMA_XLSX_URL)), raw_xlsx)

sheets <- readxl::excel_sheets(raw_xlsx)
if (length(sheets) == 0) stop("No sheets found in EMA workbook.", call. = FALSE)
sheet <- sheets[[1]]
message("Reading sheet: ", sheet)
header_row <- find_header_row(raw_xlsx, sheet = sheet)
message("Detected header row: ", header_row)

raw <- readxl::read_excel(raw_xlsx, sheet = sheet, skip = header_row - 1, .name_repair = "minimal")
names(raw) <- stringr::str_squish(stringr::str_replace_all(as.character(names(raw)), "[\r\n\t]+", " "))

colmap <- list(
  category = find_col(raw, c("^category$")),
  medicine_name = find_col(raw, c("^name of medicine$", "^medicine name$")),
  ema_product_number = find_col(raw, c("ema product number")),
  medicine_status = find_col(raw, c("^medicine status$", "authorisation status", "authorization status")),
  inn_common_name = find_col(raw, c("international non.proprietary name", "international non proprietary name", "\\binn\\b", "common name")),
  active_substance = find_col(raw, c("^active substance$", "active substance")),
  atc_code_human = find_col(raw, c("atc code.*human", "^atc code$", "atc code")),
  therapeutic_indication = find_col(raw, c("therapeutic indication")),
  marketing_authorisation_holder = find_col(raw, c("marketing authorisation developer", "marketing authorization developer", "marketing authorisation holder", "marketing authorization holder", "applicant", "holder")),
  marketing_authorisation_date = find_col(raw, c("marketing authorisation date", "marketing authorization date")),
  first_published_date = find_col(raw, c("first published date")),
  last_updated_date = find_col(raw, c("last updated date")),
  medicine_url = find_col(raw, c("^medicine url$", "medicine url", "url"))
)

message("Column mapping:")
for (nm in names(colmap)) message("  ", nm, " <- ", ifelse(is.na(colmap[[nm]]), "[missing]", colmap[[nm]]))

ema <- data.frame(
  category = get_col(raw, colmap$category),
  medicine_name = get_col(raw, colmap$medicine_name),
  ema_product_number = get_col(raw, colmap$ema_product_number),
  medicine_status = get_col(raw, colmap$medicine_status),
  inn_common_name = get_col(raw, colmap$inn_common_name),
  active_substance = get_col(raw, colmap$active_substance),
  atc_code_human = get_col(raw, colmap$atc_code_human),
  therapeutic_indication = get_col(raw, colmap$therapeutic_indication),
  marketing_authorisation_holder = get_col(raw, colmap$marketing_authorisation_holder),
  marketing_authorisation_date = get_col(raw, colmap$marketing_authorisation_date),
  first_published_date = get_col(raw, colmap$first_published_date),
  last_updated_date = get_col(raw, colmap$last_updated_date),
  medicine_url = get_col(raw, colmap$medicine_url),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

ema <- ema[nzchar(ema$medicine_name), , drop = FALSE]
if (nrow(ema) == 0) stop("No medicine rows were extracted from the EMA workbook.", call. = FALSE)

ema$ema_slug <- vapply(ema$medicine_url, ema_slug_from_url, character(1))
ema$source <- "EMA"
ema$source_label <- "European Medicines Agency"
ema$source_file_type <- "ema_download"
ema$source_file_url <- used_source
ema$source_file_downloaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")
ema$source_file_local_path <- normalizePath(raw_xlsx, winslash = "/", mustWork = FALSE)
ema$search_key <- normalise_key(paste(ema$medicine_name, ema$active_substance, ema$inn_common_name, ema$atc_code_human, ema$marketing_authorisation_holder, sep = " "))

# Keep first occurrence of exact duplicate core rows.
dedup_key <- paste(ema$medicine_name, ema$ema_product_number, ema$medicine_url, sep = "\r")
ema <- ema[!duplicated(dedup_key), , drop = FALSE]
ema <- ema[order(tolower(ema$medicine_name), ema$ema_product_number, na.last = TRUE, method = "radix"), , drop = FALSE]
row.names(ema) <- NULL

out_rds <- file.path(output_dir, "ema_medicines.rds")
out_csv <- file.path(output_dir, "ema_medicines.csv")
saveRDS(ema, out_rds)
utils::write.csv(ema, out_csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")

message("Saved ", nrow(ema), " EMA medicine rows to:")
message("  RDS: ", normalizePath(out_rds, winslash = "/", mustWork = FALSE))
message("  CSV: ", normalizePath(out_csv, winslash = "/", mustWork = FALSE))
message("  Raw workbook copy: ", normalizePath(raw_xlsx, winslash = "/", mustWork = FALSE))
message("Done.")
