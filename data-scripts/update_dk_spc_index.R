# update_dk_spc_index.R
# Builds a local LMS/Danish SPC index from Produktresume.dk.
# v6.5: adds Produktresume.dk search URLs; rich_preview is resolved on demand by the Shiny app.
#
# Server cron:
#   Rscript /srv/shiny-server/kfaapps/data-scripts/update_dk_spc_index.R \
#     /srv/shiny-server/kfaapps/spc/data
#
# Local:
#   Rscript data-scripts/update_dk_spc_index.R spc/data
#
# Source in R/RStudio:
#   source("data-scripts/update_dk_spc_index.R")
#
# Output:
#   <output_dir>/dk_spc_documents.rds
#   <output_dir>/dk_spc_documents.csv
#
# Note:
#   This script does not scrape Produktresume.dk rich_preview links. Preview URLs
#   are resolved on demand in the Shiny app from each document's homepage_url,
#   so the nightly cron job remains light and predictable.

required_packages <- c("stringr")
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
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  stringr::str_squish(x)
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
  x <- stringr::str_replace_all(x, "%20", " ")
  x <- stringr::str_replace_all(x, "%2c", ",")
  x <- stringr::str_replace_all(x, "[®™]", " ")
  x <- stringr::str_replace_all(x, "[^a-z0-9]+", " ")
  stringr::str_squish(x)
}

html_entity_decode_basic <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  replacements <- c(
    "&nbsp;" = " ", "&amp;" = "&", "&quot;" = "\"", "&#39;" = "'",
    "&#039;" = "'", "&apos;" = "'", "&lt;" = "<", "&gt;" = ">",
    "&aelig;" = "æ", "&AElig;" = "Æ", "&oslash;" = "ø", "&Oslash;" = "Ø",
    "&aring;" = "å", "&Aring;" = "Å"
  )
  for (nm in names(replacements)) x <- stringr::str_replace_all(x, stringr::fixed(nm), replacements[[nm]])
  x
}

parse_us_directory_date <- function(x) {
  x <- clean_text(x)
  m <- stringr::str_match(x, "(\\d{1,2})/(\\d{1,2})/(\\d{4})")
  if (any(is.na(m))) return("")
  sprintf("%04d-%02d-%02d", as.integer(m[4]), as.integer(m[1]), as.integer(m[2]))
}

parse_last_us_directory_date <- function(x) {
  x <- clean_text(x)
  m <- gregexpr("\\d{1,2}/\\d{1,2}/\\d{4}", x, perl = TRUE)[[1]]
  if (length(m) == 1 && identical(m[[1]], -1L)) return("")
  starts <- as.integer(m)
  lens <- attr(m, "match.length")
  vals <- substring(x, starts, starts + lens - 1L)
  parse_us_directory_date(tail(vals, 1L))
}

parse_directory_file_size <- function(x) {
  x <- clean_text(x)
  m <- stringr::str_match(x, "(\\d{1,2}/\\d{1,2}/\\d{4})\\s+\\d{1,2}:\\d{2}\\s+[AP]M\\s+(\\d+)\\s*$")
  if (any(is.na(m))) return("")
  m[3]
}

http_get_text <- function(url, timeout_sec = 45) {
  if (requireNamespace("httr2", quietly = TRUE)) {
    req <- httr2::request(url)
    req <- httr2::req_user_agent(req, "Mozilla/5.0 R regulatory-doc-viewer")
    req <- httr2::req_timeout(req, timeout_sec)
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (identical(status, 404L)) return(NULL)
    if (status >= 400L) stop("HTTP ", status, " for URL: ", url, call. = FALSE)
    return(httr2::resp_body_string(resp))
  }
  con <- NULL
  tryCatch({
    con <- base::url(url, open = "rb")
    rawToChar(readBin(con, what = "raw", n = 25 * 1024 * 1024))
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("404", msg, fixed = TRUE)) return(NULL)
    stop("Could not read URL: ", url, "\n", msg, call. = FALSE)
  }, finally = {
    if (!is.null(con)) close(con)
  })
}

encode_url_path_segments <- function(path) {
  path <- clean_text(html_entity_decode_basic(path))
  if (!nzchar(path)) return("")
  query <- ""
  if (grepl("[?#]", path)) {
    query <- sub("^[^?#]*", "", path)
    path <- sub("[?#].*$", "", path)
  }
  lead <- startsWith(path, "/")
  parts <- strsplit(path, "/", fixed = TRUE)[[1]]
  parts <- vapply(parts, function(part) {
    if (!nzchar(part)) return("")
    dec <- tryCatch(utils::URLdecode(part), error = function(e) part)
    utils::URLencode(dec, reserved = TRUE)
  }, character(1))
  out <- paste(parts, collapse = "/")
  if (lead && !startsWith(out, "/")) out <- paste0("/", out)
  paste0(out, query)
}

make_file_url <- function(folder_url, href) {
  href <- html_entity_decode_basic(clean_text(href))
  if (!nzchar(href)) return("")
  origin <- regmatches(folder_url, regexpr("^https?://[^/]+", folder_url, ignore.case = TRUE))
  if (!length(origin) || !nzchar(origin)) origin <- "https://spcweb.produktresume.dk"

  if (grepl("^https?://", href, ignore.case = TRUE)) {
    href_origin <- regmatches(href, regexpr("^https?://[^/]+", href, ignore.case = TRUE))
    href_path <- sub("^https?://[^/]+", "", href, ignore.case = TRUE)
    return(paste0(href_origin, encode_url_path_segments(href_path)))
  }
  if (startsWith(href, "/")) {
    return(paste0(origin, encode_url_path_segments(href)))
  }

  # Relative directory listing link. Encode only path segments, not the slash
  # structure. The old v6.2 implementation URL-encoded absolute /spcrepl hrefs
  # and appended them to folder_url, which produced 404 links such as
  # .../Human/S/%2Fspcrepl%2FHuman%2FS%2Ffile.docx.
  base_path <- sub("^https?://[^/]+", "", folder_url, ignore.case = TRUE)
  if (!grepl("/$", base_path)) base_path <- sub("/[^/]*$", "/", base_path)
  paste0(origin, encode_url_path_segments(paste0(base_path, href)))
}

build_url <- function(base_url, params = list()) {
  keep <- vapply(params, function(x) length(x) == 1 && !is.na(x) && nzchar(as.character(x)), logical(1))
  params <- params[keep]
  if (length(params) == 0) return(base_url)
  query <- paste(utils::URLencode(names(params), reserved = TRUE), utils::URLencode(as.character(params), reserved = TRUE), sep = "=", collapse = "&")
  paste0(base_url, "?", query)
}

make_lms_homepage_url <- function(query) {
  query <- clean_text(query)
  if (!nzchar(query)) return("")
  build_url("https://produktresume.dk/AppBuilder/search", list(q = query))
}

guess_medicine_name <- function(file_name) {
  x <- clean_text(file_name)
  x <- sub("\\.[A-Za-z0-9]{2,5}$", "", x)
  x <- stringr::str_replace_all(x, "(?i)\\b(produktresume|produktresumé|spc|summary of product characteristics)\\b", " ")
  x <- sub(",.*$", "", x)
  x <- stringr::str_replace_all(x, "\\s+", " ")
  clean_text(x)
}

extract_folder_index <- function(folder_url, folder_label) {
  html <- tryCatch(http_get_text(folder_url), error = function(e) {
    message("Could not read ", folder_url, ": ", conditionMessage(e))
    NULL
  })
  if (is.null(html) || !nzchar(html)) return(data.frame())

  pattern <- "(?is)<a\\b[^>]*href\\s*=\\s*[\"']([^\"']+\\.(?:doc|docx|pdf))(?:[?#][^\"']*)?[\"'][^>]*>(.*?)</a>"
  m <- gregexpr(pattern, html, perl = TRUE)[[1]]
  if (length(m) == 1 && identical(m[[1]], -1L)) return(data.frame())
  matches <- regmatches(html, list(m))[[1]]
  starts <- as.integer(m)

  rows <- lapply(seq_along(matches), function(i) {
    one <- matches[[i]]
    href <- sub("(?is).*href\\s*=\\s*[\"']([^\"']+)[\"'].*", "\\1", one, perl = TRUE)
    if (identical(href, one) || !nzchar(href)) return(NULL)
    if (!grepl("\\.(doc|docx|pdf)(?:[?#].*)?$", href, ignore.case = TRUE, perl = TRUE)) return(NULL)

    href_clean <- sub("[?#].*$", "", href)
    href_decoded <- tryCatch(utils::URLdecode(href_clean), error = function(e) href_clean)
    link_text <- sub("(?is)^.*?>", "", sub("(?is)</a>.*$", "", one, perl = TRUE), perl = TRUE)
    link_text <- html_entity_decode_basic(gsub("<[^>]+>", " ", link_text))
    link_text <- clean_text(link_text)

    file_name <- link_text
    if (!grepl("\\.(doc|docx|pdf)$", file_name, ignore.case = TRUE, perl = TRUE)) {
      file_name <- html_entity_decode_basic(tryCatch(utils::URLdecode(basename(href_clean)), error = function(e) basename(href_clean)))
      file_name <- clean_text(file_name)
    }
    if (!nzchar(file_name) || !grepl("\\.(doc|docx|pdf)$", file_name, ignore.case = TRUE, perl = TRUE)) return(NULL)

    context_start <- max(1L, starts[[i]] - 300L)
    context_html <- substr(html, context_start, starts[[i]] - 1L)
    context_text <- html_entity_decode_basic(gsub("<[^>]+>", " ", context_html))
    context_text <- clean_text(context_text)

    modified_date <- parse_last_us_directory_date(context_text)
    file_size <- parse_directory_file_size(context_text)
    ext <- toupper(tools::file_ext(file_name))
    med_guess <- guess_medicine_name(file_name)

    data.frame(
      source = "LMS",
      source_label = "Lægemiddelstyrelsen / Produktresume.dk",
      document_type = "Dansk produktresumé / SPC",
      file_name = file_name,
      file_url = make_file_url(folder_url, href_clean),
      folder = folder_label,
      modified_date = modified_date,
      file_size = file_size,
      format = ext,
      medicine_name_guess = med_guess,
      search_key = normalise_key(paste(file_name, med_guess, sep = " ")),
      homepage_url = make_lms_homepage_url(file_name),
      preview_url = "",
      source_file_updated_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      stringsAsFactors = FALSE
    )
  })

  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (length(rows) == 0) return(data.frame())
  out <- do.call(rbind, rows)
  out <- out[!duplicated(out$file_url), , drop = FALSE]
  row.names(out) <- NULL
  out
}

script_dir <- get_script_dir()
project_root <- if (basename(script_dir) %in% c("data-scripts", "scripts", "R")) dirname(script_dir) else normalizePath(getwd(), winslash = "/", mustWork = FALSE)
args <- commandArgs(trailingOnly = TRUE)
args_no_flags <- args[!grepl("^--", args)]

output_dir <- get_arg(args_no_flags, 1, Sys.getenv("DK_SPC_OUTPUT_DIR", unset = file.path(project_root, "spc", "data")))
output_dir <- normalizePath(output_dir, winslash = "/", mustWork = FALSE)
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

base_url <- Sys.getenv("DK_SPC_BASE_URL", unset = "https://spcweb.produktresume.dk/spcrepl/Human/")
if (!grepl("/$", base_url)) base_url <- paste0(base_url, "/")
folders <- c(LETTERS, "0", "Æ", "Ø", "Å")

message("Output directory: ", output_dir)
message("SPC base URL:     ", base_url)

out <- vector("list", length(folders))
for (i in seq_along(folders)) {
  folder <- folders[[i]]
  folder_url <- paste0(base_url, utils::URLencode(folder, reserved = TRUE), "/")
  message("Reading ", folder_url)
  out[[i]] <- extract_folder_index(folder_url, folder)
  Sys.sleep(0.10)
}

docs <- do.call(rbind, out)
if (is.null(docs) || nrow(docs) == 0) {
  stop("No Danish SPC documents were indexed. Check Produktresume.dk availability and base URL.", call. = FALSE)
}

docs <- docs[!duplicated(docs$file_url), , drop = FALSE]
docs <- docs[order(tolower(docs$file_name), docs$modified_date, method = "radix"), , drop = FALSE]
row.names(docs) <- NULL

out_rds <- file.path(output_dir, "dk_spc_documents.rds")
out_csv <- file.path(output_dir, "dk_spc_documents.csv")
saveRDS(docs, out_rds)
utils::write.csv(docs, out_csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")

message("Saved ", nrow(docs), " Danish SPC document rows to:")
message("  RDS: ", normalizePath(out_rds, winslash = "/", mustWork = FALSE))
message("  CSV: ", normalizePath(out_csv, winslash = "/", mustWork = FALSE))
message("Done.")
