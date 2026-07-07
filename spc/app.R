# app.R
# Regulatory document viewer — v6.7
#
# Source-first architecture:
#   1) User searches an ingredient or product name.
#   2) The app searches source indexes/API directly:
#        - LMS / Danish SPC index from Produktresume.dk
#        - EMA medicines table + on-demand Product Information link parsing
#        - DailyMed API / FDA SPL labels
#   3) Results are stored in one documents_found table.
#   4) User selects exactly one document and chooses: preview/view, download, RIS, website.
#
# No global resolver index is built. Query expansion is small, transparent and
# opportunistic: original query, simple DK/EN spelling variants, salt-stripped
# variants, manual aliases and active substances inferred from DK/EMA source rows.

# -----------------------------------------------------------------------------
# 1. Package checks
# -----------------------------------------------------------------------------

required_packages <- c("shiny", "stringr", "jsonlite")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop(
    "Missing required R package(s): ", paste(missing_packages, collapse = ", "),
    ". Install with install.packages(c(",
    paste(sprintf('"%s"', missing_packages), collapse = ", "),
    ")).",
    call. = FALSE
  )
}

library(shiny)
library(stringr)
library(jsonlite)

`%||%` <- function(x, y) if (is.null(x)) y else x

# -----------------------------------------------------------------------------
# 2. Generic helpers
# -----------------------------------------------------------------------------

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "[\r\n\t]+", " ")
  stringr::str_squish(x)
}

first_nonempty <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- clean_text(vals)
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) "" else vals[[1]]
}

display_value <- function(x) {
  x <- clean_text(x)
  if (length(x) == 0 || !nzchar(x[[1]])) "—" else x[[1]]
}

collapse_unique_values <- function(x, max_values = 25, sep = "; ") {
  vals <- unique(clean_text(x))
  vals <- vals[nzchar(vals)]
  if (length(vals) == 0) return("")
  shown <- utils::head(vals, max_values)
  out <- paste(shown, collapse = sep)
  if (length(vals) > max_values) out <- paste0(out, sep, "+", length(vals) - max_values, " flere")
  out
}

safe_order_key <- function(x) stringr::str_to_lower(clean_text(x))

normalise_key <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_to_lower(x)
  x <- stringr::str_replace_all(x, "æ", "ae")
  x <- stringr::str_replace_all(x, "ø", "oe")
  x <- stringr::str_replace_all(x, "å", "aa")
  x <- stringr::str_replace_all(x, "ß", "ss")
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
    "&ndash;" = "-", "&mdash;" = "-", "&aelig;" = "æ", "&AElig;" = "Æ",
    "&oslash;" = "ø", "&Oslash;" = "Ø", "&aring;" = "å", "&Aring;" = "Å"
  )
  for (nm in names(replacements)) x <- stringr::str_replace_all(x, stringr::fixed(nm), replacements[[nm]])
  x
}

strip_html <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- stringr::str_replace_all(x, "(?is)<script[^>]*>.*?</script>", " ")
  x <- stringr::str_replace_all(x, "(?is)<style[^>]*>.*?</style>", " ")
  x <- stringr::str_replace_all(x, "(?i)<br\\s*/?>", "\n")
  x <- stringr::str_replace_all(x, "(?i)</(p|div|li|tr|h1|h2|h3|h4|section|article)>", "\n")
  x <- stringr::str_replace_all(x, "(?is)<[^>]+>", " ")
  x <- html_entity_decode_basic(x)
  stringr::str_squish(x)
}

make_absolute_url <- function(href, base_url) {
  href <- clean_text(html_entity_decode_basic(href))
  if (!nzchar(href)) return("")
  if (grepl("^https?://", href, ignore.case = TRUE)) return(href)
  if (startsWith(href, "//")) return(paste0("https:", href))
  base_url <- clean_text(base_url)
  if (!nzchar(base_url)) base_url <- "https://www.ema.europa.eu"
  if (startsWith(href, "/")) {
    origin <- regmatches(base_url, regexpr("^https?://[^/]+", base_url, ignore.case = TRUE))
    if (!length(origin) || !nzchar(origin)) origin <- "https://www.ema.europa.eu"
    return(paste0(origin, href))
  }
  paste0(sub("/[^/]*$", "/", base_url), href)
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

make_absolute_file_url <- function(href, base_url) {
  href <- clean_text(html_entity_decode_basic(href))
  if (!nzchar(href)) return("")
  if (grepl("^https?://", href, ignore.case = TRUE)) {
    m <- regexpr("^https?://[^/]+", href, ignore.case = TRUE)
    origin <- regmatches(href, m)
    path <- sub("^https?://[^/]+", "", href, ignore.case = TRUE)
    return(paste0(origin, encode_url_path_segments(path)))
  }
  if (startsWith(href, "//")) return(paste0("https:", encode_url_path_segments(sub("^//[^/]+", "", href))))
  base_url <- clean_text(base_url)
  if (!nzchar(base_url)) base_url <- "https://www.ema.europa.eu"
  origin <- regmatches(base_url, regexpr("^https?://[^/]+", base_url, ignore.case = TRUE))
  if (!length(origin) || !nzchar(origin)) origin <- "https://www.ema.europa.eu"
  if (startsWith(href, "/")) return(paste0(origin, encode_url_path_segments(href)))
  base_path <- sub("^https?://[^/]+", "", base_url, ignore.case = TRUE)
  if (!grepl("/$", base_path)) base_path <- sub("/[^/]*$", "/", base_path)
  paste0(origin, encode_url_path_segments(paste0(base_path, href)))
}

make_lms_spc_url <- function(folder, file_name) {
  folder <- clean_text(folder)
  file_name <- clean_text(file_name)
  if (!nzchar(folder) || !nzchar(file_name)) return("")
  path <- paste0("/spcrepl/Human/", folder, "/", file_name)
  paste0("https://spcweb.produktresume.dk", encode_url_path_segments(path))
}

repair_lms_spc_url <- function(file_url, folder = "", file_name = "") {
  file_url <- clean_text(file_url)
  rebuilt <- mapply(make_lms_spc_url, folder, file_name, USE.NAMES = FALSE)
  # Prefer a deterministic URL built from folder + filename. This also repairs
  # older bad rows like .../S/%2Fspcrepl%2FHuman%2FS%2Ffile.docx.
  use_rebuilt <- nzchar(rebuilt)
  out <- file_url
  out[use_rebuilt] <- rebuilt[use_rebuilt]
  bad_double_path <- grepl("%2fspcrepl%2fhuman%2f|%252fspcrepl%252fhuman%252f", tolower(out), perl = TRUE)
  out[bad_double_path & use_rebuilt] <- rebuilt[bad_double_path & use_rebuilt]
  out
}

make_lms_homepage_url <- function(query) {
  query <- clean_text(query)
  if (!nzchar(query)) return("")
  build_url("https://produktresume.dk/AppBuilder/search", list(q = query))
}

make_lms_rich_preview_absolute <- function(href) {
  href <- clean_text(html_entity_decode_basic(href))
  if (!nzchar(href)) return("")
  if (grepl("^https?://", href, ignore.case = TRUE)) return(href)
  if (startsWith(href, "//")) return(paste0("https:", href))
  if (startsWith(href, "/")) return(paste0("https://produktresume.dk", href))
  href <- sub("^/", "", href)
  paste0("https://produktresume.dk/AppBuilder/", href)
}

extract_html_attr <- function(tag, attr) {
  tag <- clean_text(tag)
  attr <- clean_text(attr)
  if (!nzchar(tag) || !nzchar(attr)) return("")
  pat <- paste0("(?is)\\b", attr, "\\s*=\\s*([\"'])(.*?)\\1")
  m <- regexpr(pat, tag, perl = TRUE)
  if (identical(as.integer(m), -1L)) return("")
  html_entity_decode_basic(sub(pat, "\\2", regmatches(tag, m), perl = TRUE))
}

extract_first_lms_attr <- function(html, attr) {
  html <- as.character(html)
  if (length(html) == 0 || is.na(html[[1]]) || !nzchar(html[[1]])) return("")
  attr <- clean_text(attr)
  if (!nzchar(attr)) return("")
  pat <- paste0("(?is)\\b", attr, "\\s*=\\s*([\"'])(.*?)\\1")
  m <- regexpr(pat, html[[1]], perl = TRUE)
  if (identical(as.integer(m), -1L)) return("")
  html_entity_decode_basic(sub(pat, "\\2", regmatches(html[[1]], m), perl = TRUE))
}

extract_lms_rich_preview_url <- function(html) {
  html <- as.character(html)
  if (length(html) == 0 || is.na(html[[1]]) || !nzchar(html[[1]])) return("")
  html <- paste(html, collapse = "\n")
  html_decoded <- html_entity_decode_basic(html)

  # 1) Produktresume.dk renders the exact link as <a class="preview_link" ...
  # href="/AppBuilder/rich_preview?...">. Prefer this explicit anchor when
  # present, but do not depend on attribute order.
  anchor_matches <- gregexpr("(?is)<a\\b[^>]*>", html_decoded, perl = TRUE)
  anchors <- regmatches(html_decoded, anchor_matches)[[1]]
  if (length(anchors) > 0 && !identical(anchors[[1]], "")) {
    preview_anchors <- anchors[grepl("preview_link|rich_preview\\?", anchors, ignore.case = TRUE, perl = TRUE)]
    for (tag in preview_anchors) {
      href <- extract_html_attr(tag, "href")
      if (nzchar(href) && grepl("rich_preview\\?", href, ignore.case = TRUE, perl = TRUE)) {
        return(make_lms_rich_preview_absolute(href))
      }
    }
  }

  # 2) Fallback: any href containing rich_preview, including escaped HTML from
  # snippets or server-rendered search results.
  pat_href <- "(?is)href\\s*=\\s*([\"'])([^\"']*rich_preview\\?[^\"']*)\\1"
  m <- regexpr(pat_href, html_decoded, perl = TRUE)
  if (!identical(as.integer(m), -1L)) {
    href <- sub(pat_href, "\2", regmatches(html_decoded, m), perl = TRUE)
    return(make_lms_rich_preview_absolute(href))
  }

  # 3) Fallback: construct the rich_preview URL from attributes on the result
  # div. This handles blocks like:
  # <div class="entity-link productresume" data-query="..." preview="true"
  #      data-entity-id="..." data-entity-type="productresume">.
  entity_id <- extract_first_lms_attr(html_decoded, "data-entity-id")
  entity_type <- extract_first_lms_attr(html_decoded, "data-entity-type")
  data_query <- extract_first_lms_attr(html_decoded, "data-query")
  if (nzchar(entity_id) && nzchar(data_query)) {
    if (!nzchar(entity_type)) entity_type <- "productresume"
    return(build_url(
      "https://produktresume.dk/AppBuilder/rich_preview",
      list(id = entity_id, query = data_query, type = entity_type)
    ))
  }

  ""
}

lms_preview_cache <- new.env(parent = emptyenv())

lookup_lms_rich_preview_url_from_homepage <- function(homepage_url, timeout_sec = 20) {
  homepage_url <- clean_text(homepage_url)
  if (!nzchar(homepage_url)) return("")
  key <- paste0("url::", normalise_key(homepage_url))
  if (exists(key, envir = lms_preview_cache, inherits = FALSE)) {
    return(get(key, envir = lms_preview_cache, inherits = FALSE))
  }
  out <- tryCatch({
    html <- http_get_text(homepage_url, timeout_sec = timeout_sec)
    # The LMS homepage URL is already specific to the selected product resume.
    # Therefore the first preview_link / rich_preview link on that page is the
    # correct preview for this document.
    extract_lms_rich_preview_url(html)
  }, error = function(e) "")
  assign(key, out, envir = lms_preview_cache)
  out
}

lookup_lms_rich_preview_url <- function(query, timeout_sec = 20) {
  query <- clean_text(query)
  if (!nzchar(query)) return("")
  lookup_lms_rich_preview_url_from_homepage(make_lms_homepage_url(query), timeout_sec = timeout_sec)
}

safe_filename <- function(x, fallback = "document") {
  x <- clean_text(x)
  if (!nzchar(x)) x <- fallback
  x <- stringr::str_replace_all(x, "[^A-Za-z0-9æøåÆØÅ._-]+", "_")
  x <- stringr::str_replace_all(x, "_+", "_")
  x <- stringr::str_replace_all(x, "^_+|_+$", "")
  x <- stringr::str_sub(x, 1, 140)
  if (!nzchar(x)) fallback else x
}

url_ext <- function(url) {
  ext <- tolower(tools::file_ext(sub("[?#].*$", "", clean_text(url))))
  ok <- nzchar(ext) & nchar(ext) <= 5
  ifelse(ok, ext, "")
}

build_url <- function(base_url, params = list()) {
  keep <- vapply(params, function(x) length(x) == 1 && !is.na(x) && nzchar(as.character(x)), logical(1))
  params <- params[keep]
  if (length(params) == 0) return(base_url)
  query <- paste(utils::URLencode(names(params), reserved = TRUE), utils::URLencode(as.character(params), reserved = TRUE), sep = "=", collapse = "&")
  paste0(base_url, "?", query)
}

http_get_text <- function(url, timeout_sec = 30) {
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
    rawToChar(readBin(con, what = "raw", n = 20 * 1024 * 1024))
  }, error = function(e) {
    msg <- conditionMessage(e)
    if (grepl("404", msg, fixed = TRUE)) return(NULL)
    stop("Could not read URL: ", url, "\n", msg, call. = FALSE)
  }, finally = {
    if (!is.null(con)) close(con)
  })
}

http_get_binary <- function(url, timeout_sec = 75, max_bytes = 120 * 1024 * 1024) {
  if (requireNamespace("httr2", quietly = TRUE)) {
    req <- httr2::request(url)
    req <- httr2::req_user_agent(req, "Mozilla/5.0 R regulatory-doc-viewer")
    req <- httr2::req_timeout(req, timeout_sec)
    resp <- httr2::req_perform(req)
    status <- httr2::resp_status(resp)
    if (status >= 400L) stop("HTTP ", status, " while downloading: ", url, call. = FALSE)
    return(httr2::resp_body_raw(resp))
  }

  con <- NULL
  tryCatch({
    con <- base::url(url, open = "rb")
    readBin(con, what = "raw", n = max_bytes)
  }, error = function(e) {
    stop("Could not download URL: ", url, "\n", conditionMessage(e), call. = FALSE)
  }, finally = {
    if (!is.null(con)) close(con)
  })
}

# -----------------------------------------------------------------------------
# 3. Data loading
# -----------------------------------------------------------------------------

find_data_dir <- function() {
  env_dir <- Sys.getenv("KFA_SPC_DATA_DIR", unset = "")
  candidate_dirs <- unique(c(
    env_dir,
    file.path(getwd(), "data"),
    file.path(getwd(), "spc", "data"),
    file.path(dirname(getwd()), "spc", "data"),
    "/srv/shiny-server/kfaapps/spc/data"
  ))
  candidate_dirs <- candidate_dirs[nzchar(candidate_dirs)]
  candidate_dirs <- normalizePath(candidate_dirs, winslash = "/", mustWork = FALSE)

  has_data <- vapply(candidate_dirs, function(d) {
    file.exists(file.path(d, "ListeOverGodkendteLaegemidler.csv")) ||
      file.exists(file.path(d, "dk_spc_documents.rds")) ||
      file.exists(file.path(d, "ema_medicines.rds"))
  }, logical(1))

  if (any(has_data)) return(candidate_dirs[which(has_data)[[1]]])
  candidate_dirs[[1]]
}

read_delimited_best_effort <- function(path) {
  encodings <- c("UTF-8-BOM", "UTF-8", "latin1", "Windows-1252", "")
  separators <- c(";", "\t", ",")
  best <- NULL
  best_score <- -Inf
  errors <- character(0)

  for (enc in encodings) {
    for (sep in separators) {
      df <- tryCatch({
        args <- list(
          file = path, sep = sep, dec = if (identical(sep, ";")) "," else ".",
          stringsAsFactors = FALSE, check.names = FALSE,
          na.strings = c("", "NA", "N/A"), quote = "\"", comment.char = ""
        )
        if (nzchar(enc)) args$fileEncoding <- enc
        do.call(utils::read.csv, args)
      }, error = function(e) {
        errors <<- c(errors, paste0("enc=", enc, "; sep=", sep, "; error=", conditionMessage(e)))
        NULL
      })

      if (!is.null(df) && is.data.frame(df)) {
        score <- ncol(df) + min(nrow(df), 1000) / 1000 + sum(nzchar(clean_text(names(df)))) / max(1, length(names(df)))
        if (score > best_score) {
          best <- df
          best_score <- score
        }
      }
    }
  }

  if (is.null(best) || ncol(best) < 2) {
    stop("Could not read CSV file: ", path, "\n", paste(utils::head(errors, 8), collapse = "\n"), call. = FALSE)
  }
  names(best) <- clean_text(names(best))
  best
}

find_col_alias <- function(df, exact = character(0), contains = character(0)) {
  nm <- names(df)
  nm_key <- normalise_key(nm)
  exact_key <- normalise_key(exact)
  idx <- which(nm_key %in% exact_key)
  if (length(idx) > 0) return(nm[[idx[[1]]]])
  contains_key <- normalise_key(contains)
  for (needle in contains_key[nzchar(contains_key)]) {
    idx <- which(grepl(needle, nm_key, fixed = TRUE))
    if (length(idx) > 0) return(nm[[idx[[1]]]])
  }
  NA_character_
}

get_col <- function(df, col_name) {
  if (length(col_name) != 1 || is.na(col_name) || !nzchar(col_name) || !(col_name %in% names(df))) {
    return(rep("", nrow(df)))
  }
  clean_text(df[[col_name]])
}

empty_dk_medicines <- function() {
  data.frame(
    medicine_name = character(0), active_substance = character(0), atc = character(0),
    form = character(0), strength = character(0), holder = character(0), procedure = character(0),
    product_id = character(0), search_key = character(0), stringsAsFactors = FALSE
  )
}

load_dk_medicines <- function(data_dir) {
  path <- file.path(data_dir, "ListeOverGodkendteLaegemidler.csv")
  if (!file.exists(path)) {
    warning("Danish approved medicines CSV not found: ", path, call. = FALSE)
    return(empty_dk_medicines())
  }
  raw <- read_delimited_best_effort(path)
  colmap <- list(
    drug_id = find_col_alias(raw, c("DrugId", "Drug ID", "Laegemiddelidentifikation")),
    medicine_name = find_col_alias(raw, c("Lægemiddel", "Laegemiddel", "Lægemiddelnavn", "Laegemiddelnavn", "Navn")),
    form = find_col_alias(raw, c("LægemiddelForm", "Lægemiddelform", "LaegemiddelForm", "Laegemiddelform", "Form")),
    strength = find_col_alias(raw, c("Styrke")),
    active_substance = find_col_alias(raw, c("AktiveSubstanser", "Aktive substanser", "Aktivt stof", "Aktiv substans", "Virksomme stoffer")),
    atc = find_col_alias(raw, c("ATC", "ATC-kode", "ATC kode", "ATCCode"), contains = c("ATC")),
    procedure = find_col_alias(raw, c("Godkendt procedure", "Godkendelsesprocedure", "Procedure"), contains = c("procedure")),
    holder = find_col_alias(raw, c("Indehaver", "Virksomhed", "Firma", "MAH", "Marketing authorisation holder"), contains = c("indehaver", "holder"))
  )

  dk <- data.frame(
    medicine_name = get_col(raw, colmap$medicine_name),
    active_substance = get_col(raw, colmap$active_substance),
    atc = get_col(raw, colmap$atc),
    form = get_col(raw, colmap$form),
    strength = get_col(raw, colmap$strength),
    holder = get_col(raw, colmap$holder),
    procedure = get_col(raw, colmap$procedure),
    product_id = get_col(raw, colmap$drug_id),
    stringsAsFactors = FALSE
  )
  dk <- dk[nzchar(dk$medicine_name) | nzchar(dk$active_substance), , drop = FALSE]
  dk$search_key <- normalise_key(paste(dk$medicine_name, dk$active_substance, dk$atc, dk$holder, dk$form, sep = " "))
  row.names(dk) <- NULL
  dk
}

empty_ema_medicines <- function() {
  data.frame(
    medicine_name = character(0), ema_product_number = character(0), medicine_status = character(0),
    inn_common_name = character(0), active_substance = character(0), atc_code_human = character(0),
    therapeutic_indication = character(0), marketing_authorisation_holder = character(0),
    marketing_authorisation_date = character(0), first_published_date = character(0),
    last_updated_date = character(0), medicine_url = character(0), search_key = character(0),
    stringsAsFactors = FALSE
  )
}

load_ema_medicines <- function(data_dir) {
  rds_path <- file.path(data_dir, "ema_medicines.rds")
  csv_path <- file.path(data_dir, "ema_medicines.csv")
  raw <- NULL

  if (file.exists(rds_path)) {
    raw <- tryCatch(readRDS(rds_path), error = function(e) {
      warning("Could not read EMA medicines RDS: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  }

  if ((is.null(raw) || !is.data.frame(raw)) && file.exists(csv_path)) {
    raw <- tryCatch(read_delimited_best_effort(csv_path), error = function(e) {
      warning("Could not read EMA medicines CSV: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  }

  if (is.null(raw) || !is.data.frame(raw)) {
    warning("EMA medicines index not found/readable in: ", data_dir, call. = FALSE)
    return(empty_ema_medicines())
  }

  expected <- names(empty_ema_medicines())
  for (col in setdiff(expected, names(raw))) raw[[col]] <- ""
  out <- raw[, expected, drop = FALSE]
  for (col in names(out)) out[[col]] <- clean_text(out[[col]])
  if (!"search_key" %in% names(raw) || all(!nzchar(out$search_key))) {
    out$search_key <- normalise_key(paste(out$medicine_name, out$active_substance, out$inn_common_name, out$atc_code_human, out$marketing_authorisation_holder, sep = " "))
  }
  out <- out[nzchar(out$medicine_name) | nzchar(out$active_substance) | nzchar(out$inn_common_name), , drop = FALSE]
  row.names(out) <- NULL
  out
}

empty_dk_spc_index <- function() {
  data.frame(
    file_name = character(0), file_url = character(0), folder = character(0), modified_date = character(0),
    file_size = character(0), format = character(0), medicine_name_guess = character(0), search_key = character(0),
    homepage_url = character(0), preview_url = character(0), source_file_updated_at = character(0), stringsAsFactors = FALSE
  )
}

load_dk_spc_documents <- function(data_dir) {
  rds_path <- file.path(data_dir, "dk_spc_documents.rds")
  csv_path <- file.path(data_dir, "dk_spc_documents.csv")
  raw <- NULL

  if (file.exists(rds_path)) {
    raw <- tryCatch(readRDS(rds_path), error = function(e) {
      warning("Could not read Danish SPC index RDS: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  }

  if ((is.null(raw) || !is.data.frame(raw)) && file.exists(csv_path)) {
    raw <- tryCatch(read_delimited_best_effort(csv_path), error = function(e) {
      warning("Could not read Danish SPC index CSV: ", conditionMessage(e), call. = FALSE)
      NULL
    })
  }

  if (is.null(raw) || !is.data.frame(raw)) {
    warning("Danish SPC index not found/readable in: ", data_dir, call. = FALSE)
    return(empty_dk_spc_index())
  }
  expected <- names(empty_dk_spc_index())
  for (col in setdiff(expected, names(raw))) raw[[col]] <- ""
  out <- raw[, expected, drop = FALSE]
  for (col in names(out)) out[[col]] <- clean_text(out[[col]])
  out$format <- toupper(out$format)
  out$file_url <- repair_lms_spc_url(out$file_url, out$folder, out$file_name)
  missing_homepage <- !nzchar(out$homepage_url)
  out$homepage_url[missing_homepage] <- vapply(out$file_name[missing_homepage], make_lms_homepage_url, character(1))
  if (!"search_key" %in% names(raw) || all(!nzchar(out$search_key))) {
    out$search_key <- normalise_key(paste(out$file_name, out$medicine_name_guess, sep = " "))
  }
  out <- out[nzchar(out$file_name) & nzchar(out$file_url), , drop = FALSE]
  row.names(out) <- NULL
  out
}

# -----------------------------------------------------------------------------
# 4. Lightweight query expansion
# -----------------------------------------------------------------------------

manual_aliases <- function() {
  data.frame(
    alias = c(
      "zoloft", "sertraline", "sertraline hydrochloride", "sertralinhydrochlorid",
      "cymbalta", "duloxetine", "duloxetine hydrochloride", "duloxetinhydrochlorid",
      "diamox", "acetazolamide", "spiolto", "spiolto respimat", "tiotropium", "olodaterol",
      "keytruda", "pembrolizumab", "ozempic", "wegovy", "rybelsus", "semaglutide",
      "humira", "adalimumab", "xarelto", "rivaroxaban", "eliquis", "apixaban"
    ),
    canonical_hint = c(
      rep("sertralin", 4), rep("duloxetin", 4),
      "acetazolamid", "acetazolamid", "tiotropium olodaterol", "tiotropium olodaterol", "tiotropium", "olodaterol",
      "pembrolizumab", "pembrolizumab", "semaglutid", "semaglutid", "semaglutid", "semaglutid",
      "adalimumab", "adalimumab", "rivaroxaban", "rivaroxaban", "apixaban", "apixaban"
    ),
    stringsAsFactors = FALSE
  )
}

salt_words <- c(
  "hydrochlorid", "hydrochloride", "hydrobromid", "hydrobromide", "bromid", "bromide",
  "chlorid", "chloride", "natrium", "sodium", "kalium", "potassium", "calcium", "kalcium",
  "maleat", "maleate", "fumarat", "fumarate", "succinat", "succinate", "tartrat", "tartrate",
  "citrat", "citrate", "phosphat", "phosphate", "sulfat", "sulphate", "sulfate", "mesilat",
  "mesylate", "besilat", "besylate", "tosilat", "tosylate", "acetat", "acetate", "hemihydrat", "hydrate", "monohydrat", "dihydrat"
)

strip_salts <- function(x) {
  key <- normalise_key(x)
  if (length(key) == 0) return(character(0))
  out <- key
  for (salt in normalise_key(salt_words)) {
    out <- stringr::str_replace_all(out, paste0("\\b", salt, "\\b"), " ")
    out <- stringr::str_replace_all(out, paste0(salt, "$"), "")
  }
  stringr::str_squish(out)
}

guess_en_from_dk <- function(x) {
  key <- strip_salts(x)
  out <- key
  out <- ifelse(grepl("in$", out), sub("in$", "ine", out), out)
  out <- ifelse(grepl("id$", out), sub("id$", "ide", out), out)
  out <- ifelse(grepl("at$", out), sub("at$", "ate", out), out)
  out <- ifelse(grepl("on$", out), sub("on$", "one", out), out)
  out <- ifelse(grepl("amid$", out), sub("amid$", "amide", out), out)
  out
}

guess_dk_from_en <- function(x) {
  key <- strip_salts(x)
  out <- key
  out <- ifelse(grepl("ine$", out), sub("ine$", "in", out), out)
  out <- ifelse(grepl("ide$", out), sub("ide$", "id", out), out)
  out <- ifelse(grepl("ate$", out), sub("ate$", "at", out), out)
  out <- ifelse(grepl("one$", out), sub("one$", "on", out), out)
  out <- ifelse(grepl("amide$", out), sub("amide$", "amid", out), out)
  out
}

split_substances <- function(x) {
  x <- clean_text(x)
  if (length(x) == 0) return(character(0))
  parts <- unlist(strsplit(x, "\\s*(;|,|/|\\+| og | and )\\s*", perl = TRUE), use.names = FALSE)
  parts <- clean_text(parts)
  unique(parts[nzchar(parts)])
}

normalise_drug_query <- function(x) {
  x <- clean_text(x)
  x <- stringr::str_replace_all(x, "\\([^)]*\\)", " ")
  x <- stringr::str_replace_all(x, "\\b\\d+(\\.\\d+)?\\s*(mg|mikrogram|microgram|mcg|g|ml|ie|iu|%)\\b", " ")
  stringr::str_squish(x)
}

rows_matching_query <- function(search_key, q_key) {
  if (!nzchar(q_key)) return(rep(FALSE, length(search_key)))
  search_key == q_key | startsWith(search_key, q_key) | grepl(q_key, search_key, fixed = TRUE)
}

add_variants <- function(terms) {
  terms <- clean_text(terms)
  terms <- terms[nzchar(terms)]
  if (length(terms) == 0) return(character(0))
  keys <- unique(c(
    terms,
    normalise_key(terms),
    strip_salts(terms),
    guess_en_from_dk(terms),
    guess_dk_from_en(terms)
  ))
  keys <- clean_text(keys)
  unique(keys[nzchar(keys) & nchar(keys) >= 3])
}

build_query_terms <- function(query, dk_meds, ema_meds, max_terms = 35) {
  q <- normalise_drug_query(query)
  if (!nzchar(q)) return(character(0))

  terms <- add_variants(q)
  aliases <- manual_aliases()
  q_key <- normalise_key(q)
  alias_key <- normalise_key(aliases$alias)
  canon_key <- normalise_key(aliases$canonical_hint)
  alias_hit <- alias_key == q_key | canon_key == q_key | rows_matching_query(alias_key, q_key) | rows_matching_query(canon_key, q_key)
  if (any(alias_hit)) {
    canon <- unique(aliases$canonical_hint[alias_hit])
    terms <- c(terms, aliases$alias[canon_key %in% normalise_key(canon)], canon)
  }

  term_keys <- unique(normalise_key(terms))

  if (nrow(dk_meds) > 0 && length(term_keys) > 0) {
    hit <- rep(FALSE, nrow(dk_meds))
    for (tk in term_keys) hit <- hit | rows_matching_query(dk_meds$search_key, tk)
    if (any(hit)) {
      terms <- c(terms, dk_meds$medicine_name[hit], unlist(lapply(dk_meds$active_substance[hit], split_substances), use.names = FALSE))
    }
  }

  if (nrow(ema_meds) > 0 && length(term_keys) > 0) {
    hit <- rep(FALSE, nrow(ema_meds))
    for (tk in term_keys) hit <- hit | rows_matching_query(ema_meds$search_key, tk)
    if (any(hit)) {
      terms <- c(
        terms,
        ema_meds$medicine_name[hit],
        unlist(lapply(ema_meds$active_substance[hit], split_substances), use.names = FALSE),
        unlist(lapply(ema_meds$inn_common_name[hit], split_substances), use.names = FALSE)
      )
    }
  }

  terms <- add_variants(terms)
  # Keep user-facing original spelling first, then compact normalised variants.
  terms <- unique(c(q, terms))
  terms <- terms[nchar(terms) >= 3]
  utils::head(terms, max_terms)
}

score_text_against_terms <- function(text, terms) {
  text_key <- normalise_key(text)
  terms_key <- unique(normalise_key(terms))
  terms_key <- terms_key[nzchar(terms_key)]
  out <- rep(0L, length(text_key))
  if (length(terms_key) == 0 || length(text_key) == 0) return(out)

  for (term in terms_key) {
    if (!nzchar(term) || nchar(term) < 3) next
    out <- pmax(out, ifelse(text_key == term, 120L, 0L))
    out <- pmax(out, ifelse(startsWith(text_key, term), 95L, 0L))
    out <- pmax(out, ifelse(grepl(paste0("\\b", term, "\\b"), text_key, perl = TRUE), 85L, 0L))
    out <- pmax(out, ifelse(grepl(term, text_key, fixed = TRUE), 55L, 0L))
  }
  out
}

# -----------------------------------------------------------------------------
# 5. Unified document model and source searches
# -----------------------------------------------------------------------------

empty_documents <- function() {
  data.frame(
    doc_id = character(0), source = character(0), source_label = character(0), document_type = character(0),
    title = character(0), product_name = character(0), active_substance = character(0), company = character(0),
    document_date = character(0), version = character(0), document_id = character(0), query_used = character(0),
    url_document = character(0), url_pdf = character(0), url_html = character(0), url_source = character(0),
    file_format = character(0), can_view_inline = logical(0), notes = character(0), access_date = character(0),
    stringsAsFactors = FALSE
  )
}

standardise_documents <- function(df) {
  expected <- names(empty_documents())
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(empty_documents())
  for (col in setdiff(expected, names(df))) df[[col]] <- if (col == "can_view_inline") FALSE else ""
  df <- df[, expected, drop = FALSE]
  for (col in setdiff(names(df), "can_view_inline")) df[[col]] <- clean_text(df[[col]])
  df$can_view_inline <- as.logical(df$can_view_inline)
  df$can_view_inline[is.na(df$can_view_inline)] <- FALSE
  df
}

bind_documents <- function(...) {
  dfs <- lapply(list(...), standardise_documents)
  dfs <- dfs[vapply(dfs, nrow, integer(1)) > 0]
  if (length(dfs) == 0) return(empty_documents())
  out <- do.call(rbind, dfs)
  dedup <- paste(out$source, out$url_document, out$url_source, out$title, sep = "\r")
  out <- out[!duplicated(dedup), , drop = FALSE]
  out$doc_id <- paste0("doc_", seq_len(nrow(out)))
  row.names(out) <- NULL
  out
}

find_lms_documents <- function(query, dk_docs, dk_meds, ema_meds, max_docs = 120) {
  if (nrow(dk_docs) == 0) return(empty_documents())
  terms <- build_query_terms(query, dk_meds, ema_meds)
  if (length(terms) == 0) return(empty_documents())

  score <- score_text_against_terms(dk_docs$search_key, terms)
  keep <- which(score > 0)
  if (length(keep) == 0) return(empty_documents())

  rows <- dk_docs[keep, , drop = FALSE]
  rows$.score <- score[keep]
  rows <- rows[order(-rows$.score, safe_order_key(rows$file_name), method = "radix"), , drop = FALSE]
  rows <- utils::head(rows, max_docs)

  ext <- tolower(rows$format)
  ext[!nzchar(ext)] <- toupper(url_ext(rows$file_url[!nzchar(ext)]))

  homepage_url <- rows$homepage_url
  missing_homepage <- !nzchar(homepage_url)
  homepage_url[missing_homepage] <- vapply(rows$file_name[missing_homepage], make_lms_homepage_url, character(1))
  preview_url <- rows$preview_url

  docs <- data.frame(
    doc_id = "",
    source = "LMS",
    source_label = "LMS / danske SPC'er",
    document_type = "Dansk produktresumé / SPC",
    title = rows$file_name,
    product_name = rows$medicine_name_guess,
    active_substance = "",
    company = "",
    document_date = rows$modified_date,
    version = "",
    document_id = rows$file_name,
    query_used = query,
    url_document = rows$file_url,
    url_pdf = ifelse(tolower(rows$format) == "pdf", rows$file_url, ""),
    url_html = preview_url,
    url_source = homepage_url,
    file_format = rows$format,
    can_view_inline = TRUE,
    notes = paste0("Matchscore: ", rows$.score, "; indexeret fra Produktresume.dk"),
    access_date = format(Sys.Date(), "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  standardise_documents(docs)
}

parse_iso_or_ema_date <- function(x) {
  x <- clean_text(x)
  if (!nzchar(x)) return("")
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return(x)
  m <- stringr::str_match(x, "(\\d{1,2})/(\\d{1,2})/(\\d{4})")
  if (all(!is.na(m))) return(sprintf("%04d-%02d-%02d", as.integer(m[4]), as.integer(m[3]), as.integer(m[2])))
  ""
}

find_ema_candidate_rows <- function(query, ema_meds, dk_meds, max_candidates = 5) {
  if (nrow(ema_meds) == 0) return(ema_meds[FALSE, , drop = FALSE])
  # EMA candidate selection should be somewhat stricter than downstream source
  # searches. Otherwise a query for one component in a DK combination product can
  # add co-ingredients as expansion terms and then pull unrelated EMA products
  # such as Nustendi for atorvastatin via ezetimibe.
  q <- normalise_drug_query(query)
  primary_terms <- add_variants(q)
  aliases <- manual_aliases()
  q_key <- normalise_key(q)
  alias_key <- normalise_key(aliases$alias)
  canon_key <- normalise_key(aliases$canonical_hint)
  alias_hit <- alias_key == q_key | canon_key == q_key | rows_matching_query(alias_key, q_key) | rows_matching_query(canon_key, q_key)
  if (any(alias_hit)) {
    canon <- unique(aliases$canonical_hint[alias_hit])
    primary_terms <- c(primary_terms, aliases$alias[canon_key %in% normalise_key(canon)], canon)
  }
  terms <- unique(add_variants(primary_terms))
  if (length(terms) == 0) return(ema_meds[FALSE, , drop = FALSE])
  ema_match_key <- normalise_key(paste(ema_meds$medicine_name, ema_meds$active_substance, ema_meds$inn_common_name, ema_meds$atc_code_human, ema_meds$marketing_authorisation_holder, sep = " "))
  score <- score_text_against_terms(ema_match_key, terms)
  keep <- which(score > 0 & nzchar(ema_meds$medicine_url))
  if (length(keep) == 0) return(ema_meds[FALSE, , drop = FALSE])
  rows <- ema_meds[keep, , drop = FALSE]
  rows$.score <- score[keep]
  rows <- rows[order(-rows$.score, safe_order_key(rows$medicine_name), method = "radix"), , drop = FALSE]
  utils::head(rows, max_candidates)
}

extract_anchor_rows <- function(html, base_url) {
  pattern <- "(?is)<a\\b[^>]*href\\s*=\\s*[\"']([^\"']+)[\"'][^>]*>(.*?)</a>"
  m <- gregexpr(pattern, html, perl = TRUE)[[1]]
  if (length(m) == 1 && identical(m[[1]], -1L)) return(data.frame())
  matches <- regmatches(html, list(m))[[1]]
  starts <- as.integer(m)

  rows <- lapply(seq_along(matches), function(i) {
    one <- matches[[i]]
    href <- sub("(?is).*href\\s*=\\s*[\"']([^\"']+)[\"'].*", "\\1", one, perl = TRUE)
    link_text <- sub("(?is)^.*?>", "", sub("(?is)</a>.*$", "", one, perl = TRUE), perl = TRUE)
    link_text <- strip_html(link_text)
    before <- substr(html, max(1L, starts[[i]] - 700L), starts[[i]] - 1L)
    after <- substr(html, starts[[i]], min(nchar(html), starts[[i]] + 700L))
    context <- strip_html(paste(before, one, after, sep = " "))
    data.frame(
      href = href,
      url = make_absolute_file_url(href, base_url),
      link_text = link_text,
      context = context,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[nzchar(out$url), , drop = FALSE]
  out <- out[!duplicated(out$url), , drop = FALSE]
  row.names(out) <- NULL
  out
}

ema_language_from_url <- function(url, context = "") {
  txt <- paste(url, context)
  code <- stringr::str_match(txt, "_([a-z]{2})\\.(pdf|docx?|html?)(?:[?#].*)?$")[, 2]
  code <- clean_text(code)
  if (length(code) && nzchar(code[[1]])) return(toupper(code[[1]]))
  if (grepl("Danish|Dansk|\\bDA\\b", context, ignore.case = TRUE)) return("DA")
  if (grepl("English|\\bEN\\b", context, ignore.case = TRUE)) return("EN")
  ""
}

scrape_ema_product_information_one <- function(row) {
  page_url <- first_nonempty(row$medicine_url)
  if (!nzchar(page_url)) return(empty_documents())
  html <- http_get_text(page_url, timeout_sec = 35)
  if (is.null(html) || !nzchar(html)) return(empty_documents())

  anchors <- extract_anchor_rows(html, page_url)
  if (nrow(anchors) == 0) {
    anchors <- data.frame(
      href = character(0), url = character(0), link_text = character(0),
      context = character(0), stringsAsFactors = FALSE
    )
  }

  context_key <- normalise_key(paste(anchors$link_text, anchors$context, anchors$url, sep = " "))
  url_key <- tolower(anchors$url)
  # EMA pages place many document blocks close together.  The text context around a
  # "View" anchor may therefore include the previous block, e.g. Risk management
  # plan, even when the anchor URL itself is the correct Product Information PDF.
  # Treat the canonical URL path as authoritative and only apply negative filters
  # to weaker context-only matches.
  is_product_information_url <- grepl("/documents/product-information/", url_key, fixed = TRUE)
  is_all_presentations <- grepl("all-authorised-presentations|all_authorised_presentations|authorised-presentations", url_key, ignore.case = TRUE) |
    grepl("all authorised presentations", context_key, fixed = TRUE)
  is_pi_context <- grepl("product information", context_key, fixed = TRUE) |
    grepl("produktinformation", context_key, fixed = TRUE) |
    grepl("summary of product characteristics", context_key, fixed = TRUE) |
    grepl("produktresume", context_key, fixed = TRUE)
  is_bad_context <- grepl("assessment report|public assessment|medicine overview|summary for the public|risk management|procedural steps|orphan maintenance|paediatric|all authorised presentations", context_key, ignore.case = TRUE)
  # Base R's default regex engine does not support non-capturing groups such as
  # (?:...). Keep this check plain/perl-safe; otherwise the parser can fail inside
  # tryCatch() and return no EMA documents.
  is_doc <- grepl("\\.(pdf|doc|docx)([?#].*)?$", anchors$url, ignore.case = TRUE, perl = TRUE)
  keep <- ((is_product_information_url & !is_all_presentations) | (is_pi_context & !is_bad_context)) & is_doc

  # Defensive fallback for EMA's current markup. The product-information language
  # links are the authoritative signal, e.g.:
  # /da/documents/product-information/<slug>-epar-product-information_da.pdf
  if (!any(keep)) {
    href_pattern <- "(?is)href\\s*=\\s*[\"']([^\"']*/documents/product-information/[^\"']+\\.(pdf|docx?|html?)([?#][^\"']*)?)[\"']"
    m <- gregexpr(href_pattern, html, perl = TRUE)[[1]]
    if (!(length(m) == 1 && identical(m[[1]], -1L))) {
      raw_matches <- regmatches(html, list(m))[[1]]
      hrefs <- sub(href_pattern, "\\1", raw_matches, perl = TRUE)
      direct <- data.frame(
        href = hrefs,
        url = vapply(hrefs, make_absolute_file_url, character(1), base_url = page_url),
        link_text = "View",
        context = "Product information",
        stringsAsFactors = FALSE
      )
      direct <- direct[nzchar(direct$url), , drop = FALSE]
      direct <- direct[!duplicated(direct$url), , drop = FALSE]
      anchors <- rbind(anchors, direct)
      anchors <- anchors[!duplicated(anchors$url), , drop = FALSE]

      context_key <- normalise_key(paste(anchors$link_text, anchors$context, anchors$url, sep = " "))
      url_key <- tolower(anchors$url)
      is_product_information_url <- grepl("/documents/product-information/", url_key, fixed = TRUE)
      is_all_presentations <- grepl("all-authorised-presentations|all_authorised_presentations|authorised-presentations", url_key, ignore.case = TRUE) |
        grepl("all authorised presentations", context_key, fixed = TRUE)
      is_pi_context <- grepl("product information", context_key, fixed = TRUE) |
        grepl("produktinformation", context_key, fixed = TRUE) |
        grepl("summary of product characteristics", context_key, fixed = TRUE) |
        grepl("produktresume", context_key, fixed = TRUE)
      is_bad_context <- grepl("assessment report|public assessment|medicine overview|summary for the public|risk management|procedural steps|orphan maintenance|paediatric|all authorised presentations", context_key, ignore.case = TRUE)
      is_doc <- grepl("\\.(pdf|doc|docx)([?#].*)?$", anchors$url, ignore.case = TRUE, perl = TRUE)
      keep <- ((is_product_information_url & !is_all_presentations) | (is_pi_context & !is_bad_context)) & is_doc
    }
  }

  if (!any(keep)) return(empty_documents())

  sel <- anchors[keep, , drop = FALSE]
  sel$lang <- mapply(ema_language_from_url, sel$url, sel$context, USE.NAMES = FALSE)

  # Prefer Danish Product Information. If Danish is unavailable, fall back to
  # English, then to the first available Product Information document. This avoids
  # showing 20+ language duplicates for one EMA medicine.
  preferred_lang <- if (any(sel$lang == "DA")) "DA" else if (any(sel$lang == "EN")) "EN" else ""
  if (nzchar(preferred_lang)) {
    sel <- sel[sel$lang == preferred_lang, , drop = FALSE]
  } else {
    sel <- utils::head(sel, 1)
  }
  lang <- sel$lang

  date_guess <- vapply(sel$context, function(x) {
    m <- stringr::str_match(x, "Last updated:\\s*(\\d{1,2}/\\d{1,2}/\\d{4})")
    if (!all(is.na(m))) return(parse_iso_or_ema_date(m[2]))
    m2 <- stringr::str_match(x, "(\\d{1,2}/\\d{1,2}/\\d{4})")
    if (!all(is.na(m2))) return(parse_iso_or_ema_date(m2[1]))
    first_nonempty(row$last_updated_date, row$first_published_date)
  }, character(1))

  title <- paste0(first_nonempty(row$medicine_name, "EMA medicine"), " — EMA Product Information")
  title <- ifelse(nzchar(lang), paste0(title, " (", lang, ")"), title)
  fmt <- toupper(url_ext(sel$url))

  docs <- data.frame(
    doc_id = "",
    source = "EMA",
    source_label = "EMA",
    document_type = "EMA Product Information",
    title = title,
    product_name = clean_text(row$medicine_name),
    active_substance = first_nonempty(row$active_substance, row$inn_common_name),
    company = clean_text(row$marketing_authorisation_holder),
    document_date = date_guess,
    version = "",
    document_id = clean_text(row$ema_product_number),
    query_used = "",
    url_document = sel$url,
    url_pdf = ifelse(tolower(fmt) == "pdf", sel$url, ""),
    url_html = "",
    url_source = page_url,
    file_format = fmt,
    can_view_inline = TRUE,
    notes = paste0("EMA Product Information. EMA medicine page: ", page_url),
    access_date = format(Sys.Date(), "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  standardise_documents(docs)
}

find_ema_product_information <- function(query, ema_meds, dk_meds, max_candidates = 5) {
  candidates <- find_ema_candidate_rows(query, ema_meds, dk_meds, max_candidates = max_candidates)
  if (nrow(candidates) == 0) return(empty_documents())

  out <- vector("list", nrow(candidates))
  errors <- character(0)
  for (i in seq_len(nrow(candidates))) {
    one <- candidates[i, , drop = FALSE]
    docs <- tryCatch(scrape_ema_product_information_one(one), error = function(e) {
      errors <<- c(errors, paste0(display_value(one$medicine_name), ": ", conditionMessage(e)))
      empty_documents()
    })
    if (nrow(docs) > 0) docs$query_used <- query
    out[[i]] <- docs
    if (i < nrow(candidates)) Sys.sleep(0.1)
  }
  docs <- do.call(bind_documents, out)
  attr(docs, "candidate_count") <- nrow(candidates)
  attr(docs, "candidate_names") <- candidates$medicine_name
  attr(docs, "errors") <- errors
  docs
}

parse_dailymed_date <- function(x) {
  x <- clean_text(x)
  if (!nzchar(x)) return(NA_character_)
  m <- stringr::str_match(x, "^([A-Za-z]{3})\\s+(\\d{1,2}),\\s*(\\d{4})$")
  if (all(!is.na(m))) {
    month_map <- c(Jan = 1, Feb = 2, Mar = 3, Apr = 4, May = 5, Jun = 6, Jul = 7, Aug = 8, Sep = 9, Oct = 10, Nov = 11, Dec = 12)
    mon <- month_map[[m[2]]]
    day <- suppressWarnings(as.integer(m[3])); year <- suppressWarnings(as.integer(m[4]))
    if (!is.null(mon) && !is.na(day) && !is.na(year)) return(sprintf("%04d-%02d-%02d", year, mon, day))
  }
  iso <- suppressWarnings(as.Date(x))
  if (!is.na(iso)) format(iso, "%Y-%m-%d") else NA_character_
}

find_dailymed_one <- function(query, pagesize = 25) {
  query <- normalise_drug_query(query)
  if (!nzchar(query) || nchar(query) < 3) return(empty_documents())
  api_url <- build_url(
    "https://dailymed.nlm.nih.gov/dailymed/services/v2/spls.json",
    list(drug_name = query, name_type = "both", pagesize = min(max(as.integer(pagesize), 1), 100), page = 1)
  )
  txt <- http_get_text(api_url, timeout_sec = 30)
  if (is.null(txt) || !nzchar(txt)) return(empty_documents())
  obj <- jsonlite::fromJSON(txt, flatten = TRUE)
  if (is.null(obj$data) || length(obj$data) == 0) return(empty_documents())
  dat <- obj$data
  if (!is.data.frame(dat)) dat <- as.data.frame(dat, stringsAsFactors = FALSE)
  for (needed in c("setid", "spl_version", "title", "published_date")) if (!(needed %in% names(dat))) dat[[needed]] <- ""
  setid <- clean_text(dat$setid)
  dat <- dat[nzchar(setid), , drop = FALSE]
  if (nrow(dat) == 0) return(empty_documents())
  setid <- clean_text(dat$setid)
  published_raw <- clean_text(dat$published_date)
  published_iso <- vapply(published_raw, parse_dailymed_date, character(1))
  published_iso[is.na(published_iso)] <- ""
  html_url <- paste0("https://dailymed.nlm.nih.gov/dailymed/drugInfo.cfm?setid=", utils::URLencode(setid, reserved = TRUE))
  pdf_url <- paste0("https://dailymed.nlm.nih.gov/dailymed/downloadpdffile.cfm?setId=", utils::URLencode(setid, reserved = TRUE))

  docs <- data.frame(
    doc_id = "",
    source = "FDA",
    source_label = "FDA / DailyMed",
    document_type = "FDA label / SPL",
    title = clean_text(dat$title),
    product_name = "",
    active_substance = "",
    company = "",
    document_date = published_iso,
    version = clean_text(dat$spl_version),
    document_id = setid,
    query_used = query,
    url_document = pdf_url,
    url_pdf = pdf_url,
    url_html = html_url,
    url_source = html_url,
    file_format = "PDF/HTML",
    can_view_inline = TRUE,
    notes = paste0("DailyMed setid: ", setid, "; published: ", published_raw),
    access_date = format(Sys.Date(), "%Y-%m-%d"),
    stringsAsFactors = FALSE
  )
  standardise_documents(docs)
}

find_dailymed_documents <- function(query, dk_meds, ema_meds, pagesize = 25, max_queries = 8) {
  terms <- build_query_terms(query, dk_meds, ema_meds)
  if (length(terms) == 0) return(empty_documents())
  # Prefer terms likely to work in US labels: original, EN guesses, brands, salt terms.
  candidate_terms <- unique(c(query, guess_en_from_dk(terms), terms))
  candidate_terms <- clean_text(candidate_terms)
  candidate_terms <- candidate_terms[nzchar(candidate_terms) & nchar(candidate_terms) >= 3]
  candidate_terms <- utils::head(unique(candidate_terms), max_queries)
  if (length(candidate_terms) == 0) return(empty_documents())

  out <- vector("list", length(candidate_terms))
  errors <- character(0)
  for (i in seq_along(candidate_terms)) {
    q <- candidate_terms[[i]]
    out[[i]] <- tryCatch(find_dailymed_one(q, pagesize = pagesize), error = function(e) {
      errors <<- c(errors, paste0(q, ": ", conditionMessage(e)))
      empty_documents()
    })
    if (i < length(candidate_terms)) Sys.sleep(0.05)
  }
  docs <- do.call(bind_documents, out)
  if (nrow(docs) > 0) {
    date_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", docs$document_date)))
    date_num[is.na(date_num)] <- 0L
    docs <- docs[order(safe_order_key(docs$title), -date_num, method = "radix"), , drop = FALSE]
    docs <- docs[!duplicated(docs$document_id), , drop = FALSE]
    row.names(docs) <- NULL
  }
  attr(docs, "queries") <- candidate_terms
  attr(docs, "errors") <- errors
  docs
}

source_display <- function(x) {
  switch(x, LMS = "LMS", EMA = "EMA", FDA = "FDA / DailyMed", x)
}

# -----------------------------------------------------------------------------
# 6. RIS and viewer helpers
# -----------------------------------------------------------------------------

year_from_date <- function(x) {
  x <- clean_text(x)
  ifelse(grepl("^\\d{4}", x), substr(x, 1, 4), "")
}

as_ris_date <- function(x) {
  x <- clean_text(x)
  if (!nzchar(x)) return("")
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", x)) return(gsub("-", "/", x, fixed = TRUE))
  ""
}

ris_line <- function(tag, value) {
  value <- clean_text(value)
  if (!nzchar(value)) return(character(0))
  paste0(tag, "  - ", value)
}

make_ris_record <- function(doc) {
  source <- first_nonempty(doc$source)
  authors <- switch(
    source,
    LMS = "Lægemiddelstyrelsen",
    EMA = "European Medicines Agency",
    FDA = c("DailyMed", "National Library of Medicine", "Food and Drug Administration"),
    first_nonempty(doc$source_label)
  )
  author_lines <- unlist(lapply(unique(clean_text(authors)), function(a) ris_line("AU", a)), use.names = FALSE)
  date <- first_nonempty(doc$document_date)
  notes <- paste(c(first_nonempty(doc$notes), paste0("source: ", source), paste0("document id: ", first_nonempty(doc$document_id)))[nzchar(c(first_nonempty(doc$notes), source, first_nonempty(doc$document_id)))], collapse = "; ")
  lines <- c(
    ris_line("TY", "RPRT"),
    author_lines,
    ris_line("TI", first_nonempty(doc$title, "Regulatory document")),
    ris_line("PY", year_from_date(date)),
    ris_line("DA", as_ris_date(date)),
    ris_line("PB", first_nonempty(doc$source_label)),
    ris_line("UR", first_nonempty(doc$url_source, doc$url_html, doc$url_document)),
    ris_line("Y2", as_ris_date(first_nonempty(doc$access_date, format(Sys.Date(), "%Y-%m-%d")))),
    ris_line("N1", notes),
    "ER  -"
  )
  paste(lines[nzchar(lines)], collapse = "\r\n")
}

preview_cache_filename <- function(doc) {
  source <- first_nonempty(doc$source, "DOC")
  id <- first_nonempty(doc$document_id, doc$title, doc$url_document, doc$url_pdf)
  paste0(safe_filename(paste(source, id, sep = "_"), "preview"), ".pdf")
}

cache_pdf_preview <- function(doc, preview_cache_dir) {
  source <- first_nonempty(doc$source)
  url <- ""
  if (identical(source, "FDA")) {
    url <- first_nonempty(doc$url_pdf, doc$url_document)
  } else if (identical(source, "EMA")) {
    url <- first_nonempty(doc$url_pdf, doc$url_document)
  }
  if (!nzchar(url)) return("")
  if (!dir.exists(preview_cache_dir)) dir.create(preview_cache_dir, recursive = TRUE, showWarnings = FALSE)
  file_path <- file.path(preview_cache_dir, preview_cache_filename(doc))
  needs_download <- !file.exists(file_path) || is.na(file.info(file_path)$size) || file.info(file_path)$size < 1024
  if (needs_download) {
    tmp <- paste0(file_path, ".tmp")
    bin <- http_get_binary(url)
    writeBin(bin, tmp)
    file.rename(tmp, file_path)
  }
  paste0("regulatory_doc_previews/", utils::URLencode(basename(file_path), reserved = TRUE))
}

preview_payload_for_doc <- function(doc, preview_cache_dir) {
  source <- first_nonempty(doc$source)
  if (identical(source, "FDA")) {
    url <- tryCatch(cache_pdf_preview(doc, preview_cache_dir), error = function(e) "")
    if (nzchar(url)) return(list(mode = "iframe", url = url, external_url = first_nonempty(doc$url_pdf, doc$url_document), message = "FDA-label vises som PDF hentet fra DailyMed."))
    return(list(mode = "none", url = "", external_url = first_nonempty(doc$url_pdf, doc$url_document, doc$url_source), message = "FDA PDF-preview kunne ikke hentes. Brug Download dokument eller Gå til hjemmesiden."))
  }
  if (identical(source, "EMA")) {
    url <- tryCatch(cache_pdf_preview(doc, preview_cache_dir), error = function(e) "")
    if (nzchar(url)) return(list(mode = "iframe", url = url, external_url = first_nonempty(doc$url_pdf, doc$url_document), message = "EMA Product Information vises som PDF."))
    return(list(mode = "none", url = "", external_url = first_nonempty(doc$url_pdf, doc$url_document, doc$url_source), message = "EMA PDF-preview kunne ikke hentes. Brug Download dokument eller Gå til hjemmesiden."))
  }
  if (identical(source, "LMS")) {
    # Danish SPCs are usually Word documents and Produktresume.dk's rich_preview
    # is frontend-dependent. Repeated attempts to scrape/embed it proved unstable
    # and could trigger unwanted browser tabs. Therefore LMS inline preview is
    # intentionally disabled; use Download document or the Produktresume.dk page.
    return(list(
      mode = "none",
      url = "",
      external_url = first_nonempty(doc$url_source),
      message = "Visning i appen er ikke tilgængelig for danske SPC'er. Brug Gå til hjemmesiden eller Download dokument."
    ))
  }
  list(mode = "none", url = "", external_url = first_nonempty(doc$url_source, doc$url_document), message = "Preview er ikke tilgængelig for denne dokumenttype.")
}

download_url_for_doc <- function(doc) {
  source <- first_nonempty(doc$source)
  if (identical(source, "FDA") && nzchar(first_nonempty(doc$url_pdf))) return(first_nonempty(doc$url_pdf))
  first_nonempty(doc$url_document, doc$url_pdf, doc$url_html, doc$url_source)
}

extension_for_doc <- function(doc) {
  source <- first_nonempty(doc$source)
  fmt <- tolower(first_nonempty(doc$file_format))
  # DailyMed's PDF endpoint ends in .cfm but returns a PDF. Prefer the declared
  # document type over the CGI extension.
  if (identical(source, "FDA") && grepl("pdf", fmt, fixed = TRUE)) return("pdf")
  if (grepl("pdf", fmt, fixed = TRUE)) return("pdf")
  if (grepl("docx", fmt, fixed = TRUE)) return("docx")
  if (grepl("doc", fmt, fixed = TRUE)) return("doc")
  if (grepl("html", fmt, fixed = TRUE)) return("html")
  url <- first_nonempty(doc$url_document, doc$url_pdf, doc$url_html, doc$url_source)
  ext <- url_ext(url)
  if (nzchar(ext) && !identical(ext, "cfm")) return(ext)
  "dat"
}

mime_for_extension <- function(ext) {
  switch(tolower(ext), pdf = "application/pdf", html = "text/html", htm = "text/html", xml = "application/xml", zip = "application/zip", doc = "application/msword", docx = "application/vnd.openxmlformats-officedocument.wordprocessingml.document", "application/octet-stream")
}

make_doc_label <- function(df) {
  label <- paste0(df$source, ": ", df$title)
  label <- ifelse(nzchar(df$document_date), paste0(label, " — ", df$document_date), label)
  make.unique(clean_text(label))
}

# -----------------------------------------------------------------------------
# 7. Load data at startup
# -----------------------------------------------------------------------------

data_dir <- find_data_dir()
dk_meds <- load_dk_medicines(data_dir)
dk_spc_docs <- load_dk_spc_documents(data_dir)
ema_meds <- load_ema_medicines(data_dir)

# -----------------------------------------------------------------------------
# 8. UI
# -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML("\n    body { padding-bottom: 30px; }\n    .source-chip { display: inline-block; padding: 7px 10px; margin: 4px 6px 4px 0; border-radius: 999px; background: #f0f0f0; }\n    .small-muted { color: #666; font-size: 90%; }\n    .doc-meta { background: #fafafa; border: 1px solid #ddd; padding: 12px; border-radius: 6px; margin: 12px 0 10px 0; }
    .doc-actions { margin: 10px 0 16px 0; display: flex; gap: 8px; flex-wrap: wrap; align-items: center; }
    .doc-list { margin-top: 12px; }
    .doc-list-title { font-weight: 700; margin-bottom: 8px; }
    .doc-grid { display: grid; grid-template-columns: repeat(3, minmax(240px, 1fr)); gap: 12px; align-items: start; width: 100%; }
    .doc-column { min-width: 0; }
    .doc-column-title { font-weight: 700; padding: 7px 9px; margin-bottom: 6px; border-bottom: 2px solid #e6e6e6; }
    .doc-card { margin: 6px 0; }
    .doc-card label { width: 100%; cursor: pointer; font-weight: 400; margin-bottom: 0; }
    .doc-card input[type='radio'] { margin-right: 7px; vertical-align: top; }
    .doc-card-content { display: inline-block; width: calc(100% - 26px); padding: 9px 10px; border: 1px solid #e3e3e3; border-radius: 7px; background: #fff; vertical-align: top; }
    .doc-card label:hover .doc-card-content { background: #f8f8f8; border-color: #cfcfcf; }
    .doc-card input[type='radio']:checked + .doc-card-content { background: #f2f7ff; border-color: #8daed8; }
    .doc-card-title { display: block; font-weight: 600; line-height: 1.25; }
    .doc-card-subtitle { display: block; color: #555; font-size: 90%; margin-top: 3px; }
    .doc-card-date { display: block; color: #777; font-size: 86%; margin-top: 2px; }
    @media (max-width: 1150px) { .doc-grid { grid-template-columns: repeat(auto-fit, minmax(270px, 1fr)); } }
    iframe.doc-frame { width: 100%; height: 760px; border: 1px solid #ccc; border-radius: 4px; }\n    table { font-size: 90%; }\n  "))),
  titlePanel("Regulatorisk dokumentfinder"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      textInput("query", "Søg på indholdsstof eller præparatnavn", placeholder = "fx sertralin, Zoloft, duloxetin, Cymbalta, Spiolto"),
      actionButton("search", "Søg", class = "btn-primary"),
      tags$hr(),
      uiOutput("source_filter_ui"),
      tags$hr(),
      h5("Datastatus"),
      verbatimTextOutput("data_status")
    ),
    mainPanel(
      width = 9,
      uiOutput("source_summary"),
      textOutput("search_status"),
      uiOutput("document_list_ui"),
      uiOutput("selected_doc_meta"),
      uiOutput("doc_actions"),
      uiOutput("viewer")
    )
  )
)

# -----------------------------------------------------------------------------
# 9. Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  preview_cache_dir <- file.path(tempdir(), "regulatory_doc_previews")
  dir.create(preview_cache_dir, recursive = TRUE, showWarnings = FALSE)
  tryCatch(shiny::addResourcePath("regulatory_doc_previews", preview_cache_dir), error = function(e) NULL)

  documents_found <- reactiveVal(empty_documents())
  search_message <- reactiveVal("Skriv en søgning og tryk Søg.")
  shown_doc_id <- reactiveVal("")

  observeEvent(input$search, {
    query <- clean_text(input$query)
    shown_doc_id("")
    if (nchar(query) < 2) {
      documents_found(empty_documents())
      search_message("Skriv mindst 2 tegn.")
      return()
    }

    search_message("Søger i LMS, EMA og DailyMed ...")
    withProgress(message = "Søger efter regulatoriske dokumenter", value = 0, {
      incProgress(0.2, detail = "LMS / danske SPC'er")
      lms_docs <- find_lms_documents(query, dk_spc_docs, dk_meds, ema_meds)

      incProgress(0.35, detail = "EMA Product Information")
      ema_docs <- find_ema_product_information(query, ema_meds, dk_meds, max_candidates = 5)

      incProgress(0.35, detail = "FDA / DailyMed")
      fda_docs <- find_dailymed_documents(query, dk_meds, ema_meds, pagesize = 25, max_queries = 8)

      docs <- bind_documents(lms_docs, ema_docs, fda_docs)
      if (nrow(docs) > 0) {
        source_priority <- c(LMS = 1L, EMA = 2L, FDA = 3L)
        docs$.source_priority <- source_priority[docs$source]
        docs$.source_priority[is.na(docs$.source_priority)] <- 9L
        date_num <- suppressWarnings(as.integer(gsub("[^0-9]", "", docs$document_date)))
        date_num[is.na(date_num)] <- 0L
        docs <- docs[order(docs$.source_priority, safe_order_key(docs$title), -date_num, method = "radix"), , drop = FALSE]
        docs$.source_priority <- NULL
        docs$doc_id <- paste0("doc_", seq_len(nrow(docs)))
        row.names(docs) <- NULL
      }
      documents_found(docs)

      if (nrow(docs) == 0) {
        search_message("Ingen dokumenter fundet. Prøv indholdsstof, handelsnavn eller engelsk/dansk stavemåde.")
      } else {
        counts <- table(docs$source)
        search_message(paste0("Fundet ", nrow(docs), " dokument(er): ", paste(paste0(names(counts), "=", as.integer(counts)), collapse = ", "), "."))
      }
    })
  }, ignoreInit = TRUE)

  output$source_filter_ui <- renderUI({
    docs <- documents_found()
    if (nrow(docs) == 0) {
      tags$div(class = "small-muted", "Kildefiltre vises efter søgning, hvis der findes dokumenter.")
    } else {
      counts <- table(docs$source)
      values <- names(counts)
      labels <- paste0(vapply(values, source_display, character(1)), " (", as.integer(counts), ")")
      checkboxGroupInput("sources", "Kilder med fund", choices = stats::setNames(values, labels), selected = values)
    }
  })

  filtered_docs <- reactive({
    docs <- documents_found()
    if (nrow(docs) == 0) return(docs)
    sources <- input$sources %||% unique(docs$source)
    if (length(sources) == 0) return(docs[FALSE, , drop = FALSE])
    docs[docs$source %in% sources, , drop = FALSE]
  })

  output$document_list_ui <- renderUI({
    docs <- filtered_docs()
    if (nrow(docs) == 0) return(tags$div(class = "small-muted", "Ingen dokumenter at vise."))
    selected <- input$selected_doc
    if (is.null(selected) || !(selected %in% docs$doc_id)) selected <- docs$doc_id[[1]]

    source_order <- c("LMS", "EMA", "FDA")
    sources <- source_order[source_order %in% unique(docs$source)]
    extras <- setdiff(unique(docs$source), source_order)
    sources <- c(sources, extras)

    make_card <- function(one) {
      subtitle <- paste(c(display_value(one$document_type), display_value(one$product_name))[nzchar(c(display_value(one$document_type), display_value(one$product_name))) & c(display_value(one$document_type), display_value(one$product_name)) != "—"], collapse = " · ")
      date_txt <- first_nonempty(one$document_date, one$version)
      tags$div(
        class = "doc-card",
        tags$label(
          tags$input(
            type = "radio",
            name = "selected_doc",
            value = one$doc_id,
            checked = if (identical(one$doc_id, selected)) "checked" else NULL
          ),
          tags$span(
            class = "doc-card-content",
            tags$span(class = "doc-card-title", display_value(one$title)),
            if (nzchar(subtitle)) tags$span(class = "doc-card-subtitle", subtitle) else NULL,
            if (nzchar(date_txt)) tags$span(class = "doc-card-date", date_txt) else NULL
          )
        )
      )
    }

    columns <- lapply(sources, function(src) {
      subset <- docs[docs$source == src, , drop = FALSE]
      tags$div(
        class = "doc-column",
        tags$div(class = "doc-column-title", paste0(source_display(src), " (", nrow(subset), ")")),
        lapply(seq_len(nrow(subset)), function(i) make_card(subset[i, , drop = FALSE]))
      )
    })

    tags$div(
      class = "doc-list form-group shiny-input-radiogroup",
      id = "selected_doc",
      tags$div(class = "doc-list-title", "Fundne dokumenter — klik på ét dokument"),
      tags$div(class = "doc-grid", columns)
    )
  })

  selected_doc <- reactive({
    docs <- filtered_docs()
    if (nrow(docs) == 0) return(empty_documents())
    selected <- input$selected_doc
    if (is.null(selected) || !nzchar(selected) || !(selected %in% docs$doc_id)) selected <- docs$doc_id[[1]]
    out <- docs[docs$doc_id == selected, , drop = FALSE]
    if (nrow(out) != 1) return(empty_documents())
    out
  })

  observeEvent(input$selected_doc, { shown_doc_id("") }, ignoreInit = TRUE)
  observeEvent(input$show_doc, {
    doc <- selected_doc()
    if (nrow(doc) == 1) shown_doc_id(doc$doc_id[[1]])
  }, ignoreInit = TRUE)

  output$source_summary <- renderUI({
    docs <- documents_found()
    if (nrow(docs) == 0) return(tags$div())
    counts <- table(docs$source)
    chips <- lapply(names(counts), function(src) {
      tags$span(class = "source-chip", paste0(source_display(src), ": ", as.integer(counts[[src]])))
    })
    tags$div(chips)
  })

  output$search_status <- renderText(search_message())

  output$selected_doc_meta <- renderUI({
    doc <- selected_doc()
    if (nrow(doc) != 1) return(tags$div())
    tags$div(
      class = "doc-meta",
      tags$b(display_value(doc$title)), tags$br(),
      tags$span("Kilde: ", display_value(doc$source_label)), tags$br(),
      tags$span("Dokumenttype: ", display_value(doc$document_type)), tags$br(),
      tags$span("Produkt: ", display_value(doc$product_name)), tags$br(),
      tags$span("Aktivt stof: ", display_value(doc$active_substance)), tags$br(),
      tags$span("Dato/version: ", display_value(first_nonempty(doc$document_date, doc$version))), tags$br(),
      tags$span("Noter: ", display_value(doc$notes))
    )
  })

  output$viewer <- renderUI({
    doc <- selected_doc()
    if (nrow(doc) != 1 || !identical(shown_doc_id(), doc$doc_id[[1]])) return(tags$div())
    preview <- preview_payload_for_doc(doc, preview_cache_dir)
    ext_link <- first_nonempty(preview$external_url)
    if (!nzchar(first_nonempty(preview$url))) {
      return(tags$div(
        class = "doc-preview-note",
        tags$p(preview$message),
        if (nzchar(ext_link)) tags$a(href = ext_link, target = "_blank", class = "btn btn-default", "Åbn ekstern side") else NULL
      ))
    }
    tags$div(
      class = "doc-preview",
      tags$p(class = "small-muted", preview$message),
      if (nzchar(ext_link)) tags$p(tags$a(href = ext_link, target = "_blank", "Åbn preview/PDF i ny fane")) else NULL,
      tags$iframe(src = preview$url, class = "doc-frame")
    )
  })

  output$doc_actions <- renderUI({
    doc <- selected_doc()
    if (nrow(doc) != 1) return(tags$div())
    url <- first_nonempty(doc$url_source, doc$url_html, doc$url_document)
    can_preview <- !identical(first_nonempty(doc$source), "LMS")
    tags$div(
      class = "doc-actions",
      if (can_preview) actionButton("show_doc", "Vis dokument i appen", class = "btn-primary") else NULL,
      downloadButton("download_doc", "Download dokument"),
      downloadButton("download_ris", "Download RIS-fil"),
      if (nzchar(url)) tags$a(href = url, target = "_blank", class = "btn btn-default", "Gå til hjemmesiden") else NULL,
      if (!can_preview) tags$span(class = "small-muted", "Danske SPC'er kan ikke vises stabilt i appen; brug download eller Produktresume.dk-siden.") else NULL
    )
  })

  output$download_doc <- downloadHandler(
    filename = function() {
      doc <- selected_doc()
      if (nrow(doc) != 1) return("document.dat")
      paste0(safe_filename(first_nonempty(doc$title, "document")), ".", extension_for_doc(doc))
    },
    contentType = "application/octet-stream",
    content = function(file) {
      doc <- selected_doc()
      if (nrow(doc) != 1) stop("No document selected", call. = FALSE)
      url <- download_url_for_doc(doc)
      if (!nzchar(url)) stop("Selected document has no download URL", call. = FALSE)
      writeBin(http_get_binary(url), file)
    }
  )

  output$download_ris <- downloadHandler(
    filename = function() {
      doc <- selected_doc()
      if (nrow(doc) != 1) return("regulatory_document.ris")
      paste0(safe_filename(first_nonempty(doc$title, "regulatory_document")), ".ris")
    },
    contentType = "application/x-research-info-systems",
    content = function(file) {
      doc <- selected_doc()
      if (nrow(doc) != 1) stop("No document selected", call. = FALSE)
      writeLines(make_ris_record(doc), file, useBytes = TRUE)
    }
  )

  output$data_status <- renderText({
    paste(
      "Data directory:", data_dir,
      paste0("DK lægemiddelliste: ", nrow(dk_meds), " rækker"),
      paste0("DK SPC-indeks: ", nrow(dk_spc_docs), " dokumenter"),
      paste0("EMA-table: ", nrow(ema_meds), " rækker"),
      "Resolver: ingen global resolver; søgning sker direkte i kilderne.",
      sep = "\n"
    )
  })
}

shinyApp(ui, server)
