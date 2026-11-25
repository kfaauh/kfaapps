interaction_result_url <- function(ids) {
  paste0("https://www.interaktionsdatabasen.dk/SearchResult.aspx?pids=",
         paste(unique(ids), collapse = ","))
}

parse_interaction_matches <- function(page, count) {
  # The numbered search terms are present even when no interactions are listed.
  rows <- rvest::html_elements(page, "#ctl00_ContentPlaceHolder1__SearchParamsList td")
  rows <- trimws(rvest::html_text2(rows))
  rows <- rows[grepl("^[0-9]+\\.", rows)]
  positions <- as.integer(sub("^([0-9]+)\\..*$", "\\1", rows))
  terms <- sub("^[0-9]+\\.[[:space:]]*", "", rows)
  matched <- grepl("^Præparat[[:space:]]*:[[:space:]]*\\S", terms, perl = TRUE)
  unmatched <- grepl("matchede dette søgeord", terms, fixed = TRUE)

  if (!identical(positions, seq_len(count)) || any(!(matched | unmatched))) {
    stop("The interaction database returned an unexpected search term list.")
  }
  matched
}

check_interaction_ids <- function(ids, timeout) {
  response <- httr::GET(interaction_result_url(ids), httr::timeout(timeout))
  httr::stop_for_status(response)
  parse_interaction_matches(httr::content(response, as = "parsed", type = "text/html"), length(ids))
}

resolve_interaction_ids <- function(atc_codes, drugs, check_ids = check_interaction_ids) {
  codes <- unique(toupper(trimws(atc_codes)))
  codes <- codes[!is.na(codes) & nzchar(codes)]
  drugs <- drugs[drugs$V7 %in% codes & grepl("^[0-9]+$", drugs$V1), ]
  drugs <- drugs[order(!drugs$in_medicinpriser), ]
  candidates <- setNames(lapply(codes, function(code) {
    unique(drugs$V1[drugs$V7 == code])
  }), codes)
  result <- setNames(rep(NA_character_, length(codes)), codes)
  deadline <- proc.time()[["elapsed"]] + 12

  for (attempt in seq_len(6L)) {
    pending <- candidates[is.na(result) & lengths(candidates) > 0L]
    remaining <- deadline - proc.time()[["elapsed"]]
    if (!length(pending) || remaining < 1) break

    # Test one preferred ID per code first. Use remaining batch space for alternatives.
    batch <- vapply(pending, `[`, character(1), 1L)
    if (attempt > 1L && length(batch) < 10L) {
      batch <- c(batch, unlist(lapply(pending, function(ids) ids[-1L]), use.names = FALSE))
    }
    batch <- head(unique(batch), 10L)
    verified <- batch[check_ids(batch, timeout = min(6, remaining))]

    for (code in names(pending)) {
      hits <- intersect(pending[[code]], verified)
      if (length(hits)) result[[code]] <- hits[1L]
      candidates[[code]] <- setdiff(pending[[code]], batch)
    }
  }

  result
}
