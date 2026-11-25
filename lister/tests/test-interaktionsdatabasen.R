# Run from the repository root: Rscript lister/tests/test-interaktionsdatabasen.R
source("lister/interaktionsdatabasen.R", encoding = "UTF-8")

result_page <- function(terms, positions = seq_along(terms)) {
  xml2::read_html(paste0(
    '<div id="ctl00_ContentPlaceHolder1__SearchParamsList">',
    paste0('<table><tr><td></td></tr><tr><td></td><td><span><b>',
           positions, '.</b></span>&nbsp;', terms, '</td></tr></table>', collapse = ""),
    '</div><div>Din søgning gav intet resultat.</div>'
  ))
}

recognized <- 'Præparat: <span><b>Xarelto</b></span> - Aktivt indholdsstof: rivaroxaban'
unknown <- '- (ingen præparater og/eller indholdsstoffer matchede dette søgeord!)'
stopifnot(
  identical(parse_interaction_matches(result_page(c(unknown, recognized)), 2L), c(FALSE, TRUE)),
  identical(parse_interaction_matches(result_page(recognized), 1L), TRUE),
  inherits(try(parse_interaction_matches(result_page(recognized), 2L), silent = TRUE), "try-error"),
  inherits(try(parse_interaction_matches(result_page(recognized, 2L), 1L), silent = TRUE), "try-error"),
  inherits(try(parse_interaction_matches(result_page("Ukendt svarformat"), 1L), silent = TRUE), "try-error")
)

drugs <- data.frame(
  V1 = c("28106644821", "28104249107", "28105038412", "28105038412",
         "28106288619", "28104682910", "28100597871", "28100983067", "", NA),
  V7 = c(rep("B01AF01", 4L), rep("B01AF02", 2L), rep("N06AA04", 2L), rep("A01AA01", 2L)),
  in_medicinpriser = c(FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE)
)

tested <- list()
check_ids <- function(ids, timeout) {
  tested[[length(tested) + 1L]] <<- unname(ids)
  ids %in% c("28105038412", "28104682910", "28100983067")
}

result <- resolve_interaction_ids(
  c(" b01af01 ", "B01AF01", "B01AF02", "N06AA04", "A01AA01", "B01AF", NA, ""),
  drugs, check_ids
)
stopifnot(
  identical(tested[[1L]], c("28104249107", "28104682910", "28100983067")),
  identical(tested[[2L]], c("28105038412", "28106644821")),
  identical(result, c(B01AF01 = "28105038412", B01AF02 = "28104682910",
                      N06AA04 = "28100983067", A01AA01 = NA_character_, B01AF = NA_character_)),
  identical(interaction_result_url(unname(result[!is.na(result)])), paste0(
    "https://www.interaktionsdatabasen.dk/SearchResult.aspx?pids=",
    "28105038412,28104682910,28100983067"
  )),
  identical(resolve_interaction_ids("B01AF01", drugs, function(ids, timeout) rep(FALSE, length(ids))),
            c(B01AF01 = NA_character_)),
  inherits(try(resolve_interaction_ids("B01AF01", drugs, function(ids, timeout) stop("timeout")),
               silent = TRUE), "try-error")
)
