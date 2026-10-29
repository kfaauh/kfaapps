# Run from powerbi/: Rscript tests/import.test.R [optional-full-export.csv]
suppressPackageStartupMessages(source("app.R", local = TRUE))
fail <- function(expr, pattern = NULL) {
  error <- tryCatch({ force(expr); NULL }, error = identity)
  stopifnot(inherits(error, "error"))
  if (!is.null(pattern)) stopifnot(grepl(pattern, conditionMessage(error)))
}
stopifnot(identical(normalise_atc_input("c09ca01; C09CA01; r05db29"), c("C09CA01", "C09CA01", "R05DB29")))
fail(normalise_atc_input("junk"), "Ugyldig ATC")
long <- read_export_csv("tests/fixtures/synthetic_sample.csv")
out <- finalise_parsed_long_export(long)
stopifnot(nrow(long) == 30L, nrow(out) == 5L, ncol(out) == 11L,
          all(out$`Måned` == "Januar"), all(out$`År` == 2025L),
          out$`Antal DDD`[out$Ydertype == "Almen læge"] == 1000)
nullable <- long; nullable$value[1] <- NA_character_; nullable$value[2] <- "0"
n <- finalise_parsed_long_export(nullable)
stopifnot(is.na(n$`Antal DDD`[n$Ydertype == "Almen læge"]), n$`Antal DDD`[n$Ydertype == "Hospitalslæge"] == 0)
zeros <- long; zeros$value <- "0"
z <- finalise_parsed_long_export(zeros)
stopifnot(nrow(z) == 5L, all(as.matrix(z[metric_cols]) == 0))
only_ddd <- finalise_parsed_long_export(long[long$nøgletal_output == "Antal DDD", ])
stopifnot(all(is.na(only_ddd$`Ekspeditionsbeløb`)))
fail(finalise_parsed_long_export(bind_rows(long, long)), "Overlappende")
bad <- long; bad$value[1] <- "123xyz"; fail(finalise_parsed_long_export(bad), "numerisk")
bad <- long; bad$`År`[1] <- "2024"; fail(finalise_parsed_long_export(bad), "stemmer ikke")
bad <- long; bad$`Måned`[1] <- "0"; fail(finalise_parsed_long_export(bad), "måned")
bad <- long; bad$status[1] <- "403"; fail(finalise_parsed_long_export(bad), "HTTP")
bad <- long; bad$hasRestartToken[1] <- "true"; fail(finalise_parsed_long_export(bad), "RestartToken")
bad <- long; bad$export_error[1] <- "Error"; fail(finalise_parsed_long_export(bad), "eksportfejl")
product <- long
product$opgoerelsesniveau <- "varenummer"; product$Varenummer <- "012345"
product$`Navn (Præparat)` <- "Testprodukt"; product$Form <- "Tabletter"
product$Styrke <- "50 mg"; product$Pakningsstørrelse <- "100 stk"
p <- finalise_parsed_long_export(product); stopifnot(all(p$Varenummer == "012345"))
product$Form[1] <- NA_character_; fail(finalise_parsed_long_export(product), "produktfelter")
fail(finalise_parsed_long_export(bind_rows(long, transform(long, opgoerelsesniveau = "varenummer"))), "niveau")
for (level in c("atc5", "varenummer")) {
  js <- generate_js("C09CA01", names(metric_map), "Region Midtjylland", 2025, TRUE, level)
  stopifnot(!grepl("__[A-Z_]+__", js), !grepl("powerbi.esundhed.dk", js), grepl("INDSÆT_AUTHORIZATION_HER", js, fixed = TRUE))
  file <- tempfile(fileext = ".js"); writeLines(js, file)
  stopifnot(system2("node", c("--check", shQuote(file))) == 0)
}
request <- list(atc = "C09CA01", region = "Region Midtjylland", year = 2025,
  opgoerelse = "Mængdeforbrug", normalisering = "Ingen valg", opgoerelsesniveau = "atc5",
  status = 200, rowCountMarker = 30, hasRestartToken = FALSE, parsedRows = 30, error = NULL)
empty <- request; empty$atc <- "N05CJ03"; empty$parsedRows <- 0; empty$rowCountMarker <- 0
summary <- list(version = 2, exportId = "test", complete = TRUE, expectedQueries = 2,
  completedQueries = 2, requests = list(request, empty), files = list(list(filename = "part.csv", rows = 30)))
f <- tempfile(fileext = ".json"); jsonlite::write_json(summary, f, auto_unbox = TRUE, null = "null")
s <- read_export_summary(f); stopifnot(nrow(s$meta) == 2L, s$meta$parsedRows[2] == 0L)
with_id <- long; with_id$export_id <- "test"
validate_export_summaries(list(s), with_id)
fail(validate_export_summaries(list(s), with_id[-1, ]), "mangler CSV")
fail(validate_export_summaries(list(s), long), "hører ikke")
fail(validate_export_summaries(list(s, s), with_id), "flere gange")
summary$complete <- FALSE; jsonlite::write_json(summary, f, auto_unbox = TRUE)
fail(read_export_summary(f), "fuldført")
shiny::testServer(server, {
  session$setInputs(atc = "C09CA01", regions = "Region Midtjylland", years_range = c(2025, 2025), output_level = "atc5")
  stopifnot(grepl("INDSÆT_AUTHORIZATION_HER", generated_js(), fixed = TRUE))
  upload <- data.frame(name = "sample.csv", size = file.info("tests/fixtures/synthetic_sample.csv")$size,
    type = "text/csv", datapath = normalizePath("tests/fixtures/synthetic_sample.csv"))
  session$setInputs(parsed_csv_files = upload, import_btn = 1)
  stopifnot(nrow(parsed_data_val()) == 5L, isTRUE(import_status_val()$ok))
  session$setInputs(parsed_csv_files = rbind(upload, upload), import_btn = 2)
  stopifnot(nrow(parsed_data_val()) == 0L, identical(import_status_val()$ok, FALSE))
})
args <- commandArgs(trailingOnly = TRUE)
if (length(args)) {
  full <- read_export_csv(args[1]); wide <- finalise_parsed_long_export(full)
  stopifnot(nrow(full) == nrow(full), nrow(wide) == nrow(distinct(full, across(all_of(c("ATC, Niveau 5, kode & tekst", "År", "Måned", "Ydertype", "Bopælsregion"))))), ncol(wide) == 11L,
    !anyDuplicated(wide[c("ATC, Niveau 5, kode & tekst", "År", "Måned", "Ydertype", "Bopælsregion")]),
    all(vapply(wide[metric_cols], is.numeric, logical(1))))
  pick <- wide$`ATC, Niveau 5, kode & tekst` == "C09CA01 - Losartan" & wide$`År` == 2025 & wide$Bopælsregion == "Region Midtjylland"
  expected <- full |>
    filter(atc_filter == "C09CA01", `År` == "2025", Bopælsregion == "Region Midtjylland", nøgletal_output == "Antal DDD") |>
    pull(value)
  expected <- sum(as.numeric(sub(",", ".", expected, fixed = TRUE)), na.rm = TRUE)
  stopifnot(sum(wide$`Antal DDD`[pick]) == expected)

  csv <- tempfile(fileext = ".csv"); readr::write_excel_csv2(wide, csv, na = "")
  xlsx <- tempfile(fileext = ".xlsx"); writexl::write_xlsx(list(Medicinsalg = wide), xlsx)
  stopifnot(file.info(csv)$size > 0, file.info(xlsx)$size > 0)
  cat(sprintf("Full export PASS: %d → %d rows, %d all-zero rows retained, DDD reconciled, CSV/XLSX written.\n", nrow(full), nrow(wide), sum(rowSums(wide[metric_cols]) == 0)))
}
cat("PASS: CSV import, decimals, missing/zero values, dates, duplicates, products, summaries, generator syntax, Shiny import action.\n")
