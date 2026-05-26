# app.R
# Shiny-app: Generator til Power BI Medicinsalg-udtræk + parser
#
# Formål:
# 1) Generér JavaScript-kode, som kan køres i Chrome DevTools Console på den åbne Power BI-rapport.
# 2) JavaScript-koden henter JSON via brugerens eksisterende browser-session/cookies.
# 3) Upload den downloadede JSON-fil i appen og parse den til tabel.
#
# Pakker:
# install.packages(c(
#   "shiny", "bslib", "jsonlite", "dplyr", "tidyr", "stringr",
#   "purrr", "readr", "DT", "writexl"
# ))

library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(DT)
library(writexl)

# Tillad upload af moderate CSV-chunks/rå JSON, men undgå at basere workflowet på én kæmpe JSON-fil.
options(shiny.maxRequestSize = 200 * 1024^2)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalise_atc_input <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))

  x |>
    str_split("[\\r\\n,;]+") |>
    unlist(use.names = FALSE) |>
    str_trim() |>
    discard(~ .x == "") |>
    unique()
}

clean_atc_code_for_mapping <- function(x) {
  x |>
    str_trim() |>
    str_extract("^[A-Za-z][0-9]{2}[A-Za-z]{1,2}[0-9]{0,4}") |>
    toupper()
}

read_mapping_file <- function(path) {
  if (is.null(path) || !file.exists(path)) return(NULL)

  ext <- tolower(tools::file_ext(path))

  dat <- switch(
    ext,
    "csv" = readr::read_csv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    "txt" = readr::read_delim(path, delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    "tsv" = readr::read_tsv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    stop("Mapping-filen skal være csv, txt eller tsv.")
  )

  nms <- names(dat)
  nms_low <- tolower(nms)

  atc_col <- nms[match(TRUE, nms_low %in% c("atc", "atc_kode", "atckode", "atc_code"))]
  pbi_col <- nms[match(TRUE, nms_low %in% c("atc5_kode_tekst", "powerbi_atc", "powerbi", "atc5", "atc5tekst"))]

  if (is.na(atc_col) || is.na(pbi_col)) {
    stop("Mapping-filen skal indeholde kolonnerne ATC og ATC5_Kode_Tekst.")
  }

  dat |>
    transmute(
      ATC = toupper(str_trim(as.character(.data[[atc_col]]))),
      ATC5_Kode_Tekst = str_trim(as.character(.data[[pbi_col]]))
    ) |>
    filter(!is.na(ATC), ATC != "", !is.na(ATC5_Kode_Tekst), ATC5_Kode_Tekst != "") |>
    distinct()
}

apply_atc_mapping <- function(atc_values, mapping = NULL) {
  if (length(atc_values) == 0) return(character(0))

  already_powerbi <- str_detect(atc_values, "\\(.+\\)")
  raw_codes <- clean_atc_code_for_mapping(atc_values)

  if (is.null(mapping) || nrow(mapping) == 0) {
    return(unique(ifelse(already_powerbi, atc_values, raw_codes)))
  }

  mapped <- tibble(
    input = atc_values,
    ATC = raw_codes,
    already_powerbi = already_powerbi
  ) |>
    left_join(mapping, by = "ATC") |>
    mutate(
      powerbi_atc = case_when(
        already_powerbi ~ input,
        !is.na(ATC5_Kode_Tekst) & ATC5_Kode_Tekst != "" ~ ATC5_Kode_Tekst,
        TRUE ~ ATC
      )
    ) |>
    pull(powerbi_atc)

  unique(mapped[!is.na(mapped) & mapped != ""])
}

js_string_array <- function(x) {
  jsonlite::toJSON(as.character(x), auto_unbox = FALSE)
}

js_numeric_array <- function(x) {
  paste0("[", paste(as.integer(x), collapse = ", "), "]")
}

generate_js <- function(atc_powerbi, metrics, regions, years, split_by_year) {
  atc_js <- js_string_array(atc_powerbi)
  metrics_js <- js_string_array(metrics)
  regions_js <- js_string_array(regions)
  years_js <- js_numeric_array(years)
  split_js <- if (isTRUE(split_by_year)) "true" else "false"

  # JavaScript-template er base64-indlejret for at undgå R/editor-problemer med
  # anførselstegn, backslashes, regulære udtryk og klammer i en stor JS-string.
  js_b64 <- paste0(
"Ci8vIFBvd2VyIEJJIE1lZGljaW5zYWxnIGkgcHJpbcOmcnNla3RvcmVuIC0gZGlyZWt0ZSBDU1Yt",
    "ZXhwb3J0Ci8vIEvDuHIgaSBDaHJvbWUgRGV2VG9vbHMgQ29uc29sZSwgbWVucyBQb3dlciBCSS1y",
    "YXBwb3J0ZW4gZXIgw6ViZW4uCi8vIE91dHB1dCBkb3dubG9hZGVzIHNvbSBlbiBlbGxlciBmbGVy",
    "ZSBmaWxlcjogbWVkaWNpbnNhbGdfcGFyc2VkX3BhcnRfMDAxLmNzdiwgLi4uICsgbWVkaWNpbnNh",
    "bGdfZXhwb3J0X3N1bW1hcnkuanNvbgoKKGFzeW5jICgpID0+IHsKICBjb25zdCB1cmwgPSAiaHR0",
    "cHM6Ly9wb3dlcmJpLmVzdW5kaGVkLmRrL3Bvd2VyYmkvYXBpL2V4cGxvcmUvcmVwb3J0cy9lNzRh",
    "ODM3ZC00NjJmLTQ3MjQtYWY5ZS0yOTAyNzA3ODkyYzcvcXVlcnlkYXRhP3N5bmNocm9ub3VzPXRy",
    "dWUiOwoKICBjb25zdCBzZWxlY3RlZEF0YyA9IF9fQVRDX0pTX187CiAgY29uc3Qgc2VsZWN0ZWRN",
    "ZXRyaWNzID0gX19NRVRSSUNTX0pTX187CiAgY29uc3Qgc2VsZWN0ZWRSZWdpb25zID0gX19SRUdJ",
    "T05TX0pTX187CiAgY29uc3Qgc2VsZWN0ZWRZZWFycyA9IF9fWUVBUlNfSlNfXzsKICBjb25zdCBz",
    "cGxpdEJ5WWVhciA9IF9fU1BMSVRfSlNfXzsKCiAgLy8gSG9sZCBodmVyIGZpbCBtb2RlcmF0IGkg",
    "c3TDuHJyZWxzZSwgc8OlIGRlIGthbiB1cGxvYWRlcyBlbmtlbHR2aXMgYmFnZWZ0ZXIsIGh2aXMg",
    "w7huc2tldC4KICBjb25zdCBtYXhSb3dzUGVyQ3N2ID0gNTAwMDA7CgogIGNvbnN0IG1ldHJpY01h",
    "cCA9IHsKICAgICJNw6ZuZ2RlZm9yYnJ1ZyI6ICJBbnRhbCBEREQiLAogICAgIlJlZ2lvbmFsdCB0",
    "aWxza3VkIjogIlRpbHNrdWRzYmVsw7hiIC0gUmVnaW9uYWx0IiwKICAgICJPbXPDpnRuaW5nIjog",
    "IkVrc3BlZGl0aW9uc2JlbMO4YiIKICB9OwoKICBjb25zdCBoZWFkZXJzID0gWwogICAgIkFUQywg",
    "Tml2ZWF1IDUsIGtvZGUgJiB0ZWtzdCIsCiAgICAiVmFyZW51bW1lciIsCiAgICAiTmF2biAoUHLD",
    "pnBhcmF0KSIsCiAgICAiRm9ybSIsCiAgICAiU3R5cmtlIiwKICAgICJQYWtuaW5nc3N0w7hycmVs",
    "c2UiLAogICAgIsOFciIsCiAgICAiTcOlbmVkIiwKICAgICJZZGVydHlwZSIsCiAgICAiQm9ww6Zs",
    "c3JlZ2lvbiIsCiAgICAibsO4Z2xldGFsX291dHB1dCIsCiAgICAidmFsdWUiLAogICAgImF0Y19m",
    "aWx0ZXIiLAogICAgInJlZ2lvbl9maWx0ZXIiLAogICAgInllYXJfZmlsdGVyIiwKICAgICJvcGdv",
    "ZXJlbHNlIiwKICAgICJzdGF0dXMiLAogICAgInJvd0NvdW50TWFya2VyIiwKICAgICJoYXNSZXN0",
    "YXJ0VG9rZW4iLAogICAgImV4cG9ydF9lcnJvciIKICBdOwoKICBmdW5jdGlvbiBsaXRlcmFsVmFs",
    "dWUodikgewogICAgaWYgKHR5cGVvZiB2ID09PSAibnVtYmVyIikgcmV0dXJuIGAke3Z9TGA7CiAg",
    "ICBjb25zdCBzID0gU3RyaW5nKHYpLnJlcGxhY2VBbGwoIlxcIiwgIlxcXFwiKS5yZXBsYWNlQWxs",
    "KCInIiwgIlxcJyIpOwogICAgcmV0dXJuIGAnJHtzfSdgOwogIH0KCiAgZnVuY3Rpb24gaW5GaWx0",
    "ZXIoc291cmNlLCBwcm9wZXJ0eSwgdmFsdWVzKSB7CiAgICByZXR1cm4gewogICAgICBDb25kaXRp",
    "b246IHsKICAgICAgICBJbjogewogICAgICAgICAgRXhwcmVzc2lvbnM6IFt7IENvbHVtbjogeyBF",
    "eHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6IHNvdXJjZSB9IH0sIFByb3BlcnR5OiBw",
    "cm9wZXJ0eSB9IH1dLAogICAgICAgICAgVmFsdWVzOiB2YWx1ZXMubWFwKHYgPT4gW3sgTGl0ZXJh",
    "bDogeyBWYWx1ZTogbGl0ZXJhbFZhbHVlKHYpIH0gfV0pCiAgICAgICAgfQogICAgICB9CiAgICB9",
    "OwogIH0KCiAgZnVuY3Rpb24gc3RhcnRzV2l0aEZpbHRlcihzb3VyY2UsIHByb3BlcnR5LCB2YWx1",
    "ZSkgewogICAgcmV0dXJuIHsKICAgICAgQ29uZGl0aW9uOiB7CiAgICAgICAgU3RhcnRzV2l0aDog",
    "ewogICAgICAgICAgTGVmdDogeyBDb2x1bW46IHsgRXhwcmVzc2lvbjogeyBTb3VyY2VSZWY6IHsg",
    "U291cmNlOiBzb3VyY2UgfSB9LCBQcm9wZXJ0eTogcHJvcGVydHkgfSB9LAogICAgICAgICAgUmln",
    "aHQ6IHsgTGl0ZXJhbDogeyBWYWx1ZTogbGl0ZXJhbFZhbHVlKHZhbHVlKSB9IH0KICAgICAgICB9",
    "CiAgICAgIH0KICAgIH07CiAgfQoKICBmdW5jdGlvbiBhdGNUZXh0RmlsdGVyKHNvdXJjZSwgcHJv",
    "cGVydHksIHZhbHVlKSB7CiAgICBjb25zdCBzID0gU3RyaW5nKHZhbHVlIHx8ICIiKS50cmltKCk7",
    "CiAgICAvLyBGdWxkIFBvd2VyIEJJLXbDpnJkaTogZWtzYWt0IG1hdGNoLiBSZW4gQVRDLWtvZGU6",
    "IHByZWZpeC1tYXRjaCBww6UgQVRDNV9Lb2RlX1Rla3N0LgogICAgaWYgKHMuaW5jbHVkZXMoIigi",
    "KSAmJiBzLmluY2x1ZGVzKCIpIikpIHJldHVybiBpbkZpbHRlcihzb3VyY2UsIHByb3BlcnR5LCBb",
    "c10pOwogICAgcmV0dXJuIHN0YXJ0c1dpdGhGaWx0ZXIoc291cmNlLCBwcm9wZXJ0eSwgcy50b1Vw",
    "cGVyQ2FzZSgpKTsKICB9CgogIGZ1bmN0aW9uIGJ1aWxkUGF5bG9hZCh7IGF0Y1ZhbHVlLCBtZXRy",
    "aWNWYWx1ZXMsIHJlZ2lvblZhbHVlcywgeWVhclZhbHVlcyB9KSB7CiAgICByZXR1cm4gewogICAg",
    "ICB2ZXJzaW9uOiAiMS4wLjAiLAogICAgICBxdWVyaWVzOiBbewogICAgICAgIFF1ZXJ5OiB7CiAg",
    "ICAgICAgICBDb21tYW5kczogW3sKICAgICAgICAgICAgU2VtYW50aWNRdWVyeURhdGFTaGFwZUNv",
    "bW1hbmQ6IHsKICAgICAgICAgICAgICBRdWVyeTogewogICAgICAgICAgICAgICAgVmVyc2lvbjog",
    "MiwKICAgICAgICAgICAgICAgIEZyb206IFsKICAgICAgICAgICAgICAgICAgeyBOYW1lOiAiIyIs",
    "IEVudGl0eTogIiMgTWVhc3VyZXMiLCBUeXBlOiAwIH0sCiAgICAgICAgICAgICAgICAgIHsgTmFt",
    "ZTogImxtIiwgRW50aXR5OiAiRGltTGFlZ2VtaWRkZWwiLCBUeXBlOiAwIH0sCiAgICAgICAgICAg",
    "ICAgICAgIHsgTmFtZTogImRhdG8iLCBFbnRpdHk6ICJEaW1EYXRvIiwgVHlwZTogMCB9LAogICAg",
    "ICAgICAgICAgICAgICB7IE5hbWU6ICJ1ZCIsIEVudGl0eTogIkRpbVVkc3RlZGVyVHlwZSIsIFR5",
    "cGU6IDAgfSwKICAgICAgICAgICAgICAgICAgeyBOYW1lOiAib3BnIiwgRW50aXR5OiAiT3Bnw7hy",
    "ZWxzZSIsIFR5cGU6IDAgfSwKICAgICAgICAgICAgICAgICAgeyBOYW1lOiAia3JvbiIsIEVudGl0",
    "eTogIktyb25pc2tlIHN5Z2RvbW1lIiwgVHlwZTogMCB9LAogICAgICAgICAgICAgICAgICB7IE5h",
    "bWU6ICJnZW8iLCBFbnRpdHk6ICJEaW1Cb3JnZXJEZW1vZ3JhZmkiLCBUeXBlOiAwIH0sCiAgICAg",
    "ICAgICAgICAgICAgIHsgTmFtZTogInZvbCIsIEVudGl0eTogIkRpbVZvbHVtZSIsIFR5cGU6IDAg",
    "fQogICAgICAgICAgICAgICAgXSwKICAgICAgICAgICAgICAgIFNlbGVjdDogWwogICAgICAgICAg",
    "ICAgICAgICB7IENvbHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6ICJs",
    "bSIgfSB9LCBQcm9wZXJ0eTogIkFUQzVfS29kZV9UZWtzdCIgfSwgTmFtZTogIkRpbUxhZWdlbWlk",
    "ZGVsLkFUQzVfS29kZV9UZWtzdCIgfSwKICAgICAgICAgICAgICAgICAgeyBDb2x1bW46IHsgRXhw",
    "cmVzc2lvbjogeyBTb3VyY2VSZWY6IHsgU291cmNlOiAibG0iIH0gfSwgUHJvcGVydHk6ICJMYWVn",
    "ZW1pZGRlbHRla3N0X3NhbWxldF91bmF2bmUiIH0sIE5hbWU6ICJEaW1MYWVnZW1pZGRlbC5MYWVn",
    "ZW1pZGRlbHRla3N0X3NhbWxldF91bmF2bmUiIH0sCiAgICAgICAgICAgICAgICAgIHsgQ29sdW1u",
    "OiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTogImRhdG8iIH0gfSwgUHJvcGVy",
    "dHk6ICLDhXJfSUQiIH0sIE5hbWU6ICJEaW1EYXRvLsOFcl9JRCIgfSwKICAgICAgICAgICAgICAg",
    "ICAgeyBDb2x1bW46IHsgRXhwcmVzc2lvbjogeyBTb3VyY2VSZWY6IHsgU291cmNlOiAiZGF0byIg",
    "fSB9LCBQcm9wZXJ0eTogIk3DpW5lZMOFciIgfSwgTmFtZTogIkRpbURhdG8uTcOlbmVkw4VyIiB9",
    "LAogICAgICAgICAgICAgICAgICB7IENvbHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjog",
    "eyBTb3VyY2U6ICJ1ZCIgfSB9LCBQcm9wZXJ0eTogIlVkc3RlZGVyX3R5cGUiIH0sIE5hbWU6ICJE",
    "aW1VZHN0ZWRlclR5cGUuVWRzdGVkZXJfdHlwZSIgfSwKICAgICAgICAgICAgICAgICAgeyBDb2x1",
    "bW46IHsgRXhwcmVzc2lvbjogeyBTb3VyY2VSZWY6IHsgU291cmNlOiAiZ2VvIiB9IH0sIFByb3Bl",
    "cnR5OiAiQm9ww6Zsc3JlZ2lvbiIgfSwgTmFtZTogIkRpbUJvcmdlckRlbW9ncmFmaS5Cb3DDpmxz",
    "cmVnaW9uIiB9LAogICAgICAgICAgICAgICAgICB7IE1lYXN1cmU6IHsgRXhwcmVzc2lvbjogeyBT",
    "b3VyY2VSZWY6IHsgU291cmNlOiAiIyIgfSB9LCBQcm9wZXJ0eTogIlNlbGVjdE1lYXN1cmUxIiB9",
    "LCBOYW1lOiAiIyBNZWFzdXJlcy5TZWxlY3RNZWFzdXJlMSIgfQogICAgICAgICAgICAgICAgXSwK",
    "ICAgICAgICAgICAgICAgIFdoZXJlOiBbCiAgICAgICAgICAgICAgICAgIGluRmlsdGVyKCJ2b2wi",
    "LCAiVm9sdW1lX3Rla3N0X2dycCIsIFsiREREIl0pLAogICAgICAgICAgICAgICAgICBpbkZpbHRl",
    "cigib3BnIiwgIkNhbGN1bGF0aW9uSXRlbUNvbHVtbiAxIiwgbWV0cmljVmFsdWVzKSwKICAgICAg",
    "ICAgICAgICAgICAgaW5GaWx0ZXIoImRhdG8iLCAiw4VyX0lEIiwgeWVhclZhbHVlcyksCiAgICAg",
    "ICAgICAgICAgICAgIGluRmlsdGVyKCJrcm9uIiwgIkNhbGN1bGF0aW9uSXRlbUNvbHVtbiAxIiwg",
    "WyJJbmdlbiB2YWxnIl0pLAogICAgICAgICAgICAgICAgICBpbkZpbHRlcigiZ2VvIiwgIkJvcMOm",
    "bHNyZWdpb24iLCByZWdpb25WYWx1ZXMpLAogICAgICAgICAgICAgICAgICBhdGNUZXh0RmlsdGVy",
    "KCJsbSIsICJBVEM1X0tvZGVfVGVrc3QiLCBhdGNWYWx1ZSksCiAgICAgICAgICAgICAgICAgIGlu",
    "RmlsdGVyKCJsbSIsICJGbGFnX1QzIiwgWyJKYSJdKQogICAgICAgICAgICAgICAgXSwKICAgICAg",
    "ICAgICAgICAgIE9yZGVyQnk6IFsKICAgICAgICAgICAgICAgICAgeyBEaXJlY3Rpb246IDEsIEV4",
    "cHJlc3Npb246IHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTog",
    "ImxtIiB9IH0sIFByb3BlcnR5OiAiQVRDNV9Lb2RlX1Rla3N0IiB9IH0gfSwKICAgICAgICAgICAg",
    "ICAgICAgeyBEaXJlY3Rpb246IDEsIEV4cHJlc3Npb246IHsgQ29sdW1uOiB7IEV4cHJlc3Npb246",
    "IHsgU291cmNlUmVmOiB7IFNvdXJjZTogImxtIiB9IH0sIFByb3BlcnR5OiAiTGFlZ2VtaWRkZWx0",
    "ZWtzdF9zYW1sZXRfdW5hdm5lIiB9IH0gfSwKICAgICAgICAgICAgICAgICAgeyBEaXJlY3Rpb246",
    "IDEsIEV4cHJlc3Npb246IHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNv",
    "dXJjZTogImRhdG8iIH0gfSwgUHJvcGVydHk6ICLDhXJfSUQiIH0gfSB9LAogICAgICAgICAgICAg",
    "ICAgICB7IERpcmVjdGlvbjogMSwgRXhwcmVzc2lvbjogeyBDb2x1bW46IHsgRXhwcmVzc2lvbjog",
    "eyBTb3VyY2VSZWY6IHsgU291cmNlOiAiZGF0byIgfSB9LCBQcm9wZXJ0eTogIk3DpW5lZMOFciIg",
    "fSB9IH0sCiAgICAgICAgICAgICAgICAgIHsgRGlyZWN0aW9uOiAxLCBFeHByZXNzaW9uOiB7IENv",
    "bHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6ICJ1ZCIgfSB9LCBQcm9w",
    "ZXJ0eTogIlVkc3RlZGVyX3R5cGUiIH0gfSB9LAogICAgICAgICAgICAgICAgICB7IERpcmVjdGlv",
    "bjogMSwgRXhwcmVzc2lvbjogeyBDb2x1bW46IHsgRXhwcmVzc2lvbjogeyBTb3VyY2VSZWY6IHsg",
    "U291cmNlOiAiZ2VvIiB9IH0sIFByb3BlcnR5OiAiQm9ww6Zsc3JlZ2lvbiIgfSB9IH0KICAgICAg",
    "ICAgICAgICAgIF0KICAgICAgICAgICAgICB9LAogICAgICAgICAgICAgIEJpbmRpbmc6IHsKICAg",
    "ICAgICAgICAgICAgIFByaW1hcnk6IHsgR3JvdXBpbmdzOiBbeyBQcm9qZWN0aW9uczogWzAsIDEs",
    "IDIsIDMsIDQsIDUsIDZdIH1dIH0sCiAgICAgICAgICAgICAgICBEYXRhUmVkdWN0aW9uOiB7IERh",
    "dGFWb2x1bWU6IDQsIFByaW1hcnk6IHsgV2luZG93OiB7IENvdW50OiAzMDAwMCB9IH0gfSwKICAg",
    "ICAgICAgICAgICAgIFZlcnNpb246IDEKICAgICAgICAgICAgICB9LAogICAgICAgICAgICAgIEV4",
    "ZWN1dGlvbk1ldHJpY3NLaW5kOiAxCiAgICAgICAgICAgIH0KICAgICAgICAgIH1dCiAgICAgICAg",
    "fSwKICAgICAgICBRdWVyeUlkOiAiIgogICAgICB9XSwKICAgICAgY2FuY2VsUXVlcmllczogW10s",
    "CiAgICAgIG1vZGVsSWQ6ICI1NzUzMTk1MyIsCiAgICAgIHVzZXJQcmVmZXJyZWRMb2NhbGU6ICJk",
    "YS1ESyIKICAgIH07CiAgfQoKICBmdW5jdGlvbiBwYXJzZUxpdGVyYWwoeCkgewogICAgaWYgKHgg",
    "PT0gbnVsbCkgcmV0dXJuICIiOwogICAgaWYgKHR5cGVvZiB4ID09PSAib2JqZWN0IikgewogICAg",
    "ICBpZiAoeC5MaXRlcmFsICYmIHguTGl0ZXJhbC5WYWx1ZSAhPSBudWxsKSByZXR1cm4gcGFyc2VM",
    "aXRlcmFsKHguTGl0ZXJhbC5WYWx1ZSk7CiAgICAgIGlmICh4LlZhbHVlICE9IG51bGwpIHJldHVy",
    "biBwYXJzZUxpdGVyYWwoeC5WYWx1ZSk7CiAgICAgIGlmICh4LlYgIT0gbnVsbCkgcmV0dXJuIHBh",
    "cnNlTGl0ZXJhbCh4LlYpOwogICAgICByZXR1cm4gIiI7CiAgICB9CiAgICByZXR1cm4gU3RyaW5n",
    "KHgpLnJlcGxhY2UoL14nfCckL2csICIiKS5yZXBsYWNlKC9eXFx1MDAyN3xcXHUwMDI3JC9nLCAi",
    "Iik7CiAgfQoKICBmdW5jdGlvbiB3YWxrKG9iaiwgZm4pIHsKICAgIGlmIChvYmogPT0gbnVsbCB8",
    "fCB0eXBlb2Ygb2JqICE9PSAib2JqZWN0IikgcmV0dXJuOwogICAgZm4ob2JqKTsKICAgIGlmIChB",
    "cnJheS5pc0FycmF5KG9iaikpIHsKICAgICAgZm9yIChjb25zdCB2IG9mIG9iaikgd2Fsayh2LCBm",
    "bik7CiAgICB9IGVsc2UgewogICAgICBmb3IgKGNvbnN0IGsgb2YgT2JqZWN0LmtleXMob2JqKSkg",
    "d2FsayhvYmpba10sIGZuKTsKICAgIH0KICB9CgogIGZ1bmN0aW9uIGV4dHJhY3REaWN0aW9uYXJp",
    "ZXMocm9vdCkgewogICAgY29uc3QgZGljdHMgPSB7fTsKICAgIGZ1bmN0aW9uIGlubmVyKHgsIHBh",
    "dGgpIHsKICAgICAgaWYgKHggPT0gbnVsbCB8fCB0eXBlb2YgeCAhPT0gIm9iamVjdCIpIHJldHVy",
    "bjsKICAgICAgaWYgKCFBcnJheS5pc0FycmF5KHgpKSB7CiAgICAgICAgZm9yIChjb25zdCBbaywg",
    "dl0gb2YgT2JqZWN0LmVudHJpZXMoeCkpIHsKICAgICAgICAgIGlmICgvXkQoTnxbMC05XSspJC8u",
    "dGVzdChrKSAmJiB2ICE9IG51bGwgJiYgdHlwZW9mIHYgPT09ICJvYmplY3QiKSB7CiAgICAgICAg",
    "ICAgIGNvbnN0IHZhbHMgPSBBcnJheS5pc0FycmF5KHYpID8gdiA6IE9iamVjdC52YWx1ZXModik7",
    "CiAgICAgICAgICAgIGRpY3RzW1suLi5wYXRoLCBrXS5qb2luKCIuIildID0gdmFscy5tYXAocGFy",
    "c2VMaXRlcmFsKTsKICAgICAgICAgIH0KICAgICAgICB9CiAgICAgICAgZm9yIChjb25zdCBbaywg",
    "dl0gb2YgT2JqZWN0LmVudHJpZXMoeCkpIGlubmVyKHYsIFsuLi5wYXRoLCBrXSk7CiAgICAgIH0g",
    "ZWxzZSB7CiAgICAgICAgeC5mb3JFYWNoKCh2LCBpKSA9PiBpbm5lcih2LCBbLi4ucGF0aCwgU3Ry",
    "aW5nKGkpXSkpOwogICAgICB9CiAgICB9CiAgICBpbm5lcihyb290LCBbXSk7CiAgICByZXR1cm4g",
    "ZGljdHM7CiAgfQoKICBmdW5jdGlvbiByZXNvbHZlRGljdFJlZihyZWYsIGRpY3RzLCBwcmVmZXJy",
    "ZWRJbmRleCwgc3RyaWN0UHJlZmVycmVkID0gZmFsc2UpIHsKICAgIGNvbnN0IGlkeCA9IE51bWJl",
    "cihyZWYpOwogICAgaWYgKCFOdW1iZXIuaXNGaW5pdGUoaWR4KSkgcmV0dXJuICIiOwogICAgY29u",
    "c3Qga2V5cyA9IE9iamVjdC5rZXlzKGRpY3RzKTsKICAgIGlmIChwcmVmZXJyZWRJbmRleCAhPSBu",
    "dWxsKSB7CiAgICAgIGNvbnN0IHByZWZlcnJlZEtleXMgPSBrZXlzLmZpbHRlcihrID0+IGsuZW5k",
    "c1dpdGgoYC5EJHtwcmVmZXJyZWRJbmRleH1gKSB8fCBrID09PSBgRCR7cHJlZmVycmVkSW5kZXh9",
    "YCk7CiAgICAgIGZvciAoY29uc3QgayBvZiBwcmVmZXJyZWRLZXlzKSB7CiAgICAgICAgaWYgKGlk",
    "eCArIDEgPD0gZGljdHNba10ubGVuZ3RoKSByZXR1cm4gZGljdHNba11baWR4XTsKICAgICAgfQog",
    "ICAgICBpZiAoc3RyaWN0UHJlZmVycmVkKSByZXR1cm4gIiI7CiAgICB9CiAgICBmb3IgKGNvbnN0",
    "IGsgb2Yga2V5cy5maWx0ZXIoayA9PiBrLmVuZHNXaXRoKCIuRE4iKSB8fCBrID09PSAiRE4iKSkg",
    "ewogICAgICBpZiAoaWR4ICsgMSA8PSBkaWN0c1trXS5sZW5ndGgpIHJldHVybiBkaWN0c1trXVtp",
    "ZHhdOwogICAgfQogICAgZm9yIChjb25zdCBrIG9mIGtleXMpIHsKICAgICAgaWYgKGlkeCArIDEg",
    "PD0gZGljdHNba10ubGVuZ3RoKSByZXR1cm4gZGljdHNba11baWR4XTsKICAgIH0KICAgIHJldHVy",
    "biAiIjsKICB9CgogIGZ1bmN0aW9uIHJlc29sdmVSYXdWYWx1ZUlmVGV4dERpbWVuc2lvbihjZWxs",
    "LCBkaWN0cywgY29sSW5kZXgpIHsKICAgIGlmIChjZWxsID09IG51bGwgfHwgY2VsbC5WID09IG51",
    "bGwgfHwgY29sSW5kZXggPT0gbnVsbCB8fCBjb2xJbmRleCA+IDUpIHJldHVybiAiIjsKICAgIGNv",
    "bnN0IHJhdyA9IFN0cmluZyhjZWxsLlYpOwogICAgaWYgKCEvXlxkKyQvLnRlc3QocmF3KSkgcmV0",
    "dXJuICIiOwogICAgcmV0dXJuIHJlc29sdmVEaWN0UmVmKGNlbGwuViwgZGljdHMsIGNvbEluZGV4",
    "LCB0cnVlKTsKICB9CgogIGZ1bmN0aW9uIGV4dHJhY3RSb3dzKHJvb3QpIHsKICAgIGNvbnN0IHJv",
    "d3MgPSBbXTsKICAgIHdhbGsocm9vdCwgeCA9PiB7CiAgICAgIGlmICh4ICYmIHR5cGVvZiB4ID09",
    "PSAib2JqZWN0IiAmJiBBcnJheS5pc0FycmF5KHguQykpIHJvd3MucHVzaCh4KTsKICAgIH0pOwog",
    "ICAgcmV0dXJuIHJvd3M7CiAgfQoKICBmdW5jdGlvbiB2YWx1ZUZyb21DZWxsKGNlbGwsIGRpY3Rz",
    "LCBjb2xJbmRleCkgewogICAgaWYgKGNlbGwgPT0gbnVsbCkgcmV0dXJuICIiOwogICAgaWYgKHR5",
    "cGVvZiBjZWxsICE9PSAib2JqZWN0IikgcmV0dXJuIFN0cmluZyhjZWxsKTsKCiAgICAvLyBQb3dl",
    "ciBCSS1rb21wcmltZXJlZGUgc3ZhciBicnVnZXIgb2Z0ZSBjZWxsLlYgc29tIHLDpSBkaWN0aW9u",
    "YXJ5LWluZGVrcwogICAgLy8gc2FtbWVuIG1lZCBEL0QwL0QxLy4uLiAuIERpY3Rpb25hcnktcmVm",
    "ZXJlbmNlbiBza2FsIGRlcmZvciBsw6ZzZXMgZsO4ciBWLAogICAgLy8gZWxsZXJzIGVuZGVyIEFU",
    "Qywgw6VyLCB5ZGVydHlwZSBvc3YuIHNvbSAwLzEvMiBpIGVrc3BvcnRlbi4KICAgIGZvciAoY29u",
    "c3Qga2V5IG9mIFsiRE4iLCAiRCIsICJEMCIsICJEMSIsICJEMiIsICJEMyIsICJENCIsICJENSIs",
    "ICJENiIsICJENyIsICJEOCIsICJEOSJdKSB7CiAgICAgIGlmIChjZWxsW2tleV0gIT0gbnVsbCkg",
    "ewogICAgICAgIGNvbnN0IG0gPSAvXkQoXGQrKSQvLmV4ZWMoa2V5KTsKICAgICAgICBjb25zdCBw",
    "cmVmZXJyZWQgPSBtID8gTnVtYmVyKG1bMV0pIDogY29sSW5kZXg7CiAgICAgICAgcmV0dXJuIHJl",
    "c29sdmVEaWN0UmVmKGNlbGxba2V5XSwgZGljdHMsIHByZWZlcnJlZCk7CiAgICAgIH0KICAgIH0K",
    "CiAgICBpZiAoY2VsbC5WYWx1ZSAhPSBudWxsKSByZXR1cm4gcGFyc2VMaXRlcmFsKGNlbGwuVmFs",
    "dWUpOwogICAgaWYgKGNlbGwuTGl0ZXJhbCAmJiBjZWxsLkxpdGVyYWwuVmFsdWUgIT0gbnVsbCkg",
    "cmV0dXJuIHBhcnNlTGl0ZXJhbChjZWxsLkxpdGVyYWwuVmFsdWUpOwoKICAgIGNvbnN0IHJlc29s",
    "dmVkUmF3ID0gcmVzb2x2ZVJhd1ZhbHVlSWZUZXh0RGltZW5zaW9uKGNlbGwsIGRpY3RzLCBjb2xJ",
    "bmRleCk7CiAgICBpZiAocmVzb2x2ZWRSYXcgIT09ICIiKSByZXR1cm4gcmVzb2x2ZWRSYXc7Cgog",
    "ICAgaWYgKGNlbGwuViAhPSBudWxsKSByZXR1cm4gcGFyc2VMaXRlcmFsKGNlbGwuVik7CgogICAg",
    "Y29uc3QgdmFscyA9IE9iamVjdC52YWx1ZXMoY2VsbCk7CiAgICBpZiAodmFscy5sZW5ndGggPT09",
    "IDEpIHJldHVybiB2YWx1ZUZyb21DZWxsKHZhbHNbMF0sIGRpY3RzLCBjb2xJbmRleCk7CiAgICBy",
    "ZXR1cm4gIiI7CiAgfQoKICBmdW5jdGlvbiByb3dSZXVzZU1hc2socm93KSB7CiAgICBjb25zdCBy",
    "ID0gTnVtYmVyKHJvdy5SKTsKICAgIGlmICghTnVtYmVyLmlzRmluaXRlKHIpKSByZXR1cm4gW107",
    "CiAgICBjb25zdCBvdXQgPSBbXTsKICAgIGZvciAobGV0IGkgPSAwOyBpIDwgMzI7IGkrKykgaWYg",
    "KChyICYgKDEgPDwgaSkpICE9PSAwKSBvdXQucHVzaChpKTsKICAgIHJldHVybiBvdXQ7CiAgfQoK",
    "ICBmdW5jdGlvbiBwYXJzZVBiaU51bWJlcih4KSB7CiAgICBpZiAoeCA9PSBudWxsIHx8IHggPT09",
    "ICIiKSByZXR1cm4gMDsKICAgIGlmICh0eXBlb2YgeCA9PT0gIm51bWJlciIpIHJldHVybiB4Owog",
    "ICAgY29uc3QgcyA9IFN0cmluZyh4KS5yZXBsYWNlKC9EJC9nLCAiIikucmVwbGFjZSgvXHMvZywg",
    "IiIpLnJlcGxhY2UoIiwiLCAiLiIpOwogICAgY29uc3QgbiA9IE51bWJlcihzKTsKICAgIHJldHVy",
    "biBOdW1iZXIuaXNGaW5pdGUobikgPyBuIDogMDsKICB9CgogIGZ1bmN0aW9uIGZvcm1hdEF0YzUo",
    "eCkgewogICAgcmV0dXJuIFN0cmluZyh4IHx8ICIiKS5yZXBsYWNlKC9eKFtBLVpdWzAtOV17Mn1b",
    "QS1aXXsxLDJ9WzAtOV17MCw0fSlccypcKCguKylcKSQvLCAiJDEgLSAkMiIpOwogIH0KCgogIGZ1",
    "bmN0aW9uIG5vcm1hbGlzZU1vbnRoTGFiZWwoeCkgewogICAgY29uc3QgcmF3ID0gU3RyaW5nKHgg",
    "PT0gbnVsbCA/ICIiIDogeCkudHJpbSgpLnJlcGxhY2UoL0QkL2csICIiKTsKICAgIGlmIChyYXcg",
    "PT09ICIiKSByZXR1cm4gIiI7CiAgICBjb25zdCBtb250aE5hbWVzID0gWyJKYW51YXIiLCAiRmVi",
    "cnVhciIsICJNYXJ0cyIsICJBcHJpbCIsICJNYWoiLCAiSnVuaSIsICJKdWxpIiwgIkF1Z3VzdCIs",
    "ICJTZXB0ZW1iZXIiLCAiT2t0b2JlciIsICJOb3ZlbWJlciIsICJEZWNlbWJlciJdOwogICAgaWYg",
    "KC9eXGQrJC8udGVzdChyYXcpKSB7CiAgICAgIGNvbnN0IG4gPSBOdW1iZXIocmF3KTsKICAgICAg",
    "aWYgKE51bWJlci5pc0Zpbml0ZShuKSAmJiBuID49IDApIHJldHVybiBtb250aE5hbWVzW24gJSAx",
    "Ml07CiAgICB9CiAgICByZXR1cm4gcmF3OwogIH0KCiAgZnVuY3Rpb24gbm9ybWFsaXNlWWRlcnR5",
    "cGUoeCkgewogICAgY29uc3QgcmF3ID0gU3RyaW5nKHggPT0gbnVsbCA/ICIiIDogeCkudHJpbSgp",
    "LnJlcGxhY2UoL0QkL2csICIiKTsKICAgIGNvbnN0IG1hcCA9IHsKICAgICAgIjAiOiAiQWxtZW4g",
    "cHJha3NpcyIsCiAgICAgICIxIjogIkhvc3BpdGFsc2zDpmdlIiwKICAgICAgIjIiOiAiw5h2cmln",
    "ZSB1ZHN0ZWRlcmUiLAogICAgICAiMyI6ICJTcGVjaWFsbMOmZ2UiLAogICAgICAiNCI6ICJVa2Vu",
    "ZHQiCiAgICB9OwogICAgcmV0dXJuIG1hcFtyYXddIHx8IHJhdzsKICB9CgogIGZ1bmN0aW9uIHNw",
    "bGl0TGFlZ2VtaWRkZWx0ZWtzdCh4KSB7CiAgICBjb25zdCBwYXJ0cyA9IFN0cmluZyh4IHx8ICIi",
    "KS5zcGxpdCgvXHMqXHxccyovKTsKICAgIHdoaWxlIChwYXJ0cy5sZW5ndGggPCA1KSBwYXJ0cy5w",
    "dXNoKCIiKTsKICAgIHJldHVybiB7CiAgICAgIHZhcmVudW1tZXI6IHBhcnRzWzBdIHx8ICIiLAog",
    "ICAgICBuYXZuOiBwYXJ0c1sxXSB8fCAiIiwKICAgICAgZm9ybTogcGFydHNbMl0gfHwgIiIsCiAg",
    "ICAgIHN0eXJrZTogcGFydHNbM10gfHwgIiIsCiAgICAgIHBha25pbmc6IHBhcnRzLnNsaWNlKDQp",
    "LmpvaW4oIiB8ICIpIHx8ICIiCiAgICB9OwogIH0KCiAgZnVuY3Rpb24gcGFyc2VSZXNwb25zZVRv",
    "Um93cyhyZXNwb25zZSwgbWV0YSkgewogICAgaWYgKCFyZXNwb25zZSkgcmV0dXJuIFtdOwogICAg",
    "Y29uc3QgZGljdHMgPSBleHRyYWN0RGljdGlvbmFyaWVzKHJlc3BvbnNlKTsKICAgIGNvbnN0IHBi",
    "aVJvd3MgPSBleHRyYWN0Um93cyhyZXNwb25zZSk7CiAgICBjb25zdCBvdXQgPSBbXTsKICAgIGxl",
    "dCBwcmV2aW91cyA9IFsiIiwgIiIsICIiLCAiIiwgIiIsICIiLCAiIl07CgogICAgZm9yIChjb25z",
    "dCByb3cgb2YgcGJpUm93cykgewogICAgICBjb25zdCBjdXJyZW50ID0gWyIiLCAiIiwgIiIsICIi",
    "LCAiIiwgIiIsICIiXTsKICAgICAgZm9yIChjb25zdCBjb2wwIG9mIHJvd1JldXNlTWFzayhyb3cp",
    "KSB7CiAgICAgICAgaWYgKGNvbDAgPj0gMCAmJiBjb2wwIDwgY3VycmVudC5sZW5ndGgpIGN1cnJl",
    "bnRbY29sMF0gPSBwcmV2aW91c1tjb2wwXTsKICAgICAgfQoKICAgICAgY29uc3QgbWlzc2luZyA9",
    "IFtdOwogICAgICBmb3IgKGxldCBpID0gMDsgaSA8IGN1cnJlbnQubGVuZ3RoOyBpKyspIGlmIChj",
    "dXJyZW50W2ldID09PSAiIikgbWlzc2luZy5wdXNoKGkpOwogICAgICBjb25zdCBjZWxscyA9IEFy",
    "cmF5LmlzQXJyYXkocm93LkMpID8gcm93LkMgOiBbXTsKICAgICAgZm9yIChsZXQgaiA9IDA7IGog",
    "PCBjZWxscy5sZW5ndGg7IGorKykgewogICAgICAgIGNvbnN0IHBvcyA9IGogPCBtaXNzaW5nLmxl",
    "bmd0aCA/IG1pc3Npbmdbal0gOiBqOwogICAgICAgIGlmIChwb3MgPj0gMCAmJiBwb3MgPCBjdXJy",
    "ZW50Lmxlbmd0aCkgY3VycmVudFtwb3NdID0gdmFsdWVGcm9tQ2VsbChjZWxsc1tqXSwgZGljdHMs",
    "IHBvcyk7CiAgICAgIH0KCiAgICAgIHByZXZpb3VzID0gY3VycmVudDsKICAgICAgY29uc3QgbWVk",
    "ID0gc3BsaXRMYWVnZW1pZGRlbHRla3N0KGN1cnJlbnRbMV0pOwogICAgICBvdXQucHVzaCh7CiAg",
    "ICAgICAgIkFUQywgTml2ZWF1IDUsIGtvZGUgJiB0ZWtzdCI6IGZvcm1hdEF0YzUoY3VycmVudFsw",
    "XSksCiAgICAgICAgIlZhcmVudW1tZXIiOiBtZWQudmFyZW51bW1lciwKICAgICAgICAiTmF2biAo",
    "UHLDpnBhcmF0KSI6IG1lZC5uYXZuLAogICAgICAgICJGb3JtIjogbWVkLmZvcm0sCiAgICAgICAg",
    "IlN0eXJrZSI6IG1lZC5zdHlya2UsCiAgICAgICAgIlBha25pbmdzc3TDuHJyZWxzZSI6IG1lZC5w",
    "YWtuaW5nLAogICAgICAgICLDhXIiOiBTdHJpbmcoY3VycmVudFsyXSB8fCBtZXRhLnllYXIgfHwg",
    "IiIpLnJlcGxhY2UoL0QkL2csICIiKSwKICAgICAgICAiTcOlbmVkIjogbm9ybWFsaXNlTW9udGhM",
    "YWJlbChjdXJyZW50WzNdKSwKICAgICAgICAiWWRlcnR5cGUiOiBub3JtYWxpc2VZZGVydHlwZShj",
    "dXJyZW50WzRdKSwKICAgICAgICAiQm9ww6Zsc3JlZ2lvbiI6IGN1cnJlbnRbNV0sCiAgICAgICAg",
    "Im7DuGdsZXRhbF9vdXRwdXQiOiBtZXRyaWNNYXBbbWV0YS5vcGdvZXJlbHNlXSB8fCBtZXRhLm9w",
    "Z29lcmVsc2UsCiAgICAgICAgInZhbHVlIjogcGFyc2VQYmlOdW1iZXIoY3VycmVudFs2XSksCiAg",
    "ICAgICAgImF0Y19maWx0ZXIiOiBtZXRhLmF0YywKICAgICAgICAicmVnaW9uX2ZpbHRlciI6IG1l",
    "dGEucmVnaW9uLAogICAgICAgICJ5ZWFyX2ZpbHRlciI6IEFycmF5LmlzQXJyYXkobWV0YS55ZWFy",
    "KSA/IG1ldGEueWVhci5qb2luKCIsICIpIDogU3RyaW5nKG1ldGEueWVhciksCiAgICAgICAgIm9w",
    "Z29lcmVsc2UiOiBtZXRhLm9wZ29lcmVsc2UsCiAgICAgICAgInN0YXR1cyI6IG1ldGEuc3RhdHVz",
    "LAogICAgICAgICJyb3dDb3VudE1hcmtlciI6IG1ldGEucm93Q291bnRNYXJrZXIsCiAgICAgICAg",
    "Imhhc1Jlc3RhcnRUb2tlbiI6IG1ldGEuaGFzUmVzdGFydFRva2VuLAogICAgICAgICJleHBvcnRf",
    "ZXJyb3IiOiBtZXRhLmVycm9yIHx8ICIiCiAgICAgIH0pOwogICAgfQogICAgcmV0dXJuIG91dDsK",
    "ICB9CgogIGZ1bmN0aW9uIGNzdkVzY2FwZSh2KSB7CiAgICBpZiAodiA9PSBudWxsKSByZXR1cm4g",
    "IiI7CiAgICBpZiAodHlwZW9mIHYgPT09ICJudW1iZXIiKSB2ID0gU3RyaW5nKHYpLnJlcGxhY2Uo",
    "Ii4iLCAiLCIpOwogICAgZWxzZSB2ID0gU3RyaW5nKHYpOwogICAgaWYgKC9bIjtcclxuXS8udGVz",
    "dCh2KSkgdiA9ICciJyArIHYucmVwbGFjZUFsbCgnIicsICciIicpICsgJyInOwogICAgcmV0dXJu",
    "IHY7CiAgfQoKICBmdW5jdGlvbiByb3dzVG9Dc3Yocm93cykgewogICAgY29uc3QgbGluZXMgPSBb",
    "aGVhZGVycy5tYXAoY3N2RXNjYXBlKS5qb2luKCI7IildOwogICAgZm9yIChjb25zdCByIG9mIHJv",
    "d3MpIGxpbmVzLnB1c2goaGVhZGVycy5tYXAoaCA9PiBjc3ZFc2NhcGUocltoXSkpLmpvaW4oIjsi",
    "KSk7CiAgICByZXR1cm4gIlx1ZmVmZiIgKyBsaW5lcy5qb2luKCJcclxuIik7CiAgfQoKICBmdW5j",
    "dGlvbiBkb3dubG9hZFRleHQoZmlsZW5hbWUsIHRleHQsIHR5cGUpIHsKICAgIGNvbnN0IGJsb2Ig",
    "PSBuZXcgQmxvYihbdGV4dF0sIHsgdHlwZTogdHlwZSB8fCAidGV4dC9wbGFpbjtjaGFyc2V0PXV0",
    "Zi04IiB9KTsKICAgIGNvbnN0IGEgPSBkb2N1bWVudC5jcmVhdGVFbGVtZW50KCJhIik7CiAgICBh",
    "LmhyZWYgPSBVUkwuY3JlYXRlT2JqZWN0VVJMKGJsb2IpOwogICAgYS5kb3dubG9hZCA9IGZpbGVu",
    "YW1lOwogICAgZG9jdW1lbnQuYm9keS5hcHBlbmRDaGlsZChhKTsKICAgIGEuY2xpY2soKTsKICAg",
    "IGEucmVtb3ZlKCk7CiAgICBzZXRUaW1lb3V0KCgpID0+IFVSTC5yZXZva2VPYmplY3RVUkwoYS5o",
    "cmVmKSwgNTAwMCk7CiAgfQoKICBhc3luYyBmdW5jdGlvbiBydW5PbmUoeyBhdGMsIHJlZ2lvbiwg",
    "eWVhciwgb3Bnb2VyZWxzZSB9KSB7CiAgICBjb25zdCB5ZWFyVmFsdWVzID0gQXJyYXkuaXNBcnJh",
    "eSh5ZWFyKSA/IHllYXIgOiBbeWVhcl07CiAgICBjb25zdCBwYXlsb2FkID0gYnVpbGRQYXlsb2Fk",
    "KHsgYXRjVmFsdWU6IGF0YywgbWV0cmljVmFsdWVzOiBbb3Bnb2VyZWxzZV0sIHJlZ2lvblZhbHVl",
    "czogW3JlZ2lvbl0sIHllYXJWYWx1ZXMgfSk7CiAgICBsZXQgc3RhdHVzID0gbnVsbCwgdGV4dCA9",
    "ICIiLCBwYXJzZWRKc29uID0gbnVsbCwgZXJyb3IgPSBudWxsOwogICAgdHJ5IHsKICAgICAgY29u",
    "c3QgcmVzcG9uc2UgPSBhd2FpdCBmZXRjaCh1cmwsIHsKICAgICAgICBtZXRob2Q6ICJQT1NUIiwK",
    "ICAgICAgICBjcmVkZW50aWFsczogImluY2x1ZGUiLAogICAgICAgIGhlYWRlcnM6IHsKICAgICAg",
    "ICAgICJhY2NlcHQiOiAiYXBwbGljYXRpb24vanNvbiwgdGV4dC9wbGFpbiwgKi8qIiwKICAgICAg",
    "ICAgICJjb250ZW50LXR5cGUiOiAiYXBwbGljYXRpb24vanNvbjtjaGFyc2V0PVVURi04IiwKICAg",
    "ICAgICAgICJ4LXBvd2VyYmktcmVzb3VyY2VrZXkiOiAiYW55IgogICAgICAgIH0sCiAgICAgICAg",
    "Ym9keTogSlNPTi5zdHJpbmdpZnkocGF5bG9hZCkKICAgICAgfSk7CiAgICAgIHN0YXR1cyA9IHJl",
    "c3BvbnNlLnN0YXR1czsKICAgICAgdGV4dCA9IGF3YWl0IHJlc3BvbnNlLnRleHQoKTsKICAgICAg",
    "dHJ5IHsgcGFyc2VkSnNvbiA9IEpTT04ucGFyc2UodGV4dCk7IH0gY2F0Y2goZSkgeyBlcnJvciA9",
    "ICJLdW5uZSBpa2tlIEpTT04tcGFyc2UgcmVzcG9uc2U6ICIgKyBlLm1lc3NhZ2U7IH0KICAgIH0g",
    "Y2F0Y2goZSkgewogICAgICBlcnJvciA9ICJGZXRjaC1mZWpsOiAiICsgZS5tZXNzYWdlOwogICAg",
    "fQoKICAgIGNvbnN0IHJvd0NvdW50TWFya2VyID0gdGV4dC5zcGxpdCgnIkMiOlsnKS5sZW5ndGgg",
    "LSAxOwogICAgY29uc3QgaGFzUmVzdGFydFRva2VuID0gdGV4dC5pbmNsdWRlcygnIlJUIicpOwog",
    "ICAgY29uc3QgbWV0YSA9IHsgYXRjLCByZWdpb24sIHllYXIsIG9wZ29lcmVsc2UsIHN0YXR1cywg",
    "cm93Q291bnRNYXJrZXIsIGhhc1Jlc3RhcnRUb2tlbiwgZXJyb3IgfTsKICAgIGNvbnN0IHJvd3Mg",
    "PSBwYXJzZVJlc3BvbnNlVG9Sb3dzKHBhcnNlZEpzb24sIG1ldGEpOwogICAgcmV0dXJuIHsgbWV0",
    "YTogeyAuLi5tZXRhLCBwYXJzZWRSb3dzOiByb3dzLmxlbmd0aCB9LCByb3dzIH07CiAgfQoKICBj",
    "b25zdCB0b3RhbCA9IHNlbGVjdGVkQXRjLmxlbmd0aCAqIHNlbGVjdGVkUmVnaW9ucy5sZW5ndGgg",
    "KiBzZWxlY3RlZE1ldHJpY3MubGVuZ3RoICogKHNwbGl0QnlZZWFyID8gc2VsZWN0ZWRZZWFycy5s",
    "ZW5ndGggOiAxKTsKICBjb25zdCBzdW1tYXJ5ID0gW107CiAgbGV0IGRvbmUgPSAwOwogIGxldCBw",
    "YXJ0Tm8gPSAxOwogIGxldCBidWZmZXIgPSBbXTsKCiAgZnVuY3Rpb24gZmx1c2goZm9yY2UpIHsK",
    "ICAgIGlmIChidWZmZXIubGVuZ3RoID09PSAwKSByZXR1cm47CiAgICBpZiAoIWZvcmNlICYmIGJ1",
    "ZmZlci5sZW5ndGggPCBtYXhSb3dzUGVyQ3N2KSByZXR1cm47CiAgICBjb25zdCBmaWxlbmFtZSA9",
    "IGBtZWRpY2luc2FsZ19wYXJzZWRfcGFydF8ke1N0cmluZyhwYXJ0Tm8pLnBhZFN0YXJ0KDMsICIw",
    "Iil9LmNzdmA7CiAgICBkb3dubG9hZFRleHQoZmlsZW5hbWUsIHJvd3NUb0NzdihidWZmZXIpLCAi",
    "dGV4dC9jc3Y7Y2hhcnNldD11dGYtOCIpOwogICAgY29uc29sZS5sb2coYERvd25sb2FkZWRlICR7",
    "ZmlsZW5hbWV9IG1lZCAke2J1ZmZlci5sZW5ndGh9IHLDpmtrZXJgKTsKICAgIHBhcnRObyArPSAx",
    "OwogICAgYnVmZmVyID0gW107CiAgfQoKICBmb3IgKGNvbnN0IGF0YyBvZiBzZWxlY3RlZEF0Yykg",
    "ewogICAgZm9yIChjb25zdCByZWdpb24gb2Ygc2VsZWN0ZWRSZWdpb25zKSB7CiAgICAgIGZvciAo",
    "Y29uc3Qgb3Bnb2VyZWxzZSBvZiBzZWxlY3RlZE1ldHJpY3MpIHsKICAgICAgICBpZiAoc3BsaXRC",
    "eVllYXIpIHsKICAgICAgICAgIGZvciAoY29uc3QgeWVhciBvZiBzZWxlY3RlZFllYXJzKSB7CiAg",
    "ICAgICAgICAgIGRvbmUrKzsKICAgICAgICAgICAgY29uc29sZS5sb2coYFske2RvbmV9LyR7dG90",
    "YWx9XSBIZW50ZXJgLCB7IGF0YywgcmVnaW9uLCBvcGdvZXJlbHNlLCB5ZWFyIH0pOwogICAgICAg",
    "ICAgICBjb25zdCByZXMgPSBhd2FpdCBydW5PbmUoeyBhdGMsIHJlZ2lvbiwgb3Bnb2VyZWxzZSwg",
    "eWVhciB9KTsKICAgICAgICAgICAgc3VtbWFyeS5wdXNoKHJlcy5tZXRhKTsKICAgICAgICAgICAg",
    "YnVmZmVyLnB1c2goLi4ucmVzLnJvd3MpOwogICAgICAgICAgICBmbHVzaChmYWxzZSk7CiAgICAg",
    "ICAgICB9CiAgICAgICAgfSBlbHNlIHsKICAgICAgICAgIGRvbmUrKzsKICAgICAgICAgIGNvbnNv",
    "bGUubG9nKGBbJHtkb25lfS8ke3RvdGFsfV0gSGVudGVyYCwgeyBhdGMsIHJlZ2lvbiwgb3Bnb2Vy",
    "ZWxzZSwgeWVhcjogc2VsZWN0ZWRZZWFycyB9KTsKICAgICAgICAgIGNvbnN0IHJlcyA9IGF3YWl0",
    "IHJ1bk9uZSh7IGF0YywgcmVnaW9uLCBvcGdvZXJlbHNlLCB5ZWFyOiBzZWxlY3RlZFllYXJzIH0p",
    "OwogICAgICAgICAgc3VtbWFyeS5wdXNoKHJlcy5tZXRhKTsKICAgICAgICAgIGJ1ZmZlci5wdXNo",
    "KC4uLnJlcy5yb3dzKTsKICAgICAgICAgIGZsdXNoKGZhbHNlKTsKICAgICAgICB9CiAgICAgIH0K",
    "ICAgIH0KICB9CgogIGZsdXNoKHRydWUpOwogIGRvd25sb2FkVGV4dCgibWVkaWNpbnNhbGdfZXhw",
    "b3J0X3N1bW1hcnkuanNvbiIsIEpTT04uc3RyaW5naWZ5KHN1bW1hcnksIG51bGwsIDIpLCAiYXBw",
    "bGljYXRpb24vanNvbjtjaGFyc2V0PXV0Zi04Iik7CgogIGNvbnN0IHRvdGFsUm93cyA9IHN1bW1h",
    "cnkucmVkdWNlKChzLCB4KSA9PiBzICsgKHgucGFyc2VkUm93cyB8fCAwKSwgMCk7CiAgY29uc3Qg",
    "dHJ1bmNhdGVkID0gc3VtbWFyeS5maWx0ZXIoeCA9PiB4Lmhhc1Jlc3RhcnRUb2tlbiB8fCBOdW1i",
    "ZXIoeC5yb3dDb3VudE1hcmtlcikgPj0gMjkwMDApOwogIGNvbnNvbGUubG9nKGBGw6ZyZGlnLiBQ",
    "YXJzZWRlICR7dG90YWxSb3dzfSByw6Zra2VyIGZvcmRlbHQgcMOlICR7cGFydE5vIC0gMX0gQ1NW",
    "LWZpbChlcikuYCwgc3VtbWFyeSk7CiAgaWYgKHRydW5jYXRlZC5sZW5ndGggPiAwKSBjb25zb2xl",
    "Lndhcm4oIk11bGlndCBhZmtvcnRlZGUgUG93ZXIgQkktc3ZhcjoiLCB0cnVuY2F0ZWQpOwp9KSgp",
    "Ow=="
  )

  js_template <- rawToChar(jsonlite::base64_dec(js_b64))
  js_template <- gsub("__ATC_JS__", atc_js, js_template, fixed = TRUE)
  js_template <- gsub("__METRICS_JS__", metrics_js, js_template, fixed = TRUE)
  js_template <- gsub("__REGIONS_JS__", regions_js, js_template, fixed = TRUE)
  js_template <- gsub("__YEARS_JS__", years_js, js_template, fixed = TRUE)
  js_template <- gsub("__SPLIT_JS__", split_js, js_template, fixed = TRUE)

  js_template
}
parse_pbi_number <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_real_)
  if (is.numeric(x)) return(as.numeric(x)[1])

  x <- as.character(x)[1]
  if (is.na(x) || x == "") return(NA_real_)

  x <- str_remove(x, "D$")
  x <- str_replace_all(x, "\\s", "")
  x <- str_replace(x, ",", ".")

  suppressWarnings(as.numeric(x))
}

parse_literal <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA_character_)

  if (is.list(x)) {
    if (!is.null(x$Literal$Value)) return(parse_literal(x$Literal$Value))
    if (!is.null(x$Value)) return(parse_literal(x$Value))
    return(NA_character_)
  }

  x <- as.character(x)[1]
  x <- str_replace_all(x, "^'|'$", "")
  x <- str_replace_all(x, "^\\u0027|\\u0027$", "")
  x
}

extract_dictionary_values <- function(node) {
  found <- list()

  walk_node <- function(x, path = character()) {
    if (!is.list(x)) return(NULL)

    nms <- names(x) %||% character(0)

    for (nm in nms) {
      val <- x[[nm]]
      if (str_detect(nm, "^D(N|[0-9]+)$") && is.list(val)) {
        flat <- unlist(val, recursive = FALSE)
        if (length(flat) > 0) {
          found[[paste(c(path, nm), collapse = ".")]] <<- map_chr(flat, parse_literal)
        }
      }
    }

    for (nm in nms) {
      walk_node(x[[nm]], c(path, nm))
    }

    NULL
  }

  walk_node(node)
  found
}

resolve_dict_ref <- function(ref, dictionaries, preferred_index = NULL, strict_preferred = FALSE) {
  if (is.null(ref) || length(ref) == 0) return(NA_character_)
  if (length(dictionaries) == 0) return(NA_character_)

  idx <- suppressWarnings(as.integer(ref))
  if (is.na(idx)) return(NA_character_)

  if (!is.null(preferred_index)) {
    preferred_names <- names(dictionaries)[str_detect(names(dictionaries), paste0("\\.D", preferred_index, "$|^D", preferred_index, "$"))]
    for (dn in preferred_names) {
      vals <- dictionaries[[dn]]
      if (idx + 1 <= length(vals)) return(vals[[idx + 1]])
    }
  }

  dn_names <- names(dictionaries)[str_detect(names(dictionaries), "\\.DN$|^DN$")]
  for (dn in dn_names) {
    vals <- dictionaries[[dn]]
    if (idx + 1 <= length(vals)) return(vals[[idx + 1]])
  }

  for (vals in dictionaries) {
    if (idx + 1 <= length(vals)) return(vals[[idx + 1]])
  }

  NA_character_
}

extract_rows_from_node <- function(node) {
  rows <- list()

  walk_node <- function(x) {
    if (!is.list(x)) return(NULL)

    if (!is.null(x$C) && is.list(x$C)) {
      rows[[length(rows) + 1]] <<- x
    }

    for (child in x) {
      walk_node(child)
    }

    NULL
  }

  walk_node(node)
  rows
}

value_from_cell <- function(cell, dictionaries, col_index = NULL) {
  if (is.null(cell) || length(cell) == 0) return(NA)

  if (is.atomic(cell) && !is.list(cell)) {
    return(cell[[1]])
  }

  if (is.list(cell)) {
    # Power BI-komprimerede svar bruger ofte V som rå dictionary-indeks
    # sammen med D/D0/D1/... . Slå dictionary-referencen op før V, så
    # tekstfelter ikke ender som 0, 1, 2 osv. i eksporten.
    dict_keys <- c("DN", "D", paste0("D", 0:9))
    for (key in dict_keys) {
      if (!is.null(cell[[key]])) {
        preferred <- col_index
        if (grepl("^D[0-9]+$", key)) preferred <- as.integer(sub("^D", "", key))
        return(resolve_dict_ref(cell[[key]], dictionaries, preferred_index = preferred))
      }
    }

    if (!is.null(cell$Value)) return(parse_literal(cell$Value))
    if (!is.null(cell$Literal$Value)) return(parse_literal(cell$Literal$Value))

    # Nogle Power BI-svar gemmer tekst-dimensioner som V=<dictionary-indeks>
    # uden en eksplicit D/D0/D1-nøgle i cellen. For de første seks kolonner
    # er V derfor forsøgt slået op som dictionary-reference, før værdien
    # accepteres som råt tal. Målekolonnen (col_index 6) bevares numerisk.
    if (!is.null(cell$V) && !is.null(col_index) && col_index <= 5L) {
      raw_v <- as.character(cell$V)[1]
      if (str_detect(raw_v, "^\\d+$")) {
        resolved_v <- resolve_dict_ref(cell$V, dictionaries, preferred_index = col_index, strict_preferred = TRUE)
        if (!is.na(resolved_v) && nzchar(resolved_v)) return(resolved_v)
      }
    }

    if (!is.null(cell$V)) return(parse_literal(cell$V))

    if (length(cell) == 1) return(value_from_cell(cell[[1]], dictionaries, col_index = col_index))
  }

  NA
}

row_reuse_mask <- function(row) {
  if (is.null(row$R)) return(integer(0))

  r <- row$R
  if (is.character(r)) {
    r <- suppressWarnings(as.integer(r))
  }

  if (!is.numeric(r) || is.na(r)) return(integer(0))

  which(as.logical(intToBits(as.integer(r))[1:32])) - 1L
}

parse_one_response_dsr <- function(response, metadata = list()) {
  if (is.null(response)) return(tibble())

  dictionaries <- extract_dictionary_values(response)
  rows <- extract_rows_from_node(response)

  if (length(rows) == 0) return(tibble())

  previous <- rep(NA_character_, 7)
  parsed <- vector("list", length(rows))

  for (i in seq_along(rows)) {
    row <- rows[[i]]
    cells <- row$C %||% list()

    current <- rep(NA_character_, 7)

    reuse_cols <- row_reuse_mask(row)
    if (length(reuse_cols) > 0) {
      for (col0 in reuse_cols) {
        col <- col0 + 1L
        if (col >= 1 && col <= length(current)) {
          current[col] <- previous[col]
        }
      }
    }

    missing_positions <- which(is.na(current))
    if (length(cells) > 0) {
      for (j in seq_along(cells)) {
        if (j <= length(missing_positions)) {
          pos <- missing_positions[[j]]
        } else if (j <= length(current)) {
          pos <- j
        } else {
          next
        }

        current[pos] <- as.character(value_from_cell(cells[[j]], dictionaries, col_index = pos - 1L))
      }
    }

    previous <- current

    parsed[[i]] <- tibble(
      `ATC, Niveau 5, kode & tekst` = current[1],
      Laegemiddeltekst_samlet_unavne = current[2],
      `År` = str_remove(current[3], "D$"),
      `Måned` = month_label_da(current[4]),
      `Ydertype` = normalise_ydertype(current[5]),
      Bopælsregion = current[6],
      value = parse_pbi_number(current[7])
    )
  }

  bind_rows(parsed) |>
    mutate(
      atc_filter = metadata$atc %||% NA_character_,
      region_filter = metadata$region %||% NA_character_,
      year_filter = paste(metadata$year %||% NA_character_, collapse = ", "),
      opgoerelse = metadata$opgoerelse %||% NA_character_,
      status = metadata$status %||% NA,
      rowCountMarker = metadata$rowCountMarker %||% NA,
      hasRestartToken = metadata$hasRestartToken %||% NA,
      export_error = metadata$error %||% NA_character_
    )
}

split_laegemiddeltekst <- function(x) {
  x <- x %||% NA_character_
  x <- ifelse(is.na(x), "", x)

  parts <- str_split_fixed(x, "\\s*\\|\\s*", 5)

  tibble(
    Varenummer = na_if(parts[, 1], ""),
    `Navn (Præparat)` = na_if(parts[, 2], ""),
    Form = na_if(parts[, 3], ""),
    Styrke = na_if(parts[, 4], ""),
    Pakningsstørrelse = na_if(parts[, 5], "")
  )
}

format_atc5 <- function(x) {
  x <- as.character(x)
  str_replace(x, "^([A-Z][0-9]{2}[A-Z]{1,2}[0-9]{0,4})\\s*\\((.+)\\)$", "\\1 - \\2")
}

metric_map <- c(
  "Mængdeforbrug" = "Antal DDD",
  "Regionalt tilskud" = "Tilskudsbeløb - Regionalt",
  "Omsætning" = "Ekspeditionsbeløb"
)


month_label_da <- function(x) {
  x <- str_trim(as.character(x %||% NA_character_))
  x <- str_remove(x, "D$")
  month_names <- c("Januar", "Februar", "Marts", "April", "Maj", "Juni", "Juli", "August", "September", "Oktober", "November", "December")
  n <- suppressWarnings(as.integer(x))
  case_when(
    is.na(x) | x == "" ~ NA_character_,
    !is.na(n) & n >= 0L ~ month_names[(n %% 12L) + 1L],
    TRUE ~ x
  )
}

normalise_ydertype <- function(x) {
  raw <- str_trim(as.character(x %||% NA_character_))
  raw <- str_remove(raw, "D$")
  recode(
    raw,
    "0" = "Almen praksis",
    "1" = "Hospitalslæge",
    "2" = "Øvrige udstedere",
    "3" = "Speciallæge",
    "4" = "Ukendt",
    .default = raw,
    .missing = NA_character_
  )
}

normalise_year_month_columns <- function(dat) {
  if (!"År" %in% names(dat)) dat$`År` <- NA_character_
  if (!"Måned" %in% names(dat)) dat$`Måned` <- NA_character_

  old_col <- "År/Måned (Ekspedition)"
  if (old_col %in% names(dat)) {
    old <- str_trim(as.character(dat[[old_col]]))
    dat$`År` <- coalesce(
      na_if(str_trim(as.character(dat$`År`)), ""),
      str_extract(old, "[12][0-9]{3}"),
      if ("year_filter" %in% names(dat)) str_extract(as.character(dat$year_filter), "[12][0-9]{3}") else NA_character_
    )
    dat$`Måned` <- coalesce(
      na_if(str_trim(as.character(dat$`Måned`)), ""),
      old
    )
  } else if ("year_filter" %in% names(dat)) {
    dat$`År` <- coalesce(
      na_if(str_trim(as.character(dat$`År`)), ""),
      str_extract(as.character(dat$year_filter), "[12][0-9]{3}")
    )
  }

  dat |>
    mutate(
      `År` = str_remove(str_trim(as.character(`År`)), "D$"),
      `Måned` = month_label_da(`Måned`)
    )
}



finalise_parsed_long_export <- function(long2) {
  if (is.null(long2) || nrow(long2) == 0) return(tibble())

  # CSV-chunks fra browser-parseren bruger dansk Excel-format: semikolon + decimalkomma.
  if ("value" %in% names(long2)) {
    if (!is.numeric(long2$value)) {
      long2 <- long2 |>
        mutate(value = readr::parse_number(
          as.character(value),
          locale = readr::locale(decimal_mark = ",", grouping_mark = ".")
        ))
    }
  } else {
    long2$value <- 0
  }

  long2 <- normalise_year_month_columns(long2)

  if ("Ydertype" %in% names(long2)) {
    long2 <- long2 |> mutate(`Ydertype` = normalise_ydertype(`Ydertype`))
  } else if ("Ydertype, kode & tekst (Receptudsteder)" %in% names(long2)) {
    long2 <- long2 |> mutate(`Ydertype` = normalise_ydertype(`Ydertype, kode & tekst (Receptudsteder)`))
  } else {
    long2$`Ydertype` <- NA_character_
  }

  if (!"Bopælsregion" %in% names(long2)) {
    long2$Bopælsregion <- if ("region_filter" %in% names(long2)) long2$region_filter else NA_character_
  } else if ("region_filter" %in% names(long2)) {
    long2 <- long2 |>
      mutate(
        Bopælsregion = str_trim(as.character(Bopælsregion)),
        Bopælsregion = if_else(str_detect(Bopælsregion, "^\\d+$") | Bopælsregion == "", NA_character_, Bopælsregion),
        Bopælsregion = coalesce(Bopælsregion, as.character(region_filter))
      )
  }

  if ("atc_filter" %in% names(long2)) {
    long2 <- long2 |>
      mutate(
        `ATC, Niveau 5, kode & tekst` = case_when(
          is.na(`ATC, Niveau 5, kode & tekst`) |
            str_trim(as.character(`ATC, Niveau 5, kode & tekst`)) %in% c("", "0") ~ format_atc5(as.character(atc_filter)),
          TRUE ~ as.character(`ATC, Niveau 5, kode & tekst`)
        )
      )
  }

  needed <- c(
    "ATC, Niveau 5, kode & tekst",
    "Form",
    "Styrke",
    "Navn (Præparat)",
    "År",
    "Måned",
    "Ydertype",
    "Bopælsregion",
    "nøgletal_output"
  )

  for (nm in needed) {
    if (!nm %in% names(long2)) long2[[nm]] <- NA_character_
  }

  keys <- c(
    "ATC, Niveau 5, kode & tekst",
    "Form",
    "Styrke",
    "Navn (Præparat)",
    "År",
    "Måned",
    "Ydertype",
    "Bopælsregion"
  )

  out <- long2 |>
    mutate(
      value = replace_na(value, 0),
      across(all_of(c("Navn (Præparat)", "Form", "Styrke")), ~ na_if(str_trim(as.character(.x)), ""))
    ) |>
    filter(!(is.na(`Navn (Præparat)`) & is.na(Form) & is.na(Styrke))) |>
    group_by(across(all_of(c(keys, "nøgletal_output")))) |>
    summarise(value = sum(value, na.rm = TRUE), .groups = "drop") |>
    pivot_wider(
      names_from = nøgletal_output,
      values_from = value,
      values_fill = 0
    )

  for (col in unname(metric_map)) {
    if (!col %in% names(out)) out[[col]] <- 0
  }

  out |>
    relocate(all_of(keys), all_of(unname(metric_map))) |>
    filter(
      rowSums(across(all_of(unname(metric_map)), ~ replace_na(as.numeric(.x), 0))) != 0
    ) |>
    arrange(`ATC, Niveau 5, kode & tekst`, `Navn (Præparat)`, `År`, `Måned`)
}

parse_export_object <- function(raw) {
  if (is.null(raw) || length(raw) == 0) return(tibble())

  if (!is.list(raw) || is.null(raw[[1]])) {
    stop("JSON-filen kunne ikke tolkes som et array af Power BI-kald.")
  }

  long <- purrr::map_dfr(raw, function(one_call) {
    meta <- list(
      atc = one_call$atc %||% NA_character_,
      region = one_call$region %||% NA_character_,
      year = one_call$year %||% NA,
      opgoerelse = one_call$opgoerelse %||% NA_character_,
      status = one_call$status %||% NA,
      rowCountMarker = one_call$rowCountMarker %||% NA,
      hasRestartToken = one_call$hasRestartToken %||% NA,
      error = one_call$error %||% NA_character_
    )

    tryCatch(
      parse_one_response_dsr(one_call$response, meta),
      error = function(e) {
        tibble(
          `ATC, Niveau 5, kode & tekst` = NA_character_,
          Laegemiddeltekst_samlet_unavne = NA_character_,
          `År` = NA_character_,
          `Måned` = NA_character_,
          `Ydertype` = NA_character_,
          value = NA_real_,
          atc_filter = meta$atc,
          region_filter = meta$region,
          year_filter = paste(meta$year, collapse = ", "),
          opgoerelse = meta$opgoerelse,
          status = meta$status,
          rowCountMarker = meta$rowCountMarker,
          hasRestartToken = meta$hasRestartToken,
          export_error = paste("Parse-fejl:", e$message)
        )
      }
    )
  })

  if (nrow(long) == 0) return(tibble())

  med_split <- split_laegemiddeltekst(long$Laegemiddeltekst_samlet_unavne)

  long2 <- bind_cols(
    long |> select(-Laegemiddeltekst_samlet_unavne),
    med_split
  ) |>
    mutate(
      `ATC, Niveau 5, kode & tekst` = format_atc5(`ATC, Niveau 5, kode & tekst`),
      nøgletal_output = dplyr::recode(opgoerelse, !!!metric_map, .default = opgoerelse),
      value = replace_na(value, 0)
    )

  finalise_parsed_long_export(long2)
}

extract_export_metadata <- function(raw) {
  if (!is.list(raw) || length(raw) == 0) return(tibble())

  purrr::map_dfr(raw, function(x) {
    tibble(
      atc = paste(x$atc %||% NA_character_, collapse = ", "),
      region = paste(x$region %||% NA_character_, collapse = ", "),
      year = paste(x$year %||% NA_character_, collapse = ", "),
      opgoerelse = paste(x$opgoerelse %||% NA_character_, collapse = ", "),
      status = x$status %||% NA,
      rowCountMarker = x$rowCountMarker %||% NA,
      hasRestartToken = x$hasRestartToken %||% NA,
      error = x$error %||% NA_character_
    )
  })
}

pantone_287 <- "#0033A0"
pantone_289 <- "#0C2340"

ui <- page_sidebar(
  title = "Power BI Medicinsalg: eksportgenerator og parser",
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = pantone_289,
    primary = pantone_287,
    secondary = pantone_289,
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),

  tags$head(
    tags$style(HTML(paste0("
      :root { --p287: ", pantone_287, "; --p289: ", pantone_289, "; }
      body { background-color: #FFFFFF; }
      .bslib-sidebar-layout > .sidebar { border-right: 4px solid var(--p287); }
      .card { border: 1px solid rgba(12, 35, 64, 0.14); box-shadow: 0 0.25rem 0.9rem rgba(12, 35, 64, 0.08); }
      .card-header { background: var(--p289); color: #FFFFFF; font-weight: 700; }
      .btn-primary, .btn-default { background-color: var(--p287); border-color: var(--p287); color: #FFFFFF; }
      .btn-primary:hover, .btn-default:hover { background-color: var(--p289); border-color: var(--p289); color: #FFFFFF; }
      .btn-outline-secondary { color: var(--p289); border-color: var(--p289); background-color: #FFFFFF; }
      .btn-outline-secondary:hover { background-color: var(--p289); border-color: var(--p289); color: #FFFFFF; }
      .card-header #copy_js_btn { color: #FFFFFF !important; border-color: #FFFFFF !important; background-color: transparent !important; }
      .card-header #copy_js_btn:hover, .card-header #copy_js_btn:focus { color: #FFFFFF !important; border-color: #FFFFFF !important; background-color: var(--p287) !important; }
      h5, label, .control-label { color: var(--p289); }
      textarea, .form-control { border-color: rgba(0, 51, 160, 0.35); }
      code { color: var(--p287); }
    ")) )
  ),

  sidebar = sidebar(
    width = 390,

    h5("1. Dataudtræk"),

    textAreaInput(
      "atc",
      "ATC-koder eller Power BI ATC5-værdier",
      value = paste("A02BC02", "N06BA04", sep = "\n"),
      rows = 7,
      placeholder = "Én ATC-kode per linje, fx:\nA02BC02\nB01AF01\nN06BA04\n\nEller fuld Power BI-værdi:\nA02BC02 (Pantoprazol)"
    ),

    fileInput(
      "mapping_file",
      "Valgfri mapping-tabel: ATC → ATC5_Kode_Tekst",
      accept = c(".csv", ".txt", ".tsv")
    ),

    div(
      class = "alert alert-info py-2",
      strong("Nøgletal: "),
      "Alle tre nøgletal medtages altid: Antal DDD, regionalt tilskud og omsætning."
    ),

    checkboxGroupInput(
      "regions",
      "Region",
      choices = c(
        "Region Hovedstaden",
        "Region Sjælland",
        "Region Syddanmark",
        "Region Midtjylland",
        "Region Nordjylland"
      ),
      selected = c(
        "Region Hovedstaden",
        "Region Sjælland",
        "Region Syddanmark",
        "Region Midtjylland",
        "Region Nordjylland"
      )
    ),

    sliderInput(
      "years_range",
      "Periode (år)",
      min = 2020,
      max = as.integer(format(Sys.Date(), "%Y")),
      value = c(max(2023, as.integer(format(Sys.Date(), "%Y")) - 3), as.integer(format(Sys.Date(), "%Y"))),
      step = 1,
      sep = "",
      ticks = TRUE
    ),

    hr(),

    h5("2. Saml parsede CSV-chunks eller parse gammel JSON"),

    fileInput(
      "parsed_csv_files",
      "Upload medicinsalg_parsed_part_*.csv (kan vælge flere)",
      accept = c(".csv"),
      multiple = TRUE
    ),

    fileInput("json_file", "Alternativt: upload gammel rå powerbi_medicinsalg_export.json", accept = c(".json")),

    actionButton("parse_btn", "Saml/parse filer", class = "btn-primary"),

    hr(),

    downloadButton("download_js", "Download JavaScript"),
    downloadButton("download_csv", "Download CSV"),
    downloadButton("download_xlsx", "Download Excel")
  ),

  layout_columns(
    col_widths = c(12),

    card(
      card_header("Sådan bruges generatoren"),
      div(
        tags$ol(
          tags$li("Åbn Power BI-rapporten “Medicinsalg i primærsektoren” i Chrome."),
          tags$li("Tryk F12 og vælg fanen Console."),
          tags$li("Kopiér JavaScript-koden nedenfor og indsæt den i Console."),
          tags$li("Koden splitter automatisk deludtrækkene på år i baggrunden og downloader en eller flere kompakte CSV-filer direkte fra browseren."),
          tags$li("Upload eventuelt CSV-chunks her i appen og klik “Saml/parse filer” for at danne samlet tabel/Excel.")
        ),
        p(
          "Bemærk: Appen splitter automatisk på år i JavaScriptet for at mindske risikoen for afkortede Power BI-svar. ",
          "Hvis der alligevel vises RestartToken eller meget store rowCountMarker-værdier, er ét enkelt år/region/ATC/nøgletal sandsynligvis stadig for stort."
        )
      )
    ),

    card(
      card_header(
        div(
          "Genereret JavaScript",
          tags$button(
            id = "copy_js_btn",
            type = "button",
            class = "btn btn-sm btn-outline-secondary float-end",
            "Copy to clipboard"
          )
        )
      ),
      tags$textarea(
        id = "js_code_area",
        style = "width:100%; min-height:430px; font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', monospace; font-size: 12px;",
        readonly = NA
      ),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('set_js_code', function(message) {
          const area = document.getElementById('js_code_area');
          if (area) area.value = message;
        });

        document.addEventListener('DOMContentLoaded', function() {
          const btn = document.getElementById('copy_js_btn');
          btn.addEventListener('click', async function() {
            const txt = document.getElementById('js_code_area').value;
            try {
              await navigator.clipboard.writeText(txt);
              btn.innerText = 'Copied!';
              setTimeout(() => btn.innerText = 'Copy to clipboard', 1200);
            } catch(e) {
              const area = document.getElementById('js_code_area');
              area.select();
              document.execCommand('copy');
              btn.innerText = 'Copied!';
              setTimeout(() => btn.innerText = 'Copy to clipboard', 1200);
            }
          });
        });
      "))
    ),

    uiOutput("validation_message"),
    uiOutput("parse_status"),
    uiOutput("empty_data_warning"),
    uiOutput("truncation_warning"),

    card(
      card_header("Metadata for uploadet JSON"),
      DTOutput("metadata_table")
    ),

    card(
      card_header("Parsed data"),
      DTOutput("parsed_table")
    )
  )
)

server <- function(input, output, session) {
  mapping_data <- reactive({
    req(input$mapping_file)
    read_mapping_file(input$mapping_file$datapath)
  })

  selected_years <- reactive({
    rng <- suppressWarnings(as.integer(input$years_range))
    rng <- rng[!is.na(rng)]
    if (length(rng) < 2) return(integer(0))
    seq.int(min(rng), max(rng))
  })

  atc_powerbi <- reactive({
    atc_raw <- normalise_atc_input(input$atc)

    mapping <- NULL
    if (!is.null(input$mapping_file)) {
      mapping <- mapping_data()
    }

    apply_atc_mapping(atc_raw, mapping)
  })

  generated_js <- reactive({
    shiny::validate(
      shiny::need(length(atc_powerbi()) > 0, "Indtast mindst én ATC-kode."),
      shiny::need(length(input$regions) > 0, "Vælg mindst én region."),
      shiny::need(length(selected_years()) > 0, "Vælg mindst ét år.")
    )

    generate_js(
      atc_powerbi = atc_powerbi(),
      metrics = names(metric_map),
      regions = input$regions,
      years = selected_years(),
      split_by_year = TRUE
    )
  })

  output$validation_message <- renderUI({
    atc_raw <- normalise_atc_input(input$atc)
    atc_mapped <- atc_powerbi()

    if (length(atc_raw) == 0) {
      return(div(class = "alert alert-info", "Indtast mindst én ATC-kode."))
    }

    div(
      class = "alert alert-secondary",
      strong("ATC-værdier sendt til Power BI-filteret: "),
      tags$code(paste(atc_mapped, collapse = "; "))
    )
  })

  observe({
    session$sendCustomMessage("set_js_code", generated_js())
  })

  output$download_js <- downloadHandler(
    filename = function() {
      paste0("powerbi_medicinsalg_export_", format(Sys.Date(), "%Y%m%d"), ".js")
    },
    content = function(file) {
      writeLines(generated_js(), file, useBytes = TRUE)
    }
  )

  # Cache upload, metadata and parsed data when the user clicks Parse JSON.
  # This is intentionally not a plain reactive(), because large JSON files are expensive
  # to parse and download handlers should write exactly the same object as the table shows.
  raw_json_val <- reactiveVal(NULL)
  metadata_val <- reactiveVal(tibble())
  parsed_data_val <- reactiveVal(tibble())
  parse_status_val <- reactiveVal(NULL)

  observeEvent(input$parse_btn, {
    has_csv <- !is.null(input$parsed_csv_files) && nrow(input$parsed_csv_files) > 0
    has_json <- !is.null(input$json_file)
    req(has_csv || has_json)

    parse_status_val(NULL)
    metadata_val(tibble())
    parsed_data_val(tibble())

    tryCatch({
      if (has_csv) {
        long <- purrr::map_dfr(input$parsed_csv_files$datapath, function(path) {
          readr::read_csv2(
            path,
            show_col_types = FALSE,
            locale = readr::locale(encoding = "UTF-8", decimal_mark = ",", grouping_mark = ".")
          )
        })

        parsed <- finalise_parsed_long_export(long)

        meta <- long |>
          distinct(atc_filter, region_filter, year_filter, opgoerelse, status, rowCountMarker, hasRestartToken, export_error) |>
          rename(
            atc = atc_filter,
            region = region_filter,
            year = year_filter,
            error = export_error
          )

        raw_json_val(NULL)
        metadata_val(meta)
        parsed_data_val(parsed)

        parse_status_val(list(
          ok = TRUE,
          message = paste0(
            "CSV-chunks samlet: ", nrow(long), " lange rækker blev aggregeret til ",
            nrow(parsed), " outputrækker og ", ncol(parsed), " kolonner."
          )
        ))
      } else {
        raw <- jsonlite::fromJSON(input$json_file$datapath, simplifyVector = FALSE)
        meta <- extract_export_metadata(raw)
        parsed <- parse_export_object(raw)

        raw_json_val(raw)
        metadata_val(meta)
        parsed_data_val(parsed)

        parse_status_val(list(
          ok = TRUE,
          message = if (nrow(parsed) > 0) {
            paste0("Parsing færdig: ", nrow(parsed), " rækker og ", ncol(parsed), " kolonner.")
          } else {
            paste0(
              "Parsing gennemført, men Power BI-svaret indeholder 0 datarækker. ",
              "Brug den opdaterede JavaScript og hent data igen."
            )
          }
        ))
      }
    }, error = function(e) {
      raw_json_val(NULL)
      metadata_val(tibble())
      parsed_data_val(tibble())
      parse_status_val(list(
        ok = FALSE,
        message = paste("Parse-fejl:", e$message)
      ))
    })
  })

  metadata <- reactive({
    metadata_val()
  })

  parsed_data <- reactive({
    parsed_data_val()
  })

  output$parse_status <- renderUI({
    status <- parse_status_val()
    if (is.null(status)) return(NULL)

    div(
      class = if (isTRUE(status$ok)) "alert alert-success" else "alert alert-danger",
      status$message
    )
  })

  output$empty_data_warning <- renderUI({
    status <- parse_status_val()
    if (is.null(status) || !isTRUE(status$ok) || nrow(metadata()) == 0 || nrow(parsed_data()) > 0) {
      return(NULL)
    }

    div(
      class = "alert alert-warning",
      strong("Ingen datarækker i JSON-filen: "),
      "I den uploadede JSON er Power BI-kaldene gennemført, men resultatsættene er tomme. ",
      "Det skyldes typisk, at ATC-filteret ikke matcher feltet ATC5_Kode_Tekst. ",
      "Den reviderede JavaScript-generator bruger nu prefix-match for rå ATC-koder, fx C09AA05."
    )
  })

  output$truncation_warning <- renderUI({
    req(nrow(metadata()) > 0)
    meta <- metadata()

    problematic <- meta |>
      filter(
        hasRestartToken == TRUE |
          (!is.na(rowCountMarker) & suppressWarnings(as.numeric(rowCountMarker)) >= 29000)
      )

    if (nrow(problematic) == 0) return(NULL)

    div(
      class = "alert alert-warning",
      strong("Muligt afkortet Power BI-output: "),
      "Et eller flere deludtræk indeholder RestartToken eller har rowCountMarker omkring/over 30.000. ",
      "Prøv at lave et mindre udtræk, fx færre år, færre regioner eller færre ATC-koder."
    )
  })

  output$metadata_table <- renderDT({
    req(nrow(metadata()) > 0)
    datatable(
      metadata(),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$parsed_table <- renderDT({
    req(nrow(parsed_data()) > 0)
    datatable(
      parsed_data(),
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    )
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("medicinsalg_parsed_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      dat <- parsed_data_val()
      if (is.null(dat) || nrow(dat) == 0) {
        stop("Der er ingen parsede rækker at downloade. Hent en ny JSON med den opdaterede JavaScript-generator.")
      }
      readr::write_excel_csv2(dat, file, na = "")
    }
  )

  output$download_xlsx <- downloadHandler(
    filename = function() {
      paste0("medicinsalg_parsed_", format(Sys.Date(), "%Y%m%d"), ".xlsx")
    },
    content = function(file) {
      dat <- parsed_data_val()
      if (is.null(dat) || nrow(dat) == 0) {
        stop("Der er ingen parsede rækker at downloade. Hent en ny JSON med den opdaterede JavaScript-generator.")
      }
      writexl::write_xlsx(list("Medicinsalg" = dat), path = file)
    }
  )
}

shinyApp(ui, server)
