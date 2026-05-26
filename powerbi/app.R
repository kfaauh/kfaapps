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
    "IkVrc3BlZGl0aW9uc2JlbMO4YiIKICB9OwoKICBjb25zdCBub3JtYWxpc2F0aW9uT3B0aW9ucyA9",
    "IFsKICAgIHsgdmFsdWU6ICJJbmdlbiB2YWxnIiwgc3VmZml4OiAiIiB9LAogICAgeyB2YWx1ZTog",
    "IkFudGFsIHByLiAxLjAwMCIsIHN1ZmZpeDogIiBwci4gMS4wMDAgYm9yZ2VyZSIgfQogIF07Cgog",
    "IGZ1bmN0aW9uIG91dHB1dE1ldHJpY05hbWUob3Bnb2VyZWxzZSwgbm9ybWFsaXNlcmluZykgewog",
    "ICAgY29uc3QgYmFzZSA9IG1ldHJpY01hcFtvcGdvZXJlbHNlXSB8fCBvcGdvZXJlbHNlOwogICAg",
    "Y29uc3Qgb3B0ID0gbm9ybWFsaXNhdGlvbk9wdGlvbnMuZmluZCh4ID0+IHgudmFsdWUgPT09IG5v",
    "cm1hbGlzZXJpbmcpOwogICAgcmV0dXJuIGJhc2UgKyAob3B0ID8gb3B0LnN1ZmZpeCA6ICIiKTsK",
    "ICB9CgogIGNvbnN0IGhlYWRlcnMgPSBbCiAgICAiQVRDLCBOaXZlYXUgNSwga29kZSAmIHRla3N0",
    "IiwKICAgICJWYXJlbnVtbWVyIiwKICAgICJOYXZuIChQcsOmcGFyYXQpIiwKICAgICJGb3JtIiwK",
    "ICAgICJTdHlya2UiLAogICAgIlBha25pbmdzc3TDuHJyZWxzZSIsCiAgICAiw4VyIiwKICAgICJN",
    "w6VuZWQiLAogICAgIllkZXJ0eXBlIiwKICAgICJCb3DDpmxzcmVnaW9uIiwKICAgICJuw7hnbGV0",
    "YWxfb3V0cHV0IiwKICAgICJ2YWx1ZSIsCiAgICAiYXRjX2ZpbHRlciIsCiAgICAicmVnaW9uX2Zp",
    "bHRlciIsCiAgICAieWVhcl9maWx0ZXIiLAogICAgIm9wZ29lcmVsc2UiLAogICAgIm5vcm1hbGlz",
    "ZXJpbmciLAogICAgInN0YXR1cyIsCiAgICAicm93Q291bnRNYXJrZXIiLAogICAgImhhc1Jlc3Rh",
    "cnRUb2tlbiIsCiAgICAiZXhwb3J0X2Vycm9yIgogIF07CgogIGZ1bmN0aW9uIGxpdGVyYWxWYWx1",
    "ZSh2KSB7CiAgICBpZiAodHlwZW9mIHYgPT09ICJudW1iZXIiKSByZXR1cm4gYCR7dn1MYDsKICAg",
    "IGNvbnN0IHMgPSBTdHJpbmcodikucmVwbGFjZUFsbCgiXFwiLCAiXFxcXCIpLnJlcGxhY2VBbGwo",
    "IiciLCAiXFwnIik7CiAgICByZXR1cm4gYCcke3N9J2A7CiAgfQoKICBmdW5jdGlvbiBpbkZpbHRl",
    "cihzb3VyY2UsIHByb3BlcnR5LCB2YWx1ZXMpIHsKICAgIHJldHVybiB7CiAgICAgIENvbmRpdGlv",
    "bjogewogICAgICAgIEluOiB7CiAgICAgICAgICBFeHByZXNzaW9uczogW3sgQ29sdW1uOiB7IEV4",
    "cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTogc291cmNlIH0gfSwgUHJvcGVydHk6IHBy",
    "b3BlcnR5IH0gfV0sCiAgICAgICAgICBWYWx1ZXM6IHZhbHVlcy5tYXAodiA9PiBbeyBMaXRlcmFs",
    "OiB7IFZhbHVlOiBsaXRlcmFsVmFsdWUodikgfSB9XSkKICAgICAgICB9CiAgICAgIH0KICAgIH07",
    "CiAgfQoKICBmdW5jdGlvbiBzdGFydHNXaXRoRmlsdGVyKHNvdXJjZSwgcHJvcGVydHksIHZhbHVl",
    "KSB7CiAgICByZXR1cm4gewogICAgICBDb25kaXRpb246IHsKICAgICAgICBTdGFydHNXaXRoOiB7",
    "CiAgICAgICAgICBMZWZ0OiB7IENvbHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBT",
    "b3VyY2U6IHNvdXJjZSB9IH0sIFByb3BlcnR5OiBwcm9wZXJ0eSB9IH0sCiAgICAgICAgICBSaWdo",
    "dDogeyBMaXRlcmFsOiB7IFZhbHVlOiBsaXRlcmFsVmFsdWUodmFsdWUpIH0gfQogICAgICAgIH0K",
    "ICAgICAgfQogICAgfTsKICB9CgogIGZ1bmN0aW9uIGF0Y1RleHRGaWx0ZXIoc291cmNlLCBwcm9w",
    "ZXJ0eSwgdmFsdWUpIHsKICAgIGNvbnN0IHMgPSBTdHJpbmcodmFsdWUgfHwgIiIpLnRyaW0oKTsK",
    "ICAgIC8vIEZ1bGQgUG93ZXIgQkktdsOmcmRpOiBla3Nha3QgbWF0Y2guIFJlbiBBVEMta29kZTog",
    "cHJlZml4LW1hdGNoIHDDpSBBVEM1X0tvZGVfVGVrc3QuCiAgICBpZiAocy5pbmNsdWRlcygiKCIp",
    "ICYmIHMuaW5jbHVkZXMoIikiKSkgcmV0dXJuIGluRmlsdGVyKHNvdXJjZSwgcHJvcGVydHksIFtz",
    "XSk7CiAgICByZXR1cm4gc3RhcnRzV2l0aEZpbHRlcihzb3VyY2UsIHByb3BlcnR5LCBzLnRvVXBw",
    "ZXJDYXNlKCkpOwogIH0KCiAgZnVuY3Rpb24gYnVpbGRQYXlsb2FkKHsgYXRjVmFsdWUsIG1ldHJp",
    "Y1ZhbHVlcywgcmVnaW9uVmFsdWVzLCB5ZWFyVmFsdWVzLCBub3JtYWxpc2F0aW9uVmFsdWUgfSkg",
    "ewogICAgY29uc3QgaXNQcjEwMDAgPSBub3JtYWxpc2F0aW9uVmFsdWUgPT09ICJBbnRhbCBwci4g",
    "MS4wMDAiOwoKICAgIC8vIFZpZ3RpZ3Q6ICJBbnRhbCBwci4gMS4wMDAiIGVyIGVuIHNlcGFyYXQg",
    "Y2FsY3VsYXRpb24tZ3JvdXAvdGFibGUgaSBQb3dlciBCSS4KICAgIC8vIEh2aXMgdGFiZWxsZW4g",
    "IkFudGFsIHByICAxIDAwMCIgbWVkdGFnZXMvZmlsdHJlcmVzIHZlZCB0b3RhbHbDpnJkaWVyLCBr",
    "YW4gZGUgYWJzb2x1dHRlCiAgICAvLyBuw7hnbGV0YWwgZW5kZSBzb20gMC4gRGVyZm9yIGJydWdl",
    "cyBzb3VyY2UgImEiIGt1biB2ZWQgcHIuIDEuMDAwLWthbGQuCiAgICBjb25zdCBmcm9tSXRlbXMg",
    "PSBbCiAgICAgIHsgTmFtZTogIiMiLCBFbnRpdHk6ICIjIE1lYXN1cmVzIiwgVHlwZTogMCB9LAog",
    "ICAgICB7IE5hbWU6ICJsbSIsIEVudGl0eTogIkRpbUxhZWdlbWlkZGVsIiwgVHlwZTogMCB9LAog",
    "ICAgICB7IE5hbWU6ICJkYXRvIiwgRW50aXR5OiAiRGltRGF0byIsIFR5cGU6IDAgfSwKICAgICAg",
    "eyBOYW1lOiAidWQiLCBFbnRpdHk6ICJEaW1VZHN0ZWRlclR5cGUiLCBUeXBlOiAwIH0sCiAgICAg",
    "IHsgTmFtZTogIm9wZyIsIEVudGl0eTogIk9wZ8O4cmVsc2UiLCBUeXBlOiAwIH0sCiAgICAgIHsg",
    "TmFtZTogImtyb24iLCBFbnRpdHk6ICJLcm9uaXNrZSBzeWdkb21tZSIsIFR5cGU6IDAgfSwKICAg",
    "ICAgeyBOYW1lOiAiZ2VvIiwgRW50aXR5OiAiRGltQm9yZ2VyRGVtb2dyYWZpIiwgVHlwZTogMCB9",
    "LAogICAgICB7IE5hbWU6ICJ2b2wiLCBFbnRpdHk6ICJEaW1Wb2x1bWUiLCBUeXBlOiAwIH0KICAg",
    "IF07CgogICAgY29uc3Qgd2hlcmVJdGVtcyA9IFsKICAgICAgaW5GaWx0ZXIoInZvbCIsICJWb2x1",
    "bWVfdGVrc3RfZ3JwIiwgWyJEREQiXSksCiAgICAgIGluRmlsdGVyKCJvcGciLCAiQ2FsY3VsYXRp",
    "b25JdGVtQ29sdW1uIDEiLCBtZXRyaWNWYWx1ZXMpLAogICAgICBpbkZpbHRlcigiZGF0byIsICLD",
    "hXJfSUQiLCB5ZWFyVmFsdWVzKSwKICAgICAgaW5GaWx0ZXIoImtyb24iLCAiQ2FsY3VsYXRpb25J",
    "dGVtQ29sdW1uIDEiLCBbIkluZ2VuIHZhbGciXSksCiAgICAgIGluRmlsdGVyKCJnZW8iLCAiQm9w",
    "w6Zsc3JlZ2lvbiIsIHJlZ2lvblZhbHVlcyksCiAgICAgIGF0Y1RleHRGaWx0ZXIoImxtIiwgIkFU",
    "QzVfS29kZV9UZWtzdCIsIGF0Y1ZhbHVlKSwKICAgICAgaW5GaWx0ZXIoImxtIiwgIkZsYWdfVDMi",
    "LCBbIkphIl0pCiAgICBdOwoKICAgIGlmIChpc1ByMTAwMCkgewogICAgICBmcm9tSXRlbXMucHVz",
    "aCh7IE5hbWU6ICJhIiwgRW50aXR5OiAiQW50YWwgcHIgIDEgMDAwIiwgVHlwZTogMCB9KTsKICAg",
    "ICAgd2hlcmVJdGVtcy5zcGxpY2UoMywgMCwgaW5GaWx0ZXIoImEiLCAiQ2FsY3VsYXRpb25JdGVt",
    "Q29sdW1uIDEiLCBbIkFudGFsIHByLiAxLjAwMCJdKSk7CiAgICB9CgogICAgcmV0dXJuIHsKICAg",
    "ICAgdmVyc2lvbjogIjEuMC4wIiwKICAgICAgcXVlcmllczogW3sKICAgICAgICBRdWVyeTogewog",
    "ICAgICAgICAgQ29tbWFuZHM6IFt7CiAgICAgICAgICAgIFNlbWFudGljUXVlcnlEYXRhU2hhcGVD",
    "b21tYW5kOiB7CiAgICAgICAgICAgICAgUXVlcnk6IHsKICAgICAgICAgICAgICAgIFZlcnNpb246",
    "IDIsCiAgICAgICAgICAgICAgICBGcm9tOiBmcm9tSXRlbXMsCiAgICAgICAgICAgICAgICBTZWxl",
    "Y3Q6IFsKICAgICAgICAgICAgICAgICAgeyBDb2x1bW46IHsgRXhwcmVzc2lvbjogeyBTb3VyY2VS",
    "ZWY6IHsgU291cmNlOiAibG0iIH0gfSwgUHJvcGVydHk6ICJBVEM1X0tvZGVfVGVrc3QiIH0sIE5h",
    "bWU6ICJEaW1MYWVnZW1pZGRlbC5BVEM1X0tvZGVfVGVrc3QiIH0sCiAgICAgICAgICAgICAgICAg",
    "IHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTogImxtIiB9IH0s",
    "IFByb3BlcnR5OiAiTGFlZ2VtaWRkZWx0ZWtzdF9zYW1sZXRfdW5hdm5lIiB9LCBOYW1lOiAiRGlt",
    "TGFlZ2VtaWRkZWwuTGFlZ2VtaWRkZWx0ZWtzdF9zYW1sZXRfdW5hdm5lIiB9LAogICAgICAgICAg",
    "ICAgICAgICB7IENvbHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6ICJk",
    "YXRvIiB9IH0sIFByb3BlcnR5OiAiw4VyX0lEIiB9LCBOYW1lOiAiRGltRGF0by7DhXJfSUQiIH0s",
    "CiAgICAgICAgICAgICAgICAgIHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7",
    "IFNvdXJjZTogImRhdG8iIH0gfSwgUHJvcGVydHk6ICJNw6VuZWTDhXIiIH0sIE5hbWU6ICJEaW1E",
    "YXRvLk3DpW5lZMOFciIgfSwKICAgICAgICAgICAgICAgICAgeyBDb2x1bW46IHsgRXhwcmVzc2lv",
    "bjogeyBTb3VyY2VSZWY6IHsgU291cmNlOiAidWQiIH0gfSwgUHJvcGVydHk6ICJVZHN0ZWRlcl90",
    "eXBlIiB9LCBOYW1lOiAiRGltVWRzdGVkZXJUeXBlLlVkc3RlZGVyX3R5cGUiIH0sCiAgICAgICAg",
    "ICAgICAgICAgIHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTog",
    "ImdlbyIgfSB9LCBQcm9wZXJ0eTogIkJvcMOmbHNyZWdpb24iIH0sIE5hbWU6ICJEaW1Cb3JnZXJE",
    "ZW1vZ3JhZmkuQm9ww6Zsc3JlZ2lvbiIgfSwKICAgICAgICAgICAgICAgICAgeyBNZWFzdXJlOiB7",
    "IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTogIiMiIH0gfSwgUHJvcGVydHk6ICJT",
    "ZWxlY3RNZWFzdXJlMSIgfSwgTmFtZTogIiMgTWVhc3VyZXMuU2VsZWN0TWVhc3VyZTEiIH0KICAg",
    "ICAgICAgICAgICAgIF0sCiAgICAgICAgICAgICAgICBXaGVyZTogd2hlcmVJdGVtcywKICAgICAg",
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
    "Im7DuGdsZXRhbF9vdXRwdXQiOiBvdXRwdXRNZXRyaWNOYW1lKG1ldGEub3Bnb2VyZWxzZSwgbWV0",
    "YS5ub3JtYWxpc2VyaW5nKSwKICAgICAgICAidmFsdWUiOiBwYXJzZVBiaU51bWJlcihjdXJyZW50",
    "WzZdKSwKICAgICAgICAiYXRjX2ZpbHRlciI6IG1ldGEuYXRjLAogICAgICAgICJyZWdpb25fZmls",
    "dGVyIjogbWV0YS5yZWdpb24sCiAgICAgICAgInllYXJfZmlsdGVyIjogQXJyYXkuaXNBcnJheSht",
    "ZXRhLnllYXIpID8gbWV0YS55ZWFyLmpvaW4oIiwgIikgOiBTdHJpbmcobWV0YS55ZWFyKSwKICAg",
    "ICAgICAib3Bnb2VyZWxzZSI6IG1ldGEub3Bnb2VyZWxzZSwKICAgICAgICAibm9ybWFsaXNlcmlu",
    "ZyI6IG1ldGEubm9ybWFsaXNlcmluZywKICAgICAgICAic3RhdHVzIjogbWV0YS5zdGF0dXMsCiAg",
    "ICAgICAgInJvd0NvdW50TWFya2VyIjogbWV0YS5yb3dDb3VudE1hcmtlciwKICAgICAgICAiaGFz",
    "UmVzdGFydFRva2VuIjogbWV0YS5oYXNSZXN0YXJ0VG9rZW4sCiAgICAgICAgImV4cG9ydF9lcnJv",
    "ciI6IG1ldGEuZXJyb3IgfHwgIiIKICAgICAgfSk7CiAgICB9CiAgICByZXR1cm4gb3V0OwogIH0K",
    "CiAgZnVuY3Rpb24gY3N2RXNjYXBlKHYpIHsKICAgIGlmICh2ID09IG51bGwpIHJldHVybiAiIjsK",
    "ICAgIGlmICh0eXBlb2YgdiA9PT0gIm51bWJlciIpIHYgPSBTdHJpbmcodikucmVwbGFjZSgiLiIs",
    "ICIsIik7CiAgICBlbHNlIHYgPSBTdHJpbmcodik7CiAgICBpZiAoL1siO1xyXG5dLy50ZXN0KHYp",
    "KSB2ID0gJyInICsgdi5yZXBsYWNlQWxsKCciJywgJyIiJykgKyAnIic7CiAgICByZXR1cm4gdjsK",
    "ICB9CgogIGZ1bmN0aW9uIHJvd3NUb0Nzdihyb3dzKSB7CiAgICBjb25zdCBsaW5lcyA9IFtoZWFk",
    "ZXJzLm1hcChjc3ZFc2NhcGUpLmpvaW4oIjsiKV07CiAgICBmb3IgKGNvbnN0IHIgb2Ygcm93cykg",
    "bGluZXMucHVzaChoZWFkZXJzLm1hcChoID0+IGNzdkVzY2FwZShyW2hdKSkuam9pbigiOyIpKTsK",
    "ICAgIHJldHVybiAiXHVmZWZmIiArIGxpbmVzLmpvaW4oIlxyXG4iKTsKICB9CgogIGZ1bmN0aW9u",
    "IGRvd25sb2FkVGV4dChmaWxlbmFtZSwgdGV4dCwgdHlwZSkgewogICAgY29uc3QgYmxvYiA9IG5l",
    "dyBCbG9iKFt0ZXh0XSwgeyB0eXBlOiB0eXBlIHx8ICJ0ZXh0L3BsYWluO2NoYXJzZXQ9dXRmLTgi",
    "IH0pOwogICAgY29uc3QgYSA9IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoImEiKTsKICAgIGEuaHJl",
    "ZiA9IFVSTC5jcmVhdGVPYmplY3RVUkwoYmxvYik7CiAgICBhLmRvd25sb2FkID0gZmlsZW5hbWU7",
    "CiAgICBkb2N1bWVudC5ib2R5LmFwcGVuZENoaWxkKGEpOwogICAgYS5jbGljaygpOwogICAgYS5y",
    "ZW1vdmUoKTsKICAgIHNldFRpbWVvdXQoKCkgPT4gVVJMLnJldm9rZU9iamVjdFVSTChhLmhyZWYp",
    "LCA1MDAwKTsKICB9CgogIGFzeW5jIGZ1bmN0aW9uIHJ1bk9uZSh7IGF0YywgcmVnaW9uLCB5ZWFy",
    "LCBvcGdvZXJlbHNlLCBub3JtYWxpc2VyaW5nIH0pIHsKICAgIGNvbnN0IHllYXJWYWx1ZXMgPSBB",
    "cnJheS5pc0FycmF5KHllYXIpID8geWVhciA6IFt5ZWFyXTsKICAgIGNvbnN0IHBheWxvYWQgPSBi",
    "dWlsZFBheWxvYWQoeyBhdGNWYWx1ZTogYXRjLCBtZXRyaWNWYWx1ZXM6IFtvcGdvZXJlbHNlXSwg",
    "cmVnaW9uVmFsdWVzOiBbcmVnaW9uXSwgeWVhclZhbHVlcywgbm9ybWFsaXNhdGlvblZhbHVlOiBu",
    "b3JtYWxpc2VyaW5nIH0pOwogICAgbGV0IHN0YXR1cyA9IG51bGwsIHRleHQgPSAiIiwgcGFyc2Vk",
    "SnNvbiA9IG51bGwsIGVycm9yID0gbnVsbDsKICAgIHRyeSB7CiAgICAgIGNvbnN0IHJlc3BvbnNl",
    "ID0gYXdhaXQgZmV0Y2godXJsLCB7CiAgICAgICAgbWV0aG9kOiAiUE9TVCIsCiAgICAgICAgY3Jl",
    "ZGVudGlhbHM6ICJpbmNsdWRlIiwKICAgICAgICBoZWFkZXJzOiB7CiAgICAgICAgICAiYWNjZXB0",
    "IjogImFwcGxpY2F0aW9uL2pzb24sIHRleHQvcGxhaW4sICovKiIsCiAgICAgICAgICAiY29udGVu",
    "dC10eXBlIjogImFwcGxpY2F0aW9uL2pzb247Y2hhcnNldD1VVEYtOCIsCiAgICAgICAgICAieC1w",
    "b3dlcmJpLXJlc291cmNla2V5IjogImFueSIKICAgICAgICB9LAogICAgICAgIGJvZHk6IEpTT04u",
    "c3RyaW5naWZ5KHBheWxvYWQpCiAgICAgIH0pOwogICAgICBzdGF0dXMgPSByZXNwb25zZS5zdGF0",
    "dXM7CiAgICAgIHRleHQgPSBhd2FpdCByZXNwb25zZS50ZXh0KCk7CiAgICAgIHRyeSB7IHBhcnNl",
    "ZEpzb24gPSBKU09OLnBhcnNlKHRleHQpOyB9IGNhdGNoKGUpIHsgZXJyb3IgPSAiS3VubmUgaWtr",
    "ZSBKU09OLXBhcnNlIHJlc3BvbnNlOiAiICsgZS5tZXNzYWdlOyB9CiAgICB9IGNhdGNoKGUpIHsK",
    "ICAgICAgZXJyb3IgPSAiRmV0Y2gtZmVqbDogIiArIGUubWVzc2FnZTsKICAgIH0KCiAgICBjb25z",
    "dCByb3dDb3VudE1hcmtlciA9IHRleHQuc3BsaXQoJyJDIjpbJykubGVuZ3RoIC0gMTsKICAgIGNv",
    "bnN0IGhhc1Jlc3RhcnRUb2tlbiA9IHRleHQuaW5jbHVkZXMoJyJSVCInKTsKICAgIGNvbnN0IG1l",
    "dGEgPSB7IGF0YywgcmVnaW9uLCB5ZWFyLCBvcGdvZXJlbHNlLCBub3JtYWxpc2VyaW5nLCBzdGF0",
    "dXMsIHJvd0NvdW50TWFya2VyLCBoYXNSZXN0YXJ0VG9rZW4sIGVycm9yIH07CiAgICBjb25zdCBy",
    "b3dzID0gcGFyc2VSZXNwb25zZVRvUm93cyhwYXJzZWRKc29uLCBtZXRhKTsKICAgIHJldHVybiB7",
    "IG1ldGE6IHsgLi4ubWV0YSwgcGFyc2VkUm93czogcm93cy5sZW5ndGggfSwgcm93cyB9OwogIH0K",
    "CiAgY29uc3QgdG90YWwgPSBzZWxlY3RlZEF0Yy5sZW5ndGggKiBzZWxlY3RlZFJlZ2lvbnMubGVu",
    "Z3RoICogc2VsZWN0ZWRNZXRyaWNzLmxlbmd0aCAqIG5vcm1hbGlzYXRpb25PcHRpb25zLmxlbmd0",
    "aCAqIChzcGxpdEJ5WWVhciA/IHNlbGVjdGVkWWVhcnMubGVuZ3RoIDogMSk7CiAgY29uc3Qgc3Vt",
    "bWFyeSA9IFtdOwogIGxldCBkb25lID0gMDsKICBsZXQgcGFydE5vID0gMTsKICBsZXQgYnVmZmVy",
    "ID0gW107CgogIGZ1bmN0aW9uIGZsdXNoKGZvcmNlKSB7CiAgICBpZiAoYnVmZmVyLmxlbmd0aCA9",
    "PT0gMCkgcmV0dXJuOwogICAgaWYgKCFmb3JjZSAmJiBidWZmZXIubGVuZ3RoIDwgbWF4Um93c1Bl",
    "ckNzdikgcmV0dXJuOwogICAgY29uc3QgZmlsZW5hbWUgPSBgbWVkaWNpbnNhbGdfcGFyc2VkX3Bh",
    "cnRfJHtTdHJpbmcocGFydE5vKS5wYWRTdGFydCgzLCAiMCIpfS5jc3ZgOwogICAgZG93bmxvYWRU",
    "ZXh0KGZpbGVuYW1lLCByb3dzVG9Dc3YoYnVmZmVyKSwgInRleHQvY3N2O2NoYXJzZXQ9dXRmLTgi",
    "KTsKICAgIGNvbnNvbGUubG9nKGBEb3dubG9hZGVkZSAke2ZpbGVuYW1lfSBtZWQgJHtidWZmZXIu",
    "bGVuZ3RofSByw6Zra2VyYCk7CiAgICBwYXJ0Tm8gKz0gMTsKICAgIGJ1ZmZlciA9IFtdOwogIH0K",
    "CiAgZm9yIChjb25zdCBhdGMgb2Ygc2VsZWN0ZWRBdGMpIHsKICAgIGZvciAoY29uc3QgcmVnaW9u",
    "IG9mIHNlbGVjdGVkUmVnaW9ucykgewogICAgICBmb3IgKGNvbnN0IG9wZ29lcmVsc2Ugb2Ygc2Vs",
    "ZWN0ZWRNZXRyaWNzKSB7CiAgICAgICAgZm9yIChjb25zdCBub3JtYWxpc2VyaW5nIG9mIG5vcm1h",
    "bGlzYXRpb25PcHRpb25zLm1hcCh4ID0+IHgudmFsdWUpKSB7CiAgICAgICAgICBpZiAoc3BsaXRC",
    "eVllYXIpIHsKICAgICAgICAgICAgZm9yIChjb25zdCB5ZWFyIG9mIHNlbGVjdGVkWWVhcnMpIHsK",
    "ICAgICAgICAgICAgICBkb25lKys7CiAgICAgICAgICAgICAgY29uc29sZS5sb2coYFske2RvbmV9",
    "LyR7dG90YWx9XSBIZW50ZXJgLCB7IGF0YywgcmVnaW9uLCBvcGdvZXJlbHNlLCBub3JtYWxpc2Vy",
    "aW5nLCB5ZWFyIH0pOwogICAgICAgICAgICAgIGNvbnN0IHJlcyA9IGF3YWl0IHJ1bk9uZSh7IGF0",
    "YywgcmVnaW9uLCBvcGdvZXJlbHNlLCBub3JtYWxpc2VyaW5nLCB5ZWFyIH0pOwogICAgICAgICAg",
    "ICAgIHN1bW1hcnkucHVzaChyZXMubWV0YSk7CiAgICAgICAgICAgICAgYnVmZmVyLnB1c2goLi4u",
    "cmVzLnJvd3MpOwogICAgICAgICAgICAgIGZsdXNoKGZhbHNlKTsKICAgICAgICAgICAgfQogICAg",
    "ICAgICAgfSBlbHNlIHsKICAgICAgICAgICAgZG9uZSsrOwogICAgICAgICAgICBjb25zb2xlLmxv",
    "ZyhgWyR7ZG9uZX0vJHt0b3RhbH1dIEhlbnRlcmAsIHsgYXRjLCByZWdpb24sIG9wZ29lcmVsc2Us",
    "IG5vcm1hbGlzZXJpbmcsIHllYXI6IHNlbGVjdGVkWWVhcnMgfSk7CiAgICAgICAgICAgIGNvbnN0",
    "IHJlcyA9IGF3YWl0IHJ1bk9uZSh7IGF0YywgcmVnaW9uLCBvcGdvZXJlbHNlLCBub3JtYWxpc2Vy",
    "aW5nLCB5ZWFyOiBzZWxlY3RlZFllYXJzIH0pOwogICAgICAgICAgICBzdW1tYXJ5LnB1c2gocmVz",
    "Lm1ldGEpOwogICAgICAgICAgICBidWZmZXIucHVzaCguLi5yZXMucm93cyk7CiAgICAgICAgICAg",
    "IGZsdXNoKGZhbHNlKTsKICAgICAgICAgIH0KICAgICAgICB9CiAgICAgIH0KICAgIH0KICB9Cgog",
    "IGZsdXNoKHRydWUpOwogIGRvd25sb2FkVGV4dCgibWVkaWNpbnNhbGdfZXhwb3J0X3N1bW1hcnku",
    "anNvbiIsIEpTT04uc3RyaW5naWZ5KHN1bW1hcnksIG51bGwsIDIpLCAiYXBwbGljYXRpb24vanNv",
    "bjtjaGFyc2V0PXV0Zi04Iik7CgogIGNvbnN0IHRvdGFsUm93cyA9IHN1bW1hcnkucmVkdWNlKChz",
    "LCB4KSA9PiBzICsgKHgucGFyc2VkUm93cyB8fCAwKSwgMCk7CiAgY29uc3QgdHJ1bmNhdGVkID0g",
    "c3VtbWFyeS5maWx0ZXIoeCA9PiB4Lmhhc1Jlc3RhcnRUb2tlbiB8fCBOdW1iZXIoeC5yb3dDb3Vu",
    "dE1hcmtlcikgPj0gMjkwMDApOwogIGNvbnNvbGUubG9nKGBGw6ZyZGlnLiBQYXJzZWRlICR7dG90",
    "YWxSb3dzfSByw6Zra2VyIGZvcmRlbHQgcMOlICR7cGFydE5vIC0gMX0gQ1NWLWZpbChlcikuYCwg",
    "c3VtbWFyeSk7CiAgaWYgKHRydW5jYXRlZC5sZW5ndGggPiAwKSBjb25zb2xlLndhcm4oIk11bGln",
    "dCBhZmtvcnRlZGUgUG93ZXIgQkktc3ZhcjoiLCB0cnVuY2F0ZWQpOwp9KSgpOw=="
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
      normalisering = metadata$normalisering %||% NA_character_,
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

metric_cols <- c(
  unname(metric_map),
  paste0(unname(metric_map), " pr. 1.000 borgere")
)

output_metric_name <- function(opgoerelse, normalisering = NA_character_) {
  base <- dplyr::recode(opgoerelse, !!!metric_map, .default = opgoerelse)
  is_pr1000 <- !is.na(normalisering) & normalisering == "Antal pr. 1.000"
  if_else(is_pr1000, paste0(base, " pr. 1.000 borgere"), base)
}


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
  
  for (col in metric_cols) {
    if (!col %in% names(out)) out[[col]] <- 0
  }
  
  out |>
    relocate(all_of(keys), all_of(metric_cols)) |>
    filter(
      rowSums(across(all_of(metric_cols), ~ replace_na(as.numeric(.x), 0))) != 0
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
      normalisering = one_call$normalisering %||% NA_character_,
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
          normalisering = meta$normalisering,
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
      nøgletal_output = output_metric_name(opgoerelse, normalisering),
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
      normalisering = paste(x$normalisering %||% NA_character_, collapse = ", "),
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
      "Alle tre nøgletal medtages altid — både som absolutte værdier og pr. 1.000 borgere."
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
        
        if (!"normalisering" %in% names(long)) long$normalisering <- NA_character_
        
        parsed <- finalise_parsed_long_export(long)
        
        meta <- long |>
          distinct(atc_filter, region_filter, year_filter, opgoerelse, normalisering, status, rowCountMarker, hasRestartToken, export_error) |>
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
