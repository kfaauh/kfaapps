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

generate_js <- function(atc_powerbi, metrics, regions, years, split_by_year, output_level = "atc5") {
  atc_js <- js_string_array(atc_powerbi)
  metrics_js <- js_string_array(metrics)
  regions_js <- js_string_array(regions)
  years_js <- js_numeric_array(years)
  split_js <- if (isTRUE(split_by_year)) "true" else "false"
  output_level <- match.arg(output_level, choices = c("atc5", "varenummer"))
  output_level_js <- jsonlite::toJSON(output_level, auto_unbox = TRUE)
  
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
    "cGxpdEJ5WWVhciA9IF9fU1BMSVRfSlNfXzsKICBjb25zdCBvdXRwdXRMZXZlbCA9IF9fT1VUUFVU",
    "X0xFVkVMX0pTX187CiAgY29uc3QgaXNWYXJlbnVtbWVyTGV2ZWwgPSBvdXRwdXRMZXZlbCA9PT0g",
    "InZhcmVudW1tZXIiOwoKICAvLyBIb2xkIGh2ZXIgZmlsIG1vZGVyYXQgaSBzdMO4cnJlbHNlLCBz",
    "w6UgZGUga2FuIHVwbG9hZGVzIGVua2VsdHZpcyBiYWdlZnRlciwgaHZpcyDDuG5za2V0LgogIGNv",
    "bnN0IG1heFJvd3NQZXJDc3YgPSA1MDAwMDsKCiAgY29uc3QgbWV0cmljTWFwID0gewogICAgIk3D",
    "pm5nZGVmb3JicnVnIjogIkFudGFsIERERCIsCiAgICAiUmVnaW9uYWx0IHRpbHNrdWQiOiAiVGls",
    "c2t1ZHNiZWzDuGIgLSBSZWdpb25hbHQiLAogICAgIk9tc8OmdG5pbmciOiAiRWtzcGVkaXRpb25z",
    "YmVsw7hiIgogIH07CgogIGNvbnN0IG5vcm1hbGlzYXRpb25PcHRpb25zID0gWwogICAgeyB2YWx1",
    "ZTogIkluZ2VuIHZhbGciLCBzdWZmaXg6ICIiIH0sCiAgICB7IHZhbHVlOiAiQW50YWwgcHIuIDEu",
    "MDAwIiwgc3VmZml4OiAiIHByLiAxLjAwMCBib3JnZXJlIiB9CiAgXTsKCiAgZnVuY3Rpb24gb3V0",
    "cHV0TWV0cmljTmFtZShvcGdvZXJlbHNlLCBub3JtYWxpc2VyaW5nKSB7CiAgICBjb25zdCBiYXNl",
    "ID0gbWV0cmljTWFwW29wZ29lcmVsc2VdIHx8IG9wZ29lcmVsc2U7CiAgICBjb25zdCBvcHQgPSBu",
    "b3JtYWxpc2F0aW9uT3B0aW9ucy5maW5kKHggPT4geC52YWx1ZSA9PT0gbm9ybWFsaXNlcmluZyk7",
    "CiAgICByZXR1cm4gYmFzZSArIChvcHQgPyBvcHQuc3VmZml4IDogIiIpOwogIH0KCiAgY29uc3Qg",
    "aGVhZGVycyA9IFsKICAgICJBVEMsIE5pdmVhdSA1LCBrb2RlICYgdGVrc3QiLAogICAgIlZhcmVu",
    "dW1tZXIiLAogICAgIk5hdm4gKFByw6ZwYXJhdCkiLAogICAgIkZvcm0iLAogICAgIlN0eXJrZSIs",
    "CiAgICAiUGFrbmluZ3NzdMO4cnJlbHNlIiwKICAgICLDhXIiLAogICAgIk3DpW5lZCIsCiAgICAi",
    "WWRlcnR5cGUiLAogICAgIkJvcMOmbHNyZWdpb24iLAogICAgIm7DuGdsZXRhbF9vdXRwdXQiLAog",
    "ICAgInZhbHVlIiwKICAgICJhdGNfZmlsdGVyIiwKICAgICJyZWdpb25fZmlsdGVyIiwKICAgICJ5",
    "ZWFyX2ZpbHRlciIsCiAgICAib3Bnb2VyZWxzZSIsCiAgICAibm9ybWFsaXNlcmluZyIsCiAgICAi",
    "b3Bnb2VyZWxzZXNuaXZlYXUiLAogICAgInN0YXR1cyIsCiAgICAicm93Q291bnRNYXJrZXIiLAog",
    "ICAgImhhc1Jlc3RhcnRUb2tlbiIsCiAgICAiZXhwb3J0X2Vycm9yIgogIF07CgogIGZ1bmN0aW9u",
    "IGxpdGVyYWxWYWx1ZSh2KSB7CiAgICBpZiAodHlwZW9mIHYgPT09ICJudW1iZXIiKSByZXR1cm4g",
    "YCR7dn1MYDsKICAgIGNvbnN0IHMgPSBTdHJpbmcodikucmVwbGFjZUFsbCgiXFwiLCAiXFxcXCIp",
    "LnJlcGxhY2VBbGwoIiciLCAiXFwnIik7CiAgICByZXR1cm4gYCcke3N9J2A7CiAgfQoKICBmdW5j",
    "dGlvbiBpbkZpbHRlcihzb3VyY2UsIHByb3BlcnR5LCB2YWx1ZXMpIHsKICAgIHJldHVybiB7CiAg",
    "ICAgIENvbmRpdGlvbjogewogICAgICAgIEluOiB7CiAgICAgICAgICBFeHByZXNzaW9uczogW3sg",
    "Q29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTogc291cmNlIH0gfSwg",
    "UHJvcGVydHk6IHByb3BlcnR5IH0gfV0sCiAgICAgICAgICBWYWx1ZXM6IHZhbHVlcy5tYXAodiA9",
    "PiBbeyBMaXRlcmFsOiB7IFZhbHVlOiBsaXRlcmFsVmFsdWUodikgfSB9XSkKICAgICAgICB9CiAg",
    "ICAgIH0KICAgIH07CiAgfQoKICBmdW5jdGlvbiBzdGFydHNXaXRoRmlsdGVyKHNvdXJjZSwgcHJv",
    "cGVydHksIHZhbHVlKSB7CiAgICByZXR1cm4gewogICAgICBDb25kaXRpb246IHsKICAgICAgICBT",
    "dGFydHNXaXRoOiB7CiAgICAgICAgICBMZWZ0OiB7IENvbHVtbjogeyBFeHByZXNzaW9uOiB7IFNv",
    "dXJjZVJlZjogeyBTb3VyY2U6IHNvdXJjZSB9IH0sIFByb3BlcnR5OiBwcm9wZXJ0eSB9IH0sCiAg",
    "ICAgICAgICBSaWdodDogeyBMaXRlcmFsOiB7IFZhbHVlOiBsaXRlcmFsVmFsdWUodmFsdWUpIH0g",
    "fQogICAgICAgIH0KICAgICAgfQogICAgfTsKICB9CgogIGZ1bmN0aW9uIGF0Y1RleHRGaWx0ZXIo",
    "c291cmNlLCBwcm9wZXJ0eSwgdmFsdWUpIHsKICAgIGNvbnN0IHMgPSBTdHJpbmcodmFsdWUgfHwg",
    "IiIpLnRyaW0oKTsKICAgIC8vIEZ1bGQgUG93ZXIgQkktdsOmcmRpOiBla3Nha3QgbWF0Y2guIFJl",
    "biBBVEMta29kZTogcHJlZml4LW1hdGNoIHDDpSBBVEM1X0tvZGVfVGVrc3QuCiAgICBpZiAocy5p",
    "bmNsdWRlcygiKCIpICYmIHMuaW5jbHVkZXMoIikiKSkgcmV0dXJuIGluRmlsdGVyKHNvdXJjZSwg",
    "cHJvcGVydHksIFtzXSk7CiAgICByZXR1cm4gc3RhcnRzV2l0aEZpbHRlcihzb3VyY2UsIHByb3Bl",
    "cnR5LCBzLnRvVXBwZXJDYXNlKCkpOwogIH0KCiAgZnVuY3Rpb24gYnVpbGRQYXlsb2FkKHsgYXRj",
    "VmFsdWUsIG1ldHJpY1ZhbHVlcywgcmVnaW9uVmFsdWVzLCB5ZWFyVmFsdWVzLCBub3JtYWxpc2F0",
    "aW9uVmFsdWUgfSkgewogICAgY29uc3QgaXNQcjEwMDAgPSBub3JtYWxpc2F0aW9uVmFsdWUgPT09",
    "ICJBbnRhbCBwci4gMS4wMDAiOwoKICAgIC8vIFZpZ3RpZ3Q6ICJBbnRhbCBwci4gMS4wMDAiIGVy",
    "IGVuIHNlcGFyYXQgY2FsY3VsYXRpb24tZ3JvdXAvdGFibGUgaSBQb3dlciBCSS4KICAgIC8vIEh2",
    "aXMgdGFiZWxsZW4gIkFudGFsIHByICAxIDAwMCIgbWVkdGFnZXMvZmlsdHJlcmVzIHZlZCB0b3Rh",
    "bHbDpnJkaWVyLCBrYW4gZGUgYWJzb2x1dHRlCiAgICAvLyBuw7hnbGV0YWwgZW5kZSBzb20gMC4g",
    "RGVyZm9yIGJydWdlcyBzb3VyY2UgImEiIGt1biB2ZWQgcHIuIDEuMDAwLWthbGQuCiAgICBjb25z",
    "dCBmcm9tSXRlbXMgPSBbCiAgICAgIHsgTmFtZTogIiMiLCBFbnRpdHk6ICIjIE1lYXN1cmVzIiwg",
    "VHlwZTogMCB9LAogICAgICB7IE5hbWU6ICJsbSIsIEVudGl0eTogIkRpbUxhZWdlbWlkZGVsIiwg",
    "VHlwZTogMCB9LAogICAgICB7IE5hbWU6ICJkYXRvIiwgRW50aXR5OiAiRGltRGF0byIsIFR5cGU6",
    "IDAgfSwKICAgICAgeyBOYW1lOiAidWQiLCBFbnRpdHk6ICJEaW1VZHN0ZWRlclR5cGUiLCBUeXBl",
    "OiAwIH0sCiAgICAgIHsgTmFtZTogIm9wZyIsIEVudGl0eTogIk9wZ8O4cmVsc2UiLCBUeXBlOiAw",
    "IH0sCiAgICAgIHsgTmFtZTogImtyb24iLCBFbnRpdHk6ICJLcm9uaXNrZSBzeWdkb21tZSIsIFR5",
    "cGU6IDAgfSwKICAgICAgeyBOYW1lOiAiZ2VvIiwgRW50aXR5OiAiRGltQm9yZ2VyRGVtb2dyYWZp",
    "IiwgVHlwZTogMCB9LAogICAgICB7IE5hbWU6ICJ2b2wiLCBFbnRpdHk6ICJEaW1Wb2x1bWUiLCBU",
    "eXBlOiAwIH0KICAgIF07CgogICAgY29uc3Qgd2hlcmVJdGVtcyA9IFsKICAgICAgaW5GaWx0ZXIo",
    "InZvbCIsICJWb2x1bWVfdGVrc3RfZ3JwIiwgWyJEREQiXSksCiAgICAgIGluRmlsdGVyKCJvcGci",
    "LCAiQ2FsY3VsYXRpb25JdGVtQ29sdW1uIDEiLCBtZXRyaWNWYWx1ZXMpLAogICAgICBpbkZpbHRl",
    "cigiZGF0byIsICLDhXJfSUQiLCB5ZWFyVmFsdWVzKSwKICAgICAgaW5GaWx0ZXIoImtyb24iLCAi",
    "Q2FsY3VsYXRpb25JdGVtQ29sdW1uIDEiLCBbIkluZ2VuIHZhbGciXSksCiAgICAgIGluRmlsdGVy",
    "KCJnZW8iLCAiQm9ww6Zsc3JlZ2lvbiIsIHJlZ2lvblZhbHVlcyksCiAgICAgIGF0Y1RleHRGaWx0",
    "ZXIoImxtIiwgIkFUQzVfS29kZV9UZWtzdCIsIGF0Y1ZhbHVlKSwKICAgICAgaW5GaWx0ZXIoImxt",
    "IiwgIkZsYWdfVDMiLCBbIkphIl0pCiAgICBdOwoKICAgIGlmIChpc1ByMTAwMCkgewogICAgICBm",
    "cm9tSXRlbXMucHVzaCh7IE5hbWU6ICJhIiwgRW50aXR5OiAiQW50YWwgcHIgIDEgMDAwIiwgVHlw",
    "ZTogMCB9KTsKICAgICAgd2hlcmVJdGVtcy5zcGxpY2UoMywgMCwgaW5GaWx0ZXIoImEiLCAiQ2Fs",
    "Y3VsYXRpb25JdGVtQ29sdW1uIDEiLCBbIkFudGFsIHByLiAxLjAwMCJdKSk7CiAgICB9CgogICAg",
    "cmV0dXJuIHsKICAgICAgdmVyc2lvbjogIjEuMC4wIiwKICAgICAgcXVlcmllczogW3sKICAgICAg",
    "ICBRdWVyeTogewogICAgICAgICAgQ29tbWFuZHM6IFt7CiAgICAgICAgICAgIFNlbWFudGljUXVl",
    "cnlEYXRhU2hhcGVDb21tYW5kOiB7CiAgICAgICAgICAgICAgUXVlcnk6IHsKICAgICAgICAgICAg",
    "ICAgIFZlcnNpb246IDIsCiAgICAgICAgICAgICAgICBGcm9tOiBmcm9tSXRlbXMsCiAgICAgICAg",
    "ICAgICAgICBTZWxlY3Q6ICgoKSA9PiB7CiAgICAgICAgICAgICAgICAgIGNvbnN0IHNlbGVjdEl0",
    "ZW1zID0gWwogICAgICAgICAgICAgICAgICAgIHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291",
    "cmNlUmVmOiB7IFNvdXJjZTogImxtIiB9IH0sIFByb3BlcnR5OiAiQVRDNV9Lb2RlX1Rla3N0IiB9",
    "LCBOYW1lOiAiRGltTGFlZ2VtaWRkZWwuQVRDNV9Lb2RlX1Rla3N0IiB9CiAgICAgICAgICAgICAg",
    "ICAgIF07CiAgICAgICAgICAgICAgICAgIGlmIChpc1ZhcmVudW1tZXJMZXZlbCkgewogICAgICAg",
    "ICAgICAgICAgICAgIHNlbGVjdEl0ZW1zLnB1c2goCiAgICAgICAgICAgICAgICAgICAgICB7IENv",
    "bHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6ICJsbSIgfSB9LCBQcm9w",
    "ZXJ0eTogIlZhcmVudW1tZXIiIH0sIE5hbWU6ICJEaW1MYWVnZW1pZGRlbC5WYXJlbnVtbWVyIiwg",
    "TmF0aXZlUmVmZXJlbmNlTmFtZTogIlZhcmVudW1tZXIiIH0sCiAgICAgICAgICAgICAgICAgICAg",
    "ICB7IENvbHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6ICJsbSIgfSB9",
    "LCBQcm9wZXJ0eTogIkxhZWdlbWlkZGVsdGVrc3Rfc2FtbGV0X3VuYXZuZSIgfSwgTmFtZTogIkRp",
    "bUxhZWdlbWlkZGVsLkxhZWdlbWlkZGVsdGVrc3Rfc2FtbGV0X3VuYXZuZSIsIE5hdGl2ZVJlZmVy",
    "ZW5jZU5hbWU6ICJMYWVnZW1pZGRlbHRla3N0X3NhbWxldF91bmF2bmUiIH0KICAgICAgICAgICAg",
    "ICAgICAgICApOwogICAgICAgICAgICAgICAgICB9CiAgICAgICAgICAgICAgICAgIHNlbGVjdEl0",
    "ZW1zLnB1c2goCiAgICAgICAgICAgICAgICAgICAgeyBDb2x1bW46IHsgRXhwcmVzc2lvbjogeyBT",
    "b3VyY2VSZWY6IHsgU291cmNlOiAiZGF0byIgfSB9LCBQcm9wZXJ0eTogIsOFcl9JRCIgfSwgTmFt",
    "ZTogIkRpbURhdG8uw4VyX0lEIiB9LAogICAgICAgICAgICAgICAgICAgIHsgQ29sdW1uOiB7IEV4",
    "cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTogImRhdG8iIH0gfSwgUHJvcGVydHk6ICJN",
    "w6VuZWTDhXIiIH0sIE5hbWU6ICJEaW1EYXRvLk3DpW5lZMOFciIgfSwKICAgICAgICAgICAgICAg",
    "ICAgICB7IENvbHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6ICJ1ZCIg",
    "fSB9LCBQcm9wZXJ0eTogIlVkc3RlZGVyX3R5cGUiIH0sIE5hbWU6ICJEaW1VZHN0ZWRlclR5cGUu",
    "VWRzdGVkZXJfdHlwZSIgfSwKICAgICAgICAgICAgICAgICAgICB7IENvbHVtbjogeyBFeHByZXNz",
    "aW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6ICJnZW8iIH0gfSwgUHJvcGVydHk6ICJCb3DDpmxz",
    "cmVnaW9uIiB9LCBOYW1lOiAiRGltQm9yZ2VyRGVtb2dyYWZpLkJvcMOmbHNyZWdpb24iIH0sCiAg",
    "ICAgICAgICAgICAgICAgICAgeyBNZWFzdXJlOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7",
    "IFNvdXJjZTogIiMiIH0gfSwgUHJvcGVydHk6ICJTZWxlY3RNZWFzdXJlMSIgfSwgTmFtZTogIiMg",
    "TWVhc3VyZXMuU2VsZWN0TWVhc3VyZTEiIH0KICAgICAgICAgICAgICAgICAgKTsKICAgICAgICAg",
    "ICAgICAgICAgcmV0dXJuIHNlbGVjdEl0ZW1zOwogICAgICAgICAgICAgICAgfSkoKSwKICAgICAg",
    "ICAgICAgICAgIFdoZXJlOiB3aGVyZUl0ZW1zLAogICAgICAgICAgICAgICAgT3JkZXJCeTogKCgp",
    "ID0+IHsKICAgICAgICAgICAgICAgICAgY29uc3Qgb3JkZXJJdGVtcyA9IFsKICAgICAgICAgICAg",
    "ICAgICAgICB7IERpcmVjdGlvbjogMSwgRXhwcmVzc2lvbjogeyBDb2x1bW46IHsgRXhwcmVzc2lv",
    "bjogeyBTb3VyY2VSZWY6IHsgU291cmNlOiAibG0iIH0gfSwgUHJvcGVydHk6ICJBVEM1X0tvZGVf",
    "VGVrc3QiIH0gfSB9CiAgICAgICAgICAgICAgICAgIF07CiAgICAgICAgICAgICAgICAgIGlmIChp",
    "c1ZhcmVudW1tZXJMZXZlbCkgewogICAgICAgICAgICAgICAgICAgIG9yZGVySXRlbXMucHVzaCgK",
    "ICAgICAgICAgICAgICAgICAgICAgIHsgRGlyZWN0aW9uOiAxLCBFeHByZXNzaW9uOiB7IENvbHVt",
    "bjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjogeyBTb3VyY2U6ICJsbSIgfSB9LCBQcm9wZXJ0",
    "eTogIlZhcmVudW1tZXIiIH0gfSB9LAogICAgICAgICAgICAgICAgICAgICAgeyBEaXJlY3Rpb246",
    "IDEsIEV4cHJlc3Npb246IHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNv",
    "dXJjZTogImxtIiB9IH0sIFByb3BlcnR5OiAiTGFlZ2VtaWRkZWx0ZWtzdF9zYW1sZXRfdW5hdm5l",
    "IiB9IH0gfQogICAgICAgICAgICAgICAgICAgICk7CiAgICAgICAgICAgICAgICAgIH0KICAgICAg",
    "ICAgICAgICAgICAgb3JkZXJJdGVtcy5wdXNoKAogICAgICAgICAgICAgICAgICAgIHsgRGlyZWN0",
    "aW9uOiAxLCBFeHByZXNzaW9uOiB7IENvbHVtbjogeyBFeHByZXNzaW9uOiB7IFNvdXJjZVJlZjog",
    "eyBTb3VyY2U6ICJkYXRvIiB9IH0sIFByb3BlcnR5OiAiw4VyX0lEIiB9IH0gfSwKICAgICAgICAg",
    "ICAgICAgICAgICB7IERpcmVjdGlvbjogMSwgRXhwcmVzc2lvbjogeyBDb2x1bW46IHsgRXhwcmVz",
    "c2lvbjogeyBTb3VyY2VSZWY6IHsgU291cmNlOiAiZGF0byIgfSB9LCBQcm9wZXJ0eTogIk3DpW5l",
    "ZMOFciIgfSB9IH0sCiAgICAgICAgICAgICAgICAgICAgeyBEaXJlY3Rpb246IDEsIEV4cHJlc3Np",
    "b246IHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291cmNlUmVmOiB7IFNvdXJjZTogInVkIiB9",
    "IH0sIFByb3BlcnR5OiAiVWRzdGVkZXJfdHlwZSIgfSB9IH0sCiAgICAgICAgICAgICAgICAgICAg",
    "eyBEaXJlY3Rpb246IDEsIEV4cHJlc3Npb246IHsgQ29sdW1uOiB7IEV4cHJlc3Npb246IHsgU291",
    "cmNlUmVmOiB7IFNvdXJjZTogImdlbyIgfSB9LCBQcm9wZXJ0eTogIkJvcMOmbHNyZWdpb24iIH0g",
    "fSB9CiAgICAgICAgICAgICAgICAgICk7CiAgICAgICAgICAgICAgICAgIHJldHVybiBvcmRlckl0",
    "ZW1zOwogICAgICAgICAgICAgICAgfSkoKQogICAgICAgICAgICAgIH0sCiAgICAgICAgICAgICAg",
    "QmluZGluZzogewogICAgICAgICAgICAgICAgUHJpbWFyeTogeyBHcm91cGluZ3M6IFt7IFByb2pl",
    "Y3Rpb25zOiBpc1ZhcmVudW1tZXJMZXZlbCA/IFswLCAxLCAyLCAzLCA0LCA1LCA2LCA3XSA6IFsw",
    "LCAxLCAyLCAzLCA0LCA1XSB9XSB9LAogICAgICAgICAgICAgICAgRGF0YVJlZHVjdGlvbjogeyBE",
    "YXRhVm9sdW1lOiA0LCBQcmltYXJ5OiB7IFdpbmRvdzogeyBDb3VudDogMzAwMDAgfSB9IH0sCiAg",
    "ICAgICAgICAgICAgICBWZXJzaW9uOiAxCiAgICAgICAgICAgICAgfSwKICAgICAgICAgICAgICBF",
    "eGVjdXRpb25NZXRyaWNzS2luZDogMQogICAgICAgICAgICB9CiAgICAgICAgICB9XQogICAgICAg",
    "IH0sCiAgICAgICAgUXVlcnlJZDogIiIKICAgICAgfV0sCiAgICAgIGNhbmNlbFF1ZXJpZXM6IFtd",
    "LAogICAgICBtb2RlbElkOiAiNTc1MzE5NTMiLAogICAgICB1c2VyUHJlZmVycmVkTG9jYWxlOiAi",
    "ZGEtREsiCiAgICB9OwogIH0KCiAgZnVuY3Rpb24gcGFyc2VMaXRlcmFsKHgpIHsKICAgIGlmICh4",
    "ID09IG51bGwpIHJldHVybiAiIjsKICAgIGlmICh0eXBlb2YgeCA9PT0gIm9iamVjdCIpIHsKICAg",
    "ICAgaWYgKHguTGl0ZXJhbCAmJiB4LkxpdGVyYWwuVmFsdWUgIT0gbnVsbCkgcmV0dXJuIHBhcnNl",
    "TGl0ZXJhbCh4LkxpdGVyYWwuVmFsdWUpOwogICAgICBpZiAoeC5WYWx1ZSAhPSBudWxsKSByZXR1",
    "cm4gcGFyc2VMaXRlcmFsKHguVmFsdWUpOwogICAgICBpZiAoeC5WICE9IG51bGwpIHJldHVybiBw",
    "YXJzZUxpdGVyYWwoeC5WKTsKICAgICAgcmV0dXJuICIiOwogICAgfQogICAgcmV0dXJuIFN0cmlu",
    "Zyh4KS5yZXBsYWNlKC9eJ3wnJC9nLCAiIikucmVwbGFjZSgvXlxcdTAwMjd8XFx1MDAyNyQvZywg",
    "IiIpOwogIH0KCiAgZnVuY3Rpb24gd2FsayhvYmosIGZuKSB7CiAgICBpZiAob2JqID09IG51bGwg",
    "fHwgdHlwZW9mIG9iaiAhPT0gIm9iamVjdCIpIHJldHVybjsKICAgIGZuKG9iaik7CiAgICBpZiAo",
    "QXJyYXkuaXNBcnJheShvYmopKSB7CiAgICAgIGZvciAoY29uc3QgdiBvZiBvYmopIHdhbGsodiwg",
    "Zm4pOwogICAgfSBlbHNlIHsKICAgICAgZm9yIChjb25zdCBrIG9mIE9iamVjdC5rZXlzKG9iaikp",
    "IHdhbGsob2JqW2tdLCBmbik7CiAgICB9CiAgfQoKICBmdW5jdGlvbiBleHRyYWN0RGljdGlvbmFy",
    "aWVzKHJvb3QpIHsKICAgIGNvbnN0IGRpY3RzID0ge307CiAgICBmdW5jdGlvbiBpbm5lcih4LCBw",
    "YXRoKSB7CiAgICAgIGlmICh4ID09IG51bGwgfHwgdHlwZW9mIHggIT09ICJvYmplY3QiKSByZXR1",
    "cm47CiAgICAgIGlmICghQXJyYXkuaXNBcnJheSh4KSkgewogICAgICAgIGZvciAoY29uc3QgW2ss",
    "IHZdIG9mIE9iamVjdC5lbnRyaWVzKHgpKSB7CiAgICAgICAgICBpZiAoL15EKE58WzAtOV0rKSQv",
    "LnRlc3QoaykgJiYgdiAhPSBudWxsICYmIHR5cGVvZiB2ID09PSAib2JqZWN0IikgewogICAgICAg",
    "ICAgICBjb25zdCB2YWxzID0gQXJyYXkuaXNBcnJheSh2KSA/IHYgOiBPYmplY3QudmFsdWVzKHYp",
    "OwogICAgICAgICAgICBkaWN0c1tbLi4ucGF0aCwga10uam9pbigiLiIpXSA9IHZhbHMubWFwKHBh",
    "cnNlTGl0ZXJhbCk7CiAgICAgICAgICB9CiAgICAgICAgfQogICAgICAgIGZvciAoY29uc3QgW2ss",
    "IHZdIG9mIE9iamVjdC5lbnRyaWVzKHgpKSBpbm5lcih2LCBbLi4ucGF0aCwga10pOwogICAgICB9",
    "IGVsc2UgewogICAgICAgIHguZm9yRWFjaCgodiwgaSkgPT4gaW5uZXIodiwgWy4uLnBhdGgsIFN0",
    "cmluZyhpKV0pKTsKICAgICAgfQogICAgfQogICAgaW5uZXIocm9vdCwgW10pOwogICAgcmV0dXJu",
    "IGRpY3RzOwogIH0KCiAgZnVuY3Rpb24gcmVzb2x2ZURpY3RSZWYocmVmLCBkaWN0cywgcHJlZmVy",
    "cmVkSW5kZXgsIHN0cmljdFByZWZlcnJlZCA9IGZhbHNlKSB7CiAgICBjb25zdCBpZHggPSBOdW1i",
    "ZXIocmVmKTsKICAgIGlmICghTnVtYmVyLmlzRmluaXRlKGlkeCkpIHJldHVybiAiIjsKICAgIGNv",
    "bnN0IGtleXMgPSBPYmplY3Qua2V5cyhkaWN0cyk7CiAgICBpZiAocHJlZmVycmVkSW5kZXggIT0g",
    "bnVsbCkgewogICAgICBjb25zdCBwcmVmZXJyZWRLZXlzID0ga2V5cy5maWx0ZXIoayA9PiBrLmVu",
    "ZHNXaXRoKGAuRCR7cHJlZmVycmVkSW5kZXh9YCkgfHwgayA9PT0gYEQke3ByZWZlcnJlZEluZGV4",
    "fWApOwogICAgICBmb3IgKGNvbnN0IGsgb2YgcHJlZmVycmVkS2V5cykgewogICAgICAgIGlmIChp",
    "ZHggKyAxIDw9IGRpY3RzW2tdLmxlbmd0aCkgcmV0dXJuIGRpY3RzW2tdW2lkeF07CiAgICAgIH0K",
    "ICAgICAgaWYgKHN0cmljdFByZWZlcnJlZCkgcmV0dXJuICIiOwogICAgfQogICAgZm9yIChjb25z",
    "dCBrIG9mIGtleXMuZmlsdGVyKGsgPT4gay5lbmRzV2l0aCgiLkROIikgfHwgayA9PT0gIkROIikp",
    "IHsKICAgICAgaWYgKGlkeCArIDEgPD0gZGljdHNba10ubGVuZ3RoKSByZXR1cm4gZGljdHNba11b",
    "aWR4XTsKICAgIH0KICAgIGZvciAoY29uc3QgayBvZiBrZXlzKSB7CiAgICAgIGlmIChpZHggKyAx",
    "IDw9IGRpY3RzW2tdLmxlbmd0aCkgcmV0dXJuIGRpY3RzW2tdW2lkeF07CiAgICB9CiAgICByZXR1",
    "cm4gIiI7CiAgfQoKICBmdW5jdGlvbiByZXNvbHZlUmF3VmFsdWVJZlRleHREaW1lbnNpb24oY2Vs",
    "bCwgZGljdHMsIGNvbEluZGV4KSB7CiAgICBjb25zdCBsYXN0VGV4dENvbHVtbiA9IGlzVmFyZW51",
    "bW1lckxldmVsID8gNiA6IDQ7CiAgICBpZiAoY2VsbCA9PSBudWxsIHx8IGNlbGwuViA9PSBudWxs",
    "IHx8IGNvbEluZGV4ID09IG51bGwgfHwgY29sSW5kZXggPiBsYXN0VGV4dENvbHVtbikgcmV0dXJu",
    "ICIiOwogICAgY29uc3QgcmF3ID0gU3RyaW5nKGNlbGwuVik7CiAgICBpZiAoIS9eXGQrJC8udGVz",
    "dChyYXcpKSByZXR1cm4gIiI7CiAgICByZXR1cm4gcmVzb2x2ZURpY3RSZWYoY2VsbC5WLCBkaWN0",
    "cywgY29sSW5kZXgsIHRydWUpOwogIH0KCiAgZnVuY3Rpb24gZXh0cmFjdFJvd3Mocm9vdCkgewog",
    "ICAgY29uc3Qgcm93cyA9IFtdOwogICAgd2Fsayhyb290LCB4ID0+IHsKICAgICAgaWYgKHggJiYg",
    "dHlwZW9mIHggPT09ICJvYmplY3QiICYmIEFycmF5LmlzQXJyYXkoeC5DKSkgcm93cy5wdXNoKHgp",
    "OwogICAgfSk7CiAgICByZXR1cm4gcm93czsKICB9CgogIGZ1bmN0aW9uIHZhbHVlRnJvbUNlbGwo",
    "Y2VsbCwgZGljdHMsIGNvbEluZGV4KSB7CiAgICBpZiAoY2VsbCA9PSBudWxsKSByZXR1cm4gIiI7",
    "CiAgICBpZiAodHlwZW9mIGNlbGwgIT09ICJvYmplY3QiKSByZXR1cm4gU3RyaW5nKGNlbGwpOwoK",
    "ICAgIC8vIFBvd2VyIEJJLWtvbXByaW1lcmVkZSBzdmFyIGJydWdlciBvZnRlIGNlbGwuViBzb20g",
    "csOlIGRpY3Rpb25hcnktaW5kZWtzCiAgICAvLyBzYW1tZW4gbWVkIEQvRDAvRDEvLi4uIC4gRGlj",
    "dGlvbmFyeS1yZWZlcmVuY2VuIHNrYWwgZGVyZm9yIGzDpnNlcyBmw7hyIFYsCiAgICAvLyBlbGxl",
    "cnMgZW5kZXIgQVRDLCDDpXIsIHlkZXJ0eXBlIG9zdi4gc29tIDAvMS8yIGkgZWtzcG9ydGVuLgog",
    "ICAgZm9yIChjb25zdCBrZXkgb2YgWyJETiIsICJEIiwgIkQwIiwgIkQxIiwgIkQyIiwgIkQzIiwg",
    "IkQ0IiwgIkQ1IiwgIkQ2IiwgIkQ3IiwgIkQ4IiwgIkQ5Il0pIHsKICAgICAgaWYgKGNlbGxba2V5",
    "XSAhPSBudWxsKSB7CiAgICAgICAgY29uc3QgbSA9IC9eRChcZCspJC8uZXhlYyhrZXkpOwogICAg",
    "ICAgIGNvbnN0IHByZWZlcnJlZCA9IG0gPyBOdW1iZXIobVsxXSkgOiBjb2xJbmRleDsKICAgICAg",
    "ICByZXR1cm4gcmVzb2x2ZURpY3RSZWYoY2VsbFtrZXldLCBkaWN0cywgcHJlZmVycmVkKTsKICAg",
    "ICAgfQogICAgfQoKICAgIGlmIChjZWxsLlZhbHVlICE9IG51bGwpIHJldHVybiBwYXJzZUxpdGVy",
    "YWwoY2VsbC5WYWx1ZSk7CiAgICBpZiAoY2VsbC5MaXRlcmFsICYmIGNlbGwuTGl0ZXJhbC5WYWx1",
    "ZSAhPSBudWxsKSByZXR1cm4gcGFyc2VMaXRlcmFsKGNlbGwuTGl0ZXJhbC5WYWx1ZSk7CgogICAg",
    "Y29uc3QgcmVzb2x2ZWRSYXcgPSByZXNvbHZlUmF3VmFsdWVJZlRleHREaW1lbnNpb24oY2VsbCwg",
    "ZGljdHMsIGNvbEluZGV4KTsKICAgIGlmIChyZXNvbHZlZFJhdyAhPT0gIiIpIHJldHVybiByZXNv",
    "bHZlZFJhdzsKCiAgICBpZiAoY2VsbC5WICE9IG51bGwpIHJldHVybiBwYXJzZUxpdGVyYWwoY2Vs",
    "bC5WKTsKCiAgICBjb25zdCB2YWxzID0gT2JqZWN0LnZhbHVlcyhjZWxsKTsKICAgIGlmICh2YWxz",
    "Lmxlbmd0aCA9PT0gMSkgcmV0dXJuIHZhbHVlRnJvbUNlbGwodmFsc1swXSwgZGljdHMsIGNvbElu",
    "ZGV4KTsKICAgIHJldHVybiAiIjsKICB9CgogIGZ1bmN0aW9uIHJvd1JldXNlTWFzayhyb3cpIHsK",
    "ICAgIGNvbnN0IHIgPSBOdW1iZXIocm93LlIpOwogICAgaWYgKCFOdW1iZXIuaXNGaW5pdGUocikp",
    "IHJldHVybiBbXTsKICAgIGNvbnN0IG91dCA9IFtdOwogICAgZm9yIChsZXQgaSA9IDA7IGkgPCAz",
    "MjsgaSsrKSBpZiAoKHIgJiAoMSA8PCBpKSkgIT09IDApIG91dC5wdXNoKGkpOwogICAgcmV0dXJu",
    "IG91dDsKICB9CgogIGZ1bmN0aW9uIHBhcnNlUGJpTnVtYmVyKHgpIHsKICAgIGlmICh4ID09IG51",
    "bGwgfHwgeCA9PT0gIiIpIHJldHVybiAwOwogICAgaWYgKHR5cGVvZiB4ID09PSAibnVtYmVyIikg",
    "cmV0dXJuIHg7CiAgICBjb25zdCBzID0gU3RyaW5nKHgpLnJlcGxhY2UoL0QkL2csICIiKS5yZXBs",
    "YWNlKC9ccy9nLCAiIikucmVwbGFjZSgiLCIsICIuIik7CiAgICBjb25zdCBuID0gTnVtYmVyKHMp",
    "OwogICAgcmV0dXJuIE51bWJlci5pc0Zpbml0ZShuKSA/IG4gOiAwOwogIH0KCiAgZnVuY3Rpb24g",
    "Zm9ybWF0QXRjNSh4KSB7CiAgICByZXR1cm4gU3RyaW5nKHggfHwgIiIpLnJlcGxhY2UoL14oW0Et",
    "Wl1bMC05XXsyfVtBLVpdezEsMn1bMC05XXswLDR9KVxzKlwoKC4rKVwpJC8sICIkMSAtICQyIik7",
    "CiAgfQoKCiAgZnVuY3Rpb24gbm9ybWFsaXNlTW9udGhMYWJlbCh4KSB7CiAgICBjb25zdCByYXcg",
    "PSBTdHJpbmcoeCA9PSBudWxsID8gIiIgOiB4KS50cmltKCkucmVwbGFjZSgvRCQvZywgIiIpOwog",
    "ICAgaWYgKHJhdyA9PT0gIiIpIHJldHVybiAiIjsKICAgIGNvbnN0IG1vbnRoTmFtZXMgPSBbIkph",
    "bnVhciIsICJGZWJydWFyIiwgIk1hcnRzIiwgIkFwcmlsIiwgIk1haiIsICJKdW5pIiwgIkp1bGki",
    "LCAiQXVndXN0IiwgIlNlcHRlbWJlciIsICJPa3RvYmVyIiwgIk5vdmVtYmVyIiwgIkRlY2VtYmVy",
    "Il07CiAgICBpZiAoL15cZCskLy50ZXN0KHJhdykpIHsKICAgICAgY29uc3QgbiA9IE51bWJlcihy",
    "YXcpOwogICAgICBpZiAoTnVtYmVyLmlzRmluaXRlKG4pICYmIG4gPj0gMCkgcmV0dXJuIG1vbnRo",
    "TmFtZXNbbiAlIDEyXTsKICAgIH0KICAgIHJldHVybiByYXc7CiAgfQoKICBmdW5jdGlvbiBub3Jt",
    "YWxpc2VZZGVydHlwZSh4KSB7CiAgICBjb25zdCByYXcgPSBTdHJpbmcoeCA9PSBudWxsID8gIiIg",
    "OiB4KS50cmltKCkucmVwbGFjZSgvRCQvZywgIiIpOwogICAgY29uc3QgbWFwID0gewogICAgICAi",
    "MCI6ICJBbG1lbiBwcmFrc2lzIiwKICAgICAgIjEiOiAiSG9zcGl0YWxzbMOmZ2UiLAogICAgICAi",
    "MiI6ICLDmHZyaWdlIHVkc3RlZGVyZSIsCiAgICAgICIzIjogIlNwZWNpYWxsw6ZnZSIsCiAgICAg",
    "ICI0IjogIlVrZW5kdCIKICAgIH07CiAgICByZXR1cm4gbWFwW3Jhd10gfHwgcmF3OwogIH0KCiAg",
    "ZnVuY3Rpb24gc3BsaXRMYWVnZW1pZGRlbHRla3N0KHgpIHsKICAgIGNvbnN0IHBhcnRzID0gU3Ry",
    "aW5nKHggfHwgIiIpLnNwbGl0KC9ccypcfFxzKi8pOwogICAgd2hpbGUgKHBhcnRzLmxlbmd0aCA8",
    "IDUpIHBhcnRzLnB1c2goIiIpOwogICAgcmV0dXJuIHsKICAgICAgdmFyZW51bW1lcjogcGFydHNb",
    "MF0gfHwgIiIsCiAgICAgIG5hdm46IHBhcnRzWzFdIHx8ICIiLAogICAgICBmb3JtOiBwYXJ0c1sy",
    "XSB8fCAiIiwKICAgICAgc3R5cmtlOiBwYXJ0c1szXSB8fCAiIiwKICAgICAgcGFrbmluZzogcGFy",
    "dHMuc2xpY2UoNCkuam9pbigiIHwgIikgfHwgIiIKICAgIH07CiAgfQoKICBmdW5jdGlvbiBwYXJz",
    "ZVJlc3BvbnNlVG9Sb3dzKHJlc3BvbnNlLCBtZXRhKSB7CiAgICBpZiAoIXJlc3BvbnNlKSByZXR1",
    "cm4gW107CiAgICBjb25zdCBkaWN0cyA9IGV4dHJhY3REaWN0aW9uYXJpZXMocmVzcG9uc2UpOwog",
    "ICAgY29uc3QgcGJpUm93cyA9IGV4dHJhY3RSb3dzKHJlc3BvbnNlKTsKICAgIGNvbnN0IG91dCA9",
    "IFtdOwogICAgY29uc3QgY29sdW1uQ291bnQgPSBpc1ZhcmVudW1tZXJMZXZlbCA/IDggOiA2Owog",
    "ICAgbGV0IHByZXZpb3VzID0gQXJyYXkoY29sdW1uQ291bnQpLmZpbGwoIiIpOwoKICAgIGZvciAo",
    "Y29uc3Qgcm93IG9mIHBiaVJvd3MpIHsKICAgICAgY29uc3QgY3VycmVudCA9IEFycmF5KGNvbHVt",
    "bkNvdW50KS5maWxsKCIiKTsKICAgICAgZm9yIChjb25zdCBjb2wwIG9mIHJvd1JldXNlTWFzayhy",
    "b3cpKSB7CiAgICAgICAgaWYgKGNvbDAgPj0gMCAmJiBjb2wwIDwgY3VycmVudC5sZW5ndGgpIGN1",
    "cnJlbnRbY29sMF0gPSBwcmV2aW91c1tjb2wwXTsKICAgICAgfQoKICAgICAgY29uc3QgbWlzc2lu",
    "ZyA9IFtdOwogICAgICBmb3IgKGxldCBpID0gMDsgaSA8IGN1cnJlbnQubGVuZ3RoOyBpKyspIGlm",
    "IChjdXJyZW50W2ldID09PSAiIikgbWlzc2luZy5wdXNoKGkpOwogICAgICBjb25zdCBjZWxscyA9",
    "IEFycmF5LmlzQXJyYXkocm93LkMpID8gcm93LkMgOiBbXTsKICAgICAgZm9yIChsZXQgaiA9IDA7",
    "IGogPCBjZWxscy5sZW5ndGg7IGorKykgewogICAgICAgIGNvbnN0IHBvcyA9IGogPCBtaXNzaW5n",
    "Lmxlbmd0aCA/IG1pc3Npbmdbal0gOiBqOwogICAgICAgIGlmIChwb3MgPj0gMCAmJiBwb3MgPCBj",
    "dXJyZW50Lmxlbmd0aCkgY3VycmVudFtwb3NdID0gdmFsdWVGcm9tQ2VsbChjZWxsc1tqXSwgZGlj",
    "dHMsIHBvcyk7CiAgICAgIH0KCiAgICAgIHByZXZpb3VzID0gY3VycmVudDsKICAgICAgY29uc3Qg",
    "bWVkID0gaXNWYXJlbnVtbWVyTGV2ZWwgPyBzcGxpdExhZWdlbWlkZGVsdGVrc3QoY3VycmVudFsy",
    "XSkgOiBzcGxpdExhZWdlbWlkZGVsdGVrc3QoIiIpOwogICAgICBjb25zdCBkaXJlY3RWYXJlbnVt",
    "bWVyID0gU3RyaW5nKGN1cnJlbnRbMV0gfHwgIiIpLnRyaW0oKTsKICAgICAgY29uc3QgbWVkVmFy",
    "ZW51bW1lciA9IFN0cmluZyhtZWQudmFyZW51bW1lciB8fCAiIikudHJpbSgpOwogICAgICBjb25z",
    "dCB2YXJlbnVtbWVyID0gL15cZHs2fSQvLnRlc3QoZGlyZWN0VmFyZW51bW1lcikgPyBkaXJlY3RW",
    "YXJlbnVtbWVyIDogbWVkVmFyZW51bW1lcjsKICAgICAgY29uc3QgcHJvZHVjdEZpZWxkc0NvbXBs",
    "ZXRlID0gW21lZC5uYXZuLCBtZWQuZm9ybSwgbWVkLnN0eXJrZSwgbWVkLnBha25pbmddCiAgICAg",
    "ICAgLmV2ZXJ5KHggPT4gU3RyaW5nKHggfHwgIiIpLnRyaW0oKSAhPT0gIiIpOwogICAgICBpZiAo",
    "aXNWYXJlbnVtbWVyTGV2ZWwgJiYgKCEvXlxkezZ9JC8udGVzdCh2YXJlbnVtbWVyKSB8fCAhcHJv",
    "ZHVjdEZpZWxkc0NvbXBsZXRlKSkgY29udGludWU7CgogICAgICBvdXQucHVzaCh7CiAgICAgICAg",
    "IkFUQywgTml2ZWF1IDUsIGtvZGUgJiB0ZWtzdCI6IGZvcm1hdEF0YzUoY3VycmVudFswXSksCiAg",
    "ICAgICAgIlZhcmVudW1tZXIiOiBpc1ZhcmVudW1tZXJMZXZlbCA/IHZhcmVudW1tZXIgOiAiIiwK",
    "ICAgICAgICAiTmF2biAoUHLDpnBhcmF0KSI6IGlzVmFyZW51bW1lckxldmVsID8gbWVkLm5hdm4g",
    "OiAiIiwKICAgICAgICAiRm9ybSI6IGlzVmFyZW51bW1lckxldmVsID8gbWVkLmZvcm0gOiAiIiwK",
    "ICAgICAgICAiU3R5cmtlIjogaXNWYXJlbnVtbWVyTGV2ZWwgPyBtZWQuc3R5cmtlIDogIiIsCiAg",
    "ICAgICAgIlBha25pbmdzc3TDuHJyZWxzZSI6IGlzVmFyZW51bW1lckxldmVsID8gbWVkLnBha25p",
    "bmcgOiAiIiwKICAgICAgICAiw4VyIjogU3RyaW5nKChpc1ZhcmVudW1tZXJMZXZlbCA/IGN1cnJl",
    "bnRbM10gOiBjdXJyZW50WzFdKSB8fCBtZXRhLnllYXIgfHwgIiIpLnJlcGxhY2UoL0QkL2csICIi",
    "KSwKICAgICAgICAiTcOlbmVkIjogbm9ybWFsaXNlTW9udGhMYWJlbChpc1ZhcmVudW1tZXJMZXZl",
    "bCA/IGN1cnJlbnRbNF0gOiBjdXJyZW50WzJdKSwKICAgICAgICAiWWRlcnR5cGUiOiBub3JtYWxp",
    "c2VZZGVydHlwZShpc1ZhcmVudW1tZXJMZXZlbCA/IGN1cnJlbnRbNV0gOiBjdXJyZW50WzNdKSwK",
    "ICAgICAgICAiQm9ww6Zsc3JlZ2lvbiI6IGlzVmFyZW51bW1lckxldmVsID8gY3VycmVudFs2XSA6",
    "IGN1cnJlbnRbNF0sCiAgICAgICAgIm7DuGdsZXRhbF9vdXRwdXQiOiBvdXRwdXRNZXRyaWNOYW1l",
    "KG1ldGEub3Bnb2VyZWxzZSwgbWV0YS5ub3JtYWxpc2VyaW5nKSwKICAgICAgICAidmFsdWUiOiBw",
    "YXJzZVBiaU51bWJlcihpc1ZhcmVudW1tZXJMZXZlbCA/IGN1cnJlbnRbN10gOiBjdXJyZW50WzVd",
    "KSwKICAgICAgICAiYXRjX2ZpbHRlciI6IG1ldGEuYXRjLAogICAgICAgICJyZWdpb25fZmlsdGVy",
    "IjogbWV0YS5yZWdpb24sCiAgICAgICAgInllYXJfZmlsdGVyIjogQXJyYXkuaXNBcnJheShtZXRh",
    "LnllYXIpID8gbWV0YS55ZWFyLmpvaW4oIiwgIikgOiBTdHJpbmcobWV0YS55ZWFyKSwKICAgICAg",
    "ICAib3Bnb2VyZWxzZSI6IG1ldGEub3Bnb2VyZWxzZSwKICAgICAgICAibm9ybWFsaXNlcmluZyI6",
    "IG1ldGEubm9ybWFsaXNlcmluZywKICAgICAgICAib3Bnb2VyZWxzZXNuaXZlYXUiOiBvdXRwdXRM",
    "ZXZlbCwKICAgICAgICAic3RhdHVzIjogbWV0YS5zdGF0dXMsCiAgICAgICAgInJvd0NvdW50TWFy",
    "a2VyIjogbWV0YS5yb3dDb3VudE1hcmtlciwKICAgICAgICAiaGFzUmVzdGFydFRva2VuIjogbWV0",
    "YS5oYXNSZXN0YXJ0VG9rZW4sCiAgICAgICAgImV4cG9ydF9lcnJvciI6IG1ldGEuZXJyb3IgfHwg",
    "IiIKICAgICAgfSk7CiAgICB9CiAgICByZXR1cm4gb3V0OwogIH0KCiAgZnVuY3Rpb24gY3N2RXNj",
    "YXBlKHYpIHsKICAgIGlmICh2ID09IG51bGwpIHJldHVybiAiIjsKICAgIGlmICh0eXBlb2YgdiA9",
    "PT0gIm51bWJlciIpIHYgPSBTdHJpbmcodikucmVwbGFjZSgiLiIsICIsIik7CiAgICBlbHNlIHYg",
    "PSBTdHJpbmcodik7CiAgICBpZiAoL1siO1xyXG5dLy50ZXN0KHYpKSB2ID0gJyInICsgdi5yZXBs",
    "YWNlQWxsKCciJywgJyIiJykgKyAnIic7CiAgICByZXR1cm4gdjsKICB9CgogIGZ1bmN0aW9uIHJv",
    "d3NUb0Nzdihyb3dzKSB7CiAgICBjb25zdCBsaW5lcyA9IFtoZWFkZXJzLm1hcChjc3ZFc2NhcGUp",
    "LmpvaW4oIjsiKV07CiAgICBmb3IgKGNvbnN0IHIgb2Ygcm93cykgbGluZXMucHVzaChoZWFkZXJz",
    "Lm1hcChoID0+IGNzdkVzY2FwZShyW2hdKSkuam9pbigiOyIpKTsKICAgIHJldHVybiAiXHVmZWZm",
    "IiArIGxpbmVzLmpvaW4oIlxyXG4iKTsKICB9CgogIGZ1bmN0aW9uIGRvd25sb2FkVGV4dChmaWxl",
    "bmFtZSwgdGV4dCwgdHlwZSkgewogICAgY29uc3QgYmxvYiA9IG5ldyBCbG9iKFt0ZXh0XSwgeyB0",
    "eXBlOiB0eXBlIHx8ICJ0ZXh0L3BsYWluO2NoYXJzZXQ9dXRmLTgiIH0pOwogICAgY29uc3QgYSA9",
    "IGRvY3VtZW50LmNyZWF0ZUVsZW1lbnQoImEiKTsKICAgIGEuaHJlZiA9IFVSTC5jcmVhdGVPYmpl",
    "Y3RVUkwoYmxvYik7CiAgICBhLmRvd25sb2FkID0gZmlsZW5hbWU7CiAgICBkb2N1bWVudC5ib2R5",
    "LmFwcGVuZENoaWxkKGEpOwogICAgYS5jbGljaygpOwogICAgYS5yZW1vdmUoKTsKICAgIHNldFRp",
    "bWVvdXQoKCkgPT4gVVJMLnJldm9rZU9iamVjdFVSTChhLmhyZWYpLCA1MDAwKTsKICB9CgogIGFz",
    "eW5jIGZ1bmN0aW9uIHJ1bk9uZSh7IGF0YywgcmVnaW9uLCB5ZWFyLCBvcGdvZXJlbHNlLCBub3Jt",
    "YWxpc2VyaW5nIH0pIHsKICAgIGNvbnN0IHllYXJWYWx1ZXMgPSBBcnJheS5pc0FycmF5KHllYXIp",
    "ID8geWVhciA6IFt5ZWFyXTsKICAgIGNvbnN0IHBheWxvYWQgPSBidWlsZFBheWxvYWQoeyBhdGNW",
    "YWx1ZTogYXRjLCBtZXRyaWNWYWx1ZXM6IFtvcGdvZXJlbHNlXSwgcmVnaW9uVmFsdWVzOiBbcmVn",
    "aW9uXSwgeWVhclZhbHVlcywgbm9ybWFsaXNhdGlvblZhbHVlOiBub3JtYWxpc2VyaW5nIH0pOwog",
    "ICAgbGV0IHN0YXR1cyA9IG51bGwsIHRleHQgPSAiIiwgcGFyc2VkSnNvbiA9IG51bGwsIGVycm9y",
    "ID0gbnVsbDsKICAgIHRyeSB7CiAgICAgIGNvbnN0IHJlc3BvbnNlID0gYXdhaXQgZmV0Y2godXJs",
    "LCB7CiAgICAgICAgbWV0aG9kOiAiUE9TVCIsCiAgICAgICAgY3JlZGVudGlhbHM6ICJpbmNsdWRl",
    "IiwKICAgICAgICBoZWFkZXJzOiB7CiAgICAgICAgICAiYWNjZXB0IjogImFwcGxpY2F0aW9uL2pz",
    "b24sIHRleHQvcGxhaW4sICovKiIsCiAgICAgICAgICAiY29udGVudC10eXBlIjogImFwcGxpY2F0",
    "aW9uL2pzb247Y2hhcnNldD1VVEYtOCIsCiAgICAgICAgICAieC1wb3dlcmJpLXJlc291cmNla2V5",
    "IjogImFueSIKICAgICAgICB9LAogICAgICAgIGJvZHk6IEpTT04uc3RyaW5naWZ5KHBheWxvYWQp",
    "CiAgICAgIH0pOwogICAgICBzdGF0dXMgPSByZXNwb25zZS5zdGF0dXM7CiAgICAgIHRleHQgPSBh",
    "d2FpdCByZXNwb25zZS50ZXh0KCk7CiAgICAgIHRyeSB7IHBhcnNlZEpzb24gPSBKU09OLnBhcnNl",
    "KHRleHQpOyB9IGNhdGNoKGUpIHsgZXJyb3IgPSAiS3VubmUgaWtrZSBKU09OLXBhcnNlIHJlc3Bv",
    "bnNlOiAiICsgZS5tZXNzYWdlOyB9CiAgICB9IGNhdGNoKGUpIHsKICAgICAgZXJyb3IgPSAiRmV0",
    "Y2gtZmVqbDogIiArIGUubWVzc2FnZTsKICAgIH0KCiAgICBjb25zdCByb3dDb3VudE1hcmtlciA9",
    "IHRleHQuc3BsaXQoJyJDIjpbJykubGVuZ3RoIC0gMTsKICAgIGNvbnN0IGhhc1Jlc3RhcnRUb2tl",
    "biA9IHRleHQuaW5jbHVkZXMoJyJSVCInKTsKICAgIGNvbnN0IG1ldGEgPSB7IGF0YywgcmVnaW9u",
    "LCB5ZWFyLCBvcGdvZXJlbHNlLCBub3JtYWxpc2VyaW5nLCBzdGF0dXMsIHJvd0NvdW50TWFya2Vy",
    "LCBoYXNSZXN0YXJ0VG9rZW4sIGVycm9yIH07CiAgICBjb25zdCByb3dzID0gcGFyc2VSZXNwb25z",
    "ZVRvUm93cyhwYXJzZWRKc29uLCBtZXRhKTsKICAgIHJldHVybiB7IG1ldGE6IHsgLi4ubWV0YSwg",
    "cGFyc2VkUm93czogcm93cy5sZW5ndGggfSwgcm93cyB9OwogIH0KCiAgY29uc3QgdG90YWwgPSBz",
    "ZWxlY3RlZEF0Yy5sZW5ndGggKiBzZWxlY3RlZFJlZ2lvbnMubGVuZ3RoICogc2VsZWN0ZWRNZXRy",
    "aWNzLmxlbmd0aCAqIG5vcm1hbGlzYXRpb25PcHRpb25zLmxlbmd0aCAqIChzcGxpdEJ5WWVhciA/",
    "IHNlbGVjdGVkWWVhcnMubGVuZ3RoIDogMSk7CiAgY29uc3Qgc3VtbWFyeSA9IFtdOwogIGxldCBk",
    "b25lID0gMDsKICBsZXQgcGFydE5vID0gMTsKICBsZXQgYnVmZmVyID0gW107CgogIGZ1bmN0aW9u",
    "IGZsdXNoKGZvcmNlKSB7CiAgICBpZiAoYnVmZmVyLmxlbmd0aCA9PT0gMCkgcmV0dXJuOwogICAg",
    "aWYgKCFmb3JjZSAmJiBidWZmZXIubGVuZ3RoIDwgbWF4Um93c1BlckNzdikgcmV0dXJuOwogICAg",
    "Y29uc3QgZmlsZW5hbWUgPSBgbWVkaWNpbnNhbGdfcGFyc2VkX3BhcnRfJHtTdHJpbmcocGFydE5v",
    "KS5wYWRTdGFydCgzLCAiMCIpfS5jc3ZgOwogICAgZG93bmxvYWRUZXh0KGZpbGVuYW1lLCByb3dz",
    "VG9Dc3YoYnVmZmVyKSwgInRleHQvY3N2O2NoYXJzZXQ9dXRmLTgiKTsKICAgIGNvbnNvbGUubG9n",
    "KGBEb3dubG9hZGVkZSAke2ZpbGVuYW1lfSBtZWQgJHtidWZmZXIubGVuZ3RofSByw6Zra2VyYCk7",
    "CiAgICBwYXJ0Tm8gKz0gMTsKICAgIGJ1ZmZlciA9IFtdOwogIH0KCiAgZm9yIChjb25zdCBhdGMg",
    "b2Ygc2VsZWN0ZWRBdGMpIHsKICAgIGZvciAoY29uc3QgcmVnaW9uIG9mIHNlbGVjdGVkUmVnaW9u",
    "cykgewogICAgICBmb3IgKGNvbnN0IG9wZ29lcmVsc2Ugb2Ygc2VsZWN0ZWRNZXRyaWNzKSB7CiAg",
    "ICAgICAgZm9yIChjb25zdCBub3JtYWxpc2VyaW5nIG9mIG5vcm1hbGlzYXRpb25PcHRpb25zLm1h",
    "cCh4ID0+IHgudmFsdWUpKSB7CiAgICAgICAgICBpZiAoc3BsaXRCeVllYXIpIHsKICAgICAgICAg",
    "ICAgZm9yIChjb25zdCB5ZWFyIG9mIHNlbGVjdGVkWWVhcnMpIHsKICAgICAgICAgICAgICBkb25l",
    "Kys7CiAgICAgICAgICAgICAgY29uc29sZS5sb2coYFske2RvbmV9LyR7dG90YWx9XSBIZW50ZXJg",
    "LCB7IGF0YywgcmVnaW9uLCBvcGdvZXJlbHNlLCBub3JtYWxpc2VyaW5nLCB5ZWFyIH0pOwogICAg",
    "ICAgICAgICAgIGNvbnN0IHJlcyA9IGF3YWl0IHJ1bk9uZSh7IGF0YywgcmVnaW9uLCBvcGdvZXJl",
    "bHNlLCBub3JtYWxpc2VyaW5nLCB5ZWFyIH0pOwogICAgICAgICAgICAgIHN1bW1hcnkucHVzaChy",
    "ZXMubWV0YSk7CiAgICAgICAgICAgICAgYnVmZmVyLnB1c2goLi4ucmVzLnJvd3MpOwogICAgICAg",
    "ICAgICAgIGZsdXNoKGZhbHNlKTsKICAgICAgICAgICAgfQogICAgICAgICAgfSBlbHNlIHsKICAg",
    "ICAgICAgICAgZG9uZSsrOwogICAgICAgICAgICBjb25zb2xlLmxvZyhgWyR7ZG9uZX0vJHt0b3Rh",
    "bH1dIEhlbnRlcmAsIHsgYXRjLCByZWdpb24sIG9wZ29lcmVsc2UsIG5vcm1hbGlzZXJpbmcsIHll",
    "YXI6IHNlbGVjdGVkWWVhcnMgfSk7CiAgICAgICAgICAgIGNvbnN0IHJlcyA9IGF3YWl0IHJ1bk9u",
    "ZSh7IGF0YywgcmVnaW9uLCBvcGdvZXJlbHNlLCBub3JtYWxpc2VyaW5nLCB5ZWFyOiBzZWxlY3Rl",
    "ZFllYXJzIH0pOwogICAgICAgICAgICBzdW1tYXJ5LnB1c2gocmVzLm1ldGEpOwogICAgICAgICAg",
    "ICBidWZmZXIucHVzaCguLi5yZXMucm93cyk7CiAgICAgICAgICAgIGZsdXNoKGZhbHNlKTsKICAg",
    "ICAgICAgIH0KICAgICAgICB9CiAgICAgIH0KICAgIH0KICB9CgogIGZsdXNoKHRydWUpOwogIGRv",
    "d25sb2FkVGV4dCgibWVkaWNpbnNhbGdfZXhwb3J0X3N1bW1hcnkuanNvbiIsIEpTT04uc3RyaW5n",
    "aWZ5KHN1bW1hcnksIG51bGwsIDIpLCAiYXBwbGljYXRpb24vanNvbjtjaGFyc2V0PXV0Zi04Iik7",
    "CgogIGNvbnN0IHRvdGFsUm93cyA9IHN1bW1hcnkucmVkdWNlKChzLCB4KSA9PiBzICsgKHgucGFy",
    "c2VkUm93cyB8fCAwKSwgMCk7CiAgY29uc3QgdHJ1bmNhdGVkID0gc3VtbWFyeS5maWx0ZXIoeCA9",
    "PiB4Lmhhc1Jlc3RhcnRUb2tlbiB8fCBOdW1iZXIoeC5yb3dDb3VudE1hcmtlcikgPj0gMjkwMDAp",
    "OwogIGNvbnNvbGUubG9nKGBGw6ZyZGlnLiBQYXJzZWRlICR7dG90YWxSb3dzfSByw6Zra2VyIGZv",
    "cmRlbHQgcMOlICR7cGFydE5vIC0gMX0gQ1NWLWZpbChlcikuYCwgc3VtbWFyeSk7CiAgaWYgKHRy",
    "dW5jYXRlZC5sZW5ndGggPiAwKSBjb25zb2xlLndhcm4oIk11bGlndCBhZmtvcnRlZGUgUG93ZXIg",
    "Qkktc3ZhcjoiLCB0cnVuY2F0ZWQpOwp9KSgpOw=="
  )
  js_template <- rawToChar(jsonlite::base64_dec(js_b64))
  js_template <- gsub("__ATC_JS__", atc_js, js_template, fixed = TRUE)
  js_template <- gsub("__METRICS_JS__", metrics_js, js_template, fixed = TRUE)
  js_template <- gsub("__REGIONS_JS__", regions_js, js_template, fixed = TRUE)
  js_template <- gsub("__YEARS_JS__", years_js, js_template, fixed = TRUE)
  js_template <- gsub("__SPLIT_JS__", split_js, js_template, fixed = TRUE)
  js_template <- gsub("__OUTPUT_LEVEL_JS__", output_level_js, js_template, fixed = TRUE)
  
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
    "Varenummer",
    "Navn (Præparat)",
    "Form",
    "Styrke",
    "Pakningsstørrelse",
    "År",
    "Måned",
    "Ydertype",
    "Bopælsregion",
    "nøgletal_output",
    "opgoerelsesniveau"
  )
  
  for (nm in needed) {
    if (!nm %in% names(long2)) long2[[nm]] <- NA_character_
  }
  
  has_product_detail <- long2 |>
    transmute(
      has_detail = if_any(
        all_of(c("Varenummer", "Navn (Præparat)", "Form", "Styrke", "Pakningsstørrelse")),
        ~ !is.na(.x) & str_trim(as.character(.x)) != ""
      )
    ) |>
    pull(has_detail) |>
    any(na.rm = TRUE)
  
  is_varenummer_level <- any(long2$opgoerelsesniveau == "varenummer", na.rm = TRUE) || has_product_detail
  
  base_keys <- c(
    "ATC, Niveau 5, kode & tekst",
    "År",
    "Måned",
    "Ydertype",
    "Bopælsregion"
  )
  
  product_keys <- c(
    "Varenummer",
    "Navn (Præparat)",
    "Form",
    "Styrke",
    "Pakningsstørrelse"
  )
  
  keys <- if (is_varenummer_level) {
    c("ATC, Niveau 5, kode & tekst", product_keys, "År", "Måned", "Ydertype", "Bopælsregion")
  } else {
    base_keys
  }
  
  cleaned_long <- long2 |>
    mutate(
      value = replace_na(value, 0),
      across(all_of(c("Varenummer", "Navn (Præparat)", "Form", "Styrke", "Pakningsstørrelse")), ~ na_if(str_trim(as.character(.x)), ""))
    )
  
  if (is_varenummer_level) {
    cleaned_long <- cleaned_long |>
      filter(
        str_detect(as.character(Varenummer), "^\\d{6}$"),
        if_all(
          all_of(c("Navn (Præparat)", "Form", "Styrke", "Pakningsstørrelse")),
          ~ !is.na(.x) & str_trim(as.character(.x)) != ""
        )
      )
  }
  
  out <- cleaned_long |>
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
    arrange(across(all_of(keys)))
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
      opgoerelsesniveau = "varenummer",
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
    
    radioButtons(
      "output_level",
      "Opgørelsesniveau",
      choices = c(
        "ATC5-niveau" = "atc5",
        "Varenummerniveau" = "varenummer"
      ),
      selected = "atc5"
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
      split_by_year = TRUE,
      output_level = input$output_level %||% "atc5"
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
      tags$code(paste(atc_mapped, collapse = "; ")),
      br(),
      strong("Opgørelsesniveau: "),
      if ((input$output_level %||% "atc5") == "varenummer") "Varenummerniveau" else "ATC5-niveau"
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
        if (!"opgoerelsesniveau" %in% names(long)) long$opgoerelsesniveau <- NA_character_
        
        parsed <- finalise_parsed_long_export(long)
        
        meta <- long |>
          distinct(atc_filter, region_filter, year_filter, opgoerelse, normalisering, opgoerelsesniveau, status, rowCountMarker, hasRestartToken, export_error) |>
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
