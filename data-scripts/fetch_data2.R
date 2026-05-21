#!/usr/bin/env Rscript
# fetch_data2.R — build clean_data2.rds from erhverv.medicinpriser.dk + G_Substitutionslisten
# -----------------------------------------------------------------------------
# Usage examples:
#   Rscript fetch_data2.R
#   Rscript fetch_data2.R data "Alle lægemidler.xlsx"
#
#
# Output:
#   data/clean_data2.rds
#   data/Taksten_YYYY-MM-DD.xlsx
# -----------------------------------------------------------------------------

suppressPackageStartupMessages({
  library(httr2)
  library(rvest)
  library(readxl)
  library(writexl)
  library(dplyr)
  library(stringr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
out_dir  <- if (length(args) >= 1) args[1] else "data"
cat_file <- if (length(args) >= 2) args[2] else "Alle lægemidler.xlsx"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

clean_key <- function(x) {
  x %>%
    as.character() %>%
    str_squish() %>%
    str_to_lower(locale = "da")
}

# *** ÆNDRET: robust tal-konvertering, der håndterer både dansk og engelsk format ***
as_num <- function(x) {
  if (is.numeric(x)) return(x)
  
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "-", "NA", "NaN")] <- NA_character_
  
  x <- gsub("\\s", "", x)
  
  # Hvis både punktum og komma findes, antages punktum = tusindtalsseparator og komma = decimal
  both <- grepl("\\.", x) & grepl(",", x)
  x[both] <- gsub("\\.", "", x[both])
  x[both] <- gsub(",", ".", x[both])
  
  # Hvis kun komma findes, antages komma = decimal
  comma_only <- !both & grepl(",", x)
  x[comma_only] <- gsub(",", ".", x[comma_only])
  
  # Hvis kun punktum findes, bevares det som decimalpunktum
  suppressWarnings(as.numeric(x))
}

normalise_delivery <- function(x) {
  x_chr <- str_squish(as.character(x))
  dplyr::case_when(
    is.na(x_chr) | x_chr == "" | x_chr == "-" ~ NA_character_,
    grepl("^ja$|kan leveres|leveres", x_chr, ignore.case = TRUE) &
      !grepl("ikke|nej|leveringssvigt", x_chr, ignore.case = TRUE) ~ "Ja",
    grepl("^nej$|ikke|leveringssvigt", x_chr, ignore.case = TRUE) ~ "Nej",
    TRUE ~ x_chr
  )
}

# *** NYT: rydder tekst uden at fjerne meningsfulde enheder/detaljer ***
blank_to_na <- function(x) {
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "-", "NA", "NaN")] <- NA_character_
  x
}

# *** NYT: laver styrke med enhed, fx 250 + mg -> 250 mg ***
make_styrke <- function(styrke, enhed) {
  styrke <- blank_to_na(styrke)
  enhed  <- blank_to_na(enhed)
  
  already_has_unit <- grepl("[[:alpha:]%µμ]", styrke)
  
  dplyr::case_when(
    is.na(styrke) ~ NA_character_,
    already_has_unit ~ styrke,
    !is.na(enhed) ~ paste(styrke, enhed),
    TRUE ~ styrke
  )
}

# *** NYT: bevarer pakningsdetaljer, fx 250 mg eller 250 mg (blister) ***
make_pakning <- function(pakning, pakningsenhed = NA_character_) {
  pakning <- blank_to_na(pakning)
  pakningsenhed <- blank_to_na(pakningsenhed)
  
  already_has_unit <- grepl("[[:alpha:]%µμ]", pakning)
  
  dplyr::case_when(
    is.na(pakning) ~ NA_character_,
    already_has_unit ~ pakning,
    !is.na(pakningsenhed) ~ paste(pakning, pakningsenhed),
    TRUE ~ pakning
  )
}

# *** NYT: mere tolerant nøgle til styrke ved G-substitutions-join ***
clean_strength_key <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower(locale = "da") %>%
    stringr::str_replace_all(",", ".") %>%
    stringr::str_replace_all("mikrogram|microgram|mcg|μg", "µg") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_replace_all("\\s*/\\s*", "/") %>%
    stringr::str_replace_all("\\s*\\+\\s*", "+")
}

# -----------------------------------------------------------------------------
# 1) Credentials
# -----------------------------------------------------------------------------

email <- "kliniskfarmakologiskenhed@rn.dk"
pw    <- "kfekfe"

if (identical(email, "") || identical(pw, "")) {
  stop(
    "Missing credentials. Add MEDPRIS_EMAIL and MEDPRIS_PASSWORD to ~/.Renviron ",
    "or to the environment used by cron/shiny-server."
  )
}

# -----------------------------------------------------------------------------
# 2) Configuration
# -----------------------------------------------------------------------------

base_url    <- "https://erhverv.medicinpriser.dk"
login_url   <- base_url
sogning_url <- paste0(base_url, "/sogning.aspx")
cookie_file <- tempfile()

browser_headers <- c(
  "User-Agent"      = paste0(
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) ",
    "AppleWebKit/537.36 (KHTML, like Gecko) ",
    "Chrome/120.0.0.0 Safari/537.36"
  ),
  "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  "Accept-Language" = "da,en-US;q=0.9,en;q=0.8"
)

udleveringsgrupper <- c(
  "A", "AP", "AP4", "AP4BG", "AP4NB", "APK",
  "B", "BEGR", "BP", "BPK",
  "GH",
  "HA", "HA18", "HF", "HP", "HV", "HX", "HX18",
  "NBS", "PRMIX", "Udgaaet"
)

pris_kolonner <- c(
  "AIP (kr.)",
  "ESP pr. enhed (kr.)",
  "ESP (kr.)",
  "TSP (kr.)",
  "Pris pr. DDD (kr.)",
  "Pris for sygehusleverancer (kr.)",
  "Pris pr. enhed (D) (kr.)",
  "Tilskud beregnes af (D) (kr.)"
)

# -----------------------------------------------------------------------------
# 3) Helpers for ASP.NET form and Excel parsing
# -----------------------------------------------------------------------------

safe_attr <- function(node, attr_name) {
  val <- html_attr(node, attr_name)
  if (is.na(val)) "" else val
}

extract_all_form_fields <- function(page) {
  fields <- list()
  
  for (input in html_elements(page, "input[type='hidden']")) {
    name <- safe_attr(input, "name")
    if (nchar(name) > 0) fields[[name]] <- safe_attr(input, "value")
  }
  
  for (input in html_elements(page, "input[type='text']")) {
    name <- safe_attr(input, "name")
    if (nchar(name) > 0) fields[[name]] <- safe_attr(input, "value")
  }
  
  for (input in html_elements(page, "input[type='password']")) {
    name <- safe_attr(input, "name")
    if (nchar(name) > 0) fields[[name]] <- safe_attr(input, "value")
  }
  
  for (cb in html_elements(page, "input[type='checkbox'][checked]")) {
    name <- safe_attr(cb, "name")
    if (nchar(name) > 0) fields[[name]] <- "on"
  }
  
  for (sel in html_elements(page, "select")) {
    sel_name <- safe_attr(sel, "name")
    if (nchar(sel_name) > 0) {
      selected_opt <- html_element(sel, "option[selected]")
      if (!is.na(selected_opt)) {
        fields[[sel_name]] <- safe_attr(selected_opt, "value")
      } else {
        first_opt <- html_element(sel, "option")
        if (!is.na(first_opt)) fields[[sel_name]] <- safe_attr(first_opt, "value")
      }
    }
  }
  
  fields
}

konverter_priskolonner <- function(df) {
  for (col in pris_kolonner) {
    if (col %in% names(df)) df[[col]] <- as_num(df[[col]])
  }
  df
}

konverter_datokolonner <- function(df) {
  if ("Udgået dato" %in% names(df) && any(!is.na(df[["Udgået dato"]]))) {
    raw <- df[["Udgået dato"]]
    raw_chr <- str_squish(as.character(raw))
    
    df[["Udgået dato"]] <- ifelse(
      is.na(raw_chr) | raw_chr == "" | raw_chr == "-",
      NA_character_,
      ifelse(
        grepl("^\\d+(\\.0)?$", raw_chr),
        format(as.Date(as.numeric(raw_chr), origin = "1899-12-30"), "%d-%m-%Y"),
        raw_chr
      )
    )
  }
  df
}

parse_xls_response <- function(resp, gruppe) {
  tmp <- tempfile(fileext = ".xls")
  writeBin(resp_body_raw(resp), tmp)
  on.exit(unlink(tmp), add = TRUE)
  
  sheets <- excel_sheets(tmp)
  all_sheets <- list()
  
  for (s in sheets) {
    df <- read_xls(tmp, sheet = s, col_types = "text")
    
    if (nrow(df) > 0) {
      col_renames <- c("Cannabisprodukter" = "Lægemiddel")
      for (old_name in names(col_renames)) {
        if (old_name %in% names(df)) {
          names(df)[names(df) == old_name] <- col_renames[[old_name]]
          message("      Omdoebte kolonne '", old_name, "' -> '", col_renames[[old_name]], "' i ark '", s, "'")
        }
      }
      
      df <- konverter_priskolonner(df)
      df <- konverter_datokolonner(df)
      df$Udleveringsgruppe <- gruppe
      df$Ark <- s
      all_sheets[[s]] <- df
    }
  }
  
  if (length(all_sheets) == 0) return(NULL)
  
  if (length(sheets) > 1) {
    message("      Fandt ", length(sheets), " ark: ", paste(sheets, collapse = ", "))
  }
  
  all_cols <- unique(unlist(lapply(all_sheets, names)))
  for (nm in names(all_sheets)) {
    missing <- setdiff(all_cols, names(all_sheets[[nm]]))
    for (mc in missing) all_sheets[[nm]][[mc]] <- NA
    all_sheets[[nm]] <- all_sheets[[nm]][, all_cols]
  }
  
  bind_rows(all_sheets)
}

# -----------------------------------------------------------------------------
# 4) Fetch all current products from erhverv.medicinpriser.dk
# -----------------------------------------------------------------------------

message("== TRIN 1: Henter login-side ==")

resp_get_login <- request(login_url) |>
  req_headers(!!!browser_headers) |>
  req_options(cookiefile = cookie_file, cookiejar = cookie_file) |>
  req_perform()

login_page <- resp_body_html(resp_get_login)
login_fields <- extract_all_form_fields(login_page)

message("   Fandt ", length(login_fields), " felter")
message("== TRIN 2: Logger ind ==")

login_fields[["__EVENTTARGET"]] <- "ctl00$CanvasHolder$LoginBox$LoginButton"
login_fields[["__EVENTARGUMENT"]] <- ""
login_fields[["ctl00$CanvasHolder$LoginBox$MailBox"]] <- email
login_fields[["ctl00$CanvasHolder$LoginBox$PasswordBox"]] <- pw

resp_login <- request(login_url) |>
  req_headers(!!!browser_headers) |>
  req_headers("Referer" = login_url) |>
  req_body_form(!!!login_fields) |>
  req_options(cookiefile = cookie_file, cookiejar = cookie_file, followlocation = TRUE) |>
  req_perform()

login_result <- resp_body_html(resp_login)
still_on_login <- !is.na(html_element(login_result, "#CanvasHolder_LoginBox_MailBox"))
if (still_on_login) stop("Login fejlede! Tjek email/password.")

message("   Login OK!")
message("== TRIN 3: Henter sogning.aspx ==")

resp_sogning <- request(sogning_url) |>
  req_headers(!!!browser_headers) |>
  req_headers("Referer" = login_url) |>
  req_options(cookiefile = cookie_file, cookiejar = cookie_file) |>
  req_perform()

sogning_page <- resp_body_html(resp_sogning)
message("   OK")

message("== TRIN 4: Henter data for alle udleveringsgrupper ==")
message("   Antal grupper: ", length(udleveringsgrupper))

alle_data <- list()
current_page <- sogning_page

for (i in seq_along(udleveringsgrupper)) {
  gruppe <- udleveringsgrupper[i]
  message("\n   [", i, "/", length(udleveringsgrupper), "] Gruppe: ", gruppe)
  
  search_fields <- extract_all_form_fields(current_page)
  search_fields[["ctl00$CanvasHolder$SearchBox$UdleveringDropdown"]] <- gruppe
  search_fields[["__EVENTTARGET"]] <- "ctl00$CanvasHolder$SearchBox$SearchButton"
  search_fields[["__EVENTARGUMENT"]] <- ""
  
  resp_search <- tryCatch(
    request(sogning_url) |>
      req_headers(!!!browser_headers) |>
      req_headers("Referer" = sogning_url, "Origin" = base_url) |>
      req_body_form(!!!search_fields) |>
      req_options(cookiefile = cookie_file, cookiejar = cookie_file) |>
      req_perform(),
    error = function(e) {
      message("      FEJL ved soegning: ", e$message)
      NULL
    }
  )
  
  if (is.null(resp_search)) next
  
  search_result <- resp_body_html(resp_search)
  has_results <- !is.na(html_element(search_result, "#CanvasHolder_SearchResultBox"))
  
  if (!has_results) {
    message("      Ingen resultater - springer over")
    resp_sogning <- request(sogning_url) |>
      req_headers(!!!browser_headers) |>
      req_options(cookiefile = cookie_file, cookiejar = cookie_file) |>
      req_perform()
    current_page <- resp_body_html(resp_sogning)
    next
  }
  
  message("      Soegning OK - eksporterer...")
  
  export_fields <- extract_all_form_fields(search_result)
  export_fields[["__EVENTTARGET"]] <- "ctl00$CanvasHolder$ExcelButton"
  export_fields[["__EVENTARGUMENT"]] <- ""
  
  resp_export <- tryCatch(
    request(sogning_url) |>
      req_headers(!!!browser_headers) |>
      req_headers("Referer" = sogning_url, "Origin" = base_url) |>
      req_body_form(!!!export_fields) |>
      req_options(cookiefile = cookie_file, cookiejar = cookie_file) |>
      req_perform(),
    error = function(e) {
      message("      FEJL ved eksport: ", e$message)
      NULL
    }
  )
  
  if (is.null(resp_export)) next
  
  content_type <- resp_header(resp_export, "Content-Type") %||% ""
  
  if (grepl("excel|octet-stream|spreadsheet", content_type, ignore.case = TRUE)) {
    df <- tryCatch(
      parse_xls_response(resp_export, gruppe),
      error = function(e) {
        message("      FEJL ved parsing: ", e$message)
        NULL
      }
    )
    
    if (!is.null(df) && nrow(df) > 0) {
      alle_data[[gruppe]] <- df
      message("      OK: ", nrow(df), " raekker")
    }
  } else {
    message("      Uventet content-type: ", content_type)
  }
  
  resp_sogning <- request(sogning_url) |>
    req_headers(!!!browser_headers) |>
    req_options(cookiefile = cookie_file, cookiejar = cookie_file) |>
    req_perform()
  current_page <- resp_body_html(resp_sogning)
  
  Sys.sleep(2)
}

message("\n== TRIN 5: Sammensaetter data ==")
if (length(alle_data) == 0) stop("Ingen data hentet!")

all_cols <- unique(unlist(lapply(alle_data, names)))
for (nm in names(alle_data)) {
  missing_cols <- setdiff(all_cols, names(alle_data[[nm]]))
  for (mc in missing_cols) alle_data[[nm]][[mc]] <- NA
  alle_data[[nm]] <- alle_data[[nm]][, all_cols]
}

samlet <- bind_rows(alle_data)
rownames(samlet) <- NULL
message("   Samlet (raat): ", nrow(samlet), " raekker x ", ncol(samlet), " kolonner")

# -----------------------------------------------------------------------------
# 5) Validate and save raw takst file
# -----------------------------------------------------------------------------

message("== TRIN 6: Validerer kolonner ==")

oenskede_kolonner <- c(
  "Lægemiddel",
  "Varenr.",
  "Styrke",
  "Enhed, Styrke",
  "Pakningsstr.",
  "Form",
  "Virksomt stof",
  "Firma",
  "ATC-kode",
  "AIP (kr.)",
  "ESP pr. enhed (kr.)",
  "Prisændring",
  "ESP (kr.)",
  "Tilskudsberretigelse",
  "TSP (kr.)",
  "Kan leveres (Leveringssvigt)",
  "Følgende grossister kan ikke levere",
  "Pris pr. DDD (kr.)",
  "Pris for sygehusleverancer (kr.)",
  "Udgået dato",
  "Udleveringsgrp.",
  "Udlevering, speciale",
  "Opbevaringsbetingelser",
  "Kan dosisdispenseres (D)",
  "Pris pr. enhed (D) (kr.)",
  "Tilskud beregnes af (D) (kr.)",
  "Substitutionsgrp.",
  "Udleveringsgruppe"
)

fundne <- oenskede_kolonner[oenskede_kolonner %in% names(samlet)]
mangler <- oenskede_kolonner[!oenskede_kolonner %in% names(samlet)]
message("   Fundne kolonner: ", length(fundne), "/", length(oenskede_kolonner))

if (length(mangler) > 0) {
  message("   ADVARSEL - disse kolonner mangler i data:")
  for (m in mangler) message("     - ", m)
  message("   Kolonner i data:")
  message("     ", paste(names(samlet), collapse = ", "))
  stop("Manglende kolonner! Ret kolonnenavnene i oenskede_kolonner.")
}

valgfrie_kolonner <- c(
  "Enhed, Pakningsstr.",
  "Enhed, Pakning",
  "Pakningsenhed"
)

for (nm in valgfrie_kolonner) {
  if (!nm %in% names(samlet)) samlet[[nm]] <- NA_character_
}

samlet <- samlet[, c(oenskede_kolonner, valgfrie_kolonner)]

xlsx_file <- file.path(out_dir, paste0("Taksten_", format(Sys.Date(), "%Y-%m-%d"), ".xlsx"))
write_xlsx(samlet, xlsx_file)
message("   Rå takst gemt: ", normalizePath(xlsx_file, mustWork = FALSE))

# -----------------------------------------------------------------------------
# 6) Add categories and old G-substitution group, then save clean_data2.rds
# -----------------------------------------------------------------------------

message("== TRIN 7: Bygger clean_data2.rds ==")

if (!file.exists(cat_file)) stop("Kategori-fil findes ikke: ", cat_file)
cat_data <- read_excel(cat_file) %>%
  mutate(ATC = as.character(ATC)) %>%
  select(ATC, Kategori, Underkategori, ATCtekst)

clean_atc_key <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    stringr::str_to_upper()
}

clean_form_key <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower(locale = "da")
}

clean_strength_key <- function(x) {
  x %>%
    as.character() %>%
    stringr::str_squish() %>%
    stringr::str_to_lower(locale = "da") %>%
    stringr::str_replace_all(",", ".") %>%
    stringr::str_replace_all("mikrogram|microgram|mcg|μg", "µg") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_replace_all("\\s*/\\s*", "/") %>%
    stringr::str_replace_all("\\s*\\+\\s*", "+")
}

url_g_subst <- "https://laegemiddelstyrelsen.dk/LinkArchive.ashx?id=23D846362C144111B63358E63C44E32C&lang=en"
tmp_g_xls <- tempfile(fileext = ".xls")
download.file(url_g_subst, tmp_g_xls, mode = "wb")
on.exit(unlink(tmp_g_xls), add = TRUE)

g_subst_data <- read_excel(tmp_g_xls, sheet = 2)

required_g_cols <- c("Lægemiddel", "LægemiddelForm", "Styrke", "AtcKode", "SubstGruppe")
missing_g_cols <- setdiff(required_g_cols, names(g_subst_data))
if (length(missing_g_cols) > 0) {
  stop("G_Substitutionslisten mangler kolonner: ", paste(missing_g_cols, collapse = ", "))
}

# *** ÆNDRET: G-substitution matches på ATC + form + styrke ***
g_subst_unique <- g_subst_data %>%
  mutate(
    join_atc = clean_atc_key(AtcKode),
    join_form = clean_form_key(LægemiddelForm),
    join_styrke = clean_strength_key(Styrke),
    SubstGruppe = as.character(SubstGruppe)
  ) %>%
  filter(
    !is.na(join_atc), join_atc != "",
    !is.na(join_form), join_form != "",
    !is.na(join_styrke), join_styrke != ""
  ) %>%
  group_by(join_atc, join_form, join_styrke) %>%
  summarise(
    Overordnet_substitutionsgruppe = paste(sort(unique(na.omit(SubstGruppe))), collapse = ", "),
    .groups = "drop"
  )

clean_data2 <- samlet %>%
  mutate(
    Styrke_med_enhed = make_styrke(Styrke, `Enhed, Styrke`),
    
    Pakningsenhed_tmp = dplyr::coalesce(
      `Enhed, Pakningsstr.`,
      `Enhed, Pakning`,
      Pakningsenhed
    ),
    
    Pakning_med_enhed = make_pakning(`Pakningsstr.`, Pakningsenhed_tmp)
  ) %>%
  transmute(
    Lægemiddel = as.character(Lægemiddel),
    Varenummer = as.character(`Varenr.`),
    Styrke = Styrke_med_enhed,
    Form = as.character(Form),
    Pakning = Pakning_med_enhed,
    Indholdsstof = as.character(`Virksomt stof`),
    Firma = as.character(Firma),
    ATC = as.character(`ATC-kode`),
    
    Pris = as_num(`ESP (kr.)`),
    Pris_pr_DDD = as_num(`Pris pr. DDD (kr.)`),
    
    Tilskudssubstitutionsgruppe = as.character(`Substitutionsgrp.`),
    
    Kan_leveres = normalise_delivery(`Kan leveres (Leveringssvigt)`),
    Udleveringsgruppe = as.character(Udleveringsgruppe),
    
    # *** ÆNDRET: G-join på ATC + form + styrke ***
    join_atc = clean_atc_key(`ATC-kode`),
    join_form = clean_form_key(Form),
    join_styrke = clean_strength_key(Styrke_med_enhed)
  ) %>%
  left_join(cat_data, by = "ATC", relationship = "many-to-one") %>%
  left_join(
    g_subst_unique,
    by = c("join_atc", "join_form", "join_styrke")
  ) %>%
  select(
    Kategori,
    Underkategori,
    ATCtekst,
    ATC,
    Indholdsstof,
    Styrke,
    Form,
    Pakning,
    Lægemiddel,
    Firma,
    Varenummer,
    Pris,
    Pris_pr_DDD,
    Tilskudssubstitutionsgruppe,
    Overordnet_substitutionsgruppe,
    Kan_leveres,
    Udleveringsgruppe
  )

rds_file <- file.path(out_dir, "clean_data2.rds")
saveRDS(clean_data2, rds_file)

message("   clean_data2.rds gemt: ", normalizePath(rds_file, mustWork = FALSE))
message("   Rækker: ", nrow(clean_data2))
message("   Kolonner: ", ncol(clean_data2))
message("\n== FAERDIG! ==")

unlink(cookie_file)
