#!/usr/bin/env Rscript

library(rvest)
library(stringr)
library(readxl)
library(dplyr)
library(tidyr)
library(lubridate)

args <- commandArgs(trailingOnly = TRUE)

out_dir <- if (length(args) >= 1) args[1] else "data"
cat_file <- if (length(args) >= 2) args[2] else "Alle lægemidler.xlsx"

if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Hjælpefunktion: robust numerisk konvertering, også hvis komma bruges som decimaltegn
as_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- str_replace_all(x, "\\.", "")
  x <- str_replace_all(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

cat_data <- read_excel(cat_file)

# 1) Hent seneste Medicinpriser-fil
url_price <- "https://sundhedsdatabank.dk/medicin/medicinpriser"

page <- read_html(url_price)
nodes <- page %>% html_elements("a")

link_df <- tibble(
  text = str_squish(html_text2(nodes)),
  href = html_attr(nodes, "href")
) %>%
  filter(!is.na(href), href != "")

download_link <- link_df %>%
  filter(
    str_detect(text, regex("\\bMedicinpriser\\b", ignore_case = TRUE)),
    !str_detect(text, regex("Medicinprisindeks", ignore_case = TRUE)),
    str_detect(href, regex("\\.xlsx(\\?|$)", ignore_case = TRUE))
  ) %>%
  slice(1) %>%
  pull(href)

if (length(download_link) == 0 || is.na(download_link)) {
  stop("Ingen gyldig 'Medicinpriser' .xlsx-link fundet på siden")
}

download_link <- ifelse(
  str_detect(download_link, "^https?://"),
  download_link,
  paste0("https://sundhedsdatabank.dk", download_link)
)

xlsx_file <- tempfile(fileext = ".xlsx")
download.file(download_link, xlsx_file, mode = "wb", timeout = 180)

price_data <- read_excel(xlsx_file, sheet = 4)

# 2) Hent substitutionsdata
url_subst <- "https://laegemiddelstyrelsen.dk/LinkArchive.ashx?id=23D846362C144111B63358E63C44E32C&lang=en"

tmp_xls <- tempfile(fileext = ".xls")
download.file(url_subst, tmp_xls, mode = "wb")

subst_data <- read_excel(tmp_xls, sheet = 2)

# 3) Find prisdatokolonner
date_cols <- grep("^\\d{8}$", names(price_data), value = TRUE) |> sort()

if (length(date_cols) < 2) {
  stop("Der blev fundet færre end to prisdatokolonner i Medicinpriser-filen.")
}

date_values <- ymd(date_cols)

latest_col <- date_cols[which.max(date_values)]
latest_date <- max(date_values, na.rm = TRUE)

previous_target_date <- latest_date - days(14)
previous_col <- date_cols[which.min(abs(as.numeric(date_values - previous_target_date)))]

cols_12_months <- date_cols[date_values >= (latest_date %m-% months(12))]

# Konverter alle prisdatokolonner til numeriske værdier
price_data <- price_data %>%
  mutate(across(all_of(date_cols), as_num))

# 4) Beregn 14-dages prisændring og 12-måneders prisvariation ud fra AUP
aup_history <- price_data %>%
  filter(Indikator == "AUP") %>%
  select(Varenummer, all_of(date_cols))

price_metrics <- aup_history %>%
  rowwise() %>%
  mutate(
    `14-dages prisændring` = {
      latest_price <- .data[[latest_col]]
      previous_price <- .data[[previous_col]]
      
      if (
        is.na(latest_price) ||
        is.na(previous_price) ||
        previous_price == 0
      ) {
        NA_real_
      } else {
        round((latest_price - previous_price) / previous_price * 100, 1)
      }
    },
    Prisvariation = {
      prices <- c_across(all_of(cols_12_months))
      prices <- as.numeric(prices)
      prices <- prices[!is.na(prices)]
      
      if (length(prices) < 2) {
        NA_real_
      } else {
        round(sd(prices), 2)
      }
    }
  ) %>%
  ungroup() %>%
  select(Varenummer, `14-dages prisændring`, Prisvariation)

# 5) Lav bredt datasæt for seneste prisperiode
price_sub <- price_data %>%
  select(
    ATC,
    Lægemiddel,
    Indholdsstof,
    Varenummer,
    Pakning,
    Styrke,
    Form,
    Firma,
    Indikator,
    all_of(latest_col)
  )

price_wide <- price_sub %>%
  pivot_wider(
    names_from = Indikator,
    values_from = all_of(latest_col)
  )

price_wide <- price_wide %>%
  filter(!is.na(AIP)) %>%
  left_join(price_metrics, by = "Varenummer")

# Behold billigste pakning ved dubletter
price_wide <- price_wide %>%
  group_by(Lægemiddel, Pakning, Styrke, Form, Firma) %>%
  slice_min(AIP, with_ties = TRUE) %>%
  ungroup()

# 6) Rens substitutionsdata
subst_data <- subst_data %>%
  group_by(Lægemiddel, Styrke, LægemiddelForm, MftIndehaver) %>%
  slice_min(DrugId, with_ties = FALSE) %>%
  ungroup()

subst_data_unique <- subst_data %>%
  distinct(Lægemiddel, LægemiddelForm, Styrke, .keep_all = TRUE)

# 7) Merge med kategori og substitutionsgruppe
price_wide <- price_wide %>%
  left_join(
    cat_data %>% select(ATC, Kategori, Underkategori, ATCtekst),
    by = "ATC",
    relationship = "many-to-one"
  )

clean_data <- price_wide %>%
  left_join(
    subst_data_unique %>%
      select(Lægemiddel, LægemiddelForm, Styrke, SubstGruppe),
    by = c(
      "Lægemiddel" = "Lægemiddel",
      "Form" = "LægemiddelForm",
      "Styrke" = "Styrke"
    )
  )

# 8) Gem output
saveRDS(clean_data, file.path(out_dir, "clean_data.rds"))

message("✔ clean_data.rds skrevet til ", normalizePath(out_dir))
message("✔ Seneste prisdato: ", latest_col)
message("✔ Sammenlignet med: ", previous_col)
message("✔ Prisvariation beregnet over ", length(cols_12_months), " prisperioder")
