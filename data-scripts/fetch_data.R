
#!/usr/bin/env Rscript
library(rvest); library(stringr)
library(readxl); library(dplyr); library(tidyr)

args <- commandArgs(trailingOnly = TRUE)
out_dir  <- if (length(args)>=1) args[1] else "data"
cat_file <- if (length(args)>=2) args[2] else "Alle lægemidler.xlsx"

if (!dir.exists(out_dir)) dir.create(out_dir)
cat_data <- read_excel(cat_file)

# 2) Hent pris-data
url_price <- "https://sundhedsdatabank.dk/medicin/medicinpriser"
page <- read_html(url_price)

# Udtræk linktekst + href
link_df <- page %>%
  html_elements("a") %>%
  tibble(
    text = str_squish(html_text2(.)),
    href = html_attr(., "href")
  ) %>%
  filter(!is.na(href), href != "")

download_link <- link_df %>%
  filter(
    str_detect(text, regex("\\bMedicinpriser\\b", ignore_case = TRUE)),   # Skal indeholde "Medicinpriser"
    !str_detect(text, regex("Medicinprisindeks", ignore_case = TRUE)),    # Må ikke være et index
    str_detect(href, regex("\\.xlsx(\\?|$)", ignore_case = TRUE))         # Skal være excel
  ) %>%
  slice(1) %>%
  pull(href)

if (length(download_link) == 0 || is.na(download_link)) {
  stop("Ingen gyldig 'Medicinpriser'-Excel-link fundet på siden")
}

# Generer download link
download_link <- ifelse(
  str_detect(download_link, "^https?://"),
  download_link,
  paste0("https://sundhedsdatabank.dk", download_link)
)

# Download excel-fil
xlsx_file <- tempfile(fileext = ".xlsx")
download.file(download_link, xlsx_file, mode = "wb", timeout = 180)

# Data is now in sheet 4
price_data <- read_excel(xlsx_file, sheet = 4)

# 3) Hent substitutions‐data
url_subst <- "https://laegemiddelstyrelsen.dk/LinkArchive.ashx?id=23D846362C144111B63358E63C44E32C&lang=en"
tmp_xls   <- tempfile(fileext=".xls")
download.file(url_subst, tmp_xls, mode="wb")
subst_data<- read_excel(tmp_xls, sheet=2)

# 5) Rens pris-data
date_cols <- grep("^\\d{8}$", names(price_data), value=TRUE) |> sort()
latest   <- tail(date_cols,1)
prev     <- tail(date_cols,2)[1]
price_data$Prisændring <- (price_data[[latest]] - price_data[[prev]]) / price_data[[prev]]
price_sub <- price_data %>%
  select(ATC, Lægemiddel, Indholdsstof, Varenummer, Pakning, Styrke, Form, Firma, Indikator, all_of(latest))
price_wide <- price_sub |> pivot_wider(names_from=Indikator, values_from=all_of(latest))
prisændring_df <- price_data |> filter(Indikator=="AUP_pr_DDD") |> select(Varenummer, Prisændring)
price_wide <- price_wide |> filter(!is.na(AIP)) |> left_join(prisændring_df, by="Varenummer")
price_wide <- price_wide |> group_by(Lægemiddel, Pakning, Styrke, Form, Firma) |>
  slice_min(AIP, with_ties=TRUE) |> ungroup()
subst_data <- subst_data |> group_by(Lægemiddel, Styrke, LægemiddelForm, MftIndehaver) |>
  slice_min(DrugId, with_ties=FALSE) |> ungroup()

# 6) Merge med kategori og subst
price_wide <- price_wide |> left_join(cat_data |> select(ATC,Kategori,Underkategori,ATCtekst),
                                      by="ATC", relationship="many-to-one")
subst_data_unique <- subst_data |> distinct(Lægemiddel, LægemiddelForm, Styrke, .keep_all=TRUE)
clean_data <- price_wide |> left_join(
  subst_data_unique |> select(Lægemiddel,LægemiddelForm,Styrke,SubstGruppe),
  by=c("Lægemiddel"="Lægemiddel","Form"="LægemiddelForm","Styrke"="Styrke")
)

# 7) Gem output
# gem output i en undermappe "data"
saveRDS(clean_data, file.path(out_dir, "clean_data.rds"))
message("✔ clean_data.rds skrevet til ", normalizePath(out_dir))

