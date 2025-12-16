# data_prep.R
# This script fetches raw data, processes it, and saves a cleaned RDS file for the Shiny app.

# Load required packages
library(readxl)
library(stringr)
library(dplyr)
library(tidyr)
library(rvest)

# Ensure data directory exists
if (!dir.exists("data")) {
  dir.create("data")
}

url <- "https://sundhedsdatabank.dk/medicin/medicinsalg-og-udgifter"
page <- read_html(url)

nodes <- page %>% html_elements("a")

link_df <- tibble(
  text = str_squish(html_text2(nodes)),
  href = html_attr(nodes, "href")
) %>%
  filter(!is.na(href), href != "")

download_link <- link_df %>%
  filter(
    str_detect(text, regex("\\bMedicintilskud\\b", ignore_case = TRUE)),
    str_detect(href, regex("\\.xlsx(\\?|$)", ignore_case = TRUE))
  ) %>%
  slice(1) %>%
  pull(href)

if (length(download_link) == 0 || is.na(download_link)) {
  stop("Ingen gyldig 'Medicintilskud' .xlsx-link fundet på siden")
}

download_link <- ifelse(
  str_detect(download_link, "^https?://"),
  download_link,
  paste0("https://sundhedsdatabank.dk", download_link)
)

temp1 <- tempfile(fileext = ".xlsx")
download.file(download_link, destfile = temp1, mode = "wb", timeout = 180)

medicinforbrug_data <- read_excel(temp1, sheet = 9, skip = 6)

# Definer og download generisk data
url_excel <- "https://laegemiddelstyrelsen.dk/LinkArchive.ashx?id=23D846362C144111B63358E63C44E32C&lang=en"
temp2 <- tempfile(fileext = ".xls")
download.file(url_excel, destfile = temp2, mode = "wb")
generisk_data <- read_excel(temp2, sheet = 2)
generisk_data_grouped <- generisk_data %>%
  select(Lægemiddel, SubstGruppe, Styrke, LægemiddelForm) %>%
  distinct()

# Merge datasets
merged_data <- medicinforbrug_data %>%
   left_join(
     generisk_data_grouped,
     by           = c("Produktnavn" = "Lægemiddel"),
     relationship = "many-to-many"
   ) %>%
  mutate(
    Dato = as.Date(paste(År, sprintf("%02d", as.numeric(Måned)), "01", sep = "-"))
  )

# Save cleaned data for Shiny app
# Save cleaned data for Shiny app
saveRDS(merged_data, file = "data/merged_data.rds")
