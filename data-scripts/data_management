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

# Define and download eSundhed medicinforbrug data
url <- "https://www.esundhed.dk/Emner/Laegemidler/Medicintilskud#tabpanel8D68CD313C234CD7810555310F6171AC"
page <- read_html(url)
links <- page %>% html_nodes("a") %>% html_attr("href")
links <- links[!is.na(links)]
download_link <- links[str_detect(links, "Medicintilskud_regionsinddelt")]
if (!str_detect(download_link, "^http")) {
  download_link <- paste0("https://www.esundhed.dk", download_link)
}
temp1 <- tempfile(fileext = ".xlsx")
download.file(download_link, destfile = temp1, mode = "wb")
medicinforbrug_data <- read_excel(temp1, sheet = 7, skip = 5)

# Define and download generisk data
url_excel <- "https://laegemiddelstyrelsen.dk/LinkArchive.ashx?id=23D846362C144111B63358E63C44E32C&lang=en"
temp2 <- tempfile(fileext = ".xls")
download.file(url_excel, destfile = temp2, mode = "wb")
generisk_data <- read_excel(temp2, sheet = 2)
generisk_data_grouped <- generisk_data %>%
  select(Lægemiddel, SubstGruppe, Styrke, LægemiddelForm) %>%
  distinct()

# Merge datasets
merged_data <- medicinforbrug_data %>%
  left_join(generisk_data_grouped, by = c("Produktnavn" = "Lægemiddel")) %>%
  mutate(
    Dato = as.Date(paste(År, sprintf("%02d", as.numeric(Måned)), "01", sep = "-"))
  )

# Save cleaned data for Shiny app
# Save cleaned data for Shiny app
saveRDS(merged_data, file = "data/merged_data.rds")
