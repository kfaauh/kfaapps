library(readxl)

args <- commandArgs(trailingOnly = TRUE)

if (length(args) < 1) {
  stop(
    "No output directories supplied. Usage: Rscript Update_list.R <out_dir1> [<out_dir2> ...]",
    call. = FALSE
  )
}

out_dirs <- args

# URL of the XLSX download link
url <- "https://laegemiddelstyrelsen.dk/LinkArchive.ashx?id=0BD4960F0D7744E3BABC951431681ECC&lang=da"

# Temporary file to hold the download
tmp_xlsx <- file.path(tempdir(), "ListeOverGodkendteLaegemidler.xlsx")

# Download (binary mode)
download.file(url, destfile = tmp_xlsx, mode = "wb")

# Read sheet 1
df <- read_excel(tmp_xlsx, sheet = 1)

# Ensure no row‐names and consistent NA handling
df[] <- lapply(df, function(x) ifelse(is.na(x), "", x))

for (d in out_dirs) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }

  out_file <- file.path(d, "ListeOverGodkendteLaegemidler.csv")

  write.csv2(
    df,
    out_file,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )

  message("Wrote: ", out_file)
}
