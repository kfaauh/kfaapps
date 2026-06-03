library(dplyr)
library(rvest)
library(purrr)
library(tidyr)
library(stringr)
library(shiny)
library(readxl)
library(clipr)
library(shinyjs)
library(shinycssloaders)
library(shinymanager)

credentials <- data.frame(
  user = "KFA",
  password = "kfekfa123",
  stringsAsFactors = FALSE
)

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

clean_text <- function(x) {
  x %>%
    stringr::str_replace_all("\\u00a0", " ") %>%
    stringr::str_replace_all("[\r\n\t]+", " ") %>%
    stringr::str_replace_all("\\s+", " ") %>%
    stringr::str_trim()
}

normalize_atc_codes <- function(x) {
  atc_codes <- x %>%
    strsplit("\n") %>%
    unlist() %>%
    trimws() %>%
    toupper() %>%
    unique()
  
  # Filter out empty lines and hyphens
  atc_codes <- atc_codes[atc_codes != "" & atc_codes != "-"]
  
  # ATC recoding after input, before functions run
  atc_codes <- dplyr::recode(
    atc_codes,
    "N03AX12" = "N02BF01",
    "N03AX16" = "N02BF02",
    .default = atc_codes
  )
  
  unique(atc_codes)
}

split_side_effects <- function(x) {
  x <- clean_text(x)
  
  if (is.na(x) || !nzchar(x)) {
    return(character(0))
  }
  
  # Split on commas, but not commas inside parentheses.
  # Example: "Allergiske reaktioner (herunder anafylaktisk reaktion, angioødem og urticaria)"
  # stays as one side effect.
  parts <- unlist(strsplit(x, ",\\s*(?![^()]*\\))", perl = TRUE))
  parts <- clean_text(parts)
  parts[nzchar(parts)]
}

# -----------------------------------------------------------------------------
# pro.medicin.dk scraping
# -----------------------------------------------------------------------------

# Function to extract the first product URL for a given ATC code
extract_product_url <- function(atc, sleep_time = 1) {
  if (is.na(atc) || is.null(atc) || !nzchar(atc)) {
    return(NULL)
  }
  
  Sys.sleep(sleep_time)
  
  url <- paste0("https://pro.medicin.dk/Search/Search/Search/", utils::URLencode(atc, reserved = TRUE))
  
  url_path <- tryCatch({
    rvest::read_html(url) %>%
      rvest::html_elements("a.glob-search_link") %>%
      rvest::html_attr("href") %>%
      grep("/Medicin/Praeparater/", ., value = TRUE) %>%
      dplyr::first()
  }, error = function(e) NULL)
  
  if (!is.null(url_path) && !is.na(url_path) && nzchar(url_path)) {
    return(paste0("https://pro.medicin.dk", url_path))
  }
  
  NULL
}

# Function to fetch the ADE table and trade name from a given URL
fetch_ade_table <- function(url, sleep_time = 1) {
  if (is.na(url) || is.null(url) || !nzchar(url)) {
    return(NULL)
  }
  
  Sys.sleep(sleep_time)
  
  frequency_labels <- c(
    "Meget almindelige",
    "Almindelige",
    "Ikke almindelige",
    "Sjældne",
    "Meget sjældne",
    "Ikke kendt hyppighed"
  )
  
  frequency_regex <- paste0("^\\s*(", paste(frequency_labels, collapse = "|"), ")\\b")
  
  page <- tryCatch(
    rvest::read_html(url),
    error = function(e) NULL
  )
  
  if (is.null(page)) {
    warning("Siden kunne ikke læses: ", url)
    return(NULL)
  }
  
  trade_name <- page %>%
    rvest::html_element("title") %>%
    rvest::html_text2() %>%
    stringr::str_extract("^[^-]+") %>%
    stringr::str_trim()
  
  tables <- page %>% rvest::html_elements("table")
  
  # Robust table selection:
  # Do NOT choose the first 3-column table. Instead, identify the actual adverse-effect
  # table by its title and mandatory column headers.
  target_table <- purrr::detect(tables, function(tbl) {
    tbl_text <- tbl %>%
      rvest::html_text2() %>%
      clean_text()
    
    has_title <- stringr::str_detect(
      tbl_text,
      stringr::regex("Registrerede bivirkninger", ignore_case = TRUE)
    )
    
    has_required_headers <- all(stringr::str_detect(
      tbl_text,
      stringr::regex(c(
        "Systemorganklasse",
        "Potentielt alvorlige bivirkninger",
        "Oftest ikke alvorlige bivirkninger"
      ), ignore_case = TRUE)
    ))
    
    has_title && has_required_headers
  })
  
  if (is.null(target_table)) {
    warning("Ingen bivirkningstabel fundet for URL: ", url)
    return(NULL)
  }
  
  rows <- target_table %>% rvest::html_elements("tr")
  
  parsed_rows <- purrr::map_dfr(rows, function(row) {
    cells <- row %>%
      rvest::html_elements("td, th") %>%
      rvest::html_text2() %>%
      clean_text()
    
    colspan <- row %>%
      rvest::html_elements("td, th") %>%
      rvest::html_attr("colspan")
    
    tibble::tibble(
      n_cells = length(cells),
      colspan_3 = any(colspan == "3", na.rm = TRUE),
      c1 = if (length(cells) >= 1) cells[1] else NA_character_,
      c2 = if (length(cells) >= 2) cells[2] else NA_character_,
      c3 = if (length(cells) >= 3) cells[3] else NA_character_
    )
  })
  
  df <- parsed_rows %>%
    dplyr::mutate(
      is_title_row = stringr::str_detect(c1, stringr::regex("Registrerede bivirkninger", ignore_case = TRUE)),
      is_header_row = stringr::str_detect(c1, stringr::regex("^Systemorganklasse$", ignore_case = TRUE)),
      is_frequency_row = colspan_3 & stringr::str_detect(c1, stringr::regex(frequency_regex, ignore_case = TRUE)),
      frequency = dplyr::if_else(is_frequency_row, c1, NA_character_)
    ) %>%
    tidyr::fill(frequency, .direction = "down") %>%
    dplyr::filter(
      !is_title_row,
      !is_header_row,
      !is_frequency_row,
      !is.na(frequency),
      n_cells >= 3
    ) %>%
    dplyr::transmute(
      trade_name = trade_name,
      system_organ_class = c1,
      frequency = frequency,
      potentially_serious_side_effects = c2,
      not_serious_side_effects = c3
    ) %>%
    dplyr::filter(
      nzchar(system_organ_class),
      nzchar(potentially_serious_side_effects) | nzchar(not_serious_side_effects)
    )
  
  df_long <- df %>%
    tidyr::pivot_longer(
      cols = c(potentially_serious_side_effects, not_serious_side_effects),
      names_to = "severity",
      values_to = "side_effects_raw"
    ) %>%
    dplyr::mutate(
      side_effects = purrr::map(side_effects_raw, split_side_effects)
    ) %>%
    tidyr::unnest(side_effects) %>%
    dplyr::select(
      trade_name,
      system_organ_class,
      frequency,
      severity,
      side_effects
    ) %>%
    dplyr::filter(!is.na(side_effects), nzchar(side_effects))
  
  if (nrow(df_long) == 0) {
    warning("Bivirkningstabellen blev fundet, men ingen bivirkninger kunne parses for URL: ", url)
    return(NULL)
  }
  
  df_long
}

# -----------------------------------------------------------------------------
# Local drug list
# -----------------------------------------------------------------------------

# Read in the list of drugs
drugs <- read.csv2("data/ListeOverGodkendteLaegemidler.csv", header = FALSE, sep = ";")
drugs <- dplyr::select(drugs, Drugid = V1, drugname = V5, ATC = V7)
drugs <- drugs[-1, ]
drugs <- drugs %>%
  dplyr::mutate(
    ATC = toupper(trimws(ATC)),
    drugname = trimws(drugname)
  ) %>%
  dplyr::distinct(ATC, .keep_all = TRUE)

get_drugnames <- function(atc_codes) {
  drugs_subset <- drugs %>%
    dplyr::filter(ATC %in% atc_codes) %>%
    dplyr::mutate(ATC = factor(ATC, levels = atc_codes)) %>%
    dplyr::arrange(ATC)
  
  drugs_subset$drugname
}

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  titled_tibbles <- eventReactive(input$promedreadr, {
    atc_codes <- normalize_atc_codes(input$atc_codes)
    
    if (length(atc_codes) == 0) {
      showModal(modalDialog(
        title = "Ingen ATC-koder",
        "Indtast mindst én ATC-kode.",
        easyClose = TRUE
      ))
      return(list())
    }
    
    # Identify ATC codes that are missing in the CSV file
    missing_in_csv <- setdiff(atc_codes, drugs$ATC)
    
    if (length(missing_in_csv) > 0) {
      showModal(modalDialog(
        title = "ATC-koder ikke fundet",
        paste0(
          "Følgende ATC-koder findes ikke på listen over godkendte lægemidler: ",
          paste(missing_in_csv, collapse = ", "),
          "."
        ),
        easyClose = TRUE
      ))
      
      atc_codes <- setdiff(atc_codes, missing_in_csv)
    }
    
    if (length(atc_codes) == 0) {
      return(list())
    }
    
    # Lookup product URLs once only
    product_urls <- purrr::map(atc_codes, extract_product_url)
    names(product_urls) <- atc_codes
    
    missing_in_web <- names(product_urls)[purrr::map_lgl(product_urls, is.null)]
    
    if (length(missing_in_web) > 0) {
      showModal(modalDialog(
        title = "pro.medicin.dk-opslag fejlede",
        paste0(
          "Følgende ATC-koder kan ikke slås op på pro.medicin.dk: ",
          paste(missing_in_web, collapse = ", "),
          "."
        ),
        easyClose = TRUE
      ))
      
      keep <- !names(product_urls) %in% missing_in_web
      product_urls <- product_urls[keep]
      atc_codes <- names(product_urls)
    }
    
    if (length(product_urls) == 0) {
      return(list())
    }
    
    drugnames <- get_drugnames(atc_codes)
    
    # Fetch ADE tables for each URL
    all_ade_tables <- purrr::map(product_urls, fetch_ade_table)
    
    # Warn if the product URL was found, but the ADE table could not be parsed
    failed_ade <- names(product_urls)[purrr::map_lgl(all_ade_tables, is.null)]
    
    if (length(failed_ade) > 0) {
      showModal(modalDialog(
        title = "Bivirkningstabel ikke fundet eller ikke parsebar",
        paste0(
          "Der blev ikke fundet en parsebar bivirkningstabel for: ",
          paste(failed_ade, collapse = ", "),
          "."
        ),
        easyClose = TRUE
      ))
    }
    
    # Combine drug names and ADE tables
    titled_tibbles <- purrr::map2(all_ade_tables, drugnames, function(tbl, title) {
      if (!is.null(tbl)) {
        tbl <- tbl %>%
          dplyr::mutate(
            drug = title,
            severity = dplyr::recode(
              severity,
              "potentially_serious_side_effects" = "Potentielt alvorlig",
              "not_serious_side_effects" = "Oftest ikke alvorlig"
            )
          ) %>%
          dplyr::rename(
            Handelsnavn = trade_name,
            Lægemiddel = drug,
            Systemorganklasse = system_organ_class,
            Hyppighed = frequency,
            Alvorlighed = severity,
            Bivirkninger = side_effects
          ) %>%
          dplyr::select(
            Handelsnavn,
            Lægemiddel,
            Systemorganklasse,
            Hyppighed,
            Alvorlighed,
            Bivirkninger
          )
      }
      
      tbl
    })
    
    titled_tibbles <- purrr::compact(titled_tibbles)
    
    titled_tibbles
  })
  
  output$ade_table <- renderUI({
    tibbles <- titled_tibbles()
    
    if (is.null(tibbles) || length(tibbles) == 0) {
      return(tags$p("Ingen data tilgængelige."))
    }
    
    table_list <- lapply(seq_along(tibbles), function(i) {
      tagList(
        tags$h4(paste0("Tabel ", i)),
        tableOutput(outputId = paste0("table_", i)),
        tags$hr()
      )
    })
    
    for (i in seq_along(tibbles)) {
      local({
        table_num <- i
        output[[paste0("table_", table_num)]] <- renderTable({
          tibbles[[table_num]]
        })
      })
    }
    
    do.call(tagList, table_list)
  })
}

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- secure_app(fluidPage(
  useShinyjs(),
  titlePanel("Bivirkninger fra pro.medicin.dk"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        "atc_codes",
        label = "Indtast ATC-koder (én pr. linje)",
        placeholder = "f.eks. N05AN01\nC03CA01",
        height = "300px"
      ),
      br(),
      actionButton("promedreadr", "Bivirkninger fra pro.medicin.dk", class = "btn-primary"),
      br(),
      actionButton("select_tables", "Vælg tabeller", class = "btn-success")
    ),
    
    mainPanel(
      div(
        id = "tables-container",
        withSpinner(uiOutput("ade_table"), type = 1)
      )
    )
  ),
  
  # JavaScript to select all table content
  tags$script(HTML("\n    function selectTableContent() {\n      var container = document.getElementById('tables-container');\n      if (container) {\n        var range = document.createRange();\n        var selection = window.getSelection();\n        range.selectNodeContents(container);\n        selection.removeAllRanges();\n        selection.addRange(range);\n\n        alert('Tabellerne er valgt. Tryk Ctrl+C for at kopiere.');\n      } else {\n        alert('Ingen tabeller fundet.');\n      }\n    }\n\n    document.getElementById('select_tables').onclick = selectTableContent;\n  "))
))

shinyApp(ui = ui, server = server)
