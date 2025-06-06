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

# Function to extract the first product URL for a given ATC code
extract_product_url <- function(atc, sleep_time = 1) {
  Sys.sleep(sleep_time)
  
  url_path <- paste0("https://pro.medicin.dk/Search/Search/Search/", atc) %>%
    rvest::read_html() %>%
    rvest::html_elements("a.glob-search_link") %>%
    rvest::html_attr("href") %>%
    grep("/Medicin/Praeparater/", ., value = TRUE) %>%
    first()  # Select only the first match
  
  if (!is.null(url_path)) {
    return(paste0("https://pro.medicin.dk", url_path))
  }
  return(NULL)
}

# Function to fetch the ADE table and trade name from a given URL
fetch_ade_table <- function(url, sleep_time = 1) {
  if (is.na(url) | is.null(url)) {
    return(NULL)
  }
  Sys.sleep(sleep_time)
  
  frequency_labels <- c("Meget almindelige", "Almindelige", "Ikke almindelige", 
                        "Sjældne", "Meget sjældne", "Ikke kendt hyppighed")
  frequency_regex <- paste(sprintf("(%s)", frequency_labels), collapse = "|")
  
  replacements <- c(`\\r\\n\\s+\\(` = " \\(", `.\\r\\n\\s+` = ", ", 
                    `\\s+` = " ")
  
  page <- rvest::read_html(url)
  
  # *** Added code to extract the trade name from the <title> tag ***
  trade_name <- page %>%
    rvest::html_element("title") %>%
    rvest::html_text() %>%
    str_extract("^[^-]+") %>%
    str_trim()
  
  target_table <- page %>%
    rvest::html_elements("table") %>%
    purrr::detect(~ {
      df <- rvest::html_table(.x, fill = TRUE)
      if (ncol(df) >= 3 && all(c("X1", "X2", "X3") %in% names(df))) {
        TRUE
      } else {
        FALSE
      }
    })
  
  if (is.null(target_table)) {
    return(NULL)
  }
  
  df <- rvest::html_table(target_table, fill = TRUE)
  df <- df %>%
    mutate(frequency = ifelse(grepl(frequency_regex, X1, ignore.case = TRUE), X1, NA)) %>%
    fill(frequency, .direction = "down") %>%
    filter(!grepl(frequency_regex, X1))  # Remove rows containing the frequency labels
  
  df <- df %>%
    rename(
      system_organ_class = X1,
      potentially_serious_side_effects = X2,
      not_serious_side_effects = X3
    ) %>%
    mutate(
      potentially_serious_side_effects = stringr::str_replace_all(potentially_serious_side_effects, replacements),
      not_serious_side_effects = stringr::str_replace_all(not_serious_side_effects, replacements)
    )
  
  df_long <- df %>%
    pivot_longer(
      cols = c(potentially_serious_side_effects, not_serious_side_effects),
      names_to = "severity",
      values_to = "side_effects"
    ) %>%
    filter(!is.na(side_effects) & side_effects != "") %>%  # Remove rows where side effects are NA or empty
    mutate(side_effects = strsplit(side_effects, ",\\s*")) %>%  # Split the side effects by comma
    unnest(cols = "side_effects")  # Expand into separate rows
  
  # *** Added trade name to the data ***
  df_long <- df_long %>%
    mutate(trade_name = trade_name)  # Add trade name as a new column
  
  return(df_long)
}


# Read in the list of drugs
drugs <- read.csv2("ListeOverGodkendteLaegemidler.csv", header = FALSE, sep =";")
drugs <- select(drugs, c("Drugid" = "V1"), c("drugname" = "V5"), c("ATC" = "V7"))
drugs <- drugs[-1, ]
drugs <- distinct(drugs, ATC, .keep_all = TRUE)

get_drugnames <- function(atc_codes) {
  drugs_subset <- drugs[drugs$ATC %in% atc_codes, ]
  
  # Create a factor with levels as atc_codes to maintain the order
  drugs_subset$ATC <- factor(drugs_subset$ATC, levels = atc_codes)
  
  # Arrange by the factor level to maintain the original order
  drugs_subset <- drugs_subset %>% arrange(ATC)
  
  drugnames <- drugs_subset$drugname
  return(drugnames)
}

server <- function(input, output, session) {

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  titled_tibbles <- eventReactive(input$promedreadr, {
    atc_codes <- input$atc_codes %>% strsplit("\n") %>% unlist %>% trimws()
    atc_codes <- unique(atc_codes)
    
    # Filter out empty lines and hyphens
    atc_codes <- atc_codes[atc_codes != "" & atc_codes != "-"]
    
    # Convert N03AX12 to N02BF01
    atc_codes <- ifelse(atc_codes == "N03AX12", "N02BF01", atc_codes)
    
    # Convert N03AX12 to N02BF01
    atc_codes <- ifelse(atc_codes == "N03AX16", "N02BF02", atc_codes)
    
    # Identify ATC codes that are missing in the CSV file
    missing_in_csv <- setdiff(atc_codes, drugs$ATC)
    
    if (length(missing_in_csv) > 0) {
      showModal(modalDialog(
        title = "Error",
        paste0("Følgende ATC-koder findes ikke på listen over godkendte lægemidler: ", paste(missing_in_csv, collapse = ", "), "."),
        easyClose = TRUE
      ))
      atc_codes <- setdiff(atc_codes, missing_in_csv)  # Remove problematic codes
    }
    
    # Get drug names in the same order as the filtered atc_codes
    if (length(atc_codes) > 0) {
      drugnames <- get_drugnames(atc_codes)
    } else {
      drugnames <- NULL  # Ensure drugnames is initialized as NULL if no valid ATC codes
    }
    
    # Check for missing ATC codes on the website
    product_urls <- map(atc_codes, extract_product_url)
    missing_in_web <- atc_codes[map_lgl(product_urls, is.null)]
    
    if (length(missing_in_web) > 0) {
      showModal(modalDialog(
        title = "Error",
        paste0("Følgende ATC-koder kan ikke slås op på pro.medicin.dk: ", paste(missing_in_web, collapse = ", "), "."),
        easyClose = TRUE
      ))
      atc_codes <- setdiff(atc_codes, missing_in_web)  # Remove problematic codes
    }
    
    # Fetch ADE tables for each URL (excluding problematic ATC codes)
    product_urls <- map(atc_codes, extract_product_url)
    all_ade_tables <- map(product_urls, fetch_ade_table)
    
    # Combine drug names and ADE tables
    titled_tibbles <- map2(all_ade_tables, drugnames, function(tbl, title) {
      if (!is.null(tbl)) {
        tbl <- dplyr::mutate(tbl, drug = title)
        tbl <- dplyr::mutate(tbl, severity = dplyr::recode(severity,
                                                           "potentially_serious_side_effects" = "Potentielt alvorlig",
                                                           "not_serious_side_effects" = "Oftest ikke alvorlig"))
        tbl <- dplyr::rename(tbl,
                             Handelsnavn = trade_name,  # Trade name column
                             Lægemiddel = drug,
                             Systemorganklasse = system_organ_class,
                             Hyppighed = frequency,
                             Alvorlighed = severity,
                             Bivirkninger = side_effects)
        tbl <- tbl %>% dplyr::select(Handelsnavn, Lægemiddel, Systemorganklasse, Hyppighed, Alvorlighed, Bivirkninger)
        
        # Remove the first four rows
        tbl <- tbl %>% slice(-(1:4))
      }
      return(tbl)
    })
    
    return(titled_tibbles)
  })
  
  output$ade_table <- renderUI({
    tibbles <- titled_tibbles()
    
    if (is.null(tibbles) || length(tibbles) == 0) {
      return("No data available")
    }
    
    table_list <- lapply(seq_along(tibbles), function(i) {
      tableOutput(outputId = paste0("table_", i))
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


ui <- secure_app(fluidPage(
  useShinyjs(),
  titlePanel("Bivirkninger fra pro.medicin.dk"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("atc_codes", label = "Indtast ATC-koder (én pr. linje)", 
                    placeholder = "f.eks. N05AN01\nC03CA01", height = "300px"),
      br(),
      actionButton("promedreadr", "Bivirkninger fra pro.medicin.dk", class = "btn-primary"),
      br(),
      actionButton("select_tables", "Vælg tabeller", class = "btn-success")
    ),
    
    mainPanel(
      div(id = "tables-container", 
          withSpinner(uiOutput("ade_table"), type = 1)  # Add spinner here
      )
    )
  ),
  
  # JavaScript to select all table content
  tags$script(HTML("
    function selectTableContent() {
      var container = document.getElementById('tables-container');
      if (container) {
        var range = document.createRange();
        var selection = window.getSelection();
        range.selectNodeContents(container); // Select the content of the container
        selection.removeAllRanges(); // Clear any existing selections
        selection.addRange(range); // Add the new range

        alert('Tabellerne er valgt. Tryk Ctrl+C for at kopiere.');
      } else {
        alert('Ingen tabeller fundet.');
      }
    }

    document.getElementById('select_tables').onclick = selectTableContent;
  "))
))

shinyApp(ui = ui, server = server)

