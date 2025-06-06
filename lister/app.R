library(shiny)
library(readr)
library(dplyr)
library(openxlsx)
library(readxl)
library(readr)
library(stringr)
library(rvest)
library(httr)
library(stringr)
library(shinycssloaders)
library(shinymanager)

credentials <- data.frame(
  user = "KFA",
  password = "kfekfa123",
  stringsAsFactors = FALSE
)

# Load your data from local files
drugs_data <- read.csv2("data/ListeOverGodkendteLaegemidler.csv", header = FALSE, sep =";")
acb_data <- read_excel("data/anticholinergic_bcpt.xlsx", skip=1)  # Loading the Excel file
sepo_data <- read_delim("data/seponeringslisten_v2.csv", delim = ";")  # Semicolon-separated CSV
qtc_data <- read_delim("data/QTc_conditional.csv", delim = ";")  # Semicolon-separated CSV
ss_data <- read_delim("data/serotonerg_atc.csv", delim = ";")  # Semicolon-separated CSV
bleed_data <- read_delim("data/Blødningsrisiko_ny.csv", delim = ";")  # Semicolon-separated CSV

# Helper function to calculate the ACB score
get_acb_score <- function(atc_codes) {
  acb_subset <- acb_data[acb_data$ATC %in% atc_codes & !is.na(acb_data$DKACB), ]
  total_acb <- sum(acb_subset$DKACB, na.rm = TRUE)
  not_found <- setdiff(atc_codes, acb_subset$ATC)
  not_found_msg <- if (length(not_found) > 0) {
    paste(not_found, collapse = ", ")
  } else {
    "Alle ATC-koder blev fundet."
  }
  found_atc_codes <- acb_subset$ATC
  return(list(score = total_acb, not_found = not_found_msg, found_drugs = acb_subset$`Name of ATC Code`, found_atc_codes = found_atc_codes))
}

# Function to extract the first product URL for a given ATC code
extract_product_url <- function(atc, sleep_time = 1) {
  Sys.sleep(sleep_time)  # Adding a delay to avoid overloading the server
  
  # Search for the ATC code on the website and extract the first product URL
  url_path <- tryCatch({
    paste0("https://pro.medicin.dk/Search/Search/Search/", atc) %>%
      read_html() %>%
      html_elements("a.glob-search_link") %>%
      html_attr("href") %>%
      grep("/Medicin/Praeparater/", ., value = TRUE) %>%
      first()  # Select only the first match
  }, error = function(e) NULL)
  
  if (!is.null(url_path)) {
    return(paste0("https://pro.medicin.dk", url_path))  # Construct full URL
  }
  return(NULL)
}

# Function to scrape the trade name from the product page
extract_trade_name <- function(page) {
  trade_name <- tryCatch({
    page %>%
      html_element("title") %>%
      html_text() %>%
      str_extract("^[^-]+") %>%
      str_trim()
  }, error = function(e) NULL)
  
  return(trade_name)
}

# Function to scrape the entire "Nedsat nyrefunktion" section
extract_nedsat_nyrefunktion <- function(page) {
  nyrefunktion_section <- tryCatch({
    # Find the parent <div> that contains the "Nedsat nyrefunktion" section
    section <- page %>%
      html_element(xpath = "//h3[contains(text(), 'Nedsat nyrefunktion')]/ancestor::div[contains(@class, 'glob-content-section-wrapper')]") %>%
      html_text(trim = TRUE)
    
    if (!is.null(section)) {
      return(section)
    } else {
      return("Nedsat nyrefunktion section not found.")
    }
    
  }, error = function(e) {
    return("Error: Could not scrape the page.")
  })
  
  return(nyrefunktion_section)
}

# Main function to scrape for a given ATC code
scrape_nyrefunktion_for_atc <- function(atc) {
  product_url <- extract_product_url(atc)
  
  if (!is.null(product_url)) {
    page <- tryCatch(read_html(product_url), error = function(e) NULL)
    
    if (!is.null(page)) {
      trade_name <- extract_trade_name(page)
      kidney_output <- extract_nedsat_nyrefunktion(page)
      
      return(list(
        trade_name = trade_name,
        kidney_output = kidney_output
      ))
    }
  }
  
  return(list(
    trade_name = NULL,
    kidney_output = NULL
  ))
}

# Function to retrieve one drug ID per ATC code
get_drug_ids <- function(atc_codes) {
  # Subset the data based on ATC codes, using the correct column "V7" for ATC codes
  drugs_subset <- drugs_data[drugs_data$V7 %in% atc_codes, ]
  
  # Remove duplicates: keep only the first entry per ATC code
  drugs_subset <- drugs_subset %>%
    group_by(V7) %>%  # Group by ATC code
    slice(1) %>%      # Select the first match for each ATC code
    ungroup()
  
  # Extract the drug IDs from the correct "V1" column
  drug_ids <- drugs_subset$V1
  
  return(drug_ids)
}


# Function to generate Stockley's Interactions URL
generate_stockleys_url <- function(atc_codes) {
  base_url <- "https://www.medicinescomplete.com/#/interactions/stockley?terms="
  atc_codes_url <- paste(atc_codes, collapse = ",")
  url <- paste0(base_url, atc_codes_url)
  return(url)
}

# Define UI
ui <- secure_app(fluidPage(
  titlePanel("Lister og interaktioner"),
  
  # Add CSS to style the buttons with vertical alignment and spacing
  tags$style(HTML("
    .btn-custom, .btn-primary {
      display: block;
      width: 100%;
      margin-bottom: 15px;
      text-align: left;
    }
    .btn-custom {
      background-color: #0033A0;
      color: white;
      border: none;
    }
    .btn-custom:hover {
      background-color: #00217A;
      color: white;
    }
  ")),
  
  # JavaScript handler for redirecting the browser to a new URL
  tags$script(HTML("
    Shiny.addCustomMessageHandler('redirect', function(message) {
      window.open(message.url, '_blank');
    });
  ")),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput("atc_codes", label = "Indtast ATC-koder (én pr. linje)", 
                    placeholder = "f.eks. N05AN01\nC03CA01", height = "300px"),
      
      actionButton("generate_anticholinergic", "Antikolinerg belastning", class = "btn btn-primary"),
      actionButton("generate_seponeringslisten", "Seponeringslisten", class = "btn btn-primary"),
      actionButton("generate_qtc", "QTc-forlængelse", class = "btn btn-primary"),
      actionButton("generate_serotonergic", "Serotonergt load", class = "btn btn-primary"),
      actionButton("generate_bleeding_risk", "Blødningsrisiko", class = "btn btn-primary"),
      actionButton("generate_renal", "Nyrefunktion fra pro.medicin.dk", class = "btn btn-primary"),
      actionButton("generate_interaktionsdatabasen", "Interaktionsdatabasen.dk", class = "btn btn-primary"),
      actionButton("generate_interaksjoner", "Interaksjoner.no", class = "btn btn-primary"),
      actionButton("generate_stockleys", "Stockley's Drug Interactions", class = "btn btn-primary"),
      br(),
      actionButton("mark_and_copy", "Markér og kopier indhold", class = "btn-custom"),
      actionButton("clear_output", "Ryd indhold", class = "btn-custom")
    ),
    
mainPanel(
      div(id = "all_output",
          uiOutput("drug_name_output"),  
          hr(style = "border-top:1px solid black;"),   
          
          uiOutput("generic_atc_codes"),
          hr(style = "border-top:1px solid black;"),
          
          uiOutput("acb_output"),
          uiOutput("acb_atc_codes"),
          uiOutput("acb_cat3_output"),
          uiOutput("acb_cat2_output"),
          uiOutput("acb_cat1_output"),
          hr(style = "border-top:1px solid black;"),
          
          uiOutput("seponeringslisten_output"),
          uiOutput("seponeringslist_atc_codes"),
          hr(style = "border-top:1px solid black;"),
          
          uiOutput("qtc_output"),
          uiOutput("qtc_atc_codes"),
          hr(style = "border-top:1px solid black;"),
          
          uiOutput("serotonergic_output"),
          uiOutput("serotonergic_atc_codes"),
          hr(style = "border-top:1px solid black;"),
          
          uiOutput("bleeding_risk_output"),
          uiOutput("bleeding_risk_atc_codes"),
          hr(style = "border-top:1px solid black;"),
          
          uiOutput("kidney_output"),
          hr(style = "border-top:1px solid black;"),
          
          uiOutput("interaction_url_dk"),
          uiOutput("interaction_url_no"),
          uiOutput("stockleys_url")
      )
    )
  ),
  
  # JavaScript for copying all content
  tags$script(HTML("
    document.getElementById('mark_and_copy').addEventListener('click', function() {
      var allOutput = document.getElementById('all_output');
      var range = document.createRange();
      range.selectNodeContents(allOutput);
      var sel = window.getSelection();
      sel.removeAllRanges();
      sel.addRange(range);

      // Attempt to copy to clipboard
      try {
        document.execCommand('copy');
        alert('Indhold kopieret til udklipsholderen');
      } catch (err) {
        alert('Indholdet kunne ikke kopieres til udklipsholderen');
      }
    });
  "))
))

# Revised Server function
server <- function(input, output, session) {

 res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Reactive expression to clean and process ATC codes input
  atc_codes <- reactive({
    input$atc_codes %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      trimws() %>%
      unique() %>%
      .[nzchar(.)]  # Remove empty lines
  })
  
  # Helper function to capitalize only the first letter of each drug name, ensuring proper encoding
  capitalize_first_letter <- function(x) {
    sapply(x, function(name) {
      # Convert to UTF-8 and remove any invalid characters
      name <- iconv(name, from = "ISO-8859-1", to = "UTF-8", sub = "")  # Convert from common Danish encoding (ISO-8859-1)
      tolower(name) %>%
        sub("^.", toupper(substr(name, 1, 1)), .)  # Capitalize the first letter, keep others lowercase
    })
  }
  
  # Reactive expression to find corresponding drug names from drugs_data
  drug_names <- reactive({
    if (length(atc_codes()) > 0) {
      found <- drugs_data %>%
        filter(V7 %in% atc_codes()) %>%  # Merging on column "V7" for ATC codes
        group_by(V7) %>%  # Group by ATC code to get only the first occurrence per code
        slice(1) %>%      # Select the first match for each ATC code
        ungroup()
      
      # Get the ATC codes that were not found
      not_found <- setdiff(atc_codes(), found$V7)
      
      list(
        found_names = found %>% pull(V5) %>% capitalize_first_letter(),
        not_found_codes = not_found
      )
    } else {
      NULL
    }
  })
  
  # Render drug names and not found codes as soon as ATC codes are entered
  output$drug_name_output <- renderUI({
    if (!is.null(drug_names())) {
      output_text <- ""
      
      # Display found drug names
      if (length(drug_names()$found_names) > 0) {
        output_text <- paste("<b>Generiske navne:</b><br>", paste(drug_names()$found_names, collapse = "<br>"))
      }
      
      # Display not found ATC codes if there are any
      if (length(drug_names()$not_found_codes) > 0) {
        not_found_msg <- paste("<br><b>Følgende ATC-koder blev ikke fundet på listen over godkendte lægemidler:</b><br>", 
                               paste(drug_names()$not_found_codes, collapse = "<br>"))
        output_text <- paste(output_text, not_found_msg, sep = "")
      }
      
      HTML(output_text)
    } else {
      HTML("<b></b>")
    }
  })
  
  # Render comma-separated ATC codes for the generic names output
  output$generic_atc_codes <- renderUI({
    if (length(atc_codes()) > 0) {
      HTML(paste("<b>ATC-koder:</b><br>", paste(atc_codes(), collapse = ", "),"<br>"))
    } else {
      HTML("")
    }
  })
  
  # Calculate ACB score when the "Antikolinerg belastning" button is clicked
  observeEvent(input$generate_anticholinergic, {
    acb_result <- get_acb_score(atc_codes())
    
    # Render ACB score output
    output$acb_output <- renderUI({
      if (length(atc_codes()) > 0) {
        # Prepare list of drugs with anticholinergic burden
        drug_results <- if (length(acb_result$found_drugs) > 0) {
          paste0("<b>Lægemidler med antikolinerg belastning:</b><br>",
                 paste(acb_result$found_drugs, collapse = "<br>"), "<br><br>",
                 "<b>Matchede ATC-koder:</b> ",
                 paste(acb_result$found_atc_codes, collapse = ", "), "<br><br>")
        } else {
          "<b>Ingen lægemidler med antikolinerg belastning fundet.</b><br><br>"
        }
        
        # Combine header, ACB score, and drug results
        HTML(paste0(
          "<h4 style='font-size:16px;'><b>Antikolinerg belastning (kilde: IRF’s liste over antikolinerge lægemidler, opdateret 2023):</b></h4>",
          "<b style='font-size: 20px;'>ACB score:</b> ",
          "<span style='font-size: 20px; color: #000;'>", acb_result$score, "</span><br><br>",
          drug_results
        ))
      } else {
        HTML("<b>Ingen ATC-koder angivet.</b>")
      }
    })
    
  })
  
  # Render the ACB score categories only when the "Antikolinerg belastning" button is clicked
  observeEvent(input$generate_anticholinergic, {
    
    # Reactive expression for drugs with ACB score 3
    drugs_acb3 <- reactive({
      acb_data %>%
        filter(DKACB == 3 & ATC %in% atc_codes()) %>%
        pull(`Name of ATC Code`)
    })
    
    # Reactive expression for drugs with ACB score 2
    drugs_acb2 <- reactive({
      acb_data %>%
        filter(DKACB == 2 & ATC %in% atc_codes()) %>%
        pull(`Name of ATC Code`)
    })
    
    # Reactive expression for drugs with ACB score 1
    drugs_acb1 <- reactive({
      acb_data %>%
        filter(DKACB == 1 & ATC %in% atc_codes()) %>%
        pull(`Name of ATC Code`)
    })
    
    # Render headers and drug names for ACB score 3
    output$acb_cat3_output <- renderUI({
      if (length(drugs_acb3()) > 0) {
        HTML(paste0("<h4 style='font-size:16px;'>Lægemidler med ACB-score på 3:</h4>",
                    paste(drugs_acb3(), collapse = "<br>")))
      } else {
        ""
      }
    })
    
    # Render headers and drug names for ACB score 2
    output$acb_cat2_output <- renderUI({
      if (length(drugs_acb2()) > 0) {
        HTML(paste0("<h4 style='font-size:16px;'>Lægemidler med ACB-score på 2:</h4>",
                    paste(drugs_acb2(), collapse = "<br>")))
      } else {
        ""
      }
    })
    
    # Render headers and drug names for ACB score 1
    output$acb_cat1_output <- renderUI({
      if (length(drugs_acb1()) > 0) {
        HTML(paste0("<h4 style='font-size:16px;'>Lægemidler med ACB-score på 1:</h4>",
                    paste(drugs_acb1(), collapse = "<br>")))
      } else {
        ""
      }
    })
  })
  
  # Reactive expression to handle Seponeringslisten logic
  observeEvent(input$generate_seponeringslisten, {
    
    # Merge input ATC codes with sepo_data using flexible matching
    seponerings_data <- reactive({
      if (length(atc_codes()) > 0) {
        # Filter sepo_data by checking if any ATC code in sepo_data is a prefix of the input ATC codes
        matched_sepo_data <- sepo_data %>%
          filter(sapply(ATC, function(x) any(startsWith(atc_codes(), x))))  # Flexible matching with startsWith
        
        matched_sepo_data %>%
          select(ATC, Anbefaling)  # Select ATC and Anbefaling columns
      } else {
        NULL
      }
    })
    
    # Render the Seponeringslisten output
    output$seponeringslisten_output <- renderUI({
      if (!is.null(seponerings_data()) && nrow(seponerings_data()) > 0) {
        # Generate individual rows for the Seponeringslisten results
        seponerings_results <- lapply(1:nrow(seponerings_data()), function(i) {
          row <- seponerings_data()[i, ]
          paste0(
            "<b>ATC-kode:</b> ", row$ATC, "<br>",
            "<b>Anbefaling:</b> ", row$Anbefaling, "<br><br>"
          )
        })
        
        # Combine header and results
        HTML(paste0(
          "<h4 style='font-size:16px;'><b>Seponeringslisten 2025 (kilde: Sundhedsstyrelsen):</b></h4>",
          paste(seponerings_results, collapse = " ")
        ))
      } else {
        HTML("<b>Ingen anbefalinger fra seponeringslisten fundet.</b>")
      }
    })
    
    # Render comma-separated list of matched ATC codes
    output$seponeringslist_atc_codes <- renderUI({
      if (!is.null(seponerings_data()) && nrow(seponerings_data()) > 0) {
        matched_atc_codes <- unique(seponerings_data()$ATC)  # Unique list of matched ATC codes
        HTML(paste("<b>Matchede ATC-koder:</b>", paste(matched_atc_codes, collapse = ", ")))
      } else {
        HTML("")
      }
    })
    
  })
  
  # Reactive expression to handle QTc-forlængelse logic
  observeEvent(input$generate_qtc, {
    
    # Merge input ATC codes with qtc_data
    qtc_data_filtered <- reactive({
      if (length(atc_codes()) > 0) {
        qtc_data %>%
          filter(`ATC:` %in% atc_codes())  # Match input ATC codes with "ATC" in qtc_data
      } else {
        NULL
      }
    })
    
    # Render the QTc-forlængelse output
    output$qtc_output <- renderUI({
      if (!is.null(qtc_data_filtered()) && nrow(qtc_data_filtered()) > 0) {
        qtc_results <- lapply(1:nrow(qtc_data_filtered()), function(i) {
          row <- qtc_data_filtered()[i, ]
          
          # Generic name
          generic_name <- row$`Generic Name`
          
          # QTc Risk Category
          qtc_risk <- if (!is.na(row$`QTc kategori (special=0, possible=1, known=2, conditional=3)`)) {
            if (row$`QTc kategori (special=0, possible=1, known=2, conditional=3)` == 1) {
              "Mulig risiko for QTc-forlængelse"
            } else if (row$`QTc kategori (special=0, possible=1, known=2, conditional=3)` == 2) {
              "Kendt risiko for QTc-forlængelse"
            } else if (row$`QTc kategori (special=0, possible=1, known=2, conditional=3)` == 3) {
              paste0("Betinget risiko for QTc-forlængelse. Betingelse: ", row$Condition)
            } else if (row$`QTc kategori (special=0, possible=1, known=2, conditional=3)` == 0) {
              "Ikke risiko for QTc-forlængelse"
            } else {
              ""
            }
          } else {
            "Ukendt QTc-forlængelsesstatus"
          }
          
          # Avoid in congenital long QT syndrome
          avoid_lqt <- if (!is.na(row$`Avoid in congenital long QT (0=nej, 1=ja)`)) {
            if (row$`Avoid in congenital long QT (0=nej, 1=ja)` == 1) {
              "Ja"
            } else {
              "Nej"
            }
          } else {
            "Ukendt"
          }
          
          # Output the result for each row
          paste0("<b>Generisk navn:</b> ", generic_name, "<br>",
                 "<b>QTc-forlængelse:</b> ", qtc_risk, "<br>",
                 "<b>Undgå ved arveligt langt QT syndrom:</b> ", avoid_lqt, "<br><br>")
        })
        
        # Extract matched ATC codes
        matched_atc_codes <- qtc_data_filtered()$`ATC:` %>%
          unique() %>%
          paste(collapse = ", ")
        
        HTML(paste0(
          "<h4 style='font-size:16px;'><b>QTc-forlængende lægemidler (kilde: CredibleMeds, opdateret 8. august 2024):</b></h4>",
          paste(qtc_results, collapse = " "),
          "<br><b>Matchede ATC-koder:</b> ", matched_atc_codes
        ))
      } else {
        HTML("<b>Ingen QTc-forlængende lægemidler fundet.</b>")
      }
    })
  })
  
  # Reactive expression to handle serotonergic load logic
  observeEvent(input$generate_serotonergic, {
    
    # Merge input ATC codes with ss_data
    ss_data_filtered <- reactive({
      if (length(atc_codes()) > 0) {
        ss_data %>%
          filter(ATC %in% atc_codes())  # Match input ATC codes with "ATC" in ss_data
      } else {
        NULL
      }
    })
    
    # Render the serotonergic load output with header
    output$serotonergic_output <- renderUI({
      if (!is.null(ss_data_filtered()) && nrow(ss_data_filtered()) > 0) {
        serotonergic_results <- lapply(1:nrow(ss_data_filtered()), function(i) {
          row <- ss_data_filtered()[i, ]
          
          # Extract the name from the "Name" column
          name <- row$Name
          
          # Output the result for each row
          paste0(name)
        })
        
        HTML(paste0("<h4 style='font-size:16px;'><b>Serotonerg belastning:</b></h4><br>",
                    paste(serotonergic_results, collapse = ", ")))
      } else {
        HTML("<b>Ingen lægemidler med serotonerg aktivitet fundet.</b>")
      }
    })
    
    # Render comma-separated list of matched ATC codes for serotonergic load
    output$serotonergic_atc_codes <- renderUI({
      if (!is.null(ss_data_filtered()) && nrow(ss_data_filtered()) > 0) {
        matched_atc_codes <- unique(ss_data_filtered()$ATC)  # Unique list of matched ATC codes
        HTML(paste("<b>Matchede ATC-koder:</b>", paste(matched_atc_codes, collapse = ", ")))
      } else {
        HTML("")
      }
    })
    
  })
  
  # Reactive expression to handle Blødningsrisiko logic
  observeEvent(input$generate_bleeding_risk, {
    
    # Reactive expression to filter the bleeding risk data based on input ATC codes
    bleeding_data <- reactive({
      if (length(atc_codes()) > 0) {
        # Filter bleed_data by checking if any ATC code in bleed_data is a prefix of the input ATC codes
        matched_bleed_data <- bleed_data %>%
          filter(sapply(`ATC-kode`, function(x) any(startsWith(atc_codes(), x))))  # Flexible matching with startsWith
        
        matched_bleed_data %>%
          select(`ATC-kode`, Navn, Note)  # Select relevant columns
      } else {
        NULL
      }
    })
    
    # Reactive expression to determine matched input ATC codes
    matched_input_atc_codes <- reactive({
      if (!is.null(bleeding_data()) && nrow(bleeding_data()) > 0) {
        input_codes <- atc_codes()
        matched_codes <- input_codes[sapply(input_codes, function(x) any(startsWith(x, bleeding_data()$`ATC-kode`)))]
        matched_codes
      } else {
        NULL
      }
    })
    
    # Render the Blødningsrisiko output
    output$bleeding_risk_output <- renderUI({
      if (!is.null(bleeding_data()) && nrow(bleeding_data()) > 0) {
        # Generate individual rows for the bleeding risk results
        bleeding_results <- lapply(1:nrow(bleeding_data()), function(i) {
          row <- bleeding_data()[i, ]
          name <- row$Navn
          note <- row$Note
          matched_atc <- paste(matched_input_atc_codes()[startsWith(matched_input_atc_codes(), row$`ATC-kode`)], collapse = ", ")
          
          # Include matched ATC codes and note if it exists
          if (!is.na(note) && note != "") {
            paste0("<b>", name, " (", matched_atc, "):</b> ", note, "<br>")
          } else {
            paste0("<b>", name, " (", matched_atc, ")</b><br>")
          }
        })
        
        # Combine header and results
        HTML(paste0(
          "<h4 style='font-size:16px;'><b>Blødningsrisiko (kilde KFA's egen liste 2024):</b></h4><br>",
          paste(bleeding_results, collapse = " ")
        ))
      } else {
        HTML("<b>Ingen lægemidler der giver øget blødningsrisiko fundet.</b>")
      }
    })
    
    # Render comma-separated list of matched input ATC codes
    output$bleeding_risk_atc_codes <- renderUI({
      if (!is.null(matched_input_atc_codes()) && length(matched_input_atc_codes()) > 0) {
        HTML(paste("<b>Matchede ATC-koder:</b>", paste(matched_input_atc_codes(), collapse = ", ")))
      } else {
        HTML("")
      }
    })
    
  })
  
  
  
  
  # Handle the button click to scrape data for multiple ATC codes
  observeEvent(input$generate_renal, {
    
    # Set output to NULL initially to trigger the loading indicator
    output$kidney_output <- renderUI({
      NULL
    })
    
    # Get the ATC codes from the input
    atc_codes <- input$atc_codes %>%
      strsplit(split = "\n") %>%
      unlist() %>%
      trimws() %>%
      unique() %>%
      .[nzchar(.)]  # Remove empty lines
    
    # Iterate through each ATC code and scrape the necessary data
    kidney_output <- lapply(atc_codes, function(atc) {
      result <- scrape_nyrefunktion_for_atc(atc)
      
      if (!is.null(result$trade_name) && !is.null(result$kidney_output)) {
        paste0("<b>ATC-kode:</b> ", atc, "<br>",
               "<b>Handelsnavn:</b> ", result$trade_name, "<br>",
               "<b>Tekst:</b><br>", result$kidney_output, "<br><br>")
      } else {
        paste0("<b>ATC-kode:</b> ", atc, " - Ingen data fundet.<br><br>")
      }
    })
    
    # Render the results in the UI with a header and spacing
    output$kidney_output <- renderUI({
      HTML(paste0(
        "<h4 style='font-size:16px; margin-top:30px;'><b>Nyrefunktionsafsnit fra pro.medicin.dk:</b></h4><br>",  # Add header
        paste(kidney_output, collapse = " ")
      ))
    })
  })
  
  
  # Check interactions on interaktionsdatabasen.dk
  observeEvent(input$generate_interaktionsdatabasen, {
    atc_codes <- atc_codes()
    drug_ids <- get_drug_ids(atc_codes)
    url <- paste0("http://interaktionsdatabasen.dk/SearchResult.aspx?pids=", paste(drug_ids, collapse = ","))
    
    session$sendCustomMessage(type = "redirect", message = list(url = url))
  })
  
  # Check interactions on interaksjoner.no
  observeEvent(input$generate_interaksjoner, {
    atc_codes <- atc_codes()
    atc_codes_url <- paste(atc_codes, collapse = "%0D%0A")
    url <- paste0("https://interaksjoner.no/results.html?PreparatNavn=", atc_codes_url)
    
    session$sendCustomMessage(type = "redirect", message = list(url = url))
  })
  
  # Check interactions on Stockley's Drug Interactions
  observeEvent(input$generate_stockleys, {
    atc_codes <- atc_codes()
    url <- generate_stockleys_url(atc_codes)
    
    session$sendCustomMessage(type = "redirect", message = list(url = url))
  })
  
  observeEvent(input$clear_output, {
    output$acb_output <- renderUI({ NULL })
    output$kidney_output <- renderUI({ NULL })
    output$acb_cat3_output <- renderUI({ NULL })
    output$acb_cat2_output <- renderUI({ NULL })
    output$acb_cat1_output <- renderUI({ NULL })
    output$seponeringslisten_output <- renderUI({ NULL })
    output$seponeringslist_atc_codes <- renderUI({ NULL })
    output$qtc_output <- renderUI({ NULL })
    output$serotonergic_output <- renderUI({ NULL })
    output$serotonergic_atc_codes <- renderUI({ NULL })
    output$bleeding_risk_output <- renderUI({ NULL })
    output$bleeding_risk_atc_codes <- renderUI({ NULL })
    output$kidney_output <- renderUI({ NULL })
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
