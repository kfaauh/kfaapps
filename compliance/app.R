library(shiny)
library(shinymanager)

# 1) Brug secure_app(...) – ikke secure_page(...)
credentials <- data.frame(
  user = "KFA",
  password = "kfekfa123",
  stringsAsFactors = FALSE
)

ui <- secure_app(  # <-- her: secure_app, ikke secure_page
  fluidPage(

    # Titel
    titlePanel("Kør compliance-appen lokalt på egen computer"),

    # Introduktion
    h3("Vejledning:"),
    p("1) Installér R og RStudio via link nedenfor (husk at aktivere Heimdal Agent på AU-computere)"),
    p("2) Download 'ListeOverGodkendteLaegemidler.xlsx' og gem første ark som UTF-8 CSV-fil med navnet 'ListeOverGodkendteLaegemidler'"),
    p("3) Åbn RStudio og opret et nyt R script (Ctrl+Shift+N)"),
    p("4) Kopiér scriptet nedenfor ind i det nye R script og gem scriptet som eks. 'Compliance-app'"),
    p("5) Følg guiden inde i scriptet til at angive den korrekte fil-sti"),
    p("6) Kør appen"),

    # Links
    h3("Links"),
    p("Links til at downloade R og RStudio samt at downloade 'ListeOverGodkendteLaegemidler.xlsx'"),
    tags$ul(
      tags$li(
        a("Download R og RStudio",
          href = "https://posit.co/download/rstudio-desktop/",
          target = "_blank")
      ),
      tags$li(
        a("Download Excel-fil",
          href = "https://laegemiddelstyrelsen.dk/LinkArchive.ashx?id=0BD4960F0D7744E3BABC951431681ECC&lang=da",
          target = "_blank")
      )
    ),

    # Boksen med script og 'Kopier script'-knap
    wellPanel(
      h4("Kopier dette script ind i RStudio:"),
      tags$textarea(
        id    = "scriptContent",
        style = "width:100%; height:300px;",
        "
# Funktioner til at installere R-pakker (kan slettes efter pakkerne er installeret første gang)
install.packages('shiny')
install.packages('tidyverse')
install.packages('lubridate')
install.packages('readr')
install.packages('zoo')

# Indlæs pakker
library(shiny)
library(tidyverse)
library(lubridate)
library(readr)
library(zoo)

setwd(\"C:/Users/...\") # Indsæt sti til mappe hvor \"ListeOverGodkendteLaegemidler.csv\" ligger
                      # Guide:
                      # 1) Vælg \"Session\" -> \"Set Working Directory\" -> \"Choose Directory...\" (eller Ctrl+Shift+H)
                      # 2) Navigér til mappen og tryk \"Open\"
                      # 3) Funktionen der angiver stien kommer nu frem nede i \"Console\" som \"setwd(\"*Sti til mappen*\")\"
                      # 4) Kopiér funktionen og sæt ind i sciptet i stedet for den nuværende
                      # 5) Gem scriptet og tryk \"Run app\" eller tryk Alt+Ctrl+R
                      # 6) Ovenstående skal kun gøres første gang, efterfølgende kan man bare trykke \"Run app\" eller Alt+Ctrl+R
                      # 7) Hvis alt virker, som det skal, kan install.packages-linjerne slettes

# Definér brugergrænsefladen
ui <- fluidPage(
  titlePanel(\"Effektueringer, dagligt forbrug og visualiseringer\"),
  sidebarLayout(
    sidebarPanel(
      fileInput(\"file\", \"Upload .csv-fil\", accept = \".csv\"),
      downloadButton(\"downloadData\", \"Download .csv-fil med processeret data\")
    ),
    mainPanel(
      tableOutput(\"table\"),
      verbatimTextOutput(\"message\"),
      # Conditional panel to display the legend only when data is present
      conditionalPanel(
        condition = \"output.plots !== null\",
        div(
          style = \"margin-top: 20px;\",
          tags$h4(\"Forklaring til graferne\"),
          tags$div(
            style = \"display: flex; align-items: center;\",
            tags$span(style = \"border-bottom: 2px solid black; width: 30px; height: 0px; display: inline-block; margin-right: 5px;\"),
            tags$span(\"Dagligt gennemsnit for perioden\")
          ),
          tags$div(
            style = \"display: flex; align-items: center; margin-top: 5px;\",
            tags$span(style = \"border-bottom: 2px dashed blue; width: 30px; height: 0px; display: inline-block; margin-right: 5px;\"),
            tags$span(\"Rullende gennemsnit for de sidste 3 perioder\")
          ),
          tags$div(
            style = \"display: flex; align-items: center; margin-top: 5px;\",
            tags$span(style = \"border-bottom: 2px solid red; width: 30px; height: 0px; display: inline-block; margin-right: 5px;\"),
            tags$span(\"Gennemsnit (totalt)\")
          )
        )
      ),
      uiOutput(\"plots\") # Output for plots
    )
  )
)

# Definér serveren
server <- function(input, output, session) {
  # Indlæs og processér den indlæste fil
  processed_data <- reactive({
    req(input$file)
    tryCatch({
      # Læs den uploadede fil i ISO-8859-1-format
      data <- read.csv(input$file$datapath, header = FALSE, sep = \";\", stringsAsFactors = FALSE, fileEncoding = \"ISO-8859-1\") %>%
        select(V1:V5) %>%
        rename(Dato = V1, Lægemiddel = V2, Styrke = V3, Pakkestørrelse = V4, Antal.pakker = V5) %>%
        mutate(Dato = dmy(Dato)) %>%  # Konvertér datoformat
        mutate(
          Lægemiddel = gsub(pattern = '\"[^\"]+\"|e$', replacement = '', x = Lægemiddel),
          first_word = str_extract(Lægemiddel, \"\\\\w+\")
        ) %>% # Første ord af drugname
        mutate(pack_numeric = as.numeric(gsub(\",\", \".\", str_extract(Pakkestørrelse, \"\\\\d+,?\\\\d*\")))) # Håndter danske decimaler og konverter til numerisk

      # Læs listen over godkendte lægemidler, vælg relevante kolonner og rens kolonnen med lægemiddelnavne
      drugs <- read.csv2(\"ListeOverGodkendteLaegemidler.csv\", header = FALSE, sep = \";\", fileEncoding = \"ISO-8859-1\") %>%
        select(Lægemiddel = V2, Styrke = V4, ATC = V7) %>%
        filter(str_length(ATC) <= 7) %>%
        distinct(Lægemiddel, Styrke, ATC, .keep_all = TRUE) %>%
        mutate(Lægemiddel = str_replace(Lægemiddel, pattern = '\"[^\"]+\"|e$', replacement = '')) %>%
        mutate(first_word = str_extract(Lægemiddel, \"\\\\w+\")) # Udtræk det første ord af lægemiddelnavnet

      data <- data %>%
        distinct(first_word, Styrke, Dato, .keep_all = TRUE) # Sikr at rækkerne i data er unikke

      drugs <- drugs %>%
        distinct(first_word, Styrke, .keep_all = TRUE) # Sikr at rækkerne i 'drugs' er unikke

      # Sammenkobling at uploadet fil og godkendte lægemidler
      atcdata <- left_join(data, drugs, by = c(\"first_word\", \"Styrke\")) %>%
        rename(Lægemiddel = Lægemiddel.x) %>%
        distinct(Dato, Lægemiddel, Styrke, Pakkestørrelse, Antal.pakker, ATC, pack_numeric, .keep_all = FALSE) %>% # Fjern duplikater efter join
        group_by(ATC, Dato) %>%
        summarize(
          Lægemiddel = last(Lægemiddel), # Behold første lægemiddelnavn
          `Total dosis` = sum(pack_numeric * as.numeric(Antal.pakker) * as.numeric(gsub(\",\", \".\", str_extract(Styrke, \"\\\\d+,?\\\\d*\"))), na.rm = TRUE) # Udregn totale dosis direkte
        ) %>%
        ungroup() %>%
        arrange(ATC, Dato) %>%
        group_by(ATC) %>% # Gruppér efter ATC-kode
        mutate(
          `Dagligt forbrug (gennemsnit for aktuelle periode)` = round(`Total dosis` / abs(as.numeric(difftime(Dato, lead(Dato), units = \"days\"))), digits = 1),
          # Tilføj et rullende gennemsnit over tre perioder
          `Rullende gennemsnit (sidste 3 perioder)` = zoo::rollmean(`Dagligt forbrug (gennemsnit for aktuelle periode)`, k = 3, fill = NA, align = \"right\"),  
          # Ekskluder sidste datos dosis fra udregningen
          total_dosage = sum(`Total dosis`[Dato != max(Dato)], na.rm = TRUE),
          # Og tæl antal dage fra første til sidste dosis
          total_days   = as.numeric(max(Dato) - min(Dato)),
          # Genudregn gennemsnit for hele perioden
          `Gennemsnit for hele perioden` = round(total_dosage / total_days, digits = 1),
          Dato = format(Dato, \"%d-%m-%Y\")
        ) %>%
        ungroup() %>%
        select(Dato, Lægemiddel, ATC, `Total dosis`, `Dagligt forbrug (gennemsnit for aktuelle periode)`, `Rullende gennemsnit (sidste 3 perioder)`, `Gennemsnit for hele perioden`)

      return(atcdata)
    }, error = function(e) {
      output$message <- renderPrint({
        paste(\"Error:\", e$message)
      })
      return(NULL)
    })
  })

  output$table <- renderUI({
    data <- processed_data()
    if (is.null(data)) return(NULL)

    # Split data efter ATC‐kode
    groups <- split(data, data$ATC)

    # For hver ATC‐gruppe, lav en overskrift og en tabel med kanter
    group_tables <- lapply(names(groups), function(atc) {
      df <- groups[[atc]]

      # Fjern ATC‐kolonnen
      df <- df[, !(names(df) %in% \"ATC\")]

      # Lav en overskrift for gruppen med en baggrundsfarve
      header <- tags$h3(
        style = \"background-color: #f0f0f0; padding: 10px; margin: 0;\",
        paste(atc)
      )

      # Lav en overskriftsrække uden ATC‐kolonnen
      table_header <- tags$tr(
        lapply(names(df), function(colname) {
          tags$th(colname, style = \"padding: 8px; border: 1px solid #ddd; background-color: #e0e0e0;\")
        })
      )

      # Generér data‐rækkerne
      table_rows <- lapply(seq_len(nrow(df)), function(i) {
        tags$tr(
          lapply(df[i, ], function(cell) {
            tags$td(cell, style = \"padding: 8px; border: 1px solid #ddd;\")
          })
        )
      })

      # Kombinér overskrifter og data i en tabel
      tags$div(
        style = \"margin-bottom: 20px;\",
        header,
        tags$table(
          style = \"width: 100%; border-collapse: collapse;\",
          table_header,
          table_rows
        )
      )
    })

    do.call(tagList, group_tables)
  })

  # Stil en download‐håndteringsfunktion til rådighed for de bearbejdede data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(\"processeret_data_\", Sys.Date(), \".csv\", sep = \"\")
    },
    content = function(file) {
      # Write the CSV file with semicolons as separators
      write.csv2(processed_data(), file, row.names = FALSE)
    }
  )

  # Generér grafer for hver ATC‐kode
  output$plots <- renderUI({
    data <- processed_data()
    if (is.null(data)) return(NULL)

    atc_list <- unique(data$ATC)

    plot_list <- lapply(atc_list, function(atc_code) {
      plot_data <- data %>% filter(ATC == atc_code)

      # Create a new variable that fills NA with the previous non‐NA value
      plot_data <- plot_data %>%
        mutate(filled_value = if_else(
          is.na(`Dagligt forbrug (gennemsnit for aktuelle periode)`),
          dplyr::lag(`Dagligt forbrug (gennemsnit for aktuelle periode)`, default = first(`Dagligt forbrug (gennemsnit for aktuelle periode)`)),
          `Dagligt forbrug (gennemsnit for aktuelle periode)`
        ))

      # Calculate the minimum, maximum and midpoint dates for positioning the label
      min_date <- min(as.Date(plot_data$Dato, format = \"%d-%m-%Y\"))
      max_date <- max(as.Date(plot_data$Dato, format = \"%d-%m-%Y\"))
      mid_date <- min_date + as.numeric(max_date - min_date) / 2

      # Spring over, hvis alle værdier i Dagligt forbrug (gennemsnit) er NA, eller hvis der ikke er nogen datapunkter
      if (nrow(plot_data) == 0 || all(is.na(plot_data$`Dagligt forbrug (gennemsnit for aktuelle periode)`))) return(NULL)

      # Sikr at total_mean er en enkelt værdi
      total_mean <- unique(plot_data$`Gennemsnit for hele perioden`)[1] # Use the first unique value

        plot_output <- renderPlot({
          ggplot(plot_data, aes(x = as.Date(Dato, format = "%d-%m-%Y"))) +
            geom_step(aes(y = filled_value), direction = "hv") +
            geom_point(aes(y = filled_value), size = 3) +
            geom_line(aes(y = `Rullende gennemsnit (sidste 3 perioder)`), color = "blue", size = 1.2, linetype = "dashed") +
            geom_hline(yintercept = total_mean, color = "red", size = 0.8, linetype = "solid") +
            geom_text(
              x = mid_date, 
              y = total_mean,
              label = paste(total_mean),
              color = "red", 
              hjust = 0.5, 
              vjust = -0.5, 
              size = 5
            ) +
            labs(
              title = paste(atc_code, "| Handelsnavn:", last(plot_data$Lægemiddel)),
              x = "Dato",
              y = "Dagligt forbrug"
            ) +
            theme_minimal() +
            theme(
              text = element_text(size = 14),
              plot.title = element_text(size = 18, face = "bold"),
              axis.title = element_text(size = 16),
              axis.text = element_text(size = 16, face = "bold")
            ) +
            scale_y_continuous(
              expand = c(0.02, 0.02),
              limits = c(0, max(plot_data$`Dagligt forbrug (gennemsnit for aktuelle periode)`, na.rm = TRUE) * 1.1)
            ) +
            scale_x_date(date_labels = "%Y-%m")
        })
        
        plot_output
      })
      
      # Fjern eventuelle NULL-plot outputs
      plot_list <- plot_list[!sapply(plot_list, is.null)]
      
      do.call(tagList, plot_list)
    })
  }

  shinyApp(ui, server)
