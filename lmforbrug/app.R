# app.R
# This Shiny app reads pre-processed data and provides interactive visualization with fixed toggles and stratification handling.

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load cleaned data
merged_data <- readRDS('data/merged_data.rds')

# Define UI for the Shiny app
ui <- fluidPage(
  titlePanel("Medicinforbrug fra eSundhed.dk"),
  
  sidebarLayout(
    sidebarPanel(
      textAreaInput(
        inputId = "atc_input",
        label = "Indtast ATC-kode:",
        rows = 1
      ),
      uiOutput("product_selection"),
      uiOutput("subst_group_selection"),
      uiOutput("region_selection"),
      radioButtons(
        inputId = "stratify_option",
        label = "Stratificering af data:",
        choices = list(
          "Ingen" = "None",
          "Produktnavn" = "Produktnavn",
          "Substitutionsgruppe" = "SubstGruppe",
          "Region" = "Område"
        ),
        selected = "None"
      ),
      radioButtons(
        inputId = "y_axis_option",
        label = "Y-akse:",
        choices = list(
          "Samlede tilskudsudgifter, kr." = "Regionale udgifter til medicintilskud, kr.",
          "Tilskudsudgifter per 100.000 indbyggere, kr." = "Regionale udgifter til medicintilskud, kr. pr. indbygger",
          "Samlet forbrug" = "Mængdesalg",
          "Forbrug pr. 100.000 indbyggere" = "Mængdesalgsenhed pr. indbygger"
        ),
        selected = "Regionale udgifter til medicintilskud, kr."
      ),
      downloadButton("download_pdf", "Download figur som PDF")
    ),
    
    mainPanel(
      plotOutput("line_plot"),
      tableOutput("summary_table")
    )
  )
)

# Define server logic for the Shiny app
server <- function(input, output, session) {
  
  # Y-axis label mapping (missing definition)
  y_axis_labels <- list(
    "Regionale udgifter til medicintilskud, kr." = "Samlede tilskudsudgifter, kr.",
    "Regionale udgifter til medicintilskud, kr. pr. indbygger" = "Tilskudsudgifter per 100.000 indbyggere, kr.",
    "Mængdesalg" = "Samlet forbrug",
    "Mængdesalgsenhed pr. indbygger" = "Forbrug pr. 100.000 indbyggere"
  )
  
  filtered_data <- reactive({
    atc_codes <- unlist(strsplit(input$atc_input, "\n"))
    atc_codes <- atc_codes[atc_codes != ""]
    if (length(atc_codes) == 0) return(data.frame())
    data <- merged_data %>%
      filter(`ATC kode` %in% atc_codes) %>%
      tidyr::separate_rows(SubstGruppe, sep = ", ")
    return(data)
  })
  
  # Extract ATC text for plot title (missing definition restored)
  atc_text <- reactive({
    data <- filtered_data()
    if (nrow(data) > 0) {
      unique_texts <- unique(data$`ATC tekst`)
      paste(unique_texts, collapse = ", ")
    } else {
      "Medicinforbrug"
    }
  })
  
  checkbox_ui_with_toggle <- function(id, label, choices) {
    checkboxGroupInput(id, label, choices = c("Markér alle", choices), selected = c("Markér alle", choices))
  }
  
  observe_toggle <- function(inputId, input_choices) {
    observeEvent(input[[inputId]], {
      if ("Markér alle" %in% input[[inputId]]) {
        updateCheckboxGroupInput(session, inputId, selected = c("Markér alle", input_choices))
      } else if (setequal(input[[inputId]], input_choices)) {
        updateCheckboxGroupInput(session, inputId, selected = character(0))
      }
    })
  }
  
  output$product_selection <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    products <- unique(data$Produktnavn)
    checkbox_ui_with_toggle("selected_products", "Vælg produktnavne:", products)
  })
  
  output$subst_group_selection <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    subst_groups <- unique(unlist(strsplit(paste(data$SubstGruppe, collapse = ", "), ", ")))
    checkbox_ui_with_toggle("selected_subst_groups", "Vælg substitutionsgrupper:", subst_groups)
  })
  
  output$region_selection <- renderUI({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    regions <- unique(data$Område)
    checkbox_ui_with_toggle("selected_regions", "Vælg regioner:", regions)
  })
  
  observe({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    products <- unique(data$Produktnavn)
    subst_groups <- unique(unlist(strsplit(paste(data$SubstGruppe, collapse = ", "), ", ")))
    regions <- unique(data$Område)
    
    observe_toggle("selected_products", products)
    observe_toggle("selected_subst_groups", subst_groups)
    observe_toggle("selected_regions", regions)
  })
  
  # Create a reactive version of the plot to use for both display and download
  plot_reactive <- reactive({
    data <- filtered_data()
    req(nrow(data) > 0)
    
    data <- data %>%
      filter(
        Produktnavn %in% input$selected_products,
        SubstGruppe %in% input$selected_subst_groups,
        Område %in% input$selected_regions
      )
    
    y_var <- input$y_axis_option
    y_label <- y_axis_labels[[y_var]]
    
    if (y_var %in% c("Regionale udgifter til medicintilskud, kr. pr. indbygger", "Mængdesalgsenhed pr. indbygger")) {
      data[[y_var]] <- data[[y_var]] * 100000
    }
    
    stratify_var <- input$stratify_option
    group_vars <- c("Dato")
    
    if (stratify_var != "None") {
      group_vars <- c(group_vars, stratify_var)
    }
    
    data <- data %>%
      group_by(across(all_of(group_vars))) %>%
      summarise(across(all_of(y_var), sum, na.rm = TRUE), .groups = "drop")
    
    ggplot(data, aes(x = Dato, y = .data[[y_var]], color = if (stratify_var != "None") .data[[stratify_var]] else NULL)) +
      geom_line(size=1) +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()), limits = c(0, NA)) +
      labs(title = atc_text(), x = "Dato", y = y_label, color = if (stratify_var != "None") stratify_var else NULL) +
      theme_minimal(base_size = 16)
  })
  
  # Update renderPlot to use the reactive plot
  output$line_plot <- renderPlot({
    plot_reactive()
  })
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("medicinforbrug_plot_", Sys.Date(), ".pdf")
    },
    content = function(file) {
      pdf(file, width = 12, height = 8)  # Set desired dimensions
      print(plot_reactive())  # Ensure the plot is printed to the PDF
      dev.off()  # Close the PDF device
    }
  )
  
  # Generate summary table
  output$summary_table <- renderTable({
    data <- filtered_data()
    if (nrow(data) == 0) return(NULL)
    
    summary_data <- data %>%
      group_by(SubstGruppe, Styrke) %>%
      summarise(
        Lægemiddelform = paste(first(LægemiddelForm)),
        Produktnavne = paste(unique(Produktnavn), collapse = ", "),
        `Gennemsnitlig pris pr. DDD` = mean(`Pris, kr. pr. solgt enhed`, na.rm = TRUE),
        .groups = "drop"
      )
    return(summary_data)
  })
  
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
