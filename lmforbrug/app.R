# app_fixed_local.R
# Shiny app for pre‑processed data with robust stratification
# – SubstGruppe labels include form & strength
# – SubstGruppe selection UI removed

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinymanager)

credentials <- data.frame(
  user     = "KFA",
  password = "kfekfa123",
  stringsAsFactors = FALSE
)

# Load cleaned data
merged_data <- readRDS('data/merged_data.rds')

# Define UI
ui <- secure_app(
  fluidPage(
    titlePanel("Medicinforbrug fra eSundhed.dk"),
    sidebarLayout(
      sidebarPanel(
        textAreaInput("atc_input", "Indtast ATC-kode:", rows = 1),
        uiOutput("product_selection"),
        uiOutput("region_selection"),
        # Removed SubstGruppe option here
        radioButtons("stratify_option", "Stratificering af data:",
                     choices = list(
                       "Ingen"       = "None",
                       "Produktnavn" = "Produktnavn",
                       "Region"      = "Område"
                     ),
                     selected = "None"
        ),
        radioButtons("y_axis_option", "Y-akse:",
                     choices = list(
                       "Samlede tilskudsudgifter, kr."        = "Regionale udgifter til medicintilskud, kr.",
                       "Tilskudsudgifter per 100.000 indbyggere, kr." = "Regionale udgifter til medicintilskud, kr. pr. indbygger",
                       "Samlet forbrug"                       = "Mængdesalg",
                       "Forbrug pr. 100.000 indbyggere"            = "Mængdesalgsenhed pr. indbygger"
                     ),
                     selected = "Regionale udgifter til medicintilskud, kr."
        ),
        downloadButton("download_pdf", "Download figur som PDF")
      ),
      mainPanel(
        plotOutput("line_plot"),
        tableOutput("product_table"),
        br(),
        tableOutput("group_table")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  # 1) Authentication
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  # 2) Y-axis label mapping
  y_axis_labels <- list(
    "Regionale udgifter til medicintilskud, kr."               = "Samlede tilskudsudgifter, kr.",
    "Regionale udgifter til medicintilskud, kr. pr. indbygger" = "Tilskudsudgifter per 100.000 indbyggere, kr.",
    "Mængdesalg"                                              = "Samlet forbrug",
    "Mængdesalgsenhed pr. indbygger"                          = "Forbrug pr. 100.000 indbyggere"
  )
  
  # 3) Reactive filtered data by ATC codes
  filtered_data <- reactive({
    codes <- trimws(unlist(strsplit(input$atc_input, "\n")))
    req(length(codes) > 0)
    merged_data %>%
      filter(`ATC kode` %in% codes)
  })
  
  # 4) Title text from ATC descriptions
  atc_text <- reactive({
    df <- filtered_data()
    if (nrow(df)) paste(unique(df$`ATC tekst`), collapse = ", ")
    else           "Medicinforbrug"
  })
  
  # 5) Product selection UI + observers (unchanged)  
  output$product_selection <- renderUI({
    df <- filtered_data(); req(nrow(df) > 0)
    prods <- unique(df$Produktnavn)
    tagList(
      actionLink("select_all_products",   "Markér alle"), " | ",
      actionLink("deselect_all_products", "Fjern markering"),
      checkboxGroupInput(
        "selected_products", "Vælg produktnavne:",
        choices = prods, selected = prods
      )
    )
  })
  observeEvent(input$select_all_products, {
    updateCheckboxGroupInput(
      session, "selected_products",
      selected = unique(filtered_data()$Produktnavn)
    )
  })
  observeEvent(input$deselect_all_products, {
    updateCheckboxGroupInput(
      session, "selected_products",
      selected = character(0)
    )
  })
  
  # 6) Region selection UI + observers (unchanged)  
  output$region_selection <- renderUI({
    df <- filtered_data(); req(nrow(df) > 0)
    regs <- unique(df$Område)
    tagList(
      actionLink("select_all_regions",   "Markér alle"), " | ",
      actionLink("deselect_all_regions", "Fjern markering"),
      checkboxGroupInput(
        "selected_regions", "Vælg regioner:",
        choices = regs, selected = regs
      )
    )
  })
  observeEvent(input$select_all_regions, {
    updateCheckboxGroupInput(
      session, "selected_regions",
      selected = unique(filtered_data()$Område)
    )
  })
  observeEvent(input$deselect_all_regions, {
    updateCheckboxGroupInput(
      session, "selected_regions",
      selected = character(0)
    )
  })
  
  # 7) Reactive plot construction without SubstGruppe
  plot_reactive <- reactive({
    df0 <- filtered_data() %>%
      filter(
        Produktnavn %in% input$selected_products,
        Område      %in% input$selected_regions
      )
    req(nrow(df0) > 0)
    
    # Scale per-capita if needed
    y_var <- input$y_axis_option
    if (y_var %in% c(
      "Regionale udgifter til medicintilskud, kr. pr. indbygger",
      "Mængdesalgsenhed pr. indbygger"
    )) {
      df0[[y_var]] <- df0[[y_var]] * 100000
    }
    
    # Upstream aggregation
    df_nogroup <- df0 %>%
      group_by(Dato, Område, Produktnavn) %>%
      summarize(y0 = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop")
    
    strat <- input$stratify_option
    
    # Build plot based on stratification
    if (strat == "None") {
      summed <- df_nogroup %>%
        group_by(Dato) %>%
        summarize(value = sum(y0, na.rm = TRUE), .groups = "drop")
      p <- ggplot(summed, aes(Dato, value)) + geom_line(size = 1)
      
    } else if (strat == "Produktnavn") {
      summed <- df_nogroup %>%
        group_by(Dato, Produktnavn) %>%
        summarize(value = sum(y0, na.rm = TRUE), .groups = "drop")
      p <- ggplot(summed, aes(Dato, value, color = Produktnavn)) + geom_line(size = 1)
      
    } else if (strat == "Område") {
      summed <- df_nogroup %>%
        group_by(Dato, Område) %>%
        summarize(value = sum(y0, na.rm = TRUE), .groups = "drop")
      p <- ggplot(summed, aes(Dato, value, color = Område)) + geom_line(size = 1)
      
    } else {
      stop("Ugyldig stratificeringsvalg.")
    }
    
    # Common styling
    p +
      scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        limits = c(0, NA)
      ) +
      labs(
        title = atc_text(),
        x     = "Dato",
        y     = y_axis_labels[[y_var]],
        color = if (strat != "None") strat else NULL
      ) +
      theme_minimal(base_size = 16)
  })
  
  # 8) Render outputs
  output$line_plot <- renderPlot({ plot_reactive() })
  output$download_pdf <- downloadHandler(
    filename = function() paste0("medicinforbrug_plot_", Sys.Date(), ".pdf"),
    content  = function(file) {
      pdf(file, width = 12, height = 8)
      print(plot_reactive())
      dev.off()
    }
  )
  
  # 9a) Produkt‑oversigt: blank when no group
  output$product_table <- renderTable({
    df <- filtered_data() %>%
      filter(
        Produktnavn %in% input$selected_products,
        Område      %in% input$selected_regions
      )
    req(nrow(df) > 0)
    
    df %>%
      group_by(Produktnavn) %>%
      summarise(
        `Gennemsnitlig pris over perioden, kr.` =
          mean(`Pris, kr. pr. solgt enhed`, na.rm = TRUE),
        `Seneste pris, kr.` =
          `Pris, kr. pr. solgt enhed`[which.max(Dato)],
        `Substitutionsgrupper` = {
          idx <- !is.na(SubstGruppe)
          if (any(idx)) {
            paste(
              unique(
                paste0(
                  SubstGruppe[idx], " (",
                  LægemiddelForm[idx], ", ",
                  Styrke[idx], ")"
                )
              ),
              collapse = ", "
            )
          } else {
            ""  # blank when no substgroup
          }
        },
        .groups = "drop"
      )
  })
  
  # 9b) Substitutionsgruppe‑oversigt: rename first column
  output$group_table <- renderTable({
    df <- filtered_data() %>%
      filter(
        Produktnavn %in% input$selected_products,
        Område      %in% input$selected_regions
      )
    req(nrow(df) > 0)
    
    df %>%
      distinct(SubstGruppe, Produktnavn, LægemiddelForm, Styrke) %>%
      group_by(Substitutionsgruppe = SubstGruppe) %>%
      summarise(
        `Produktnavne der tilhører gruppen` = paste(unique(Produktnavn), collapse = ", "),
        `Lægemiddelform`                   = paste(unique(LægemiddelForm), collapse = ", "),
        `Styrke`                           = paste(unique(Styrke), collapse = ", "),
        .groups = "drop"
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
