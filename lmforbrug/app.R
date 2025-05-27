# app_fixed_local.R
# Shiny app for pre-processed data with robust stratification and explicit select/deselect controls

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinymanager)

credentials <- data.frame(
  user = "KFA",
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
        uiOutput("subst_group_selection"),
        uiOutput("region_selection"),
        radioButtons("stratify_option", "Stratificering af data:",
          choices = list(
            "Ingen" = "None",
            "Produktnavn" = "Produktnavn",
            "Substitutionsgruppe" = "SubstGruppe",
            "Region" = "Område"
          ), selected = "None"),
        radioButtons("y_axis_option", "Y-akse:",
          choices = list(
            "Samlede tilskudsudgifter, kr." = "Regionale udgifter til medicintilskud, kr.",
            "Tilskudsudgifter per 100.000 indbyggere, kr." = "Regionale udgifter til medicintilskud, kr. pr. indbygger",
            "Samlet forbrug" = "Mængdesalg",
            "Forbrug pr. 100.000 indbyggere" = "Mængdesalgsenhed pr. indbygger"
          ), selected = "Regionale udgifter til medicintilskud, kr."),
        downloadButton("download_pdf", "Download figur som PDF")
      ),
      mainPanel(
        plotOutput("line_plot"),
        tableOutput("summary_table")
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  # Y-axis label mapping
  y_axis_labels <- list(
    "Regionale udgifter til medicintilskud, kr."              = "Samlede tilskudsudgifter, kr.",
    "Regionale udgifter til medicintilskud, kr. pr. indbygger" = "Tilskudsudgifter per 100.000 indbyggere, kr.",
    "Mængdesalg"                                              = "Samlet forbrug",
    "Mængdesalgsenhed pr. indbygger"                          = "Forbrug pr. 100.000 indbyggere"
  )

  # Reactive filtered data (no explosive separate_rows here)
  filtered_data <- reactive({
    codes <- trimws(unlist(strsplit(input$atc_input, '\n')))
    req(length(codes) > 0)
    merged_data %>%
      filter(`ATC kode` %in% codes)
  })

  # Reactive ATC text for title
  atc_text <- reactive({
    df <- filtered_data()
    if (nrow(df)) paste(unique(df$`ATC tekst`), collapse = ", ") else "Medicinforbrug"
  })

  # Dynamic UIs with explicit select/deselect links
  output$product_selection <- renderUI({
    df <- filtered_data(); req(nrow(df) > 0)
    prods <- unique(df$Produktnavn)
    tagList(
      actionLink("select_all_products", "Markér alle"), " | ",
      actionLink("deselect_all_products", "Fjern markering"),
      checkboxGroupInput("selected_products", "Vælg produktnavne:", choices = prods, selected = prods)
    )
  })

  output$subst_group_selection <- renderUI({
    df <- filtered_data(); req(nrow(df) > 0)
    groups <- unique(unlist(strsplit(paste(df$SubstGruppe, collapse = ", "), ", ")))
    tagList(
      actionLink("select_all_subst", "Markér alle"), " | ",
      actionLink("deselect_all_subst", "Fjern markering"),
      checkboxGroupInput("selected_subst_groups", "Vælg substitutionsgrupper:", choices = groups, selected = groups)
    )
  })

  output$region_selection <- renderUI({
    df <- filtered_data(); req(nrow(df) > 0)
    regs <- unique(df$Område)
    tagList(
      actionLink("select_all_regions", "Markér alle"), " | ",
      actionLink("deselect_all_regions", "Fjern markering"),
      checkboxGroupInput("selected_regions", "Vælg regioner:", choices = regs, selected = regs)
    )
  })

  # Select/deselect observers
  observeEvent(input$select_all_products,   updateCheckboxGroupInput(session, "selected_products",   selected = unique(filtered_data()$Produktnavn)))
  observeEvent(input$deselect_all_products, updateCheckboxGroupInput(session, "selected_products",   selected = character(0)))
  observeEvent(input$select_all_subst,      updateCheckboxGroupInput(session, "selected_subst_groups", selected = unique(unlist(strsplit(paste(filtered_data()$SubstGruppe, collapse = ", "), ", ")))))
  observeEvent(input$deselect_all_subst,    updateCheckboxGroupInput(session, "selected_subst_groups", selected = character(0)))
  observeEvent(input$select_all_regions,    updateCheckboxGroupInput(session, "selected_regions",   selected = unique(filtered_data()$Område)))
  observeEvent(input$deselect_all_regions,  updateCheckboxGroupInput(session, "selected_regions",   selected = character(0)))

   # Reactive plot with conditional explode for stratification (Option A)
  plot_reactive <- reactive({
    # Base filtering by product and region
    df_base <- filtered_data() %>%
      filter(
        Produktnavn %in% input$selected_products,
        Område      %in% input$selected_regions
      )
    req(nrow(df_base) > 0)

    # Scale per-100k if selected
    y_var <- input$y_axis_option
    if (y_var %in% c(
      "Regionale udgifter til medicintilskud, kr. pr. indbygger",
      "Mængdesalgsenhed pr. indbygger"
    )) {
      df_base[[y_var]] <- df_base[[y_var]] * 100000
    }

    strat <- input$stratify_option

    if (strat == "None") {
  # 1) filter out any products whose groups are fully de-selected
  df0 <- df_base %>%
    filter(
      sapply(strsplit(SubstGruppe, ", "), function(x)
        any(x %in% input$selected_subst_groups)
      )
    )
  
  # 2) de-duplicate per date + product (+ region, if you care),
  #    then sum across products
  summed <- df0 %>%
    group_by(Dato, Produktnavn, Område) %>%             #  <-- remove duplicates here
    summarize(y = first(.data[[y_var]]), .groups = "drop") %>%
    group_by(Dato) %>%
    summarize(value = sum(y, na.rm = TRUE), .groups = "drop")
  
  p <- ggplot(summed, aes(x = Dato, y = value)) +
    geom_line(size = 1)

    } else {
      # Explode into separate substitution-groups
      df1 <- df_base %>%
        separate_rows(SubstGruppe, sep = ", ") %>%
        filter(SubstGruppe %in% input$selected_subst_groups)

      # Deduplicate and sum per group
      df_unique <- df1 %>%
        group_by(Dato, Produktnavn, SubstGruppe, Område) %>%
        summarize(y = first(.data[[y_var]]), .groups = "drop")

      summed <- df_unique %>%
        group_by(Dato, .data[[strat]]) %>%
        summarize(value = sum(y, na.rm = TRUE), .groups = "drop")

      p <- ggplot(summed, aes(x = Dato, y = value, color = .data[[strat]])) +
        geom_line(size = 1)
    }

    # Common plot styling
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

  output$line_plot <- renderPlot({ plot_reactive() })

  output$download_pdf <- downloadHandler(
    filename = function() paste0("medicinforbrug_plot_", Sys.Date(), ".pdf"),
    content  = function(file) {
      pdf(file, width = 12, height = 8)
      print(plot_reactive())
      dev.off()
    }
  )

  output$summary_table <- renderTable({
    df <- filtered_data(); req(nrow(df) > 0)
    df %>%
      group_by(SubstGruppe, Styrke) %>%
      summarise(
        Lægemiddelform               = first(LægemiddelForm),
        Produktnavne                 = paste(unique(Produktnavn), collapse = ", "),
        `Gennemsnitlig pris pr. DDD` = mean(`Pris, kr. pr. solgt enhed`, na.rm = TRUE),
        .groups                     = "drop"
      )
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
