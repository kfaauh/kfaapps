# -------------------------
# Updated app_fixed_local.R
# -------------------------

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shinymanager)

# Credentials (example)
credentials <- data.frame(
  user     = "KFA",
  password = "kfekfa123",
  stringsAsFactors = FALSE
)

# Load cleaned data
merged_data <- readRDS('data/merged_data.rds')

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
                       "Ingen"                = "None",
                       "Produktnavn"          = "Produktnavn",
                       "Substitutionsgruppe"  = "SubstGruppe",
                       "Region"               = "Område"
                     ), selected = "None"),
        radioButtons("y_axis_option", "Y-akse:",
                     choices = list(
                       "Samlede tilskudsudgifter, kr."                      = "Regionale udgifter til medicintilskud, kr.",
                       "Tilskudsudgifter per 100.000 indbyggere, kr."       = "Regionale udgifter til medicintilskud, kr. pr. indbygger",
                       "Samlet forbrug"                                      = "Mængdesalg",
                       "Forbrug pr. 100.000 indbyggere"                      = "Mængdesalgsenhed pr. indbygger"
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

server <- function(input, output, session) {
  # Authentication
  res_auth <- secure_server(check_credentials = check_credentials(credentials))

  # Labels for Y axis
  y_axis_labels <- list(
    "Regionale udgifter til medicintilskud, kr."               = "Samlede tilskudsudgifter, kr.",
    "Regionale udgifter til medicintilskud, kr. pr. indbygger"= "Tilskudsudgifter per 100.000 indbyggere, kr.",
    "Mængdesalg"                                             = "Samlet forbrug",
    "Mængdesalgsenhed pr. indbygger"                         = "Forbrug pr. 100.000 indbyggere"
  )

  # Reactive filtered & exploded data
  filtered_data <- reactive({
    codes <- trimws(unlist(strsplit(input$atc_input, '\\n')))
    req(length(codes) > 0)

    merged_data %>%
      filter(`ATC kode` %in% codes,
             Produktnavn %in% input$selected_products,
             Område      %in% input$selected_regions) %>%
      # explode substitution groups to filter correctly
      separate_rows(SubstGruppe, sep = ", ") %>%
      filter(SubstGruppe %in% input$selected_subst_groups)
  })

  # Dynamic UI selectors
  output$product_selection <- renderUI({
    df <- filtered_data(); req(nrow(df) > 0)
    prods <- unique(df$Produktnavn)
    tagList(
      actionLink("select_all_products", "Markér alle"), " | ",
      actionLink("deselect_all_products", "Fjern markering"),
      checkboxGroupInput("selected_products", "Vælg produktnavne:",
                         choices = prods, selected = prods)
    )
  })
  output$subst_group_selection <- renderUI({
    df <- filtered_data(); req(nrow(df) > 0)
    groups <- unique(df$SubstGruppe)
    tagList(
      actionLink("select_all_subst", "Markér alle"), " | ",
      actionLink("deselect_all_subst", "Fjern markering"),
      checkboxGroupInput("selected_subst_groups", "Vælg substitutionsgrupper:",
                         choices = groups, selected = groups)
    )
  })
  output$region_selection <- renderUI({
    df <- filtered_data(); req(nrow(df) > 0)
    regs <- unique(df$Område)
    tagList(
      actionLink("select_all_regions", "Markér alle"), " | ",
      actionLink("deselect_all_regions", "Fjern markering"),
      checkboxGroupInput("selected_regions", "Vælg regioner:",
                         choices = regs, selected = regs)
    )
  })

  # Selection observers
  observeEvent(input$select_all_products,   updateCheckboxGroupInput(session, "selected_products",   selected = unique(filtered_data()$Produktnavn)))
  observeEvent(input$deselect_all_products, updateCheckboxGroupInput(session, "selected_products",   selected = character(0)))
  observeEvent(input$select_all_subst,      updateCheckboxGroupInput(session, "selected_subst_groups", selected = unique(filtered_data()$SubstGruppe)))
  observeEvent(input$deselect_all_subst,    updateCheckboxGroupInput(session, "selected_subst_groups", selected = character(0)))
  observeEvent(input$select_all_regions,    updateCheckboxGroupInput(session, "selected_regions",   selected = unique(filtered_data()$Område)))
  observeEvent(input$deselect_all_regions,  updateCheckboxGroupInput(session, "selected_regions",   selected = character(0)))

  # Reactive plot
  plot_reactive <- reactive({
    df <- filtered_data()
    req(nrow(df) > 0)
    y_var <- input$y_axis_option

    # scale per 100k
    if (y_var %in% c(
      "Regionale udgifter til medicintilskud, kr. pr. indbygger",
      "Mængdesalgsenhed pr. indbygger"
    )) {
      df[[y_var]] <- df[[y_var]] * 100000
    }

    strat <- input$stratify_option

    if (strat == "None") {
      # Aggregate across all products, groups, and regions
      summed <- df %>%
        distinct(Dato, Produktnavn, Område, SubstGruppe, .keep_all = TRUE) %>%
        group_by(Dato) %>%
        summarize(value = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(summed, aes(x = Dato, y = value)) + geom_line(size = 1)
    } else {
      # Aggregate by chosen stratification
      grouped <- df %>%
        distinct(Dato, Produktnavn, Område, SubstGruppe, .keep_all = TRUE) %>%
        group_by(Dato, group = .data[[strat]]) %>%
        summarize(value = sum(.data[[y_var]], na.rm = TRUE), .groups = "drop")
      p <- ggplot(grouped, aes(x = Dato, y = value, color = group)) + geom_line(size = 1)
    }

    p +
      scale_y_continuous(
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        limits = c(0, NA)
      ) +
      labs(
        title = paste(unique(df$`ATC tekst`), collapse = ", "),
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

  # Summary table unchanged
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

shinyApp(ui = ui, server = server)
