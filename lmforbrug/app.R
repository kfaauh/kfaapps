# app.R
# This Shiny app reads pre-processed data and provides interactive visualization with fixed toggles and stratification handling.

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load cleaned data
merged_data <- readRDS('data/merged_data.rds')

# Define UI
ui <- fluidPage(
  titlePanel('Medicinforbrug fra eSundhed.dk'),
  sidebarLayout(
    sidebarPanel(
      textAreaInput('atc_input', 'Indtast ATC-kode:', rows = 1),
      uiOutput('product_selection'),
      uiOutput('subst_group_selection'),
      uiOutput('region_selection'),
      radioButtons('stratify_option', 'Stratificering af data:',
                   choices = list(
                     'Ingen'             = 'None',
                     'Produktnavn'       = 'Produktnavn',
                     'Substitutionsgruppe' = 'SubstGruppe',
                     'Region'            = 'Område'
                   ), selected = 'None'),
      radioButtons('y_axis_option', 'Y-akse:',
                   choices = list(
                     'Samlede tilskudsudgifter, kr.'           = 'Regionale udgifter til medicintilskud, kr.',
                     'Tilskudsudgifter per 100.000 indbyggere, kr.' = 'Regionale udgifter til medicintilskud, kr. pr. indbygger',
                     'Samlet forbrug'                           = 'Mængdesalg',
                     'Forbrug pr. 100.000 indbyggere'           = 'Mængdesalgsenhed pr. indbygger'
                   ), selected = 'Regionale udgifter til medicintilskud, kr.'),
      downloadButton('download_pdf', 'Download figur som PDF')
    ),
    mainPanel(
      plotOutput('line_plot'),
      tableOutput('summary_table')
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Mapping for y-axis labels
  y_axis_labels <- list(
    'Regionale udgifter til medicintilskud, kr.'              = 'Samlede tilskudsudgifter, kr.',
    'Regionale udgifter til medicintilskud, kr. pr. indbygger' = 'Tilskudsudgifter per 100.000 indbyggere, kr.',
    'Mængdesalg'                                              = 'Samlet forbrug',
    'Mængdesalgsenhed pr. indbygger'                          = 'Forbrug pr. 100.000 indbyggere'
  )

  # Reactive filtered data based on ATC input
  filtered_data <- reactive({
    codes <- unlist(strsplit(input$atc_input, '\n'))
    codes <- trimws(codes)
    req(length(codes) > 0)
    merged_data %>%
      filter(`ATC kode` %in% codes) %>%
      separate_rows(SubstGruppe, sep = ', ')
  })

  # Dynamic UI with explicit select/deselect buttons to avoid flicker
  output$product_selection <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 0)
    prods <- unique(df$Produktnavn)
    tagList(
      actionLink('select_all_products', 'Markér alle'), ' | ',
      actionLink('deselect_all_products', 'Fjern markering'),
      checkboxGroupInput('selected_products', 'Vælg produktnavne:', choices = prods, selected = prods)
    )
  })

  output$subst_group_selection <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 0)
    groups <- unique(unlist(strsplit(paste(df$SubstGruppe, collapse = ', '), ', ')))
    tagList(
      actionLink('select_all_subst', 'Markér alle'), ' | ',
      actionLink('deselect_all_subst', 'Fjern markering'),
      checkboxGroupInput('selected_subst_groups', 'Vælg substitutionsgrupper:', choices = groups, selected = groups)
    )
  })

  output$region_selection <- renderUI({
    df <- filtered_data()
    req(nrow(df) > 0)
    regs <- unique(df$Område)
    tagList(
      actionLink('select_all_regions', 'Markér alle'), ' | ',
      actionLink('deselect_all_regions', 'Fjern markering'),
      checkboxGroupInput('selected_regions', 'Vælg regioner:', choices = regs, selected = regs)
    )
  })

  # Observers for select/deselect all
  observeEvent(input$select_all_products, {
    updateCheckboxGroupInput(session, 'selected_products', selected = unique(filtered_data()$Produktnavn))
  })
  observeEvent(input$deselect_all_products, {
    updateCheckboxGroupInput(session, 'selected_products', selected = character(0))
  })

  observeEvent(input$select_all_subst, {
    grps <- unique(unlist(strsplit(paste(filtered_data()$SubstGruppe, collapse = ', '), ', ')))
    updateCheckboxGroupInput(session, 'selected_subst_groups', selected = grps)
  })
  observeEvent(input$deselect_all_subst, {
    updateCheckboxGroupInput(session, 'selected_subst_groups', selected = character(0))
  })

  observeEvent(input$select_all_regions, {
    updateCheckboxGroupInput(session, 'selected_regions', selected = unique(filtered_data()$Område))
  })
  observeEvent(input$deselect_all_regions, {
    updateCheckboxGroupInput(session, 'selected_regions', selected = character(0))
  })

  # Reactive plot logic with proper handling when no stratification
  plot_reactive <- reactive({
    df <- filtered_data() %>%
      filter(Produktnavn %in% input$selected_products,
             SubstGruppe %in% input$selected_subst_groups,
             Område %in% input$selected_regions)
    req(nrow(df) > 0)

    y_var <- input$y_axis_option
    if (y_var %in% c('Regionale udgifter til medicintilskud, kr. pr. indbygger',
                     'Mængdesalgsenhed pr. indbygger')) {
      df[[y_var]] <- df[[y_var]] * 100000
    }

    strat <- input$stratify_option
    if (strat == 'None') {
      summed <- df %>%
        group_by(Dato) %>%
        summarise(value = sum(.data[[y_var]], na.rm = TRUE), .groups = 'drop')
      p <- ggplot(summed, aes(x = Dato, y = value)) + geom_line(size = 1)
    } else {
      summed <- df %>%
        group_by(Dato, .data[[strat]]) %>%
        summarise(value = sum(.data[[y_var]], na.rm = TRUE), .groups = 'drop')
      p <- ggplot(summed, aes(x = Dato, y = value, color = .data[[strat]])) + geom_line(size = 1)
    }

    p +
      scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale()), limits = c(0, NA)) +
      labs(
        title = ifelse(length(unique(df$`ATC tekst`)) > 0,
                       paste(unique(df$`ATC tekst`), collapse = ', '),
                       'Medicinforbrug'),
        x = 'Dato',
        y = y_axis_labels[[y_var]]
      ) +
      theme_minimal(base_size = 16)
  })

  output$line_plot <- renderPlot({ plot_reactive() })

  output$download_pdf <- downloadHandler(
    filename = function() paste0('medicinforbrug_plot_', Sys.Date(), '.pdf'),
    content = function(file) {
      pdf(file, width = 12, height = 8)
      print(plot_reactive())
      dev.off()
    }
  )

  output$summary_table <- renderTable({
    df <- filtered_data()
    req(nrow(df) > 0)
    df %>%
      group_by(SubstGruppe, Styrke) %>%
      summarise(
        Lægemiddelform             = first(LægemiddelForm),
        Produktnavne               = paste(unique(Produktnavn), collapse = ', '),
        `Gennemsnitlig pris pr. DDD` = mean(`Pris, kr. pr. solgt enhed`, na.rm = TRUE),
        .groups                     = 'drop'
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
