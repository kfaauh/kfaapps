# app.R — Shiny-app for lmgrupper (uses precomputed clean_data.rds)

library(shiny)
library(shinyTree)
library(dplyr)
library(DT)
library(shinymanager)

credentials <- data.frame(
  user = "KFA",
  password = "kfekfa123",
  stringsAsFactors = FALSE
)

clean_data <- readRDS("data/clean_data.rds")

valid_categories <- unique(clean_data$Kategori)
valid_categories <- valid_categories[!is.na(valid_categories) & valid_categories != "unknown"]

# Build the tree structure for the sidebar
tree_data <- list()

for (cat in valid_categories) {
  cat_rows <- clean_data[clean_data$Kategori == cat, ]
  
  subcats <- unique(cat_rows$Underkategori)
  subcats <- subcats[!is.na(subcats) & subcats != "unknown"]
  
  if (length(subcats) == 0) {
    tree_data[[cat]] <- cat
  } else {
    subcat_list <- list()
    
    for (subcat in subcats) {
      subcat_rows <- cat_rows[cat_rows$Underkategori == subcat, ]
      
      atc_values <- unique(subcat_rows$ATCtekst)
      atc_values <- atc_values[!is.na(atc_values) & atc_values != "unknown"]
      
      if (length(atc_values) == 0) {
        subcat_list[[subcat]] <- subcat
      } else {
        atc_list <- as.list(atc_values)
        names(atc_list) <- atc_values
        subcat_list[[subcat]] <- atc_list
      }
    }
    
    tree_data[[cat]] <- subcat_list
  }
}

ui <- secure_app(
  fluidPage(
    titlePanel("Prisoversigter for lægemiddelgrupper"),
    
    tabsetPanel(
      tabPanel(
        "Lægemiddelgrupper",
        
        sidebarLayout(
          sidebarPanel(
            shinyTree(
              "tree",
              checkbox = FALSE,
              search = TRUE,
              multiple = FALSE
            ),
            
            checkboxInput(
              "advanced_view",
              "Avanceret visning",
              value = FALSE
            ),
            
            tags$div(
              tags$p("Sådan bruger du appen:"),
              tags$ol(
                tags$li("Vælg en kategori, underkategori eller et generisk lægemiddel til venstre."),
                tags$li("Tabellen opdateres automatisk med de relevante lægemidler."),
                tags$li("Brug filtrene over hver kolonne for at indsnævre søgningen."),
                tags$li("Sortér efter en kolonne ved at klikke på overskriften.")
              ),
              tags$p("Priserne er hentet fra 14-dages medicinpriser fra esundhed.dk, der publiceres hver 14. dag."),
              tags$p("WHO defineret døgndosis (DDD) er defineret som den formodede gennemsnitlige vedligeholdelsesdosis pr. døgn for et lægemiddel anvendt til dets primære indikation. Formålet er at kunne sammenligne lægemidler på tværs af forskellige doseringer. DDD er ikke defineret for alle præparater, fx kan det ikke defineres for kombinationspræparater."),
              tags$p("Substitutionsgruppen er en gruppe af synonyme lægemidler. Apoteket har pligt til at udlevere det billigste lægemiddel inden for en given substitutionsgruppe med mindre recepten specificerer andet."),
              tags$p("I avanceret visning vises 14-dages prisændring som procentvis ændring i AUP siden forrige prisperiode. Prisvariation er standardafvigelsen i AUP over de seneste 12 måneder.")
            )
          ),
          
          mainPanel(
            h3(textOutput("tableTitle")),
            DTOutput("tableOutput")
          )
        )
      ),
      
      tabPanel(
        "Prisadvarsler",
        fluidRow(
          column(
            width = 12,
            h3("Største prishop de seneste 14 dage"),
            p("Tabellen viser alle lægemidler med mindst 20 % prisstigning siden forrige prisperiode, sorteret efter størst procentvis prisstigning."),
            p("Listen er beregnet på hele clean_data og er derfor ikke begrænset til de lægemidler, der er valgt eller defineret i træstrukturen."),
            DTOutput("priceAlertsTable")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$tree <- renderTree({
    tree_data
  })
  
  selectedValue <- reactive({
    req(input$tree)
    
    sel <- tryCatch(
      unlist(shinyTree::get_selected(input$tree, format = "names")),
      error = function(e) NULL
    )
    
    if (length(sel) == 0) return(NULL)
    
    sel <- sel[1]
    
    if (sel %in% valid_categories) {
      list(type = "kategori", value = sel)
    } else if (sel %in% unique(clean_data$Underkategori)) {
      list(type = "underkategori", value = sel)
    } else if (sel %in% unique(clean_data$ATCtekst)) {
      list(type = "atctekst", value = sel)
    } else {
      list(type = "other", value = sel)
    }
  })
  
  categoryData <- reactive({
    sel <- selectedValue()
    
    if (is.null(sel)) return(clean_data[FALSE, ])
    
    switch(
      sel$type,
      kategori = filter(clean_data, Kategori == sel$value),
      underkategori = filter(clean_data, Underkategori == sel$value),
      atctekst = filter(clean_data, ATCtekst == sel$value),
      data.frame()
    )
  })
  
  filteredData <- reactive({
    df <- categoryData() %>%
      mutate(
        Styrke = as.character(Styrke),
        AUP = suppressWarnings(as.numeric(AUP)),
        AUP_pr_DDD = suppressWarnings(as.numeric(AUP_pr_DDD)),
        DDD = suppressWarnings(as.numeric(DDD)),
        `14-dages prisændring` = suppressWarnings(as.numeric(`14-dages prisændring`)),
        Prisvariation = suppressWarnings(as.numeric(Prisvariation))
      )
    
    simple_df <- df %>%
      select(
        `Indholdsstof` = Indholdsstof,
        `Styrke` = Styrke,
        `Form` = Form,
        `Pakning` = Pakning,
        `Handelsnavn` = Lægemiddel,
        `Pris` = AUP,
        `Pris per DDD` = AUP_pr_DDD
      )
    
    if (!isTRUE(input$advanced_view)) {
      return(simple_df)
    }
    
    advanced_df <- df %>%
      select(
        `DDD` = DDD,
        `Substitutionsgruppe` = SubstGruppe,
        `14-dages prisændring` = `14-dages prisændring`,
        `Prisvariation` = Prisvariation
      )
    
    bind_cols(simple_df, advanced_df)
  })
  
  
  priceAlertsData <- reactive({
    clean_data %>%
      mutate(
        Styrke = as.character(Styrke),
        AUP = suppressWarnings(as.numeric(AUP)),
        AUP_pr_DDD = suppressWarnings(as.numeric(AUP_pr_DDD)),
        DDD = suppressWarnings(as.numeric(DDD)),
        `14-dages prisændring` = suppressWarnings(as.numeric(`14-dages prisændring`)),
        Prisvariation = suppressWarnings(as.numeric(Prisvariation))
      ) %>%
      filter(
        !is.na(`14-dages prisændring`),
        `14-dages prisændring` >= 20
      ) %>%
      arrange(desc(`14-dages prisændring`)) %>%
      select(
        `Indholdsstof` = Indholdsstof,
        `Styrke` = Styrke,
        `Form` = Form,
        `Pakning` = Pakning,
        `Handelsnavn` = Lægemiddel,
        `Pris` = AUP,
        `Pris per DDD` = AUP_pr_DDD,
        `DDD` = DDD,
        `Substitutionsgruppe` = SubstGruppe,
        `14-dages prisændring` = `14-dages prisændring`,
        `Prisvariation` = Prisvariation
      )
  })
  
  output$tableTitle <- renderText({
    sel <- selectedValue()
    
    if (is.null(sel)) {
      "Vælg kategori eller underkategori"
    } else {
      sel$value
    }
  })
  
  output$tableOutput <- renderDT({
    df <- filteredData()
    
    if (nrow(df) == 0) {
      return(datatable(df, rownames = FALSE))
    }
    
    filter_columns <- intersect(
      c("Indholdsstof", "Styrke", "Form", "Pakning", "Handelsnavn", "Substitutionsgruppe"),
      names(df)
    )
    
    df[filter_columns] <- lapply(df[filter_columns], as.factor)
    
    dt <- datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        order = list(list(0, "asc")),
        orderClasses = TRUE,
        lengthMenu = list(c(-1, 10, 50, 100), c("Alle", "10", "50", "100")),
        language = list(
          lengthMenu = "Vis _MENU_ rækker",
          zeroRecords = "Ingen rækker fundet",
          info = "Viser _START_ til _END_ af _TOTAL_ rækker",
          infoEmpty = "Ingen rækker fundet",
          infoFiltered = "(filtreret fra _MAX_ rækker)",
          search = "Søg",
          paginate = list(
            first = "Første",
            last = "Sidste",
            `next` = "Næste",
            previous = "Forrige"
          )
        )
      )
    )
    
    numeric_cols <- intersect(
      c("Pris", "Pris per DDD", "DDD", "14-dages prisændring", "Prisvariation"),
      names(df)
    )
    
    if (length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, 2)
    }
    
    if ("Pris per DDD" %in% names(df)) {
      valid_vals <- df[["Pris per DDD"]][is.finite(df[["Pris per DDD"]])]
      
      if (length(valid_vals) > 0) {
        brks <- quantile(valid_vals, probs = seq(0, 1, 0.01), na.rm = TRUE)
        base_clrs <- colorRampPalette(c("green", "yellow", "red"))(length(brks) + 1)
        clrs <- sapply(base_clrs, function(x) adjustcolor(x, alpha.f = 0.5))
        
        dt <- dt %>%
          formatStyle(
            columns = c("Pris per DDD"),
            backgroundColor = styleInterval(brks, clrs)
          )
      }
    }
    
    dt
  })
  
  output$priceAlertsTable <- renderDT({
    df <- priceAlertsData()
    
    if (nrow(df) == 0) {
      return(datatable(df, rownames = FALSE))
    }
    
    filter_columns <- intersect(
      c("Indholdsstof", "Styrke", "Form", "Pakning", "Handelsnavn", "Substitutionsgruppe"),
      names(df)
    )
    
    df[filter_columns] <- lapply(df[filter_columns], as.factor)
    
    dt <- datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        order = list(list(which(names(df) == "14-dages prisændring") - 1, "desc")),
        orderClasses = TRUE,
        pageLength = 50,
        lengthMenu = list(c(25, 50, 100, -1), c("25", "50", "100", "Alle")),
        language = list(
          lengthMenu = "Vis _MENU_ rækker",
          zeroRecords = "Ingen rækker fundet",
          info = "Viser _START_ til _END_ af _TOTAL_ rækker",
          infoEmpty = "Ingen rækker fundet",
          infoFiltered = "(filtreret fra _MAX_ rækker)",
          search = "Søg",
          paginate = list(
            first = "Første",
            last = "Sidste",
            `next` = "Næste",
            previous = "Forrige"
          )
        )
      )
    )
    
    numeric_cols <- intersect(
      c("Pris", "Pris per DDD", "DDD", "14-dages prisændring", "Prisvariation"),
      names(df)
    )
    
    if (length(numeric_cols) > 0) {
      dt <- dt %>% formatRound(numeric_cols, 2)
    }
    
    dt
  })
}

shinyApp(ui = ui, server = server)
