# app.R — Shiny-app for lmgrupper (uses precomputed clean_data.rds)

# Load required libraries
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

# Load preprocessed data
# Adjust this path if your RDS is in a different location
clean_data  <- readRDS("data/clean_data.rds")

# Filter out invalid categories/subcategories.
valid_categories <- unique(clean_data$Kategori)
valid_categories <- valid_categories[!is.na(valid_categories) & valid_categories != "unknown"]

# Build the tree structure for the sidebar.
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
      atc_values  <- unique(subcat_rows$ATCtekst)
      atc_values  <- atc_values[!is.na(atc_values) & atc_values != "unknown"]
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
    sidebarLayout(
      sidebarPanel(
        shinyTree("tree", checkbox = FALSE, search = TRUE, multiple = FALSE),    
        tags$div(
          tags$p("Sådan bruger du appen:"),
          tags$ol(
            tags$li("Vælg en kategori, underkategori eller et generisk lægemiddel til venstre."),
            tags$li("Tabellen opdateres automatisk med de relevante lægemidler."),
            tags$li("Brug filtrene over hver kolonne for at indsnævre søgningen."),
            tags$li("Sortér efter en kolonne ved at klikke på overskriften.")
          ),
          tags$p("Priserne er hentet fra 14-dages medicinpriser fra esundhed.dk, der publiceres hver 14. dag.")
        )
      ),
      mainPanel(
        h3(textOutput("tableTitle")),
        DTOutput("tableOutput")
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
    switch(sel$type,
           kategori = filter(clean_data, Kategori == sel$value),
           underkategori = filter(clean_data, Underkategori == sel$value),
           atctekst = filter(clean_data, ATCtekst == sel$value),
           data.frame()
    )
  })

  filteredData <- reactive({
    categoryData() %>% 
      select(
        `Indholdsstof`       = Indholdsstof,
        `Styrke`             = Styrke,
        `Form`               = Form,
        `Pakning`            = Pakning,
        `Handelsnavn`        = Lægemiddel,
        `Pris`               = AUP,
        `Pris pr. DDD`       = AUP_pr_DDD,
        `Substitutionsgruppe` = SubstGruppe
      ) %>% 
      mutate(Styrke = as.character(Styrke))
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
    if (nrow(df) == 0) return(datatable(df))

    filter_columns <- c("Indholdsstof", "Styrke", "Form", "Pakning", "Handelsnavn")
    df[filter_columns] <- lapply(df[filter_columns], as.factor)

    col_vals <- df[["Pris pr. DDD"]]
    brks <- quantile(col_vals, probs = seq(0, 1, 0.01), na.rm = TRUE)
    base_clrs <- colorRampPalette(c("green","yellow","red"))(length(brks) + 1)
    clrs <- sapply(base_clrs, function(x) adjustcolor(x, alpha.f = 0.5))

    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        order = list(list(0, "asc")),
        orderClasses = TRUE,
        lengthMenu = list(c(-1,10,50,100), c("Alle","10","50","100")),
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
            'next' = "Næste",
            'previous' = "Forrige"
          )
        )
      )
    ) %>%
      formatStyle(
        columns = c("Pris pr. DDD"),
        backgroundColor = styleInterval(brks, clrs)
      ) %>%
      formatRound("Pris pr. DDD", 2)
  })
}

shinyApp(ui = ui, server = server)
