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
  # All rows for the current kategori
  cat_rows <- clean_data[clean_data$Kategori == cat, ]
  
  # Find unique underkategorier
  subcats <- unique(cat_rows$Underkategori)
  subcats <- subcats[!is.na(subcats) & subcats != "unknown"]
  
  # If no underkategorier for this kategori, treat kategori as a leaf
  if (length(subcats) == 0) {
    tree_data[[cat]] <- cat
  } else {
    # Build a named list of subcategories (which themselves may have children)
    subcat_list <- list()
    
    for (subcat in subcats) {
      subcat_rows <- cat_rows[cat_rows$Underkategori == subcat, ]
      atc_values  <- unique(subcat_rows$ATCtekst)
      atc_values  <- atc_values[!is.na(atc_values) & atc_values != "unknown"]
      
      if (length(atc_values) == 0) {
        # No ATCtekst here, so subcat is a leaf
        subcat_list[[subcat]] <- subcat
      } else {
        # Build a named list of ATCtekst as children
        atc_list <- as.list(atc_values)
        names(atc_list) <- atc_values
        subcat_list[[subcat]] <- atc_list
      }
    }
    
    # Assign the subcategory list to the kategori
    tree_data[[cat]] <- subcat_list
  }
}

ui <- secure_app( 
fluidPage(
  titlePanel("Prisoversigter for lægemiddelgrupper"),
  sidebarLayout(
    sidebarPanel(
      shinyTree("tree", checkbox = FALSE, search = TRUE, multiple = FALSE),
      hr(),
      helpText("userguide")
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

  # You may want to control the guide text from the server (optional)
  output$userGuide <- renderText({
    "Sådan bruger du appen: 
    1. Vælg en kategori eller underkategori til venstre.
    2. Tabellen opdateres automatisk med de relevante lægemidler.
    3. Brug filtrene over hver kolonne for at indsnævre din søgning.
    4. Hold musen over kolonneoverskrifterne for at se en forklaring."
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
      return(list(type = "kategori", value = sel))
    } else if (sel %in% unique(clean_data$Underkategori)) {
      return(list(type = "underkategori", value = sel))
    } else if (sel %in% unique(clean_data$ATCtekst)) {
      return(list(type = "atctekst", value = sel))
    } else {
      return(list(type = "other", value = sel))
    }
  })

  categoryData <- reactive({
    sel <- selectedValue()
    if (is.null(sel)) {
      return(clean_data[FALSE, ])
    }
    if (sel$type == "kategori") {
      clean_data %>% dplyr::filter(Kategori == sel$value)
    } else if (sel$type == "underkategori") {
      clean_data %>% dplyr::filter(Underkategori == sel$value)
    } else if (sel$type == "atctekst") {
      clean_data %>% dplyr::filter(ATCtekst == sel$value)
    } else {
      data.frame()
    }
  })

  filteredData <- reactive({
    # Rearranged order, Substitutionsgruppe is now LAST.
    categoryData() %>%
      select(
        `Indholdsstof`       = Indholdsstof,
        `Styrke`             = Styrke,
        `Form`               = Form,
        `Pakning`            = Pakning,
        `Handelsnavn`        = Lægemiddel,
        `Pris`               = AUP,
        `Pris pr. DDD`       = AUP_pr_DDD,
        #`14-dages prisændring` = Prisændring, # No longer shown in the DT table
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
    if (nrow(df) == 0) {
      return(datatable(df))
    }
    # Convert to factor for dropdown filters
    filter_columns <- c(
      "Indholdsstof",
      "Styrke",
      "Form",
      "Pakning",
      "Handelsnavn",
      "Substitutionsgruppe"
    )
    df[filter_columns] <- lapply(df[filter_columns], as.factor)

    # Prepare color gradient for Pris pr. DDD
    col_vals <- df[["Pris pr. DDD"]]
    brks <- quantile(col_vals, probs = seq(0, 1, 0.01), na.rm = TRUE)
    base_clrs <- colorRampPalette(c("green","yellow", "red"))(length(brks) + 1)
    clrs <- sapply(base_clrs, function(x) adjustcolor(x, alpha.f = 0.5))

    # Define tooltips for each header (in the same order as columns)
    header_tooltips <- c(
      "Det aktive stof i lægemidlet.",
      "Styrken på lægemidlet (fx 500 mg).",
      "Lægemidlets form (fx tablet, kapsel, opløsning).",
      "Pakningens størrelse og indhold.",
      "Produktets handelsnavn.",
      "Prisen på hele pakningen.",
      "Pris pr. defineret døgndosis (DDD).",
      "Substitutionsgruppe, dvs. om præparatet kan substitueres med andre."
    )
    # Custom JS to add tooltips to header cells
    js_callback <- JS(sprintf("
      table.on('draw', function() {
        var tooltips = %s;
        table.columns().header().each(function(th, i) {
          $(th).attr('title', tooltips[i]);
        });
      });
    ", jsonlite::toJSON(header_tooltips, auto_unbox = TRUE)))

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
        ),
        columnDefs = list(
          list(visible = FALSE, targets = which(names(df) == "14-dages prisændring") - 1) # If the col is present (shouldn't be)
        ),
        # JS callback for tooltips
        drawCallback = js_callback
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
