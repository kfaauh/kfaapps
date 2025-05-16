# app.R — Shiny-app for lmgrupper (uses precomputed clean_data.rds)

# Load required libraries
library(shiny)
library(shinyTree)
library(dplyr)
library(DT)
library(shinymanager)

credentials <- data.frame(
  user = "KFA",
  password = kfekfa123,
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
      shinyTree("tree", checkbox = FALSE, search = TRUE, multiple = FALSE)
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
    
    # Grab the first selected name
    sel <- sel[1]
    
    if (sel %in% valid_categories) {
      return(list(type = "kategori", value = sel))
    } else if (sel %in% unique(clean_data$Underkategori)) {
      return(list(type = "underkategori", value = sel))
    } else if (sel %in% unique(clean_data$ATCtekst)) {
      return(list(type = "atctekst", value = sel))
    } else {
      # Fallback
      return(list(type = "other", value = sel))
    }
  })
  
    categoryData <- reactive({
     sel <- selectedValue()
     if (is.null(sel)) {
       # return an empty slice of clean_data so it still has all your columns
        return(clean_data[FALSE, ])
      }
    
    if (sel$type == "kategori") {
      clean_data %>% dplyr::filter(Kategori == sel$value)
    } else if (sel$type == "underkategori") {
      clean_data %>% dplyr::filter(Underkategori == sel$value)
    } else if (sel$type == "atctekst") {
      clean_data %>% dplyr::filter(ATCtekst == sel$value)
    } else {
      # Fallback or empty
      data.frame()
    }
  })
  
  filteredData <- reactive({
    # First filter based on category selection, then select and reorder columns.
    # Also convert 'Styrke' to character so that the DT search works on it.
    categoryData() %>% 
      select(
        `Substitutionsgruppe` = SubstGruppe,
        `Indholdsstof`       = Indholdsstof,
        `Styrke`             = Styrke,
        `Form`               = Form,
        `Pakning`            = Pakning,
        `Handelsnavn`        = Lægemiddel,
        `Pris` = AUP,
        `Pris pr. DDD` = AUP_pr_DDD,
        `14-dages prisændring` = Prisændring
      ) %>% 
      mutate(Styrke = as.character(Styrke))
  })
  
  output$tableTitle <- renderText({
    sel <- selectedValue()
    if(is.null(sel)){
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
    
    # Convert columns used in filtering explicitly to factors
    filter_columns <- c(
      "Substitutionsgruppe",
      "Indholdsstof",
      "Styrke",
      "Form",
      "Pakning",
      "Handelsnavn"
    )
    
    df[filter_columns] <- lapply(df[filter_columns], as.factor)
    
    # Gradient color preparation (your existing logic)
    col_vals <- df[["Pris pr. DDD"]]
    col_vals_price <- df[["14-dages prisændring"]]
    brks <- quantile(col_vals, probs = seq(0, 1, 0.01), na.rm = TRUE)
    
    n1 <- 50
    n2 <- 50
    brks_price <- c(
      seq(-1, 0, length.out = n1),
      seq(0, 1, length.out = n2)[-1]
    )
    
    clrs_green_to_white <- colorRampPalette(c("green", "white"))(n1)
    clrs_white_to_red <- colorRampPalette(c("white", "red"))(n2)
    
    base_clrs <- colorRampPalette(c("green","yellow", "red"))(length(brks) + 1)
    
    clrs <- sapply(base_clrs, function(x) adjustcolor(x, alpha.f = 0.5))
    clrs_price <- c(
      "green",
      clrs_green_to_white[-1],
      clrs_white_to_red[-1],
      "red"
    )
    clrs_price <- adjustcolor(clrs_price, alpha.f = 0.5)
    
    # Datatable with proper dropdown filters
    datatable(
      df,
      rownames = FALSE,
      filter = "top", # <-- Use simple 'top' here, factors ensure dropdown filters
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
      formatStyle(
        columns = c("14-dages prisændring"),
        backgroundColor = styleInterval(brks_price,clrs_price)
      ) %>%
      formatRound("Pris pr. DDD", 2) %>%
      formatPercentage("14-dages prisændring", digits = 0)
  })
}

shinyApp(ui = ui, server = server)
