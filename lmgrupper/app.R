# app.R — Shiny-app for lmgrupper (uses precomputed clean_data.rds)

# Load required libraries
library(shiny)
library(shinyTree)
library(dplyr)
library(DT)

# Load preprocessed data
# Adjust this path if your RDS is in a different location
clean_data <- readRDS("/srv/shiny-server/kfaapps/data-scripts/data/clean_data.rds")

# Prepare valid categories for the tree
valid_categories <- clean_data$Kategori %>%
  unique() %>%
  na.omit() %>%
  setdiff("unknown")

# Build nested list structure for shinyTree
tree_data <- lapply(valid_categories, function(cat) {
  df_cat <- filter(clean_data, Kategori == cat)
  subcats <- df_cat$Underkategori %>% unique() %>% na.omit() %>% setdiff("unknown")
  if (length(subcats) == 0) return(cat)
  uc_list <- lapply(subcats, function(uc) {
    df_uc <- filter(df_cat, Underkategori == uc)
    atc_vals <- df_uc$ATCtekst %>% unique() %>% na.omit() %>% setdiff("unknown")
    if (length(atc_vals) == 0) return(uc)
    lst <- as.list(atc_vals)
    names(lst) <- atc_vals
    lst
  })
  names(uc_list) <- subcats
  uc_list
})
names(tree_data) <- valid_categories

# UI definition
ui <- fluidPage(
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

# Server logic
server <- function(input, output, session) {
  # Render the tree
  output$tree <- renderTree({ tree_data })

  # Determine selection
  selectedValue <- reactive({
    sel <- unlist(shinyTree::get_selected(input$tree, format = "names"))
    if (length(sel) == 0) return(NULL)
    val <- sel[1]
    type <- if (val %in% valid_categories) {
      "kategori"
    } else if (val %in% clean_data$Underkategori) {
      "underkategori"
    } else {
      "atctekst"
    }
    list(type = type, value = val)
  })

  # Filter data based on selection
  filteredData <- reactive({
    sel <- selectedValue()
    if (is.null(sel)) return(clean_data[0, ])
    df <- switch(sel$type,
      kategori      = filter(clean_data, Kategori == sel$value),
      underkategori = filter(clean_data, Underkategori == sel$value),
      atctekst      = filter(clean_data, ATCtekst == sel$value)
    )
    df %>%
      select(
        Substitutionsgruppe = SubstGruppe,
        Indholdsstof       = Indholdsstof,
        Styrke             = Styrke,
        Form               = Form,
        Pakning            = Pakning,
        Handelsnavn        = Lægemiddel,
        Pris               = AUP,
        Pris_pr_DDD        = AUP_pr_DDD,
        Prisændring        = Prisændring
      ) %>%
      mutate(Styrke = as.character(Styrke))
  })

  # Output title
  output$tableTitle <- renderText({
    sel <- selectedValue()
    if (is.null(sel)) {
      "Vælg kategori eller underkategori"
    } else {
      sel$value
    }
  })

  # Render DataTable
  output$tableOutput <- renderDT({
    df <- filteredData()
    if (nrow(df) == 0) return(datatable(df))
    # Convert certain columns to factor for dropdown filters
    for (col in c("Substitutionsgruppe", "Indholdsstof", "Styrke", "Form", "Pakning", "Handelsnavn")) {
      df[[col]] <- as.factor(df[[col]])
    }
    # Compute breakpoints for color scales
    brks <- quantile(df$Pris_pr_DDD, probs = seq(0, 1, 0.01), na.rm = TRUE)
    brks_price <- c(seq(-1, 0, length.out = 50), seq(0, 1, length.out = 50)[-1])
    base_clrs <- colorRampPalette(c("green","yellow","red"))(length(brks) + 1)
    clrs <- sapply(base_clrs, function(x) adjustcolor(x, alpha.f = 0.5))
    clrs_price <- c(
      "green",
      colorRampPalette(c("green","white"))(50)[-1],
      colorRampPalette(c("white","red"))(50)[-1],
      "red"
    )
    clrs_price <- adjustcolor(clrs_price, alpha.f = 0.5)

    datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(orderClasses = TRUE)
    ) %>%
      formatStyle("Pris_pr_DDD", backgroundColor = styleInterval(brks, clrs)) %>%
      formatStyle("Prisændring", backgroundColor = styleInterval(brks_price, clrs_price)) %>%
      formatRound("Pris_pr_DDD", 2) %>%
      formatPercentage("Prisændring", digits = 0)
  })
}

# Run the app
shinyApp(ui = ui, server = server)
