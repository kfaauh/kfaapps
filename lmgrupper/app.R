# 1) Load biblioteker ---------------------------------------------------
library(shiny)
library(shinyTree)
library(dplyr)
library(DT)

# 2) Load for-processed data ---------------------------------------------
# Sørg for, at du har kørt fetch_data.R, så clean_data.rds ligger i data/
clean_data <- readRDS("/srv/shiny-server/kfaapps/data-scripts/data/clean_data.rds")

# 3) Forbered dropdown/tree strukturen -----------------------------------
valid_categories <- clean_data$Kategori %>%
  unique() %>%
  na.omit() %>%
  setdiff("unknown")

# Build nested liste til shinyTree
tree_data <- lapply(valid_categories, function(cat) {
  sub <- clean_data %>% filter(Kategori == cat)
  subcats <- unique(sub$Underkategori) %>% na.omit() %>% setdiff("unknown")
  
  if (length(subcats)==0) {
    # Kategori uden underkategori bliver et leaf
    return(cat)
  } else {
    # Byg underkategori‐liste
    uc_list <- lapply(subcats, function(uc) {
      atc <- sub %>% filter(Underkategori==uc) %>%
        pull(ATCtekst) %>% unique() %>% na.omit() %>% setdiff("unknown")
      if(length(atc)==0) {
        return(uc)
      } else {
        # ATCtekst som navngivet liste
        lst <- as.list(atc); names(lst) <- atc
        return(lst)
      }
    })
    names(uc_list) <- subcats
    return(uc_list)
  }
})
names(tree_data) <- valid_categories

# 4) UI ------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Prisoversigter for lægemiddelgrupper"),
  fluidRow(
    column(12, shinyTree("tree", checkbox = FALSE, search = TRUE, multiple = FALSE))
  ),
  fluidRow(
    column(12,
           h3(textOutput("tableTitle")),
           DTOutput("tableOutput")
    )
  )
)

# 5) Server --------------------------------------------------------------
server <- function(input, output, session) {
  
  # Render træ
  output$tree <- renderTree({ tree_data })
  
  # Fortolk valgt node
  selectedValue <- reactive({
    sel <- unlist(get_selected(input$tree, format="names"))
    if (length(sel)==0) return(NULL)
    val <- sel[[1]]
    type <- if (val %in% valid_categories) {
      "kategori"
    } else if (val %in% unique(clean_data$Underkategori)) {
      "underkategori"
    } else {
      "atctekst"
    }
    list(type=type, value=val)
  })
  
  # Filtrér data baseret på selection
  filteredData <- reactive({
    sel <- selectedValue()
    if (is.null(sel)) return(clean_data[0,])
    df <- switch(sel$type,
                 kategori     = filter(clean_data, Kategori==sel$value),
                 underkategori= filter(clean_data, Underkategori==sel$value),
                 atctekst     = filter(clean_data, ATCtekst==sel$value)
    )
    # Vælg kolonner og muter Styrke til character
    df %>%
      select(
        Substitutionsgruppe = SubstGruppe,
        Indholdsstof       = Indholdsstof,
        Styrke             = Styrke,
        Form               = Form,
        Pakning            = Pakning,
        Handelsnavn        = Lægemiddel,
        `AIP`              = AIP,
        `AUP`              = AUP,
        `DDD`              = DDD,
        `AUP_pr_DDD`       = AUP_pr_DDD,
        `Prisændring`      = Prisændring
      ) %>%
      mutate(Styrke = as.character(Styrke))
  })
  
  # Titel over tabel
  output$tableTitle <- renderText({
    sel <- selectedValue()
    if (is.null(sel)) "Vælg kategori eller underkategori" else sel$value
  })
  
  # Render DT-tabel
  output$tableOutput <- renderDT({
    df <- filteredData()
    dt  <- datatable(
      df, rownames=FALSE, filter="top", 
      options=list(pageLength=25, orderClasses=TRUE)
    )
    # Tilføj styling om ønsket...
    dt
  })
}

# 6) Start app -----------------------------------------------------------
shinyApp(ui, server)
