# app2.R — Shiny-app for lmgrupper using clean_data2.rds
# -----------------------------------------------------------------------------
# Uses improved current price/substitution data from erhverv.medicinpriser.dk
# plus old G_Substitutionslisten substitution group from fetch_data2.R.
#
# Expected file:
#   data/clean_data2.rds
# -----------------------------------------------------------------------------

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

# -----------------------------------------------------------------------------
# Helpers
# -----------------------------------------------------------------------------

# *** ÆNDRET: robust tal-konvertering, der håndterer både dansk og engelsk format ***
as_num <- function(x) {
  if (is.numeric(x)) return(x)
  
  x <- as.character(x)
  x <- stringr::str_squish(x)
  x[x %in% c("", "-", "NA", "NaN")] <- NA_character_
  
  x <- gsub("\\s", "", x)
  
  # Hvis både punktum og komma findes, antages punktum = tusindtalsseparator og komma = decimal
  both <- grepl("\\.", x) & grepl(",", x)
  x[both] <- gsub("\\.", "", x[both])
  x[both] <- gsub(",", ".", x[both])
  
  # Hvis kun komma findes, antages komma = decimal
  comma_only <- !both & grepl(",", x)
  x[comma_only] <- gsub(",", ".", x[comma_only])
  
  # Hvis kun punktum findes, bevares det som decimalpunktum
  suppressWarnings(as.numeric(x))
}

# -----------------------------------------------------------------------------
# Load preprocessed data
# -----------------------------------------------------------------------------

clean_data <- readRDS("data/clean_data2.rds") %>%
  mutate(
    across(
      c(Kategori, Underkategori, ATCtekst, ATC, Indholdsstof, Styrke, Form, Pakning,
        Lægemiddel, Firma, Varenummer, Substitutionsgruppe, G_Substitutionsgruppe,
        Kan_leveres, Udleveringsgruppe),
      as.character
    ),
    Pris = as_num(Pris),
    Pris_pr_DDD = as_num(Pris_pr_DDD)
  )

valid_categories <- unique(clean_data$Kategori)
valid_categories <- valid_categories[!is.na(valid_categories) & valid_categories != "unknown" & valid_categories != ""]

# Build the tree structure for the sidebar.
tree_data <- list()

for (cat in valid_categories) {
  cat_rows <- clean_data[clean_data$Kategori == cat, ]
  subcats <- unique(cat_rows$Underkategori)
  subcats <- subcats[!is.na(subcats) & subcats != "unknown" & subcats != ""]
  
  if (length(subcats) == 0) {
    tree_data[[cat]] <- cat
  } else {
    subcat_list <- list()
    
    for (subcat in subcats) {
      subcat_rows <- cat_rows[cat_rows$Underkategori == subcat, ]
      atc_values <- unique(subcat_rows$ATCtekst)
      atc_values <- atc_values[!is.na(atc_values) & atc_values != "unknown" & atc_values != ""]
      
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

# -----------------------------------------------------------------------------
# UI
# -----------------------------------------------------------------------------

ui <- secure_app(
  fluidPage(
    titlePanel("Prisoversigter for lægemiddelgrupper"),
    sidebarLayout(
      sidebarPanel(
        shinyTree("tree", checkbox = FALSE, search = TRUE, multiple = FALSE),
        
        radioButtons(
          inputId = "view_mode",
          label = "Visning",
          choices = c(
            "Simpel" = "simple",
            "Avanceret" = "advanced"
          ),
          selected = "simple",
          inline = TRUE
        ),
        
        tags$div(
          tags$p("Sådan bruger du appen:"),
          tags$ol(
            tags$li("Vælg en kategori, underkategori eller et generisk lægemiddel til venstre."),
            tags$li("Tabellen opdateres automatisk med de relevante lægemidler."),
            tags$li("Vælg simpel eller avanceret visning."),
            tags$li("Brug filtrene over hver kolonne for at indsnævre søgningen."),
            tags$li("Sortér efter en kolonne ved at klikke på overskriften.")
          ),
          tags$p("Priserne er hentet fra erhverv.medicinpriser.dk. ESP svarer her til AUP og anvendes som pris i tabellen."),
          tags$p("WHO defineret døgndosis (DDD) er defineret som den formodede gennemsnitlige vedligeholdelsesdosis pr. døgn for et lægemiddel anvendt til dets primære indikation. Formålet er at kunne sammenligne lægemidler på tværs af forskellige doseringer. DDD er ikke defineret for alle præparater, fx kan det ikke defineres for kombinationspræparater."),
          tags$p("Substitutionsgruppen er den aktuelle substitutionsgruppe fra erhverv.medicinpriser.dk. G-Substitutionsgruppe stammer fra G_Substitutionslisten.")
        )
      ),
      mainPanel(
        h3(textOutput("tableTitle")),
        DTOutput("tableOutput")
      )
    )
  )
)

# -----------------------------------------------------------------------------
# Server
# -----------------------------------------------------------------------------

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
      clean_data[FALSE, ]
    )
  })
  
  filteredData <- reactive({
    df <- categoryData()
    
    if (identical(input$view_mode, "advanced")) {
      df %>%
        transmute(
          `Indholdsstof` = Indholdsstof,
          `Styrke` = Styrke,
          `Form` = Form,
          `Pakning` = Pakning,
          `Handelsnavn` = Lægemiddel,
          `Pris` = Pris,
          `Pris pr. DDD` = Pris_pr_DDD,
          `Substitutionsgruppe` = Substitutionsgruppe,
          `G-Substitutionsgruppe` = G_Substitutionsgruppe,
          `Kan leveres?` = Kan_leveres
        )
    } else {
      df %>%
        transmute(
          `Indholdsstof` = Indholdsstof,
          `Styrke` = Styrke,
          `Form` = Form,
          `Pakning` = Pakning,
          `Handelsnavn` = Lægemiddel,
          `Pris` = Pris,
          `Pris pr. DDD` = Pris_pr_DDD
        )
    }
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
    if (nrow(df) == 0) return(datatable(df, rownames = FALSE))
    
    filter_columns <- intersect(
      c("Indholdsstof", "Styrke", "Form", "Pakning", "Handelsnavn", "Kan leveres?"),
      names(df)
    )
    df[filter_columns] <- lapply(df[filter_columns], as.factor)
    
    numeric_cols <- intersect(c("Pris", "Pris pr. DDD"), names(df))
    df[numeric_cols] <- lapply(df[numeric_cols], as_num)
    
    valid_vals <- df[["Pris pr. DDD"]][is.finite(df[["Pris pr. DDD"]])]
    
    dt <- datatable(
      df,
      rownames = FALSE,
      filter = "top",
      options = list(
        order = list(list(0, "asc")),
        orderClasses = TRUE,
        pageLength = -1,
        lengthMenu = list(c(-1, 10, 50, 100), c("Alle", "10", "50", "100")),
        scrollX = TRUE,
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
    ) %>%
      formatRound(intersect(c("Pris", "Pris pr. DDD"), names(df)), 2)
    
    if (length(valid_vals) > 0) {
      brks <- unique(quantile(valid_vals, probs = seq(0, 1, 0.01), na.rm = TRUE))
      
      if (length(brks) > 1) {
        base_clrs <- colorRampPalette(c("green", "yellow", "red"))(length(brks) + 1)
        clrs <- sapply(base_clrs, function(x) adjustcolor(x, alpha.f = 0.5))
        
        dt <- dt %>%
          formatStyle(
            columns = "Pris pr. DDD",
            backgroundColor = styleInterval(brks, clrs)
          )
      }
    }
    
    if ("Kan leveres?" %in% names(df)) {
      dt <- dt %>%
        formatStyle(
          "Kan leveres?",
          fontWeight = styleEqual(c("Ja", "Nej"), c("normal", "bold"))
        )
    }
    
    dt
  })
}

shinyApp(ui = ui, server = server)
