## ATC kode søge-app
##
## Denne Shiny‑applikation hjælper sundhedspersonale med at slå ATC‑koder op på
## baggrund af handelsnavne, generiske navne (aktive substanser) eller selve
## ATC‑koden. App'en er designet til at arbejde offline med et lokalt
## datagrundlag fra Lægemiddelstyrelsens liste over godkendte lægemidler i
## Danmark. Hvis Medicin.dk’s webservice eller produktresuméer bliver
## tilgængelige via API'er i fremtiden, kan koden tilpasses til at trække
## data derfra.  
##
## Datafilen (ListeOverGodkendteLaegemidler.csv) skal ligge i samme mappe som
## denne app. Den indeholder kolonnerne 'Navn' (handelsnavn),
## 'AktiveSubstanser' (generisk navn) og 'ATC-kode'.

library(shiny)
library(DT)
library(stringdist)

## -------------------------------------------------------------------------
## Indlæs datagrundlag
## Vi læser CSV-filen med semikolon som separator. Encoding "UTF-8-BOM" fjerner
## eventuelle byte‑order marks. Det tomme kolonnenavn "Unnamed: 11" fjernes.
## -------------------------------------------------------------------------
path <- file.path(getwd(), "data", "ListeOverGodkendteLaegemidler.csv")
atc_data <- read.csv(path, sep = ";", fileEncoding = "UTF-8-BOM",
                     stringsAsFactors = FALSE, check.names = FALSE)
names(atc_data) <- gsub("^Unnamed:.*", "", names(atc_data))

## Hold kun relevante kolonner
atc_data <- atc_data[, c("Navn", "AktiveSubstanser", "ATC-kode")]

## Fjern eventuelle NA'er
atc_data[is.na(atc_data)] <- ""

## -------------------------------------------------------------------------
## Hjælpefunktion: beregn maksimal Jaro‑Winkler‑similaritet mellem en søgestreng
## og både handelsnavne, aktive substanser og ATC‑kode. Jaro‑Winkler giver en
## værdi mellem 0 og 1 (1 er et perfekt match). Vi anvender maksimal
## similaritet på aktive substanser, da hver række kan indeholde flere
## substansnavne adskilt af komma.
## -------------------------------------------------------------------------
calc_similarity <- function(query, navn, substanser, atc) {
  q <- tolower(trimws(query))
  if (nchar(q) == 0) return(0)
  ## similarity med handelsnavn
  sim_navn <- stringsim(q, tolower(navn), method = "jw")
  ## similarity med aktive substanser (split på komma og fjern whitespace)
  subs <- unlist(strsplit(substanser, ","))
  subs <- trimws(subs)
  subs <- subs[subs != ""]
  if (length(subs) > 0) {
    sim_subs <- max(sapply(subs, function(s) stringsim(q, tolower(s), method = "jw")))
  } else {
    sim_subs <- 0
  }
  ## check om søgestrengen optræder i ATC‑koden (præfiks eller delstreng)
  sim_atc <- if (grepl(toupper(q), atc, fixed = TRUE)) 1 else 0
  ## returnér højeste score
  max(sim_navn, sim_subs, sim_atc)
}

## -------------------------------------------------------------------------
## Søgning: givet en søgestreng returneres et datatable med de mest relevante
## resultater. Antallet af resultater (n) kan justeres i app'en. Resultaterne
## sorteres efter faldende similarity. Vi filtrerer resultater med en
## minimumscore på 0.3 – det giver rimelig tolerance for stavefejl, men
## udelukker irrelevante matches.
## -------------------------------------------------------------------------
perform_search <- function(query, n = 15, threshold = 0.3) {
  if (nchar(trimws(query)) == 0) {
    return(data.frame())
  }
  scores <- mapply(calc_similarity, query = query,
                   navn = atc_data$Navn,
                   substanser = atc_data$AktiveSubstanser,
                   atc = atc_data$`ATC-kode`)
  keep <- which(scores >= threshold)
  if (length(keep) == 0) {
    return(data.frame())
  }
  res <- atc_data[keep, ]
  res$Score <- scores[keep]
  res <- res[order(-res$Score), ]
  head(res, n)[, c("Navn", "AktiveSubstanser", "ATC-kode")]
}

## -------------------------------------------------------------------------
## UI
## Layout med tre kolonner: søgefelt, forslag og valgte ATC-koder. Vi bruger
## DT til at gøre tabellerne interaktive. 'Search' opdateres automatisk via
## reactive expression, så resultater vises mens man skriver (debounce på 300 ms
## for at undgå unødige beregninger).
## -------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("ATC‑kode søge‑app"),
  fluidRow(
    column(
      4,
      textInput("query", "Søg på handelsnavn, aktiv substans eller ATC‑kode:", ""),
      helpText("Skriv mindst 2 tegn. Appen finder forslag selv ved stavefejl."),
      numericInput("maxResults", "Antal forslag:", value = 15, min = 5, max = 50, step = 5)
    ),
    column(
      4,
      h4("Forslag"),
      DTOutput("suggestions")
    ),
    column(
      4,
      h4("Valgte ATC‑koder"),
      DTOutput("selected_table"),
      br(),
      actionButton("copy_clip", "Kopier til udklipsholder"),
      br(),
      textOutput("copy_status")
    )
  )
)

## -------------------------------------------------------------------------
## Server
## - 'suggestions' genereres reaktivt med debounce på 300 ms.
## - Når en række i forslags‑tabellen klikkes, tilføjes den til listen over
##   valgte ATC‑koder (duplikater fjernes).
## - 'copy_clip' anvender clipr::write_clip til at kopiere koder til
##   udklipsholder; hvis clipr ikke er tilgængelig, vises en fejlbesked.
## -------------------------------------------------------------------------
server <- function(input, output, session) {
  ## reaktiv søgning med debounce
  debounced_query <- debounce(reactive(input$query), 300)

  suggestions_data <- reactive({
    q <- debounced_query()
    if (nchar(q) < 2) return(data.frame())
    perform_search(q, n = input$maxResults)
  })

  output$suggestions <- renderDT({
    dat <- suggestions_data()
    datatable(
      dat,
      selection = 'single',
      rownames = FALSE,
      options = list(pageLength = 5, scrollY = '300px')
    )
  }, server = FALSE)

  ## reactive values til valgte koder
  vals <- reactiveValues(selected = character())

  observeEvent(input$suggestions_rows_selected, {
    idx <- input$suggestions_rows_selected
    dat <- suggestions_data()
    if (length(idx) > 0 && nrow(dat) >= idx) {
      code <- dat[idx, 'ATC-kode']
      vals$selected <- unique(c(vals$selected, code))
    }
  })

  output$selected_table <- renderDT({
    datatable(data.frame(`ATC-kode` = vals$selected), rownames = FALSE,
              options = list(dom = 't', pageLength = 100))
  }, server = FALSE)

  observeEvent(input$copy_clip, {
    if (length(vals$selected) == 0) {
      output$copy_status <- renderText("Der er ingen ATC‑koder at kopiere.")
    } else {
      if (requireNamespace("clipr", quietly = TRUE)) {
        clipr::write_clip(paste(vals$selected, collapse = "\n"))
        output$copy_status <- renderText("ATC‑koder kopieret til udklipsholder.")
      } else {
        output$copy_status <- renderText("Pakke 'clipr' ikke installeret – kopier koderne manuelt.")
      }
    }
  })
}

shinyApp(ui, server)
