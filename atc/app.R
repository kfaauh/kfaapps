## ATC-kode søge-app
## Robust version med hurtigere søgning og browser-baseret kopiering
##
## Forventet placering på server:
## /srv/shiny-server/kfaapps/atc/atc.R
## /srv/shiny-server/kfaapps/atc/data/ListeOverGodkendteLaegemidler.csv

library(shiny)
library(DT)
library(stringdist)

## -----------------------------------------------------------------------------
## 1. Find og indlæs CSV-fil
## -----------------------------------------------------------------------------

csv_candidates <- c(
  file.path(getwd(), "data", "ListeOverGodkendteLaegemidler.csv"),
  file.path(getwd(), "data", "ListeOverGodkendteLægemidler.csv"),
  file.path(getwd(), "ListeOverGodkendteLaegemidler.csv"),
  file.path(getwd(), "ListeOverGodkendteLægemidler.csv")
)

csv_path <- csv_candidates[file.exists(csv_candidates)][1]

if (is.na(csv_path)) {
  stop(
    "CSV-filen blev ikke fundet. Appen ledte her:\n",
    paste(csv_candidates, collapse = "\n")
  )
}

read_lgl_csv <- function(path) {
  encodings <- c("UTF-8-BOM", "UTF-8", "latin1")
  required_cols <- c("Navn", "AktiveSubstanser", "ATC-kode")

  for (enc in encodings) {
    dat <- try(
      read.csv(
        path,
        sep = ";",
        fileEncoding = enc,
        stringsAsFactors = FALSE,
        check.names = FALSE
      ),
      silent = TRUE
    )

    if (!inherits(dat, "try-error") && all(required_cols %in% names(dat))) {
      return(dat)
    }
  }

  stop(
    "CSV-filen kunne læses, men de nødvendige kolonner blev ikke fundet. ",
    "Forventede kolonner: Navn, AktiveSubstanser og ATC-kode."
  )
}

raw_data <- read_lgl_csv(csv_path)
raw_data[is.na(raw_data)] <- ""

## Behold de mest nyttige kolonner, hvis de findes.
display_cols <- intersect(
  c("Navn", "AktiveSubstanser", "Lægemiddelform", "Styrketekst", "ATC-kode", "Er i Medicinpriser"),
  names(raw_data)
)

atc_data <- unique(raw_data[, display_cols, drop = FALSE])
atc_data <- atc_data[atc_data$`ATC-kode` != "", , drop = FALSE]
atc_data$row_id_internal <- seq_len(nrow(atc_data))

## -----------------------------------------------------------------------------
## 2. Tekstnormalisering og søgeindeks
## -----------------------------------------------------------------------------

normalize_text <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

normalize_atc <- function(x) {
  toupper(gsub("[^A-Za-z0-9]", "", as.character(x)))
}

add_index_rows <- function(row_id, term_raw, field) {
  term_raw <- unique(trimws(term_raw))
  term_raw <- term_raw[term_raw != ""]

  if (length(term_raw) == 0) {
    return(NULL)
  }

  data.frame(
    row_id = row_id,
    term_raw = term_raw,
    term_norm = normalize_text(term_raw),
    field = field,
    stringsAsFactors = FALSE
  )
}

index_list <- vector("list", nrow(atc_data) * 4)
k <- 1

for (i in seq_len(nrow(atc_data))) {
  row_id <- atc_data$row_id_internal[i]

  ## Handelsnavn
  index_list[[k]] <- add_index_rows(row_id, atc_data$Navn[i], "Handelsnavn")
  k <- k + 1

  ## Aktive substanser: både samlet felt og enkelt-substanser.
  active_full <- atc_data$AktiveSubstanser[i]
  active_parts <- unlist(strsplit(active_full, "[,;/+]"))
  active_parts <- trimws(active_parts)
  active_terms <- unique(c(active_full, active_parts))
  index_list[[k]] <- add_index_rows(row_id, active_terms, "Aktiv substans")
  k <- k + 1

  ## ATC-kode.
  index_list[[k]] <- add_index_rows(row_id, atc_data$`ATC-kode`[i], "ATC-kode")
  k <- k + 1
}

search_index <- do.call(rbind, index_list)
search_index <- search_index[!is.na(search_index$term_norm) & search_index$term_norm != "", , drop = FALSE]
search_index$term_atc_norm <- normalize_atc(search_index$term_raw)

## Lille cache, så gentagne søgninger ikke beregnes igen.
search_cache <- new.env(parent = emptyenv())

## -----------------------------------------------------------------------------
## 3. Hurtig søgefunktion
## -----------------------------------------------------------------------------

perform_search <- function(query, n = 25) {
  query_raw <- trimws(query)
  query_norm <- normalize_text(query_raw)
  query_atc <- normalize_atc(query_raw)

  if (nchar(query_norm) < 2 && nchar(query_atc) < 2) {
    return(data.frame())
  }

  cache_key <- paste0(query_norm, "__", query_atc, "__", n)
  if (exists(cache_key, envir = search_cache, inherits = FALSE)) {
    return(get(cache_key, envir = search_cache, inherits = FALSE))
  }

  term <- search_index$term_norm
  score <- rep(0, length(term))

  ## 1) Hurtige eksakte/prefix/substring-match.
  exact_match <- term == query_norm
  prefix_match <- startsWith(term, query_norm)
  contains_match <- grepl(query_norm, term, fixed = TRUE)

  score[exact_match] <- pmax(score[exact_match], 1.00)
  score[prefix_match] <- pmax(score[prefix_match], 0.95)
  score[contains_match] <- pmax(score[contains_match], 0.88)

  ## 2) Alle søgetokens skal optræde i termen, fx "eso prazol" -> "esomeprazol".
  tokens <- unlist(strsplit(query_norm, "\\s+"))
  tokens <- tokens[nchar(tokens) >= 2]
  if (length(tokens) >= 2) {
    token_match <- rep(TRUE, length(term))
    for (tok in tokens) {
      token_match <- token_match & grepl(tok, term, fixed = TRUE)
    }
    score[token_match] <- pmax(score[token_match], 0.90)
  }

  ## 3) ATC-kode-match. Dette skal vægtes højt, da brugere ofte søger på dele af ATC.
  if (nchar(query_atc) >= 2) {
    atc_term <- search_index$term_atc_norm
    atc_exact <- atc_term == query_atc
    atc_prefix <- startsWith(atc_term, query_atc)
    atc_contains <- grepl(query_atc, atc_term, fixed = TRUE)

    score[atc_exact] <- pmax(score[atc_exact], 1.00)
    score[atc_prefix] <- pmax(score[atc_prefix], 0.97)
    score[atc_contains] <- pmax(score[atc_contains], 0.92)
  }

  ## 4) Fuzzy matching for stavefejl. Kun fra 3 tegn for at undgå støj og ventetid.
  if (nchar(query_norm) >= 3) {
    fuzzy <- stringdist::stringsim(query_norm, term, method = "jw", p = 0.10)
    score <- pmax(score, fuzzy)
  }

  ## Små felt-vægte for mere intuitiv sortering.
  score <- score + ifelse(search_index$field == "ATC-kode", 0.03, 0)
  score <- score + ifelse(search_index$field == "Handelsnavn", 0.02, 0)
  score <- pmin(score, 1)

  ## Threshold afhænger af søgelængde. Korte søgninger kræver bedre match.
  threshold <- if (nchar(query_norm) <= 3) 0.88 else 0.78
  keep <- which(score >= threshold)

  if (length(keep) == 0) {
    res <- data.frame()
    assign(cache_key, res, envir = search_cache)
    return(res)
  }

  tmp <- data.frame(
    row_id = search_index$row_id[keep],
    Score = score[keep],
    Match = paste0(search_index$field[keep], ": ", search_index$term_raw[keep]),
    stringsAsFactors = FALSE
  )

  tmp <- tmp[order(-tmp$Score), , drop = FALSE]
  tmp <- tmp[!duplicated(tmp$row_id), , drop = FALSE]
  tmp <- head(tmp, n)

  res <- atc_data[match(tmp$row_id, atc_data$row_id_internal), display_cols, drop = FALSE]
  res$Match <- tmp$Match
  res <- res[, c(setdiff(names(res), "Match"), "Match"), drop = FALSE]

  assign(cache_key, res, envir = search_cache)
  res
}

## -----------------------------------------------------------------------------
## 4. UI
## -----------------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("\n      body { background-color: #f7f8fa; }\n      .well { background-color: white; border-radius: 10px; }\n      .btn-primary, .btn-success { width: 100%; margin-bottom: 8px; }\n      .btn-default, .btn-danger, .btn-warning { width: 100%; margin-bottom: 8px; }\n      textarea.form-control { font-family: Consolas, 'Liberation Mono', monospace; }\n      .small-help { color: #666; font-size: 0.9em; }\n      .app-title { margin-bottom: 0; }\n      .subtitle { margin-top: 4px; color: #555; }\n    ")),
    tags$script(HTML("\n      $(document).on('keydown', '#query', function(e) {\n        if (e.key === 'Enter') {\n          e.preventDefault();\n          $('#add_top').click();\n        }\n      });\n\n      Shiny.addCustomMessageHandler('copy-atc-codes', function(message) {\n        const text = message.text || '';\n\n        function report(ok, msg) {\n          Shiny.setInputValue('copy_result', {ok: ok, msg: msg, nonce: Math.random()}, {priority: 'event'});\n        }\n\n        function fallbackCopy(t) {\n          try {\n            const textArea = document.createElement('textarea');\n            textArea.value = t;\n            textArea.setAttribute('readonly', '');\n            textArea.style.position = 'fixed';\n            textArea.style.left = '-9999px';\n            document.body.appendChild(textArea);\n            textArea.focus();\n            textArea.select();\n            const ok = document.execCommand('copy');\n            document.body.removeChild(textArea);\n            return ok;\n          } catch (err) {\n            return false;\n          }\n        }\n\n        if (!text.length) {\n          report(false, 'Der er ingen ATC-koder at kopiere.');\n          return;\n        }\n\n        if (navigator.clipboard && window.isSecureContext) {\n          navigator.clipboard.writeText(text).then(function() {\n            report(true, 'ATC-koder kopieret til udklipsholderen.');\n          }).catch(function() {\n            const ok = fallbackCopy(text);\n            report(ok, ok ? 'ATC-koder kopieret til udklipsholderen.' : 'Automatisk kopiering mislykkedes. Markér feltet nederst og kopier manuelt.');\n          });\n        } else {\n          const ok = fallbackCopy(text);\n          report(ok, ok ? 'ATC-koder kopieret til udklipsholderen.' : 'Automatisk kopiering kræver typisk HTTPS. Markér feltet nederst og kopier manuelt.');\n        }\n      });\n    "))
  ),

  fluidRow(
    column(
      12,
      h2("ATC-kode søge-app", class = "app-title"),
      p("Søg på handelsnavn, aktiv substans eller ATC-kode. Tilføj koder til arbejdslisten, og kopier dem derefter én kode per linje.", class = "subtitle")
    )
  ),

  fluidRow(
    column(
      4,
      wellPanel(
        h4("1) Søg"),
        textInput("query", "Søgetekst", placeholder = "fx esomeprazol, panodil, ramipril, N02BE01"),
        div("Tryk Enter for at tilføje øverste forslag.", class = "small-help"),
        br(),
        sliderInput("max_results", "Antal forslag", min = 10, max = 50, value = 25, step = 5),
        actionButton("clear_search", "Ryd søgefelt", class = "btn-default")
      )
    ),

    column(
      4,
      wellPanel(
        h4("2) Vælg forslag"),
        DTOutput("suggestions"),
        br(),
        actionButton("add_selected", "Tilføj valgt ATC-kode", class = "btn-primary"),
        actionButton("add_top", "Tilføj øverste forslag", class = "btn-success"),
        div("Klik på en række og tryk 'Tilføj valgt ATC-kode' – eller brug Enter i søgefeltet.", class = "small-help")
      )
    ),

    column(
      4,
      wellPanel(
        h4("3) Arbejdsliste"),
        div("Listen bevares, mens du laver flere søgninger. Brug 'Start ny liste', når du vil begynde forfra.", class = "small-help"),
        br(),
        DTOutput("selected_table"),
        br(),
        actionButton("remove_selected", "Fjern valgt kode", class = "btn-warning"),
        actionButton("clear_list", "Start ny liste", class = "btn-danger"),
        actionButton("copy_codes", "Kopier ATC-koder", class = "btn-primary"),
        textOutput("copy_status"),
        br(),
        textAreaInput("copy_box", "Kopifelt / manuel backup", value = "", rows = 10, width = "100%")
      )
    )
  )
)

## -----------------------------------------------------------------------------
## 5. Server
## -----------------------------------------------------------------------------

server <- function(input, output, session) {
  vals <- reactiveValues(
    selected_codes = character(),
    status = ""
  )

  debounced_query <- debounce(reactive(input$query), 350)

  suggestions_data <- reactive({
    q <- debounced_query()
    perform_search(q, n = input$max_results)
  })

  output$suggestions <- renderDT({
    dat <- suggestions_data()

    if (nrow(dat) == 0) {
      return(datatable(
        data.frame(Besked = "Ingen forslag endnu. Skriv mindst 2 tegn."),
        rownames = FALSE,
        options = list(dom = "t", pageLength = 1)
      ))
    }

    datatable(
      dat,
      selection = "single",
      rownames = FALSE,
      options = list(
        pageLength = 8,
        lengthChange = FALSE,
        scrollX = TRUE,
        scrollY = "330px",
        dom = "tip"
      )
    )
  }, server = FALSE)

  add_codes <- function(codes) {
    codes <- unique(trimws(codes))
    codes <- codes[codes != ""]

    if (length(codes) == 0) {
      vals$status <- "Ingen kode blev tilføjet."
      return(invisible(NULL))
    }

    before <- length(vals$selected_codes)
    vals$selected_codes <- unique(c(vals$selected_codes, codes))
    added <- length(vals$selected_codes) - before

    vals$status <- if (added == 0) {
      "Koden var allerede på listen."
    } else if (added == 1) {
      paste0("Tilføjede: ", paste(codes, collapse = ", "))
    } else {
      paste0("Tilføjede ", added, " nye ATC-koder.")
    }
  }

  observeEvent(input$add_selected, {
    dat <- suggestions_data()
    idx <- input$suggestions_rows_selected

    if (nrow(dat) == 0 || length(idx) == 0 || idx > nrow(dat)) {
      vals$status <- "Vælg først en række i forslagslisten."
      return(NULL)
    }

    add_codes(dat$`ATC-kode`[idx])
  })

  observeEvent(input$add_top, {
    dat <- suggestions_data()

    if (nrow(dat) == 0) {
      vals$status <- "Der er ingen forslag at tilføje."
      return(NULL)
    }

    add_codes(dat$`ATC-kode`[1])
  })

  observeEvent(input$remove_selected, {
    idx <- input$selected_table_rows_selected

    if (length(idx) == 0 || length(vals$selected_codes) == 0) {
      vals$status <- "Vælg først en kode i arbejdslisten."
      return(NULL)
    }

    removed <- vals$selected_codes[idx]
    vals$selected_codes <- vals$selected_codes[-idx]
    vals$status <- paste0("Fjernede: ", paste(removed, collapse = ", "))
  })

  observeEvent(input$clear_list, {
    vals$selected_codes <- character()
    vals$status <- "Arbejdslisten er ryddet."
  })

  observeEvent(input$clear_search, {
    updateTextInput(session, "query", value = "")
    vals$status <- "Søgefeltet er ryddet. Arbejdslisten er uændret."
  })

  selected_df <- reactive({
    data.frame(`ATC-kode` = vals$selected_codes, check.names = FALSE)
  })

  selected_text <- reactive({
    paste(vals$selected_codes, collapse = "\n")
  })

  output$selected_table <- renderDT({
    datatable(
      selected_df(),
      selection = "single",
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = 100,
        scrollY = "220px"
      )
    )
  }, server = FALSE)

  observe({
    updateTextAreaInput(session, "copy_box", value = selected_text())
  })

  observeEvent(input$copy_codes, {
    session$sendCustomMessage("copy-atc-codes", list(text = selected_text()))
  })

  observeEvent(input$copy_result, {
    vals$status <- input$copy_result$msg
  })

  output$copy_status <- renderText({
    vals$status
  })
}

shinyApp(ui, server)
