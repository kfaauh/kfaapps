# app.R
# ------------------------------------------------------------
# ATC → Pro.medicin (præparatliste + bruger-valg)
# → Pro.medicin-sektioner (graviditet/amning)  [uden "Se også"/"Referencer"]
# → Janusmed score (først /lakemedel via flere brand-varianter, fallback til /substans)
# → LactMed Summary of Use during Lactation (NBK501922 index + fuzzy match)
# ------------------------------------------------------------

library(shiny)
library(dplyr)
library(stringr)
library(rvest)
library(xml2)
library(DT)
library(shinymanager)

credentials <- data.frame(
  user     = "KFA",
  password = "kfekfa123",
  stringsAsFactors = FALSE
)

# ------------------------------------------------------------
# Utilities
# ------------------------------------------------------------


`%||%` <- function(a, b) if (!is.null(a)) a else b

clean_text <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  str_squish(x)
}

# Robust HTML fetch (shiny-server venlig)
safe_read_html <- function(url) {
  tryCatch({
    if (!requireNamespace("httr", quietly = TRUE)) return(NULL)
    
    resp <- httr::GET(
      url,
      httr::user_agent("Mozilla/5.0 (Shiny; +https://shiny.rstudio.com)"),
      httr::accept("text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"),
      httr::timeout(20)
    )
    
    if (httr::http_error(resp)) return(NULL)
    
    html <- httr::content(resp, as = "text", encoding = "UTF-8")
    xml2::read_html(html)
  }, error = function(e) NULL)
}

parse_atc_input <- function(x) {
  v <- (x %||% "")
  v <- unlist(strsplit(v, "\\r?\\n"))
  v <- trimws(v)
  v <- v[v != "" & v != "-"]
  toupper(unique(v))
}

# Til LactMed/Janusmed substans: lav en “rimelig” substansstreng
simplify_substance <- function(x) {
  x <- clean_text(x)
  if (!nzchar(x)) return("")
  x <- strsplit(x, "\\+|,|/|\\bog\\b", perl = TRUE)[[1]][1]
  x <- str_replace_all(x, "\\(.*?\\)", "")
  clean_text(x)
}

to_ascii_lower <- function(x) {
  x <- clean_text(x)
  if (!nzchar(x)) return("")
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }
  tolower(x)
}

# ------------------------------------------------------------
# Pro.medicin.dk
# ------------------------------------------------------------

promedicin_search_url <- function(atc) {
  atc <- toupper(str_squish(atc))
  paste0("https://pro.medicin.dk/Search/Search/ShowAtc/", atc)
}

# ATC → liste af præparater (handelsnavn + URL)
promedicin_get_products_from_atc <- function(atc) {
  url <- promedicin_search_url(atc)
  doc <- safe_read_html(url)
  if (is.null(doc)) return(tibble(name = character(), url = character()))
  
  a <- doc %>% html_elements("a")
  href <- a %>% html_attr("href") %>% as.character()
  txt  <- a %>% html_text2() %>% clean_text()
  
  tibble(name = txt, href = href) %>%
    filter(
      !is.na(href),
      str_detect(href, "^/Medicin/Praeparater/\\d+($|#)"),
      name != ""
    ) %>%
    mutate(url = paste0("https://pro.medicin.dk", href)) %>%
    select(name, url) %>%
    distinct()
}

# Præparatside → aktiv substans (generisk) (robust links)
promedicin_extract_active_substance <- function(doc) {
  if (is.null(doc)) return("")
  
  nodes <- doc %>%
    rvest::html_elements(".glob-praeparat-indholdsstofffer-header a")
  
  if (length(nodes) > 0) {
    subs <- nodes %>%
      rvest::html_text2() %>%
      clean_text()
    subs <- unique(subs[subs != ""])
    if (length(subs) > 0) return(paste(subs, collapse = " + "))
  }
  
  ""
}

promedicin_get_active_substance_from_url <- function(product_url) {
  doc <- safe_read_html(product_url)
  if (is.null(doc)) return("")
  promedicin_extract_active_substance(doc)
}

# --- NYT: fjern "Se også:" og "Referencer:" fra output ---
strip_pm_tail_sections <- function(txt) {
  txt <- txt %||% ""
  txt <- gsub("\u00a0", " ", txt, fixed = TRUE)
  txt <- str_replace_all(txt, "\\r", "")
  
  # Fjern de PRÆCISE "Se også"-linjer (uanset placering)
  txt <- str_replace_all(txt, "\\n?\\s*Se også:\\s*Klassifikation\\s*-\\s*graviditet\\s*\\n?", "\n")
  txt <- str_replace_all(txt, "\\n?\\s*Se også:\\s*Klassifikation\\s*-\\s*amning\\s*\\n?", "\n")
  
  # (Behold gerne denne også – den fjerner "Referencer:"-hale hvis den sniger sig ind)
  txt <- str_replace(txt, "(?s)\\n\\s*Referencer:\\s*.*$", "")
  
  # Ryd op i whitespace
  txt <- str_replace_all(txt, "\\n{3,}", "\n\n")
  trimws(txt)
}

promedicin_format_text <- function(x) {
  x <- x %||% ""
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x <- str_replace_all(x, "[ \t]+", " ")
  x <- str_replace_all(x, "\\s*(Baggrund:)\\s*", "\n\n\\1 ")
  x <- str_replace_all(x, "\\n{3,}", "\n\n")
  trimws(x)
}

# Udtræk Pro.medicin-sektion (Graviditet/Amning) fra præparatside
promedicin_extract_section <- function(doc, section = c("Graviditet", "Amning"),
                                       max_chars = 8000) {
  section <- match.arg(section)
  if (is.null(doc)) return("")
  
  h3 <- doc %>%
    html_elements("h3") %>%
    (\(nodes) nodes[tolower(str_squish(html_text2(nodes))) == tolower(section)])()
  
  if (length(h3) == 0) return("")
  h3 <- h3[[1]]
  
  wrapper <- xml_find_first(h3, "ancestor::div[contains(@class,'glob-content-section-wrapper')]")
  if (is.na(wrapper) || length(wrapper) == 0) return("")
  
  wrapper_doc <- read_html(as.character(wrapper))
  
  concl <- wrapper_doc %>%
    html_elements(".glob-content-header-openclose-image-wrapper img") %>%
    html_attr("alt") %>%
    na.omit() %>%
    as.character()
  
  concl <- if (length(concl) > 0) str_squish(concl[[1]]) else ""
  
  text_node <- wrapper_doc %>%
    html_element(".glob-content-section-text")
  
  if (is.na(text_node) || length(text_node) == 0) {
    raw <- wrapper_doc %>% html_text2()
    raw <- promedicin_format_text(raw)
    raw <- strip_pm_tail_sections(raw)
    return(substr(raw, 1, min(nchar(raw), max_chars)))
  }
  
  # Fjern referencer-elementer i DOM, og tag kun brødteksten
  tn_doc <- read_html(as.character(text_node))
  tn_doc %>% html_elements(".referencer") %>% xml_remove()
  
  body_txt <- tn_doc %>% html_text2() %>% str_squish()
  body_txt <- promedicin_format_text(body_txt)
  
  # Drop gentagelse af konklusion, hvis den allerede står i brødtekst
  if (concl != "" && str_starts(tolower(body_txt), tolower(concl))) concl <- ""
  
  out <- c(if (concl != "") concl else NULL, body_txt)
  res <- trimws(str_replace_all(paste(out, collapse = "\n"), "\\n{3,}", "\n\n"))
  
  # NYT: strip "Se også"/"Referencer" hvis de ligger som hale i teksten
  res <- strip_pm_tail_sections(res)
  
  if (nchar(res) > max_chars) res <- substr(res, 1, max_chars)
  res
}

promedicin_scrape_from_product_url <- function(product_url, what = c("graviditet", "amning")) {
  what <- match.arg(what)
  doc <- safe_read_html(product_url)
  if (is.null(doc)) return(list(text = "", url = product_url))
  section <- if (what == "graviditet") "Graviditet" else "Amning"
  txt <- promedicin_extract_section(doc, section = section)
  list(text = txt, url = product_url)
}

# ------------------------------------------------------------
# Janusmed – score via /lakemedel/<Handelsnavn>
# Fallback: loop ALLE handelsnavne fra Pro.medicin for samme ATC,
# indtil der findes en score.
# ------------------------------------------------------------

clean_trade_name_candidates <- function(x) {
  x0 <- clean_text(x)
  if (!nzchar(x0)) return(character())
  
  # ✅ Fjern citationstegn, ®™ osv, parenteser og trailing ", ..."
  x1 <- str_replace_all(x0, "[\"“”‘’®™℠©]", "")
  x1 <- str_replace_all(x1, "\\(.*?\\)", "")
  x1 <- str_replace_all(x1, ",.*$", "")
  x1 <- str_squish(x1)
  
  tokens <- unlist(strsplit(x1, "\\s+"))
  tokens <- tokens[tokens != ""]
  if (length(tokens) == 0) return(character())
  
  # Tokens vi typisk IKKE vil have med i Janusmed lakemedel-URL
  # (du kan udvide listen løbende)
  drop_modifiers <- c(
    "cr","mr","sr","er","xr","xl","la","dr",
    "retard","depot","depottabletter","depotkapsler",
    "uno","mite","zoc",
    "forte"
  )
  
  # Heuristik:
  # - Første token antages at være “kerne-navnet” (fx Warfarin, Simvastatin, Ramipril)
  # - Resten: behold især firma/brand tokens (ofte ALL CAPS), drop modifiers
  core <- tokens[1]
  
  rest <- tokens[-1]
  if (length(rest) > 0) {
    rest_keep <- rest[
      !tolower(rest) %in% drop_modifiers
    ]
  } else {
    rest_keep <- character()
  }
  
  # Firma-token er ofte ALL CAPS eller “Capitalized” brand (Orion)
  # Vi laver en prioriteret liste over “sandsynlige firma/brand”-tokens fra rest_keep
  is_all_caps <- function(t) grepl("^[A-ZÅÆØ]{2,}$", t)
  is_capitalized <- function(t) grepl("^[A-ZÅÆØ][a-zåæø]+$", t)
  
  company_like <- rest_keep[is_all_caps(rest_keep) | is_capitalized(rest_keep)]
  company_like <- unique(company_like)
  
  # Kandidater:
  cand <- c()
  
  # 1) core alene
  cand <- c(cand, core)
  
  # 2) core + firma (første “company-like”)
  if (length(company_like) >= 1) cand <- c(cand, paste(core, company_like[1], collapse = " "))
  
  # 3) core + alle “company-like” tokens (hvis der er flere)
  if (length(company_like) >= 2) cand <- c(cand, paste(c(core, company_like), collapse = " "))
  
  # 4) core + (rest_keep uden modifiers) som en bred fallback
  if (length(rest_keep) >= 1) cand <- c(cand, paste(c(core, rest_keep), collapse = " "))
  
  # 5) Også en “renset core” hvis der er underlige tegn
  core_clean <- str_replace_all(core, "[^A-Za-z0-9\\-ÅÆØåæø]", "")
  if (nzchar(core_clean)) cand <- c(cand, core_clean)
  
  cand <- unique(str_squish(cand))
  cand <- cand[cand != ""]
  cand
}

janusmed_get_score <- function(doc) {
  if (is.null(doc)) return(NA_integer_)
  
  score_txt <- doc %>%
    html_elements("[data-cy='card-classification'] span.classification, [data-cy='card-classification-small'] span.classification") %>%
    html_text2() %>%
    str_squish()
  
  score_txt <- score_txt[score_txt != ""]
  if (length(score_txt) == 0) return(NA_integer_)
  
  suppressWarnings(as.integer(score_txt[[1]]))
}

janusmed_url_lakemedel <- function(kind = c("foster", "amning"), trade_name) {
  kind <- match.arg(kind)
  base <- if (kind == "foster") {
    "https://janusmed.se/fosterpaverkan/lakemedel/"
  } else {
    "https://janusmed.se/amning/lakemedel/"
  }
  paste0(base, utils::URLencode(trade_name, reserved = TRUE))
}

janusmed_score_from_url <- function(url) {
  doc <- safe_read_html(url)
  if (is.null(doc)) return(NA_integer_)
  janusmed_get_score(doc)
}

# NYT: prøv (1) valgt præparat først, derefter (2) alle Pro.medicin-handelsnavne for samme ATC
janusmed_try_score_loop_tradenames <- function(kind = c("foster", "amning"),
                                               atc,
                                               selected_trade,
                                               products_by_atc_df) {
  kind <- match.arg(kind)
  
  # Handelsnavne fra Pro.medicin for samme ATC
  all_names <- products_by_atc_df %>%
    filter(ATC == atc) %>%
    pull(name) %>%
    as.character()
  
  # Byg kandidat-liste:
  # - først kandidater fra det valgte præparat
  # - derefter kandidater fra alle andre handelsnavne under samme ATC
  candidates <- c(
    clean_trade_name_candidates(selected_trade),
    unlist(lapply(all_names, clean_trade_name_candidates), use.names = FALSE)
  )
  candidates <- unique(candidates)
  candidates <- candidates[!is.na(candidates) & candidates != ""]
  
  for (nm in candidates) {
    url <- janusmed_url_lakemedel(kind, nm)
    sc  <- janusmed_score_from_url(url)
    if (!is.na(sc)) {
      return(list(score = sc, url = url, hit_name = nm))
    }
  }
  
  list(score = NA_integer_, url = "", hit_name = "")
}

# NYT: “Prøv indtil hit” (lakemedel-kandidater → substans-fallback)
janusmed_try_score <- function(kind = c("foster", "amning"), trade_name, generic_da) {
  kind <- match.arg(kind)
  
  # 1) Prøv /lakemedel/ med flere kandidat-navne
  cands <- clean_trade_name_candidates(trade_name)
  
  for (nm in cands) {
    url <- janusmed_url_lakemedel(kind, nm)
    sc <- janusmed_score_from_url(url)
    if (!is.na(sc)) return(list(score = sc, url = url, method = paste0("lakemedel:", nm)))
  }
  
  # 2) Fallback: /substans/
  sub <- simplify_substance(generic_da)
  if (nzchar(sub)) {
    url2 <- janusmed_url_substans(kind, sub)
    sc2 <- janusmed_score_from_url(url2)
    if (!is.na(sc2)) return(list(score = sc2, url = url2, method = paste0("substans:", sub)))
  }
  
  list(score = NA_integer_, url = "", method = "no-hit")
}

# ------------------------------------------------------------
# LactMed via NBK501922 index + fuzzy match
# ------------------------------------------------------------

lactmed_index_url <- function() "https://www.ncbi.nlm.nih.gov/books/NBK501922/"

lactmed_search_url <- function(term) {
  q <- URLencode(paste(term, "LactMed"), reserved = TRUE)
  paste0("https://www.ncbi.nlm.nih.gov/books/?term=", q)
}

norm_lactmed_key <- function(x) {
  x <- clean_text(x)
  if (!nzchar(x)) return("")
  x <- tolower(x)
  
  if (requireNamespace("stringi", quietly = TRUE)) {
    x <- stringi::stri_trans_general(x, "Latin-ASCII")
  }
  
  x <- stringr::str_replace_all(x, "[^a-z0-9]", "")
  x <- stringr::str_replace_all(x, "ph", "f")
  x <- stringr::str_replace(x, "ate$", "at")
  
  x
}

lactmed_get_index_links <- function() {
  doc <- safe_read_html(lactmed_index_url())
  if (is.null(doc)) return(tibble(title = character(), url = character()))
  
  a <- doc %>% rvest::html_elements("a")
  titles <- a %>% rvest::html_text2() %>% clean_text()
  hrefs  <- a %>% rvest::html_attr("href") %>% as.character()
  
  df <- tibble(title = titles, href = hrefs) %>%
    filter(!is.na(href), href != "", title != "") %>%
    filter(
      stringr::str_detect(href, "^/books/NBK\\d+/?$") |
        stringr::str_detect(href, "^/books/n/lactmed/LM\\d+/?$")
    ) %>%
    mutate(url = paste0("https://www.ncbi.nlm.nih.gov", href)) %>%
    select(title, url) %>%
    distinct()
  
  df
}

lactmed_pick_best_link <- function(generic_da, links_df) {
  if (nrow(links_df) == 0) return(list(url = NA_character_, best_title = NA_character_, dist = NA_integer_))
  
  key <- norm_lactmed_key(generic_da)
  if (!nzchar(key)) return(list(url = NA_character_, best_title = NA_character_, dist = NA_integer_))
  
  keys <- vapply(links_df$title, norm_lactmed_key, character(1))
  
  hit <- which(keys == key)
  if (length(hit) > 0) {
    i <- hit[1]
    return(list(url = links_df$url[i], best_title = links_df$title[i], dist = 0L))
  }
  
  d <- adist(key, keys, ignore.case = TRUE)
  i <- which.min(d)
  
  if (is.infinite(d[i]) || d[i] > max(6, nchar(key) * 0.35)) {
    return(list(url = NA_character_, best_title = NA_character_, dist = as.integer(d[i])))
  }
  
  list(url = links_df$url[i], best_title = links_df$title[i], dist = as.integer(d[i]))
}

lactmed_extract_summary_from_doc <- function(doc) {
  if (is.null(doc)) return("")
  
  h <- doc %>%
    rvest::html_elements(xpath = "//*[self::h2 or self::h3][normalize-space()='Summary of Use during Lactation']")
  
  if (length(h) > 0) {
    sibs <- xml2::xml_find_all(h[[1]], "following-sibling::*")
    out <- character()
    
    for (node in sibs) {
      nm <- tolower(xml2::xml_name(node))
      if (nm %in% c("h2", "h3")) break
      if (nm == "p") {
        out <- c(out, clean_text(rvest::html_text2(xml2::read_html(as.character(node)))))
      }
    }
    
    out <- out[out != ""]
    if (length(out) > 0) return(paste(out, collapse = "\n"))
  }
  
  p <- doc %>%
    rvest::html_elements(xpath = "//*[self::div][contains(@id,'Summary_of_Use_during_Lactation')]//p")
  
  if (length(p) > 0) {
    txt <- p %>% rvest::html_text2() %>% clean_text()
    txt <- txt[txt != ""]
    if (length(txt) > 0) return(paste(txt, collapse = "\n"))
  }
  
  ""
}

# Global cache (OK på shiny-server; sparer mange requests)
lactmed_links_cache <- local({
  cache <- NULL
  function(force = FALSE) {
    if (is.null(cache) || force) cache <<- lactmed_get_index_links()
    cache
  }
})

lactmed_scrape_from_index <- function(generic_da) {
  term <- clean_text(generic_da)
  if (!nzchar(term)) return(list(text = "", url = ""))
  
  links <- lactmed_links_cache(FALSE)
  
  pick <- lactmed_pick_best_link(term, links)
  if (is.na(pick$url)) {
    return(list(text = "", url = lactmed_search_url(term)))
  }
  
  doc <- safe_read_html(pick$url)
  if (is.null(doc)) return(list(text = "", url = pick$url))
  
  txt <- lactmed_extract_summary_from_doc(doc)
  
  list(text = txt, url = pick$url)
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- secure_app(
  fluidPage(
  titlePanel("Graviditet & amning"),
  
  tags$head(
    tags$style(HTML("
    /* Scrollbar i præparatlisten, så sidebaren ikke eksploderer */
    #product_picker_wrap {
      max-height: 45vh;
      overflow-y: auto;
      padding-right: 8px;
      border: 1px solid #e5e5e5;
      border-radius: 6px;
      padding: 8px;
      background: #fafafa;
    }
    /* Gør sidebar knapper sticky øverst */
    .sidebar-sticky {
      position: sticky;
      top: 10px;
      z-index: 10;
      background: white;
      padding-bottom: 8px;
    }
  "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      div(class = "sidebar-sticky",
          tags$small("Indtast ATC-koder (én per linje). Tomme linjer og '-' ignoreres."),
          textAreaInput("atc_input", "ATC-koder:", rows = 10, placeholder = "Fx:\nB01AC22\nN06BA04"),
          
          actionButton("btn_fetch_products", "Hent præparater fra Pro.medicin.dk", width = "100%"),
          
          fluidRow(
            column(6, actionButton("btn_select_all", "Markér alle", width = "100%")),
            column(6, actionButton("btn_clear_all",  "Ryd alle",    width = "100%"))
          ),
          
          # Toggle-knap til præparatlisten
          actionButton("btn_toggle_picker", "Fold sammen: Vælg præparater", width = "100%")
      ),
      
      br(),
      
      # Collapsible container til selve præparatlisten
      conditionalPanel(
        condition = "input.btn_toggle_picker % 2 == 0",
        div(id = "product_picker_wrap",
            uiOutput("product_picker")
        ),
        br(),
        actionButton("btn_apply_selection", "Brug valgte præparater", width = "100%")
      ),
      
      hr(),
      
      fluidRow(
        column(6, actionButton("btn_pm_preg", "Pro.medicin – graviditet", width = "100%")),
        column(6, actionButton("btn_pm_lact", "Pro.medicin – amning",     width = "100%"))
      ),
      fluidRow(
        column(6, actionButton("btn_jm_foster", "Janusmed – fosterpåverkan (score)", width = "100%")),
        column(6, actionButton("btn_jm_lact",   "Janusmed – amning (score)",         width = "100%"))
      ),
      actionButton("btn_lactmed", "LactMed – Summary (amning)", width = "100%"),
      
      hr(),
      actionButton("btn_copy", "Kopiér indhold", width = "100%"),
      actionButton("btn_clear", "Ryd indhold", width = "100%"),
      
      hr(),
      tags$small("Flow: ATC → hent præparater → markér relevante præparater → brug → klik opslag.")
    ),
    
    mainPanel(
      DTOutput("out_table"),
      tags$hr(),
      tags$small("Der vises én række per valgt præparat. Handelsnavne og generiske navne hentes fra pro.medicin.dk.
                 Artikler på Janusmed.se fremsøges via handelsnavne, hvor alle dansk markedsførte handelsnavne afprøves indtil,
                 der er et hit. Artikler på LactMed® findes via det engelske generiske navn, 
                 der ligner det danske generiske navn mest (fuzzy matching)")
    )
  )
)
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {
  
  res_auth <- secure_server(check_credentials = check_credentials(credentials))
  
  atc_codes <- reactive({
    parse_atc_input(input$atc_input)
  })
  
  products_by_atc <- reactiveVal(tibble(ATC = character(), name = character(), url = character()))
  
  # Resultater: én række pr. valgt præparat
  results <- reactiveVal(
    tibble(
      ATC = character(),
      product_name = character(),
      product_url  = character(),
      generic_da   = character(),
      
      pm_preg_text = character(),
      pm_preg_url  = character(),
      pm_lact_text = character(),
      pm_lact_url  = character(),
      
      jm_foster_score = integer(),
      jm_foster_url   = character(),
      jm_lact_score   = integer(),
      jm_lact_url     = character(),
      
      lactmed_text = character(),
      lactmed_url  = character()
    )
  )
  
  observe({
    n <- (input$btn_toggle_picker %||% 0)
    lab <- if (n %% 2 == 0) {
      "Fold sammen: Vælg præparater"
    } else {
      "Fold ud: Vælg præparater"
    }
    updateActionButton(session, "btn_toggle_picker", label = lab)
  })
  
  observeEvent(input$btn_fetch_products, {
    atc <- atc_codes()
    if (length(atc) == 0) {
      showNotification("Indtast mindst én ATC-kode først.", type = "warning")
      return()
    }
    
    withProgress(message = "Henter præparater fra Pro.medicin.dk...", value = 0, {
      all <- tibble(ATC = character(), name = character(), url = character())
      
      for (i in seq_along(atc)) {
        incProgress(1 / length(atc), detail = atc[i])
        df <- promedicin_get_products_from_atc(atc[i])
        if (nrow(df) > 0) all <- bind_rows(all, mutate(df, ATC = atc[i]))
      }
      
      products_by_atc(all)
    })
  })
  
  output$product_picker <- renderUI({
    df <- products_by_atc()
    atc <- atc_codes()
    if (length(atc) == 0) return(NULL)
    
    if (nrow(df) == 0) {
      return(tags$em("Ingen præparater hentet endnu. Klik 'Hent præparater' først."))
    }
    
    tagList(
      tags$strong("Vælg præparater (handelsnavne) pr. ATC:"),
      lapply(atc, function(code) {
        d <- df %>% filter(ATC == code)
        if (nrow(d) == 0) {
          return(tags$div(tags$em(paste0(code, ": ingen præparater fundet."))))
        }
        
        checkboxGroupInput(
          inputId = paste0("pick_", code),
          label = paste0(code),
          choices = stats::setNames(d$url, d$name),
          selected = NULL
        )
      })
    )
  })
  
  observeEvent(input$btn_select_all, {
    df <- products_by_atc()
    atc <- atc_codes()
    if (nrow(df) == 0 || length(atc) == 0) return()
    
    for (code in atc) {
      d <- df %>% filter(ATC == code)
      if (nrow(d) == 0) next
      updateCheckboxGroupInput(session, paste0("pick_", code), selected = d$url)
    }
  })
  
  observeEvent(input$btn_clear_all, {
    atc <- atc_codes()
    if (length(atc) == 0) return()
    for (code in atc) {
      updateCheckboxGroupInput(session, paste0("pick_", code), selected = character(0))
    }
  })
  
  observeEvent(input$btn_apply_selection, {
    dfp <- products_by_atc()
    atc <- atc_codes()
    
    if (nrow(dfp) == 0 || length(atc) == 0) {
      showNotification("Hent præparater først.", type = "warning")
      return()
    }
    
    picked <- unlist(lapply(atc, function(code) input[[paste0("pick_", code)]]))
    picked <- unique(picked)
    picked <- picked[!is.na(picked) & picked != ""]
    
    rows <- dfp %>%
      filter(url %in% picked) %>%
      distinct(ATC, url, .keep_all = TRUE) %>%
      transmute(
        ATC = ATC,
        product_name = name,
        product_url  = url
      )
    
    if (nrow(rows) == 0) {
      results(results()[0, ])
      showNotification("Ingen præparater valgt.", type = "message")
      return()
    }
    
    withProgress(message = "Henter aktiv substans fra præparatsider...", value = 0, {
      generics <- character(nrow(rows))
      for (i in seq_len(nrow(rows))) {
        incProgress(1 / nrow(rows), detail = rows$product_name[i])
        generics[i] <- promedicin_get_active_substance_from_url(rows$product_url[i])
      }
      rows$generic_da <- clean_text(generics)
    })
    
    out <- rows %>%
      mutate(
        pm_preg_text = "", pm_preg_url = product_url,
        pm_lact_text = "", pm_lact_url = product_url,
        
        jm_foster_score = NA_integer_, jm_foster_url = "",
        jm_lact_score   = NA_integer_, jm_lact_url   = "",
        
        lactmed_text = "", lactmed_url = ""
      )
    
    results(out)
  })
  
  observeEvent(input$btn_pm_preg, {
    df <- results()
    if (nrow(df) == 0) return()
    
    withProgress(message = "Scraper Pro.medicin.dk (graviditet)...", value = 0, {
      for (i in seq_len(nrow(df))) {
        incProgress(1 / nrow(df), detail = df$product_name[i])
        res <- promedicin_scrape_from_product_url(df$product_url[i], what = "graviditet")
        df$pm_preg_text[i] <- res$text
        df$pm_preg_url[i]  <- res$url
      }
    })
    results(df)
  })
  
  observeEvent(input$btn_pm_lact, {
    df <- results()
    if (nrow(df) == 0) return()
    
    withProgress(message = "Scraper Pro.medicin.dk (amning)...", value = 0, {
      for (i in seq_len(nrow(df))) {
        incProgress(1 / nrow(df), detail = df$product_name[i])
        res <- promedicin_scrape_from_product_url(df$product_url[i], what = "amning")
        df$pm_lact_text[i] <- res$text
        df$pm_lact_url[i]  <- res$url
      }
    })
    results(df)
  })
  
  observeEvent(input$btn_jm_foster, {
    df <- results()
    if (nrow(df) == 0) return()
    
    pm_all <- products_by_atc()  # ✅ NYT: brug hele Pro.medicin-listen som fallback-pulje
    
    withProgress(message = "Henter Janusmed-score (fosterpåverkan) – prøver handelsnavne fra Pro.medicin...", value = 0, {
      for (i in seq_len(nrow(df))) {
        incProgress(1 / nrow(df), detail = df$product_name[i])
        
        res <- janusmed_try_score_loop_tradenames(
          kind = "foster",
          atc  = df$ATC[i],
          selected_trade = df$product_name[i],
          products_by_atc_df = pm_all
        )
        
        df$jm_foster_score[i] <- res$score
        df$jm_foster_url[i]   <- res$url
      }
    })
    
    results(df)
  })
  
  observeEvent(input$btn_jm_lact, {
    df <- results()
    if (nrow(df) == 0) return()
    
    pm_all <- products_by_atc()  # ✅ NYT
    
    withProgress(message = "Henter Janusmed-score (amning) – prøver handelsnavne fra Pro.medicin...", value = 0, {
      for (i in seq_len(nrow(df))) {
        incProgress(1 / nrow(df), detail = df$product_name[i])
        
        res <- janusmed_try_score_loop_tradenames(
          kind = "amning",
          atc  = df$ATC[i],
          selected_trade = df$product_name[i],
          products_by_atc_df = pm_all
        )
        
        df$jm_lact_score[i] <- res$score
        df$jm_lact_url[i]   <- res$url
      }
    })
    
    results(df)
  })
  
  observeEvent(input$btn_lactmed, {
    df <- results()
    if (nrow(df) == 0) return()
    
    withProgress(message = "Henter LactMed...", value = 0, {
      for (i in seq_len(nrow(df))) {
        incProgress(1 / nrow(df), detail = df$generic_da[i])
        res <- lactmed_scrape_from_index(df$generic_da[i])
        df$lactmed_text[i] <- res$text
        df$lactmed_url[i]  <- res$url
      }
    })
    
    results(df)
  })

                     
  build_copy_text <- function(df) {
    if (nrow(df) == 0) return("")
    blocks <- lapply(seq_len(nrow(df)), function(i) {
      lines <- c(
        paste0("ATC: ", df$ATC[i]),
        paste0("Præparat: ", df$product_name[i]),
        paste0("Pro.medicin præparat-link: ", df$product_url[i]),
        paste0("Aktiv substans (DA): ", df$generic_da[i]),
        "",
        "Pro.medicin – graviditet:",
        df$pm_preg_text[i],
        "",
        "Pro.medicin – amning:",
        df$pm_lact_text[i],
        "",
        "Janusmed – fosterpåverkan (score):",
        paste0("Score: ", ifelse(is.na(df$jm_foster_score[i]), "(ukendt)", df$jm_foster_score[i])),
        if (nzchar(df$jm_foster_url[i])) paste0("Link: ", df$jm_foster_url[i]) else "",
        "",
        "Janusmed – amning (score):",
        paste0("Score: ", ifelse(is.na(df$jm_lact_score[i]), "(ukendt)", df$jm_lact_score[i])),
        if (nzchar(df$jm_lact_url[i])) paste0("Link: ", df$jm_lact_url[i]) else "",
        "",
        "LactMed – Summary of Use during Lactation:",
        if (nzchar(df$lactmed_url[i])) paste0("Link: ", df$lactmed_url[i]) else "",
        df$lactmed_text[i]
      )
      paste(lines, collapse = "\n")
    })
    paste(blocks, collapse = "\n\n----------------------------------------\n\n")
  }
  
  observeEvent(input$btn_copy, {
    df <- results()
    txt <- build_copy_text(df)
    
    showModal(modalDialog(
      title = "Kopiér indhold",
      tags$textarea(
        id = "copy_area",
        style = "width: 100%; height: 300px;",
        txt
      ),
      footer = tagList(
        tags$button(
          class = "btn btn-primary",
          onclick = "var ta=document.getElementById('copy_area'); ta.select(); document.execCommand('copy');",
          "Kopiér til udklipsholder"
        ),
        modalButton("Luk")
      ),
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$btn_clear, {
    products_by_atc(tibble(ATC = character(), name = character(), url = character()))
    results(results()[0, ])
    updateTextAreaInput(session, "atc_input", value = "")
  })
  
  output$out_table <- renderDT({
    df <- results()
    
    if (nrow(df) == 0) {
      return(datatable(
        tibble(Besked = "Ingen data endnu. ATC → hent præparater → vælg → brug → klik opslag."),
        options = list(dom = "t")
      ))
    }
    
    df_view <- df %>%
      mutate(
        link_pm = paste0("<a href='", product_url, "' target='_blank'>Pro.medicin præparat</a>"),
        link_jm_f = ifelse(nzchar(jm_foster_url),
                           paste0("<a href='", jm_foster_url, "' target='_blank'>Janusmed fosterpåverkan</a>"),
                           ""),
        link_jm_a = ifelse(nzchar(jm_lact_url),
                           paste0("<a href='", jm_lact_url, "' target='_blank'>Janusmed amning</a>"),
                           ""),
        link_lm = ifelse(nzchar(lactmed_url),
                         paste0("<a href='", lactmed_url, "' target='_blank'>LactMed</a>"),
                         ""),
        Links = paste(link_pm, link_jm_f, link_jm_a, link_lm, sep = " | ")
      ) %>%
      transmute(
        ATC,
        `Præparat` = product_name,
        `Aktiv substans (DA)` = generic_da,
        `Pro.medicin – graviditet` = pm_preg_text,
        `Pro.medicin – amning`     = pm_lact_text,
        `Janusmed – fosterpåverkan (score)` = jm_foster_score,
        `Janusmed – amning (score)`         = jm_lact_score,
        `LactMed – amning` = lactmed_text,
        Links = Links
      )
    
    tbl <- datatable(
      df_view,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
    
    score_cols <- c("Janusmed – fosterpåverkan (score)", "Janusmed – amning (score)")
    
    tbl %>%
      formatStyle(
        score_cols,
        backgroundColor = styleEqual(c(1, 2, 3), c("#1f7a1f", "#ffeb3b", "#d32f2f")),
        color          = styleEqual(c(1, 2, 3), c("white", "black", "white")),
        fontWeight = "bold"
      )
  })
}

shinyApp(ui, server)
