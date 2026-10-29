# app.R
# Shiny-app: Power BI Medicinsalg – CSV-eksport og tabel/Excel
#
# Formål:
# 1) Generér JavaScript-kode, som kan køres i Chrome DevTools Console på den åbne Power BI-rapport.
# 2) Browserkoden bruger Authorization lokalt, afkoder Power BI-svar og downloader CSV.
# 3) Appen importerer CSV til en bred tabel/Excel; rå Power BI-JSON parses ikke i R.
#
# Pakker:
# install.packages(c(
#   "shiny", "bslib", "jsonlite", "dplyr", "tidyr", "stringr",
#   "purrr", "readr", "DT", "writexl"
# ))

library(shiny)
library(bslib)
library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(readr)
library(DT)
library(writexl)

# Tillad flere CSV-dele og den tilhørende eksportoversigt.
options(shiny.maxRequestSize = 200 * 1024^2)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

normalise_atc_input <- function(x) {
  if (is.null(x) || !nzchar(x)) return(character(0))
  tokens <- unique(str_trim(unlist(str_split(x, "[\\r\\n,;]+"), use.names = FALSE)))
  tokens <- tokens[tokens != ""]
  valid <- str_detect(str_to_upper(tokens), "^[A-Z][0-9]{2}[A-Z]{1,2}[0-9]{0,4}(\\s*\\(.+\\))?$")
  if (any(!valid)) stop("Ugyldig ATC-værdi: ", paste(tokens[!valid], collapse = ", "))
  vapply(tokens, function(token) {
    code <- str_extract(token, "^[A-Za-z][0-9]{2}[A-Za-z]{1,2}[0-9]{0,4}")
    paste0(toupper(code), substring(token, nchar(code) + 1L))
  }, character(1), USE.NAMES = FALSE)
}

clean_atc_code_for_mapping <- function(x) {
  x |>
    str_trim() |>
    str_extract("^[A-Za-z][0-9]{2}[A-Za-z]{1,2}[0-9]{0,4}") |>
    toupper()
}

read_mapping_file <- function(path, filename = path) {
  if (is.null(path) || !file.exists(path)) return(NULL)
  
  ext <- tolower(tools::file_ext(filename))
  
  dat <- switch(
    ext,
    "csv" = readr::read_csv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    "txt" = readr::read_delim(path, delim = ";", show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    "tsv" = readr::read_tsv(path, show_col_types = FALSE, locale = locale(encoding = "UTF-8")),
    stop("Mapping-filen skal være csv, txt eller tsv.")
  )
  
  nms <- names(dat)
  nms_low <- tolower(nms)
  
  atc_col <- nms[match(TRUE, nms_low %in% c("atc", "atc_kode", "atckode", "atc_code"))]
  pbi_col <- nms[match(TRUE, nms_low %in% c("atc5_kode_tekst", "powerbi_atc", "powerbi", "atc5", "atc5tekst"))]
  
  if (is.na(atc_col) || is.na(pbi_col)) {
    stop("Mapping-filen skal indeholde kolonnerne ATC og ATC5_Kode_Tekst.")
  }
  
  dat |>
    transmute(
      ATC = toupper(str_trim(as.character(.data[[atc_col]]))),
      ATC5_Kode_Tekst = str_trim(as.character(.data[[pbi_col]]))
    ) |>
    filter(!is.na(ATC), ATC != "", !is.na(ATC5_Kode_Tekst), ATC5_Kode_Tekst != "") |>
    distinct()
}

apply_atc_mapping <- function(atc_values, mapping = NULL) {
  if (length(atc_values) == 0) return(character(0))
  
  already_powerbi <- str_detect(atc_values, "\\(.+\\)")
  raw_codes <- clean_atc_code_for_mapping(atc_values)
  
  if (is.null(mapping) || nrow(mapping) == 0) {
    return(unique(ifelse(already_powerbi, atc_values, raw_codes)))
  }
  
  mapped <- tibble(
    input = atc_values,
    ATC = raw_codes,
    already_powerbi = already_powerbi
  ) |>
    left_join(mapping, by = "ATC") |>
    mutate(
      powerbi_atc = case_when(
        already_powerbi ~ input,
        !is.na(ATC5_Kode_Tekst) & ATC5_Kode_Tekst != "" ~ ATC5_Kode_Tekst,
        TRUE ~ ATC
      )
    ) |>
    pull(powerbi_atc)
  
  unique(mapped[!is.na(mapped) & mapped != ""])
}

js_string_array <- function(x) {
  jsonlite::toJSON(as.character(x), auto_unbox = FALSE)
}

js_numeric_array <- function(x) {
  paste0("[", paste(as.integer(x), collapse = ", "), "]")
}

generate_js <- function(atc_powerbi, metrics, regions, years, split_by_year = TRUE, output_level = "atc5") {
  output_level <- match.arg(output_level, c("atc5", "varenummer"))
  template <- paste(readLines("export_template.js", encoding = "UTF-8", warn = FALSE), collapse = "\n")
  replacements <- list(
    "__ATC_JS__" = js_string_array(atc_powerbi),
    "__METRICS_JS__" = js_string_array(metrics),
    "__REGIONS_JS__" = js_string_array(regions),
    "__YEARS_JS__" = js_numeric_array(years),
    "__SPLIT_JS__" = if (isTRUE(split_by_year)) "true" else "false",
    "__OUTPUT_LEVEL_JS__" = jsonlite::toJSON(output_level, auto_unbox = TRUE)
  )
  for (key in names(replacements)) template <- gsub(key, replacements[[key]], template, fixed = TRUE)
  template
}

metric_map <- c(
  "Mængdeforbrug" = "Antal DDD",
  "Regionalt tilskud" = "Tilskudsbeløb - Regionalt",
  "Omsætning" = "Ekspeditionsbeløb"
)
metric_cols <- c(unname(metric_map), paste0(unname(metric_map), " pr. 1.000 borgere"))
month_names_da <- c("Januar", "Februar", "Marts", "April", "Maj", "Juni", "Juli", "August", "September", "Oktober", "November", "December")

normalise_year_month_columns <- function(dat) {
  month_raw <- str_trim(as.character(dat$`Måned`))
  year_raw <- str_trim(as.character(dat$`År`))
  embedded_year <- str_extract(month_raw, "[12][0-9]{3}$")
  if (any(!is.na(embedded_year) & embedded_year != year_raw, na.rm = TRUE)) {
    stop("År og årstallet i Måned stemmer ikke overens.")
  }
  month_text <- str_to_lower(str_trim(str_remove(month_raw, "\\s+[12][0-9]{3}$")))
  month_no <- match(month_text, str_to_lower(month_names_da))
  short_no <- match(month_text, c("jan", "feb", "mar", "apr", "maj", "jun", "jul", "aug", "sep", "okt", "nov", "dec"))
  month_no <- coalesce(month_no, short_no)
  if (any(is.na(month_no)) || any(is.na(year_raw) | !str_detect(year_raw, "^[12][0-9]{3}$"))) {
    stop("Ukendt år/måned. Brug et nyt CSV-udtræk med læsbare månedsnavne; rå ordbogsindeks kan ikke tolkes sikkert.")
  }
  dat$`År` <- as.integer(year_raw)
  dat$`Måned` <- month_names_da[month_no]
  dat
}

read_export_csv <- function(path) {
  # Bevar identifikatorer, herunder foranstillede nuller i varenumre.
  dat <- readr::read_delim(path, delim = ";", col_types = cols(.default = col_character()),
    locale = locale(encoding = "UTF-8"), na = "", trim_ws = FALSE, show_col_types = FALSE)
  if (nrow(readr::problems(dat))) stop("CSV-filen har ugyldige felter eller forskudte kolonner.")
  dat
}

finalise_parsed_long_export <- function(long2) {
  required <- c("ATC, Niveau 5, kode & tekst", "År", "Måned", "Ydertype", "Bopælsregion", "nøgletal_output", "value")
  missing <- setdiff(required, names(long2))
  if (length(missing)) stop("CSV mangler kolonner: ", paste(missing, collapse = ", "))
  if (!nrow(long2)) return(tibble())

  if ("status" %in% names(long2) && any(is.na(long2$status) | as.character(long2$status) != "200")) stop("CSV indeholder mislykkede eller ukendte HTTP-kald.")
  if ("export_error" %in% names(long2) && any(!is.na(long2$export_error) & nzchar(str_trim(long2$export_error)))) stop("CSV indeholder eksportfejl.")
  if ("hasRestartToken" %in% names(long2) && any(tolower(as.character(long2$hasRestartToken)) %in% c("true", "1"))) stop("CSV indeholder afkortede svar (RestartToken).")
  if ("rowCountMarker" %in% names(long2) && any(suppressWarnings(as.numeric(long2$rowCountMarker)) >= 30000, na.rm = TRUE)) stop("CSV har nået rækkegrænsen. Hent et mindre deludtræk.")

  if (!is.numeric(long2$value)) {
    raw_value <- str_trim(as.character(long2$value))
    converted <- suppressWarnings(readr::parse_double(raw_value, locale = locale(decimal_mark = ","), na = c("", "NA")))
    invalid <- !is.na(raw_value) & !raw_value %in% c("", "NA") & (is.na(converted) | !is.finite(converted))
    if (any(invalid)) stop("Ugyldig numerisk værdi i CSV. Forventet decimalkomma uden tusindtalsseparator.")
    long2$value <- converted
  }
  if (any(!is.na(long2$value) & !is.finite(long2$value))) stop("CSV indeholder ikke-endelige tal.")
  long2 <- normalise_year_month_columns(long2)
  base_keys <- c("ATC, Niveau 5, kode & tekst", "År", "Måned", "Ydertype", "Bopælsregion")
  product_keys <- c("Varenummer", "Navn (Præparat)", "Form", "Styrke", "Pakningsstørrelse")
  for (nm in product_keys) {
    if (!nm %in% names(long2)) long2[[nm]] <- NA_character_
    long2[[nm]] <- na_if(str_trim(as.character(long2[[nm]])), "")
  }
  if (!"opgoerelsesniveau" %in% names(long2)) long2$opgoerelsesniveau <- NA_character_
  levels <- unique(na.omit(na_if(long2$opgoerelsesniveau, "")))
  if (length(levels) > 1 || any(!levels %in% c("atc5", "varenummer"))) stop("Upload kun ét opgørelsesniveau ad gangen.")
  has_product <- any(!is.na(long2$Varenummer))
  is_product <- identical(levels, "varenummer") || has_product
  if (identical(levels, "atc5") && has_product) stop("ATC5-udtrækket indeholder uventede produktfelter.")
  if (is_product) {
    if (any(!str_detect(coalesce(long2$Varenummer, ""), "^\\d{6}$")) || any(is.na(as.matrix(long2[product_keys])))) stop("Ufuldstændige produktfelter. Ingen rækker er fjernet automatisk.")
  }
  keys <- if (is_product) c(base_keys[1], product_keys, base_keys[-1]) else base_keys
  if (any(is.na(as.matrix(long2[base_keys]))) || any(str_trim(as.character(unlist(long2[base_keys]))) == "")) stop("En eller flere datanøgler mangler.")
  if (any(str_detect(long2$Ydertype, "^\\d+$")) || any(!str_detect(long2[[base_keys[1]]], "^[A-Z][0-9]{2}[A-Z]{2}[0-9]{2}"))) stop("CSV indeholder uafkodede ATC-/ydertypeværdier. Hent data med den nye generator.")
  if (any(is.na(long2$nøgletal_output) | !long2$nøgletal_output %in% metric_cols)) stop("CSV indeholder ukendte nøgletal.")
  if (anyDuplicated(long2[c(keys, "nøgletal_output")])) stop("Overlappende datarækker: upload ikke samme udtræk eller overlappende CSV-filer flere gange. Pr. 1.000-værdier må ikke summeres.")

  # Hver datanøgle har højst én værdi per nøgletal. Manglende værdier forbliver NA.
  out <- long2 |>
    select(all_of(c(keys, "nøgletal_output", "value"))) |>
    pivot_wider(names_from = nøgletal_output, values_from = value)
  for (nm in metric_cols) if (!nm %in% names(out)) out[[nm]] <- NA_real_
  out |>
    mutate(.month_order = match(`Måned`, month_names_da)) |>
    arrange(.data[[base_keys[1]]], `År`, .month_order, across(all_of(setdiff(keys, c(base_keys[1], "År", "Måned"))))) |>
    select(-.month_order) |>
    relocate(all_of(keys), all_of(metric_cols))
}

csv_metadata <- function(dat) {
  mapping <- c(atc = "atc_filter", region = "region_filter", year = "year_filter", opgoerelse = "opgoerelse", normalisering = "normalisering", opgoerelsesniveau = "opgoerelsesniveau", status = "status", rowCountMarker = "rowCountMarker", hasRestartToken = "hasRestartToken", error = "export_error")
  for (nm in unname(mapping)) if (!nm %in% names(dat)) dat[[nm]] <- NA_character_
  out <- dat |> select(all_of(unname(mapping))) |> distinct()
  names(out) <- names(mapping)
  out
}

read_export_summary <- function(path) {
  x <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  if (!identical(as.integer(x$version), 2L) || !isTRUE(x$complete)) stop("Oversigten er ikke fra et fuldført udtræk med den nye generator.")
  requests <- x$requests %||% list()
  if (!length(x$exportId) || length(requests) != x$expectedQueries || x$completedQueries != x$expectedQueries) stop("Oversigten har manglende forespørgsler.")
  meta <- map_dfr(requests, function(r) tibble(
    atc = as.character(r$atc), region = as.character(r$region), year = paste(unlist(r$year), collapse = ", "),
    opgoerelse = as.character(r$opgoerelse), normalisering = as.character(r$normalisering),
    opgoerelsesniveau = as.character(r$opgoerelsesniveau), status = as.integer(r$status),
    rowCountMarker = as.integer(r$rowCountMarker), hasRestartToken = isTRUE(r$hasRestartToken),
    parsedRows = as.integer(r$parsedRows), error = r$error %||% NA_character_
  ))
  if (nrow(meta) != x$expectedQueries || any(is.na(meta$status) | meta$status != 200 | meta$hasRestartToken | !is.na(meta$error)) || any(is.na(meta$parsedRows) | meta$parsedRows < 0)) stop("Oversigten indeholder fejl eller manglende metadata.")
  expected_rows <- sum(meta$parsedRows)
  file_rows <- sum(vapply(x$files %||% list(), function(f) as.numeric(f$rows), numeric(1)))
  if (expected_rows != file_rows) stop("Rækkeantal i oversigten stemmer ikke overens.")
  list(export_id = x$exportId, rows = expected_rows, meta = meta)
}

validate_export_summaries <- function(summaries, long) {
  if (!length(summaries)) return(invisible(NULL))
  ids <- vapply(summaries, function(s) as.character(s$export_id), character(1))
  if (anyDuplicated(ids)) stop("Samme eksportoversigt er uploadet flere gange.")
  if (nrow(long)) {
    if (!"export_id" %in% names(long) || any(is.na(long$export_id) | !long$export_id %in% ids)) stop("CSV og oversigt hører ikke sammen, eller der mangler en oversigt. CSV fra den tidligere test kan importeres uden oversigt.")
  }
  for (s in summaries) {
    found <- if (nrow(long)) sum(long$export_id == s$export_id) else 0L
    if (found != s$rows) stop("Der mangler CSV-dele, eller der er ekstra rækker for udtræk ", s$export_id, ".")
  }
  invisible(NULL)
}

pantone_287 <- "#0033A0"
pantone_289 <- "#0C2340"

ui <- page_sidebar(
  title = "Power BI Medicinsalg: udtræk og Excel",
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = pantone_289,
    primary = pantone_287,
    secondary = pantone_289,
    base_font = font_google("Inter"),
    heading_font = font_google("Inter")
  ),
  
  tags$head(
    tags$style(HTML(paste0("
      :root { --p287: ", pantone_287, "; --p289: ", pantone_289, "; }
      body { background-color: #FFFFFF; }
      .bslib-sidebar-layout > .sidebar { border-right: 4px solid var(--p287); }
      .card { border: 1px solid rgba(12, 35, 64, 0.14); box-shadow: 0 0.25rem 0.9rem rgba(12, 35, 64, 0.08); }
      .card-header { background: var(--p289); color: #FFFFFF; font-weight: 700; }
      .btn-primary, .btn-default { background-color: var(--p287); border-color: var(--p287); color: #FFFFFF; }
      .btn-primary:hover, .btn-default:hover { background-color: var(--p289); border-color: var(--p289); color: #FFFFFF; }
      .btn-outline-secondary { color: var(--p289); border-color: var(--p289); background-color: #FFFFFF; }
      .btn-outline-secondary:hover { background-color: var(--p289); border-color: var(--p289); color: #FFFFFF; }
      .card-header #copy_js_btn { color: #FFFFFF !important; border-color: #FFFFFF !important; background-color: transparent !important; }
      .card-header #copy_js_btn:hover, .card-header #copy_js_btn:focus { color: #FFFFFF !important; border-color: #FFFFFF !important; background-color: var(--p287) !important; }
      h5, label, .control-label { color: var(--p289); }
      textarea, .form-control { border-color: rgba(0, 51, 160, 0.35); }
      code { color: var(--p287); }
    ")) )
  ),
  
  sidebar = sidebar(
    width = 390,
    
    h5("1. Dataudtræk"),
    
    textAreaInput(
      "atc",
      "ATC-koder eller Power BI ATC5-værdier",
      value = paste("C09CA01", "N05CJ03", "R05DB29", sep = "\n"),
      rows = 7,
      placeholder = "Én ATC-kode per linje, fx:\nA02BC02\nB01AF01\nN06BA04\n\nEller fuld Power BI-værdi:\nA02BC02 (Pantoprazol)"
    ),
    
    radioButtons(
      "output_level",
      "Opgørelsesniveau",
      choices = c(
        "ATC5-niveau" = "atc5",
        "Varenummerniveau" = "varenummer"
      ),
      selected = "atc5"
    ),
    
    fileInput(
      "mapping_file",
      "Valgfri mapping-tabel: ATC → ATC5_Kode_Tekst",
      accept = c(".csv", ".txt", ".tsv")
    ),
    
    div(
      class = "alert alert-info py-2",
      strong("Nøgletal: "),
      "Alle tre nøgletal medtages altid — både som absolutte værdier og pr. 1.000 borgere."
    ),
    
    checkboxGroupInput(
      "regions",
      "Region",
      choices = c(
        "Region Hovedstaden",
        "Region Sjælland",
        "Region Syddanmark",
        "Region Midtjylland",
        "Region Nordjylland"
      ),
      selected = c(
        "Region Hovedstaden",
        "Region Sjælland",
        "Region Syddanmark",
        "Region Midtjylland",
        "Region Nordjylland"
      )
    ),
    
    sliderInput(
      "years_range",
      "Periode (år)",
      min = 2020,
      max = as.integer(format(Sys.Date(), "%Y")),
      value = c(max(2023, as.integer(format(Sys.Date(), "%Y")) - 3), as.integer(format(Sys.Date(), "%Y"))),
      step = 1,
      sep = "",
      ticks = TRUE
    ),
    
    hr(),
    
    h5("2. Importér CSV til tabel og Excel"),
    
    fileInput(
      "parsed_csv_files",
      "Upload alle CSV-dele fra udtrækket (kan vælge flere)",
      accept = c(".csv"),
      multiple = TRUE
    ),
    
    fileInput("summary_files", "Valgfrit: eksportoversigt (*_summary.json)", accept = c(".json"), multiple = TRUE),
    
    actionButton("import_btn", "Importér CSV", class = "btn-primary"),
    
    hr(),
    
    downloadButton("download_js", "Download JavaScript"),
    downloadButton("download_csv", "Download CSV"),
    downloadButton("download_xlsx", "Download Excel")
  ),
  
  layout_columns(
    col_widths = c(12),
    
    card(
      card_header("Sådan bruges generatoren"),
      div(
        tags$ol(
          tags$li(tags$a(href = "https://sundhedsdatabank.dk/medicin/medicinsalg-og-udgifter-menu/medicinsalg", target = "_blank", rel = "noopener", "Åbn rapporten Medicinsalg i Sundhedsdatabanken.")),
          tags$li("Tryk F12 → Network, filtrér på querydata, og ændr ét filter i rapporten. Vælg et kald med status 200."),
          tags$li("Under Headers → Request Headers kopierer du hele Authorization-værdien, inklusive præfikset."),
          tags$li("Vælg Console og app.powerbi.com i frame-menuen øverst (ofte står der top)."),
          tags$li("Kopiér koden nedenfor til Console eller en lokal teksteditor. Erstat INDSÆT_AUTHORIZATION_HER med den kopierede værdi, og kør hele koden. Indsæt ikke værdien i denne Shiny-app."),
          tags$li("Vent til Console skriver Færdig. Tillad flere downloads, hvis browseren spørger. CSV-filerne er allerede afkodet og kan bruges direkte."),
          tags$li("Upload eventuelt alle CSV-dele og eksportoversigten her, og klik Importér CSV for at samle nøgletallene i én tabel og hente Excel.")
        ),
        p("Authorization bruges kun i din browser. Ved 401/403 kopierer du en ny værdi fra et fungerende kald og starter igen. Stop et udtræk med ", tags$code("medicinsalgExport.stop()"), "."),
        p("Koden stopper ved fejl eller tegn på afkortning. Ingen CSV downloades før alle deludtræk er færdige. Oversigten viser også tomme deludtræk; tomme svar omdannes ikke til nul."),
        p("Varenummerniveau er bevaret, men er endnu ikke kontrolleret mod et levende svar efter flytningen. Test et enkelt lægemiddel, år og region først.")
      )
    ),
    
    card(
      card_header(
        div(
          "Genereret JavaScript",
          tags$button(
            id = "copy_js_btn",
            type = "button",
            class = "btn btn-sm btn-outline-secondary float-end",
            "Kopiér kode"
          )
        )
      ),
      tags$textarea(
        id = "js_code_area",
        style = "width:100%; min-height:430px; font-family: ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', monospace; font-size: 12px;",
        readonly = NA
      ),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('set_js_code', function(message) {
          const area = document.getElementById('js_code_area');
          if (area) area.value = message;
        });

        document.addEventListener('DOMContentLoaded', function() {
          const btn = document.getElementById('copy_js_btn');
          btn.addEventListener('click', async function() {
            const txt = document.getElementById('js_code_area').value;
            try {
              await navigator.clipboard.writeText(txt);
              btn.innerText = 'Kopieret!';
              setTimeout(() => btn.innerText = 'Kopiér kode', 1200);
            } catch(e) {
              const area = document.getElementById('js_code_area');
              area.select();
              document.execCommand('copy');
              btn.innerText = 'Kopieret!';
              setTimeout(() => btn.innerText = 'Kopiér kode', 1200);
            }
          });
        });
      "))
    ),
    
    uiOutput("validation_message"),
    uiOutput("import_status"),
    uiOutput("import_notice"),
    
    card(
      card_header("Oversigt over deludtræk"),
      DTOutput("metadata_table")
    ),
    
    card(
      card_header("Data til CSV og Excel"),
      DTOutput("parsed_table")
    )
  )
)

server <- function(input, output, session) {
  mapping_data <- reactive({
    req(input$mapping_file)
    # Shiny datapath har ikke nødvendigvis filtypen fra det uploadede filnavn.
    read_mapping_file(input$mapping_file$datapath, input$mapping_file$name)
  })
  selected_years <- reactive({
    rng <- suppressWarnings(as.integer(input$years_range))
    rng <- rng[!is.na(rng)]
    if (length(rng) < 2) return(integer(0))
    seq.int(min(rng), max(rng))
  })
  atc_powerbi <- reactive({
    apply_atc_mapping(normalise_atc_input(input$atc), if (!is.null(input$mapping_file)) mapping_data() else NULL)
  })
  generated_js <- reactive({
    shiny::validate(
      shiny::need(length(atc_powerbi()) > 0, "Indtast mindst én ATC-kode."),
      shiny::need(length(input$regions) > 0, "Vælg mindst én region."),
      shiny::need(length(selected_years()) > 0, "Vælg mindst ét år.")
    )
    generate_js(atc_powerbi(), names(metric_map), input$regions, selected_years(), TRUE, input$output_level %||% "atc5")
  })
  output$validation_message <- renderUI({
    div(class = "alert alert-secondary",
      strong("ATC-værdier sendt til Power BI-filteret: "), tags$code(paste(atc_powerbi(), collapse = "; ")), br(),
      strong("Opgørelsesniveau: "), if ((input$output_level %||% "atc5") == "varenummer") "Varenummerniveau (kontrollér første udtræk mod rapporten)" else "ATC5-niveau")
  })
  observe({ session$sendCustomMessage("set_js_code", generated_js()) })
  output$download_js <- downloadHandler(
    filename = function() paste0("powerbi_medicinsalg_export_", format(Sys.Date(), "%Y%m%d"), ".js"),
    content = function(file) writeLines(generated_js(), file, useBytes = TRUE)
  )
  metadata_val <- reactiveVal(tibble())
  parsed_data_val <- reactiveVal(tibble())
  import_status_val <- reactiveVal(NULL)
  import_notice_val <- reactiveVal(NULL)

  observeEvent(input$import_btn, {
    has_csv <- !is.null(input$parsed_csv_files) && nrow(input$parsed_csv_files) > 0
    has_summary <- !is.null(input$summary_files) && nrow(input$summary_files) > 0
    req(has_csv || has_summary)
    metadata_val(tibble()); parsed_data_val(tibble()); import_status_val(NULL); import_notice_val(NULL)
    tryCatch({
      long <- if (has_csv) map_dfr(input$parsed_csv_files$datapath, read_export_csv) else tibble()
      summaries <- if (has_summary) map(input$summary_files$datapath, read_export_summary) else list()
      validate_export_summaries(summaries, long)
      parsed <- if (has_csv) finalise_parsed_long_export(long) else tibble()
      meta <- if (has_summary) bind_rows(map(summaries, "meta")) else csv_metadata(long)
      metadata_val(meta); parsed_data_val(parsed)
      import_status_val(list(ok = TRUE, message = paste0("Importeret: ", nrow(long), " datarækker fordelt på ", nrow(parsed), " tabelrækker. Manglende værdier og nulværdier er bevaret hver for sig.")))
      notice <- character()
      if (!has_summary) notice <- c(notice, "Ingen eksportoversigt uploadet: CSV'en viser kun de kald, der returnerede data. Tomme udtræk og manglende CSV-dele kan derfor ikke kontrolleres.")
      if (has_summary && any(meta$parsedRows == 0)) notice <- c(notice, paste0(sum(meta$parsedRows == 0), " deludtræk gav ingen rækker. Berørte ATC-valg: ", paste(unique(meta$atc[meta$parsedRows == 0]), collapse = ", "), ". Ingen rækker er ikke det samme som et forbrug på nul."))
      if (nrow(parsed)) {
        month_no <- match(parsed$`Måned`, month_names_da)
        dates <- as.Date(sprintf("%04d-%02d-01", parsed$`År`, month_no))
        notice <- c(notice, paste0("Observeret periode i filerne: ", format(min(dates), "%m/%Y"), "–", format(max(dates), "%m/%Y"), ". Dækningen kan variere mellem lægemidler og regioner."))
        absent <- metric_cols[colSums(!is.na(parsed[metric_cols])) == 0]
        if (length(absent)) notice <- c(notice, paste0("Ingen værdier for: ", paste(absent, collapse = "; "), "."))
      }
      import_notice_val(notice)
    }, error = function(e) {
      metadata_val(tibble()); parsed_data_val(tibble())
      import_status_val(list(ok = FALSE, message = paste("Importfejl:", e$message)))
    })
  })
  output$import_status <- renderUI({
    s <- import_status_val(); if (is.null(s)) return(NULL)
    div(class = if (isTRUE(s$ok)) "alert alert-success" else "alert alert-danger", s$message)
  })
  output$import_notice <- renderUI({
    notices <- import_notice_val(); if (!length(notices)) return(NULL)
    div(class = "alert alert-info", lapply(notices, tags$p))
  })
  output$metadata_table <- renderDT({
    req(nrow(metadata_val()) > 0)
    datatable(metadata_val(), options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$parsed_table <- renderDT({
    req(nrow(parsed_data_val()) > 0)
    datatable(parsed_data_val(), options = list(pageLength = 25, scrollX = TRUE, order = list()), rownames = FALSE)
  })
  output$download_csv <- downloadHandler(
    filename = function() paste0("medicinsalg_", format(Sys.Date(), "%Y%m%d"), ".csv"),
    content = function(file) {
      dat <- parsed_data_val(); if (!nrow(dat)) stop("Importér CSV-data først.")
      readr::write_excel_csv2(dat, file, na = "")
    }
  )
  output$download_xlsx <- downloadHandler(
    filename = function() paste0("medicinsalg_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      dat <- parsed_data_val(); if (!nrow(dat)) stop("Importér CSV-data først.")
      writexl::write_xlsx(list("Medicinsalg" = dat, "Udtræk" = metadata_val()), path = file)
    }
  )
}

shinyApp(ui, server)
