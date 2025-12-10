library(shiny)
library(here)
library(shinyjs)
library(lubridate)
library(grid)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* Layout */
      html, body {
        height: 100%;
        margin: 0;
      }
      body {
        background-color: white;
        color: #0033A0;
        font-family: Arial, sans-serif;
        text-align: center;
        padding: 40px 20px 30px;
        display: flex;
        flex-direction: column;
        min-height: 100vh;
      }
      .content {
        flex: 1;
        max-width: 1200px;
        margin: 0 auto;
      }
      .header {
        font-size: 3em;
        margin-bottom: 0.3em;
      }
      .subheader {
        font-size: 1.5em;
        margin-top: 1.0em;
        margin-bottom: 0.4em;
        color: #555;
      }
      .links {
        margin: 0.3em 0;
      }
      .link-button {
        display: inline-block;
        margin: 0.3em;
        padding: 0.8em 1.8em;
        font-size: 1.1em;
        color: white;
        background-color: #0033A0;
        border-radius: 6px;
        text-decoration: none;
        border: none;
        cursor: pointer;
      }
      .link-button:hover {
        opacity: 0.9;
      }
      .footer {
        margin-top: auto;
        padding: 15px 0;
        font-size: 0.9em;
        color: #555;
      }

      /* Status box (only for errors now) */
      .status-message {
        margin-top: 0.2em;
        padding: 0.4em 0.7em;
        border-radius: 4px;
        font-weight: bold;
        text-align: left;
        display: inline-block;
        max-width: 900px;
        font-size: 0.9em;
      }
      .status-error {
        background-color: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }

      /* Console styling */
      .console-output {
        background-color: #2b2b2b;
        color: #f8f8f8;
        font-family: 'Courier New', monospace;
        text-align: left;
        padding: 12px;
        margin: 8px auto 15px auto;
        border-radius: 4px;

        min-height: 150px;
        max-height: 50vh;
        width: 100%;
        max-width: 1100px;

        overflow-y: auto;
        border: 1px solid #555;

        white-space: pre-wrap;
        word-wrap: break-word;
      }

      details {
        margin-top: 5px;
      }
      details > summary {
        cursor: pointer;
        font-weight: bold;
        color: #0033A0;
        list-style: none;
      }
      details > summary::-webkit-details-marker {
        display: none;
      }

      .last-sync-text {
        font-size: 0.9em;
        color: #444;
        margin-top: 0.2em;
        display: inline-block;
        padding: 0.2em 0.5em;
        border-radius: 4px;
      }
      .last-sync-ok {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
        font-weight: 600;
      }

      .section-separator {
        border-top: 1px solid #cccccc;
        margin: 8px auto;
        width: 85%;
      }

      /* Dedicated separator just above KFE/KFA section */
      .section-separator-above-kfe {
        border-top: 1px solid #cccccc;
        width: 85%;
        margin: 8px auto 2px auto;  /* adjust 8 for more/less vertical space */
        position: relative;
        z-index: 2;
        clear: both;
      }

      .section-separator-thin {
        border-top: 1px solid #dddddd;
        margin: 4px auto 6px auto;
        width: 80%;
      }

      /* === Plot + download tight spacing === */
      .plot-container {
        margin: 0 !important;
        padding: 0 !important;
      }
      .plot-container .shiny-plot-output,
      .plot-container .shiny-plot-output > div,
      .plot-container .shiny-plot-output > svg {
        margin: 0 !important;
        padding: 0 !important;
        border: none !important;
      }

      /* Separate download button spacing for the two plots */
      .download-links-activity {
        margin: -0px 0 0 0 !important;  /* tight under Aktivitetsstatus plot */
        padding: 0 !important;
      }
      .download-links-svartype {
        margin: -130px 0 0 0 !important;   /* less tight under Tidligere aktiviteter plot */
        padding: 0 !important;
      }

      /* Small button styling */
      .small-button {
        padding: 0.35em 0.8em !important;
        font-size: 0.8em !important;
        margin: 0.05em !important;
      }

      .small-download-button {
        padding: 0.3em 0.7em !important;
        font-size: 0.75em !important;
        margin: 0.05em !important;
      }

      /* KFE/KFA section styling: very tight vertical spacing */
      .kfe-kfa-section {
        margin: 4px 0 6px;
      }
      .kfe-kfa-title {
        font-weight: bold;
        color: #0033A0;
        margin-bottom: 0px;
        font-size: 1.1em;
      }
      .kfe-kfa-subtitle {
        font-size: 0.8em;
        color: #000000;
        margin-top: 0px;
        margin-bottom: 3px;
      }
      .kfe-kfa-buttons {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 4px;
        flex-wrap: wrap;
        margin-top: 1px;
        margin-bottom: 1px;
      }

      /* Dropdown container: dropdown + Download on same line, minimal spacing */
      .dropdown-container {
        display: flex;
        flex-direction: row;
        align-items: center;
        justify-content: center;
        gap: 4px;
        margin-bottom: 1px;
        margin-top: 1px;
      }

      .dropdown-container .shiny-input-container {
        margin-bottom: 0 !important;
      }

      .dropdown-container .selectize-control {
        font-size: 0.6em;
        min-width: 170px;
        max-width: 500px;
      }

      .dropdown-container .selectize-input {
        padding: 2px 6px;
        min-height: 26px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .dropdown-container .btn {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }

      .dropdown-select {
        font-size: 0.7em !important;
        width: 180px !important;
        margin-bottom: 5px;
        padding: 2px 5px !important;
        height: 28px !important;
      }

      /* Controls row for 'Tidligere aktiviteter' */
      .controls-row {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 10px;
        margin-bottom: 10px;
      }
      .controls-row .shiny-input-container {
        margin-bottom: 0.1em;
      }
      .controls-row label {
        font-size: 0.9em;
      }

/* Tighter row for third line of checkboxes - FIXED with even spacing */
.controls-row-tight {
  display: flex;
  flex-wrap: nowrap;
  justify-content: space-between;
  align-items: flex-start;
  margin-top: 0px;
  margin-bottom: 0px;
  width: 100%;
  max-width: 1100px;
  margin-left: auto;
  margin-right: auto;
}

/* Each checkbox block */
.controls-row-tight .shiny-input-container {
  margin: 0;
  padding: 0;
  white-space: normal;
  flex: 1;
  text-align: center;
  min-width: 0; /* Allow shrinking if needed */
}

/* Style the checkbox container */
.controls-row-tight .shiny-input-container .checkbox {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: flex-start;
  height: 100%;
}

/* Tighter row for third line of checkboxes */
.controls-row-tight {
  display: flex;
  flex-wrap: wrap;          /* allows wrapping to next line on narrow window */
  justify-content: center;  /* center the whole group */
  gap: 2px 0px;            /* row-gap, column-gap - adjust 24 for more/less horizontal spacing */
  margin-top: -4px;
  margin-bottom: 0;
  width: 100%;
  max-width: 1100px;
  margin-left: auto;
  margin-right: auto;
}

/* Wrapper for each checkbox + label text */
.cb-wrap {
  display: flex;
  flex-direction: column;
  align-items: center;
  text-align: center;
  font-size: 0.9em;
  line-height: 1;
  flex: 0 0 auto;      /* do not stretch */
  margin: 0 -50px;       /* small space between neighbours */
}

/* Tighten spacing around the checkbox input itself */
.cb-wrap .shiny-input-container {
  margin: -10px 0 0 0;
  padding: 0;
}
    "))
  ),

  div(
    class = "content",

    div(class = "header", "Statistik over afdelingens aktiviteter"),

    # ---- Synkronisering (top) ----
    div(
      class = "links",
      actionButton(
        "download_data",
        "Synkroniser data med Sharepoint",
        class = "link-button"
      )
    ),

    div(
      class = "links",
      uiOutput("last_sync")
    ),

    div(
      class = "links",
      uiOutput("status_message")
    ),

        # Always: line above KFE/KFA, regardless of plot
        div(class = "section-separator"),

    # ---- Aktivitetsstatus (eksisterende) ----
    div(class = "subheader", "Aktivitetsstatus"),

    div(
      class = "links",
      actionButton(
        "run_plot",
        "Aktivitetsstatus",
        class = "link-button"
      )
    ),

    conditionalPanel(
      condition = "output.plot_available == true",
      div(
        class = "plot-container",
        plotOutput("activity_plot", inline = TRUE)
      ),
      div(
        class = "download-links-activity",  # Changed to specific class
        downloadButton("download_plot_png", "Download PNG", class = "small-button"),
        downloadButton("download_plot_svg", "Download SVG", class = "small-button")
      )
    ),

    div(class = "section-separator"),

    # ---- Tidligere aktiviteter (tidligere 'Antal aktiviteter over tid') ----
    div(class = "subheader", "Tidligere aktiviteter"),

    div(
      class = "links",
      actionButton(
        "run_svartype_plot",
        "Plot aktiviteter",
        class = "link-button"
      )
    ),

    # Line 1: Fra, Til, Tidsopløsning, Vis antal
    div(
      class = "controls-row",
      dateInput(
        "from_date",
        label = "Fra",
        value = floor_date(Sys.Date(), "month") %m-% years(1),  # first day of month, one year back
        format = "dd-mm-yyyy",
        width = "120px"
      ),
dateInput(
  "to_date",
  label = "Til",
  value = floor_date(Sys.Date(), "month") - days(1),  # Last day of previous month
  format = "dd-mm-yyyy",
  width = "120px"
),
      selectInput(
        "timeGranularity",
        label = "Tidsopløsning",
        choices = c("Uge", "Måned", "Kvartal"),
        selected = "Måned",
        width = "120px"
      ),
      selectInput(
        "countMode",
        label = "Vis antal",
        choices = c("Ingen", "Samlet", "Opdelt"),
        selected = "Samlet",
        width = "140px"
      )
    ),

    # Line 2: Svartype, Speciale, Region
    div(
      class = "controls-row",
      selectInput(
        "svartypeFilterToggle",
        label = "Svartype",
        choices = c(
          "Alle",
          "Klinisk rådgivning",
          "Almindeligt svar",
          "Kortsvar",
          "Generel forespørgsel",
          "Medicingennemgang",
          "Ibrugtagningssag"
        ),
        selected = "Alle",
        width = "190px"
      ),
      selectInput(
        "specialeToggle",
        label = "Speciale og sektor",
        choices = "Alle",     # updated in server when data is known
        selected = "Alle",
        width = "220px"
      ),
      selectInput(
        "regionToggle",
        label = "Region (rekvirent)",
        choices = c("Begge", "Nordjylland", "Midtjylland"),
        selected = "Begge",
        width = "180px"
      )
    ),

# Line 3: Grupper..., Vis Psykiatrikonference og Andre, Trendlinje, Vis samlet antal
div(
  class = "controls-row-tight",
  div(
    class = "cb-wrap",
    div(HTML("Gruppér kliniske henvendelser<br>(alm., kort, generel)")),
    checkboxInput(
      "groupSvarToggle",
      label = NULL,
      value = FALSE
    )
  ),
  div(
    class = "cb-wrap",
    div("Vis Psykiatrikonference og Andre"),
    checkboxInput(
      "psykiatrikonf_AndreToggle",
      label = NULL,
      value = FALSE
    )
  ),
  div(
    class = "cb-wrap",
    div("Trendlinje"),
    checkboxInput(
      "showTrendlineToggle",
      label = NULL,
      value = FALSE
    )
  ),
  div(
    class = "cb-wrap",
    div("Vis samlet antal"),
    checkboxInput(
      "showCountInLegend",
      label = NULL,
      value = FALSE
    )
  )
),

    conditionalPanel(
      condition = "output.svartype_plot_available == true",
      div(class = "section-separator-thin"),
      div(
        class = "plot-container",
        plotOutput("svartype_plot", inline = TRUE),
        div(
          class = "download-links-svartype",  # Changed to specific class
          downloadButton("download_svartype_png", "Download PNG", class = "small-button"),
          downloadButton("download_svartype_svg", "Download SVG", class = "small-button")
        )
      )
    ),

    # Always: line above KFE/KFA, regardless of plot
        div(class = "section-separator-above-kfe"),

    # ---- KFE/KFA tilhørsforhold ----
    div(
      class = "kfe-kfa-section",
      div(class = "kfe-kfa-title", "Opdater tilhørsforhold til KFE og KFA"),
      div(class = "kfe-kfa-subtitle", "Nyeste Excel anvendes i scripts"),
      uiOutput("download_dropdown_ui"),
      div(
        class = "kfe-kfa-buttons",
        tags$div(
          style = "display: none;",
          fileInput("excel_file", NULL,
                    accept = c(".xlsx", ".xls"),
                    buttonLabel = "Upload")
        ),
        actionButton("upload_excel", "Upload", class = "small-button"),
        downloadButton("download_original", "Download original", class = "small-button")
      )
    ),

    div(class = "section-separator"),

    tags$details(
      tags$summary("Tekniske detaljer (klik for at folde ud)"),
      div(
        id = "console_output",
        class = "console-output",
        ""
      )
    )
  ),

  div(
    class = "footer",
    "Support: Ole Andersen, oleemil@biomed.au.dk"
  )
)

server <- function(input, output, session) {

  script_status <- reactiveVal(NULL)

  activity_plot      <- reactiveVal(NULL)
  svartype_plot_obj  <- reactiveVal(NULL)

  output$plot_available <- reactive({
    !is.null(activity_plot())
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)

  output$svartype_plot_available <- reactive({
    !is.null(svartype_plot_obj())
  })
  outputOptions(output, "svartype_plot_available", suspendWhenHidden = FALSE)

  # -------------------------- LAST SYNC -------------------------- #

  data_dir <- here("statistik", "Data", "Azure data")

  get_newest_rds_file <- function(directory_path) {
    rds_files <- list.files(
      path   = directory_path,
      pattern = "^azure_.*\\.rds$",
      full.names = TRUE
    )
    if (length(rds_files) == 0) return(NULL)
    file_info <- file.info(rds_files)
    rds_files[which.max(file_info$mtime)]
  }

  last_sync_time <- reactiveVal(NA)
  sync_ok        <- reactiveVal(FALSE)

  update_last_sync <- function(initial = FALSE) {
    if (!dir.exists(data_dir)) {
      last_sync_time(NA)
      if (initial) sync_ok(FALSE)
      return(invisible(NULL))
    }
    newest <- get_newest_rds_file(data_dir)
    if (is.null(newest)) {
      last_sync_time(NA)
      if (initial) sync_ok(FALSE)
    } else {
      last_sync_time(file.info(newest)$mtime)
      if (initial) sync_ok(FALSE)
    }
    invisible(NULL)
  }

  update_last_sync(initial = TRUE)

  output$last_sync <- renderUI({
    t <- last_sync_time()
    text <- if (is.null(t) || is.na(t)) {
      "Seneste synkronisering: Ingen filer fundet"
    } else {
      paste0(
        "Seneste synkronisering: ",
        format(t, "%d-%m-%Y %H:%M")
      )
    }
    cls <- "last-sync-text"
    if (isTRUE(sync_ok())) {
      cls <- paste(cls, "last-sync-ok")
    }
    div(class = cls, text)
  })

  # -------------------------- KFE/KFA TILHØRSFORHOLD -------------------------- #

  main_folder     <- here("statistik", "data", "tilknytning, KFE_KFA")
  original_folder <- here("statistik", "data", "tilknytning, KFE_KFA", "Original")

  observe({
    if (!dir.exists(main_folder)) dir.create(main_folder, recursive = TRUE)
    if (!dir.exists(original_folder)) dir.create(original_folder, recursive = TRUE)
  })

  get_excel_files <- reactive({
    files <- list.files(main_folder, pattern = "\\.xlsx$", full.names = FALSE)
    if (length(files) == 0) return(NULL)
    file_info     <- file.info(file.path(main_folder, files))
    files_ordered <- files[order(file_info$mtime, decreasing = TRUE)]
    files_ordered
  })

  output$download_dropdown_ui <- renderUI({
    files <- get_excel_files()
    if (is.null(files)) return(NULL)

    tagList(
      div(
        class = "dropdown-container",
        selectInput("excel_files", NULL,
                    choices = files,
                    width = "170px",
                    selectize = TRUE),
        downloadButton("download_selected", "Download valgt", class = "small-download-button")
      )
    )
  })

  observeEvent(input$upload_excel, {
    shinyjs::click("excel_file")
  })

  observeEvent(input$excel_file, {
    req(input$excel_file)

    if (!grepl("\\.xlsx?$", input$excel_file$name, ignore.case = TRUE)) {
      script_status(list(
        type = "error",
        message = "Kun Excel-filer (.xlsx, .xls) er tilladte."
      ))
      return()
    }

    tryCatch({
      timestamp    <- format(Sys.time(), "%Y-%m-%d %H.%M")
      new_filename <- paste0("Tilhørsforhold (", timestamp, ").xlsx")
      new_filepath <- file.path(main_folder, new_filename)

      file.copy(input$excel_file$datapath, new_filepath)

      files <- list.files(main_folder, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(files) > 10) {
        file_info     <- file.info(files)
        files_ordered <- files[order(file_info$mtime)]
        files_to_remove <- files_ordered[1:(length(files) - 10)]
        file.remove(files_to_remove)
      }

      script_status(list(
        type    = "message",
        message = paste("Filen er uploadet succesfuldt:", new_filename)
      ))

    }, error = function(e) {
      script_status(list(
        type    = "error",
        message = paste("Fejl under upload:", e$message)
      ))
    })
  })

  output$download_selected <- downloadHandler(
    filename = function() {
      req(input$excel_files)
      input$excel_files
    },
    content = function(file) {
      req(input$excel_files)
      file_path <- file.path(main_folder, input$excel_files)
      file.copy(file_path, file)
    }
  )

  output$download_original <- downloadHandler(
    filename = function() {
      original_files <- list.files(original_folder, pattern = "\\.xlsx$")
      if (length(original_files) > 0) return(original_files[1])
      "Original.xlsx"
    },
    content = function(file) {
      original_files <- list.files(original_folder, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(original_files) > 0) {
        file.copy(original_files[1], file)
      }
    }
  )

  # -------------------------- DOWNLOAD DATA SCRIPT ------------------------- #

  observeEvent(input$download_data, {
    shinyjs::disable("download_data")

    shinyjs::html("console_output", "")
    script_status(NULL)
    sync_ok(FALSE)

    shinyjs::html(
      id   = "console_output",
      html = "Starter download- og forberedelsesscript...\n\n",
      add  = TRUE
    )

    script_path <- file.path(
      here("statistik", "scripts"),
      "download, prepare, save.R"
    )

    if (!file.exists(script_path)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0("FEJL: Script ikke fundet:\n", script_path, "\n"),
        add  = TRUE
      )

      script_status(list(
        type    = "error",
        message = "Scriptfilen til data-download blev ikke fundet. Kontakt support."
      ))
library(shiny)
library(here)
library(shinyjs)
library(lubridate)
library(grid)

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      /* Layout */
      html, body {
        height: 100%;
        margin: 0;
      }
      body {
        background-color: white;
        color: #0033A0;
        font-family: Arial, sans-serif;
        text-align: center;
        padding: 40px 20px 30px;
        display: flex;
        flex-direction: column;
        min-height: 100vh;
      }
      .content {
        flex: 1;
        max-width: 1200px;
        margin: 0 auto;
      }
      .header {
        font-size: 3em;
        margin-bottom: 0.3em;
      }
      .subheader {
        font-size: 1.5em;
        margin-top: 1.0em;
        margin-bottom: 0.4em;
        color: #555;
      }
      .links {
        margin: 0.3em 0;
      }
      .link-button {
        display: inline-block;
        margin: 0.3em;
        padding: 0.8em 1.8em;
        font-size: 1.1em;
        color: white;
        background-color: #0033A0;
        border-radius: 6px;
        text-decoration: none;
        border: none;
        cursor: pointer;
      }
      .link-button:hover {
        opacity: 0.9;
      }
      .footer {
        margin-top: auto;
        padding: 15px 0;
        font-size: 0.9em;
        color: #555;
      }

      /* Status box (only for errors now) */
      .status-message {
        margin-top: 0.2em;
        padding: 0.4em 0.7em;
        border-radius: 4px;
        font-weight: bold;
        text-align: left;
        display: inline-block;
        max-width: 900px;
        font-size: 0.9em;
      }
      .status-error {
        background-color: #f8d7da;
        color: #721c24;
        border: 1px solid #f5c6cb;
      }

      /* Console styling */
      .console-output {
        background-color: #2b2b2b;
        color: #f8f8f8;
        font-family: 'Courier New', monospace;
        text-align: left;
        padding: 12px;
        margin: 8px auto 15px auto;
        border-radius: 4px;

        min-height: 150px;
        max-height: 50vh;
        width: 100%;
        max-width: 1100px;

        overflow-y: auto;
        border: 1px solid #555;

        white-space: pre-wrap;
        word-wrap: break-word;
      }

      details {
        margin-top: 5px;
      }
      details > summary {
        cursor: pointer;
        font-weight: bold;
        color: #0033A0;
        list-style: none;
      }
      details > summary::-webkit-details-marker {
        display: none;
      }

      .last-sync-text {
        font-size: 0.9em;
        color: #444;
        margin-top: 0.2em;
        display: inline-block;
        padding: 0.2em 0.5em;
        border-radius: 4px;
      }
      .last-sync-ok {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
        font-weight: 600;
      }

      .section-separator {
        border-top: 1px solid #cccccc;
        margin: 8px auto;
        width: 85%;
      }

      /* Dedicated separator just above KFE/KFA section */
      .section-separator-above-kfe {
        border-top: 1px solid #cccccc;
        width: 85%;
        margin: 8px auto 2px auto;  /* adjust 8 for more/less vertical space */
        position: relative;
        z-index: 2;
        clear: both;
      }

      .section-separator-thin {
        border-top: 1px solid #dddddd;
        margin: 4px auto 6px auto;
        width: 80%;
      }

      /* === Plot + download tight spacing === */
      .plot-container {
        margin: 0 !important;
        padding: 0 !important;
      }
      .plot-container .shiny-plot-output,
      .plot-container .shiny-plot-output > div,
      .plot-container .shiny-plot-output > svg {
        margin: 0 !important;
        padding: 0 !important;
        border: none !important;
      }

      /* Separate download button spacing for the two plots */
      .download-links-activity {
        margin: -0px 0 0 0 !important;  /* tight under Aktivitetsstatus plot */
        padding: 0 !important;
      }
      .download-links-svartype {
        margin: -130px 0 0 0 !important;   /* less tight under Tidligere aktiviteter plot */
        padding: 0 !important;
      }

      /* Small button styling */
      .small-button {
        padding: 0.35em 0.8em !important;
        font-size: 0.8em !important;
        margin: 0.05em !important;
      }

      .small-download-button {
        padding: 0.3em 0.7em !important;
        font-size: 0.75em !important;
        margin: 0.05em !important;
      }

      /* KFE/KFA section styling: very tight vertical spacing */
      .kfe-kfa-section {
        margin: 4px 0 6px;
      }
      .kfe-kfa-title {
        font-weight: bold;
        color: #0033A0;
        margin-bottom: 0px;
        font-size: 1.1em;
      }
      .kfe-kfa-subtitle {
        font-size: 0.8em;
        color: #000000;
        margin-top: 0px;
        margin-bottom: 3px;
      }
      .kfe-kfa-buttons {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 4px;
        flex-wrap: wrap;
        margin-top: 1px;
        margin-bottom: 1px;
      }

      /* Dropdown container: dropdown + Download on same line, minimal spacing */
      .dropdown-container {
        display: flex;
        flex-direction: row;
        align-items: center;
        justify-content: center;
        gap: 4px;
        margin-bottom: 1px;
        margin-top: 1px;
      }

      .dropdown-container .shiny-input-container {
        margin-bottom: 0 !important;
      }

      .dropdown-container .selectize-control {
        font-size: 0.6em;
        min-width: 170px;
        max-width: 500px;
      }

      .dropdown-container .selectize-input {
        padding: 2px 6px;
        min-height: 26px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .dropdown-container .btn {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }

      .dropdown-select {
        font-size: 0.7em !important;
        width: 180px !important;
        margin-bottom: 5px;
        padding: 2px 5px !important;
        height: 28px !important;
      }

      /* Controls row for 'Tidligere aktiviteter' */
      .controls-row {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 10px;
        margin-bottom: 10px;
      }
      .controls-row .shiny-input-container {
        margin-bottom: 0.1em;
      }
      .controls-row label {
        font-size: 0.9em;
      }

/* Tighter row for third line of checkboxes - FIXED with even spacing */
.controls-row-tight {
  display: flex;
  flex-wrap: nowrap;
  justify-content: space-between;
  align-items: flex-start;
  margin-top: 0px;
  margin-bottom: 0px;
  width: 100%;
  max-width: 1100px;
  margin-left: auto;
  margin-right: auto;
}

/* Each checkbox block */
.controls-row-tight .shiny-input-container {
  margin: 0;
  padding: 0;
  white-space: normal;
  flex: 1;
  text-align: center;
  min-width: 0; /* Allow shrinking if needed */
}

/* Style the checkbox container */
.controls-row-tight .shiny-input-container .checkbox {
  display: flex;
  flex-direction: column;
  align-items: center;
  justify-content: flex-start;
  height: 100%;
}

/* Tighter row for third line of checkboxes */
.controls-row-tight {
  display: flex;
  flex-wrap: wrap;          /* allows wrapping to next line on narrow window */
  justify-content: center;  /* center the whole group */
  gap: 2px 0px;            /* row-gap, column-gap - adjust 24 for more/less horizontal spacing */
  margin-top: -4px;
  margin-bottom: 0;
  width: 100%;
  max-width: 1100px;
  margin-left: auto;
  margin-right: auto;
}

/* Wrapper for each checkbox + label text */
.cb-wrap {
  display: flex;
  flex-direction: column;
  align-items: center;
  text-align: center;
  font-size: 0.9em;
  line-height: 1;
  flex: 0 0 auto;      /* do not stretch */
  margin: 0 -50px;       /* small space between neighbours */
}

/* Tighten spacing around the checkbox input itself */
.cb-wrap .shiny-input-container {
  margin: -10px 0 0 0;
  padding: 0;
}
    "))
  ),

  div(
    class = "content",

    div(class = "header", "Statistik over afdelingens aktiviteter"),

    # ---- Synkronisering (top) ----
    div(
      class = "links",
      actionButton(
        "download_data",
        "Synkroniser data med Sharepoint",
        class = "link-button"
      )
    ),

    div(
      class = "links",
      uiOutput("last_sync")
    ),

    div(
      class = "links",
      uiOutput("status_message")
    ),

        # Always: line above KFE/KFA, regardless of plot
        div(class = "section-separator"),

    # ---- Aktivitetsstatus (eksisterende) ----
    div(class = "subheader", "Aktivitetsstatus"),

    div(
      class = "links",
      actionButton(
        "run_plot",
        "Aktivitetsstatus",
        class = "link-button"
      )
    ),

    conditionalPanel(
      condition = "output.plot_available == true",
      div(
        class = "plot-container",
        plotOutput("activity_plot", inline = TRUE)
      ),
      div(
        class = "download-links-activity",  # Changed to specific class
        downloadButton("download_plot_png", "Download PNG", class = "small-button"),
        downloadButton("download_plot_svg", "Download SVG", class = "small-button")
      )
    ),

    div(class = "section-separator"),

    # ---- Tidligere aktiviteter (tidligere 'Antal aktiviteter over tid') ----
    div(class = "subheader", "Tidligere aktiviteter"),

    div(
      class = "links",
      actionButton(
        "run_svartype_plot",
        "Plot aktiviteter",
        class = "link-button"
      )
    ),

    # Line 1: Fra, Til, Tidsopløsning, Vis antal
    div(
      class = "controls-row",
      dateInput(
        "from_date",
        label = "Fra",
        value = floor_date(Sys.Date(), "month") %m-% years(1),  # first day of month, one year back
        format = "dd-mm-yyyy",
        width = "120px"
      ),
dateInput(
  "to_date",
  label = "Til",
  value = floor_date(Sys.Date(), "month") - days(1),  # Last day of previous month
  format = "dd-mm-yyyy",
  width = "120px"
),
      selectInput(
        "timeGranularity",
        label = "Tidsopløsning",
        choices = c("Uge", "Måned", "Kvartal"),
        selected = "Måned",
        width = "120px"
      ),
      selectInput(
        "countMode",
        label = "Vis antal",
        choices = c("Ingen", "Samlet", "Opdelt"),
        selected = "Samlet",
        width = "140px"
      )
    ),

    # Line 2: Svartype, Speciale, Region
    div(
      class = "controls-row",
      selectInput(
        "svartypeFilterToggle",
        label = "Svartype",
        choices = c(
          "Alle",
          "Klinisk rådgivning",
          "Almindeligt svar",
          "Kortsvar",
          "Generel forespørgsel",
          "Medicingennemgang",
          "Ibrugtagningssag"
        ),
        selected = "Alle",
        width = "190px"
      ),
      selectInput(
        "specialeToggle",
        label = "Speciale og sektor",
        choices = "Alle",     # updated in server when data is known
        selected = "Alle",
        width = "220px"
      ),
      selectInput(
        "regionToggle",
        label = "Region (rekvirent)",
        choices = c("Begge", "Nordjylland", "Midtjylland"),
        selected = "Begge",
        width = "180px"
      )
    ),

# Line 3: Grupper..., Vis Psykiatrikonference og Andre, Trendlinje, Vis samlet antal
div(
  class = "controls-row-tight",
  div(
    class = "cb-wrap",
    div(HTML("Gruppér kliniske henvendelser<br>(alm., kort, generel)")),
    checkboxInput(
      "groupSvarToggle",
      label = NULL,
      value = FALSE
    )
  ),
  div(
    class = "cb-wrap",
    div("Vis Psykiatrikonference og Andre"),
    checkboxInput(
      "psykiatrikonf_AndreToggle",
      label = NULL,
      value = FALSE
    )
  ),
  div(
    class = "cb-wrap",
    div("Trendlinje"),
    checkboxInput(
      "showTrendlineToggle",
      label = NULL,
      value = FALSE
    )
  ),
  div(
    class = "cb-wrap",
    div("Vis samlet antal"),
    checkboxInput(
      "showCountInLegend",
      label = NULL,
      value = FALSE
    )
  )
),

    conditionalPanel(
      condition = "output.svartype_plot_available == true",
      div(class = "section-separator-thin"),
      div(
        class = "plot-container",
        plotOutput("svartype_plot", inline = TRUE),
        div(
          class = "download-links-svartype",  # Changed to specific class
          downloadButton("download_svartype_png", "Download PNG", class = "small-button"),
          downloadButton("download_svartype_svg", "Download SVG", class = "small-button")
        )
      )
    ),

    # Always: line above KFE/KFA, regardless of plot
        div(class = "section-separator-above-kfe"),

    # ---- KFE/KFA tilhørsforhold ----
    div(
      class = "kfe-kfa-section",
      div(class = "kfe-kfa-title", "Opdater tilhørsforhold til KFE og KFA"),
      div(class = "kfe-kfa-subtitle", "Nyeste Excel anvendes i scripts"),
      uiOutput("download_dropdown_ui"),
      div(
        class = "kfe-kfa-buttons",
        tags$div(
          style = "display: none;",
          fileInput("excel_file", NULL,
                    accept = c(".xlsx", ".xls"),
                    buttonLabel = "Upload")
        ),
        actionButton("upload_excel", "Upload", class = "small-button"),
        downloadButton("download_original", "Download original", class = "small-button")
      )
    ),

    div(class = "section-separator"),

    tags$details(
      tags$summary("Tekniske detaljer (klik for at folde ud)"),
      div(
        id = "console_output",
        class = "console-output",
        ""
      )
    )
  ),

  div(
    class = "footer",
    "Support: Ole Andersen, oleemil@biomed.au.dk"
  )
)

server <- function(input, output, session) {

  script_status <- reactiveVal(NULL)

  activity_plot      <- reactiveVal(NULL)
  svartype_plot_obj  <- reactiveVal(NULL)

  output$plot_available <- reactive({
    !is.null(activity_plot())
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)

  output$svartype_plot_available <- reactive({
    !is.null(svartype_plot_obj())
  })
  outputOptions(output, "svartype_plot_available", suspendWhenHidden = FALSE)

  # -------------------------- LAST SYNC -------------------------- #

  data_dir <- here("statistik", "Data", "Azure data")

  get_newest_rds_file <- function(directory_path) {
    rds_files <- list.files(
      path   = directory_path,
      pattern = "^azure_.*\\.rds$",
      full.names = TRUE
    )
    if (length(rds_files) == 0) return(NULL)
    file_info <- file.info(rds_files)
    rds_files[which.max(file_info$mtime)]
  }

  last_sync_time <- reactiveVal(NA)
  sync_ok        <- reactiveVal(FALSE)

  update_last_sync <- function(initial = FALSE) {
    if (!dir.exists(data_dir)) {
      last_sync_time(NA)
      if (initial) sync_ok(FALSE)
      return(invisible(NULL))
    }
    newest <- get_newest_rds_file(data_dir)
    if (is.null(newest)) {
      last_sync_time(NA)
      if (initial) sync_ok(FALSE)
    } else {
      last_sync_time(file.info(newest)$mtime)
      if (initial) sync_ok(FALSE)
    }
    invisible(NULL)
  }

  update_last_sync(initial = TRUE)

  output$last_sync <- renderUI({
    t <- last_sync_time()
    text <- if (is.null(t) || is.na(t)) {
      "Seneste synkronisering: Ingen filer fundet"
    } else {
      paste0(
        "Seneste synkronisering: ",
        format(t, "%d-%m-%Y %H:%M")
      )
    }
    cls <- "last-sync-text"
    if (isTRUE(sync_ok())) {
      cls <- paste(cls, "last-sync-ok")
    }
    div(class = cls, text)
  })

  # -------------------------- KFE/KFA TILHØRSFORHOLD -------------------------- #

  main_folder     <- here("statistik", "data", "tilknytning, KFE_KFA")
  original_folder <- here("statistik", "data", "tilknytning, KFE_KFA", "Original")

  observe({
    if (!dir.exists(main_folder)) dir.create(main_folder, recursive = TRUE)
    if (!dir.exists(original_folder)) dir.create(original_folder, recursive = TRUE)
  })

  get_excel_files <- reactive({
    files <- list.files(main_folder, pattern = "\\.xlsx$", full.names = FALSE)
    if (length(files) == 0) return(NULL)
    file_info     <- file.info(file.path(main_folder, files))
    files_ordered <- files[order(file_info$mtime, decreasing = TRUE)]
    files_ordered
  })

  output$download_dropdown_ui <- renderUI({
    files <- get_excel_files()
    if (is.null(files)) return(NULL)

    tagList(
      div(
        class = "dropdown-container",
        selectInput("excel_files", NULL,
                    choices = files,
                    width = "170px",
                    selectize = TRUE),
        downloadButton("download_selected", "Download valgt", class = "small-download-button")
      )
    )
  })

  observeEvent(input$upload_excel, {
    shinyjs::click("excel_file")
  })

  observeEvent(input$excel_file, {
    req(input$excel_file)

    if (!grepl("\\.xlsx?$", input$excel_file$name, ignore.case = TRUE)) {
      script_status(list(
        type = "error",
        message = "Kun Excel-filer (.xlsx, .xls) er tilladte."
      ))
      return()
    }

    tryCatch({
      timestamp    <- format(Sys.time(), "%Y-%m-%d %H.%M")
      new_filename <- paste0("Tilhørsforhold (", timestamp, ").xlsx")
      new_filepath <- file.path(main_folder, new_filename)

      file.copy(input$excel_file$datapath, new_filepath)

      files <- list.files(main_folder, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(files) > 10) {
        file_info     <- file.info(files)
        files_ordered <- files[order(file_info$mtime)]
        files_to_remove <- files_ordered[1:(length(files) - 10)]
        file.remove(files_to_remove)
      }

      script_status(list(
        type    = "message",
        message = paste("Filen er uploadet succesfuldt:", new_filename)
      ))

    }, error = function(e) {
      script_status(list(
        type    = "error",
        message = paste("Fejl under upload:", e$message)
      ))
    })
  })

  output$download_selected <- downloadHandler(
    filename = function() {
      req(input$excel_files)
      input$excel_files
    },
    content = function(file) {
      req(input$excel_files)
      file_path <- file.path(main_folder, input$excel_files)
      file.copy(file_path, file)
    }
  )

  output$download_original <- downloadHandler(
    filename = function() {
      original_files <- list.files(original_folder, pattern = "\\.xlsx$")
      if (length(original_files) > 0) return(original_files[1])
      "Original.xlsx"
    },
    content = function(file) {
      original_files <- list.files(original_folder, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(original_files) > 0) {
        file.copy(original_files[1], file)
      }
    }
  )

  # -------------------------- DOWNLOAD DATA SCRIPT ------------------------- #

  observeEvent(input$download_data, {
    shinyjs::disable("download_data")

    shinyjs::html("console_output", "")
    script_status(NULL)
    sync_ok(FALSE)

    shinyjs::html(
      id   = "console_output",
      html = "Starter download- og forberedelsesscript...\n\n",
      add  = TRUE
    )

    script_path <- file.path(
      here("statistik", "scripts"),
      "download, prepare, save.R"
    )

    if (!file.exists(script_path)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0("FEJL: Script ikke fundet:\n", script_path, "\n"),
        add  = TRUE
      )

      script_status(list(
        type    = "error",
        message = "Scriptfilen til data-download blev ikke fundet. Kontakt support."
      ))

      shinyjs::enable("download_data")
      return(invisible(NULL))
    }

    tryCatch(
      {
        withCallingHandlers(
          {
            message("Kører script: ", script_path)
            source(script_path, local = TRUE)
          },
          message = function(m) {
            shinyjs::html(
              id   = "console_output",
              html = paste0(m$message, "\n"),
              add  = TRUE
            )
            invokeRestart("muffleMessage")
          }
        )

        update_last_sync(initial = FALSE)
        sync_ok(TRUE)

        shinyjs::html(
          id   = "console_output",
          html = "\nScript færdigt.\n",
          add  = TRUE
        )
      },
      error = function(e) {
        err_msg <- e$message
        sync_ok(FALSE)

        msg_ui <- "Der opstod en fejl under synkronisering af data. Se tekniske detaljer eller kontakt support."

if (grepl("Azure authentication missing or expired", err_msg, fixed = TRUE)) {
  msg_ui <- HTML(paste0(
    "Azure skal verificeres:<br>",
    "1. Login på server fra AU net (indsæt AUID med Azure adgang som au123456): ",
    "<code>ssh -J AUID@ssh.au.dk AUID@kfaapps.uni.au.dk</code><br>",
    "2. Kopier dette til server-terminalen (kræver sudo-rettigheder):<br>",
    "<pre>",
    "sudo -u shiny -H R --vanilla << 'EOF'\n",
    "library(AzureAuth)\n",
    "Sys.setenv(R_AZURE_DATA_DIR = '/srv/shiny-server/kfaapps/statistik/azure_cache')\n",
    "library(Microsoft365R)\n\n",
    "cat('\\nStarting Microsoft365R device-code login for shiny user...\\n\\n')\n\n",
    "site_list <- list_sharepoint_sites(auth_type = 'device_code')\n\n",
    "cat('\\nAuthentication complete. Token should now be cached in R_AZURE_DATA_DIR.\\n')\n",
    "q(save = 'no')\n",
    "EOF",
    "</pre>"
  ))
}

        script_status(list(
          type    = "error",
          message = msg_ui
        ))

        shinyjs::html(
          id   = "console_output",
          html = paste0(
            "\n*** TEKNISK FEJL ***\n",
            err_msg,
            "\n********************\n"
          ),
          add  = TRUE
        )
      }
    )

    shinyjs::enable("download_data")
  })

  # -------------------------- CURRENT ACTIVITIES PLOT ---------------------- #

  observeEvent(input$run_plot, {
    shinyjs::disable("run_plot")

    shinyjs::html("console_output", "")
    shinyjs::html(
      id   = "console_output",
      html = "Starter script til aktivitetsplot...\n\n",
      add  = TRUE
    )

    script_path_plot <- file.path(
      here("statistik", "scripts"),
      "current activities graph.R"
    )

    if (!file.exists(script_path_plot)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0(
          "FEJL: Aktivitetsplot-script ikke fundet:\n",
          script_path_plot, "\n"
        ),
        add  = TRUE
      )

      script_status(list(
        type    = "error",
        message = "Scriptet til aktivitetsplot blev ikke fundet. Kontakt support."
      ))

      shinyjs::enable("run_plot")
      return(invisible(NULL))
    }

    tryCatch(
      {
        plot_env <- new.env(parent = globalenv())

        withCallingHandlers(
          {
            message("Kører aktivitetsplot-script: ", script_path_plot)
            source(script_path_plot, local = plot_env)
          },
          message = function(m) {
            shinyjs::html(
              id   = "console_output",
              html = paste0(m$message, "\n"),
              add  = TRUE
            )
            invokeRestart("muffleMessage")
          }
        )

        if (!exists("combined_grob", envir = plot_env, inherits = FALSE)) {
          stop("combined_grob not found efter kørsel af aktivitetsplot-script.")
        }

        activity_plot(get("combined_grob", envir = plot_env))

        shinyjs::html(
          id   = "console_output",
          html = "\nAktivitetsplot-script færdigt.\n",
          add  = TRUE
        )

      },
      error = function(e) {
        err_msg <- e$message

        script_status(list(
          type    = "error",
          message = "Der opstod en fejl under opdatering af aktivitetsplot. Se tekniske detaljer eller kontakt support."
        ))

        shinyjs::html(
          id   = "console_output",
          html = paste0(
            "\n*** TEKNISK FEJL (aktivitetsplot) ***\n",
            err_msg,
            "\n*************************************\n"
          ),
          add  = TRUE
        )
      }
    )

    shinyjs::enable("run_plot")
  })

  output$activity_plot <- renderPlot(
    {
      req(activity_plot())
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
    },
    width  = 600,
    height = 500,
    res    = 96
  )

  output$download_plot_png <- downloadHandler(
    filename = function() {
      paste0("activity_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(activity_plot())
      grDevices::png(file, width = 8, height = 5, units = "in", res = 300)
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
      grDevices::dev.off()
    }
  )

  output$download_plot_svg <- downloadHandler(
    filename = function() {
      paste0("activity_plot_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(activity_plot())
      grDevices::svg(file, width = 8, height = 5)
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
      grDevices::dev.off()
    }
  )

  # ------------------- LOAD DATA FOR SPECIALTY DROPDOWN -------------------- #

  observe({
  newest <- get_newest_rds_file(data_dir)
  if (is.null(newest)) return()

  df <- tryCatch(
    readRDS(newest),
    error = function(e) NULL
  )
  if (is.null(df)) return()
  if (!"specialeCorrected" %in% names(df)) return()

  specs <- sort(unique(na.omit(df$specialeCorrected)))

  # Ensure "Hospital" is available as a virtual "speciale" that triggers sektor filtering
  if (!"Hospital" %in% specs) {
    specs <- c(specs, "Hospital")
  }

  # Order: Hospital, Almen praksis, then the rest alphabetically
  priority <- c("Hospital", "Almen praksis")
  priority_in_specs <- priority[priority %in% specs]
  rest <- setdiff(specs, priority_in_specs)
  specs_ordered <- c(priority_in_specs, sort(rest))

  updateSelectInput(
    session,
    "specialeToggle",
    choices = c("Alle", specs_ordered),
    selected = "Alle"
  )
})

  # -------------------------- SVARTYPE PLOT (Tidligere aktiviteter) -------------------------- #

  observeEvent(input$run_svartype_plot, {
    shinyjs::disable("run_svartype_plot")

    shinyjs::html(
      id   = "console_output",
      html = "Starter script til 'Tidligere aktiviteter'...\n\n",
      add  = TRUE
    )

    script_path_plot <- file.path(
      here("statistik", "scripts"),
      "plot svartype.R"
    )

    if (!file.exists(script_path_plot)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0(
          "FEJL: 'plot svartype' script ikke fundet:\n",
          script_path_plot, "\n"
        ),
        add  = TRUE
      )

      script_status(list(
        type    = "error",
        message = "Scriptet 'plot svartype.R' blev ikke fundet. Kontakt support."
      ))

      shinyjs::enable("run_svartype_plot")
      return(invisible(NULL))
    }

    tryCatch(
      {
        plot_env <- new.env(parent = globalenv())

        # Map new UI inputs to old script toggles
        plot_env$showStackCount  <- (input$countMode == "Opdelt")
        plot_env$showTotalCount  <- (input$countMode == "Samlet")
        plot_env$timeGranularity <- input$timeGranularity
        plot_env$showTrendlineToggle   <- isTRUE(input$showTrendlineToggle)
        plot_env$groupSvarToggle       <- isTRUE(input$groupSvarToggle)
        plot_env$psykiatrikonf_AndreToggle <- isTRUE(input$psykiatrikonf_AndreToggle)
        plot_env$sektorToggle          <- "Alle"  # no sektor dropdown in UI anymore
        plot_env$specialeToggle        <- input$specialeToggle
        plot_env$regionToggle          <- input$regionToggle
        plot_env$svartypeFilterToggle  <- input$svartypeFilterToggle
        plot_env$showCountInLegend     <- isTRUE(input$showCountInLegend)

        # Map dates to start_*/end_* as before
        from <- as.Date(input$from_date)
        to   <- as.Date(input$to_date)

        plot_env$start_year  <- as.integer(format(from, "%Y"))
        plot_env$start_month <- as.integer(format(from, "%m"))
        plot_env$start_day   <- as.integer(format(from, "%d"))
        plot_env$end_year    <- as.integer(format(to, "%Y"))
        plot_env$end_month   <- as.integer(format(to, "%m"))
        plot_env$end_day     <- as.integer(format(to, "%d"))

        withCallingHandlers(
          {
            message("Kører 'plot svartype.R': ", script_path_plot)
            source(script_path_plot, local = plot_env)
          },
          message = function(m) {
            shinyjs::html(
              id   = "console_output",
              html = paste0(m$message, "\n"),
              add  = TRUE
            )
            invokeRestart("muffleMessage")
          }
        )

        if (!exists("p", envir = plot_env, inherits = FALSE)) {
          stop("Plot-objekt 'p' ikke fundet efter kørsel af 'plot svartype.R'.")
        }

        svartype_plot_obj(get("p", envir = plot_env))

        shinyjs::html(
          id   = "console_output",
          html = "\n'Tidligere aktiviteter' plot-script færdigt.\n",
          add  = TRUE
        )

      },
      error = function(e) {
        err_msg <- e$message

        script_status(list(
          type    = "error",
          message = "Der opstod en fejl under opdatering af 'Tidligere aktiviteter'. Se tekniske detaljer eller kontakt support."
        ))

        shinyjs::html(
          id   = "console_output",
          html = paste0(
            "\n*** TEKNISK FEJL (svartype-plot) ***\n",
            err_msg,
            "\n****************************************\n"
          ),
          add  = TRUE
        )
      }
    )

    shinyjs::enable("run_svartype_plot")
  })

  output$svartype_plot <- renderPlot(
    {
      req(svartype_plot_obj())
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
    },
    width  = 800,   # wider in app
    height = 650,   # taller to match original ggsave height = 6.5
    res    = 96
  )

  output$download_svartype_png <- downloadHandler(
    filename = function() {
      paste0("svartype_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(svartype_plot_obj())
      grDevices::png(file, width = 10, height = 6.5, units = "in", res = 300)
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
      grDevices::dev.off()
    }
  )

  output$download_svartype_svg <- downloadHandler(
    filename = function() {
      paste0("svartype_plot_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(svartype_plot_obj())
      grDevices::svg(file, width = 10, height = 6.5)
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
      grDevices::dev.off()
    }
  )

  # -------------------------- STATUS MESSAGE -------------------------- #

  output$status_message <- renderUI({
    status <- script_status()
    if (!is.null(status)) {
      div(
        class = paste("status-message", if (status$type == "error") "status-error" else "status-ok"),
        status$message
      )
    }
  })
}

shinyApp(ui, server)

      shinyjs::enable("download_data")
      return(invisible(NULL))
    }

    tryCatch(
      {
        withCallingHandlers(
          {
            message("Kører script: ", script_path)
            source(script_path, local = TRUE)
          },
          message = function(m) {
            shinyjs::html(
              id   = "console_output",
              html = paste0(m$message, "\n"),
              add  = TRUE
            )
            invokeRestart("muffleMessage")
          }
        )

        update_last_sync(initial = FALSE)
        sync_ok(TRUE)

        shinyjs::html(
          id   = "console_output",
          html = "\nScript færdigt.\n",
          add  = TRUE
        )
      },
error = function(e) {
  err_msg <- e$message
  sync_ok(FALSE)

  # Default UI message
  msg_ui <- "Der opstod en fejl under synkronisering af data. Se tekniske detaljer eller kontakt support."

  # 1) Special case: invalid or expired SharePoint secret
  if (grepl("INVALID_SHAREPOINT_SECRET", err_msg, fixed = TRUE)) {
    msg_ui <- HTML(paste0(
      "Client secret til Azure/SharePoint skal opdateres på serveren:<br>",
      "1) Åbn Terminal og login på serveren fra AU-net (indsæt eget AUID begge steder):<br>",
      "<code>ssh -J AUID@ssh.au.dk AUID@kfaapps.uni.au.dk</code><br><br>",
      "2) Gå til mappen med secret-filen:<br>",
      "<code>cd /srv/shiny-server/kfaapps/statistik</code><br><br>",
      "3) Åbn secret-filen med sudo, fjern gammel secret og indsæt ny secret:<br>",
      "<code>sudo nano secret_for_sharepoint.txt</code><br>",
      "   - Indsæt ny secret på én linje<br>",
      "   - Gem og luk nano med: <code>Ctrl+O</code>, <code>Enter</code>, <code>Ctrl+X</code><br><br>",
      "4) Kør synkronisering igen i denne app (knappen 'Synkroniser data med Sharepoint')."
    ))
  }

  script_status(list(
    type    = "error",
    message = msg_ui
  ))

  shinyjs::html(
    id   = "console_output",
    html = paste0(
      "\n*** TEKNISK FEJL ***\n",
      err_msg,
      "\n********************\n"
    ),
    add  = TRUE
  )
}
    )

    shinyjs::enable("download_data")
  })

  # -------------------------- CURRENT ACTIVITIES PLOT ---------------------- #

  observeEvent(input$run_plot, {
    shinyjs::disable("run_plot")

    shinyjs::html("console_output", "")
    shinyjs::html(
      id   = "console_output",
      html = "Starter script til aktivitetsplot...\n\n",
      add  = TRUE
    )

    script_path_plot <- file.path(
      here("statistik", "scripts"),
      "current activities graph.R"
    )

    if (!file.exists(script_path_plot)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0(
          "FEJL: Aktivitetsplot-script ikke fundet:\n",
          script_path_plot, "\n"
        ),
        add  = TRUE
      )

      script_status(list(
        type    = "error",
        message = "Scriptet til aktivitetsplot blev ikke fundet. Kontakt support."
      ))

      shinyjs::enable("run_plot")
      return(invisible(NULL))
    }

    tryCatch(
      {
        plot_env <- new.env(parent = globalenv())

        withCallingHandlers(
          {
            message("Kører aktivitetsplot-script: ", script_path_plot)
            source(script_path_plot, local = plot_env)
          },
          message = function(m) {
            shinyjs::html(
              id   = "console_output",
              html = paste0(m$message, "\n"),
              add  = TRUE
            )
            invokeRestart("muffleMessage")
          }
        )

        if (!exists("combined_grob", envir = plot_env, inherits = FALSE)) {
          stop("combined_grob not found efter kørsel af aktivitetsplot-script.")
        }

        activity_plot(get("combined_grob", envir = plot_env))

        shinyjs::html(
          id   = "console_output",
          html = "\nAktivitetsplot-script færdigt.\n",
          add  = TRUE
        )

      },
      error = function(e) {
        err_msg <- e$message

        script_status(list(
          type    = "error",
          message = "Der opstod en fejl under opdatering af aktivitetsplot. Se tekniske detaljer eller kontakt support."
        ))

        shinyjs::html(
          id   = "console_output",
          html = paste0(
            "\n*** TEKNISK FEJL (aktivitetsplot) ***\n",
            err_msg,
            "\n*************************************\n"
          ),
          add  = TRUE
        )
      }
    )

    shinyjs::enable("run_plot")
  })

  output$activity_plot <- renderPlot(
    {
      req(activity_plot())
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
    },
    width  = 600,
    height = 500,
    res    = 96
  )

  output$download_plot_png <- downloadHandler(
    filename = function() {
      paste0("activity_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(activity_plot())
      grDevices::png(file, width = 8, height = 5, units = "in", res = 300)
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
      grDevices::dev.off()
    }
  )

  output$download_plot_svg <- downloadHandler(
    filename = function() {
      paste0("activity_plot_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(activity_plot())
      grDevices::svg(file, width = 8, height = 5)
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
      grDevices::dev.off()
    }
  )

  # ------------------- LOAD DATA FOR SPECIALTY DROPDOWN -------------------- #

  observe({
  newest <- get_newest_rds_file(data_dir)
  if (is.null(newest)) return()

  df <- tryCatch(
    readRDS(newest),
    error = function(e) NULL
  )
  if (is.null(df)) return()
  if (!"specialeCorrected" %in% names(df)) return()

  specs <- sort(unique(na.omit(df$specialeCorrected)))

  # Ensure "Hospital" is available as a virtual "speciale" that triggers sektor filtering
  if (!"Hospital" %in% specs) {
    specs <- c(specs, "Hospital")
  }

  # Order: Hospital, Almen praksis, then the rest alphabetically
  priority <- c("Hospital", "Almen praksis")
  priority_in_specs <- priority[priority %in% specs]
  rest <- setdiff(specs, priority_in_specs)
  specs_ordered <- c(priority_in_specs, sort(rest))

  updateSelectInput(
    session,
    "specialeToggle",
    choices = c("Alle", specs_ordered),
    selected = "Alle"
  )
})

  # -------------------------- SVARTYPE PLOT (Tidligere aktiviteter) -------------------------- #

  observeEvent(input$run_svartype_plot, {
    shinyjs::disable("run_svartype_plot")

    shinyjs::html(
      id   = "console_output",
      html = "Starter script til 'Tidligere aktiviteter'...\n\n",
      add  = TRUE
    )

    script_path_plot <- file.path(
      here("statistik", "scripts"),
      "plot svartype.R"
    )

    if (!file.exists(script_path_plot)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0(
          "FEJL: 'plot svartype' script ikke fundet:\n",
          script_path_plot, "\n"
        ),
        add  = TRUE
      )

      script_status(list(
        type    = "error",
        message = "Scriptet 'plot svartype.R' blev ikke fundet. Kontakt support."
      ))

      shinyjs::enable("run_svartype_plot")
      return(invisible(NULL))
    }

    tryCatch(
      {
        plot_env <- new.env(parent = globalenv())

        # Map new UI inputs to old script toggles
        plot_env$showStackCount  <- (input$countMode == "Opdelt")
        plot_env$showTotalCount  <- (input$countMode == "Samlet")
        plot_env$timeGranularity <- input$timeGranularity
        plot_env$showTrendlineToggle   <- isTRUE(input$showTrendlineToggle)
        plot_env$groupSvarToggle       <- isTRUE(input$groupSvarToggle)
        plot_env$psykiatrikonf_AndreToggle <- isTRUE(input$psykiatrikonf_AndreToggle)
        plot_env$sektorToggle          <- "Alle"  # no sektor dropdown in UI anymore
        plot_env$specialeToggle        <- input$specialeToggle
        plot_env$regionToggle          <- input$regionToggle
        plot_env$svartypeFilterToggle  <- input$svartypeFilterToggle
        plot_env$showCountInLegend     <- isTRUE(input$showCountInLegend)

        # Map dates to start_*/end_* as before
        from <- as.Date(input$from_date)
        to   <- as.Date(input$to_date)

        plot_env$start_year  <- as.integer(format(from, "%Y"))
        plot_env$start_month <- as.integer(format(from, "%m"))
        plot_env$start_day   <- as.integer(format(from, "%d"))
        plot_env$end_year    <- as.integer(format(to, "%Y"))
        plot_env$end_month   <- as.integer(format(to, "%m"))
        plot_env$end_day     <- as.integer(format(to, "%d"))

        withCallingHandlers(
          {
            message("Kører 'plot svartype.R': ", script_path_plot)
            source(script_path_plot, local = plot_env)
          },
          message = function(m) {
            shinyjs::html(
              id   = "console_output",
              html = paste0(m$message, "\n"),
              add  = TRUE
            )
            invokeRestart("muffleMessage")
          }
        )

        if (!exists("p", envir = plot_env, inherits = FALSE)) {
          stop("Plot-objekt 'p' ikke fundet efter kørsel af 'plot svartype.R'.")
        }

        svartype_plot_obj(get("p", envir = plot_env))

        shinyjs::html(
          id   = "console_output",
          html = "\n'Tidligere aktiviteter' plot-script færdigt.\n",
          add  = TRUE
        )

      },
      error = function(e) {
        err_msg <- e$message

        script_status(list(
          type    = "error",
          message = "Der opstod en fejl under opdatering af 'Tidligere aktiviteter'. Se tekniske detaljer eller kontakt support."
        ))

        shinyjs::html(
          id   = "console_output",
          html = paste0(
            "\n*** TEKNISK FEJL (svartype-plot) ***\n",
            err_msg,
            "\n****************************************\n"
          ),
          add  = TRUE
        )
      }
    )

    shinyjs::enable("run_svartype_plot")
  })

  output$svartype_plot <- renderPlot(
    {
      req(svartype_plot_obj())
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
    },
    width  = 800,   # wider in app
    height = 650,   # taller to match original ggsave height = 6.5
    res    = 96
  )

  output$download_svartype_png <- downloadHandler(
    filename = function() {
      paste0("svartype_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(svartype_plot_obj())
      grDevices::png(file, width = 10, height = 6.5, units = "in", res = 300)
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
      grDevices::dev.off()
    }
  )

  output$download_svartype_svg <- downloadHandler(
    filename = function() {
      paste0("svartype_plot_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(svartype_plot_obj())
      grDevices::svg(file, width = 10, height = 6.5)
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
      grDevices::dev.off()
    }
  )

  # -------------------------- STATUS MESSAGE -------------------------- #

  output$status_message <- renderUI({
    status <- script_status()
    if (!is.null(status)) {
      div(
        class = paste("status-message", if (status$type == "error") "status-error" else "status-ok"),
        status$message
      )
    }
  })
}

shinyApp(ui, server)
