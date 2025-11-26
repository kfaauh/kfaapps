library(shiny)
library(here)
library(shinyjs)

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
        margin: 0.6em 0;
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
        margin: 12px auto;
        width: 85%;
      }
      .section-separator-thin {
        border-top: 1px solid #dddddd;
        margin: 2px auto;
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
      .download-links {
        margin: 0 !important;
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

  /* Remove extra margin from selectInput wrapper inside dropdown */
  .dropdown-container .shiny-input-container {
    margin-bottom: 0 !important;
  }

      /* Make selectize dropdown ~170px wide and keep text on one line */
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

  /* Remove vertical margins from the Download button so it lines up */
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
    "))
  ),

  div(
    class = "content",

    # Main title
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

    # Seneste synkronisering (neutral / green after success)
    div(
      class = "links",
      uiOutput("last_sync")
    ),

    # Error messages (if any)
    div(
      class = "links",
      uiOutput("status_message")
    ),

    div(class = "section-separator"),

    # ---- Aktivitetsstatus ----
    div(class = "subheader", "Aktivitetsstatus"),

    div(
      class = "links",
      actionButton(
        "run_plot",
        "Aktivitetsstatus",
        class = "link-button"
      )
    ),

        # Plot + download section (only when plot exists)
    conditionalPanel(
      condition = "output.plot_available == true",
      div(
        class = "plot-container",
        plotOutput("activity_plot", inline = TRUE)  # Added inline = TRUE
      ),
      div(
        class = "download-links",
        downloadButton("download_plot_png", "Download PNG", class = "small-button"),
        downloadButton("download_plot_svg", "Download SVG", class = "small-button")
      )
    ),

    # Always show horizontal line above KFE/KFA section
    div(class = "section-separator"),

      # ---- KFE/KFA tilhørsforhold ----
    div(
      class = "kfe-kfa-section",
      div(class = "kfe-kfa-title", "Opdater tilhørsforhold til KFE og KFA"),
      div(class = "kfe-kfa-subtitle", "Nyeste Excel anvendes i scripts"),
      # Dropdown section above buttons
      uiOutput("download_dropdown_ui"),
      div(
        class = "kfe-kfa-buttons",
        # Hidden file input for upload
        tags$div(
          style = "display: none;",
          fileInput("excel_file", NULL,
                    accept = c(".xlsx", ".xls"),
                    buttonLabel = "Upload")
        ),
        actionButton("upload_excel", "Upload", class = "small-button"),
        downloadButton("download_original", "Download Original", class = "small-button")
      )
    ),

    # Horizontal line above technical details
    div(class = "section-separator"),

    # Foldable technical console
    tags$details(
      tags$summary("Tekniske detaljer (klik for at folde ud)"),
      div(
        id = "console_output",
        class = "console-output",
        ""   # start empty
      )
    )
  ),

  # Footer
  div(
    class = "footer",
    "Support: Ole Andersen, oleemil@biomed.au.dk"
  )
)

server <- function(input, output, session) {

  # Only used for errors now
  script_status <- reactiveVal(NULL)

  # For the activity plot
  activity_plot <- reactiveVal(NULL)

  # For controlling download buttons & plot section
  output$plot_available <- reactive({
    !is.null(activity_plot())
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)

  # Track last sync time and whether a sync was done in this session
  data_dir <- here("statistik", "Data", "Azure data")

  get_newest_rds_file <- function(directory_path) {
    rds_files <- list.files(
      path   = directory_path,
      pattern = "^azure_.*\\.rds$",  # azure_YYYY-MM-DD.rds
      full.names = TRUE
    )
    if (length(rds_files) == 0) return(NULL)
    file_info <- file.info(rds_files)
    rds_files[which.max(file_info$mtime)]
  }

  last_sync_time <- reactiveVal(NA)
  sync_ok <- reactiveVal(FALSE)  # TRUE after a successful sync in this session

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

  # Initialize from existing files (if any) but not marked as "just synced"
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

  # -------------------------- KFE/KFA TILHØRSFORHOLD FUNCTIONS ------------------------- #

  # Define folders
  main_folder <- here("statistik", "data", "tilknytning, KFE_KFA")
  original_folder <- here("statistik", "data", "tilknytning, KFE_KFA", "Original")

  # Ensure folders exist
  observe({
    if (!dir.exists(main_folder)) dir.create(main_folder, recursive = TRUE)
    if (!dir.exists(original_folder)) dir.create(original_folder, recursive = TRUE)
  })

  # Get list of Excel files in main folder
  get_excel_files <- reactive({
    files <- list.files(main_folder, pattern = "\\.xlsx$", full.names = FALSE)
    if (length(files) == 0) return(NULL)

    # Sort by modification time (newest first)
    file_info <- file.info(file.path(main_folder, files))
    files_ordered <- files[order(file_info$mtime, decreasing = TRUE)]
    return(files_ordered)
  })

  # Render dropdown for downloading Excel files
  output$download_dropdown_ui <- renderUI({
    files <- get_excel_files()
    if (is.null(files)) {
      return(NULL)
    }

    tagList(
      div(
        class = "dropdown-container",
        selectInput("excel_files", NULL,
                    choices = files,
                    width = '170px',
                    selectize = TRUE),
        downloadButton("download_selected", "Download valgt", class = "small-download-button")
      )
    )
  })

  # Handle file upload - trigger the hidden file input
  observeEvent(input$upload_excel, {
    shinyjs::click("excel_file")
  })

  # Handle the actual file upload
  observeEvent(input$excel_file, {
    req(input$excel_file)

    # Validate file type
    if (!grepl("\\.xlsx?$", input$excel_file$name, ignore.case = TRUE)) {
      script_status(list(
        type = "error",
        message = "Kun Excel-filer (.xlsx, .xls) er tilladte."
      ))
      return()
    }

    tryCatch({
      # Generate filename with timestamp
      timestamp <- format(Sys.time(), "%Y-%m-%d %H.%M")
      new_filename <- paste0("Tilhørsforhold (", timestamp, ").xlsx")
      new_filepath <- file.path(main_folder, new_filename)

      # Copy uploaded file
      file.copy(input$excel_file$datapath, new_filepath)

      # Clean up old files if more than 10
      files <- list.files(main_folder, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(files) > 10) {
        file_info <- file.info(files)
        files_ordered <- files[order(file_info$mtime)]
        # Remove oldest files beyond limit
        files_to_remove <- files_ordered[1:(length(files) - 10)]
        file.remove(files_to_remove)
      }

      # Success message
      script_status(list(
        type = "message",
        message = paste("Filen er uploadet succesfuldt:", new_filename)
      ))

    }, error = function(e) {
      script_status(list(
        type = "error",
        message = paste("Fejl under upload:", e$message)
      ))
    })
  })

  # Download selected Excel file
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

  # Download original Excel file
  output$download_original <- downloadHandler(
    filename = function() {
      original_files <- list.files(original_folder, pattern = "\\.xlsx$")
      if (length(original_files) > 0) {
        return(original_files[1])
      }
      return("Original.xlsx")
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

    # Clear technical console and error status
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
        html = paste0(
          "FEJL: Script ikke fundet:\n",
          script_path, "\n"
        ),
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

        # On success: update last sync + mark as ok
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
            "2. Kopier dette til server terminalen:<br>",
            "<pre>",
            "R --vanilla << 'EOF'\n",
            "library(Microsoft365R)\n\n",
            "cat('\\nStarting Microsoft365R device-code login...\\n\\n')\n\n",
            "# One-time (or occasional) device-code login\n",
            "site_list <- list_sharepoint_sites(auth_type = 'device_code')\n\n",
            "cat('\\nAuthentication complete. Token cached for future Shiny sessions.\\n')\n",
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

    # Only clear console; keep sync status & errors from sync untouched
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

  # Plot in app: 600x400 device, container 500px
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

  # Downloads: 8x5 in at 300dpi
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

  # Error status box (only errors)
  output$status_message <- renderUI({
    status <- script_status()
    if (!is.null(status)) {
      div(
        class = paste("status-message", if(status$type == "error") "status-error" else "status-ok"),
        status$message
      )
    }
  })
}

shinyApp(ui, server)
