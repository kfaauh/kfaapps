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
      }
      .last-sync-ok {
        color: #155724;
        font-weight: 600;
      }

      .section-separator {
        border-top: 1px solid #cccccc;
        margin: 12px auto;
        width: 85%;
      }
      .section-separator-thin {
        border-top: 1px solid #dddddd;
        margin: 4px auto;
        width: 80%;
      }

      .download-links {
        margin-top: 0.1em;
        margin-bottom: 0.3em;
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

    # Seneste synkronisering (neutral/green depending on last action)
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

    div(class = "section-separator-thin"),

    # Plot area
    div(
      class = "links",
      plotOutput("activity_plot", height = "500px")
    ),

    # Horizontal line between plot and download buttons (only when plot exists)
    conditionalPanel(
      condition = "output.plot_available == true",
      div(class = "section-separator-thin")
    ),

    # Download buttons
    div(
      class = "links download-links",
      conditionalPanel(
        condition = "output.plot_available == true",
        downloadButton("download_plot_png", "Download plot (PNG)"),
        downloadButton("download_plot_svg", "Download plot (SVG)")
      )
    ),

    # Separator below download buttons
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

  # For controlling download buttons & separator
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
  sync_ok <- reactiveVal(FALSE)  # becomes TRUE after a successful sync in this session

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
      # only set sync_ok(TRUE) when called from a successful sync event
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

        # On success: update last sync + mark as ok (no extra success text box)
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
            "1. Login på server fra AU net: ",
            "<code>ssh -J au309166@ssh.au.dk au309166@kfaapps.uni.au.dk</code><br>",
            "2. Kopier dette til server terminalen:<br>",
            "<pre>",
            \"R --vanilla << 'EOF'\n\",
            \"library(Microsoft365R)\n\n\",
            \"cat(\\\"\\\\nStarting Microsoft365R device-code login...\\\\n\\\\n\\\")\n\n\",
            \"# One-time (or occasional) device-code login\n\",
            \"site_list <- list_sharepoint_sites(auth_type = \\\"device_code\\\")\n\n\",
            \"cat(\\\"\\\\nAuthentication complete. Token cached for future Shiny sessions.\\\\n\\\")\n\",
            \"q(save = \\\"no\\\")\n\",
            \"EOF\",
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

  # Plot in app: use 600x400 device (good size), container 500px
  output$activity_plot <- renderPlot(
    {
      req(activity_plot())
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
    },
    width  = 600,
    height = 400,
    res    = 96
  )

  # Downloads: keep 8x5 in at 300dpi
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
        class = paste("status-message", paste0("status-", status$type)),
        status$message
      )
    }
  })
}

shinyApp(ui, server)
