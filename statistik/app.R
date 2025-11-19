# statistik_app.R for statistics page
library(shiny)
library(here)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      /* Ensure the page always fills viewport */
      html, body {
        height: 100%;
        margin: 0;
      }
      body {
        background-color: white;
        color: #0033A0;          /* Pantone 287 */
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
      .status-success {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
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

      .section-separator {
        border-top: 1px solid #cccccc;
        margin: 12px auto;
        width: 85%;
      }

      .section-separator-thin {
        border-top: 1px solid #dddddd;
        margin: 6px auto;
        width: 80%;
      }

      .download-links {
        margin-top: 0.3em;
        margin-bottom: 0.5em;
      }
    "))
  ),

  div(
    class = "content",

    # Main title
    div(class = "header", "Statistik over afdelingens aktiviteter"),

    # ---- Synkronisering (NOW AT TOP) ----
    div(
      class = "links",
      actionButton(
        "download_data",
        "Synkroniser data med Sharepoint",
        class = "link-button"
      )
    ),

    div(
      class = "last-sync-text",
      textOutput("last_sync", inline = TRUE)
    ),

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

    # Horizontal line between plot and download buttons (when plot exists)
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

    # Keep separator below download buttons
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

  # Reactive value to track script execution status (for user-facing box)
  script_status <- reactiveVal(NULL)

  # Reactive to hold the current activity plot (grid grob)
  activity_plot <- reactiveVal(NULL)

  # Indicator for plot existence (for showing download buttons + separator)
  output$plot_available <- reactive({
    !is.null(activity_plot())
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)

  # -------------------------- LAST SYNC TIME ------------------------------- #

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

  update_last_sync <- function() {
    if (!dir.exists(data_dir)) {
      last_sync_time(NA)
      return(invisible(NULL))
    }
    newest <- get_newest_rds_file(data_dir)
    if (is.null(newest)) {
      last_sync_time(NA)
    } else {
      last_sync_time(file.info(newest)$mtime)
    }
    invisible(NULL)
  }

  # Initialize on session start
  update_last_sync()

  output$last_sync <- renderText({
    t <- last_sync_time()
    if (is.null(t) || is.na(t)) {
      "Seneste synkronisering: Ingen filer fundet"
    } else {
      paste0(
        "Seneste synkronisering: ",
        format(t, "%d-%m-%Y %H:%M")
      )
    }
  })

  # -------------------------- DOWNLOAD DATA SCRIPT ------------------------- #

  observeEvent(input$download_data, {
    shinyjs::disable("download_data")

    # Clear technical console
    shinyjs::html("console_output", "")
    script_status(NULL)

    shinyjs::html(
      id   = "console_output",
      html = "Starter download- og forberedelsesscript...\n\n",
      add  = TRUE
    )

    script_path <- file.path(
      here("statistik", "scripts"),
      "download, prepare, save.R"
    )

    # If script does not exist, show error
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

            # Run your script; use message() inside it for progress updates
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

        # If we got here, script finished successfully
        script_status(list(
          type    = "success",
          message = "Data synkroniseret"
        ))

        # Update last sync time after successful save
        update_last_sync()

        shinyjs::html(
          id   = "console_output",
          html = "\nScript færdigt.\n",
          add  = TRUE
        )
      },
      error = function(e) {

        err_msg <- e$message

        # Default user-friendly text
        msg_ui <- "Der opstod en fejl under synkronisering af data. Se tekniske detaljer eller kontakt support."

        # If it's the Azure-auth error from your Section 3 code,
        # show your explicit re-auth instructions.
        if (grepl("Azure authentication missing or expired", err_msg, fixed = TRUE)) {

          msg_ui <- HTML(paste0(
            "Azure skal verificeres:<br>",
            "1. Login på server fra AU net: ",
            "<code>ssh -J au309166@ssh.au.dk au309166@kfaapps.uni.au.dk</code><br>",
            "2. Kopier dette til server terminalen:<br>",
            "<pre>",
            "R --vanilla << 'EOF'\n",
            "library(Microsoft365R)\n\n",
            "cat(\"\\nStarting Microsoft365R device-code login...\\n\\n\")\n\n",
            "# One-time (or occasional) device-code login\n",
            "site_list <- list_sharepoint_sites(auth_type = \"device_code\")\n\n",
            "cat(\"\\nAuthentication complete. Token cached for future Shiny sessions.\\n\")\n",
            "q(save = \"no\")\n",
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

    # Clear console & status for this run
    shinyjs::html("console_output", "")
    script_status(NULL)

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

            # Script expected to create 'combined_grob'
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

        script_status(list(
          type    = "success",
          message = "Aktivitetsplot opdateret (data uændret)."
        ))

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

  # Render the activity plot in the UI
  # Smaller device → browser upscales → larger text on page
  output$activity_plot <- renderPlot(
    {
      req(activity_plot())
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
    },
    width  = 450,
    height = 300,
    res    = 96
  )

  # Download handlers for PNG / SVG (8x5 in @ 300dpi unchanged)
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

  # -------------------------- STATUS BOX RENDERING ------------------------- #

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
