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
        padding: 60px 20px 40px;
        display: flex;
        flex-direction: column;
        min-height: 100vh;
      }
      .content {
        flex: 1;
      }
      .header {
        font-size: 3em;
        margin-bottom: 0.2em;
      }
      .subheader {
        font-size: 1.5em;
        margin-top: 1.5em;
        margin-bottom: 0.5em;
        color: #555;
      }
      .links {
        margin: 1.5em 0;
      }
      .link-button {
        display: inline-block;
        margin: 0.5em;
        padding: 1em 2em;
        font-size: 1.2em;
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
        padding: 20px 0;
        font-size: 0.9em;
        color: #555;
      }
      .status-message {
        margin-top: 1em;
        padding: 1em;
        border-radius: 4px;
        font-weight: bold;
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
        padding: 15px;
        margin: 10px auto 20px auto;
        border-radius: 4px;

        min-height: 200px;
        max-height: 60vh;
        width: 100%;
        max-width: 1100px;

        overflow-y: auto;
        border: 1px solid #555;

        white-space: pre-wrap;
        word-wrap: break-word;
      }

      .console-caption {
        font-size: 0.9em;
        color: #777;
        margin-top: 5px;
      }
    "))
  ),

  div(
    class = "content",
    # Main title
    div(class = "header", "Statistik over afdelingens aktiviteter"),

    # Data preparation section
    div(class = "subheader", "Dataforberedelse"),
    div(
      class = "links",
      actionButton(
        "download_data",
        "Download Sharepoint data",
        class = "link-button"
      )
    ),

    # Nice, user-facing status box
    uiOutput("status_message"),

    # Toggle for technical log
    div(
      class = "links",
      actionLink("toggle_console", "Vis/skjul tekniske detaljer")
    ),
    div(
      class = "console-caption",
      "Tekniske detaljer vises kun for fejlsøgning (kan ignoreres for almindelig brug)."
    ),

    # Technical console (initially hidden)
    div(
      id = "console_output",
      class = "console-output",
      ""   # start empty
    )
  ),

  # Footer
  div(
    class = "footer",
    "Support: Ole Andersen, oleemil@biomed.au.dk"
  )
)

server <- function(input, output, session) {

  # Reactive value to track script execution status
  script_status <- reactiveVal(NULL)

  # Hide the console by default
  shinyjs::hide("console_output")

  # Toggle visibility of technical console
  observeEvent(input$toggle_console, {
    shinyjs::toggle("console_output")
  })

  observeEvent(input$download_data, {
    shinyjs::disable("download_data")

    # Clear previous console and status
    shinyjs::html("console_output", "")
    script_status(NULL)

    # User-friendly info in console start
    shinyjs::html(
      id   = "console_output",
      html = "Starter download- og forberedelsesscript...\n\n",
      add  = TRUE
    )

    # Path to your script
    script_path <- file.path(
      here("statistik", "scripts"),
      "download, prepare, save.R"
    )

    # If script does not exist, show an error directly in the UI + console
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

    # Try to run script and stream messages (technical) to console
    tryCatch(
      {

        withCallingHandlers(
          {
            # High-level technical note
            message("Kører script: ", script_path)

            # Run your script.
            # IMPORTANT: inside this script, use message(\"...\") for progress updates.
            source(script_path, local = TRUE)
          },
          message = function(m) {
            # Append each message line to the technical console
            shinyjs::html(
              id   = "console_output",
              html = paste0(m$message, "\n"),
              add  = TRUE
            )
            # Prevent duplicate printing in the real R console
            invokeRestart("muffleMessage")
          }
        )

        # If we got here, script finished successfully
        script_status(list(
          type    = "success",
          message = "Data er downloadet og forberedt uden fejl."
        ))

        shinyjs::html(
          id   = "console_output",
          html = "\nScript færdigt.\n",
          add  = TRUE
        )
      },
      error = function(e) {

        err_msg <- e$message

        # Special friendly message if Azure-token mangler/er udløbet
        if (grepl("Azure authentication missing or expired", err_msg, fixed = TRUE)) {
          script_status(list(
            type    = "error",
            message = paste(
              "Azure-login er udløbet eller mangler.",
              "Kontakt systemansvarlig for at genaktivere forbindelsen til SharePoint."
            )
          ))
        } else {
          # Generic user-facing error
          script_status(list(
            type    = "error",
            message = "Der opstod en fejl under download/forberedelse af data. Se tekniske detaljer eller kontakt support."
          ))
        }

        # Full technical error in console only
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

  # Render status message (user-facing)
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
