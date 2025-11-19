# statistik_app.R for statistics page
library(shiny)
library(here)
library(shinyjs)
library(htmltools)   # for htmlEscape if needed

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
        margin: 20px auto;
        border-radius: 4px;

        /* Make it larger */
        min-height: 250px;
        max-height: 65vh;
        width: 100%;
        max-width: 1000px;

        overflow-y: auto;
        border: 1px solid #555;

        /* Preserve line breaks and wrap long lines */
        white-space: pre-wrap;
        word-wrap: break-word;
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
    uiOutput("status_message"),

    # Console output area:
    # use a plain div so shinyjs::html() can write into it directly
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

  observeEvent(input$download_data, {
    # Disable button during execution
    shinyjs::disable("download_data")

    # Clear previous console and status
    shinyjs::html("console_output", "")
    script_status(NULL)

    # Inform user that execution started
    shinyjs::html(
      id   = "console_output",
      html = "Starter download- og forberedelsesscript...\n",
      add  = TRUE
    )

    script_path <- file.path(here("statistik", "scripts"), "download, prepare, save.R")

    tryCatch({

      # Stream messages from the script to the 'console_output' div
      withCallingHandlers(
        {
          # Run your script; use message() in this script to send updates here
          source(script_path, local = TRUE)
        },
        message = function(m) {
          # Append each message line to the console
          shinyjs::html(
            id   = "console_output",
            html = paste0(m$message, "\n"),
            add  = TRUE
          )
          # Avoid also printing to the real console
          invokeRestart("muffleMessage")
        }
      )

      # If we got here, script finished successfully
      script_status(list(
        type    = "success",
        message = "Data succesfuldt downloadet og forberedt!"
      ))

      shinyjs::html(
        id   = "console_output",
        html = "\nScript færdigt.\n",
        add  = TRUE
      )

    }, error = function(e) {

      # On error, show status and error message in the console
      script_status(list(
        type    = "error",
        message = paste("Fejl under kørsel:", e$message)
      ))

      shinyjs::html(
        id   = "console_output",
        html = paste0(
          "\nERROR: ",
          htmltools::htmlEscape(e$message),
          "\n"
        ),
        add  = TRUE
      )
    })

    # Re-enable button after execution
    shinyjs::enable("download_data")
  })

  # Render status message
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
