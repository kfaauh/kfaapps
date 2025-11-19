# statistik_app.R for statistics page
library(shiny)
library(here)
library(shinyjs)

ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  tags$head(
    tags$style(HTML("
      /* Your existing CSS styles */
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
      /* CONSOLE STYLING - LARGER */
      .console-output {
        background-color: #2b2b2b;
        color: #f8f8f8;
        font-family: 'Courier New', monospace;
        text-align: left;
        padding: 20px;
        margin: 20px auto;
        border-radius: 6px;
        min-height: 200px;
        height: 400px;
        overflow-y: auto;
        width: 90%;
        border: 2px solid #666;
        font-size: 14px;
        white-space: pre-wrap;
      }
    "))
  ),

  div(class = "content",
    # Main title
    div(class = "header", "Statistik over afdelingens aktiviteter"),

    # Data preparation section
    div(class = "subheader", "Dataforberedelse"),
    div(class = "links",
        actionButton(
          "download_data",
          "Download Sharepoint data",
          class = "link-button"
        )
    ),
    uiOutput("status_message"),

    # Console output area with styling - CHANGED to htmlOutput
    div(class = "console-output",
        htmlOutput("console_output")
    )
  ),

  # Footer
  div(class = "footer",
      "Support: Ole Andersen, oleemil@biomed.au.dk"
  )
)

server <- function(input, output, session) {

  # Reactive value to track script execution status
  script_status <- reactiveVal(NULL)

  # Initialize console with empty content
  output$console_output <- renderUI({
    HTML("Click 'Download Sharepoint data' to run the script and see output here.")
  })

  # Observe download data button click - USING withCallingHandlers
  observeEvent(input$download_data, {
    # Disable button during execution
    shinyjs::disable("download_data")

    # Clear the console
    shinyjs::html("console_output", "")

    tryCatch({
      # Get script path
      script_path <- file.path(here("statistik", "scripts"), "download, prepare, save.R")

      # Use withCallingHandlers to capture all output
      withCallingHandlers(
        {
          # Add starting message
          shinyjs::html("console_output", "=== Starting script execution ===<br>", add = TRUE)
          shinyjs::html("console_output", paste("Script path:", script_path, "<br>"), add = TRUE)
          shinyjs::html("console_output", paste("File exists:", file.exists(script_path), "<br>"), add = TRUE)

          if(file.exists(script_path)) {
            shinyjs::html("console_output", "--- Running script ---<br>", add = TRUE)
            source(script_path, echo = TRUE, max.deparse.length = 1000)
            shinyjs::html("console_output", "--- Script completed successfully ---<br>", add = TRUE)
          } else {
            stop("Script file not found")
          }
        },
        message = function(m) {
          shinyjs::html(id = "console_output", html = paste0(m$message, "<br>"), add = TRUE)
        },
        warning = function(w) {
          shinyjs::html(id = "console_output", html = paste0("WARNING: ", w$message, "<br>"), add = TRUE)
        },
        error = function(e) {
          shinyjs::html(id = "console_output", html = paste0("ERROR: ", e$message, "<br>"), add = TRUE)
          stop(e)
        }
      )

      # Set success status
      script_status(list(type = "success", message = "Data succesfuldt downloadet og forberedt!"))

    }, error = function(e) {
      # Set error status
      script_status(list(type = "error", message = paste("Fejl under kørsel:", e$message)))
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
