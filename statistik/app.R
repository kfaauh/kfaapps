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

  # Initialize console with empty content
  output$console_output <- renderUI({
    HTML("Click 'Download Sharepoint data' to run the script and see output here.")
  })

  # Observe download data button click - DEBUG VERSION
  observeEvent(input$download_data, {
    # Disable button during execution
    shinyjs::disable("download_data")

    # Clear the console
    shinyjs::html("console_output", "")

    tryCatch({
      # Get script path
      script_path <- file.path(here("statistik", "scripts"), "download, prepare, save.R")

      # First, let's test if the script exists and can be read
      shinyjs::html("console_output", paste("Script path:", script_path, "<br>"), add = TRUE)
      shinyjs::html("console_output", paste("File exists:", file.exists(script_path), "<br>"), add = TRUE)

      if(file.exists(script_path)) {
        shinyjs::html("console_output", "File exists! Reading script content...<br>", add = TRUE)

        # Read the first few lines of the script to verify it's accessible
        script_content <- readLines(script_path, n = 10)
        shinyjs::html("console_output", "First 10 lines of script:<br>", add = TRUE)
        for(line in script_content) {
          shinyjs::html("console_output", paste(line, "<br>"), add = TRUE)
        }

        shinyjs::html("console_output", "<br>--- Attempting to run script ---<br>", add = TRUE)

        # Try a simple test first - run a basic R command
        shinyjs::html("console_output", "Testing basic R command: 2+2<br>", add = TRUE)
        result <- 2 + 2
        shinyjs::html("console_output", paste("Result:", result, "<br>"), add = TRUE)

        # Now try to source the script with capture.output
        shinyjs::html("console_output", "<br>--- Sourcing script with capture.output ---<br>", add = TRUE)
        captured <- capture.output({
          source(script_path, echo = TRUE)
        }, type = "output")

        if(length(captured) > 0) {
          shinyjs::html("console_output", paste("Captured", length(captured), "lines of output<br>"), add = TRUE)
          for(line in captured) {
            shinyjs::html("console_output", paste(line, "<br>"), add = TRUE)
          }
        } else {
          shinyjs::html("console_output", "No output was captured from the script<br>", add = TRUE)
        }

      } else {
        shinyjs::html("console_output", "ERROR: Script file not found!<br>", add = TRUE)
        stop("Script file not found")
      }

      # Set success status
      script_status(list(type = "success", message = "Script execution completed!"))

    }, error = function(e) {
      # Set error status
      script_status(list(type = "error", message = paste("Fejl under kørsel:", e$message)))

      # Show error in console
      shinyjs::html("console_output", paste("<br>ERROR:", e$message, "<br>"), add = TRUE)
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
