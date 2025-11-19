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
        margin: 20px auto;
        border-radius: 4px;

        /* Bigger console box */
        min-height: 300px;
        max-height: 70vh;
        width: 100%;
        max-width: 1100px;

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

    # Console output area: we fill this with shinyjs::html()
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

  # ---- helpers to monkey-patch cat() inside packages -----------------------

  patch_cat_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return(invisible(NULL))

    ns <- asNamespace(pkg)

    # Try to grab the package's cat binding (import from base)
    orig <- tryCatch(
      get("cat", envir = ns, inherits = FALSE),
      error = function(e) NULL
    )
    if (is.null(orig)) return(invisible(NULL))

    # Save original under a private name so we can restore it later
    assign("cat_for_shiny_original", orig, envir = ns)

    # New cat: send output to message(), which we already capture with withCallingHandlers
    new_cat <- function(..., file = "", sep = " ", fill = FALSE, labels = NULL, append = FALSE) {
      txt <- paste(..., sep = sep, collapse = "")
      # cat() doesn't automatically add newline unless you include \n; we just forward as-is
      message(txt)
    }

    # Replace the binding (it may be locked because it's imported from base)
    tryCatch({
      if (bindingIsLocked("cat", ns)) unlockBinding("cat", ns)
      assign("cat", new_cat, envir = ns)
      lockBinding("cat", ns)
    }, error = function(e) {
      # If patching fails, just leave things as they are
      message("Kunne ikke patche cat() i pakken ", pkg, ": ", conditionMessage(e))
    })

    invisible(NULL)
  }

  unpatch_cat_package <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) return(invisible(NULL))

    ns <- asNamespace(pkg)
    if (!exists("cat_for_shiny_original", envir = ns, inherits = FALSE)) {
      return(invisible(NULL))
    }

    orig <- get("cat_for_shiny_original", envir = ns, inherits = FALSE)

    tryCatch({
      if (bindingIsLocked("cat", ns)) unlockBinding("cat", ns)
      assign("cat", orig, envir = ns)
      lockBinding("cat", ns)
      rm("cat_for_shiny_original", envir = ns)
    }, error = function(e) {
      message("Kunne ikke reset cat() i pakken ", pkg, ": ", conditionMessage(e))
    })

    invisible(NULL)
  }

  # -------------------------------------------------------------------------

  observeEvent(input$download_data, {
    shinyjs::disable("download_data")

    # Clear previous console and status
    shinyjs::html("console_output", "")
    script_status(NULL)

    script_path <- file.path(
      here("statistik", "scripts"),
      "download, prepare, save.R"
    )

    # Always show at least something
    shinyjs::html(
      id   = "console_output",
      html = paste0("Knap trykket – kører script:\n", script_path, "\n\n"),
      add  = TRUE
    )

    if (!file.exists(script_path)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0("FEJL: Script ikke fundet:\n", script_path, "\n"),
        add  = TRUE
      )
      script_status(list(
        type    = "error",
        message = "Scriptfilen blev ikke fundet."
      ))
      shinyjs::enable("download_data")
      return(invisible(NULL))
    }

    # --- PATCH cat() INSIDE AzureAuth & Microsoft365R ----------------------
    # So that any cat() calls (e.g. device-code URL) become message() calls.

    patch_cat_package("AzureAuth")
    patch_cat_package("Microsoft365R")

    # Ensure we restore original cat() and re-enable button afterwards
    on.exit({
      unpatch_cat_package("AzureAuth")
      unpatch_cat_package("Microsoft365R")
      shinyjs::enable("download_data")
    }, add = TRUE)

    # -----------------------------------------------------------------------

    tryCatch({

      withCallingHandlers(
        {
          # This message will always appear in the console
          message("Starter download- og forberedelsesscript...")

          # Run your script (which does Azure auth etc.)
          # Any message() AND cat() from AzureAuth/Microsoft365R should now appear here.
          source(script_path, local = TRUE)
        },
        message = function(m) {
          shinyjs::html(
            id   = "console_output",
            html = paste0(m$message, "\n"),
            add  = TRUE
          )
          # Don't also spam the R console
          invokeRestart("muffleMessage")
        }
      )

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
      script_status(list(
        type    = "error",
        message = paste("Fejl under kørsel:", e$message)
      ))

      shinyjs::html(
        id   = "console_output",
        html = paste0("\nERROR: ", e$message, "\n"),
        add  = TRUE
      )
    })
  })

  # Status box
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
