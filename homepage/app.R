# app.R for homepage (revised)

library(shiny)
library(shinymanager)

# 1) adjust this to point at whichever file you want to show the update date of
DATA_FILE <- "/srv/shiny-server/kfaapps/data-scripts/data/merged_data.rds"

ui <- fluidPage(
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
    "))
  ),

  div(class = "content",
    # Main title
    div(class = "header", "Klinisk Farmakologi, AUH"),

    # Section 1: Redskaber til medicinmonitorering
    div(class = "subheader", "Redskaber til medicinmonitorering"),
    div(class = "links",
        tags$a(
          href = "https://kfaapps.au.dk/lmgrupper/",
          class = "link-button",
          "Opdaterede prisoversigter for lægemiddelgrupper"
        ),
        tags$a(
          href = "https://kfaapps.au.dk/lmforbrug/",
          class = "link-button",
          "Forbrug og tilskudsudgifter over tid"
        )
    ),

    # Section 2: Batchopslag af ATC-koder
    div(class = "subheader", "Batchopslag af ATC-koder"),
    div(class = "links",
        tags$a(
          href = "https://kfaapps.au.dk/lister/",
          class = "link-button",
          "Lister og interaktioner"
        ),
        tags$a(
          href = "https://kfaapps.au.dk/bivirkninger/",
          class = "link-button",
          "Bivirkninger fra pro.medicin.dk"
        )
    ),

    # Section 3: Visualiseringer
    div(class = "subheader", "Visualiseringer"),
    div(class = "links",
        tags$a(
          href = "https://kfaapps.au.dk/farmakokinetik/",
          class = "link-button",
          "Farmakokinetik"
        ),
        tags$a(
          href = "https://kfaapps.au.dk/compliance/",
          class = "link-button",
          "Compliance"
        )
    ),

    # NEW SECTION: Statistik over aktiviteter
    div(class = "subheader", "Statistik over aktiviteter"),
    div(class = "links",
        tags$a(
          href = "https://kfaapps.au.dk/statistik/",
          class = "link-button",
          "Visualisering af afdelingsaktiviteter"
        )
    )
  ),

  # Footer (now at the true bottom)
  div(class = "footer", textOutput("footer_text"))
)

server <- function(input, output, session) {
  # Compute last-update date
  last_date <- if (file.exists(DATA_FILE)) {
    format(file.info(DATA_FILE)$mtime, "%d-%m-%Y")
  } else {
    format(Sys.Date(), "%d-%m-%Y")
  }

  output$footer_text <- renderText({
    paste0(
      "Senest opdateret ", last_date,
      " | Support: Frederik Kraglund, frekra@biomed.au.dk"
    )
  })
}

shinyApp(ui, server)


