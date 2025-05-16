# app.R for homepage
library(shiny)
library(shinymanager)

credentials <- data.frame(
  user = "KFA",
  password = "kfekfa123",
  stringsAsFactors = FALSE
)

# 1) adjust this to point at whichever file you want to show the update date of
DATA_FILE <- "/srv/shiny-server/kfaapps/data-scripts/data/merged_data.rds"

ui <- secure_app(
fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: white;
        color: #0033A0;          /* Pantone 287 */
        font-family: Arial, sans-serif;
        text-align: center;
        padding: 60px;
      }
      .header {
        font-size: 3em;
        margin-bottom: 0.2em;
      }
      .subheader {
        font-size: 1.5em;
        margin-top: 0;
        color: #555;
      }
      .links {
        margin: 2em 0;
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
        margin-top: 4em;
        font-size: 0.9em;
        color: #555;
      }
    "))
  ),

  div(class = "header",    "Klinisk Farmakologi, AUH"),
  div(class = "subheader", "Redskaber til medicinmonitorering"),

  div(class="links",
    tags$a(
      href="https://kfaapps.au.dk/lmgrupper/",
      class="link-button",
      "Opdaterede prisoversigter for lægemiddelgrupper"
    ),
    tags$a(
      href="https://kfaapps.au.dk/lmforbrug/",
      class="link-button",
      "Forbrug og tilskudsudgifter over tid"
    )
  ),

  div(class="footer", textOutput("footer_text"))
)
)

server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
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
