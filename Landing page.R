library(shiny)

# Define UI
ui <- fluidPage(
  
  # Custom CSS and JavaScript for styling and redirection
  tags$head(
    tags$style(HTML("
      .container {
        display: flex;
        justify-content: center;
        align-items: center;
        flex-direction: column;
        min-height: 100vh;
        text-align: center;
      }
      .card {
        background-color: white;
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0, 0, 0, 0.1);
        padding: 20px;
        width: 320px; /* Slightly wider to accommodate buttons */
        text-align: center;
      }
      body {
        background: white;
        font-family: 'Roboto', sans-serif;
      }
      .btn-custom {
        background-color: #0033A0; /* Pantone 287 EC */
        color: white;
        width: 100%;
        padding: 20px;
        font-size: 18px;
        margin-bottom: 15px; /* Smaller margin between buttons */
        text-align: left;
        border: none;
      }
      .btn-custom:hover {
        background-color: #00217A; /* Slightly darker shade of Pantone 287 EC */
        transform: scale(1.05);
        color: white;
      }
      .title {
        font-family: 'Roboto', sans-serif;
        color: #0033A0;
        margin-top: 20px;
        margin-bottom: 30px;
        text-align: center;
      }
      .footer {
        position: fixed;
        left: 0;
        bottom: 0;
        width: 100%;
        background-color: #0033A0;
        color: white;
        text-align: center;
        padding: 10px 0;
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('navigate', function(url) {
        window.location.href = url;
      });
    "))
  ),
  
  # UI layout
  div(class = "container",
      h1("Klinisk Farmakologi, AUH", class = "title"),
      h3("Redskaber til medicingennemgang:", class = "title"),
      div(class = "card",
          actionButton("app1", "Lister og interaktioner", class = "btn-custom"),
          actionButton("app2", "Bivirkninger fra pro.medicin.dk", class = "btn-custom"),
          actionButton("app3", "Effektueringer og compliance", class = "btn-custom"),
          actionButton("app4", "Farmakokinetik", class = "btn-custom")
      )
  ),
  
  tags$div(class = "footer", "Sidst opdateret 27-02-2025 | Support: Frederik Kraglund (frekra@biomed.au.dk)")
)

# Define server logic
server <- function(input, output, session) {
  observeEvent(input$app1, {
    session$sendCustomMessage(type = "navigate", message = "https://kfaarhus2.shinyapps.io/Lister/")
  })
  observeEvent(input$app2, {
    session$sendCustomMessage(type = "navigate", message = "https://kfaarhus.shinyapps.io/Promedscraper/")
  })
  observeEvent(input$app3, {
    session$sendCustomMessage(type = "navigate", message = "https://kfaarhus2.shinyapps.io/Compliance/")
  })
  observeEvent(input$app4, {
    session$sendCustomMessage(type = "navigate", message = "https://kfaarhus2.shinyapps.io/Farmakokinetik/")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

