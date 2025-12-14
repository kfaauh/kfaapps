library(shiny)
library(here)
library(shinyjs)
library(lubridate)
library(grid)
library(dplyr)

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

      /* Reduced margins around all section subheadings */
      .subheader {
        font-size: 1.5em;
        margin-top: 0.55em;
        margin-bottom: 0.20em;
        color: #555;
      }

      .links {
        margin: 0.3em 0;
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

      /* Black summary text for Tidligere aktiviteter settings */
      .tidligere-settings > summary {
        color: #000000;
      }

      /* Black summary text for Svartider settings */
      .svartider-settings > summary {
        color: #000000;
      }

      /* Black summary text for Specialefordeling settings */
      .specialefordeling-settings > summary {
        color: #000000;
      }

      .section-separator-first {
        border-top: 2px solid #000000 !important;
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
        display: inline-block;
        padding: 0.2em 0.5em;
        border-radius: 4px;
      }
      .last-sync-ok {
        background-color: #d4edda;
        color: #155724;
        border: 1px solid #c3e6cb;
        font-weight: 600;
      }

      .section-separator {
        border-top: 1px solid #cccccc;
        margin: 8px auto;
        width: 85%;
      }

      /* Dedicated separator just above KFE/KFA section */
      .section-separator-above-kfe {
        border-top: 2px solid #000000 !important;
        width: 85%;
        margin: 8px auto 2px auto;
        position: relative;
        z-index: 2;
        clear: both;
      }

      .section-separator-thin {
        border-top: 1px solid #dddddd;
        margin: 4px auto 6px auto;
        width: 80%;
      }

      /* === Plot + download tight spacing === */
      .plot-container {
        margin: 0 !important;
        padding: 0 !important;
      }
      .plot-container .shiny-plot-output,
      .plot-container .shiny-plot-output > div,
      .plot-container .shiny-plot-output > svg {
        margin: 0 !important;
        padding: 0 !important;
        border: none !important;
      }

      /* Separate download button spacing for the two plots */
      .download-links-activity {
        margin: 0 0 0 0 !important;
        padding: 0 !important;
      }
      .download-links-svartype {
        margin: -10px 0 0 0 !important;
        padding: 0 !important;
      }
      .download-links-svartider {
        margin: -10px 0 0 0 !important;
        padding: 0 !important;
      }
            .download-links-specialefordeling {
        margin: -10px 0 0 0 !important;
        padding: 0 !important;
      }

      /* Small button styling */
      .small-button {
        padding: 0.35em 0.8em !important;
        font-size: 0.8em !important;
        margin: 0.05em !important;
      }

           /* Ensure both Top buttons are blue (match link-button styling) */
     #top8_specialer,
     #top15_specialer {
       background-color: #0033A0 !important;
       color: #ffffff !important;
       border: none !important;
     }


            .top-buttons-stack {
        display: flex;
        flex-direction: column;
        gap: 4px;
        align-items: center;
        justify-content: center;
      }

           /* Make the Top 8 button blue (match link-button styling) */
      #top8_specialer {
        background-color: #0033A0 !important;
        color: #ffffff !important;
        border: none !important;
      }

      .small-download-button {
        padding: 0.3em 0.7em !important;
        font-size: 0.75em !important;
        margin: 0.05em !important;
      }

      /* KFE/KFA collapsible details styling (simplified and safe) */
      .kfe-kfa-details {
        margin: 4px 0 6px;
        border: none;
        border-radius: 4px;
        padding: 8px;
      }

      .kfe-kfa-summary {
        cursor: pointer;
        list-style: none;
        padding: 0;
        margin: 0;
        font-weight: bold;
        color: #0033A0;
        font-size: 1.1em;
      }

      .kfe-kfa-summary::-webkit-details-marker {
        display: none;
      }

      .kfe-kfa-content {
        margin-top: 10px;
        padding-top: 10px;
        border-top: 1px solid #eeeeee;
      }

      .kfe-kfa-subtitle {
        font-size: 0.8em;
        color: #000000;
        margin-top: 0px;
        margin-bottom: 3px;
      }

      .kfe-kfa-buttons {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 4px;
        flex-wrap: wrap;
        margin-top: 1px;
        margin-bottom: 1px;
      }

      /* Dropdown container: dropdown + Download on same line, minimal spacing */
      .dropdown-container {
        display: flex;
        flex-direction: row;
        align-items: center;
        justify-content: center;
        gap: 4px;
        margin-bottom: 1px;
        margin-top: 1px;
      }

      .dropdown-container .shiny-input-container {
        margin-bottom: 0 !important;
      }

      .dropdown-container .selectize-control {
        font-size: 0.6em;
        min-width: 170px;
        max-width: 500px;
      }

      .dropdown-container .selectize-input {
        padding: 2px 6px;
        min-height: 26px;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .dropdown-container .btn {
        margin-top: 0 !important;
        margin-bottom: 0 !important;
      }

      .dropdown-select {
        font-size: 0.7em !important;
        width: 180px !important;
        margin-bottom: 5px;
        padding: 2px 5px !important;
        height: 28px !important;
      }

      /* Controls row for Tidligere aktiviteter (and reused for Svartider + Specialefordeling) */
      .controls-row {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 8px;
        margin-bottom: 10px;
      }
      .controls-row .shiny-input-container {
        margin-bottom: 0.1em;
      }
      .controls-row label {
        font-size: 0.9em;
      }

      /* Compact dropdowns in Tidligere aktiviteter */
      .controls-row .selectize-control.single .selectize-input {
        font-size: 0.8em;
        padding: 1px 1px;
        min-height: 25px !important;
        height: 20px !important;
        line-height: 10px;
      }

      .controls-row .selectize-control.single .selectize-input > .item,
      .controls-row .selectize-control.single .selectize-input > .item + input {
        line-height: 20px;
        padding-top: 0;
        padding-bottom: 0;
      }

      /* Compact, centered date fields (Fra/Til) */
      .controls-row input.form-control {
        height: 24px;
        padding: 2px 4px;
        font-size: 0.8em;
        text-align: center;
        line-height: 1.2;
      }

      /* Smaller text inside the opened dropdown menu */
      .controls-row .selectize-dropdown,
      .controls-row .selectize-dropdown .option {
        font-size: 0.9em;
        line-height: 1.2;
        padding: 2px 4px;
      }

      /* Tighter row for third line of checkboxes */
      .controls-row-tight {
        display: flex;
        flex-wrap: wrap;
        justify-content: center;
        gap: 2px 0px;
        margin-top: -4px;
        margin-bottom: 0;
        width: 100%;
        max-width: 1100px;
        margin-left: auto;
        margin-right: auto;
      }

      /* Wrapper for each checkbox + label text */
      .cb-wrap {
        display: flex;
        flex-direction: column;
        align-items: center;
        text-align: center;
        font-size: 0.9em;
        line-height: 1;
        flex: 0 0 auto;
        margin: 0 -50px;
      }

      /* Tighten spacing around the checkbox input itself */
      .cb-wrap .shiny-input-container {
        margin: -10px 0 0 0;
        padding: 0;
      }

/* Compact multiselect dropdowns in controls-row (selectizeInput, multiple=TRUE) - allow wrapping */
.controls-row .selectize-control.multi .selectize-input {
  font-size: 0.8em;
  padding: 1px 1px;
  min-height: 25px !important;

  height: auto !important;         /* allow multi-line growth */
  max-height: 60px;                /* prevent huge controls */
  overflow-y: auto;                /* scroll when many items */

  white-space: normal !important;  /* allow wrapping */
}

.controls-row .selectize-control.multi .selectize-input > .item {
  line-height: 20px;
  padding: 0 4px;
  margin: 0 2px 2px 0;             /* small vertical spacing between lines */
}

.controls-row .selectize-control.multi .selectize-input > input {
  line-height: 20px;
  padding: 0 !important;
  margin: 0 !important;
}

    "))
  ),

  div(
    class = "content",

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

    div(
      class = "links",
      uiOutput("last_sync")
    ),

    div(
      class = "links",
      uiOutput("status_message")
    ),

    # Line before first section
    div(class = "section-separator section-separator-first"),

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

    conditionalPanel(
      condition = "output.plot_available == true",
      div(
        class = "plot-container",
        plotOutput("activity_plot", inline = TRUE)
      ),
      div(
        class = "download-links-activity",
        downloadButton("download_plot_png", "Download PNG", class = "small-button"),
        downloadButton("download_plot_svg", "Download SVG", class = "small-button")
      )
    ),

    div(class = "section-separator"),

    # ---- Tidligere aktiviteter ----
    div(class = "subheader", "Antal tidligere aktiviteter"),

    div(
      class = "links",
      actionButton(
        "run_svartype_plot",
        "Plot aktiviteter",
        class = "link-button"
      )
    ),

    tags$details(
      class = "tidligere-settings",
      tags$summary("Indstillinger for 'Antal tidligere aktiviteter' (klik for at folde ud)"),

      # Line 1: Fra, Til, Tidsopløsning, Vis antal
      div(
        class = "controls-row",
        dateInput(
          "from_date",
          label = "Fra",
          value = floor_date(Sys.Date(), "month") %m-% years(2),
          format = "dd-mm-yyyy",
          width = "120px"
        ),
        dateInput(
          "to_date",
          label = "Til",
          value = floor_date(Sys.Date(), "month") - days(1),
          format = "dd-mm-yyyy",
          width = "120px"
        ),
        selectInput(
          "timeGranularity",
          label = "Tidsopløsning",
          choices = c("Uge", "Måned", "Kvartal", "År"),
          selected = "Måned",
          width = "120px"
        ),
        selectInput(
          "countMode",
          label = "Vis antal",
          choices = c("Ingen", "Samlet", "Opdelt"),
          selected = "Samlet",
          width = "140px"
        )
      ),

      # Line 2: Svartype, Speciale, Region
      div(
        class = "controls-row",
        selectInput(
          "svartypeFilterToggle",
          label = "Svartype",
          choices = c(
            "Alle",
            "Klinisk rådgivning",
            "Almindeligt svar",
            "Kortsvar",
            "Generel forespørgsel",
            "Medicingennemgang",
            "Ibrugtagningssag"
          ),
          selected = "Alle",
          width = "190px"
        ),
        selectInput(
          "specialeToggle",
          label = "Speciale og sektor",
          choices = "Alle",
          selected = "Alle",
          width = "220px"
        ),
        selectInput(
          "regionToggle",
          label = "Region (rekvirent)",
          choices = c("Begge", "Nordjylland", "Midtjylland"),
          selected = "Begge",
          width = "180px"
        )
      ),

      # Line 3: checkboxes
      div(
        class = "controls-row-tight",
        div(
          class = "cb-wrap",
          div(HTML("Gruppér kliniske henvendelser<br>(alm., kort, generel)")),
          checkboxInput("groupSvarToggle", label = NULL, value = FALSE)
        ),
        div(
          class = "cb-wrap",
          div("Trendlinje"),
          checkboxInput("showTrendlineToggle", label = NULL, value = FALSE)
        ),
        div(
          class = "cb-wrap",
          div("Vis samlet antal"),
          checkboxInput("showCountInLegend", label = NULL, value = FALSE)
        )
      )
    ),

    conditionalPanel(
      condition = "output.svartype_plot_available == true",
      div(class = "section-separator-thin"),
      div(
        class = "plot-container",
        plotOutput("svartype_plot", inline = TRUE),
        div(
          class = "download-links-svartype",
          downloadButton("download_svartype_png", "Download PNG", class = "small-button"),
          downloadButton("download_svartype_svg", "Download SVG", class = "small-button")
        )
      ),
      div(style = "height: 0px;")
    ),

    # --- Thin separator line between "Tidligere aktiviteter" and "Svartider"
    div(class = "section-separator-thin"),

    # ---- Svartider ----
    div(class = "subheader", "Svartider"),

    div(
      class = "links",
      actionButton(
        "run_svartider_plot",
        "Plot svartider",
        class = "link-button"
      )
    ),

    tags$details(
      class = "svartider-settings",
      tags$summary("Indstillinger for 'Svartider' (klik for at folde ud)"),

      # Line 1: Fra, Til, Tidsopløsning, Inkluderede dage
      div(
        class = "controls-row",
        dateInput(
          "from_date_timePlot",
          label = "Fra",
          value = floor_date(Sys.Date(), "month") %m-% years(2),
          format = "dd-mm-yyyy",
          width = "120px"
        ),
        dateInput(
          "to_date_timePlot",
          label = "Til",
          value = floor_date(Sys.Date(), "month") - days(1),
          format = "dd-mm-yyyy",
          width = "120px"
        ),
        selectInput(
          "timeGranularity_timePlot",
          label = "Tidsopløsning",
          choices = c("Uge", "Måned", "Kvartal", "År"),
          selected = "Måned",
          width = "120px"
        ),
        selectInput(
          "svartidTypeToggle_timePlot",
          label = "Inkluderede dage",
          choices = c(
            "Alle dage",
            "Hverdage m. helligdage",
            "Hverdage u. helligdage og weekend"
          ),
          selected = "Hverdage u. helligdage og weekend",
          width = "250px"
        )
      ),

      # Line 2: Svartider-specific: Svartype, Region
      div(
        class = "controls-row",
        selectInput(
          "svartypeFilterToggle_timePlot",
          label = "Svartype",
          choices = c(
            "Alle",
            "Klinisk rådgivning",
            "Almindeligt svar",
            "Kortsvar",
            "Generel forespørgsel",
            "Medicingennemgang"
          ),
          selected = "Almindeligt svar",
          width = "190px"
        ),
        selectInput(
          "regionToggle_timePlot",
          label = "Region (rekvirent)",
          choices = c("Begge", "Nordjylland", "Midtjylland"),
          selected = "Begge",
          width = "180px"
        )
      ),

      # Line 3: Percentiles
      div(
        class = "controls-row",
        selectInput(
          "graf1_percentile_timePlot",
          label = "Graf 1 (percentil)",
          choices = c("Ingen", "20", "40", "Median (50)", "75", "80", "90", "100"),
          selected = "90",
          width = "160px"
        ),
        selectInput(
          "graf2_percentile_timePlot",
          label = "Graf 2 (Percentil)",
          choices = c("Ingen", "20", "40", "Median (50)", "75", "80", "90", "100"),
          selected = "Median (50)",
          width = "160px"
        ),
        selectInput(
          "graf3_percentile_timePlot",
          label = "Graf 3 (Percentil)",
          choices = c("Ingen", "20", "40", "Median (50)", "75", "80", "90", "100"),
          selected = "Ingen",
          width = "160px"
        )
      ),

      # Line 4: checkbox with same look as Trendlinje checkbox
      div(
        class = "controls-row-tight",
        div(
          class = "cb-wrap",
          div("Vis gennemsnit"),
          checkboxInput(
            "showMeanToggle_timePlot",
            label = NULL,
            value = FALSE
          )
        )
      )
    ),

    conditionalPanel(
      condition = "output.svartider_plot_available == true",
      div(class = "section-separator-thin"),
      div(
        class = "plot-container",
        plotOutput("svartider_plot", inline = TRUE),
        div(
          class = "download-links-svartider",
          downloadButton("download_svartider_png", "Download PNG", class = "small-button"),
          downloadButton("download_svartider_svg", "Download SVG", class = "small-button")
        )
      ),
      div(style = "height: 0px;")
    ),

    # --- Thin separator line between "Svartider" and "Specialefordeling"
    div(class = "section-separator-thin"),

    # ---- Specialefordeling ----
    div(class = "subheader", "Specialefordeling"),

    div(
      class = "links",
      actionButton(
        "run_specialefordeling_plot",
        "Plot specialefordeling",
        class = "link-button"
      )
    ),

    tags$details(
      class = "specialefordeling-settings",
      tags$summary("Indstillinger for specialefordeling (klik for at folde ud)"),

       # Line 1: Fra, Til, Specialer
      div(
        class = "controls-row",
        dateInput(
          "from_date_specialePlot",
          label = "Fra",
          value = floor_date(Sys.Date(), "month") %m-% years(2),
          format = "dd-mm-yyyy",
          width = "120px"
        ),
        dateInput(
          "to_date_specialePlot",
          label = "Til",
          value = floor_date(Sys.Date(), "month") - days(1),
          format = "dd-mm-yyyy",
          width = "120px"
        ),
        selectizeInput(
          "pie_specialer",
          label = "Specialer",
          choices = c(
            "Andre specialer",
            "Intern medicin",
            "Neurologi",
            "Pædiatri",
            "Onkologi",
            "Gynækologi og obstetrik",
            "Børne- og ungdomspsykiatri",
            "Psykiatri",
            "Almen praksis"
          ),
          selected = c(
            "Andre specialer",
            "Intern medicin",
            "Neurologi",
            "Pædiatri",
            "Onkologi",
            "Gynækologi og obstetrik",
            "Børne- og ungdomspsykiatri",
            "Psykiatri",
            "Almen praksis"
          ),
          multiple = TRUE,
          width = "420px",
          options = list(plugins = list("remove_button"))
        ),
        div(
          class = "top-buttons-stack",
          actionButton("top8_specialer",  "Top 8",  class = "small-button"),
          actionButton("top15_specialer", "Top 15", class = "small-button")
        )
      ),

      # Line 2: Svartype, Region, Spørgsmålskategori
      div(
        class = "controls-row",
        selectInput(
          "svartypeFilterToggle_specialePlot",
          label = "Svartype",
          choices = c(
            "Alle",
            "Klinisk rådgivning",
            "Almindeligt svar",
            "Kortsvar",
            "Generel forespørgsel",
            "Medicingennemgang",
            "Ibrugtagningssag"
          ),
          selected = "Klinisk rådgivning",
          width = "190px"
        ),
        selectInput(
          "regionToggle_specialePlot",
          label = "Region (rekvirent)",
          choices = c("Begge", "Nordjylland", "Midtjylland"),
          selected = "Begge",
          width = "180px"
        ),
        selectizeInput(
          "spmKategoriFilterToggle_specialePlot",
          label = "Spørgsmålskategori",
          choices = "Alle",
          selected = "Alle",
          multiple = TRUE,
          width = "320px",
          options = list(plugins = list("remove_button"))
        )
      ),

      # Line 3: Vis antal + Signaturforklaring
      # Line 3: Vis antal + Signaturforklaring + Farveskema
      div(
        class = "controls-row",
        selectInput(
          "showNumbersToggle_specialePlot",
          label = "Vis antal",
          choices = c("%", "Absolut", "Ingen"),
          selected = "%",
          width = "120px"
        ),
        selectInput(
          "legendPositionToggle_specialePlot",
          label = "Signaturforklaring",
          choices = c("I diagram", "Udenfor diagram"),
          selected = "I diagram",
          width = "170px"
        ),
        selectInput(
          "colorToggle_specialePlot",
          label = "Farveskema",
          choices = c(
            "Set2", "Set3", "Paired", "Set1", "Dark2", "Accent",
            "viridis", "plasma", "magma", "inferno", "cividis"
          ),
          selected = "Set2",
          width = "170px"
        )
      )
    ),

conditionalPanel(
  condition = "output.specialefordeling_plot_available == true",
  div(class = "section-separator-thin"),
  div(
    class = "plot-container",
    plotOutput("specialefordeling_plot", inline = TRUE),
    div(
      class = "download-links-specialefordeling",
      downloadButton("download_specialefordeling_png", "Download PNG", class = "small-button"),
      downloadButton("download_specialefordeling_svg", "Download SVG", class = "small-button")
    )
  ),
  div(style = "height: 0px;")
),

    # Line above KFE/KFA, regardless of plot
    div(class = "section-separator-above-kfe"),

    # ---- KFE/KFA tilhørsforhold (collapsible) ----
    tags$details(
      class = "kfe-kfa-details",
      tags$summary(
        class = "kfe-kfa-summary",
        "Opdater tilhørsforhold til KFE og KFA (klik for at folde ud)"
      ),
      div(
        class = "kfe-kfa-content",
        div(class = "kfe-kfa-subtitle", "Nyeste Excel anvendes i scripts"),
        uiOutput("download_dropdown_ui"),
        div(
          class = "kfe-kfa-buttons",
          tags$div(
            style = "display: none;",
            fileInput(
              "excel_file", NULL,
              accept = c(".xlsx", ".xls"),
              buttonLabel = "Upload"
            )
          ),
          actionButton("upload_excel", "Upload", class = "small-button"),
          downloadButton("download_original", "Download original", class = "small-button")
        )
      )
    ),

    div(class = "section-separator"),

    tags$details(
      tags$summary("Tekniske detaljer (klik for at folde ud)"),
      div(
        id = "console_output",
        class = "console-output",
        ""
      )
    )
  ),

  div(
    class = "footer",
    "Support: Ole Andersen, oleemil@biomed.au.dk"
  )
)

server <- function(input, output, session) {

  script_status <- reactiveVal(NULL)

  activity_plot        <- reactiveVal(NULL)
  svartype_plot_obj    <- reactiveVal(NULL)
  svartider_plot_obj   <- reactiveVal(NULL)
  specialefordeling_plot_obj <- reactiveVal(NULL)

  svartype_plot_active        <- reactiveVal(FALSE)
  svartider_plot_active       <- reactiveVal(FALSE)
  specialefordeling_plot_active <- reactiveVal(FALSE)

  output$plot_available <- reactive({
    !is.null(activity_plot())
  })
  outputOptions(output, "plot_available", suspendWhenHidden = FALSE)

  output$svartype_plot_available <- reactive({
    !is.null(svartype_plot_obj())
  })
  outputOptions(output, "svartype_plot_available", suspendWhenHidden = FALSE)

  output$svartider_plot_available <- reactive({
    !is.null(svartider_plot_obj())
  })
  outputOptions(output, "svartider_plot_available", suspendWhenHidden = FALSE)

  output$specialefordeling_plot_available <- reactive({
    !is.null(specialefordeling_plot_obj())
  })
  outputOptions(output, "specialefordeling_plot_available", suspendWhenHidden = FALSE)

  # -------------------------- LAST SYNC -------------------------- #

  data_dir <- here("statistik", "Data", "Azure data")

  get_newest_rds_file <- function(directory_path) {
    rds_files <- list.files(
      path   = directory_path,
      pattern = "^azure_.*\\.rds$",
      full.names = TRUE
    )
    if (length(rds_files) == 0) return(NULL)
    file_info <- file.info(rds_files)
    rds_files[which.max(file_info$mtime)]
  }

  last_sync_time <- reactiveVal(NA)
  sync_ok        <- reactiveVal(FALSE)

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
      if (initial) sync_ok(FALSE)
    }
    invisible(NULL)
  }

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

  # -------------------------- KFE/KFA TILHØRSFORHOLD -------------------------- #

  main_folder     <- here("statistik", "data", "tilknytning, KFE_KFA")
  original_folder <- here("statistik", "data", "tilknytning, KFE_KFA", "Original")

  observe({
    if (!dir.exists(main_folder)) dir.create(main_folder, recursive = TRUE)
    if (!dir.exists(original_folder)) dir.create(original_folder, recursive = TRUE)
  })

  get_excel_files <- reactive({
    files <- list.files(main_folder, pattern = "\\.xlsx$", full.names = FALSE)
    if (length(files) == 0) return(NULL)
    file_info     <- file.info(file.path(main_folder, files))
    files_ordered <- files[order(file_info$mtime, decreasing = TRUE)]
    files_ordered
  })

  output$download_dropdown_ui <- renderUI({
    files <- get_excel_files()
    if (is.null(files)) return(NULL)

    tagList(
      div(
        class = "dropdown-container",
        selectInput(
          "excel_files", NULL,
          choices = files,
          width = "170px",
          selectize = TRUE
        ),
        downloadButton("download_selected", "Download valgt", class = "small-download-button")
      )
    )
  })

  observeEvent(input$upload_excel, {
    shinyjs::click("excel_file")
  })

  observeEvent(input$excel_file, {
    req(input$excel_file)

    if (!grepl("\\.xlsx?$", input$excel_file$name, ignore.case = TRUE)) {
      script_status(list(
        type = "error",
        message = "Kun Excel-filer (.xlsx, .xls) er tilladte."
      ))
      return()
    }

    tryCatch({
      timestamp    <- format(Sys.time(), "%Y-%m-%d %H.%M")
      new_filename <- paste0("Tilhørsforhold (", timestamp, ").xlsx")
      new_filepath <- file.path(main_folder, new_filename)

      file.copy(input$excel_file$datapath, new_filepath)

      files <- list.files(main_folder, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(files) > 10) {
        file_info     <- file.info(files)
        files_ordered <- files[order(file_info$mtime)]
        files_to_remove <- files_ordered[1:(length(files) - 10)]
        file.remove(files_to_remove)
      }

      script_status(list(
        type    = "message",
        message = paste("Filen er uploadet succesfuldt:", new_filename)
      ))

    }, error = function(e) {
      script_status(list(
        type    = "error",
        message = paste("Fejl under upload:", e$message)
      ))
    })
  })

  output$download_selected <- downloadHandler(
    filename = function() {
      req(input$excel_files)
      input$excel_files
    },
    content = function(file) {
      req(input$excel_files)
      file_path <- file.path(main_folder, input$excel_files)
      file.copy(file_path, file)
    }
  )

  output$download_original <- downloadHandler(
    filename = function() {
      original_files <- list.files(original_folder, pattern = "\\.xlsx$")
      if (length(original_files) > 0) return(original_files[1])
      "Original.xlsx"
    },
    content = function(file) {
      original_files <- list.files(original_folder, pattern = "\\.xlsx$", full.names = TRUE)
      if (length(original_files) > 0) {
        file.copy(original_files[1], file)
      }
    }
  )

  # -------------------------- DOWNLOAD DATA SCRIPT ------------------------- #

  observeEvent(input$download_data, {
    shinyjs::disable("download_data")

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
        html = paste0("FEJL: Script ikke fundet:\n", script_path, "\n"),
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

        if (grepl("INVALID_SHAREPOINT_SECRET", err_msg, fixed = TRUE)) {
          msg_ui <- HTML(paste0(
            "Client secret til Azure/SharePoint skal opdateres på serveren:<br>",
            "1) Åbn Terminal og login på serveren fra AU-net (indsæt eget AUID begge steder):<br>",
            "<code>ssh -J AUID@ssh.au.dk AUID@kfaapps.uni.au.dk</code><br><br>",
            "2) Gå til mappen med secret-filen:<br>",
            "<code>cd /srv/shiny-server/kfaapps/statistik</code><br><br>",
            "3) Åbn secret-filen med sudo, fjern gammel secret og indsæt ny secret:<br>",
            "<code>sudo nano secret_for_sharepoint.txt</code><br>",
            "   - Indsæt ny secret på én linje<br>",
            "   - Gem og luk nano med: <code>Ctrl+O</code>, <code>Enter</code>, <code>Ctrl+X</code><br><br>",
            "4) Kør synkronisering igen i denne app (knappen 'Synkroniser data med Sharepoint')."
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

  output$activity_plot <- renderPlot(
    {
      req(activity_plot())
      grid::grid.newpage()
      grid::grid.draw(activity_plot())
    },
    width  = 600,
    height = 500,
    res    = 96
  )

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

  # ------------------- LOAD DATA FOR DROPDOWNS -------------------- #

  observe({
    newest <- get_newest_rds_file(data_dir)
    if (is.null(newest)) return()

    df <- tryCatch(
      readRDS(newest),
      error = function(e) NULL
    )
    if (is.null(df)) return()
    if (!"specialeCorrected" %in% names(df)) return()

    # Speciale dropdown (existing)
    specs <- sort(unique(na.omit(df$specialeCorrected)))

    if (!"Hospital" %in% specs) {
      specs <- c(specs, "Hospital")
    }

    priority <- c("Hospital", "Almen praksis")
    priority_in_specs <- priority[priority %in% specs]
    rest <- setdiff(specs, priority_in_specs)
    specs_ordered <- c(priority_in_specs, sort(rest))

    updateSelectInput(
      session,
      "specialeToggle",
      choices = c("Alle", specs_ordered),
      selected = "Alle"
    )

    # Specialefordeling: specialer multiselect
    default_specialer <- c(
      "Andre specialer",
      "Intern medicin",
      "Neurologi",
      "Pædiatri",
      "Onkologi",
      "Gynækologi og obstetrik",
      "Børne- og ungdomspsykiatri",
      "Psykiatri",
      "Almen praksis"
    )
    selected_specialer <- default_specialer[default_specialer %in% specs_ordered]
    if (length(selected_specialer) == 0) {
      selected_specialer <- head(specs_ordered, 5)
    }

    updateSelectizeInput(
      session,
      "pie_specialer",
      choices = specs_ordered,
      selected = selected_specialer,
      server = TRUE
    )

    # Specialefordeling: spørgsmålskategori multiselect
    if ("Spørgsmålskategori (*)" %in% names(df)) {
      cats <- sort(unique(na.omit(df$`Spørgsmålskategori (*)`)))
      updateSelectizeInput(
        session,
        "spmKategoriFilterToggle_specialePlot",
        choices = c("Alle", cats),
        selected = "Alle",
        server = TRUE
      )
    }
  })

  # -------------------------- SVARTYPE PLOT (Tidligere aktiviteter) -------------------------- #
  # (UNCHANGED - omitted here for brevity in comment only; code below is identical)
  run_svartype_plot <- function() {
    shinyjs::html(
      id   = "console_output",
      html = "Opdaterer 'Tidligere aktiviteter'...\n\n",
      add  = TRUE
    )

    script_path_plot <- file.path(
      here("statistik", "scripts"),
      "plot svartype.R"
    )

    if (!file.exists(script_path_plot)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0(
          "FEJL: 'plot svartype' script ikke fundet:\n",
          script_path_plot, "\n"
        ),
        add  = TRUE
      )

      script_status(list(
        type    = "error",
        message = "Scriptet 'plot svartype.R' blev ikke fundet. Kontakt support."
      ))
      return(invisible(NULL))
    }

    tryCatch(
      {
        plot_env <- new.env(parent = globalenv())

        plot_env$showStackCount  <- (input$countMode == "Opdelt")
        plot_env$showTotalCount  <- (input$countMode == "Samlet")
        plot_env$timeGranularity <- input$timeGranularity
        plot_env$showTrendlineToggle   <- isTRUE(input$showTrendlineToggle)
        plot_env$groupSvarToggle       <- isTRUE(input$groupSvarToggle)
        plot_env$sektorToggle          <- "Alle"
        plot_env$specialeToggle        <- input$specialeToggle
        plot_env$regionToggle          <- input$regionToggle
        plot_env$svartypeFilterToggle  <- input$svartypeFilterToggle
        plot_env$showCountInLegend     <- isTRUE(input$showCountInLegend)

        from <- as.Date(input$from_date)
        to   <- as.Date(input$to_date)

        plot_env$start_year  <- as.integer(format(from, "%Y"))
        plot_env$start_month <- as.integer(format(from, "%m"))
        plot_env$start_day   <- as.integer(format(from, "%d"))
        plot_env$end_year    <- as.integer(format(to, "%Y"))
        plot_env$end_month   <- as.integer(format(to, "%m"))
        plot_env$end_day     <- as.integer(format(to, "%d"))

        withCallingHandlers(
          {
            message("Kører 'plot svartype.R': ", script_path_plot)
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

        if (!exists("p", envir = plot_env, inherits = FALSE)) {
          stop("Plot-objekt 'p' ikke fundet efter kørsel af 'plot svartype.R'.")
        }

        svartype_plot_obj(get("p", envir = plot_env))
        script_status(NULL)

        shinyjs::html(
          id   = "console_output",
          html = "\n'Tidligere aktiviteter' opdateret.\n",
          add  = TRUE
        )

      },
      error = function(e) {
        err_msg <- e$message

        script_status(list(
          type    = "error",
          message = "Der opstod en fejl under opdatering af 'Tidligere aktiviteter'. Se tekniske detaljer eller kontakt support."
        ))

        shinyjs::html(
          id   = "console_output",
          html = paste0(
            "\n*** TEKNISK FEJL (svartype-plot) ***\n",
            err_msg,
            "\n****************************************\n"
          ),
          add  = TRUE
        )
      }
    )

    invisible(NULL)
  }

  observeEvent(input$run_svartype_plot, {
    shinyjs::disable("run_svartype_plot")
    svartype_plot_active(TRUE)
    run_svartype_plot()
    shinyjs::enable("run_svartype_plot")
  })

  observeEvent(
    list(
      input$from_date,
      input$to_date,
      input$timeGranularity,
      input$countMode,
      input$svartypeFilterToggle,
      input$specialeToggle,
      input$regionToggle,
      input$groupSvarToggle,
      input$showTrendlineToggle,
      input$showCountInLegend
    ),
    {
      if (isTRUE(svartype_plot_active())) {
        run_svartype_plot()
      }
    },
    ignoreInit = TRUE
  )

  output$svartype_plot <- renderPlot(
    {
      req(svartype_plot_obj())
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
    },
    width  = 800,
    height = 650,
    res    = 96
  )

  output$download_svartype_png <- downloadHandler(
    filename = function() {
      paste0("svartype_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(svartype_plot_obj())
      grDevices::png(file, width = 10, height = 6.5, units = "in", res = 300)
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
      grDevices::dev.off()
    }
  )

  output$download_svartype_svg <- downloadHandler(
    filename = function() {
      paste0("svartype_plot_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(svartype_plot_obj())
      grDevices::svg(file, width = 10, height = 6.5)
      grid::grid.newpage()
      grid::grid.draw(svartype_plot_obj())
      grDevices::dev.off()
    }
  )

  # -------------------------- SVARTIDER PLOT (time plot) -------------------------- #

  map_percentile_input <- function(x) {
    if (is.null(x) || identical(x, "Ingen")) return("Ingen")
    if (identical(x, "Median (50)")) return(50)
    as.numeric(x)
  }

  run_svartider_plot <- function() {
    script_path_plot <- file.path(
      here("statistik", "scripts"),
      "plot svartid.R"
    )

    if (!file.exists(script_path_plot)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0("FEJL: 'plot svartid.R' script ikke fundet:\n", script_path_plot, "\n"),
        add  = TRUE
      )
      script_status(list(
        type    = "error",
        message = "Scriptet til 'Svartider' blev ikke fundet. Kontakt support."
      ))
      return(invisible(NULL))
    }

    plot_env <- new.env(parent = globalenv())

    plot_env$timeGranularity.timePlot      <- input$timeGranularity_timePlot
    plot_env$regionToggle.timePlot         <- input$regionToggle_timePlot
    plot_env$svartypeFilterToggle.timePlot <- input$svartypeFilterToggle_timePlot

    from_tp <- as.Date(input$from_date_timePlot)
    to_tp   <- as.Date(input$to_date_timePlot)

    plot_env$start_year.timePlot  <- as.integer(format(from_tp, "%Y"))
    plot_env$start_month.timePlot <- as.integer(format(from_tp, "%m"))
    plot_env$start_day.timePlot   <- as.integer(format(from_tp, "%d"))
    plot_env$end_year.timePlot    <- as.integer(format(to_tp, "%Y"))
    plot_env$end_month.timePlot   <- as.integer(format(to_tp, "%m"))
    plot_env$end_day.timePlot     <- as.integer(format(to_tp, "%d"))

    plot_env$svartidTypeToggle <- input$svartidTypeToggle_timePlot
    plot_env$graf1_percentile  <- map_percentile_input(input$graf1_percentile_timePlot)
    plot_env$graf2_percentile  <- map_percentile_input(input$graf2_percentile_timePlot)
    plot_env$graf3_percentile  <- map_percentile_input(input$graf3_percentile_timePlot)

    plot_env$showMeanToggle <- isTRUE(input$showMeanToggle_timePlot)

    tryCatch(
      {
        withCallingHandlers(
          {
            message("Kører 'Svartider' script: ", script_path_plot)
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

        if (!exists("p", envir = plot_env, inherits = FALSE)) {
          stop("Plot-objekt 'p' ikke fundet efter kørsel af 'plot svartid.R'.")
        }

        svartider_plot_obj(get("p", envir = plot_env))
        script_status(NULL)

        shinyjs::html(
          id   = "console_output",
          html = "\n'Svartider' plot-script færdigt.\n",
          add  = TRUE
        )

      },
      error = function(e) {
        err_msg <- e$message
        script_status(list(
          type    = "error",
          message = "Der opstod en fejl under opdatering af 'Svartider'. Se tekniske detaljer eller kontakt support."
        ))
        shinyjs::html(
          id   = "console_output",
          html = paste0(
            "\n*** TEKNISK FEJL (svartider-plot) ***\n",
            err_msg,
            "\n****************************************\n"
          ),
          add  = TRUE
        )
      }
    )

    invisible(NULL)
  }

  observeEvent(input$run_svartider_plot, {
    shinyjs::disable("run_svartider_plot")
    shinyjs::html(
      id   = "console_output",
      html = "Starter script til 'Svartider'...\n\n",
      add  = TRUE
    )

    svartider_plot_active(TRUE)
    run_svartider_plot()

    shinyjs::enable("run_svartider_plot")
  })

  observeEvent(
    list(
      input$from_date_timePlot,
      input$to_date_timePlot,
      input$timeGranularity_timePlot,
      input$svartidTypeToggle_timePlot,
      input$svartypeFilterToggle_timePlot,
      input$regionToggle_timePlot,
      input$graf1_percentile_timePlot,
      input$graf2_percentile_timePlot,
      input$graf3_percentile_timePlot,
      input$showMeanToggle_timePlot
    ),
    {
      if (isTRUE(svartider_plot_active())) {
        run_svartider_plot()
      }
    },
    ignoreInit = TRUE
  )

  output$svartider_plot <- renderPlot(
    {
      req(svartider_plot_obj())
      print(svartider_plot_obj())
    },
    width  = 800,
    height = 650,
    res    = 96
  )

  output$download_svartider_png <- downloadHandler(
    filename = function() {
      paste0("svartider_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(svartider_plot_obj())
      grDevices::png(file, width = 10, height = 6.5, units = "in", res = 300)
      print(svartider_plot_obj())
      grDevices::dev.off()
    }
  )

  output$download_svartider_svg <- downloadHandler(
    filename = function() {
      paste0("svartider_plot_", Sys.Date(), ".svg")
    },
    content = function(file) {
      req(svartider_plot_obj())
      grDevices::svg(file, width = 10, height = 6.5)
      print(svartider_plot_obj())
      grDevices::dev.off()
    }
  )

  # -------------------------- SPECIALEFORDELING (pie chart) -------------------------- #

  observeEvent(input$top8_specialer, {
    newest <- get_newest_rds_file(data_dir)
    if (is.null(newest)) return()

    df <- tryCatch(readRDS(newest), error = function(e) NULL)
    if (is.null(df)) return()
    if (!"specialeCorrected" %in% names(df)) return()

    # Resolve date column (same logic as in plot script)
    resolve_date_col_local <- function(d) {
      if ("AdjustedDate" %in% names(d)) {
        dd <- suppressWarnings(as.Date(d$AdjustedDate))
        if (sum(!is.na(dd)) > 0) return(dd)
      }
      if (all(c("Year", "Month") %in% names(d))) {
        y <- suppressWarnings(as.integer(d$Year))
        m <- suppressWarnings(as.integer(d$Month))
        return(suppressWarnings(as.Date(sprintf("%04d-%02d-01", y, m))))
      }
      rep(as.Date(NA), nrow(d))
    }

    from_p <- as.Date(input$from_date_specialePlot)
    to_p   <- as.Date(input$to_date_specialePlot)

    dvec <- resolve_date_col_local(df)

    filtered <- df %>%
      mutate(.date_for_filter = dvec) %>%
      filter(!is.na(.date_for_filter),
             .date_for_filter >= from_p,
             .date_for_filter <= to_p)

    # Region
    if (!identical(input$regionToggle_specialePlot, "Begge") && "Region (*)" %in% names(filtered)) {
      filtered <- filtered %>% filter(`Region (*)` == input$regionToggle_specialePlot)
    }

    # Svartype (incl. Klinisk rådgivning grouping)
    if (!identical(input$svartypeFilterToggle_specialePlot, "Alle") && "Svartype (*)" %in% names(filtered)) {
      if (identical(input$svartypeFilterToggle_specialePlot, "Klinisk rådgivning")) {
        filtered <- filtered %>%
          filter(`Svartype (*)` %in% c("Almindeligt svar", "Kortsvar", "Generel forespørgsel"))
      } else {
        filtered <- filtered %>% filter(`Svartype (*)` == input$svartypeFilterToggle_specialePlot)
      }
    }

    # Spørgsmålskategori (multiple) - treat "Alle" as no filter (even if combined)
    if ("Spørgsmålskategori (*)" %in% names(filtered)) {
      spm_sel <- input$spmKategoriFilterToggle_specialePlot
      if (!is.null(spm_sel) && length(spm_sel) > 0 && !("Alle" %in% spm_sel)) {
        filtered <- filtered %>% filter(`Spørgsmålskategori (*)` %in% spm_sel)
      }
    }

    if (nrow(filtered) == 0) {
      shinyjs::html(
        id = "console_output",
        html = "Top 8: Ingen rækker efter filtrering - ingen ændring.\n",
        add = TRUE
      )
      return()
    }

    # Top 8 handling:
    # Keep "Andre specialer" as a required bucket in the plot script,
    # so we select top 7 actual specialties + "Andre specialer" = 8.
    top_specs <- filtered %>%
      filter(!is.na(specialeCorrected)) %>%
      count(specialeCorrected, sort = TRUE) %>%
      slice_head(n = 7) %>%
      pull(specialeCorrected)

    new_selected <- unique(c("Andre specialer", top_specs))

    # Ensure choices include "Andre specialer"
    all_specs <- sort(unique(na.omit(df$specialeCorrected)))
    if (!("Andre specialer" %in% all_specs)) all_specs <- c("Andre specialer", all_specs)

    updateSelectizeInput(
      session,
      "pie_specialer",
      choices  = all_specs,
      selected = new_selected,
      server   = TRUE
    )
  })

  observeEvent(input$top15_specialer, {
  newest <- get_newest_rds_file(data_dir)
  if (is.null(newest)) return()

  df <- tryCatch(readRDS(newest), error = function(e) NULL)
  if (is.null(df)) return()
  if (!"specialeCorrected" %in% names(df)) return()

  resolve_date_col_local <- function(d) {
    if ("AdjustedDate" %in% names(d)) {
      dd <- suppressWarnings(as.Date(d$AdjustedDate))
      if (sum(!is.na(dd)) > 0) return(dd)
    }
    if (all(c("Year", "Month") %in% names(d))) {
      y <- suppressWarnings(as.integer(d$Year))
      m <- suppressWarnings(as.integer(d$Month))
      return(suppressWarnings(as.Date(sprintf("%04d-%02d-01", y, m))))
    }
    rep(as.Date(NA), nrow(d))
  }

  from_p <- as.Date(input$from_date_specialePlot)
  to_p   <- as.Date(input$to_date_specialePlot)

  dvec <- resolve_date_col_local(df)

  filtered <- df %>%
    mutate(.date_for_filter = dvec) %>%
    filter(!is.na(.date_for_filter),
           .date_for_filter >= from_p,
           .date_for_filter <= to_p)

  if (!identical(input$regionToggle_specialePlot, "Begge") && "Region (*)" %in% names(filtered)) {
    filtered <- filtered %>% filter(`Region (*)` == input$regionToggle_specialePlot)
  }

  if (!identical(input$svartypeFilterToggle_specialePlot, "Alle") && "Svartype (*)" %in% names(filtered)) {
    if (identical(input$svartypeFilterToggle_specialePlot, "Klinisk rådgivning")) {
      filtered <- filtered %>%
        filter(`Svartype (*)` %in% c("Almindeligt svar", "Kortsvar", "Generel forespørgsel"))
    } else {
      filtered <- filtered %>% filter(`Svartype (*)` == input$svartypeFilterToggle_specialePlot)
    }
  }

  if ("Spørgsmålskategori (*)" %in% names(filtered)) {
    spm_sel <- input$spmKategoriFilterToggle_specialePlot
    if (!is.null(spm_sel) && length(spm_sel) > 0 && !("Alle" %in% spm_sel)) {
      filtered <- filtered %>% filter(`Spørgsmålskategori (*)` %in% spm_sel)
    }
  }

  if (nrow(filtered) == 0) {
    shinyjs::html(
      id = "console_output",
      html = "Top 15: Ingen rækker efter filtrering - ingen ændring.\n",
      add = TRUE
    )
    return()
  }

  # Top 15: pick top 14 specialties + "Andre specialer" = 15
  top_specs <- filtered %>%
    filter(!is.na(specialeCorrected)) %>%
    count(specialeCorrected, sort = TRUE) %>%
    slice_head(n = 14) %>%
    pull(specialeCorrected)

  new_selected <- unique(c("Andre specialer", top_specs))

  all_specs <- sort(unique(na.omit(df$specialeCorrected)))
  if (!("Andre specialer" %in% all_specs)) all_specs <- c("Andre specialer", all_specs)

  updateSelectizeInput(
    session,
    "pie_specialer",
    choices  = all_specs,
    selected = new_selected,
    server   = TRUE
  )
})

  run_specialefordeling_plot <- function() {
    shinyjs::html(
      id   = "console_output",
      html = "Opdaterer 'Specialefordeling'...\n\n",
      add  = TRUE
    )

    script_path_plot <- file.path(
      here("statistik", "scripts"),
      "plot specialefordeling.R"
    )

    if (!file.exists(script_path_plot)) {
      shinyjs::html(
        id   = "console_output",
        html = paste0("FEJL: 'plot specialefordeling.R' script ikke fundet:\n", script_path_plot, "\n"),
        add  = TRUE
      )
      script_status(list(
        type    = "error",
        message = "Scriptet til 'Specialefordeling' blev ikke fundet. Kontakt support."
      ))
      return(invisible(NULL))
    }

    plot_env <- new.env(parent = globalenv())

    from_p <- as.Date(input$from_date_specialePlot)
    to_p   <- as.Date(input$to_date_specialePlot)

    plot_env$start_year.specialePlot  <- as.integer(format(from_p, "%Y"))
    plot_env$start_month.specialePlot <- as.integer(format(from_p, "%m"))
    plot_env$start_day.specialePlot   <- as.integer(format(from_p, "%d"))
    plot_env$end_year.specialePlot    <- as.integer(format(to_p, "%Y"))
    plot_env$end_month.specialePlot   <- as.integer(format(to_p, "%m"))
    plot_env$end_day.specialePlot     <- as.integer(format(to_p, "%d"))

    plot_env$regionToggle.specialePlot         <- input$regionToggle_specialePlot
    plot_env$svartypeFilterToggle.specialePlot <- input$svartypeFilterToggle_specialePlot
    plot_env$spmKategoriFilterToggle.specialePlot <- input$spmKategoriFilterToggle_specialePlot

    plot_env$specialerAtPlotte.specialePlot <- input$pie_specialer

    plot_env$showNumbersToggle.specialePlot  <- input$showNumbersToggle_specialePlot
    plot_env$legendPositionToggle.specialePlot <- input$legendPositionToggle_specialePlot
    plot_env$colorToggle.specialePlot <- input$colorToggle_specialePlot

    tryCatch(
      {
        withCallingHandlers(
          {
            message("Kører 'Specialefordeling' script: ", script_path_plot)
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

        if (!exists("p", envir = plot_env, inherits = FALSE)) {
          stop("Plot-objekt 'p' ikke fundet efter kørsel af 'plot speciale piechart.R'.")
        }

        specialefordeling_plot_obj(get("p", envir = plot_env))
        script_status(NULL)

        shinyjs::html(
          id   = "console_output",
          html = "\n'Specialefordeling' opdateret.\n",
          add  = TRUE
        )

      },
      error = function(e) {
        err_msg <- e$message
        script_status(list(
          type    = "error",
          message = "Der opstod en fejl under opdatering af 'Specialefordeling'. Se tekniske detaljer eller kontakt support."
        ))
        shinyjs::html(
          id   = "console_output",
          html = paste0(
            "\n*** TEKNISK FEJL (specialefordeling) ***\n",
            err_msg,
            "\n****************************************\n"
          ),
          add  = TRUE
        )
      }
    )

    invisible(NULL)
  }

  observeEvent(input$run_specialefordeling_plot, {
    shinyjs::disable("run_specialefordeling_plot")
    specialefordeling_plot_active(TRUE)
    run_specialefordeling_plot()
    shinyjs::enable("run_specialefordeling_plot")
  })

  observeEvent(
    list(
      input$from_date_specialePlot,
      input$to_date_specialePlot,
      input$pie_specialer,
      input$svartypeFilterToggle_specialePlot,
      input$regionToggle_specialePlot,
      input$spmKategoriFilterToggle_specialePlot,
      input$showNumbersToggle_specialePlot,
      input$legendPositionToggle_specialePlot,
      input$colorToggle_specialePlot
    ),
    {
      if (isTRUE(specialefordeling_plot_active())) {
        run_specialefordeling_plot()
      }
    },
    ignoreInit = TRUE
  )

  output$specialefordeling_plot <- renderPlot(
    {
      req(specialefordeling_plot_obj())
      print(specialefordeling_plot_obj())
    },
    width  = 800,
    height = 650,
    res    = 96
  )
  output$download_specialefordeling_png <- downloadHandler(
  filename = function() {
    paste0("specialefordeling_plot_", Sys.Date(), ".png")
  },
  content = function(file) {
    req(specialefordeling_plot_obj())
    grDevices::png(file, width = 10, height = 6.5, units = "in", res = 300)
    print(specialefordeling_plot_obj())
    grDevices::dev.off()
  }
)

output$download_specialefordeling_svg <- downloadHandler(
  filename = function() {
    paste0("specialefordeling_plot_", Sys.Date(), ".svg")
  },
  content = function(file) {
    req(specialefordeling_plot_obj())
    grDevices::svg(file, width = 10, height = 6.5)
    print(specialefordeling_plot_obj())
    grDevices::dev.off()
  }
)


  # -------------------------- STATUS MESSAGE -------------------------- #

  output$status_message <- renderUI({
    status <- script_status()
    if (!is.null(status)) {
      div(
        class = paste(
          "status-message",
          if (status$type == "error") "status-error" else "status-ok"
        ),
        status$message
      )
    }
  })
}

shinyApp(ui, server)
