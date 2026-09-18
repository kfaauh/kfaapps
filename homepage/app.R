library(shiny)
library(bslib)

# -------------------------------------------------------------------------
# Theme
# -------------------------------------------------------------------------

kfa_theme <- bs_theme(
  version = 5,
  bg = "#F6F8FB",
  fg = "#1F2933",
  primary = "#0033A0",
  secondary = "#667085",
  base_font = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif",
  heading_font = "-apple-system, BlinkMacSystemFont, 'Segoe UI', Arial, sans-serif"
)

# -------------------------------------------------------------------------
# Reusable UI components
# -------------------------------------------------------------------------

app_card <- function(title, description, href, badge = NULL) {
  tags$a(
    href = href,
    class = "app-card",

    div(
      class = "app-card-content",

      div(
        class = "app-card-header",

        tags$h3(
          class = "app-card-title",
          title
        ),

        if (!is.null(badge)) {
          tags$span(
            class = "app-badge",
            badge
          )
        }
      ),

      tags$p(
        class = "app-card-description",
        description
      )
    ),

    tags$span(
      class = "app-card-arrow",
      "\u2192"
    )
  )
}


app_section <- function(title, description = NULL, ...) {
  div(
    class = "app-section",

    div(
      class = "section-heading",

      tags$h2(title),

      if (!is.null(description)) {
        tags$p(description)
      }
    ),

    div(
      class = "app-grid",
      ...
    )
  )
}


# -------------------------------------------------------------------------
# UI
# -------------------------------------------------------------------------

ui <- fluidPage(
  theme = kfa_theme,

  tags$head(

    tags$title("KFA apps"),

    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),

    tags$style(
      HTML(
        "
        /* ---------------------------------------------------------------
           Base layout
        ---------------------------------------------------------------- */

        html,
        body {
          min-height: 100%;
        }

        body {
          margin: 0;
          background: #F6F8FB;
          color: #1F2933;
        }

        .container-fluid {
          padding: 0;
        }

        .page-shell {
          min-height: 100vh;
          display: flex;
          flex-direction: column;
        }

        .page-content {
          width: 100%;
          max-width: 1180px;
          margin: 0 auto;
          padding: 48px 28px 60px;
          flex: 1;
        }


        /* ---------------------------------------------------------------
           Hero
        ---------------------------------------------------------------- */

        .hero {
          background: #FFFFFF;
          border: 1px solid #E3E8EF;
          border-radius: 18px;
          padding: 42px 46px;
          margin-bottom: 46px;
          box-shadow: 0 8px 28px rgba(16, 24, 40, 0.045);
        }

        .hero-eyebrow {
          margin: 0 0 9px 0;
          color: #0033A0;
          font-size: 0.78rem;
          font-weight: 700;
          letter-spacing: 0.085em;
          text-transform: uppercase;
        }

        .hero h1 {
          margin: 0;
          color: #182230;
          font-size: clamp(2.2rem, 4vw, 3.4rem);
          font-weight: 700;
          letter-spacing: -0.035em;
          line-height: 1.08;
        }

        .hero-subtitle {
          max-width: 700px;
          margin: 16px 0 0;
          color: #667085;
          font-size: 1.08rem;
          line-height: 1.65;
        }


        /* ---------------------------------------------------------------
           Sections
        ---------------------------------------------------------------- */

        .app-section {
          margin-bottom: 46px;
        }

        .section-heading {
          margin-bottom: 17px;
        }

        .section-heading h2 {
          margin: 0;
          color: #182230;
          font-size: 1.28rem;
          font-weight: 650;
          letter-spacing: -0.015em;
        }

        .section-heading p {
          margin: 5px 0 0;
          color: #667085;
          font-size: 0.94rem;
        }


        /* ---------------------------------------------------------------
           App cards
        ---------------------------------------------------------------- */

        .app-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 14px;
        }

        .app-card {
          position: relative;
          display: flex;
          align-items: center;
          min-height: 142px;
          padding: 22px 54px 22px 24px;

          background: #FFFFFF;
          border: 1px solid #E3E8EF;
          border-radius: 13px;

          color: inherit;
          text-decoration: none;

          transition:
            border-color 140ms ease,
            box-shadow 140ms ease,
            transform 140ms ease,
            background-color 140ms ease;
        }

        .app-card:hover,
        .app-card:focus {
          color: inherit;
          text-decoration: none;
          border-color: #9CB6E8;
          background: #FFFFFF;
          box-shadow: 0 9px 24px rgba(16, 24, 40, 0.075);
          transform: translateY(-2px);
        }

        .app-card:focus-visible {
          outline: 3px solid rgba(0, 51, 160, 0.18);
          outline-offset: 2px;
        }

        .app-card-content {
          min-width: 0;
        }

        .app-card-header {
          display: flex;
          align-items: center;
          gap: 9px;
          flex-wrap: wrap;
        }

        .app-card-title {
          margin: 0;
          color: #182230;
          font-size: 1.05rem;
          font-weight: 650;
          line-height: 1.3;
        }

        .app-card-description {
          margin: 8px 0 0;
          color: #667085;
          font-size: 0.91rem;
          line-height: 1.5;
        }

        .app-card-arrow {
          position: absolute;
          top: 50%;
          right: 23px;
          transform: translateY(-50%);
          color: #0033A0;
          font-size: 1.3rem;
          font-weight: 500;
          transition: transform 140ms ease;
        }

        .app-card:hover .app-card-arrow {
          transform: translate(3px, -50%);
        }


        /* ---------------------------------------------------------------
           Badge
        ---------------------------------------------------------------- */

        .app-badge {
          display: inline-flex;
          align-items: center;
          padding: 3px 8px;

          color: #0033A0;
          background: #EDF3FF;

          border-radius: 999px;

          font-size: 0.7rem;
          font-weight: 650;
          line-height: 1.3;
        }


        /* ---------------------------------------------------------------
           Footer
        ---------------------------------------------------------------- */

        .footer {
          border-top: 1px solid #E3E8EF;
          background: #FFFFFF;
        }

        .footer-inner {
          width: 100%;
          max-width: 1180px;
          margin: 0 auto;
          padding: 22px 28px;

          display: flex;
          justify-content: space-between;
          gap: 24px;
          flex-wrap: wrap;

          color: #7A8494;
          font-size: 0.82rem;
        }

        .footer a {
          color: #667085;
          text-decoration: none;
        }

        .footer a:hover {
          color: #0033A0;
          text-decoration: underline;
        }


        /* ---------------------------------------------------------------
           Responsive
        ---------------------------------------------------------------- */

        @media (max-width: 760px) {

          .page-content {
            padding: 25px 16px 42px;
          }

          .hero {
            padding: 30px 25px;
            margin-bottom: 36px;
            border-radius: 14px;
          }

          .hero-subtitle {
            font-size: 1rem;
          }

          .app-grid {
            grid-template-columns: 1fr;
          }

          .app-section {
            margin-bottom: 38px;
          }

          .app-card {
            min-height: auto;
          }

          .footer-inner {
            padding: 20px 16px;
            flex-direction: column;
            gap: 6px;
          }
        }
        "
      )
    )
  ),

  div(
    class = "page-shell",

    main(
      class = "page-content",

      # -----------------------------------------------------------------
      # Hero
      # -----------------------------------------------------------------

      section(
        class = "hero",

        tags$p(
          class = "hero-eyebrow",
          "Klinisk Farmakologisk Enhed · Aalborg Universitetshospital"
        ),

        tags$h1("KFA apps"),

        tags$p(
          class = "hero-subtitle",
          paste(
            "Digitale værktøjer til lægemiddelrådgivning,",
            "medicinmonitorering, dataanalyse og klinisk farmakologi."
          )
        )
      ),


      # -----------------------------------------------------------------
      # Medicinmonitorering
      # -----------------------------------------------------------------

      app_section(
        "Medicinmonitorering",
        "Pris- og forbrugsdata til monitorering og analyse.",

        app_card(
          title = "Lægemiddelpriser · Sundhedsdatastyrelsen",
          description = paste(
            "Sammenlign priser, substitutionsgrupper",
            "og prisudvikling i nationale data."
          ),
          href = "https://kfaapps.au.dk/lmgrupper/"
        ),

        app_card(
          title = "Lægemiddelpriser · Medicinpriser.dk",
          description = paste(
            "Aktuelle priser og substitutionsgrupper",
            "baseret på erhverv.medicinpriser.dk."
          ),
          href = "https://kfaapps.au.dk/lmgrupper2/"
        ),

        app_card(
          title = "Lægemiddelforbrug · Excel",
          description = paste(
            "Visualisér lægemiddelforbrug og",
            "tilskudsudgifter fra eSundhed-data."
          ),
          href = "https://kfaapps.au.dk/lmforbrug/"
        ),

        app_card(
          title = "Lægemiddelforbrug · Power BI",
          description = paste(
            "Hent og bearbejd medicinsalgsdata",
            "fra Power BI."
          ),
          href = "https://kfaapps.au.dk/powerbi/"
        )
      ),


      # -----------------------------------------------------------------
      # Batchopslag
      # -----------------------------------------------------------------

      app_section(
        "Medicingennemgang og batchopslag",
        "Arbejd med flere lægemidler eller ATC-koder på én gang.",

        app_card(
          title = "ATC-kodegenerator",
          description = paste(
            "Find ATC-koder ud fra lægemiddel- eller",
            "substansnavne og saml dem til videre opslag."
          ),
          href = "https://kfaapps.au.dk/atc/"
        ),

        app_card(
          title = "Lister og interaktioner",
          description = paste(
            "Screen en medicinliste for farmakologiske",
            "risici, relevante lister og interaktioner."
          ),
          href = "https://kfaapps.au.dk/lister/",
          badge = "Moderniseres"
        ),

        app_card(
          title = "Bivirkninger",
          description = paste(
            "Hent bivirkningstabeller fra pro.medicin.dk",
            "for flere ATC-koder ad gangen."
          ),
          href = "https://kfaapps.au.dk/bivirkninger/"
        ),

        app_card(
          title = "Graviditet og amning",
          description = paste(
            "Saml oplysninger fra pro.medicin.dk,",
            "Janusmed og LactMed."
          ),
          href = "https://kfaapps.au.dk/graviditet/"
        )
      ),


      # -----------------------------------------------------------------
      # Andre kliniske værktøjer
      # -----------------------------------------------------------------

      app_section(
        "Kliniske værktøjer",
        "Simulation, dokumentation og patientnære analyser.",

        app_card(
          title = "Farmakokinetisk simulation",
          description = paste(
            "Simulér koncentrationsprofiler og PK/PD",
            "ved forskellige doseringsregimer."
          ),
          href = "https://kfaapps.au.dk/farmakokinetik/"
        ),

        app_card(
          title = "Compliance",
          description = paste(
            "Lokal analyse af apoteksudleveringer,",
            "dagligt forbrug og behandlingsmønstre."
          ),
          href = "https://kfaapps.au.dk/compliance/"
        ),

        app_card(
          title = "Regulatorisk dokumentfinder",
          description = paste(
            "Find produktresuméer og regulatoriske dokumenter",
            "fra danske, europæiske og amerikanske kilder."
          ),
          href = "https://kfaapps.au.dk/spc/"
        )
      ),


      # -----------------------------------------------------------------
      # Enhedsdata
      # -----------------------------------------------------------------

      app_section(
        "Enhedsdata",
        "Overblik over Klinisk Farmakologisk Enheds aktiviteter.",

        app_card(
          title = "Afdelingsstatistik",
          description = paste(
            "Visualisér aktiviteter, svartider",
            "og øvrige nøgletal for enheden."
          ),
          href = "https://kfaapps.au.dk/statistik/"
        )
      )
    ),


    # -------------------------------------------------------------------
    # Footer
    # -------------------------------------------------------------------

    footer(
      class = "footer",

      div(
        class = "footer-inner",

        div(
          "KFA apps · Klinisk Farmakologisk Enhed, AUH"
        ),

        div(
          textOutput(
            "footer_text",
            inline = TRUE
          ),
          " · ",
          tags$a(
            href = "mailto:frekra@biomed.au.dk",
            "Support"
          )
        )
      )
    )
  )
)


# -------------------------------------------------------------------------
# Server
# -------------------------------------------------------------------------

server <- function(input, output, session) {

  output$footer_text <- renderText({

    app_file <- "app.R"

    last_date <- if (file.exists(app_file)) {
      format(
        file.info(app_file)$mtime,
        "%d-%m-%Y"
      )
    } else {
      format(
        Sys.Date(),
        "%d-%m-%Y"
      )
    }

    paste0(
      "Senest opdateret ",
      last_date
    )
  })
}


shinyApp(
  ui = ui,
  server = server
)
