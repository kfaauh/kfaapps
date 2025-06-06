library(shiny)
library(mrgsolve)
library(ggplot2)
library(dplyr)

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Farmakokinetisk simulation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("model_type", "Vælg PK-model:", 
                  choices = c(
                    "1-kompartment IV bolus"    = "1cpt_ivbolus",
                    "1-kompartment IV infusion" = "1cpt_ivinf",
                    "1-kompartment oralt"       = "1cpt_oral",
                    "2-kompartment IV bolus"    = "2cpt_ivbolus",
                    "2-kompartment IV infusion" = "2cpt_ivinf",
                    "2-kompartment oralt"       = "2cpt_oral"
                  )
      ),
      
      # 1-kompartment parametre
      conditionalPanel(
        condition = "input.model_type == '1cpt_ivbolus' ||
                     input.model_type == '1cpt_ivinf'  ||
                     input.model_type == '1cpt_oral'",
        numericInput("CL_1cpt", "Clearance (L/t):", 1.43, min = 0.01, step = 0.01),
        numericInput("V_1cpt",  "Fordelingsvolumen (L):", 54, min = 1, step = 0.1)
      ),
      
      # 2-kompartment parametre
      conditionalPanel(
        condition = "input.model_type == '2cpt_ivbolus' ||
                     input.model_type == '2cpt_ivinf'  ||
                     input.model_type == '2cpt_oral'",
        numericInput("CL_2cpt", "Clearance (L/t):",            1.43, min = 0.01, step = 0.01),
        numericInput("V_2cpt",  "Central volumen (L):",         54,   min = 1,    step = 0.1),
        numericInput("Q_2cpt",  "Interkompartmental clearance (L/t):", 2,  min = 0,    step = 0.1),
        numericInput("V2_2cpt", "Perifer volumen (L):",         20,   min = 1,    step = 0.1)
      ),
      
      # Orale parametre (første-ordens absorption)
      conditionalPanel(
        condition = "input.model_type == '1cpt_oral' ||
                     input.model_type == '2cpt_oral'",
        numericInput("Ka_oral",   "Absorptionsratekonstant Ka (1/t):", 0.5, min = 0,  step = 0.01),
        numericInput("F_oral",    "Biotilgængelighed (F):",           1,   min = 0,  max = 1, step = 0.01),
        numericInput("Tlag_oral", "Forsinkelsestid (Tlag, t):",       0,   min = 0,  step = 0.1)
      ),
      
      # Infusionsparametre
      conditionalPanel(
        condition = "input.model_type == '1cpt_ivinf' ||
                     input.model_type == '2cpt_ivinf'",
        numericInput("infTime", "Infusionsvarighed (t):", 1, min = 0.01, step = 0.1)
      ),
      
      numericInput("sim_days", "Simulationsvarighed (dage):", 7, min = 1),
      
      hr(),
      h4("Doseringsregime 1"),
      textInput("amt1",  "Dosis (kommasepareret):",  "500,500"),
      textInput("time1", "Doseringstidspunkter (HH:MM, kommasepareret):", "08:00,20:00"),
      numericInput("days1", "Antal behandlingsdage:", 7, min = 1),
      
      checkboxInput("enableRegimen2", "Tilføj andet doseringsregime?", FALSE),
      
      conditionalPanel(
        condition = "input.enableRegimen2",
        h4("Doseringsregime 2"),
        textInput("amt2",  "Dosis (kommasepareret):", "1000"),
        textInput("time2", "Doseringstidspunkter (HH:MM, kommasepareret):", "12:00"),
        numericInput("days2", "Antal behandlingsdage:", 7, min = 1)
      ),
      
      hr(),
      # 4) Enheds-felt
      textInput("unit", "Enhed (f.eks. 'mg' eller 'g'):", "mg"),
      
      hr(),
      # 3) Tærskellinje option
      checkboxInput("enableThreshold", "Vis tærskellinje?", FALSE),
      conditionalPanel(
        condition = "input.enableThreshold",
        numericInput("threshold", "Tærskelværdi (samme enhed som koncentration):", 16, min = 0)
      ),
      
      actionButton("simulate", "Generér plot")
    ),
    
    mainPanel(
      # Tilføj hoverOpts i plotOutput
      plotOutput(
        "pkPlot", 
        height = "500px",
        hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "throttle")
      ),
      verbatimTextOutput("hover_info")
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # Omregn tid "HH:MM" til decimale timer
  convert_time_to_hours <- function(time_vector) {
    sapply(time_vector, function(time_str) {
      parts <- strsplit(time_str, ":")[[1]]
      as.numeric(parts[1]) + as.numeric(parts[2]) / 60
    })
  }
  
  # Generer mrgsolve-events for et givent doseringsregime
  generate_dosing_events <- function(amounts, times, days, model_type) {
    amounts <- as.numeric(unlist(strsplit(amounts, ",")))
    times   <- convert_time_to_hours(unlist(strsplit(times, ",")))
    
    if (length(amounts) != length(times)) {
      showNotification("Fejl: Antal doser og tidspunkter skal matche", type = "error")
      return(NULL)
    }
    
    # Udvid doser over flere dage
    full_times   <- rep(times, times = days) + 
      rep(seq(0, (days - 1) * 24, by = 24), each = length(times))
    full_amounts <- rep(amounts, times = days)
    
    # Sorter efter tid
    df <- data.frame(time = full_times, amt = full_amounts)
    df <- df[order(df$time), ]
    
    # Vælg administrationsmåde baseret på model_type
    if (model_type %in% c("1cpt_ivbolus", "2cpt_ivbolus")) {
      ev_obj <- ev(time = df$time, cmt = 1, amt = df$amt)
      
    } else if (model_type %in% c("1cpt_ivinf", "2cpt_ivinf")) {
      if (input$infTime <= 0) {
        showNotification("Infusionsvarighed skal være > 0", type = "error")
        return(NULL)
      }
      rates <- df$amt / input$infTime
      ev_obj <- ev(time = df$time, cmt = 1, amt = df$amt, rate = rates)
      
    } else {
      ev_obj <- ev(time = df$time, cmt = 1, amt = df$amt)
    }
    
    return(ev_obj)
  }
  
  # Gem det simulerede data i en reaktiv værdi
  sim_data <- reactiveVal(NULL)
  
  observeEvent(input$simulate, {
    # Byg modelkode baseret på brugerens valg
    model_code <- ""
    
    if (input$model_type == "1cpt_ivbolus") {
      model_code <- paste0(
        "$PARAM CL=", input$CL_1cpt, ", V=", input$V_1cpt, "\n",
        "$CMT CENT\n",
        "$ODE\n",
        "dxdt_CENT = -(CL/V)*CENT;\n",
        "$TABLE\n",
        "double CP = CENT / V;\n",
        "$CAPTURE CP\n"
      )
      
    } else if (input$model_type == "1cpt_ivinf") {
      model_code <- paste0(
        "$PARAM CL=", input$CL_1cpt, ", V=", input$V_1cpt, "\n",
        "$CMT CENT\n",
        "$ODE\n",
        "// Elimination ved første-orden\n",
        "dxdt_CENT = -(CL/V)*CENT;\n",
        "$TABLE\n",
        "double CP = CENT / V;\n",
        "$CAPTURE CP\n"
      )
      
    } else if (input$model_type == "1cpt_oral") {
      model_code <- paste0(
        "$PARAM CL=", input$CL_1cpt, ", V=", input$V_1cpt,
        ", Ka=", input$Ka_oral, ", F=", input$F_oral, ", Tlag=", input$Tlag_oral, "\n",
        "$CMT GUT CENT\n",
        "$ODE\n",
        "if (SOLVERTIME >= Tlag) {\n",
        "  dxdt_GUT  = -Ka*GUT;\n",
        "  dxdt_CENT =  Ka*GUT*F - (CL/V)*CENT;\n",
        "} else {\n",
        "  dxdt_GUT  = 0;\n",
        "  dxdt_CENT = 0;\n",
        "}\n",
        "$TABLE\n",
        "double CP = CENT / V;\n",
        "$CAPTURE CP\n"
      )
      
    } else if (input$model_type == "2cpt_ivbolus") {
      model_code <- paste0(
        "$PARAM CL=", input$CL_2cpt, ", V=", input$V_2cpt,
        ", Q=", input$Q_2cpt, ", V2=", input$V2_2cpt, "\n",
        "$CMT CENT PERIPH\n",
        "$ODE\n",
        "dxdt_CENT   = -(CL/V)*CENT - Q*(CENT/V) + Q*(PERIPH/V2);\n",
        "dxdt_PERIPH =  Q*(CENT/V - PERIPH/V2);\n",
        "$TABLE\n",
        "double CP = CENT / V;\n",
        "$CAPTURE CP\n"
      )
      
    } else if (input$model_type == "2cpt_ivinf") {
      model_code <- paste0(
        "$PARAM CL=", input$CL_2cpt, ", V=", input$V_2cpt,
        ", Q=", input$Q_2cpt, ", V2=", input$V2_2cpt, "\n",
        "$CMT CENT PERIPH\n",
        "$ODE\n",
        "dxdt_CENT   = -(CL/V)*CENT - Q*(CENT/V) + Q*(PERIPH/V2);\n",
        "dxdt_PERIPH =  Q*(CENT/V - PERIPH/V2);\n",
        "$TABLE\n",
        "double CP = CENT / V;\n",
        "$CAPTURE CP\n"
      )
      
    } else if (input$model_type == "2cpt_oral") {
      model_code <- paste0(
        "$PARAM CL=", input$CL_2cpt, ", V=", input$V_2cpt,
        ", Q=", input$Q_2cpt, ", V2=", input$V2_2cpt,
        ", Ka=", input$Ka_oral, ", F=", input$F_oral, ", Tlag=", input$Tlag_oral, "\n",
        "$CMT GUT CENT PERIPH\n",
        "$ODE\n",
        "if (SOLVERTIME >= Tlag) {\n",
        "  dxdt_GUT  = -Ka*GUT;\n",
        "  dxdt_CENT = Ka*GUT*F - (CL/V)*CENT - Q*(CENT/V) + Q*(PERIPH/V2);\n",
        "  dxdt_PERIPH = Q*(CENT/V - PERIPH/V2);\n",
        "} else {\n",
        "  dxdt_GUT      = 0;\n",
        "  dxdt_CENT     = 0;\n",
        "  dxdt_PERIPH   = 0;\n",
        "}\n",
        "$TABLE\n",
        "double CP = CENT / V;\n",
        "$CAPTURE CP\n"
      )
    }
    
    # Kompilér mrgsolve-model
    mod <- mcode("pk_model", model_code)
    
    # Simulationstid i timer
    sim_end <- input$sim_days * 24
    
    # 1) Simuler Regime 1
    regimen1 <- generate_dosing_events(
      amounts    = input$amt1,
      times      = input$time1,
      days       = input$days1,
      model_type = input$model_type
    )
    if (is.null(regimen1)) {
      sim_data(NULL)
      return()
    }
    sim1 <- mod %>% ev(regimen1) %>% mrgsim(end = sim_end, delta = 0.1)
    df1 <- as.data.frame(sim1)
    df1$Regime <- "Regime 1"
    
    # 2) Simuler Regime 2 (hvis valgt)
    df2 <- NULL
    if (input$enableRegimen2) {
      regimen2 <- generate_dosing_events(
        amounts    = input$amt2,
        times      = input$time2,
        days       = input$days2,
        model_type = input$model_type
      )
      if (is.null(regimen2)) {
        sim_data(NULL)
        return()
      }
      sim2 <- mod %>% ev(regimen2) %>% mrgsim(end = sim_end, delta = 0.1)
      df2 <- as.data.frame(sim2)
      df2$Regime <- "Regime 2"
    }
    
    # Kombinér de to dataframes (hvis df2 findes)
    if (!is.null(df2)) {
      df_all <- bind_rows(df1, df2)
    } else {
      df_all <- df1
    }
    
    sim_data(df_all)
  })
  
  # ------------------------------------------------------------
  # 2) Tegn graf med ggplot, men brug Shiny’s hover til at fange nærmeste punkt
  # ------------------------------------------------------------
  output$pkPlot <- renderPlot({
    df_all <- sim_data()
    if (is.null(df_all)) return(NULL)
    
    sim_end <- input$sim_days * 24
    
    # Selve ggplot
    p <- ggplot(df_all, aes(x = time, y = CP, color = Regime)) +
      geom_line(size = 1.2) +
      labs(
        title = "Plasmakoncentration over tid",
        x = "Tid (timer)",
        y = paste0("Koncentration i ", input$unit, "/L")
      ) +
      theme_minimal(base_size = 14) +
      scale_color_manual(values = c("Regime 1" = "blue", "Regime 2" = "red")) +
      scale_x_continuous(breaks = seq(0, sim_end, by = 24), limits = c(0, sim_end))
    
    # Tærskellinje, hvis valgt
    if (isTRUE(input$enableThreshold) && !is.null(input$threshold)) {
      p <- p +
        geom_hline(yintercept = input$threshold,
                   linetype = "dashed",
                   color = "black",
                   size = 1) +
        annotate(
          "text",
          x = sim_end * 0.05,
          y = input$threshold * 1.02,
          label = paste0("Tærskel: ", input$threshold, " ", input$unit, "/L"),
          hjust = 0,
          vjust = 0,
          size = 4,
          color = "black"
        )
    }
    
    p
  })
  
  # ------------------------------------------------------------
  # 3) Brug nearPoints() til at finde det datapunkt, brugeren hover over,
  #    og udskriv tiden og koncentrationen
  # ------------------------------------------------------------
  output$hover_info <- renderText({
    df_all <- sim_data()
    hover <- input$plot_hover
    if (is.null(df_all) || is.null(hover)) {
      # returnér kun denne tekst (ingen cat(), så intet NULL kommer med)
      return("Før musen hen over grafen for at se tids- og koncentrations-værdier.")
    }
    point <- nearPoints(
      df_all, hover,
      xvar = "time",
      yvar = "CP",
      maxpoints = 1,
      threshold = 10
    )
    if (nrow(point) == 0) {
      return("Ingen data tæt på hover-punktet.")
    }
    # Ellers returnér også bare som én streng:
    paste0(
      "Regime: ", point$Regime, "\n",
      "Tid: ", round(point$time, 2), " timer\n",
      "CP: ", round(point$CP, 2), " ", input$unit, "/L"
    )
  })
}

shinyApp(ui, server)
