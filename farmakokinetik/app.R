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
      selectInput("model_type", "Vælg model:", 
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
      textInput("time1", "Doseringstidspunkter (TT:MM, kommasepareret):", "08:00,20:00"),
      numericInput("days1", "Antal behandlingsdage:", 7, min = 1),
      
      checkboxInput("enableRegimen2", "Tilføj andet doseringsregime", FALSE),
      
      conditionalPanel(
        condition = "input.enableRegimen2",
        h4("Doseringsregime 2"),
        textInput("amt2",  "Dosis (kommasepareret):", "1000"),
        textInput("time2", "Doseringstidspunkter (TT:MM, kommasepareret):", "12:00"),
        numericInput("days2", "Antal behandlingsdage:", 7, min = 1)
      ),
      
      hr(),
      # 4) Enheds-felt
      textInput("unit", "Enhed (f.eks. 'mg' eller 'g'):", "mg"),
      
      hr(),
      # 3) Tærskellinje option
      checkboxInput("enableThreshold", "Vis tærskellinje", FALSE),
      conditionalPanel(
        condition = "input.enableThreshold",
        numericInput("threshold", "Tærskelværdi (samme enhed som koncentration):", 16, min = 0)
      ),
      
      actionButton("simulate", "Generér plot")
    ),
    
    mainPanel(
      # -------------------------
      # NEW: Tabs i mainPanel
      # -------------------------
      tabsetPanel(
        tabPanel(
          "Simulation",
          plotOutput(
            "pkPlot", 
            height = "500px",
            hover = hoverOpts(id = "plot_hover", delay = 100, delayType = "throttle")
          ),
          verbatimTextOutput("hover_info")
        ),
        
        tabPanel(
          "Variabel-udregner",
          tags$div(
            style = "max-width: 900px;",
            h4("Udregner af farmakokinetiske variable"),
            helpText(
              "Felterne kan være tomme. Når du indtaster en eller flere værdier, udfyldes de øvrige felter automatisk, hvis de kan udledes.",
              br(),
              "Antagelser: Ke = CL/V, T½ = ln(2)/Ke. For Tmax/Ka anvendes Bateman-funktionen for 1-kompartment, 1.-ordens absorption: Tmax = ln(Ka/Ke)/(Ka-Ke) med Ka > Ke."
            ),
            fluidRow(
              column(4, textInput("pk_V",   "Fordelingsvolumen V (L):", "")),
              column(4, textInput("pk_CL",  "Clearance CL (L/t):", "")),
              column(4, textInput("pk_Ke",  "Eliminationskonstant Ke (1/t):", ""))
            ),
            fluidRow(
              column(4, textInput("pk_t12", "T½ (t):", "")),
              column(4, textInput("pk_Tmax","Tmax (t):", "")),
              column(4, textInput("pk_Ka",  "Absorptionsratekonstant Ka (1/t):", ""))
            ),
            hr(),
            actionButton("pk_reset", "Nulstil felter")
          )
        )
      )
    )
  )
)

# ------------------------------------------------------------
# SERVER
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  # Omregn tid "TT:MM" til decimale timer
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
  # 3) Hover-info
  # ------------------------------------------------------------
  output$hover_info <- renderText({
    df_all <- sim_data()
    hover <- input$plot_hover
    if (is.null(df_all) || is.null(hover)) {
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
    paste0(
      "Regime: ", point$Regime, "\n",
      "Tid: ", round(point$time, 2), " timer\n",
      "CP: ", round(point$CP, 2), " ", input$unit, "/L"
    )
  })
  
  # ------------------------------------------------------------
  # NEW: PK-UDREGNER (auto-udfyld felter)
  # ------------------------------------------------------------
  
  to_num_or_na <- function(x) {
    x <- trimws(x %||% "")
    if (identical(x, "") || is.na(x)) return(NA_real_)
    x <- gsub(",", ".", x, fixed = FALSE)  # tillad 0,5 -> 0.5
    suppressWarnings(as.numeric(x))
  }
  
  fmt <- function(x) {
    if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
    format(round(x, 6), scientific = FALSE, trim = TRUE)
  }
  
  tmax_from_ka_ke <- function(Ka, Ke) {
    if (is.na(Ka) || is.na(Ke)) return(NA_real_)
    if (Ka <= Ke || Ka <= 0 || Ke <= 0) return(NA_real_)
    log(Ka / Ke) / (Ka - Ke)
  }
  
  solve_ka_from_tmax_ke <- function(Tmax, Ke) {
    if (is.na(Tmax) || is.na(Ke)) return(NA_real_)
    if (Tmax <= 0 || Ke <= 0) return(NA_real_)
    f <- function(Ka) tmax_from_ka_ke(Ka, Ke) - Tmax
    lower <- Ke * 1.0001
    upper <- max(Ke * 1000, 1)
    if (is.na(f(lower)) || is.na(f(upper))) return(NA_real_)
    if (f(lower) * f(upper) > 0) return(NA_real_)
    uniroot(f, lower = lower, upper = upper)$root
  }
  
  solve_ke_from_tmax_ka <- function(Tmax, Ka) {
    if (is.na(Tmax) || is.na(Ka)) return(NA_real_)
    if (Tmax <= 0 || Ka <= 0) return(NA_real_)
    f <- function(Ke) tmax_from_ka_ke(Ka, Ke) - Tmax
    lower <- Ka / 1000
    upper <- Ka / 1.0001
    if (lower <= 0) lower <- 1e-6
    if (is.na(f(lower)) || is.na(f(upper))) return(NA_real_)
    if (f(lower) * f(upper) > 0) return(NA_real_)
    uniroot(f, lower = lower, upper = upper)$root
  }
  
  updating_pk <- reactiveVal(FALSE)
  
  update_pk_fields <- function(vals) {
    updating_pk(TRUE)
    on.exit(updating_pk(FALSE), add = TRUE)
    updateTextInput(session, "pk_V",    value = fmt(vals$V))
    updateTextInput(session, "pk_CL",   value = fmt(vals$CL))
    updateTextInput(session, "pk_Ke",   value = fmt(vals$Ke))
    updateTextInput(session, "pk_t12",  value = fmt(vals$t12))
    updateTextInput(session, "pk_Tmax", value = fmt(vals$Tmax))
    updateTextInput(session, "pk_Ka",   value = fmt(vals$Ka))
  }
  
  compute_pk <- function(V, CL, Ke, t12, Tmax, Ka) {
    vals <- list(V = V, CL = CL, Ke = Ke, t12 = t12, Tmax = Tmax, Ka = Ka)
    
    for (i in 1:10) {
      # Ke <-> CL/V
      if (is.na(vals$Ke) && !is.na(vals$CL) && !is.na(vals$V) && vals$V > 0) {
        vals$Ke <- vals$CL / vals$V
      }
      if (is.na(vals$CL) && !is.na(vals$Ke) && !is.na(vals$V)) {
        vals$CL <- vals$Ke * vals$V
      }
      if (is.na(vals$V) && !is.na(vals$CL) && !is.na(vals$Ke) && vals$Ke > 0) {
        vals$V <- vals$CL / vals$Ke
      }
      
      # t1/2 <-> Ke
      if (is.na(vals$t12) && !is.na(vals$Ke) && vals$Ke > 0) {
        vals$t12 <- log(2) / vals$Ke
      }
      if (is.na(vals$Ke) && !is.na(vals$t12) && vals$t12 > 0) {
        vals$Ke <- log(2) / vals$t12
      }
      
      # Tmax <-> Ka,Ke
      if (is.na(vals$Tmax) && !is.na(vals$Ka) && !is.na(vals$Ke)) {
        vals$Tmax <- tmax_from_ka_ke(vals$Ka, vals$Ke)
      }
      if (is.na(vals$Ka) && !is.na(vals$Tmax) && !is.na(vals$Ke)) {
        vals$Ka <- solve_ka_from_tmax_ke(vals$Tmax, vals$Ke)
      }
      if (is.na(vals$Ke) && !is.na(vals$Tmax) && !is.na(vals$Ka)) {
        vals$Ke <- solve_ke_from_tmax_ka(vals$Tmax, vals$Ka)
      }
    }
    
    vals
  }
  
  observeEvent(
    {
      input$pk_V; input$pk_CL; input$pk_Ke; input$pk_t12; input$pk_Tmax; input$pk_Ka
    },
    {
      if (isTRUE(updating_pk())) return()
      
      V    <- to_num_or_na(input$pk_V)
      CL   <- to_num_or_na(input$pk_CL)
      Ke   <- to_num_or_na(input$pk_Ke)
      t12  <- to_num_or_na(input$pk_t12)
      Tmax <- to_num_or_na(input$pk_Tmax)
      Ka   <- to_num_or_na(input$pk_Ka)
      
      vals <- compute_pk(V, CL, Ke, t12, Tmax, Ka)
      
      # Mild feedback hvis Ka/Ke-kravet brydes
      if (!is.na(vals$Ka) && !is.na(vals$Ke) && vals$Ka <= vals$Ke) {
        showNotification("Bemærk: Tmax-formlen kræver Ka > Ke. Tjek dine input.", type = "warning", duration = 4)
      }
      
      update_pk_fields(vals)
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$pk_reset, {
    updating_pk(TRUE)
    on.exit(updating_pk(FALSE), add = TRUE)
    updateTextInput(session, "pk_V",    value = "")
    updateTextInput(session, "pk_CL",   value = "")
    updateTextInput(session, "pk_Ke",   value = "")
    updateTextInput(session, "pk_t12",  value = "")
    updateTextInput(session, "pk_Tmax", value = "")
    updateTextInput(session, "pk_Ka",   value = "")
  })
}

shinyApp(ui, server)
