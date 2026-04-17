library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(openxlsx)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Farmakokinetisk simulation"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "model_type", "Vælg model:",
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
        numericInput("CL_2cpt", "Clearance (L/t):", 1.43, min = 0.01, step = 0.01),
        numericInput("V_2cpt",  "Central volumen (L):", 54, min = 1, step = 0.1),
        numericInput("Q_2cpt",  "Interkompartmental clearance (L/t):", 2, min = 0, step = 0.1),
        numericInput("V2_2cpt", "Perifer volumen (L):", 20, min = 1, step = 0.1)
      ),
      
      # Orale parametre
      conditionalPanel(
        condition = "input.model_type == '1cpt_oral' ||
                     input.model_type == '2cpt_oral'",
        numericInput("Ka_oral",   "Absorptionsratekonstant Ka (1/t):", 0.5, min = 0, step = 0.01),
        numericInput("F_oral",    "Biotilgængelighed (F):", 1, min = 0, max = 1, step = 0.01),
        numericInput("Tlag_oral", "Forsinkelsestid (Tlag, t):", 0, min = 0, step = 0.1)
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
      textInput("amt1", "Dosis (kommasepareret):", "500,500"),
      textInput("time1", "Doseringstidspunkter (TT:MM, kommasepareret):", "08:00,20:00"),
      numericInput("days1", "Antal behandlingsdage:", 7, min = 1),
      
      checkboxInput("enableRegimen2", "Tilføj andet doseringsregime", FALSE),
      
      conditionalPanel(
        condition = "input.enableRegimen2",
        h4("Doseringsregime 2"),
        textInput("amt2", "Dosis (kommasepareret):", "1000"),
        textInput("time2", "Doseringstidspunkter (TT:MM, kommasepareret):", "12:00"),
        numericInput("days2", "Antal behandlingsdage:", 7, min = 1)
      ),
      
      hr(),
      textInput("unit", "Enhed (f.eks. 'mg' eller 'g'):", "mg"),
      
      hr(),
      h4("Proteinbinding og visning"),
      numericInput("protein_binding", "Proteinbinding (%):", 0, min = 0, max = 100, step = 1),
      selectInput(
        "conc_display", "Vis koncentration i plot:",
        choices = c("Total" = "total", "Fri" = "free", "Begge" = "both"),
        selected = "total"
      ),
      
      hr(),
      h4("PK/PD-analyse"),
      checkboxInput("enableMIC", "Aktivér MIC-analyse", FALSE),
      conditionalPanel(
        condition = "input.enableMIC",
        numericInput("MIC", "MIC (samme enhed som koncentration):", 16, min = 0, step = 0.1),
        radioButtons(
          "mic_basis", "Beregn T > MIC ud fra:",
          choices = c("Total koncentration" = "total", "Fri koncentration" = "free"),
          selected = "total"
        )
      ),
      selectInput(
        "analysis_window", "Analysevindue:",
        choices = c(
          "Hele simulationen" = "all",
          "Første 24 timer" = "first24",
          "Sidste 24 timer" = "last24",
          "Brugerdefineret interval" = "custom"
        ),
        selected = "all"
      ),
      conditionalPanel(
        condition = "input.analysis_window == 'custom'",
        numericInput("analysis_start", "Fra tid (timer):", 0, min = 0, step = 0.1),
        numericInput("analysis_end", "Til tid (timer):", 24, min = 0, step = 0.1)
      ),
      
      actionButton("simulate", "Generér plot")
    ),
    
    mainPanel(
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
          "Analyse",
          tags$div(
            style = "max-width: 1100px;",
            h4("Opsummering af simulerede data"),
            helpText(
              "AUC beregnes numerisk med trapezmetoden i det valgte analysevindue.",
              br(),
              "T > MIC beregnes med lineær interpolation mellem simuleringspunkter."
            ),
            tableOutput("analysisTable"),
            br(),
            downloadButton("downloadExcel", "Download Excel med baggrundsdata")
          )
        ),
        
        tabPanel(
          "Variabel-udregner",
          tags$div(
            style = "max-width: 900px;",
            h4("Udregner af farmakokinetiske variable"),
            helpText(
              "Felter med indhold betragtes som bruger-input og overskrives ikke.",
              br(),
              "Slet et felt for at frigøre det, så appen kan beregne det ud fra de øvrige værdier.",
              br(),
              "Antagelser: Ke = CL/V, T½ = ln(2)/Ke. For Tmax/Ka anvendes Bateman-funktionen for",
              " 1-kompartment, 1.-ordens absorption: Tmax = ln(Ka/Ke)/(Ka-Ke) med Ka > Ke."
            ),
            fluidRow(
              column(4, textInput("pk_V", "Fordelingsvolumen V (L):", "")),
              column(4, textInput("pk_CL", "Clearance CL (L/t):", "")),
              column(4, textInput("pk_Ke", "Eliminationskonstant Ke (1/t):", ""))
            ),
            fluidRow(
              column(4, textInput("pk_t12", "T½ (t):", "")),
              column(4, textInput("pk_Tmax", "Tmax (t):", "")),
              column(4, textInput("pk_Ka", "Absorptionsratekonstant Ka (1/t):", ""))
            ),
            hr(),
            verbatimTextOutput("pk_status"),
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
  
  model_labels <- c(
    "1cpt_ivbolus" = "1-kompartment IV bolus",
    "1cpt_ivinf"   = "1-kompartment IV infusion",
    "1cpt_oral"    = "1-kompartment oralt",
    "2cpt_ivbolus" = "2-kompartment IV bolus",
    "2cpt_ivinf"   = "2-kompartment IV infusion",
    "2cpt_oral"    = "2-kompartment oralt"
  )
  
  sim_result <- reactiveVal(NULL)
  
  # ------------------------------------------------------------
  # Hjælpefunktioner til simulation
  # ------------------------------------------------------------
  split_trim <- function(x) {
    trimws(unlist(strsplit(x, ",")))
  }
  
  convert_time_to_hours <- function(time_vector) {
    sapply(time_vector, function(time_str) {
      if (!grepl("^\\d{1,2}:\\d{2}$", time_str)) {
        stop(paste0("Ugyldigt klokkeslæt: ", time_str, ". Brug formatet TT:MM."))
      }
      parts <- strsplit(time_str, ":")[[1]]
      hh <- as.numeric(parts[1])
      mm <- as.numeric(parts[2])
      
      if (is.na(hh) || is.na(mm) || hh < 0 || hh > 23 || mm < 0 || mm > 59) {
        stop(paste0("Ugyldigt klokkeslæt: ", time_str, ". Timer skal være 0-23 og minutter 0-59."))
      }
      
      hh + mm / 60
    })
  }
  
  decimal_to_clock <- function(hours_vec) {
    sapply(hours_vec, function(h) {
      hh <- floor(h %% 24)
      mm <- round((h %% 1) * 60)
      if (mm == 60) {
        hh <- (hh + 1) %% 24
        mm <- 0
      }
      sprintf("%02d:%02d", hh, mm)
    })
  }
  
  get_model_params <- function() {
    if (input$model_type %in% c("1cpt_ivbolus", "1cpt_ivinf", "1cpt_oral")) {
      list(
        CL = input$CL_1cpt,
        Vc = input$V_1cpt,
        Ka = if (input$model_type == "1cpt_oral") input$Ka_oral else NA_real_,
        Q = NA_real_,
        Vp = NA_real_
      )
    } else {
      list(
        CL = input$CL_2cpt,
        Vc = input$V_2cpt,
        Ka = if (input$model_type == "2cpt_oral") input$Ka_oral else NA_real_,
        Q = input$Q_2cpt,
        Vp = input$V2_2cpt
      )
    }
  }
  
  generate_dosing_events <- function(amounts, times, days, model_type, regimen_name) {
    amounts_vec <- as.numeric(split_trim(amounts))
    times_chr   <- split_trim(times)
    times_vec   <- convert_time_to_hours(times_chr)
    
    if (any(is.na(amounts_vec))) {
      stop("Doser skal være numeriske værdier adskilt af komma.")
    }
    
    if (length(amounts_vec) != length(times_vec)) {
      stop("Antal doser og antal tidspunkter skal matche.")
    }
    
    full_times <- rep(times_vec, times = days) +
      rep(seq(0, (days - 1) * 24, by = 24), each = length(times_vec))
    full_amounts <- rep(amounts_vec, times = days)
    
    route_label <- "Oral"
    target_compartment <- "GUT"
    rate_values <- rep(NA_real_, length(full_times))
    event_doses <- full_amounts
    event_times <- full_times
    
    if (model_type %in% c("1cpt_ivbolus", "2cpt_ivbolus")) {
      route_label <- "IV bolus"
      target_compartment <- "CENT"
    } else if (model_type %in% c("1cpt_ivinf", "2cpt_ivinf")) {
      if (is.null(input$infTime) || is.na(input$infTime) || input$infTime <= 0) {
        stop("Infusionsvarighed skal være > 0.")
      }
      route_label <- "IV infusion"
      target_compartment <- "CENT"
      rate_values <- full_amounts / input$infTime
    } else if (model_type %in% c("1cpt_oral", "2cpt_oral")) {
      route_label <- "Oral"
      target_compartment <- "GUT"
      event_doses <- full_amounts * input$F_oral
      event_times <- full_times + input$Tlag_oral
    }
    
    data.frame(
      Regime = regimen_name,
      Day = floor(full_times / 24) + 1,
      Oprindelig_tid_h = full_times,
      Tid_h = event_times,
      Clock_time = decimal_to_clock(full_times),
      Event_clock_time = decimal_to_clock(event_times),
      Dose = full_amounts,
      Event_dose = event_doses,
      Rate = rate_values,
      Target = target_compartment,
      Route = route_label,
      stringsAsFactors = FALSE
    )
  }
  
  simulate_regimen <- function(events_df, model_type, params, sim_end, delta) {
    tol <- 1e-10
    
    regular_grid <- seq(0, sim_end, by = delta)
    
    bolus_times <- events_df %>%
      filter(Route %in% c("IV bolus", "Oral"), Tid_h <= sim_end) %>%
      pull(Tid_h)
    
    infusion_change_times <- events_df %>%
      filter(Route == "IV infusion") %>%
      transmute(start = Tid_h, stop = pmin(Tid_h + input$infTime, sim_end)) %>%
      as.data.frame()
    
    extra_times <- c(bolus_times)
    if (nrow(infusion_change_times) > 0) {
      extra_times <- c(extra_times, infusion_change_times$start, infusion_change_times$stop)
    }
    
    sim_times <- sort(unique(round(c(regular_grid, extra_times, sim_end), 10)))
    sim_times <- sim_times[sim_times >= 0 & sim_times <= sim_end]
    
    state <- c(GUT = 0, CENT = 0, PERIPH = 0)
    
    derivs <- function(state_vec, rate_in) {
      d <- c(GUT = 0, CENT = 0, PERIPH = 0)
      
      if (model_type %in% c("1cpt_ivbolus", "1cpt_ivinf")) {
        d["CENT"] <- rate_in - (params$CL / params$Vc) * state_vec["CENT"]
        
      } else if (model_type == "1cpt_oral") {
        d["GUT"]  <- -params$Ka * state_vec["GUT"]
        d["CENT"] <-  params$Ka * state_vec["GUT"] - (params$CL / params$Vc) * state_vec["CENT"]
        
      } else if (model_type %in% c("2cpt_ivbolus", "2cpt_ivinf")) {
        d["CENT"]   <- rate_in -
          (params$CL / params$Vc) * state_vec["CENT"] -
          params$Q * (state_vec["CENT"] / params$Vc) +
          params$Q * (state_vec["PERIPH"] / params$Vp)
        
        d["PERIPH"] <- params$Q * (state_vec["CENT"] / params$Vc - state_vec["PERIPH"] / params$Vp)
        
      } else if (model_type == "2cpt_oral") {
        d["GUT"]  <- -params$Ka * state_vec["GUT"]
        d["CENT"] <- params$Ka * state_vec["GUT"] -
          (params$CL / params$Vc) * state_vec["CENT"] -
          params$Q * (state_vec["CENT"] / params$Vc) +
          params$Q * (state_vec["PERIPH"] / params$Vp)
        
        d["PERIPH"] <- params$Q * (state_vec["CENT"] / params$Vc - state_vec["PERIPH"] / params$Vp)
      }
      
      d
    }
    
    apply_bolus_events <- function(state_vec, t_now) {
      bolus_now <- events_df %>%
        filter(Route %in% c("IV bolus", "Oral"), abs(Tid_h - t_now) < tol)
      
      if (nrow(bolus_now) == 0) {
        return(state_vec)
      }
      
      if (any(bolus_now$Target == "GUT")) {
        state_vec["GUT"] <- state_vec["GUT"] + sum(bolus_now$Event_dose[bolus_now$Target == "GUT"])
      }
      if (any(bolus_now$Target == "CENT")) {
        state_vec["CENT"] <- state_vec["CENT"] + sum(bolus_now$Event_dose[bolus_now$Target == "CENT"])
      }
      
      state_vec
    }
    
    infusion_rate_at_time <- function(t_now) {
      inf_now <- events_df %>%
        filter(
          Route == "IV infusion",
          Tid_h <= t_now + tol,
          Tid_h + input$infTime > t_now + tol
        )
      
      if (nrow(inf_now) == 0) 0 else sum(inf_now$Rate)
    }
    
    out_list <- vector("list", length(sim_times))
    
    for (i in seq_along(sim_times)) {
      t_now <- sim_times[i]
      
      state <- apply_bolus_events(state, t_now)
      
      out_list[[i]] <- data.frame(
        time = t_now,
        CP_total = state["CENT"] / params$Vc,
        stringsAsFactors = FALSE
      )
      
      if (i == length(sim_times)) next
      
      t_next <- sim_times[i + 1]
      dt <- t_next - t_now
      rate_in <- infusion_rate_at_time(t_now)
      
      k1 <- derivs(state, rate_in)
      k2 <- derivs(state + 0.5 * dt * k1, rate_in)
      k3 <- derivs(state + 0.5 * dt * k2, rate_in)
      k4 <- derivs(state + dt * k3, rate_in)
      
      state <- state + (dt / 6) * (k1 + 2 * k2 + 2 * k3 + k4)
      state <- pmax(state, 0)
    }
    
    bind_rows(out_list) %>%
      distinct(time, .keep_all = TRUE)
  }
  
  observeEvent(input$simulate, {
    sim_end <- input$sim_days * 24
    delta_sim <- 0.1
    params <- get_model_params()
    
    regimen_inputs <- list(
      list(name = "Regime 1", amt = input$amt1, time = input$time1, days = input$days1)
    )
    
    if (isTRUE(input$enableRegimen2)) {
      regimen_inputs <- append(
        regimen_inputs,
        list(list(name = "Regime 2", amt = input$amt2, time = input$time2, days = input$days2))
      )
    }
    
    sim_list <- list()
    event_list <- list()
    
    for (reg in regimen_inputs) {
      events_df <- tryCatch(
        {
          generate_dosing_events(
            amounts = reg$amt,
            times = reg$time,
            days = reg$days,
            model_type = input$model_type,
            regimen_name = reg$name
          )
        },
        error = function(e) {
          showNotification(paste("Fejl i", reg$name, ":", e$message), type = "error", duration = 8)
          NULL
        }
      )
      
      if (is.null(events_df)) {
        sim_result(NULL)
        return()
      }
      
      sim_df <- tryCatch(
        {
          simulate_regimen(
            events_df = events_df,
            model_type = input$model_type,
            params = params,
            sim_end = sim_end,
            delta = delta_sim
          ) %>%
            mutate(Regime = reg$name) %>%
            select(time, Regime, CP_total)
        },
        error = function(e) {
          showNotification(paste("Fejl under simulation af", reg$name, ":", e$message), type = "error", duration = 8)
          NULL
        }
      )
      
      if (is.null(sim_df)) {
        sim_result(NULL)
        return()
      }
      
      sim_list[[reg$name]] <- sim_df
      event_list[[reg$name]] <- events_df
    }
    
    sim_raw <- bind_rows(sim_list)
    events_df <- bind_rows(event_list)
    
    sim_snapshot <- list(
      model_label = unname(model_labels[input$model_type]),
      sim_engine = "Base R RK4",
      model_type = input$model_type,
      sim_days = input$sim_days,
      sim_end = sim_end,
      delta = delta_sim,
      unit = input$unit,
      regimen1_amt = input$amt1,
      regimen1_time = input$time1,
      regimen1_days = input$days1,
      regimen2_enabled = input$enableRegimen2,
      regimen2_amt = if (isTRUE(input$enableRegimen2)) input$amt2 else "",
      regimen2_time = if (isTRUE(input$enableRegimen2)) input$time2 else "",
      regimen2_days = if (isTRUE(input$enableRegimen2)) input$days2 else ""
    )
    
    if (input$model_type %in% c("1cpt_ivbolus", "1cpt_ivinf", "1cpt_oral")) {
      sim_snapshot$CL <- input$CL_1cpt
      sim_snapshot$V  <- input$V_1cpt
    } else {
      sim_snapshot$CL <- input$CL_2cpt
      sim_snapshot$V  <- input$V_2cpt
      sim_snapshot$Q  <- input$Q_2cpt
      sim_snapshot$V2 <- input$V2_2cpt
    }
    
    if (input$model_type %in% c("1cpt_oral", "2cpt_oral")) {
      sim_snapshot$Ka   <- input$Ka_oral
      sim_snapshot$F    <- input$F_oral
      sim_snapshot$Tlag <- input$Tlag_oral
    }
    
    if (input$model_type %in% c("1cpt_ivinf", "2cpt_ivinf")) {
      sim_snapshot$Infusion_duration_h <- input$infTime
    }
    
    sim_result(list(
      sim_raw = sim_raw,
      events = events_df,
      sim_snapshot = sim_snapshot
    ))
    
    showNotification("Simulation opdateret.", type = "message", duration = 3)
  })
  
  simulated_data <- reactive({
    res <- sim_result()
    shiny::req(res)
    
    fu <- 1 - input$protein_binding / 100
    
    res$sim_raw %>%
      mutate(
        fu = fu,
        CP_free = CP_total * fu
      )
  })
  
  analysis_window_bounds <- reactive({
    res <- sim_result()
    shiny::req(res)
    
    sim_end <- res$sim_snapshot$sim_end
    
    if (input$analysis_window == "all") {
      return(c(0, sim_end))
    }
    
    if (input$analysis_window == "first24") {
      return(c(0, min(24, sim_end)))
    }
    
    if (input$analysis_window == "last24") {
      return(c(max(0, sim_end - 24), sim_end))
    }
    
    start <- input$analysis_start
    end   <- input$analysis_end
    
    if (is.null(start) || is.null(end) || is.na(start) || is.na(end)) {
      return(NULL)
    }
    if (start < 0 || end <= start || end > sim_end) {
      return(NULL)
    }
    
    c(start, end)
  })
  
  calc_auc <- function(time, conc) {
    ok <- is.finite(time) & is.finite(conc)
    time <- time[ok]
    conc <- conc[ok]
    
    if (length(time) < 2) {
      return(NA_real_)
    }
    
    ord <- order(time)
    time <- time[ord]
    conc <- conc[ord]
    
    sum(diff(time) * (head(conc, -1) + tail(conc, -1)) / 2)
  }
  
  calc_time_above_threshold <- function(time, conc, threshold) {
    ok <- is.finite(time) & is.finite(conc)
    time <- time[ok]
    conc <- conc[ok]
    
    if (length(time) < 2 || !is.finite(threshold)) {
      return(NA_real_)
    }
    
    ord <- order(time)
    time <- time[ord]
    conc <- conc[ord]
    
    total <- 0
    
    for (i in seq_len(length(time) - 1)) {
      t1 <- time[i]
      t2 <- time[i + 1]
      c1 <- conc[i]
      c2 <- conc[i + 1]
      
      if (!is.finite(t1) || !is.finite(t2) || !is.finite(c1) || !is.finite(c2) || t2 <= t1) {
        next
      }
      
      above1 <- c1 >= threshold
      above2 <- c2 >= threshold
      
      if (above1 && above2) {
        total <- total + (t2 - t1)
      } else if (!above1 && !above2) {
        next
      } else if (c2 != c1) {
        t_cross <- t1 + (threshold - c1) * (t2 - t1) / (c2 - c1)
        t_cross <- max(min(t_cross, t2), t1)
        
        if (above1 && !above2) {
          total <- total + (t_cross - t1)
        } else if (!above1 && above2) {
          total <- total + (t2 - t_cross)
        }
      }
    }
    
    total
  }
  
  analysis_summary <- reactive({
    df <- simulated_data()
    bounds <- analysis_window_bounds()
    
    shiny::req(!is.null(bounds))
    
    start <- bounds[1]
    end   <- bounds[2]
    
    df_window <- df %>%
      filter(time >= start, time <= end)
    
    if (nrow(df_window) < 2) {
      return(data.frame())
    }
    
    basis_col <- if (isTRUE(input$enableMIC)) {
      if (input$mic_basis == "free") "CP_free" else "CP_total"
    } else {
      NULL
    }
    
    rows <- lapply(split(df_window, df_window$Regime), function(dfr) {
      out <- data.frame(
        Regime = unique(dfr$Regime),
        Fra_t = start,
        Til_t = end,
        Cmax_total = max(dfr$CP_total, na.rm = TRUE),
        Cmin_total = min(dfr$CP_total, na.rm = TRUE),
        AUC_total = calc_auc(dfr$time, dfr$CP_total),
        Cmax_fri = max(dfr$CP_free, na.rm = TRUE),
        Cmin_fri = min(dfr$CP_free, na.rm = TRUE),
        AUC_fri = calc_auc(dfr$time, dfr$CP_free),
        Proteinbinding_pct = input$protein_binding,
        stringsAsFactors = FALSE
      )
      
      if (isTRUE(input$enableMIC)) {
        time_above <- calc_time_above_threshold(dfr$time, dfr[[basis_col]], input$MIC)
        interval_len <- end - start
        
        out$MIC <- input$MIC
        out$MIC_basis <- if (input$mic_basis == "free") "Fri" else "Total"
        out$Tid_over_MIC_t <- time_above
        out$Pct_tid_over_MIC <- if (is.finite(interval_len) && interval_len > 0) {
          100 * time_above / interval_len
        } else {
          NA_real_
        }
      } else {
        out$MIC <- NA_real_
        out$MIC_basis <- ""
        out$Tid_over_MIC_t <- NA_real_
        out$Pct_tid_over_MIC <- NA_real_
      }
      
      out
    })
    
    bind_rows(rows)
  })
  
  output$analysisTable <- renderTable({
    shiny::req(sim_result())
    
    bounds <- analysis_window_bounds()
    validate(
      need(!is.null(bounds), "Ugyldigt analysevindue. Kontrollér start- og sluttid.")
    )
    
    df <- analysis_summary()
    
    validate(
      need(nrow(df) > 0, "Der er ikke nok datapunkter i det valgte analysevindue.")
    )
    
    df %>%
      mutate(across(where(is.numeric), ~ round(.x, 4)))
  }, rownames = FALSE)
  
  input_metadata <- reactive({
    res <- sim_result()
    shiny::req(res)
    
    snapshot <- res$sim_snapshot
    
    sim_rows <- data.frame(
      Parameter = c(
        "Model",
        "Simulationsvarighed_dage",
        "Simulationsslut_timer",
        "Delta_timer",
        "Enhed_ved_simulation"
      ),
      Value = c(
        snapshot$model_label,
        snapshot$sim_days,
        snapshot$sim_end,
        snapshot$delta,
        snapshot$unit
      ),
      stringsAsFactors = FALSE
    )
    
    optional_rows <- data.frame(
      Parameter = names(snapshot),
      Value = unlist(snapshot, use.names = FALSE),
      stringsAsFactors = FALSE
    ) %>%
      filter(!Parameter %in% c("model_label", "sim_days", "sim_end", "delta", "unit"))
    
    analysis_rows <- data.frame(
      Parameter = c(
        "Aktuel_enhed",
        "Proteinbinding_pct",
        "Visning_i_plot",
        "MIC_aktiv",
        "MIC",
        "MIC_basis",
        "Analysevindue",
        "Analyse_fra_t",
        "Analyse_til_t"
      ),
      Value = c(
        input$unit,
        input$protein_binding,
        input$conc_display,
        input$enableMIC,
        if (isTRUE(input$enableMIC)) input$MIC else NA,
        if (isTRUE(input$enableMIC)) input$mic_basis else NA,
        input$analysis_window,
        if (!is.null(analysis_window_bounds())) analysis_window_bounds()[1] else NA,
        if (!is.null(analysis_window_bounds())) analysis_window_bounds()[2] else NA
      ),
      stringsAsFactors = FALSE
    )
    
    bind_rows(sim_rows, optional_rows, analysis_rows)
  })
  
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste0("farmakokinetik_baggrundsdata_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      res <- sim_result()
      shiny::req(res)
      
      sim_df <- simulated_data() %>%
        mutate(across(where(is.numeric), ~ round(.x, 6)))
      
      events_df <- res$events %>%
        mutate(across(where(is.numeric), ~ round(.x, 6)))
      
      summary_df <- analysis_summary() %>%
        mutate(across(where(is.numeric), ~ round(.x, 6)))
      
      meta_df <- input_metadata()
      
      wb <- createWorkbook()
      
      addWorksheet(wb, "Inputs")
      writeData(wb, "Inputs", meta_df)
      
      addWorksheet(wb, "Doseringsregimer")
      writeData(wb, "Doseringsregimer", events_df)
      
      addWorksheet(wb, "Simulation")
      writeData(wb, "Simulation", sim_df)
      
      addWorksheet(wb, "Analyse")
      writeData(wb, "Analyse", summary_df)
      
      for (sheet in c("Inputs", "Doseringsregimer", "Simulation", "Analyse")) {
        setColWidths(wb, sheet = sheet, cols = 1:50, widths = "auto")
      }
      
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  plot_data <- reactive({
    df <- simulated_data()
    shiny::req(df)
    
    if (input$conc_display == "both") {
      bind_rows(
        df %>%
          transmute(time, Regime, Koncentrationstype = "Total", Conc = CP_total),
        df %>%
          transmute(time, Regime, Koncentrationstype = "Fri", Conc = CP_free)
      )
    } else if (input$conc_display == "free") {
      df %>%
        transmute(time, Regime, Koncentrationstype = "Fri", Conc = CP_free)
    } else {
      df %>%
        transmute(time, Regime, Koncentrationstype = "Total", Conc = CP_total)
    }
  })
  
  output$pkPlot <- renderPlot({
    df <- plot_data()
    res <- sim_result()
    
    shiny::req(df, res)
    
    sim_end <- res$sim_snapshot$sim_end
    
    p <- ggplot(df, aes(x = time, y = Conc, color = Regime))
    
    if (dplyr::n_distinct(df$Koncentrationstype) > 1) {
      p <- p + geom_line(aes(linetype = Koncentrationstype), linewidth = 1.1)
    } else {
      p <- p + geom_line(linewidth = 1.1)
    }
    
    p <- p +
      labs(
        title = "Plasmakoncentration over tid",
        x = "Tid (timer)",
        y = paste0("Koncentration i ", input$unit, "/L"),
        linetype = "Koncentrationstype"
      ) +
      theme_minimal(base_size = 14) +
      scale_color_manual(values = c("Regime 1" = "blue", "Regime 2" = "red")) +
      scale_x_continuous(breaks = seq(0, sim_end, by = 24), limits = c(0, sim_end))
    
    show_mic_line <- isTRUE(input$enableMIC) &&
      (
        input$conc_display == input$mic_basis ||
          input$conc_display == "both"
      )
    
    if (show_mic_line) {
      mic_label <- if (input$mic_basis == "free") "MIC (fri)" else "MIC (total)"
      
      p <- p +
        geom_hline(
          yintercept = input$MIC,
          linetype = "dashed",
          color = "black",
          linewidth = 0.9
        ) +
        annotate(
          "text",
          x = sim_end * 0.05,
          y = input$MIC * 1.02,
          label = paste0(mic_label, ": ", input$MIC, " ", input$unit, "/L"),
          hjust = 0,
          vjust = 0,
          size = 4,
          color = "black"
        )
    }
    
    p
  })
  
  output$hover_info <- renderText({
    df <- plot_data()
    hover <- input$plot_hover
    
    if (is.null(df) || is.null(hover)) {
      return("Før musen hen over grafen for at se tids- og koncentrationsværdier.")
    }
    
    point <- nearPoints(
      df, hover,
      xvar = "time",
      yvar = "Conc",
      maxpoints = 1,
      threshold = 10
    )
    
    if (nrow(point) == 0) {
      return("Ingen data tæt på hover-punktet.")
    }
    
    paste0(
      "Regime: ", point$Regime, "\n",
      "Koncentrationstype: ", point$Koncentrationstype, "\n",
      "Tid: ", round(point$time, 2), " timer\n",
      "Koncentration: ", round(point$Conc, 4), " ", input$unit, "/L"
    )
  })
  
  # ------------------------------------------------------------
  # Variabel-udregner (mere dynamisk og respekt for brugerinput)
  # ------------------------------------------------------------
  to_num_or_na <- function(x) {
    x <- trimws(x %||% "")
    if (identical(x, "") || is.na(x)) return(NA_real_)
    x <- gsub(",", ".", x, fixed = FALSE)
    suppressWarnings(as.numeric(x))
  }
  
  fmt <- function(x) {
    if (is.na(x) || is.nan(x) || is.infinite(x)) return("")
    format(round(x, 6), scientific = FALSE, trim = TRUE)
  }
  
  tmax_from_ka_ke <- function(Ka, Ke) {
    if (is.na(Ka) || is.na(Ke) || Ka <= Ke || Ka <= 0 || Ke <= 0) return(NA_real_)
    log(Ka / Ke) / (Ka - Ke)
  }
  
  solve_ka_from_tmax_ke <- function(Tmax, Ke) {
    if (is.na(Tmax) || is.na(Ke) || Tmax <= 0 || Ke <= 0) return(NA_real_)
    f <- function(Ka) tmax_from_ka_ke(Ka, Ke) - Tmax
    lower <- Ke * 1.0001
    upper <- max(Ke * 1000, 1)
    if (is.na(f(lower)) || is.na(f(upper)) || f(lower) * f(upper) > 0) return(NA_real_)
    uniroot(f, lower = lower, upper = upper)$root
  }
  
  solve_ke_from_tmax_ka <- function(Tmax, Ka) {
    if (is.na(Tmax) || is.na(Ka) || Tmax <= 0 || Ka <= 0) return(NA_real_)
    f <- function(Ke) tmax_from_ka_ke(Ka, Ke) - Tmax
    lower <- max(Ka / 1000, 1e-6)
    upper <- Ka / 1.0001
    if (is.na(f(lower)) || is.na(f(upper)) || f(lower) * f(upper) > 0) return(NA_real_)
    uniroot(f, lower = lower, upper = upper)$root
  }
  
  pk_fields <- c("V", "CL", "Ke", "t12", "Tmax", "Ka")
  pk_input_ids <- c(
    V = "pk_V",
    CL = "pk_CL",
    Ke = "pk_Ke",
    t12 = "pk_t12",
    Tmax = "pk_Tmax",
    Ka = "pk_Ka"
  )
  
  pk_state <- reactiveValues(
    locked = as.list(setNames(rep(FALSE, length(pk_fields)), pk_fields)),
    user_values = as.list(setNames(rep(NA_real_, length(pk_fields)), pk_fields)),
    status = "Tip: Felter med indhold betragtes som bruger-input. Slet et felt for at lade appen beregne det."
  )
  
  pk_updating <- reactiveVal(FALSE)
  
  compute_pk_from_locked <- function(user_values, locked) {
    vals <- user_values
    
    for (nm in names(vals)) {
      if (!isTRUE(locked[[nm]])) {
        vals[[nm]] <- NA_real_
      }
    }
    
    for (i in 1:10) {
      if (is.na(vals$Ke) && !is.na(vals$CL) && !is.na(vals$V) && vals$V > 0) {
        vals$Ke <- vals$CL / vals$V
      }
      if (is.na(vals$CL) && !is.na(vals$Ke) && !is.na(vals$V)) {
        vals$CL <- vals$Ke * vals$V
      }
      if (is.na(vals$V) && !is.na(vals$CL) && !is.na(vals$Ke) && vals$Ke > 0) {
        vals$V <- vals$CL / vals$Ke
      }
      
      if (is.na(vals$t12) && !is.na(vals$Ke) && vals$Ke > 0) {
        vals$t12 <- log(2) / vals$Ke
      }
      if (is.na(vals$Ke) && !is.na(vals$t12) && vals$t12 > 0) {
        vals$Ke <- log(2) / vals$t12
      }
      
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
  
  check_pk_consistency <- function(user_values, locked) {
    msgs <- c()
    tol <- 1e-4
    
    if (isTRUE(locked$V) && isTRUE(locked$CL) && isTRUE(locked$Ke) &&
        all(is.finite(c(user_values$V, user_values$CL, user_values$Ke))) &&
        user_values$V > 0) {
      expected_ke <- user_values$CL / user_values$V
      if (abs(expected_ke - user_values$Ke) > tol) {
        msgs <- c(msgs, "V, CL og Ke er ikke indbyrdes konsistente (Ke bør være CL/V).")
      }
    }
    
    if (isTRUE(locked$Ke) && isTRUE(locked$t12) &&
        all(is.finite(c(user_values$Ke, user_values$t12))) &&
        user_values$Ke > 0) {
      expected_t12 <- log(2) / user_values$Ke
      if (abs(expected_t12 - user_values$t12) > tol) {
        msgs <- c(msgs, "Ke og T½ er ikke indbyrdes konsistente (T½ bør være ln(2)/Ke).")
      }
    }
    
    if (isTRUE(locked$Ka) && isTRUE(locked$Ke) && isTRUE(locked$Tmax) &&
        all(is.finite(c(user_values$Ka, user_values$Ke, user_values$Tmax)))) {
      if (user_values$Ka <= user_values$Ke) {
        msgs <- c(msgs, "Tmax-formlen kræver Ka > Ke.")
      } else {
        expected_tmax <- tmax_from_ka_ke(user_values$Ka, user_values$Ke)
        if (is.finite(expected_tmax) && abs(expected_tmax - user_values$Tmax) > tol) {
          msgs <- c(msgs, "Ka, Ke og Tmax er ikke indbyrdes konsistente.")
        }
      }
    }
    
    msgs
  }
  
  update_pk_ui <- function(vals, locked) {
    pk_updating(TRUE)
    on.exit(pk_updating(FALSE), add = TRUE)
    
    for (nm in names(pk_input_ids)) {
      if (!isTRUE(locked[[nm]])) {
        updateTextInput(session, pk_input_ids[[nm]], value = fmt(vals[[nm]]))
      }
    }
  }
  
  handle_pk_field_change <- function(field_name, input_id) {
    observeEvent(input[[input_id]], {
      if (isTRUE(pk_updating())) return()
      
      raw_value <- trimws(input[[input_id]] %||% "")
      parsed_value <- to_num_or_na(raw_value)
      is_locked <- nzchar(raw_value)
      
      pk_state$locked[[field_name]] <- is_locked
      pk_state$user_values[[field_name]] <- if (is_locked) parsed_value else NA_real_
      
      invalid_msgs <- c()
      for (nm in names(pk_input_ids)) {
        raw_nm <- trimws(input[[pk_input_ids[[nm]]] ] %||% "")
        if (nzchar(raw_nm) && is.na(to_num_or_na(raw_nm))) {
          invalid_msgs <- c(invalid_msgs, paste0("Ugyldigt tal i feltet ", nm, "."))
        }
      }
      
      vals <- compute_pk_from_locked(pk_state$user_values, pk_state$locked)
      consistency_msgs <- check_pk_consistency(pk_state$user_values, pk_state$locked)
      
      status_msgs <- c(invalid_msgs, consistency_msgs)
      if (length(status_msgs) == 0) {
        pk_state$status <- "Tip: Felter med indhold betragtes som bruger-input. Slet et felt for at lade appen beregne det."
      } else {
        pk_state$status <- paste(status_msgs, collapse = "\n")
      }
      
      update_pk_ui(vals, pk_state$locked)
    }, ignoreInit = TRUE)
  }
  
  for (nm in names(pk_input_ids)) {
    local({
      field_name <- nm
      input_id <- pk_input_ids[[nm]]
      handle_pk_field_change(field_name, input_id)
    })
  }
  
  output$pk_status <- renderText({
    pk_state$status
  })
  
  observeEvent(input$pk_reset, {
    pk_updating(TRUE)
    on.exit(pk_updating(FALSE), add = TRUE)
    
    for (nm in names(pk_input_ids)) {
      pk_state$locked[[nm]] <- FALSE
      pk_state$user_values[[nm]] <- NA_real_
      updateTextInput(session, pk_input_ids[[nm]], value = "")
    }
    
    pk_state$status <- "Tip: Felter med indhold betragtes som bruger-input. Slet et felt for at lade appen beregne det."
  })
}

shinyApp(ui, server)
