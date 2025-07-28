# app.R

library(shiny)
library(shinydashboard)
library(plotly)
library(DT)
library(glue)
library(stringr)
library(dplyr)
library(readr)
library(lubridate)
library(scales)
library(tidyr)
library(ggplot2)

# Source your helper functions
source("functions/functions.R")

ui <- dashboardPage(
  skin = "blue",  # shinydashboard built‑in skin
  
  # ---- HEADER ----
  dashboardHeader(
    title = tags$div(
      style = "font-family: 'Roboto', sans-serif; font-weight: 500;",
      icon("satellite-dish"), " NINA Log Analyzer"
    )
  ),
  
  # ---- SIDEBAR ----
  dashboardSidebar(
    tags$head(
      # load a crisp Google font and add some CSS overrides
      tags$link(
        href = "https://fonts.googleapis.com/css?family=Roboto:400,500,700",
        rel  = "stylesheet"
      ),
      tags$style(HTML("
        /* Sidebar styling */
        .skin-blue .main-sidebar { background: #2C3E50; }
        .skin-blue .sidebar-menu .menu-item > a { color: #ECF0F1; }
        .skin-blue .sidebar-menu .menu-item > a:hover { background: #1ABC9C; }
        /* Rounded inputs & buttons */
        .form-control, .btn { border-radius: 8px !important; }
      "))
    ),
    sidebarMenu(
      menuItem("Controls", icon = icon("sliders-h")),
      fileInput(
        "logfileInput", "Upload .log File",
        accept = c(".log","text/plain")
      ),
      actionButton(
        "submit", "Submit",
        icon = icon("paper-plane"),
        class = "btn-primary btn-sm",
        style = "width:120px; padding: 4px 8px; font-size: 12px;"
      ),
      uiOutput("sliderUI"),
      hr(),
      tags$div(
        style = "padding-left: 15px;",
        helpText("Developer: Brian Carter"),
        helpText(a("bcarter6@me.com", href = "mailto:bcarter6@me.com"))
      )
    )
  ),
  
  # ---- BODY ----
  dashboardBody(
    # Custom content background
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background: #ECF0F1; }
      .box { border-radius: 12px; box-shadow: 0 2px 6px rgba(0,0,0,0.1); }
    "))),
      fluidRow(
        box(title = "NINA Log Parser", solidHeader = TRUE, width = 6,
            helpText(
              "Welcome to the **N.I.N.A. Log Parser tool.\n N.I.N.A ('Nighttime Imaging 'N' Astronomy')
            is fantastic for automating our astrophotography, but sometimes we push play on a sequence to run over 
            a 10-hour session only to wake up and find 4-hours worth of subs.  What happened?  You can go to the logs,
            and this tool will parse them and help you identify areas of optimization.\n"
            )),
        box(title = "Getting Started", solidHeader = TRUE, width = 6,
            helpText(
              "1. Locate your NINA logs and upload them into the left sidebar", br(),
              "2. Click Submit to run the analysis", br(),
              "3. Outputs will apear to the right once processing is complete", br(),
              "4. Adjust the time range slider to focus on a particular period")
        )),
      uiOutput("resultsUI")
  ))


server <- function(input, output, session) {
  
  # --- Step 1: Parse & pivot on Submit ---
  rawParsed <- eventReactive(input$submit, {
    req(input$logfileInput)
    dir.create("www", showWarnings = FALSE)
    dest <- file.path("www","latest.log")
    file.copy(input$logfileInput$datapath, dest, overwrite = TRUE)
    
    raw_lines <- pullLogs(path = "www")
    
    dat <- read_delim(
      paste(raw_lines, collapse = "\n"),
      delim     = "|",
      col_names = c("DATE","LEVEL","SOURCE","MEMBER","LINE","MESSAGE"),
      trim_ws   = TRUE,
      col_types = cols(.default = "c")
    ) %>%
      mutate(
        index = row_number(),
        DATE  = ymd_hms(DATE, tz = "UTC", quiet = TRUE)
      ) %>%
      eventPairs() %>%
      cleanupLogs() %>%
      times() %>%
      filter(!is.na(TIME))
    
    df_wide <- dat %>%
      select(ROLE, EVENT_ID, TYPE, DATE) %>%
      mutate(DATE = as.POSIXct(DATE, tz = "America/New_York")) %>%
      ungroup() %>%
      pivot_wider(
        id_cols     = c(EVENT_ID, ROLE),
        names_from  = TYPE,
        values_from = DATE,
        values_fn = mean
      ) %>%
      filter(!is.na(start) & !is.na(end)) 
    
    allRoles <<- df_wide %>% distinct(ROLE) %>% pull(ROLE) %>% as.character()
    df_wide
  })
  # --- Step 2: Time‐range slider ---
  output$sliderUI <- renderUI({
    df <- rawParsed()
    req(df)
    rng_min <- min(df$start)
    rng_max <- max(df$end)
    sliderInput(
      "timeRange", "Time Range:",
      min        = rng_min,
      max        = rng_max,
      value      = c(rng_min, rng_max),
      timeFormat = "%H:%M",
      width      = "100%"
    )
  })
  
  # --- Step 3: Analysis reactive ---
  analysis <- reactive({
    df <- rawParsed()
    req(df, input$timeRange)
    df_f <- df %>%
      filter(start >= input$timeRange[1],
             end   <= input$timeRange[2])
    logChartDev(df_f)
  })
  
  # --- Step 4: Render results UI ---
  output$resultsUI <- renderUI({
    req(analysis())
    tagList(
      fluidRow(
        box(
          title = "NINA Imaging Analysis",
          status = "info", solidHeader = TRUE, width = 12,
          plotlyOutput("logPlot", height = "500px")
        )
      ),
      fluidRow(
        box(
          title = "Summary Table",
          status = "primary", solidHeader = TRUE, width = 12,
          DTOutput("logTable")
        )
      )
    )
  })
  
  # --- Step 5: Interactive Plotly Gantt ---
  output$logPlot <- renderPlotly({
    req(analysis()$plot)
    ggplotly(analysis()$plot, tooltip = "text") %>%
      layout(
        showlegend = FALSE,
        margin     = list(l = 80, r = 20, t = 50, b = 50)
      )
  })
  
  # --- Step 6: Full‐table output ---
  output$logTable <- renderDT({
    req(analysis()$summary_table)
    datatable(
      analysis()$summary_table,
      options = list(paging = FALSE, dom = "t"),
      class = "stripe hover"
    )
  })
}

shinyApp(ui, server)
