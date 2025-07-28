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
library(forcats)

# source your helpers
source("functions/functions.R")

ui <- dashboardPage(
  dashboardHeader(title = "NINALogAnalyzer"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Controls", icon = icon("sliders-h")),
      fileInput("logfileInput", "Upload .log File", accept = c(".log","text/plain")),
      actionButton("submit", "Submit", icon = icon("paper-plane")),
      uiOutput("sliderUI"),
      uiOutput("sliderHelp"),
      hr(),
      tags$div(
        style = "padding-left: 15px;",
        h5(" E-mail the developer"),
        helpText("Developer: Brian Carter"),
        helpText(a("bcarter6@me.com", href = "mailto:bcarter6@me.com"))
      )
    )
  ),
  dashboardBody(
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
  )
)

server <- function(input, output, session) {
  
  # 1) Parse raw data on Submit
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
      pivot_wider(
        id_cols     = c(EVENT_ID, ROLE),
        names_from  = TYPE,
        values_from = DATE
      ) %>%
      filter(!is.na(start) & !is.na(end))
    
    allRoles <<- df_wide %>% distinct(ROLE) %>% pull(ROLE) %>% as.character()
    df_wide
  })
  
  # 2) Render time-range slider after data loaded
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
      timeFormat = "%H:%M:%S",
      width      = "100%"
    )
  })
  
  output$sliderHelp <- renderUI({
    df <- rawParsed()
    req(df)
    helpText("You can use the slider to adjust the time range for this analysis.")
  })
  
  # 3) Perform analysis filtered by slider
  analysis <- reactive({
    df <- rawParsed()
    req(df, input$timeRange)
    
    df_f <- df %>%
      filter(start >= input$timeRange[1],
             end   <= input$timeRange[2])
    
    logChartDev(df_f)
  })
  
  # 4) Show title, plot and table after analysis
  output$resultsUI <- renderUI({
    req(analysis())
    tagList(
      fluidRow(
      ),
      fluidRow(
        plotlyOutput("logPlot", width = "100%", height = "600px")
      ),
      hr(),
      fluidRow(
        column(
          width = 6, offset = 3,
          DTOutput("logTable")
        )
      )
    )
  })
  
  # 5) Render plotly without legend
  output$logPlot <- renderPlotly({
    req(analysis()$plot)
    # Turn your ggplot into a plotly object
    ggplotly(analysis()$plot, tooltip = "x") %>%
      layout(
        showlegend = FALSE,
        margin     = list(b = 50, t = 50)
      )
  })
  
  # 6) Render full table (no paging)
  output$logTable <- renderDT({
    req(analysis()$summary_table)
    datatable(
      analysis()$summary_table,
      options = list(paging = FALSE, dom = "t")
    )
  })
}

shinyApp(ui, server)

