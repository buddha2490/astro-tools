#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(bslib)
library(DT)
library(magrittr)
library(dplyr)
library(DBI)
library(RPostgreSQL)



ui <- page_navbar(
  title = "Imaging Data",
  theme = bs_theme(version = 5),
  nav_panel(
    title = "ImagingLog",
    card(
      full_screen = TRUE,
      card_header("Imaging Log"),
      DTOutput("imaging_log_table")
    )
  ),
  nav_panel(
    title = "ImagingSummary",
    card(
      full_screen = TRUE,
      card_header("Imaging Summary"),
      DTOutput("imaging_summary_table")
    )
  )
)

server <- function(input, output, session) {
  
  myDB <- dbConnect(dbDriver("PostgreSQL"),
                    dbname = "astro",
                    host = "100.97.181.35",
                    port = 5432,
                    user = "shiny",
                    password = "shiny")
  
  imaging_log_data <- dbReadTable(myDB, "ImagingLog") %>%
    as.data.frame()
  
  imaging_summary_data <- dbReadTable(myDB, "ImagingLogSummary") %>%
    as.data.frame()
  
  output$imaging_log_table <- renderDT({
    datatable(imaging_log_data, options = list(pageLength = 50))
  })
  
  output$imaging_summary_table <- renderDT({
    datatable(imaging_summary_data, options = list(pageLength = 50))
  })
}

shinyApp(ui, server)
