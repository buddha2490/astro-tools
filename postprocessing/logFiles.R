library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)
library(readxl)
library(ggplot2)
library(scales)
library(gridExtra)
library(grid)
library(patchwork)
library(cowplot)
library(ggplotify)
library(forcats)


debug <- FALSE


os <- Sys.info()["sysname"]
machine <- Sys.info()["nodename"]

os <- ifelse(os == "Darwin", "Mac", "Windows") %>% as.character()
machine <- ifelse(machine == "BRIANC-MacUS.attlocal.net", "MBP13",
                  ifelse(machine == "Brians-MBP.attlocal.net", "MBP14",
                         ifelse(machine == "ES127", "ES127", machine))) %>%
  as.character()



# Environmental parameters ------------------------------------------------

# Path to the log file on the mele-astro mini computer
# change for particular pc

if (os == "Windows" & debug == FALSE) {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  logPath <- "C:/Users/Brian Carter/AppData/Local/NINA/Logs"
  subsPath <- "C:/Users/Brian Carter/Astronomy/ASI2600MM/ES127"
  phd2Logs <- "C:/Users/Brian Carter/Documents/PHD2"
} 

if (os == "Windows" & debug == TRUE) {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  logPath <- "data"
  subsPath <- "D:/NAS/ASI2600MM/ES127"
  phd2Logs <- logPath
}

if (os == "Mac") {
  setwd("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  logPath <- "/Users/briancarter/Astronomy/astro-tools/data/sample logs"
  subsPath <- "/Users/briancarter/Astronomy/testing"
  phd2Logs <- logPath
}

source("functions/functions.R")


# Guide logs --------------------------------------------------------------

guideFiles <- data.frame(file = list.files(phd2Logs, pattern = ".txt", full.names = TRUE)) %>%
  mutate(mtime = file.mtime(file)) %>%
  arrange(desc(mtime)) %>%
  distinct(file, .keep_all = TRUE) %>%
  filter(stringr::str_detect(file, "GuideLog") == TRUE)

guideFiles %>%
  slice(1) %>%
  pull(file) %>%
  file.copy(subsPath)

if (os == "Windows" &  debug == FALSE) {
guideFiles %>%
  slice(-1) %>%
  pull(file) %>%
  sapply(file.remove)
}




# Process logs --------------------------------------------------------------------

# creates a clean delimited character vector of the logs
#logPath <- file.path("../data/sample logs/chris/20250726-210820-3.1.2.9001.11544-202507.log") %>% normalizePath()

logfile <- logPath %>% pullLogs(guest = FALSE)

logReport <- read_delim(
  paste(logfile, collapse = "\n"),
  delim = "|",
  col_names = c("DATE", "LEVEL", "SOURCE", "MEMBER", "LINE", "MESSAGE"),
  trim_ws = TRUE,
  col_types = cols(.default = "c")
)  %>%
  mutate(index = row_number()) %>%
  mutate(DATE = ymd_hms(DATE, tz = "UTC", quiet = TRUE)) %>%
  eventPairs() %>%
  cleanupLogs() %>%
  filter(!is.na(EVENT_ID)) %>%
  filter(!is.na(DATE)) %>%
  times()

allRoles <- logReport %>%
  distinct(ROLE) %>%
  pull(ROLE) %>%
  as.character()

logReshaped <- logReport %>%
  select(ROLE, EVENT_ID, TYPE, DATE) %>%
  pivot_wider(
    id_cols     = c(EVENT_ID, ROLE),
    names_from  = TYPE,
    values_from = DATE
  ) %>%
  filter(!is.na(start) & !is.na(end)) %>%
  bind_rows(addSubsToSequence())

logReport <- logReport %>%
  reportGen() %>%
  mutate_if(is.numeric, ~as.character(.)) %>%
  mutate_if(is.character, ~tidyr::replace_na(., "")) 

# Save reshaped version for later
#openxlsx::write.xlsx(logReshaped, glue::glue("{subsPath}/NINA Logs - Reshaped.xlsx"))

logAnalysis <- logReshaped %>% logChartDev()
chart <- logAnalysis$plot



# Report on the night's subs ----------------------------------------------

myPaths <- list.dirs(subsPath, full.names = TRUE, recursive = FALSE)


subsReport <- lapply(subsPath, processMetaData) %>%
  do.call("rbind", .) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~as.character(.)) %>%
  mutate_if(is.character, ~tidyr::replace_na(., ""))

cols <- c("Object", "Filter", "N",
          "Minutes",
          "HFR\n(mean)", "FWHM\n(mean)",
          "Guiding\n(mean)")
names(subsReport) <- cols



# Create smaller, compact table themes
compact_theme <- ttheme_default(
  base_size = 8,
  core = list(
    bg_params = list(fill = "white"),
    padding = unit(c(1, 1), "mm")
  ),
  colhead = list(bg_params = list(fill = "white")),
  rowhead = list(bg_params = list(fill = "white"))
)

rownames(logAnalysis$summary_table) <- NULL
table1_grob <- logAnalysis$summary_table %>% tableGrob(rows = NULL, theme = compact_theme)
table2_grob <- subsReport %>% tableGrob(rows = NULL, theme = compact_theme)

# Wrap each table in a ggplot and set the background to white
table1_plot <- as.ggplot(table1_grob) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

table2_plot <- as.ggplot(table2_grob) +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

# Combine tables side by side
tables_combined <- plot_grid(
  table1_plot, 
  table2_plot,
  nrow = 1,
  align = "h",
  rel_widths = c(0.75, 1.25)
)

# Final combined layout with Gantt plot on top
final_plot <- plot_grid(
  chart,
  tables_combined,
  ncol = 1,
  rel_heights = c(1.5, 1),
  align = "v"
)

# Export with white background
ggsave(
  glue::glue("{subsPath}/Imaging summary - {Sys.Date()}.png"),
  final_plot,
  width = 11,
  height = 8.5,
  units = "in",
  dpi = 300,
  bg = "white"  # critical for white output
)




