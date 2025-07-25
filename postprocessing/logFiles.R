library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)
library(huxtable)
library(pharmaRTF)


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
} 

if (os == "Windows" & debug == TRUE) {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  logPath <- "data"
  subsPath <- "D:/NAS/ASI2600MM/ES127"
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
logfile <- logPath %>% pullLogs()



logReport <- read_delim(
  paste(logfile, collapse = "\n"),
  delim = "|",
  col_names = c("DATE", "LEVEL", "SOURCE", "MEMBER", "LINE", "MESSAGE"),
  trim_ws = TRUE,
  col_types = cols(.default = "c")
)  %>%
  mutate(DATE = ymd_hms(DATE, tz = "UTC", quiet = TRUE)) %>%
  eventPairs() %>%
  cleanupLogs() %>%
  devFix(debug) %>%
  times()

logReshaped <- logReport %>%
  select(ROLE, EVENT_ID, TYPE, DATE) %>%
  pivot_wider(
    id_cols     = c(EVENT_ID, ROLE),
    names_from  = TYPE,
    values_from = DATE
  ) %>%
  filter(!is.na(start) & !is.na(end))

logReport <- logReport %>%
  reportGen() %>%
  mutate_if(is.numeric, ~as.character(.)) %>%
  mutate_if(is.character, ~tidyr::replace_na(., "")) 

# Save reshaped version for later
openxlsx::write.xlsx(logReshaped, glue::glue("{subsPath}/NINA Logs - Reshaped.xlsx"))



# Report on the night's subs ----------------------------------------------

# n = 169, 338 minutes
# This doesn't match with above report
# it seems that the log doesn't catch some of the "end times" for captures,
# so there are a bunch with ONLY START

myPaths <- list.dirs(subsPath, full.names = TRUE, recursive = FALSE)


subsReport <- lapply(subsPath, processMetaData) %>%
  do.call("rbind", .) %>%
  mutate_if(is.numeric, ~as.character(.)) %>%
  mutate_if(is.character, ~tidyr::replace_na(., ""))

cols <- c("Object", "Filter", "Total Subs",
          "Total Minutes",
          "HFR\n(mean)", "FWHM\n(mean)",
          "Guiding RMS\n(mean)")
names(subsReport) <- cols

# RTF Report --------------------------------------------------------------

report1 <- subsReport %>%
  huxtable() %>%
  set_all_padding(0.0) %>%
  huxtable::set_bold(1, 1:ncol(.)) %>%
  huxtable::set_top_padding(6) %>%
  huxtable::set_bottom_padding(6) %>%
  huxtable::set_top_border(1, 1:ncol(.), 1) %>%
  huxtable::set_bottom_border(1, 1:ncol(.), 1) %>%
  huxtable::set_width(1.5) %>%
  huxtable::set_font("arial") %>%
  huxtable::set_font_size(10) %>%
  huxtable::map_align(huxtable::by_cols(from = 2, "center"))

report2 <- logReport %>%
  mutate_if(is.character, ~ifelse(stringr::str_detect(., "NA"), "", .)) %>%
  huxtable() %>%
  set_all_padding(0.0) %>%
  huxtable::set_bold(1, 1:ncol(.)) %>%
  huxtable::set_top_padding(6) %>%
  huxtable::set_bottom_padding(6) %>%
  huxtable::set_top_border(1, 1:ncol(.), 1) %>%
  huxtable::set_bottom_border(1, 1:ncol(.), 1) %>%
  huxtable::set_width(1.5) %>%
  huxtable::set_font("arial") %>%
  huxtable::set_font_size(10) %>%
  huxtable::map_align(huxtable::by_cols(from = 2, "center"))




# Export the table --------------------------------------------------------
telescope <- subsPath %>% basename()
camera <- subsPath %>% dirname() %>% basename()
title1 <- glue::glue("Imaging report for {Sys.Date() - 1}")
title2 <- glue::glue("{telescope} --- {camera}")

# Create the RTF document with both tables
doc1 <- report1 %>%
  rtf_doc(header_rows = 1) %>%
  add_titles(hf_line(title1, bold = TRUE, font = "arial", font_size = 12)) %>%
  add_titles(hf_line(title2, bold = TRUE, font = "arial", font_size = 12)) %>%
  add_titles(hf_line(""))

pagesize(doc1) <- c(height = 8.5, width = 11)
margins(doc1) <- c(top = 0.25, bottom = 0.25, left = 0.25, right = 0.25)



doc2 <- report2 %>%
  rtf_doc(header_rows = 1) %>%
  add_titles(hf_line(title1, bold = TRUE, font = "arial", font_size = 12)) %>%
  add_titles(hf_line(title2, bold = TRUE, font = "arial", font_size = 12)) %>%
  add_titles(hf_line(""))

pagesize(doc2) <- c(height = 8.5, width = 11)
margins(doc2) <- c(top = 0.25, bottom = 0.25, left = 0.25, right = 0.25)

write_rtf(doc1, file = glue::glue("{subsPath}/NINA Subs report - {Sys.Date()-1}.rtf"))
write_rtf(doc2, file = glue::glue("{subsPath}/NINA Logs report - {Sys.Date()-1}.rtf"))

# cleanLogFolder(logPath)





