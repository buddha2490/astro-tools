library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)
library(huxtable)
library(pharmaRTF)




# Edit sections -----------------------------------------------------------
# This section may be editted to make the program portable
# otherwise it should work


# change this for production
debug <- TRUE

# Path to the log file on the mele-astro mini computer
# change for particular pc
logPath <- "C:/Users/bcart/AppData/Local/NINA/Logs"

# Path to my subs
subsPath <- "C:/Users/bcart/Astronomy/ASI2600MM/ES127"


# Functions ---------------------------------------------------------------
eventPairs <- function(df) {
  
  starts <- df %>%
    filter(str_detect(MESSAGE, "^Starting Category:")) %>%
    mutate(
      ROLE = str_match(MESSAGE, "Item: ([^,]+)")[,2],
      EVENT_ID = row_number(),  # temporary unique ID
      TYPE = "start"
    )
  
  finishes <- df %>%
    filter(str_detect(MESSAGE, "^Finishing Category:")) %>%
    mutate(
      ROLE = str_match(MESSAGE, "Item: ([^,]+)")[,2],
      TYPE = "end"
    )
  
  bind_rows(starts, finishes) %>%
    arrange(DATE) %>%
    filter(!is.na(ROLE)) %>%
    mutate(EVENT_ID = ifelse(is.na(EVENT_ID), lag(EVENT_ID), EVENT_ID)) %>%
    select(-LEVEL, -SOURCE, - MEMBER, -LINE)
  
}

pullLogs <- function(path, debug = debug) {
  
  # Get a list of log files- ideally I want to take the most recent one
  # Maybe create an archive for the the log files after I run this script?
  allFiles <- data.frame(files = list.files(logPath, pattern = ".log", full.names = TRUE)) %>%
    mutate(mtime = file.mtime(files)) %>%
    filter(stringr::str_detect(files, "robocopy") == FALSE)
  
  # This is the most recent log file, assuming it is the correct one
  logFilePath <- allFiles %>%
    arrange(desc(mtime)) %>%
    slice(1) %>%
    pull(files) %>%
    as.character()
  
  allFiles <- allFiles %>%
    filter(!files == logFilePath) %>%
    filter(stringr::str_detect(files, "log.log") == FALSE)
  
  archive <- glue::glue("{logPath}/archive") 
  archive %>% dir.create(showWarnings = FALSE)
  lapply(allFiles$files, file.copy, to = archive)
  lapply(allFiles$files, file.remove) # removes the old stuff into archive, if any
  
  
  # cached log file for testing
  if (debug == TRUE) {
    logFilePath <- glue::glue("{path}/log.log")
  }
  
  # Read file and keep only the time entries
  logfile <- read_lines(logFilePath)
  logfile <- logfile[str_detect(logfile, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+")]
  
  # clean up
  if (debug != TRUE) {
    logfile %>% file.copy(to = archive)
    logfile %>% file.remove()
  }
  return(logfile)
}

devFix <- function(dat, debug = debug) {
  
  # The dev logs I have need to be editted a bit to match
  # the standardized versions from my current sequence
  # I want to cut out prep time or time that the application is idle.
  # the start and stop of the actual sequence I want to review
  # are flagged with annotations
  
  if(debug == TRUE) {
    start <- dat[148:149,]  # "WaitforTIme" - will just replace
    
    
    foo <- dat %>%
      filter(ROLE == "Annotation" & 
               str_detect(MESSAGE, "This group here will be executed in parallel.") == TRUE) 
    
    foo <- foo %>%
      mutate(MESSAGE = stringr::str_replace(MESSAGE,
                                            "This group here will be executed in parallel.",
                                            "START SEQUENCE NOW")) 
    start$MESSAGE <- foo$MESSAGE
    start$ROLE <- "Annotation"
    start$DATE[1] <- start$DATE[2] # simulate them as equal
    dat[148:149,] <- start
    
    end <- dat[1710:1711,]
    
    foo <- foo %>%
      mutate(MESSAGE = stringr::str_replace(MESSAGE,
                                            "START SEQUENCE NOW",
                                            "END SEQUENCE NOW"))
    end$MESSAGE <- foo$MESSAGE
    end$ROLE <- "Annotation"
    dat[1710:1711,] <- end
    
  }
  return(dat)
}

times <- function(dat) {
  
  # There's a lot of trash in these logs.  I have an annotation for start/end the sequence.
  # I can use these to subset to only the active sequence section
  starttime <<- dat %>% 
    filter(ROLE == "Annotation" & 
             str_detect(MESSAGE, "START SEQUENCE NOW") == TRUE &
             TYPE == "start") %>% 
    pull(DATE)
  
  endtime <<- dat %>%
    filter(ROLE == "Annotation" & 
             str_detect(MESSAGE, "END SEQUENCE NOW") == TRUE &
             TYPE == "end") %>% 
    pull(DATE)
  
  dat %>%
    filter(DATE >= starttime & DATE <= endtime) %>%
    group_by(EVENT_ID) %>%
    mutate(TIME = ifelse(TYPE == "end", difftime(DATE, lag(DATE), units = "secs"), 0)) %>%
    ungroup()
  
}

reportGen <- function(dat) {
  
  report <- dat %>%
    group_by(ROLE) %>%
    summarise(
      TotalMinutes = sum(TIME, na.rm = TRUE) / 60,
      N_events = ceiling(n() / 2)
    ) %>%
    mutate(AverageTime = TotalMinutes / N_events)
  
  tots <- data.frame(
    ROLE = c("", "Total Event Time", "Total Sequence Time"),
    TotalMinutes = c(NA_real_, 
                     sum(report$TotalMinutes, na.rm = TRUE),
                     difftime(endtime, starttime, units = "mins")))
  
  report <- report %>%
    bind_rows(tots) %>%
    mutate(TotalMinutes = format(round(TotalMinutes, 2), nsmall = 2)) %>%
    mutate(AverageTime = format(round(AverageTime, 2), nsmall = 2))
  
  colnames(report) <- c("Role", "Total Minutes", "N Events", "Average Time (mins)")
  
  return(report)
}

getMetadata <- function(path) {
  
  metaDf <- path %>%
    list.files(pattern = "ImageMetaData.csv", 
               recursive = TRUE,
               full.names = TRUE) %>%
    readr::read_csv() %>%
    select(FilterName, Duration, HFR, FWHM, GuidingRMSArcSec) %>%
    group_by(FilterName) %>%
    summarize(Total_Subs = n(),
              Minutes = sum(Duration / 60),
              HFR = mean(HFR, na.rm = TRUE),
              FWHM = mean(FWHM, na.rm = TRUE),
              GuidingRMS = mean(GuidingRMSArcSec, na.rm = TRUE)) %>%
    mutate(FilterName = factor(FilterName,
                               c("L", "R", "G", "B", "H", "S", "O"))) %>%
    arrange(FilterName)
  
  tots <- data.frame(
    FilterName = c("", "Total exposures"),
    Total_Subs = c(NA_real_, sum(metaDf$Total_Subs)),
    Minutes = c(NA_real_, sum(metaDf$Minutes)),
    HFR = c(NA_real_, mean(metaDf$HFR)),
    FWHM = c(NA_real_, mean(metaDf$FWHM)),
    GuidingRMS = c(NA_real_, mean(metaDf$GuidingRMS))
  )
  
  metaDf <- data.frame(Object = c(basename(path), rep("", nrow(metaDf))),
                       rbind(NA, metaDf)) %>%
                       bind_rows(tots) %>%
    mutate(HFR = ifelse(!is.na(HFR), format(round(HFR, 2), nsmall = 2), NA_character_)) %>%
    mutate(FWHM = ifelse(!is.na(FWHM), format(round(FWHM, 2), nsmall = 2), NA_character_)) %>%
    mutate(GuidingRMS = ifelse(!is.na(GuidingRMS), paste0(format(round(GuidingRMS, 2), nsmall = 2),'"'), NA_character_))

}


# Process logs --------------------------------------------------------------------

# creates a clean delimited character vector of the logs
logfile <- logPath %>% pullLogs(debug = debug)

# Main data file will have start and stop times of each event
# From there it should be easy to group_by()
logReport <- read_delim(
  paste(logfile, collapse = "\n"),
  delim = "|",
  col_names = c("DATE", "LEVEL", "SOURCE", "MEMBER", "LINE", "MESSAGE"),
  trim_ws = TRUE,
  col_types = cols(.default = "c")
)  %>% 
  mutate(DATE = ymd_hms(DATE, tz = "UTC", quiet = TRUE)) %>%
  eventPairs() %>%
  mutate(ROLE = ifelse(stringr::str_detect(MESSAGE, "Flat") == TRUE, "FlatWizard", ROLE)) %>%
  mutate(ROLE = ifelse(ROLE %in% c("CenterAndRotate", "Center"), "Slew, rotate, platesolve", ROLE)) %>%
  devFix(debug) %>%
  times() %>%
  reportGen() %>%
  mutate_if(is.numeric, ~as.character(.)) %>%
  mutate_if(is.character, ~tidyr::replace_na(., ""))



# Report on the night's subs ----------------------------------------------

subsReport <- lapply(subDirs, getMetadata) %>%
  do.call("rbind", .) %>%
  mutate_if(is.numeric, ~as.character(.)) %>%
  mutate_if(is.character, ~tidyr::replace_na(., ""))

cols <- c("Object", "Filter", "Total Subs",
          "Total Minutes",
          "HFR\n(mean)", "FWHM\n(mean)",
          "Guiding RMS\n(mean)")
names(subsReport) <- cols

# RTF Report --------------------------------------------------------------

subsReport <- rbind("", "", colnames(subsReport), subsReport)
names(subsReport) <- names(logReport)


final <- bind_rows(logReport,  subsReport) %>%
  mutate_if(is.character, ~tidyr::replace_na(.,""))

final[,names(final)] <- lapply(final[,names(final)], function(x) {
  ifelse(stringr::str_detect(x, "NA") == TRUE, "", x)
})
names(final)[5:7] <- rep("", 3)



report <- final %>% 
  huxtable() %>%
  set_all_padding(0.0) %>%
  huxtable::set_bold(1, 1:ncol(.)) %>%
  huxtable::set_bold(14, 1:ncol(.)) %>%
  huxtable::set_top_padding(6) %>%
  huxtable::set_bottom_padding(6) %>%
  huxtable::set_top_border(1, 1:ncol(.), 1) %>%
  huxtable::set_bottom_border(1, 1:ncol(.), 1) %>%
  huxtable::set_width(1.5) %>%
  huxtable::set_font("arial") %>%
  huxtable::set_font_size(12) %>%
  huxtable::map_align(huxtable::by_cols(from = 2, "center"))



# Export the table --------------------------------------------------------
telescope <- subsPath %>% basename()
camera <- subsPath %>% dirname() %>% basename()


title1 <- glue::glue("Imaging report for {Sys.Date()}")
title2 <- glue::glue("{telescope} --- {camera}")



# Create the RTF document with both tables
report %>%
  rtf_doc() %>%
  add_titles(hf_line(title1, bold = TRUE, font = "arial", font_size = 12)) %>%
  add_titles(hf_line(title2, bold = TRUE, font = "arial", font_size = 12)) %>%
  write_rtf(file = glue::glue("{subsPath}/NINA Imaging report - {Sys.Date()}.rtf"))


  
  
