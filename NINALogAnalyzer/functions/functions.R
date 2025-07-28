# Environmental variables

# Log files

eventPairs <- function(dat) {
  
  starts <- dat %>%
    filter(str_detect(MESSAGE, "^Starting Category:") |
           MESSAGE == "Meridian Flip - Initializing Meridian Flip.") %>%
    mutate(
      ROLE = str_match(MESSAGE, "Item: ([^,]+)")[,2],
      EVENT_ID = row_number(),  # temporary unique ID
      TYPE = "start"
    ) %>%
    mutate(ROLE = ifelse(is.na(ROLE) & MESSAGE == "Meridian Flip - Initializing Meridian Flip.", "MeridianFlip", ROLE))
  
  finishes <- dat %>%
    filter(str_detect(MESSAGE, "^Finishing Category:") |
            MESSAGE == "Meridian Flip - Exiting meridian flip") %>%
    mutate(
      ROLE = str_match(MESSAGE, "Item: ([^,]+)")[,2],
      TYPE = "end"
    ) %>%
    mutate(ROLE = ifelse(is.na(ROLE) & MESSAGE == "Meridian Flip - Exiting meridian flip", "MeridianFlip", ROLE))
  
  
  bind_rows(starts, finishes) %>%
    arrange(DATE) %>%
    filter(!is.na(ROLE)) %>%
    mutate(EVENT_ID = ifelse(is.na(EVENT_ID), lag(EVENT_ID), EVENT_ID)) %>%
   select(-any_of(c("LEVEL", "SOURCE", "MEMBER", "LINE")))
    
  
}

pullLogs <- function(path = "www") {
  
  # Read file and keep only the time entries
  logfile2 <- readr::read_lines(file.path(path, "latest.log"))
  logfile2 <- logfile2[str_detect(logfile2, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+")]
  
  return(logfile2)
}

times <- function(dat, start = NULL, end = NULL) {
  
  dat$DATE <- as.POSIXlt(format(dat$DATE, "%Y-%m-%d %H:%M:%S"), tz = "America/New_York")
  dat$DATE <- as.POSIXct(dat$DATE)
  
  if (is.null(start)) {
    start <- min(dat$DATE, na.rm = TRUE) %>% as.POSIXct(tz = "America/New_York")
  }
  if (is.null(end)) {
    end <- max(dat$DATE, na.rm = TRUE) %>% as.POSIXct(tz = "America/New_York")
  }
  
  thisNight <<- as.Date(dat$DATE[1])
  
  dat %>%
    group_by(EVENT_ID) %>%
    mutate(TIME = ifelse(TYPE == "end", difftime(DATE, lag(DATE), units = "secs"), 0)) %>%
    ungroup() %>%
    filter(DATE >= start & DATE <= end)
  
}

addSubsToSequence <- function(path = subsPath) {
  
  dirs <- list.dirs(path, recursive = TRUE) %>%
    setdiff(path) %>%
    setdiff(list.dirs(path, recursive = FALSE))
  
 foo <-  lapply(dirs, function(x) {
    readr::read_csv(glue::glue("{x}/ImageMetaData.csv"))
  }) %>%
    do.call("rbind", .) %>%
    mutate(File = basename(FilePath)) %>%
    filter(File %in% basename(list.files(path, pattern = "fit",  recursive = TRUE))) %>%
    arrange((ExposureStart)) %>%
    mutate(ROLE = basename(dirname(dirname(FilePath)))) %>%
    group_by(ROLE) %>%
    summarize(start = min(ExposureStart, na.rm = TRUE),
              end = max(ExposureStart, na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(start = as.POSIXct(as.character(start), tz = "America/New_York")) %>%
    mutate(end = as.POSIXct(as.character(end), tz = "America/New_York"))
 

  
 return(foo)
}

getMetaDataStartStop <- function(path, os = os, machine = machine) {
  metadatFiles <- list.files(path = path, 
                             pattern = "ImageMetaData.csv", 
                             recursive = TRUE, 
                             full.names = TRUE)
  metadatFiles <- metadatFiles[!duplicated(metadatFiles)]
  
  metaDf <- lapply(metadatFiles, readr::read_csv) %>%
    do.call("rbind", .) %>%
    mutate(Object = basename(dirname(FilePath)))  %>%
    mutate(start = ExposureStart) %>%
    arrange(desc(ExposureStart)) %>%
    slice(1)
  
  
}

processMetaData <- function(path, os = os, machine = machine) {
  
  
  metadatFiles <- list.files(path = path, 
                             pattern = "ImageMetaData.csv", 
                             recursive = TRUE, 
                             full.names = TRUE)
  metadatFiles <- metadatFiles[!duplicated(metadatFiles)]
  
  metaDf <- lapply(metadatFiles, readr::read_csv) %>%
    do.call("rbind", .) %>%
    mutate(Object = basename(dirname(FilePath)))  %>%
    select(Object, FilterName, Duration, HFR, FWHM, GuidingRMSArcSec) %>%
    group_by(Object, FilterName) %>%
    summarize(Total_Subs = n(),
              Minutes = sum(Duration / 60),
              HFR = mean(HFR, na.rm = TRUE),
              FWHM = mean(FWHM, na.rm = TRUE),
              GuidingRMS = mean(GuidingRMSArcSec, na.rm = TRUE)) %>%
    mutate(FilterName = factor(FilterName,
                               c("L", "R", "G", "B", "H", "S", "O"))) %>%
    arrange(Object, FilterName) %>%
    group_by(Object) %>%
    mutate(Object = ifelse(row_number() != 1, "", Object))
  
  tots <- data.frame(
    FilterName = c("", "Total exposures"),
    Total_Subs = c(NA_real_, sum(metaDf$Total_Subs)),
    Minutes = c(NA_real_, sum(metaDf$Minutes)),
    HFR = c(NA_real_, mean(metaDf$HFR)),
    FWHM = c(NA_real_, mean(metaDf$FWHM)),
    GuidingRMS = c(NA_real_, mean(metaDf$GuidingRMS))
  )
  
  metaDf  %>% bind_rows(tots) %>%
    mutate(HFR = ifelse(!is.na(HFR), format(round(HFR, 2), nsmall = 2), NA_character_)) %>%
    mutate(FWHM = ifelse(!is.na(FWHM), format(round(FWHM, 2), nsmall = 2), NA_character_)) %>%
    mutate(GuidingRMS = ifelse(!is.na(GuidingRMS), paste0(format(round(GuidingRMS, 2), nsmall = 2),'"'), NA_character_))
  
}

cleanupLogs <- function(dat, os = os, machine = machine) {
  
  dat %>%
    filter(stringr::str_detect(MESSAGE, "Trigger") == FALSE) %>%
    mutate(ROLE =  ifelse(ROLE %in% c("CenterAndRotate", "FineHome", "Center"), "Slew, rotate, platesolve", ifelse(
      ((ROLE %in% c("CloseClover", "OpenCover", "SetBrightness", "ToggleLight")) |
         (stringr::str_detect(MESSAGE, "Flat") == TRUE)), "Flats", ROLE)
    )) %>%
    mutate(ROLE = ifelse(ROLE == "TakeExposure" & stringr::str_detect(MESSAGE, "FLAT") == TRUE, "Flats", ROLE))
  
}

cleanTime <- function(time) {
  
  utc_time_clean <- sub("(\\+|\\-)(\\d{2}):(\\d{2})", "\\1\\2\\3", time)
  
  utc_posix <- as.POSIXct(utc_time_clean, format = "%Y-%m-%dT%H:%M:%S%z", tz = "UTC")
  
  format(utc_posix, tz = "America/New_York", usetz = TRUE)
  
}

logChartDev <- function(dat) {
  
  df <- dat
  
  date <- as.Date(df$start[1]) %>% as.character()
  
  allRoles <- sort(allRoles)
  objects <- setdiff(df$ROLE, allRoles)
  levels <- c(allRoles, objects)
  
  
  df <- df %>%
    mutate(ROLE = factor(ROLE, levels) %>% droplevels())
  
  # Color code my events
  df$color <- ifelse(df$ROLE %in% objects, "red", "royalblue")
  
  
  # Labels
  x <- "Time (HH:MM:SS)"
  y <- "Event"
  title <- glue::glue("NINA Imaging Analysis - {date}")
  
  df$start <- as.POSIXct(df$start, tz = "America/New_York")
  df$end <- as.POSIXct(df$end, tz = "America/New_York")
  
  df$start <- as.POSIXct(format(df$start, "%Y-%m-%d %H:%M:%S"), tz = "America/New_York")
  df$end   <- as.POSIXct(format(df$end,   "%Y-%m-%d %H:%M:%S"), tz = "America/New_York")
  
  gantt_plot <- ggplot(df, aes(x = start, xend = end, y = ROLE, yend = ROLE)) +
    geom_segment(aes(color = color), size = 4, show.legend = FALSE) +
    scale_color_identity() +
    scale_x_datetime(
      date_breaks = "1 hour",
      date_labels = "%H:%M",
      expand      = c(0, 0)
    ) +
    labs(x = "Time (HH:MM)", title = glue("NINA Log Analysis — {date}"),
         subtitle = "Timeline of imaging events", y = NULL) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title         = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle      = element_text(size = 12, hjust = 0.5, margin = margin(b = 10)),
      axis.text.x        = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y        = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.background   = element_rect(fill = "gray98", color = NA),
      plot.background    = element_rect(fill = "white",  color = NA),
      plot.margin        = margin(10, 10, 10, 10)
    )
  
  # Summarize durations
  summary_table <- df %>%
    filter(!ROLE %in% objects) %>%
    mutate(duration = as.numeric(difftime(end, start, units = "secs"))) %>%
    group_by(ROLE) %>%
    summarise(
      N = as.numeric(n()),
      total_time_mins = round(sum(duration) / 60, 1),
      .groups = "drop"
    ) %>%
    filter(total_time_mins != 0)  %>%
    arrange(desc(total_time_mins))
  
  # Add total event and sequence time rows
  total_event_time <- round(sum(summary_table$total_time_mins), 1) %>% format(nsmall = 1)
  sequence_time <- round(as.numeric(difftime(max(df$end), min(df$start), units = "mins")), 1) %>%
    format(nsmall = 1)
  
  summary_table <- summary_table %>%
    mutate(percent = paste0(round(100 * as.numeric(total_time_mins) / as.numeric(total_event_time), 1), "%"))  %>%
    mutate(total_time_mins = as.character(total_time_mins))
  
  
  summary_table2 <- data.frame(ROLE = c("Total event time"),
                               total_time_mins = c(total_event_time))
  
  
  summary_table <- bind_rows(
    summary_table,
    summary_table2
  ) %>%
    mutate_if(is.numeric, ~as.character(.)) %>%
    mutate_if(is.character, ~tidyr::replace_na(., "")) 
  
  names(summary_table) <- c("Event", "N", "Total time\n(mins)", "%")

  # Return these object
  list(summary_table = summary_table,
       plot = gantt_plot) %>%
    return()
  
}

