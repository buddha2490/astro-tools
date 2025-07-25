# Environmental variables



# Functions ---------------------------------------------------------------
# postprocessing.R
getDarks <- function(temp = temp, gain = gain, duration = duration, copyto = sessions) {
  
  biasfile <- glue::glue("{darks}/{temp}/Gain{gain}/masterBias_gain{gain}.xisf") %>% normalizePath()
  
  darkfile <- glue::glue("{darks}/{temp}/Gain{gain}/masterDark_{duration}s.xisf") %>% normalizePath()
  
  biasCopyto <- glue::glue("{copyto}/flats") %>% normalizePath()
  
  file.copy(biasfile, biasCopyto)
  
  file.copy(darkfile, copyto)
  
}

processObjects <- function(myObject, os = os, machine = machine) {
  
  
  sessions <- data.frame(object = basename(dirname(myObject)))  %>%
    mutate(folder = (myObject)) %>%
    mutate(session = basename(myObject)) %>%
    mutate(dates = stringr::str_replace(session, paste0(object, "_"), "")) 
  
  date <- sessions$dates %>%  as.character()
  
  objectName <- sessions$object %>% as.character() 
  
  path <- sessions$folder[1]
  
  # Get the image metadata
  # This metadata file does not include calibration subs
  metadata <- file.path(path, "ImageMetaData.csv") %>%
    readr::read_csv() %>%
    mutate(FilePath = stringr::str_replace(FilePath, "/ES127/ES127/", "/ES127/")) %>%
    mutate(filename = basename(FilePath)) %>%
    mutate(FilePath = stringr::str_replace(FilePath, filename, stringr::str_replace(filename, " ", "")))
  
  
  # create some directories
  objectFlats <- glue::glue(path, "/flats"); dir.create(objectFlats, showWarnings = FALSE)
  objectFits <- glue::glue(path, "/checkFits"); dir.create(objectFits, showWarnings = FALSE)
  
  # Move flats into the flats folder
  #TODO: with mono, there are about 200 flats per object and this copy is a significant bottleneck
  # see if I can leverage some system command
  flatfiles <- list.files(path, pattern = "FLAT", full.names = TRUE)
  if (length(flatfiles) > 0) {
    returnStatus = sapply(flatfiles, function(x) file.copy(from = x, to = glue::glue(path, "/flats"), overwrite = T))
    names(returnStatus) <- NULL
    if (sum(returnStatus) == length(flatfiles)) {
      sapply(flatfiles, file.remove)
    }
  }
  
  # grab some info from the file
  # Figure out how many image loops I need
  # i.e. if I have two filters, two sets of flats
  # I need this metric for later
  imageCombo <- metadata %>%
    group_by(CameraTargetTemp, Gain, Duration, FilterName) %>%
    distinct(CameraTargetTemp, Gain, Duration, FilterName)
  
  
  # Flag the fit files that need individual review
  for (i in 1:nrow(imageCombo)) {
    df <- filter(metadata, FilterName == imageCombo$FilterName[i])
    
    
    measures <- c("DetectedStars", "HFR", "FWHM", "Eccentricity", "GuidingRMSArcSec")
    metrics <- lapply(measures, function(x) {
      sd(df[[x]], na.rm = TRUE)
    })
    names(metrics) <- c("stars", "HFR", "FWHM", "roundness", "guiding")
    
    # Flag files 
    df <- df %>%
      mutate(LowStars = ifelse(DetectedStars < mean(DetectedStars, na.rm = TRUE) - 2 * metrics$stars, 1, 0)) %>%
      mutate(HighHFR = ifelse(HFR > mean(HFR, na.rm = TRUE) + 2 * metrics$HFR, 1, 0)) %>%
      mutate(HighFWHM = ifelse(FWHM > mean(FWHM, na.rm = TRUE) + 2 * metrics$FWHM, 1, 0)) %>%
      mutate(notRound = ifelse(Eccentricity > 0.6, 1, 0)) %>%
      mutate(badGuiding = ifelse(GuidingRMSArcSec > 1, 1, 0)) %>%
      mutate(Exclusion = ifelse(LowStars == 1, 1, ifelse(
        HighHFR == 1, 2, ifelse(HighFWHM == 1, 3, ifelse(
          notRound == 1, 4, ifelse(
            badGuiding == 1, 5, 99)))))) %>%
      mutate(Exclusion = factor(Exclusion, c(1:5, 99), c("Low star count", ">HFR", ">FWHM", 
                                                         "Eccentricity > 0.6", "Poor Guiding", ""))) %>%
      mutate(flag = ifelse(Exclusion == "", 0 , 1))
    
    
    # Move flagged subs to a directory for individual review
    flaggedSubs <- df %>%  
      filter(flag == 1) %>%
      select(FilePath, FilterName, DetectedStars, HFR, HFRStDev, FWHM, Eccentricity, GuidingRMSArcSec, Exclusion)
    
    lapply(flaggedSubs$FilePath, function(x) {
      if (file.exists(x)) file.copy(x, glue::glue("{path}/checkFits"))
      if (file.exists(x)) file.remove(x)
    })
    
  }
  
  # Copy over my darks
  # Matches the dark to the temp/gain/duration
  # Also gets a matched master bias to stack with the flats
  # Runs in mac or windows
  for (i in 1:nrow(imageCombo)) {
    getDarks(
      temp = imageCombo$CameraTargetTemp[i],
      gain = imageCombo$Gain[i],
      duration = imageCombo$Duration[i],
      copyto = path
    )
  }

  # Process my flats
  # This creates a bat file that calls PixInsight
  wbppFlats(objectFlats)
  
}

wbppFlats <- function(objectFlats) {
  
  if (os == "Windows" & machine == "ES127") {
    
    exe <- '"C:\\Program Files\\PixInsight\\bin\\PixInsight.exe"  -n --automation-mode -r='
    
    js1 <- '"C:\\Program Files\\PixInsight\\src\\scripts\\BatchPreprocessing\\WBPP.js,automationMode=true,outputDirectory='
    
    wbpp <- file("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing/wbpp.bat", "a")
  }
  
  if (os == "Mac" & machine == "MBP14") {
    exe <- '"/Applications/PixInsight/PixInsight.app/Contents/MacOS/PixInsight"  -n --automation-mode -r='
    
    js1 <- '"/Applications/Pixinsight/src/scripts/BatchPreprocessing/FBPP.js,automationMode=true,outputDirectory='
    
    wbpp <- file("/Users/briancarter/Astronomy/astro-tools/postprocessing/wbpp.sh", "a")
    system('chmod 777 "/Users/briancarter/Astronomy/astro-tools/postprocessing/wbpp.sh"')
    
  }
  
  outputdir <- objectFlats
  
  dir <- glue::glue(objectFlats, '"')
  
  js2 <- '--force-exit'
  
  foo <- glue::glue("{exe}{js1}{outputdir},dir={dir} {js2}")
  
  write(foo, wbpp, append = TRUE)
  close(wbpp)
  

  
}

testit <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}

countFlats <- function(objectFlats, os = os, machine = machine) {
  
  nFlats <-list.files(objectFlats, pattern = "fits")
  L <- nFlats[grep("L", nFlats)]
  R <- nFlats[grep("R", nFlats)]
  G <- nFlats[grep("G", nFlats)]
  B <- nFlats[grep("B", nFlats)]
  H <- nFlats[grep("H", nFlats)]
  S <- nFlats[grep("S", nFlats)]
  O <- nFlats[grep("O", nFlats)]
  
  
  
  L <- ifelse(length(L) > 0, 1, 0)
  R <- ifelse(length(R) > 0, 1, 0)
  G <- ifelse(length(G) > 0, 1, 0)
  B <- ifelse(length(B) > 0, 1, 0)
  H <- ifelse(length(H) > 0, 1, 0)
  S <- ifelse(length(S) > 0, 1, 0)
  O <- ifelse(length(O) > 0, 1, 0)
  
  sum(L, R, G, B, H, S, O)
  
  
}

bulkRename <- function(myObject, os = os, machine = machine) {
  
  # Remove spaces from the file names
  # Sometimes I forget to do this in NINA
  subs <- list.files(myObject, pattern = "fit")
  newname <- stringr::str_replace(subs, " ", "")
  for (i in 1:length(subs)) {
    file.rename(from = glue::glue("{myObject}/{subs[i]}"),
                to = glue::glue("{myObject}/{newname[i]}"))
  }

}
  


# Cleanup
cleanup <- function(myObject, os = os, machine = machine) {
  
  sessions <- data.frame(sessions = list.dirs(myObject, recursive = FALSE, full.names = TRUE)) %>%
    mutate(object = basename(myObject)) %>%
    mutate(folder = basename(sessions)) %>%
    mutate(dates = stringr::str_replace(folder, paste0(object, "_"), ""))  %>%
    filter(dates == Sys.Date() - 1) %>%
    pull(sessions)
  
  flatDir <- file.path(sessions, "flats")
  masterDir <- file.path(flatDir, "master")
  file.path(flatDir, "calibrated") %>% unlink(recursive = TRUE, force = TRUE)
  file.path(flatDir, "logs") %>% unlink(recursive = TRUE, force = TRUE)
  
  # Count the types of flats
  # flats <- list.files(flatDir, pattern = "fits")
  # l <- ifelse(length(flats[grep("L", flats)]) >0, 1, 0)
  # r <- ifelse(length(flats[grep("R", flats)]) >0, 1, 0)
  # g <- ifelse(length(flats[grep("G", flats)]) >0, 1, 0)
  # b <- ifelse(length(flats[grep("B", flats)]) >0, 1, 0)
  # h <- ifelse(length(flats[grep("H", flats)]) >0, 1, 0)
  # s <- ifelse(length(flats[grep("S", flats)]) >0, 1, 0)
  # o <- ifelse(length(flats[grep("O", flats)]) >0, 1, 0)
  # 
  # totalFlats <- sum(l, r, g, b, h, s, o)
  # 
   #  Process the stacked flats
  nMaster <- list.files(masterDir, pattern = ".xisf", full.names = TRUE)
  renameStatus <- renameFlats(nMaster)
  
  
  # copy the files
  # should fail safe if the flats didn't rename correctly
  copyFiles <- c(
    renameStatus %>%
      filter(returnStatus == "TRUE") %>%
      mutate(foo = glue::glue("{path}/{newFlats}")) %>%
      pull(foo) %>%
      file.path(),
    renameStatus %>%
      filter(returnStatus == "FALSE") %>%
      mutate(foo = glue::glue("{path}/{file}")) %>%
      pull(foo) %>%
      file.path()
   ) 
  
  lapply(copyFiles, function(x) file.copy(from = x, to = sessions, overwrite = TRUE))

  xisf <- list.files(sessions, pattern = "masterFlat")

  if (length(xisf) == length(copyFiles)) {
    unlink(flatDir, recursive = TRUE, force = TRUE)
  }
}

renameFlats <- function(flatsList) {
  if (length(flatsList) == 0) {
    return(NULL)
  }
  
  flats <- data.frame(path = dirname(flatsList),
                      file = basename(flatsList)) %>%
    mutate(newFlats = ifelse(stringr::str_detect(file, "L_mono") == TRUE, "masterFlat_L.xisf", ifelse(
      stringr::str_detect(file, "R_mono") == TRUE, "masterFlat_R.xisf", ifelse(
        stringr::str_detect(file, "G_mono") == TRUE, "masterFlat_G.xisf", ifelse(
          stringr::str_detect(file, "B_mono") == TRUE, "masterFlat_B.xisf", ifelse(
            stringr::str_detect(file, "S_mono") == TRUE, "masterFlat_S.xisf", ifelse(
              stringr::str_detect(file, "H_mono") == TRUE, "masterFlat_H.xisf", ifelse(
                stringr::str_detect(file, "O_mono") == TRUE, "masterFlat_O.xisf", "")))))))) %>%
    mutate(returnStatus = "") 
  
  suppressWarnings({
  for (i in 1:nrow(flats)) {
    if (file.exists(glue::glue("{flats$path[i]}/{flats$file[i]}"))) {
      returnStatus <- file.rename(
        from = glue::glue("{flats$path[i]}/{flats$file[i]}"),
        to = glue::glue("{flats$path[i]}/{flats$newFlats[i]}")
      )
      flats$returnStatus[i] <- as.character(returnStatus)
    }
  }
  })
  return(flats)
}

# Log files

eventPairs <- function(dat, os = os, machine = machine) {
  
  starts <- dat %>%
    filter(str_detect(MESSAGE, "^Starting Category:")) %>%
    mutate(
      ROLE = str_match(MESSAGE, "Item: ([^,]+)")[,2],
      EVENT_ID = row_number(),  # temporary unique ID
      TYPE = "start"
    )
  
  finishes <- dat %>%
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


pullLogs <- function(path, myDebug = debug, os = os, machine = machine) {
  
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
  if (myDebug == TRUE) {
    logFilePath <- glue::glue("{path}/log.log")
  }
  
  # Read file and keep only the time entries
  logfile <- read_lines(logFilePath)
  logfile <- logfile[str_detect(logfile, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+")]
  
  # clean up
  if (myDebug != TRUE) {
    
    logFilePath %>% file.copy(to = subsPath, overwrite = FALSE)
    
    # logFilePath %>% file.remove()
  }
  return(logfile)
}

devFix <- function(dat, debug = debug, os = os, machine = machine) {
  
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

times <- function(dat, os = os, machine = machine) {
  
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
  
  if (length(endtime) == 0) endtime <- as.POSIXct("2025-08-24 05:30:00", tz = "UTC")
  
  dat %>%
    filter(DATE >= starttime & DATE <= endtime) %>%
    filter(ROLE != "Annotation") %>%
    group_by(EVENT_ID) %>%
    mutate(TIME = ifelse(TYPE == "end", difftime(DATE, lag(DATE), units = "secs"), 0)) %>%
    ungroup()
  
}

reportGen <- function(dat, endtime = endtime, starttime = starttime, os = os, machine = machine) {
  
  starttime <- dat$DATE[1]
  endtime <- dat$DATE[nrow(dat)]
  
  
  totaltime <- difftime(endtime, starttime, units = "mins") %>% as.numeric()
  
  report <- dat %>%
    group_by(ROLE) %>%
    summarise(
      TotalMinutes = sum(TIME, na.rm = TRUE) / 60,
      N_events = ceiling(n() / 2)
    ) %>%
    mutate(AverageTime = TotalMinutes / N_events) %>%
    mutate(PercentTime = TotalMinutes / totaltime * 100) %>%
    arrange(desc(PercentTime))
  
  tots <- data.frame(
    ROLE = c("", "Total Event Time", "Total Sequence Time"),
    TotalMinutes = c(NA_real_, 
                     sum(report$TotalMinutes, na.rm = TRUE),
                     difftime(endtime, starttime, units = "mins")))
  
  report <- report %>%
    bind_rows(tots) %>%
    mutate(TotalMinutes = format(round(TotalMinutes, 2), nsmall = 2)) %>%
    mutate(AverageTime = format(round(AverageTime, 2), nsmall = 2)) %>%
    mutate(PercentTime = format(round(PercentTime, 2), nsmall = 2))
  
  colnames(report) <- c("Role", "Total Minutes", "N Events", "Average Time (mins)", "Percent (Total Sequence)")
  
  return(report)
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


logChart <- function(df) {
  
  df <- logReshaped %>%
    mutate(
      start = as.POSIXct(start, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC"),
      end = as.POSIXct(end, format = "%Y-%m-%d %H:%M:%OS", tz = "UTC")
    ) %>%
    filter(!is.na(start) & !is.na(end))
  
  gantt_plot <- ggplot(df, aes(x = start, xend = end, y = ROLE, yend = ROLE)) +
    geom_segment(color = "royalblue", size = 4) +
    scale_x_datetime(
      labels = scales::time_format("%H:%M:%S"),
      breaks = seq(min(df$start), max(df$end), by = "1 hour")
    ) +
    labs(x = "Time (HH:MM:SS)", y = "Event", title = "NINA Imaging Analysis") +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.text.y = element_text(size = 8),
      axis.title.y = element_text(size = 10),
      plot.margin = margin(5, 5, 5, 5),
      panel.spacing.y = unit(0.1, "lines")
    ) +
    scale_y_discrete(expand = expansion(mult = c(0.02, 0.02)))
  
  # Summarize durations
  summary_table <- df %>%
    mutate(duration = as.numeric(difftime(end, start, units = "secs"))) %>%
    group_by(ROLE) %>%
    summarise(
      N = as.numeric(n()),
      total_time_mins = round(sum(duration) / 60, 1),
      .groups = "drop"
    )
  
  # Add total event and sequence time rows
  total_event_time <- round(sum(summary_table$total_time_mins), 1) %>% format(nsmall = 1)
  sequence_time <- round(as.numeric(difftime(max(df$end), min(df$start), units = "mins")), 1) %>%
    format(nsmall = 1)
  
  summary_table <- summary_table %>%
    mutate(percent = paste0(round(100 * as.numeric(total_time_mins) / as.numeric(sequence_time), 1), "%"))  %>%
    mutate(total_time_mins = as.character(total_time_mins))
  
  
  summary_table2 <- data.frame(ROLE = c("Total event time", "Total sequence time"),
                               total_time_mins = c(total_event_time, sequence_time))
  
  
  summary_table <- bind_rows(
    summary_table,
    summary_table2
  ) %>%
    mutate_if(is.numeric, ~as.character(.)) %>%
    mutate_if(is.character, ~tidyr::replace_na(., ""))
  
  names(summary_table) <- c("Event", "N", "Total time\n(mins)", "%")
  
  # Format table as grob
  table_grob <- tableGrob(summary_table, rows = NULL, theme = ttheme_default(base_size = 10))
  
  # Combine plot and table
  final_plot <- gantt_plot / table_grob + plot_layout(heights = c(1.5, 1))
  
  # Save as PNG (8.5 x 11 inches)
  ggsave(glue::glue("{subsPath}/Imaging summary - {Sys.Date()}.png"), final_plot, width = 11, height = 8.5, units = "in", dpi = 300)
  
}


