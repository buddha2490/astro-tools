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
  
  # Change the file paths in the metadata folder
  # Only necessary in dev (mac)
  # function will be useful later.
  homePath <- stringr::str_replace(path, file.path(sessions$object[1], sessions$session[1]), "")
  file.path(path, "ImageMetaData.csv") %>%
    readr::read_csv() %>%
    mutate(FilePath = stringr::str_replace(FilePath, 
                                            dirname(dirname(dirname(FilePath))), 
                                            homePath)) %>%
    readr::write_csv(file.path(path, "ImageMetaData.csv"))
  

  
  
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
  
  if (os == "Mac") {
    exe <- '"/Applications/PixInsight/PixInsight.app/Contents/MacOS/PixInsight"  -n --automation-mode -r='
    
    js1 <- '"/Applications/Pixinsight/src/scripts/BatchPreprocessing/FBPP.js,automationMode=true,outputDirectory='
    
    wbpp <- file("/Volumes/Office-SSD/Astronomy/astro-tools/postprocessing/wbpp.sh", "a")
    system('chmod 777 "/Volumes/Office-SSD/Astronomy/astro-tools/postprocessing/wbpp.sh"')
    
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
    pull(sessions)
  
  flatDir <- file.path(sessions, "flats")
  masterDir <- file.path(flatDir, "master")
  fakeDir <- file.path(flatDir, "fake") # testing purposes
  file.path(flatDir, "calibrated") %>% unlink(recursive = TRUE, force = TRUE)
  file.path(flatDir, "logs") %>% unlink(recursive = TRUE, force = TRUE)
  fakeDir %>% unlink(recursive = TRUE, force = TRUE) # testing purposes
  
  
  
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
    select(-LEVEL, -SOURCE, - MEMBER, -LINE)
  
}

pullLogs <- function(path, guest = FALSE) {
  
  
  # TEMP for guest
  if (guest == TRUE) {
    logFilePath <- path
  } else {
  
  
  # Get a list of log files- ideally I want to take the most recent one
  # Maybe create an archive for the the log files after I run this script?
  allFiles <- data.frame(files = list.files(logPath, pattern = ".log", full.names = TRUE)) %>%
    mutate(mtime = file.mtime(files)) %>%
    filter(stringr::str_detect(files, "robocopy") == FALSE)
  
  # This is the most recent log file, assuming it is the correct one
  # output to global environment for later copy
  logFilePath <<- allFiles %>%
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
  

  }
  # Read file and keep only the time entries
  logfile <- read_lines(logFilePath)
  logfile <- logfile[str_detect(logfile, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+")]
  
  # clean up

  return(logfile)
}

times <- function(dat, os = os, machine = machine) {
  
  dat$DATE <- as.POSIXlt(format(dat$DATE, "%Y-%m-%d %H:%M:%S"), tz = "America/New_York")
  dat$DATE <- as.POSIXct(dat$DATE)
  
  
  thisNight <<- as.Date(dat$DATE[1])
  
  
  # There's a lot of trash in these logs.  I have an annotation for start/end the sequence.
  # I can use these to subset to only the active sequence section
  starttime <<- dat %>% 
    filter(ROLE == "Annotation" & 
             str_detect(MESSAGE, "START SEQUENCE NOW") == TRUE &
             TYPE == "start") %>% 
    pull(DATE) 
  attr(starttime, "tzone") <- "America/New_York"
  
  endtime <<- dat %>%
    filter(ROLE == "Annotation" & 
             str_detect(MESSAGE, "STOP SEQUENCE NOW") == TRUE &
             TYPE == "end") %>% 
    pull(DATE)
  attr(endtime, "tzone") <- "America/New_York"
  
  # API for light/dark times for Atlanta
  if (length(starttime) == 0) starttime = getDarkTimesAPI()$getDark %>% as.POSIXct() - 86400
  if (length(endtime) == 0) endtime <- getDarkTimesAPI()$getLight %>% as.POSIXct() 
  
  dat %>%
    filter(DATE >= starttime & DATE <= endtime) %>%
    filter(ROLE != "Annotation") %>%
    group_by(EVENT_ID) %>%
    mutate(TIME = ifelse(TYPE == "end", difftime(DATE, lag(DATE), units = "secs"), 0)) %>%
    ungroup()
  
}

addSubsToSequence <- function(paths = dirname(sessions)) {
  
  dirs <- list.dirs(paths, recursive = TRUE) %>%
    setdiff(list.dirs(paths, recursive = FALSE)) %>% 
    data.frame(file = . )  %>%
    filter(!stringr::str_detect(file, "flats") & 
             !stringr::str_detect(file, "checkFits") & 
             !stringr::str_detect(file, "master") & 
             !stringr::str_detect(file, "logs") & 
             !stringr::str_detect(file, "calibrated")) %>%
    unique() %>%
    pull()

  
 foo <-  lapply(dirs, function(x) {
    readr::read_csv(glue::glue("{x}/ImageMetaData.csv"))
  }) %>%
    do.call("rbind", .) %>%
    mutate(File = basename(FilePath)) %>%
    arrange((ExposureStart)) %>%
    mutate(ROLE = basename(dirname(dirname(FilePath)))) %>%
    mutate(ROLE = stringr::str_replace_all(ROLE, " ", "")) %>%
    group_by(ROLE) %>%
    summarize(start = min((ExposureStartUTC), na.rm = TRUE),
              end = max((ExposureStartUTC), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(start =  as.POSIXct(format(start, tz = "America/New_York", usetz = TRUE))) %>%
    mutate(end =  as.POSIXct(format(end, tz = "America/New_York", usetz = TRUE)))

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


getAllMetaData <- function(path) {
  
  metadatFiles <- list.files(path = path, 
                             pattern = "ImageMetaData.csv", 
                             recursive = TRUE, 
                             full.names = TRUE) %>%
    lapply(readr::read_csv) %>%
    do.call("rbind", .) %>%
    mutate(Object = basename(dirname(FilePath)))  %>%
    select(Object, FilterName, Duration, HFR, FWHM, GuidingRMSArcSec,
           ExposureStart, FocuserTemp)
}


processMetaData <- function(path) {
  
  
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

getDarkTimesAPI <- function(lat = 33, lng = -84, date = thisNight) {
  require(httr)
  require(jsonlite)

  # Create the API URL
  url <- "https://api.sunrise-sunset.org/json"
  params1 <- list(
    lat = lat,
    lng = lng,
    date = thisNight,
    formatted = 0  # Use 0 for ISO 8601 format (UTC)
  )
  params2 <- list(
    lat = lat,
    lng = lng,
    date = thisNight + 1,
    formatted = 0  # Use 0 for ISO 8601 format (UTC)
  )
  
  # Make the GET request
  response1 <- GET(url, query = params1)
  response2 <- GET(url, query = params2)
  
  # Parse the content
  content1 <- content(response1, as = "text", encoding = "UTF-8")
  json_data1 <- fromJSON(content1)
  
  content2 <- content(response2, as = "text", encoding = "UTF-8")
  json_data2 <- fromJSON(content2)
  
  getDark <- json_data2$results$nautical_twilight_end   %>% cleanTime() 
  getLight <- json_data2$results$nautical_twilight_begin %>% cleanTime()
  
  data.frame(getDark = getDark,
             getLight = getLight)
  
}

logChartDev <- function(df = logReshaped) {
  
  df <- df %>%
    filter(!is.na(start) & !is.na(end))
  
  date <- as.Date(df$start[1]) %>% as.character()
  
  # Add the dark and light times
  
  starttime = getDarkTimesAPI()$getDark %>% as.POSIXct() - 86400
  endtime <- getDarkTimesAPI()$getLight %>% as.POSIXct() 
  
  allRoles <- allRoles %>% sort()
  objects <- setdiff(df$ROLE, allRoles)
  levels <- c(allRoles, objects, "Astronomical\nDusk-Dawn")
  
  
  df <- data.frame(EVENT_ID = 0, ROLE = "Astronomical\nDusk-Dawn", start = starttime, end = endtime) %>%
    bind_rows(df) %>%
    mutate(ROLE = factor(ROLE, levels) %>% droplevels())
  
  # Color code my events
  df$color <- ifelse(df$ROLE == "Astronomical\nDusk-Dawn", "black",
                     ifelse(df$ROLE %in% objects, "red", "royalblue"))
  
  
  # Labels
  x <- "Time (HH:MM:SS)"
  y <- "Event"
  title <- glue::glue("NINA Imaging Analysis - {date}")
  
  df$start <- as.POSIXct(df$start, tz = "America/New_York")
  df$end <- as.POSIXct(df$end, tz = "America/New_York")
  
  df$start <- as.POSIXct(format(df$start, "%Y-%m-%d %H:%M:%S"), tz = "America/New_York")
  df$end   <- as.POSIXct(format(df$end,   "%Y-%m-%d %H:%M:%S"), tz = "America/New_York")
  
  df <- df %>%
    mutate(
      ROLE = factor(ROLE, levels = unique(ROLE)),
      ROLE = fct_rev(ROLE),
      # compute midpoint and duration in seconds for geom_tile
      mid = start + (end - start) / 2,
      dur = as.numeric(difftime(end, start, units = "secs"))
    )
  
  
  gantt_plot <- ggplot(df, aes(x = mid, y = ROLE)) +
    # rectangles from start→end
    geom_tile(aes(width = dur, height = 0.6, fill = color),
              color = NA,       # no border
              show.legend = FALSE) +
    scale_fill_identity() +
    
    # subtle vertical gridlines at each hour
    geom_vline(
      xintercept = seq(min(df$start), max(df$end), by = "1 hour"),
      color      = "gray90",
      size       = 0.5
    ) +
    
    # clean time axis
    scale_x_datetime(
      date_breaks = "1 hour",
      date_labels = "%H:%M",
      expand      = c(0, 0)
    ) +
    
    # labels
    labs(
      title    = glue("NINA Imaging Analysis — {date}"),
      subtitle = "Gantt‑style timeline of imaging events",
      x        = "Time (HH:MM)",
      y        = NULL
    ) +
    
    # polished theme
    theme_minimal(base_size = 14) +
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
    filter(total_time_mins != 0) %>%
    filter(ROLE != "Astronomical\nDusk-Dawn") %>%
    arrange(desc(total_time_mins))
  
  # Add total event and sequence time rows
  total_event_time <- round(sum(summary_table$total_time_mins), 1) %>% format(nsmall = 1)
  sequence_time <- round(as.numeric(difftime(max(df$end), min(df$start), units = "mins")), 1) %>%
    format(nsmall = 1)
  
  summary_table <- summary_table %>%
    mutate(percent = paste0(round(100 * as.numeric(total_time_mins) / as.numeric(total_event_time), 1), "%"))  %>%
    mutate(total_time_mins = as.character(total_time_mins))
  
  
  summary_table2 <- data.frame(ROLE = c("Total event time", "Total darkness time"),
                               total_time_mins = c(total_event_time, sequence_time))
  
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







