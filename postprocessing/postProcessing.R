library(dplyr)
library(magrittr)

setwd("C:/users/bcart/Astronomy/astro-tools/postprocessing")



# Functions ---------------------------------------------------------------

# go get my darks
getDarks <- function(temp = temp, gain = gain, duration = duration, copyto = sessions) {
  
  biasfile <- glue::glue("{darks}/{temp}/Gain{gain}/masterBias_gain{gain}.xisf")
  
  darkfile <- glue::glue("{darks}/{temp}/Gain{gain}/masterDark_{duration}s.xisf")
  
  biasCopyto <- glue::glue("{copyto}/flats")
  
  file.copy(biasfile, biasCopyto)
  
  file.copy(darkfile, copyto)
  
}

processObjects <- function(myObject) {
  

  # I only want to process the last night
  # The most recent imaging session will be labeled with last nights' date
  sessions <- data.frame(sessions = list.dirs(myObject, recursive = FALSE, full.names = TRUE)) %>%
    mutate(object = basename(myObject)) %>%
    mutate(folder = basename(sessions)) %>%
    mutate(dates = stringr::str_replace(folder, paste0(object, "_"), ""))  %>%
    filter(dates == Sys.Date() - 1)
  
  date <- sessions$dates %>% as.character()
  
  objectName <- sessions$object %>% as.character() 
  
  path <- sessions$sessions[1]
  
  # Get the image metadata
  # This metadata file does not include calibration subs
  metadata <- file.path(path, "ImageMetaData.csv") %>%
    readr::read_csv()
  

  # create some directories
  objectFlats <- glue::glue(path, "/flats"); dir.create(objectFlats, showWarnings = FALSE)
  objectFits <- glue::glue(path, "/checkFits"); dir.create(objectFits, showWarnings = FALSE)
  
  
  # Move flats into the flats folder
  flatfiles <- list.files(path, pattern = "FLAT", full.names = TRUE)
  returnStatus = sapply(flatfiles, function(x) file.copy(from = x, to = glue::glue(path, "/flats"), overwrite = T))
  names(returnStatus) <- NULL
  if (sum(returnStatus) == length(flatfiles)) {
    sapply(flatfiles, file.remove)
  }
  
  # grab some info from the file
  # Figure out how many image loops I need
  # i.e. if I have two filters, two sets of flats
  # I need this metric for later
  temp <- metadata %>%
    distinct(CameraTargetTemp) %>%
    pull() %>%
    as.character()
  gain <- metadata %>%
    distinct(Gain) %>%
    as.character()
  duration <- metadata %>%
    distinct(Duration) %>%
    pull()
  filter <- metadata %>%
    distinct(FilterName) %>%
    pull()
  imageCombo <- data.frame(temp, gain, duration, filter) # one row per filter
  
  
  # Flag the fit files that need individual review
  for (i in 1:nrow(imageCombo)) {
    df <- filter(metadata, FilterName == imageCombo$filter[i])
    
    
    measures <- c("DetectedStars", "HFR", "FWHM", "Eccentricity", "GuidingRMSArcSec")
    metrics <- lapply(measures, function(x) {
      sd(df[[x]], na.rm = TRUE)
    })
    names(metrics) <- c("stars", "HFR", "FWHM", "roundness", "guiding")
    
    # Flag files 
    df <- df %>%
      mutate(LowStars = ifelse(DetectedStars < mean(DetectedStars) - 2* metrics$stars, 1, 0)) %>%
      mutate(HighHFR = ifelse(HFR > mean(HFR) + 2 * metrics$HFR, 1, 0)) %>%
      mutate(HighFWHM = ifelse(FWHM > mean(FWHM) + 2 * metrics$FWHM, 1, 0)) %>%
      mutate(notRound = ifelse(Eccentricity > 0.6, 1, 0)) %>%
      mutate(badGuiding = ifelse(GuidingRMSArcSec > 1.05, 1, 0)) %>%
      mutate(flag = ifelse(LowStars == 1 | HighHFR == 1 | HighFWHM == 1 | notRound == 1 | badGuiding == 1, 1, 0)) %>%
      filter(flag == 1) %>%
      select(FilePath, FilterName, DetectedStars, HFR, HFRStDev, FWHM, Eccentricity, GuidingRMSArcSec, flag)
    
    lapply(df$FilePath, function(x) {
      if (file.exists(x)) file.copy(x, glue::glue("{path}/checkFits"))
      if (file.exists(x)) file.remove(x)
    })
    
    write.csv(df, file = glue::glue("{path}/checkFits/{imageCombo$filter[i]}_summary.csv"), row.names = FALSE)
    
  }
  
  # Copy over my darks
  # Matches the dark to the temp/gain/duration
  # Also gets a matched master bias to stack with the flats
  for (i in 1:nrow(imageCombo)) {
    getDarks(
      temp = imageCombo$temp[i],
      gain = imageCombo$gain[i],
      duration = imageCombo$duration[i],
      copyto = path
    )
  }
  
  # Process my flats
  # This creates a bat file that calls PixInsight
  wbppFlats(objectFlats)
  
  
}

wbppFlats <- function(objectFlats) {
  
  exe <- '"C:\\Program Files\\PixInsight\\bin\\PixInsight.exe"  -n --automation-mode -r='
  
  js1 <- '"C:\\Program Files\\PixInsight\\src\\scripts\\WeightedBatchPreprocessing\\WeightedBatchPreprocessing.js,automationMode=true,outputDirectory='
  
  outputdir <- objectFlats
  
  dir <- glue::glue(objectFlats, '"')
  
  js2 <- '--force-exit'
  
  foo <- glue::glue("{exe}{js1}{outputdir},dir={dir} {js2}")

  wbpp <- file("C:/users/bcart/Astronomy/astro-tools/postprocessing/wbpp.bat", "a")
  write(foo, wbpp, append = TRUE)
  close(wbpp)
  
}

testit <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}

countFlats <- function(objectFlats) {
  
  nFlats <-list.files(objectFlats, pattern = "fits")
  UVIR <- nFlats[grep("UVIR", nFlats)]
  LPRO <- nFlats[grep("LPRO", nFlats)]
  HO <- nFlats[grep("HO", nFlats)]
  SO <- nFlats[grep("SO", nFlats)]
  
  UVIR <- ifelse(length(UVIR) > 0, 1, 0)
  LPRO <- ifelse(length(LPRO) > 0, 1, 0)
  HO <- ifelse(length(HO) > 0, 1, 0)
  SO <- ifelse(length(SO) > 0, 1, 0)
  
  sum(UVIR, LPRO, HO, SO)
}




# Directories -------------------------------------------------------------

src <- file.path("C:/users/bcart/Astronomy/astro-tools/postprocessing")

camera <- "c:/users/bcart/astronomy/ASI2600MC/ES127"

darks <- file.path(camera, "../Dark Library/") 

objects <- list.dirs(camera, recursive = FALSE, full.names = TRUE) 


objects %>%  lapply(processObjects)



  









