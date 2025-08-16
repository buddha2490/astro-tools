

# AstroDB
library(dplyr)
library(DBI)
library(stringr)
library(magrittr)
library(tidyr)


debug <- FALSE

os <- Sys.info()["sysname"]
machine <- Sys.info()["nodename"]

os <- ifelse(os == "Darwin", "Mac", "Windows") %>% as.character()
machine <- ifelse(
  stringr::str_detect(machine, "BRIANC-MacUS") == TRUE, "MBP13", ifelse(
    stringr::str_detect(machine, "Brians-MacBook-Pro") == TRUE, "MBP14", ifelse(
      stringr::str_detect(machine, "Office-Mac") == TRUE, "OfficeMac",
      ifelse(machine == "ES127", "ES127", machine)))) %>%
  as.character()

# Environmental parameters ------------------------------------------------
setwd("/Volumes/Office-SSD/Astronomy/astro-tools")
subs <- "/Volumes/Office-SSD/Astronomy/Astrophotography"
subdirs <- subs %>%
  list.dirs(recursive = FALSE) %>%
  stringr::str_subset("Dark Library", negate = TRUE) # drops darks



# Functions ---------------------------------------------------------------
connectDB <- function () {
  username <- Sys.getenv("username")
  password <- Sys.getenv("password")
  tryCatch({
    if (length(username) != 0 & length(password) != 0) {
      dbConnect(RPostgres::Postgres(), dbname = "astroDB", 
                host = "100.85.227.75", port = 5432, user = username, 
                password = password, sslmode = "prefer")
    }
    else {
      dbConnect(RPostgres::Postgres(), dbname = "astroDB", 
                host = "100.85.227.75", port = 5432, user = getPass::getPass("Username"), 
                password = getPass::getPass("Password"), sslmode = "prefer")
    }
  }, error = function(cond) {
    message("Can't connect to Raspberry PI")
    message(cond)
    return(NA)
  })
}


# This will change the file paths in the metadata files
# to match the /Volumes/Office-SSD location
changeMetaPath <- function(file, subsDf = subsDf) {
  
  # change only if necessary
  df <- readr::read_csv(file) 

  name_order <- colnames(df)
  
  loc <- df$FilePath[1]
  
  if ((stringr::str_detect(loc, "c:/users") == TRUE) |
      (stringr::str_detect(loc, "C:/Users") == TRUE)) {
  
    df <- df %>%
      mutate(Filename = basename(FilePath)) %>%
      mutate(Object = basename(dirname(dirname(FilePath)))) 
    
      
    subsList <- subsDf %>%
      dplyr::filter(Object == df$Object[1]) %>%
      dplyr::filter(Filename %in% df$Filename) %>%
      select(FilePathCorrect = FilePath, Filename)
    
    df %>%
      left_join(subsList, "Filename") %>%
      select(-FilePath, -Filename, -Object) %>%
      rename(FilePath = FilePathCorrect) %>%
      select(all_of(name_order)) %>% # re-order columns to match original
      readr::write_csv(file) # write out the new file paths to metadata file
  }
}


# Inventory subs ----------------------------------------------------------
subsDf <- data.frame(FilePath = list.files(subdirs, all.files = TRUE,
                                             recursive = TRUE, full.names = TRUE,
                                             pattern = ".fit")) %>%
  mutate(Filename = basename(FilePath)) %>%
  mutate(Object = basename(dirname(dirname(FilePath)))) %>%
  mutate(Date = stringr::str_extract(FilePath, "\\d{4}-\\d{2}-\\d{2}")) %>%
  mutate(DurationSubs = ifelse(stringr::str_detect(Filename, "_30.00") == TRUE, 30, ifelse(
    stringr::str_detect(Filename, "_60.0") == TRUE, 60, ifelse(
      stringr::str_detect(Filename, "_120.0") == TRUE, 120, ifelse(
        stringr::str_detect(Filename, "_180.0") == TRUE, 180, ifelse(
          stringr::str_detect(Filename, "_300.0") == TRUE, 300, ifelse(
            stringr::str_detect(Filename, "_600.0") == TRUE, 600, NA_real_))))))) %>%
  mutate(FilterSubs = ifelse(stringr::str_detect(Filename, "_L_") == TRUE, "L", ifelse(
    stringr::str_detect(Filename, "_R_") == TRUE, "R", ifelse(
      stringr::str_detect(Filename, "_G_") == TRUE, "G", ifelse(
        stringr::str_detect(Filename, "_B_") == TRUE, "B", ifelse(
          stringr::str_detect(Filename, "_H_") == TRUE, "H", ifelse(
            stringr::str_detect(Filename, "_S_") == TRUE, "S", ifelse(
              stringr::str_detect(Filename, "_O_") == TRUE, "O", ifelse(
                stringr::str_detect(Filename, "UVIR") == TRUE, "UVIR", ifelse(
                  stringr::str_detect(Filename, "_HO_") == TRUE, "HO", ""))))))))))
                

# Inventory metadata ------------------------------------------------------
metadataFiles <- list.files(subdirs, pattern = "ImageMetaData.csv",
                            full.names = TRUE, recursive = TRUE, all.files = TRUE)


# Update the metadata file with the correct file path information
# they are listed as "c:/users/Brian Carter" and I want them on the NAS
sapply(metadataFiles, changeMetaPath, subsDf = subsDf)

# nrow = 4740 at development - August 15th 2025
metadata <- lapply(metadataFiles, readr::read_csv) %>%
  Reduce(function(x,y) bind_rows(x, y), .) %>%
  mutate(Object = basename(dirname(dirname(FilePath)))) %>%
  mutate(Filename = basename(FilePath)) %>%
  mutate(Date = stringr::str_remove(basename(dirname(FilePath)), paste0(Object, "_")) )%>%
  full_join(subsDf %>% 
              select(FilePathSubs = FilePath, Filename, FilterSubs, DurationSubs) %>% 
              mutate(inSubs = TRUE), 
            c("Filename")) %>%
  mutate(Status = ifelse(is.na(inSubs), "Excluded", ifelse(
    is.na(Date), "Missing Metadata", "Included"))) %>%
  mutate(Object = basename(dirname(dirname(FilePath))))  %>%
  mutate(FilePath = ifelse(is.na(FilePath), FilePathSubs, FilePath)) %>%
  mutate(FilterName = ifelse(is.na(FilterName), FilterSubs, FilterName)) %>%
  mutate(Duration = ifelse(is.na(Duration), DurationSubs, Duration)) %>%
  select(Object, Date, Filename, ExposureStart, FilterName, Duration, CameraTemp,
         Gain, ADUMean, DetectedStars, HFR, FWHM, Eccentricity, GuidingRMSArcSec,
         FocuserPosition, FocuserTemp, RotatorPosition, Status)
  

astroDB <- connectDB()
dbWriteTable(astroDB, "astroSubs", metadata, overwrite = TRUE)
dbDisconnect(astroDB)

rm(list = ls())



