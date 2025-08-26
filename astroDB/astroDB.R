

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
"astroDB/functions/functions.R" %>% source()
# Inventory subs ----------------------------------------------------------




subsDf <- subsInventory()
                

# Inventory metadata ------------------------------------------------------
metadataFiles <- list.files(subdirs, pattern = "ImageMetaData.csv",
                            full.names = TRUE, recursive = TRUE, all.files = TRUE)


# Update the metadata file with the correct file path information
# they are listed as "c:/users/Brian Carter" and I want them on the NAS
invisible(capture.output(
  suppressMessages(
    suppressWarnings({
      sapply(metadataFiles, changeMetaPath, subsDf = subsDf)
    })
  )
))

# nrow = 4740 at development - August 15th 2025
invisible(capture.output(
  suppressMessages(
metadata <- lapply(metadataFiles, readr::read_csv) %>%
  Reduce(function(x,y) bind_rows(x, y), .) %>%
  processMetadata() %>%
  fixMissingMeta()
  )
)) 


metadata <- metadata %>%
  dplyr::filter(!is.na(Filename))

astroDB <- connectDB()
old <- tbl(astroDB, "astroSubs") %>%
  collect()

new_rows <- nrow(metadata) - nrow(old)


if (new_rows > 0) {
  message <- glue::glue("There were {new_rows} new images added") 
  log <- data.frame(
    Timestamp = Sys.time(),
    Action = "Inventory added",
    Status = message)

  dbWriteTable(astroDB, "astroSubs", metadata, overwrite = TRUE)
  dbAppendTable(astroDB, "astroDBLog", log)
  message(message)
  
}
if (new_rows < 0) {
  message <- glue::glue("There were {abs(new_rows)} images removed from the database") 
  log <- data.frame(
    Timestamp = Sys.time(),
    Action = "Inventory removed",
    Status = message)
  dbWriteTable(astroDB, "astroSubs", metadata, overwrite = TRUE)
  dbAppendTable(astroDB, "astroDBLog", log)
  message(message)
  
}

if (new_rows == 0) {
  message("No new images to add to the database")
  log <- data.frame(
    Timestamp = Sys.time(),
    Action = "Inventory checked",
    Status = "No new images to add")
  dbAppendTable(astroDB, "astroDBLog", log)
  
}

dbDisconnect(astroDB)


rm(list = ls())



