

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


astroDB <- connectDB()
old <- tbl(astroDB, "astroSubs") %>%
  collect()


# compare the database table to the new version
# If the same: stop
# If there is new stuff: write it to the database
add_to_database <- metadata %>%
  anti_join(old)
if (nrow(add_to_database) > 0) {
  message("Adding new metadata to the database")
  dbAppendTable(astroDB, "astroSubs", add_to_database)
} else {
  message("No new metadata to add to the database")
}

# Add a log
log <- data.frame(
  Timestamp = Sys.time(),
  Action = "Inventory metadata",
  Status = ifelse(nrow(add_to_database) > 0, "Added new metadata", "No new metadata")
)

dbAppendTable(astroDB, "astroDBLog", log)
dbDisconnect(astroDB)


rm(list = ls())



