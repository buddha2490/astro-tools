

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
message(glue::glue("Found {nrow(subsDf)} files on disk"))

# Inventory metadata ------------------------------------------------------
metadataFiles <- list.files(subdirs, pattern = "ImageMetaData.csv",
                            full.names = TRUE, recursive = TRUE, all.files = TRUE)


# Read all CSV metadata and cross-reference with actual files on disk.
# Only files that physically exist in astrophotography/ will be included.
invisible(capture.output(
  suppressMessages(
    suppressWarnings({
      metadata <- lapply(metadataFiles, readr::read_csv) %>%
        Reduce(function(x,y) bind_rows(x, y), .) %>%
        processMetadata()
    })
  )
))

metadata <- metadata %>%
  dplyr::filter(!is.na(Filename))

astroDB <- connectDB()
old <- tryCatch(
  tbl(astroDB, "astroSubs") %>% collect(),
  error = function(e) data.frame()
)

old_count <- nrow(old)
new_count <- nrow(metadata)
diff <- new_count - old_count

# Always refresh the table with current file-system truth
dbExecute(astroDB, 'DELETE FROM "astroSubs"')
dbAppendTable(astroDB, "astroSubs", metadata)

if (diff > 0) {
  msg <- glue::glue("There were {diff} new images added (total: {new_count})")
  log <- data.frame(Timestamp = Sys.time(), Action = "Inventory added", Status = msg)
} else if (diff < 0) {
  msg <- glue::glue("There were {abs(diff)} images removed (total: {new_count})")
  log <- data.frame(Timestamp = Sys.time(), Action = "Inventory removed", Status = msg)
} else {
  msg <- glue::glue("No change in inventory (total: {new_count})")
  log <- data.frame(Timestamp = Sys.time(), Action = "Inventory checked", Status = msg)
}

dbAppendTable(astroDB, "astroDBLog", log)
message(msg)

dbDisconnect(astroDB)


rm(list = ls())



