# AstroDB
library(dplyr)
library(DBI)
library(stringr)
library(magrittr)
library(tidyr)

# Environmental parameters ------------------------------------------------
# subs location differs per machine — use whichever path exists
subs <- Filter(dir.exists, c(
  path.expand("~/Astronomy/subs"),                        # Texas machine
  "/Volumes/Office-SSD/Astronomy/In Progress"             # this Mac
))[1]
if (is.na(subs)) stop("No subs directory found on this machine.")


# Functions ---------------------------------------------------------------
"astroDB/functions/functions.R" %>% source()


# Fix some filenames, strips _a from the file
result <- stripASuffix(subs)

# Build inventory from SubFrameSelected folders ---------------------------
metadata <- buildInventory(subs) %>%
  dplyr::filter(!is.na(Filename))

# Diff against existing astroSubs and update -------------------------------
astroDB <- connectDB()
old <- tbl(astroDB, "astroSubs") %>% collect() %>%
  dplyr::filter(stringr::str_detect(Filename, ".fit") == FALSE)


# Remove anything in metadata that was already written to the database
new <- metadata %>%
  anti_join(old, "Filename")

new_rows <- new %>% nrow()



if (new_rows > 0) {
  msg <- glue::glue("There were {new_rows} new images added")
  log <- data.frame(Timestamp = Sys.time(), Action = "Inventory added", Status = msg)
  dbAppendTable(astroDB, "astroSubs", metadata)
  dbAppendTable(astroDB, "astroDBLog", log)
  message(msg)
}

dbDisconnect(astroDB)

rm(list = ls())
