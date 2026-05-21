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
# Compare against EVERY existing filename. Do not filter by extension here:
# excluding ".fit" rows hides already-stored .fits subs from the anti-join,
# so they get re-appended on every run (duplicates).
old <- tbl(astroDB, "astroSubs") %>%
  collect() %>%
  distinct(Filename)

# Remove anything in metadata that was already written to the database
new <- metadata %>%
  anti_join(old, "Filename")

new_rows <- new %>% nrow()

if (new_rows > 0) {
  msg <- glue::glue("There were {new_rows} new images added")
  log <- data.frame(Timestamp = Sys.time(), Action = "Inventory added", Status = msg)
  dbAppendTable(astroDB, "astroSubs", new)
  dbAppendTable(astroDB, "astroDBLog", log)
  message(msg)
}

dbDisconnect(astroDB)

rm(list = ls())
