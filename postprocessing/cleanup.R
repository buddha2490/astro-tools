library(dplyr)
library(magrittr)
library(tidyr)



os <- Sys.info()["sysname"]
machine <- Sys.info()["nodename"]

os <- ifelse(os == "Darwin", "Mac", "Windows") %>% as.character()
machine <- ifelse(machine == "BRIANC-MacUS.attlocal.net", "MBP13",
                  ifelse(machine == "Brians-MBP.attlocal.net", "MBP14",
                         ifelse(machine == "Office-Mac.attlocal.net", "OfficeMac", 
                            ifelse(machine == "ES127", "ES127", machine)))) %>%
  as.character()



# Environmental parameters ------------------------------------------------

if (machine == "ES127") {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  src <- file.path("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  cameraSrc <- "c:/users/Brian Carter/astronomy/ASI2600MM/ES127"
  transfer <- "z:/transfer"
  wbpp <- "C:/users/Brian Carter/Astronomy/astro-tools/postprocessing/wbpp.bat"
} else {
  setwd("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  src <- file.path("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  cameraSrc <- "/Volumes/Astro-SSD/testing/data/subs"
  transfer <- "/Volumes/Astro-SSD/transfer"
  wbpp <- file.path(src, "wbpp.sh")
}

source("functions/functions.R")



# Run the scripts ---------------------------------------------------------

list.dirs(cameraSrc, recursive = FALSE, full.names = TRUE) %>%
  lapply(cleanup, os = os, machine = machine)

# reinitialize the wbpp script for next time!
file.remove(wbpp)
con <- file(wbpp)
writeLines("", con)
close(con)



# Logs --------------------------------------------------------------------

glue::glue("{src}/logFiles.R") %>% source()


# transfer over to astro-ssd
# 10 minutes for 355 files
dirs_to_transfer <- list.dirs(cameraSrc, recursive = FALSE) %>%
  stringr::str_remove("Dark Library")

files_to_transfer <- list.files(cameraSrc, recursive = FALSE, full.names = TRUE) %>%
  setdiff(dirs_to_transfer)

sapply(files_to_transfer, function(x) {
  file.copy(x, transfer, recursive = TRUE)
})


returnStatus <- sapply(dirs_to_transfer, function(x) {
  file.copy(x, transfer, recursive = TRUE)
})

for (i in 1:length(returnStatus)) {
  if (returnStatus[i] == TRUE) {unlink(dirs_to_transfer[i], recursive = TRUE, force = TRUE)}
}

png <- list.files(file.path(cameraSrc), pattern = ".png", recursive = TRUE, full.names = TRUE)
sapply(png, file.remove)
