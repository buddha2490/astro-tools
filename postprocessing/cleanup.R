library(dplyr)
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
      ifelse(machine == "BDC-AM5", "ES127", ifelse(
        machine == "mele-astro", "ES127", machine)
      )))) %>%
  as.character()



# Environmental parameters ------------------------------------------------

if (os == "Windows" & debug == FALSE) {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing") # execpath
  src <- file.path("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing") # execpath
  cameraSrc <- "c:/users/Brian Carter/astronomy/ASI2600MM/ES127" # subspath
  transfer <- "z:/In Progress" # transfer path
  wbpp <- "C:/users/Brian Carter/Astronomy/astro-tools/postprocessing/wbpp.bat" # wbpp
} else if  (os == "Windows" & debug == TRUE) {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing") # execpath
  src <- file.path("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing") # execpath
  cameraSrc <- "D:/NAS/testing/subs"
  transfer <- "z:/In Progress" # transfer path
  wbpp <- file.path(src, "wbpp.sh")
} else if (machine == "OfficeMac") {
  setwd("/Volumes/Office-SSD/Astronomy/astro-tools/postprocessing")
  src <- file.path("/Volumes/Office-SSD/Astronomy/astro-tools/postprocessing")
  cameraSrc <- "/Volumes/Office-SSD/Astronomy/testing/data/subs"
  transfer <- "/Volumes/Office-SSD/Astronomy/In Progress"
  wbpp <- file.path(src, "wbpp.sh")
} else if (machine == "MBP14") {
  setwd("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  src <- file.path("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  cameraSrc <- "/Volumes/Office-SSD/Astronomy/testing/data/subs"
  transfer <- "/Volumes/Office-SSD/Astronomy/In Progress"
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

files_to_transfer <- list.files(cameraSrc, recursive = FALSE, full.names = TRUE)  %>%
  c(list.files(file.path(cameraSrc), pattern = ".json", recursive = TRUE, full.names = TRUE)) %>%
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

png <- list.files(file.path(cameraSrc), pattern = ".png", recursive = TRUE, full.names = TRUE) %>%
  c(list.files(file.path(cameraSrc), pattern = ".json", recursive = TRUE, full.names = TRUE))
sapply(png, file.remove)
