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
      ifelse(machine == "APERTURA-75", "TELESCOPE", ifelse(
        machine == "ES127", "TELESCOPE", machine)
      )))) %>%
  as.character()



# Environmental parameters ------------------------------------------------

  setwd("C:/users/bcart/Astronomy/astro-tools/postprocessing")
  src <- file.path("C:/users/bcart/Astronomy/astro-tools/postprocessing")
  cameraSrc <- "c:/users/bcart/astronomy/ASI2600MM/Subs"
  wbpp <- "C:/Users/bcart/astronomy/astro-tools/postprocessing/wbpp.bat"
  source("functions/functions.R")

  transfer <- "Z:/Astronomy/subs"


# Run the scripts ---------------------------------------------------------


folders <- list.dirs(cameraSrc, recursive = FALSE, full.names = TRUE)
folders <- folders[-grep("flats", folders)]
data.frame(dir = list.dirs(cameraSrc, recursive = TRUE, full.names = TRUE)) %>%
  dplyr::filter(!stringr::str_detect(dir, "darks") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "flats") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "metadata") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "calibrated") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "logs") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "SubFrameSelected") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "lights") == TRUE) %>%
  dplyr::filter(dir %in% setdiff(dir, folders)) %>%
  dplyr::filter(dir %in% setdiff(dir, cameraSrc)) %>%
  pull(dir) %>%
  lapply(cleanup)



# reinitialize the wbpp script for next time!
file.remove(wbpp)
con <- file(wbpp)
writeLines("", con)
close(con)



# Logs --------------------------------------------------------------------

glue::glue("{src}/logFiles.R") %>% source()



# transfer over to aria-bot
dirs_to_transfer <- list.dirs(cameraSrc, recursive = FALSE) %>%
  stringr::str_remove("c:/users/bcart/astronomy/ASI2600MM/Subs/flats")

dirs_to_transfer <- dirs_to_transfer[dirs_to_transfer != ""]


returnStatus <- sapply(dirs_to_transfer, function(x) {
  file.copy(x, transfer, recursive = TRUE, overwrite = FALSE)
})

for (i in 1:length(returnStatus)) {
  if (returnStatus[i] == TRUE)  {unlink(dirs_to_transfer[i], recursive = TRUE, force = TRUE)}
}

png <- list.files(file.path(cameraSrc), pattern = ".png", recursive = TRUE, full.names = TRUE) %>%
  c(list.files(file.path(cameraSrc), pattern = ".json", recursive = TRUE, full.names = TRUE))
sapply(png, file.remove)

unlink("c:/users/bcart/astronomy/ASI2600MM/Subs/flats", recursive = TRUE, force = TRUE)
