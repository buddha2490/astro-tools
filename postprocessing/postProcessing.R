library(dplyr)
library(magrittr)





os <- Sys.info()["sysname"]
machine <- Sys.info()["nodename"]

# We are always working on the "TELESCOPE", this is a relic of development
os <- ifelse(os == "Darwin", "Mac", "Windows") %>% as.character()
machine <- ifelse(
  stringr::str_detect(machine, "BRIANC-MacUS") == TRUE, "MBP13", ifelse(
    stringr::str_detect(machine, "Brians-MacBook-Pro") == TRUE, "MBP14", ifelse(
      stringr::str_detect(machine, "Office-Mac") == TRUE, "OfficeMac",
      ifelse(machine == "APERTURA-75", "TELESCOPE", ifelse(
        machine == "MELE-ASTRO", "TELESCOPE", machine)
      )))) %>%
  as.character()



# Environmental parameters ------------------------------------------------


if (machine == "TELESCOPE") {
  setwd("C:/users/bcart/Astronomy/astro-tools/postprocessing")
  src <- file.path("C:/users/bcart/Astronomy/astro-tools/postprocessing")
  camera <- "c:/users/bcart/astronomy/ASI2600MM/Subs"
  darks <- file.path(camera, "../Dark Library/") 
  flatsDir <- file.path(camera, "flats") # new - May 2026
  source("functions/functions.R")
}



#########   Process my flats for the night
rotatorAngle <- flatsDir %>% list.dirs(recursive = FALSE) %>% basename()
lapply(rotatorAngle, wbppFlats)
glue::glue("C:/users/bcart/Astronomy/astro-tools/postprocessing/wbpp.bat") %>% sys::exec_wait()
lapply(rotatorAngle, moveFlats)
lapply(rotatorAngle, function(x) renameFlats(x, flatsDir = flatsDir))


# Run the scripts ---------------------------------------------------------


folders <- list.dirs(camera, recursive = FALSE, full.names = TRUE)
folders <- folders[-grep("flats", folders)]
objects <- data.frame(dir = list.dirs(camera, recursive = TRUE, full.names = TRUE)) %>%
  dplyr::filter(!stringr::str_detect(dir, "darks") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "flats") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "metadata") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "calibrated") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "logs") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "SubFrameSelected") == TRUE) %>%
  dplyr::filter(!stringr::str_detect(dir, "lights") == TRUE) %>%
  dplyr::filter(dir %in% setdiff(dir, folders)) %>%
  dplyr::filter(dir %in% setdiff(dir, camera)) %>%
  pull(dir)



objects %>%  lapply(bulkRename)

# MBP14 - /volumes/office-ssd/astronomy/testing: 271.737 elapsed
system.time({
objects %>% lapply(processObjects)
})




  









