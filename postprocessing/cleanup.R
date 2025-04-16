library(dplyr)
library(magrittr)
library(tidyr)


src <- file.path("C:/users/bcart/Astronomy/astro-tools/postprocessing")

camera <- "c:/users/bcart/astronomy/ASI2600MC/ES127"

objects <- list.dirs(camera, recursive = FALSE, full.names = TRUE) 

cleanup <- function(myObject) {
  
  sessions <- data.frame(sessions = list.dirs(myObject, recursive = FALSE, full.names = TRUE)) %>%
    mutate(object = basename(myObject)) %>%
    mutate(folder = basename(sessions)) %>%
    mutate(dates = stringr::str_replace(folder, paste0(object, "_"), ""))  %>%
    filter(dates == Sys.Date() - 1) %>%
    pull(sessions)
  
  flatDir <- file.path(sessions, "flats")
  masterDir <- file.path(flatDir, "master")
  file.path(flatDir, "calibrated") %>% unlink(recursive = TRUE, force = TRUE)
  file.path(flatDir, "logs") %>% unlink(recursive = TRUE, force = TRUE)
  
  # Count the types of flats
  flats <- list.files(flatDir, pattern = "fits")
  uvir <- ifelse(length(flats[grep("UVIR", flats)]) >0, 1, 0)
  lpro <- ifelse(length(flats[grep("LPRO", flats)]) >0, 1, 0)
  ho <- ifelse(length(flats[grep("HO", flats)]) >0, 1, 0)
  so <- ifelse(length(flats[grep("SO", flats)]) >0, 1, 0)
  totalFlats <- sum(uvir, lpro, ho, so)

  # Count the stacked flats
  nMaster <- list.files(masterDir, pattern = ".xisf", full.names = TRUE)
 
  lapply(nMaster, function(x) file.copy(from = x, to = sessions))
  print(nMaster); print(sessions);

  xisf <- list.files(sessions, pattern = "masterFlat")
  if (length(xisf) != 0) {
    unlink(flatDir, recursive = TRUE, force = TRUE)
  }

}

objects %>% lapply(cleanup)
file.remove("C:/users/bcart/Astronomy/astro-tools/postprocessing/wbpp.bat")
con <- file("C:/users/bcart/Astronomy/astro-tools/postprocessing/wbpp.bat")
writeLines("", con)
close(con)
