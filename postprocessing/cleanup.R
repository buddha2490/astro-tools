library(dplyr)
library(magrittr)
library(tidyr)


# First run the report

source("C:/Users/bcart/Astronomy/astro-tools/postprocessing/logFiles.R")

src <- file.path("C:/users/bcart/Astronomy/astro-tools/postprocessing")

camera <- "c:/users/bcart/astronomy/ASI2600MM/ES127"

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
  l <- ifelse(length(flats[grep("L", flats)]) >0, 1, 0)
  r <- ifelse(length(flats[grep("R", flats)]) >0, 1, 0)
  g <- ifelse(length(flats[grep("G", flats)]) >0, 1, 0)
  b <- ifelse(length(flats[grep("B", flats)]) >0, 1, 0)
  h <- ifelse(length(flats[grep("H", flats)]) >0, 1, 0)
  s <- ifelse(length(flats[grep("S", flats)]) >0, 1, 0)
  o <- ifelse(length(flats[grep("O", flats)]) >0, 1, 0)

  totalFlats <- sum(l, r, g, b, h, s, o)

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
