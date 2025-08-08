library(dplyr)
library(magrittr)




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

if (machine == "ES127" & debug != TRUE) {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  src <- file.path("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  camera <- "c:/users/Brian Carter/astronomy/ASI2600MM/ES127"
  darks <- file.path(camera, "../Dark Library/") 
  source("functions/functions.R")
  
  
} else if (machine == "ES127" & debug == TRUE) {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  src <- file.path("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  camera <- "D:/NAS/testing/subs"
  darks <-  "c:/users/Brian Carter/astronomy/ASI2600MM/Dark Library/"
  source("functions/functions.R")
  
} else if (machine == "OfficeMac") {
  setwd("/Volumes/Office-SSD/Astronomy/astro-tools/postprocessing")
  src <- file.path("/Volumes/Office-SSD/Astronomy/astro-tools/postprocessing")
  camera <- file.path("/Volumes/Office-SSD/Astronomy/testing/data/subs")
  username <- Sys.getenv("username")
  password <- Sys.getenv("password")
  darks <- file.path("/Volumes/Office-SSD/Astronomy/ASI2600MM/Dark Library")
  source("functions/functions.R")
  
} else if (machine == "MBP14") {
  setwd("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  src <- file.path("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  camera <- file.path("/Volumes/Office-SSD/Astronomy/testing/data/subs")
  username <- Sys.getenv("username")
  password <- Sys.getenv("password")
  darks <- file.path("/Volumes/Office-SSD/Astronomy/ASI2600MM/Dark Library")
  source("functions/functions.R")
}
                  
                  


# Run the scripts ---------------------------------------------------------

# Past versions could only handle 1 night of data at a time and would fail if the object fold had two nights
# This is fixed now - 25July2025
# Note: 26July - R is picking up the xisf file as directories
folders <- list.dirs(camera, recursive = FALSE, full.names = TRUE)
objects <- data.frame(dir = list.dirs(camera, recursive = TRUE, full.names = TRUE)) %>%
  filter(!stringr::str_detect(dir, "checkFits")) %>%
  filter(!stringr::str_detect(dir, "flats")) %>%
  filter(!stringr::str_detect(dir, "metadata")) %>%
  filter(!stringr::str_detect(dir, "calibrated")) %>%
  filter(!stringr::str_detect(dir, "logs")) %>%
  filter(!stringr::str_detect(dir, "Flat_BIN")) %>%
  filter(dir %in% setdiff(dir, folders)) %>%
  filter(dir %in% setdiff(dir, camera)) %>%
  pull(dir)



objects %>%  lapply(bulkRename)
objects %>% lapply(processObjects)


# go ahead and run it if on the dev rig
if (os == "Mac") {
  glue::glue("/Volumes/Office-SSD/Astronomy/astro-tools/postprocessing/wbpp.sh") %>% system()
}


# There are some weird errors, but everything runs with below.
if (os == "Windows" & debug == TRUE) {
  glue::glue("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing/wbpp.bat") %>% sys::exec_wait()
}

  









