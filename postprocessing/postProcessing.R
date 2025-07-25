library(dplyr)
library(magrittr)




os <- Sys.info()["sysname"]
machine <- Sys.info()["nodename"]

os <- ifelse(os == "Darwin", "Mac", "Windows") %>% as.character()
machine <- ifelse(machine == "BRIANC-MacUS.attlocal.net", "MBP13",
                  ifelse(machine == "Brians-MBP.attlocal.net", "MBP14",
                         ifelse(machine == "ES127", "ES127", machine))) %>%
  as.character()








# Environmental parameters ------------------------------------------------

if (machine == "ES127") {
  setwd("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  src <- file.path("C:/users/Brian Carter/Astronomy/astro-tools/postprocessing")
  camera <- "c:/users/Brian Carter/astronomy/ASI2600MM/ES127"
  darks <- file.path(camera, "../Dark Library/") 
  source("functions/functions.R")
  
  
} else {
  setwd("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  src <- file.path("/Users/briancarter/Astronomy/astro-tools/postprocessing")
  camera <- file.path("/Users/briancarter/Astronomy/testing")
  username <- Sys.getenv("username")
  password <- Sys.getenv("password")
  es127 <- paste0("open 'smb://", username, ":", password, "@", "ES127", "/Users/Brian Carter/Astronomy/ASI2600MM/Dark Library'")
  system(es127) # mini computer connection
  darks <- file.path("/Volumes/Dark Library")
  
  source("functions/functions.R")
  
}
                  
                  


# Run the scripts ---------------------------------------------------------

# Past versions could only handle 1 night of data at a time and would fail if the object fold had two nights
# This is fixed now - 25July2025
folders <- list.dirs(camera, recursive = FALSE, full.names = TRUE)
objects <- list.dirs(camera, recursive = TRUE, full.names = TRUE) %>%
  stringr::str_remove("checkFits") %>%
  stringr::str_remove("flats") %>%
  stringr::str_remove("metadata") %>%
  setdiff(folders)  %>%
  setdiff(camera)  # remove the home folder, keep session-specific folders  

objects %>%  lapply(bulkRename)
objects %>% lapply(processObjects)


# go ahead and run it if on the dev rig
if (os == "Mac") {
  glue::glue("/Users/briancarter/Astronomy/astro-tools/postprocessing/wbpp.sh") %>% system()
}



  









