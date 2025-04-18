# daily cronjob Pixinsight

library(dplyr)
library(magrittr)
source("/Users/briancarter/astro-tools/buildDB.R")


wait <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}
wbppScripter <- function(dir, outputdir = dir) {
  
  app <- '"/Applications/PixInsight/PixInsight.app/Contents/MacOS/PixInsight"  -n --automation-mode -r='
  
  js1 <- '"/Applications/Pixinsight/src/scripts/BatchPreprocessing/FBPP.js,automationMode=true,outputDirectory='
  
  outputdir <- outputdir
  
  dir <- glue::glue(dir, '"')
  
  js2 <- '--force-exit'
  
  foo <- glue::glue("{app}{js1}{outputdir},dir={dir} {js2}")
  
  wbpp <- file(glue::glue("{outputdir}/wbpp.sh"), "a")
  write(foo, wbpp, append = TRUE)
  close(wbpp)
  
}

# Process the data in the Transfer folder
one <- TRUE
if (one) {
homePath <- "/Volumes/Astro-SSD"
path <- glue::glue("{homePath}/Transfer")
objects <- list.dirs(path, recursive = FALSE, full.names = TRUE)
subdirs <- list.dirs(path, recursive = TRUE, full.names = TRUE)
subdirs <- subdirs[!subdirs %in% c(objects, path)]
subdirs <- subdirs[!grepl("master", subdirs, ignore.case = TRUE)] # Exclude master directories
subdirs <- subdirs[!grepl("checkFits", subdirs, ignore.case = TRUE)] # Exclude checkFits directories"
}


if (length(subdirs) > 0) {
for (i in 1:length(subdirs)) {
  
  # First write the WBPP script and execute it
  wbppScripter(dir = subdirs[i], outputdir = subdirs[i])
  glue::glue("/bin/bash {subdirs[i]}/wbpp.sh") %>% system() 
  

  # Process the voluminous output
  object <- subdirs[i] %>% basename() # format: objects_date, m1_2024-01-01
  master <- glue::glue("{subdirs[i]}/master") # master stack location
  
  
  # Remove unnecessary directories
  junk <- list.dirs(subdirs[i], recursive = FALSE) # logs, registered, calibrated
  junk <- junk[!junk %in% master] 
  junk <- junk[!grepl("checkFits", junk, ignore.case = TRUE)] # Exclude checkFits directories
  
  
  # Uncomment if you want to keep these folders
  # junk <- junk[!junk %in% glue::glue("{subdirs[i]}/logs")]
  # junk <- junk[!junk %in% glue::glue("{subdirs[i]}/registered")]
  # junk <- junk[!junk %in% glue::glue("{subdirs[i]}/calibrated")]
  
  # Remove that junk
  lapply(junk, unlink, recursive = T, force = T)
  
  
  # Master files - can have multiple if I have multiple filters, but typically I have one a night
  masterFiles <- list.files(master, full.names = TRUE)
  autocropped <- masterFiles[grep("autocrop", masterFiles, ignore.case = TRUE)] # this is the only one I want to keep
  
  # Rename and move to the transfer directory
  newname <- glue::glue("{object}.xisf")
  file.rename(autocropped, glue::glue("{master}/{newname}"))
  file.copy(glue::glue("{master}/{newname}"), glue::glue("{path}/"))
  
  # remove the master directory
  glue::glue("{master}") %>% unlink(recursive = TRUE, force = TRUE) 
  
  # Remove the WBPP.sh file 
  if (file.exists(glue::glue("{subdirs[i]}/wbpp.sh"))) {
  file.remove(glue::glue("{subdirs[i]}/wbpp.sh"))
  }
  
}
}

rm(homePath, path, objects, subdirs)


# Functions() -------------------------------------------------------------

