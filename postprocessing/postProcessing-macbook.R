library(dplyr)
library(magrittr)


# Functions ---------------------------------------------------------------


wait <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}
wbppScripter <- function(dir, outputdir = dir) {
  
  app <- '"/Applications/PixInsight/PixInsight.app/Contents/MacOS/PixInsight"  -n --automation-mode -r='
  
  js1 <- '"/Applications/Pixinsight/src/scripts/BatchPreprocessing/WBPP.js,automationMode=true,outputDirectory='
  
  outputdir <- outputdir
  
  dir <- glue::glue(dir, '"')
  
  js2 <- '--force-exit'
  
  foo <- glue::glue("{app}{js1}{outputdir},dir={dir} {js2}")
  
  wbpp <- file(glue::glue("{outputdir}/wbpp.sh"), "a")
  write(foo, wbpp, append = TRUE)
  close(wbpp)
  
}
cleanup <- function(dir) {
  
  # my lights
  lightPath <- file.path(dir, "..") %>% normalizePath
  
  # masterPath 
  masterPath <- file.path(dir, "master")
  
  # copy MasterFlat
  masterFlat <- list.files(masterPath, pattern = "masterFlat", ignore.case = TRUE,
                           full.names = TRUE)
  file.copy(masterFlat, lightPath, overwrite = TRUE)
  
  test <- list.files(lightPath, pattern = "xisf")
  flat <- test[grep("masterFlat", test, ignore.case = TRUE)]
  if (length(flat) != 0) {
    unlink(dir, recursive = TRUE, force = TRUE)
  }
  
  bias <- test[grep("masterBias", test, ignore.case = TRUE)]
  if (length(bias) != 0) {
    file.remove(file.path(lightPath, bias))
  }
  
  file.rename("/Users/briancarter/astro-tools/wbpp.sh",
              glue::glue("/Users/briancarter/astro-tools/wbpp_{Sys.time()}.sh"))
  
}  
wbppFlats <- function(dir) {
  
  # my lights
  lightPath <- file.path(dir, "..") %>% normalizePath()
  
  # make sure the masterBias is there, it usually is
  file.copy(masterBias, dir, overwrite = TRUE)
  
  
  # create new log file
  logfile <- glue::glue("/Volumes/Astro-SSD/logs/WBPPlog_{Sys.Date()}.log")
  xisfFiles <- list.files(dir, pattern = "xisf")
  fitFiles <- list.files(dir, pattern = "fit")
  
  allFiles <- list.files(dir)
  masterFlat <- allFiles[grep("masterFlat", allFiles)]
  
  
  if (length(masterFlat) != 0) {
    
    lapply(masterFlat, function(x) {
      file.copy(from = file.path(dir, x), to = file.path(dir, ".."))
    })
    cleanup(dir)
    
  } else {
    
    allFiles2 <- list.files(dir, pattern = "fit")
    
    if (length(allFiles2) > 0) {
    wbppScripter(dir, dir) 
    masterPath <- file.path(dir, "master")
    dir.create(masterPath, showWarnings = FALSE)
    wbppLoop <<- c(wbppLoop, dir)
  } 
  }
}




# Process flats - already done --------------------------------------------


homePath <- "/Volumes/Astro-SSD"
path <- glue::glue("{homePath}/Nebulae")
subdirs <- list.dirs(path, recursive = TRUE, full.names = TRUE)
flatdirs <- subdirs[grepl("flats", subdirs, ignore.case = TRUE)]
masterBias <- file.path("/Volumes/Astro-SSD/Dark Library/-10/gain100/masterBias_gain100.xisf")
wbppLoop <- NULL  # vector that holds the names of the final directories I need to work on


# Run wbpp to generate the wbpp script
# flat directories that were already done and just needed a flat file moved, those are cleaned up
for (i in 1:length(flatdirs)) {
  wbppFlats(flatdirs[i])
}

# run it
system("/bin/bash /Users/briancarter/astro-tools/wbpp.sh", wait = TRUE, intern = TRUE)
lapply(wbppLoop, cleanup)




# Functions ---------------------------------------------------------------

dir <- "/Volumes/Astro-SSD/Messier Globs/M68"
outputdir <- file.path("/Volumes/Astro-SSD/Stacks")


stackObject <- function(dir, outputdir = dir) {
  
  objectName <- basename(dir)
  wbppScripter(dir, outputdir)
  
  glue::glue("/bin/bash {outputdir}/wbpp.sh") %>%
    system(wait = TRUE, intern = TRUE)
  
}
stackObject(dir, outputdir)




