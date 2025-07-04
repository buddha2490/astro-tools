# daily cronjob Pixinsight

library(dplyr)
library(magrittr)

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


homePath <- "/Volumes/Astro-SSD/"
path <- glue::glue("{homePath}/Transfer/foo")
objects <- list.dirs(path, recursive = FALSE, full.names = TRUE)
subdirs <- list.dirs(path, recursive = TRUE, full.names = TRUE)
subdirs <- subdirs[!subdirs %in% c(objects, path)]


if (length(subdirs) > 0) {

for (i in 1:length(subdirs)) {
  wbppScripter(dir = subdirs[i], outputdir = subdirs[i])
  glue::glue("/bin/bash {subdirs[i]}/wbpp.sh") %>% system()
  
  
  
  object <- subdirs[i] %>% basename()
  master <- glue::glue("{subdirs[i]}/master")
  junk <- list.dirs(subdirs[i], recursive = FALSE)
  junk <- junk[!junk %in% master] 
  lapply(junk, unlink, recursive = T, force = T)
  masterFiles <- list.files(master, full.names = TRUE)
  autocropped <- masterFiles[grep("autocrop", masterFiles, ignore.case = TRUE)]
  newname <- glue::glue("{object}.xisf")
  file.rename(autocropped, glue::glue("{master}{newname}"))
  file.copy(glue::glue("{master}/{newname}"), glue::glue("{path}/"))
  glue::glue("{master}") %>% unlink(recursive = TRUE, force = TRUE) 
  file.remove(glue::glue("{subdirs[i]}/wbpp.sh"))
}


}
