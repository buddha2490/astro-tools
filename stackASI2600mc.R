# daily cronjob Pixinsight

library(dplyr)
library(magrittr)
library(dplyr)
source("/Users/briancarter/astro-tools/buildDB.R")



getSessions <- function(x) {
  data.frame(
    path = list.dirs(x, recursive = TRUE, full.names = TRUE)
  ) %>%
    filter(path != x) %>%
    filter(stringr::str_detect(path, "master") == FALSE) %>%
    filter(stringr::str_detect(path, "logs") == FALSE) %>%
    filter(stringr::str_detect(path, "calibrated") == FALSE) %>%
    filter(stringr::str_detect(path, "registered") == FALSE) %>%
    filter(stringr::str_detect(path, "debayered") == FALSE) %>%
    filter(stringr::str_detect(path, "fastintegration") == FALSE) %>%
    filter(stringr::str_detect(path, "checkFits") == FALSE) %>%
    filter(stringr::str_detect(path, "metadata") == FALSE)
} 


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

# Home paths
paths <- file.path(
  "/Volumes/Astro-SSD",
  c("Galaxies", "Messier Globs", "Messier Objects", "Nebulae")
)


objects <- lapply(paths, function(x) {
  df <- data.frame(
    path = list.dirs(x, recursive = FALSE, full.names = TRUE)
  ) %>%
    filter(path != x) 
  
 lapply(df$path, getSessions) %>%
    do.call("rbind", .) %>%
   mutate(home = ifelse(
     stringr::str_detect(path, "Galaxies") == TRUE, "/Volumes/Astro-SSD/Galaxies", ifelse(
       stringr::str_detect(path, "Messier Globs") == TRUE, "/Volumes/Astro-SSD/Messier Globs", ifelse(
         stringr::str_detect(path, "Messier Objects") == TRUE, "/Volumes/Astro-SSD/Messier Objects", ifelse(
           stringr::str_detect(path, "Nebulae") == TRUE, "/Volumes/Astro-SSD/Nebulae", ""))))) %>%
   mutate(to = ifelse(
     stringr::str_detect(path, "Galaxies") == TRUE, "/Volumes/Astro-SSD/ASI2600MC/alaxies", ifelse(
       stringr::str_detect(path, "Messier Globs") == TRUE, "/Volumes/Astro-SSD/ASI2600MC/Messier Globs", ifelse(
         stringr::str_detect(path, "Messier Objects") == TRUE, "/Volumes/Astro-SSD/ASI2600MC/Messier Objects", ifelse(
           stringr::str_detect(path, "Nebulae") == TRUE, "/Volumes/Astro-SSD/ASI2600MC/Nebulae", "")))))
})
names(objects) <- basename(paths)



galaxies <- objects$Galaxies
globs <- objects$`Messier Globs`
messier <- objects$`Messier Objects`
nebulae <- objects$Nebulae



checkFits <- data.frame(path = list.dirs("/Volumes/Astro-SSD", recursive = TRUE, full.names = TRUE)) %>%
  filter(stringr::str_detect(path, "checkFits") == TRUE)




# Individual objects within each home


# individual sessions within each object
subfolders <- lapply(objects$path, function(x) 





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
  dropfiles <- setdiff(masterFiles, autocropped)
  lapply(dropfiles, file.remove)
  
  # rename files
  for (k in 1:length(autocropped)) {
    oldname = autocropped[k]
    newname <- glue::glue("{object}_{k}.xisf")
    file.rename(oldname, file.path(master, newname))
  }
  
  
  #  move to the transfer directory
  lapply(list.files(master, full.names = TRUE), function(x) {
    file.copy(x, file.path(path))
  })
  
  # remove the master directory
  glue::glue("{master}") %>% unlink(recursive = TRUE, force = TRUE) 
  
  # Remove the WBPP.sh file 
  if (file.exists(glue::glue("{subdirs[i]}/wbpp.sh"))) {
  file.remove(glue::glue("{subdirs[i]}/wbpp.sh"))
  }
  
}
}

rm(homePath, path, objects, subdirs)



