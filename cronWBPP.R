### cronjob - automatic PI stacking


library(dplyr)
library(magrittr)

transfer <- "/Volumes/Astro-SSD/Transfer"



# Functions ---------------------------------------------------------------
wbppScripter <- function(path) {
  

    exe <- '"/Applications/PixInsight/PixInsight.app/Contents/MacOS/PixInsight"  -n --automation-mode -r='
    
    js1 <- '"/Applications/Pixinsight/src/scripts/BatchPreprocessing/FBPP.js,automationMode=true,outputDirectory='
    
    wbpp <- file("/Users/briancarter/Astronomy/astro-tools/Shell/wbpp.sh", "a")
    
    system('chmod 777 "/Users/briancarter/Astronomy/astro-tools/postprocessing/wbpp.sh"')
    
  
  outputdir <- "/Volumes/Astro-SSD/transfer"
  
  dir <- glue::glue(path, '"')
  
  js2 <- '--force-exit'
  
  foo <- glue::glue("{exe}{js1}{outputdir},dir={dir} {js2}")
  
  write(foo, wbpp, append = TRUE)
  close(wbpp)
  
  
  
}

wbppInitializer <- function() {
  wbpp <- file.path("/Users/briancarter/Astronomy/astro-tools/Shell/wbpp.sh")
  file.remove(wbpp)
  file(wbpp, "a")
  system('chmod 777 "/Users/briancarter/Astronomy/astro-tools/Shell/wbpp.sh"')
}

listSubDirs <- function() {
  data.frame(dir = list.dirs("/Volumes/Astro-SSD/Transfer", recursive = TRUE, full.names = TRUE)) %>%
    filter(!stringr::str_detect(dir, "metadata") == TRUE) %>%
    filter(!stringr::str_detect(dir, "checkFits") == TRUE) %>%
    filter(!stringr::str_detect(dir, "flats") == TRUE) %>%
    filter(!stringr::str_detect(dir, "master") == TRUE) %>%
    filter(!stringr::str_detect(dir, "registered") == TRUE) %>%
    filter(!stringr::str_detect(dir, "calibrated") == TRUE) %>%
    filter(!dir %in% list.dirs("/Volumes/Astro-SSD/Transfer", recursive = FALSE, full.names = TRUE)) %>%
    filter(dir != "/Volumes/Astro-SSD/Transfer") %>%
    pull(dir)
  
}





objects <- listSubDirs()

# The rest needs to be a loop.
# 1. initialize wbpp
# 2. run the scripter on objects[i]
# 3. clean up the output
# 4. loop

for (i in 1:length(objects)) {

  path <- objects[i]
  dir <- dirname(path)
  objectName <- basename(dir)
  date <- stringr::str_remove(basename(path), glue::glue("{objectName}_")) # extract date from object name
  
  wbppInitializer() # initializes the wbpp script
  path %>% wbppScripter()
  "/Users/briancarter/Astronomy/astro-tools/Shell/wbpp.sh" %>% system()
  
  # clean up the output
  file.path(transfer, "calibrated") %>% unlink(force = TRUE, recursive = TRUE)
  file.path(transfer, "logs")  %>% unlink(force = TRUE, recursive = TRUE)
  file.path(transfer, "fastIntegration") %>% unlink(force = TRUE, recursive = TRUE)
  master <- file.path(transfer, "master")
  stacks <- data.frame(file = list.files(master, pattern = "autocrop"))  %>%
    mutate(Filter = ifelse(
      stringr::str_detect(file, "-L_") == TRUE, "L", ifelse(
        stringr::str_detect(file, "-R_") == TRUE, "R", ifelse(
          stringr::str_detect(file, "-G_") == TRUE, "G", ifelse(
            stringr::str_detect(file, "-B_") == TRUE, "B", ifelse(
              stringr::str_detect(file, "-H_") == TRUE, "H", ifelse(
                stringr::str_detect(file, "-O_") == TRUE, "O", ifelse(
                  stringr::str_detect(file, "-S_") == TRUE, "S", "Unknown"
                )))))))) %>%
    mutate(filename = glue::glue("{objectName}_{Filter}_{date}.xisf"))
  
  for (i in 1:nrow(stacks)) {
    file.rename(file.path(master, stacks$file[i]),
                file.path(master, stacks$filename[i]))
    file.copy(file.path(master, stacks$filename[i]),
              dir)
  }
  master %>% unlink(force = TRUE, recursive = TRUE)
}  




