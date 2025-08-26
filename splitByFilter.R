


### Split data by filter

#' sometimes i need to look at single-filter data
#' this function will take an input directory
#' and split the data by filter into directories w/i the parent directory
#' 
#' I can then go into each of these directories and use BLINK in pixinsight
#' in Blink, I can select the subs I want to keep and copy them back into the src directory
#' then simply delete the rest


library(magrittr)
library(dplyr)
library(readr)

src <- "/Volumes/Office-SSD/Astronomy/In Progress/CygnusWall/CygnusWall_2025-08-24"

splitFilter <- function(src) {
  # get list of files
  files <- data.frame(file = list.files(src, pattern = ".fits", full.names = TRUE))
  
  # extract filter names from filenames
  files <- files %>%
    mutate(Filter = ifelse(
      stringr::str_detect(basename(file), "_L_") == TRUE, "L", ifelse(
        stringr::str_detect(basename(file), "_R_") == TRUE, "R", ifelse(
          stringr::str_detect(basename(file), "_G_") == TRUE, "G", ifelse(
            stringr::str_detect(basename(file), "_B_") == TRUE, "B", ifelse(
              stringr::str_detect(basename(file), "_H_") == TRUE, "H", ifelse(
                stringr::str_detect(basename(file), "_O_") == TRUE, "O", ifelse(
                  stringr::str_detect(basename(file), "_S_") == TRUE, "S", "Unknown"))))))))

  
  
  # create directories for each filter and move files
  files %>%
    dplyr::distinct(Filter) %>%
    pull(Filter) %>%
    sapply(function(x) dir.create(file.path(src, x)))
  
  sapply(1:nrow(files), function(x) {
    file <- files$file[x]
    filter <- files$Filter[x]
    to <- file.path(src,filter)
    returnStatus <- file.copy(file, to)
    if (returnStatus == TRUE) file.remove(file)
  })
  
  
}
splitFilter(src)



