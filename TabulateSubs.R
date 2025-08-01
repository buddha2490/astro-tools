library(dplyr)
library(magrittr)



# Todo: break this down by date
# Should be easy to add



# Functions ---------------------------------------------------------------



eachFilter <- function(x) {
  
  data.frame(file = x) %>%
    mutate(filter = substr(x, 1, 1)) %>%
    mutate(secs = stringr::str_replace(file, paste0(filter, "_"), "")) %>%
    mutate(secs = as.numeric(secs))
}

replace <- function(x, object) {
  string <- substr(x,
                   1,
                   paste0(object, "_",Sys.Date(),"_LIGHT_") %>% nchar())
  
  stringr::str_replace(x, string, "")   %>%
    stringr::str_replace(".fits", "") %>% 
    substr(1, (nchar(.) - 5))
  
}

tallySubs <- function(src) {
  
  object <- basename(src)
  
  files <- list.files(src,
                      recursive = TRUE,
                      pattern = "fit") %>%
    basename()
  
  filelist <- list()
  filelist$l <- files[stringr::str_detect(files, "_L_") == TRUE] %>% replace(object)
  filelist$r <- files[stringr::str_detect(files, "_R_") == TRUE] %>% replace(object)
  filelist$g <- files[stringr::str_detect(files, "_G_") == TRUE] %>% replace(object)
  filelist$b <- files[stringr::str_detect(files, "_B_") == TRUE] %>% replace(object)
  filelist$s <- files[stringr::str_detect(files, "_S_") == TRUE] %>% replace(object)
  filelist$h <- files[stringr::str_detect(files, "_H_") == TRUE] %>% replace(object)
  filelist$o <- files[stringr::str_detect(files, "_O_") == TRUE] %>% replace(object)
  
  lapply(filelist, eachFilter) %>%
    do.call("rbind", .) %>%
    mutate(filter = factor(filter,
                           levels = c("L", "R", "G", "B", "H", "S", "O"))) %>%
    group_by(filter, secs) %>%
    summarize(N = n(),
              mins = sum(secs) / 60) %>%
    arrange(filter) %>%
    ungroup() %>%
    group_by(filter) %>%
    mutate(total_mins = cumsum(mins))
  
}  



# Run in ------------------------------------------------------------------



# Pick an object path
src <- "/Volumes/Astro-SSD/Transfer/M16"

tallySubs(src)
