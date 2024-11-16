library(openxlsx)
library(magrittr)
library(dplyr)




# Connect to astro-pc/users to get the most recent copy of the images
#

username <- Sys.getenv("AstroPCUsername")
password <- Sys.getenv("AstroPCPassword")
mbp14 <- Sys.getenv("mbp14")
users <- paste0("open 'smb://", username, ":", password, "@es127/users'")
laptop <- paste0("open 'smb://", username, ":", password, "@", mbp14, "/briancarter/Astronomy'")
system(laptop)
system(users)
rm(users, username, password, mbp14, laptop)

testit <- function(x)
{
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}



testit(10) # wait a few seconds for the drives to mount

officeSSD <- "/Volumes/Astro-SSD/In Progress/ES127" # this is my SSD attached to office laptop
pcSSD <- "/Volumes/Users/Brian Carter/astronomy/In Progress/ES127" # this is the SSD internal to astro-pc
laptop <- "/Volumes/Astronomy" # macbook pro 14", main computer


# Write a function to copy everything from the internal Scope SSD to the office SSD
# Ideally this is only a limited number of subs, because I transfer this every morning.
# there is also a larger backup on the Samsung SSD attached to the telescope
# I'm just leaving that for now to occasionally back up to the HDD in the office.

myDirs <- list.dirs(pcSSD, recursive = FALSE, full.names = TRUE)
lapply(myDirs, function(x) {
  file.copy(x, officeSSD, recursive = TRUE, overwrite = FALSE)
})


myLog <- function(path, object) {
  src <- glue::glue("{path}/{object}")
  subdirs <- list.dirs(src, full.names = TRUE, recursive = TRUE)
  
  metadata <- readr::read_csv(file.path(src, "object_info.csv"))
  
  
  foo <- lapply(subdirs, function(x) {
    
    
    
    subfolder <- basename(x) |>
      stringr::str_remove(glue::glue("{object}_"))
    
    # exposure time comes from the darks file
    exposure <- list.files(x, pattern = "masterDark") |>
      stringr::str_remove("masterDark_") |>
      stringr::str_remove("s.xisf") |>
      as.numeric()
    
    # Filter
    filter <- list.files(x, pattern = "masterFlat") |>
      stringr::str_remove("masterFlat_") |>
      stringr::str_remove(".xisf") |>
      stringr::str_remove(".fits") |>
      as.character()
    
    date <- file.info(file.path(x, list.files(x, pattern = ".fit")[1]))$mtime %>%
      as.Date()
    
    filter <- ifelse(filter == "UVIR", "Broadband", ifelse(
      filter == "LPRO", "L-PRO", "L-Ultimate"
    ))
    
    # Number of frames
    files <- list.files(x, pattern = ".fit") |> 
      length()
    
    if (files != 0) {
      
      df <- data.frame(
        Object = object, 
        Name = metadata$Name |> as.character(),
        Constellation = metadata$Constellation |> as.character(),
        Type = metadata$Type |> as.character(),
        Date = date,
        Filter = filter,
        Subfolder = subfolder,
        Exposure = exposure,
        TotalSubs = files,
        TotalTimeHrs = (files * exposure / 3600) |> round(2)
      )
      return(df)
    }
  }
  )
  do.call("rbind", foo)
}
myLogWrapper <- function(path) {
  
  # Remove old version of log file
  logs <- list.files(laptop, pattern = "Imaging Log", full.names = TRUE) 
  file.remove(logs)

  dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  images <- data.frame()
  for (i in 1:length(dirs)) {
    images <- rbind(images, myLog(path, dirs[i]))
  }
  
  
  images <- lapply(dirs, function(x) {
    myLog(path, x)
  }) %>%
    do.call("rbind", .)
  
  summaries <- images %>%
    dplyr::group_by(Object, Name, Filter) %>%
    dplyr::summarize(TotalSubs = sum(TotalSubs), 
                     TotalTimeHrs = sum(TotalTimeHrs))
  
  
  wb <- createWorkbook(glue::glue("{laptop}/Imaging Log - {Sys.Date()}.xlsx"))
  addWorksheet(wb, "Images")
  addWorksheet(wb, "Summary")
  writeData(wb, "Images", images)
  writeData(wb, "Summary", summaries)
  saveWorkbook(wb, glue::glue("{laptop}/Imaging Log - {Sys.Date()}.xlsx"))
  
  
}
myLogWrapper("/Volumes/Astro-SSD/In Progress/ES127")






