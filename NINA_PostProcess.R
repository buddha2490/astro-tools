library(openxlsx)
library(magrittr)
library(dplyr)
library(DBI)
library(RPostgreSQL)


# Connect to Astro-PC2 before next script
backups <- "/Volumes/astronomy/In Progress/ES127"
src <- "/Volumes/Astro-SSD/In Progress/ES127"
lapply(list.dirs(src, full.names = FALSE, recursive = FALSE), function(x) {
  
  subdirs <- list.dirs(file.path(backups, x), full.names = FALSE, recursive = FALSE)
  srcdirs <- list.dirs(file.path(src, x), full.names = FALSE, recursive = FALSE)
  new <- setdiff(srcdirs, subdirs)
  
  
  file.copy(file.path(src, x, new),
            file.path(backups, x),
            recursive = TRUE)
  
  
})


myLog <- function(path, object) {
  src <- glue::glue("{path}/{object}")
  subdirs <- list.dirs(src, full.names = TRUE, recursive = FALSE)
  
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
      as.character()
    
    date <- file.info(file.path(x, list.files(x, pattern = ".fit")[1]))$mtime %>%
      as.Date()
    
    filter <- ifelse(filter == "UVIR", "Broadband", ifelse(
      filter == "LPRO", "L-PRO", "L-Ultimate"
    ))
    
    # Number of frames
    files <- list.files(x, pattern = ".fit") |> 
      length()
    
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
  })
  do.call("rbind", foo)
}
myLogWrapper <- function(path) {
  
  logPath <- "/users/briancarter/Astronomy/Imaging Logs"
  backupLog <- file.path(logPath, "backups")
  dir.create(backupLog, showWarnings = FALSE)
  
  logs <- list.files("/users/briancarter/Astronomy/Imaging Logs", pattern = ".xlsx", full.names = TRUE) 
  lapply(logs, function(x) {
    file.copy(x, file.path(backupLog))
    file.remove(x)
  })
  
  dirs <- list.dirs(path, full.names = FALSE, recursive = FALSE)
  images <- lapply(dirs, function(x) {
    myLog(path, x)
  }) %>%
    do.call("rbind", .)
  
  summaries <- images %>%
    dplyr::group_by(Object, Name, Filter) %>%
    dplyr::summarize(TotalSubs = sum(TotalSubs), 
                     TotalTimeHrs = sum(TotalTimeHrs))
  
  
  wb <- createWorkbook(glue::glue("/users/briancarter/Astronomy/Imaging Logs/Imaging Log - {Sys.Date()}.xlsx"))
  addWorksheet(wb, "Images")
  addWorksheet(wb, "Summary")
  writeData(wb, "Images", images)
  writeData(wb, "Summary", summaries)
  saveWorkbook(wb, glue::glue("/users/briancarter/Astronomy/Imaging Logs/Imaging Log - {Sys.Date()}.xlsx"))
  
  username <- Sys.getenv("username")
  password <- Sys.getenv("password")
  
  myDB <- dbConnect(dbDriver("PostgreSQL"),
                    dbname = "astro",
                    host = "astro-pc2",
                    port = 5432,
                    user = username,
                    password = password)
  
  dbWriteTable(myDB, "ImagingLog", images, overwrite = TRUE)
  dbWriteTable(myDB, "ImagingLogSummary", summaries, overwrite = TRUE)
  
  dbDisconnect(myDB)
  
}
myLogWrapper("/Volumes/Astro-SSD/In Progress/ES127")






