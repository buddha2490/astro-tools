library(openxlsx)
library(magrittr)
library(dplyr)

nina_rename <- function(path, scope, object, night, filter) {

  src <- glue::glue("{path}/{scope}/{object}/{object}_{night}")

  df <- data.frame(
    fit = list.files(src, pattern = ".fit")
  )
  df$time <- file.info(file.path(src, df$fit))$mtime
  df <- df |>
    dplyr::arrange(time)

  df$number <- 1:nrow(df) |> as.character()
  df$number <- stringr::str_pad(df$number, width = 4, side = "left", pad = "0")
  df$newname <- glue::glue("{object}_{night}_{filter}_{df$number}.fit")
  lapply(df$fit, function(x) {
    foo <- dplyr::filter(df, fit == x)
    oldname <- foo$fit[1] |> as.character()
    newname <- foo$newname[1] |> as.character()
    file.rename(from = file.path(src, oldname), to = file.path(src, newname))
  })

}
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
myLogWrapper <- function() {
  
  logPath <- "/users/briancarter/Astronomy/Imaging Logs"
  backupLog <- file.path(logPath, "backups")
  dir.create(backupLog, showWarnings = FALSE)
  
  logs <- list.files("/users/briancarter/Astronomy/Imaging Logs", pattern = ".xlsx", full.names = TRUE) 
  lapply(logs, function(x) {
    file.copy(x, file.path(backupLog))
    file.remove(x)
  })
  
  dirs <- list.dirs(progressPath, full.names = FALSE, recursive = FALSE)
  images <- lapply(dirs, function(x) {
    myLog(progressPath, x)
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
  
}

# REname stuff
myPath <- ("/volumes/SSD-Astro/In Progress")
nina_rename(myPath, "ES127", "M33", "night1", "BROADBAND")
nina_rename(myPath, "ES127", "M33", "night2", "BROADBAND")
nina_rename(myPath, "ES127", "M33", "night3", "BROADBAND")


# Update my excel file with new data
progressPath <- "/Volumes/SSD-Astro/In Progress/ES127"

myLogWrapper()



# renaming darks and flats
calibration <- function(path, type, time) {
  foo <- list.files(path, pattern = ".fit")
  df <- data.frame(
    old = foo)
  df$time <- file.info(file.path(path, df$old))$mtime
  df <- df |>
    dplyr::arrange(time)
  df$number <- 1:length(foo) |> as.character() |> stringr::str_pad(3, side = "left", pad = "0")
  df$new <- glue::glue("{type}_{time}_{df$number}.fit")

  for (i in 1:nrow(df)) {
    file.rename(from = file.path(path, df$old[i]), to = file.path(path, df$new[i]))
  }

}

path <- file.path("/users/briancarter/Astronomy/Dark Frames/darks subs/darks_60s")
calibration(path, "darks", "60s")

path <- file.path("/users/briancarter/Astronomy/Dark Frames/darks subs/darks_120s")
calibration(path, "darks", "120s")

path <- file.path("/users/briancarter/Astronomy/Dark Frames/darks subs/darks_180s")
calibration(path, "darks", "180s")

path <- file.path("/users/briancarter/Astronomy/Dark Frames/darks subs/darks_300s")
calibration(path, "darks", "300s")

path <- file.path("/users/briancarter/Astronomy/Dark Frames/darks/bias")
calibration(path, "bias")

path <- file.path("/volumes/SSD-Astro/Automated Copy/2024-10-17/Flats")
calibration(path, "flats", "LULTIMATE")
