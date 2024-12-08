library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)



# Functions ---------------------------------------------------------------
testit <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
eachScope <- function(path, scope) {
  
  one <- list.dirs(file.path(path, scope))
  one_fits <- sapply(one, function(x) list.files(x, pattern = "fit", full.names = TRUE, recursive = TRUE)) |>
    unlist()
  
  df <- data.frame(fits = one_fits) |>
    dplyr::mutate(Camera = basename(path),
                  Scope = scope)  |>
    dplyr::mutate(file = basename(fits))
  
  if (nrow(df) > 0 ) {
    df <- df |>
      dplyr::rowwise() |>
      dplyr::mutate(Folder = sub(file, "", fits)) |>
      dplyr::mutate(object = basename(dirname(Folder))) |>
      dplyr::mutate(date = sub(glue::glue("{path}/{scope}/{object}/{object}_"), "", Folder)) |>
      dplyr::mutate(date = sub("/", "", date)) |>
      dplyr::ungroup() |>
      dplyr::distinct(fits, .keep_all = TRUE)
    
    df$filter <- ""
    df$filter[grep("UVIR", df$file)] <- "UVIR"
    df$filter[grep("LPRO", df$file)] <- "LPRO"
    df$filter[grep("LULTIMATE", df$file)] <- "LULTIMATE"
    
    df <- df |>
      dplyr::mutate(time = stringr::str_replace(file, glue::glue("{object}_{date}_{filter}_"), ""))
    
    suppressWarnings(
      df$time <- substr(df$time,
                        1,
                        nchar(df$time) - 10) |>
        as.numeric()
    )
    
    return(df)
  }
}
getFiles <- function(path) {
  babyscope <- eachScope(path, "BabyScope")
  bigscope <- eachScope(path, "ES127")
  rbind(bigscope, babyscope)
}
summaries <- function() {
  allFiles <- getFiles(ASI533) |>
    dplyr::bind_rows(getFiles(ASI2600)) |>
    dplyr::select(-fits) |>
    dplyr::filter(object != "FlatWizard")
  
  sessions <- allFiles |>
    dplyr::group_by(Camera, Scope, object, date, filter) |>
    dplyr::summarize(Subs = n(),
                     Time = sum(time, na.rm = TRUE) /60/60) |>
    dplyr::ungroup()
  
  totals <- sessions |>
    dplyr::group_by(Camera, Scope, object) |>
    dplyr::summarize(Subs = sum(Subs),
                     Time = sum(Time))
  
  list(allFiles = allFiles,
       sessions = sessions,
       totals = totals)
}
saveWB <- function(images) {
  
  allFiles <- images$allFiles
  sessions <- images$sessions |>
    dplyr::mutate(Time = round(Time, 2))
  totals <- images$totals |>
    dplyr::mutate(Time = round(Time, 2))
  
  
  delete <- list.files(mbp14, pattern = "Imaging Log", full.names = TRUE)
  orig <- openxlsx::read.xlsx(delete, sheet = "Sessions") |>
    dplyr::mutate(Subs = as.integer(Subs))
  file.remove(delete)
  
  
  new <- sessions %>%
    dplyr::anti_join(orig)
  
  newObjects <- !duplicated(new$object)
  nSubs <- sum(new$Subs)
  time <- sum(new$Time)
  
  message <- glue::glue("Since the last repo, {nSubs} subs have been taken across {length(newObjects)} objects for a total of {time} hours.")
  cat(message)
  
  wb <- createWorkbook(glue::glue("{mbp14}/Imaging Log - {Sys.Date()}.xlsx"))
  addWorksheet(wb, "FitFiles")
  addWorksheet(wb, "Sessions")
  addWorksheet(wb, "Objects")
  
  writeData(wb, "Objects", totals)
  writeData(wb, "Sessions", sessions)
  writeData(wb, "FitFiles", allFiles)
  
  saveWorkbook(wb, glue::glue("{mbp14}/Imaging Log - {Sys.Date()}.xlsx"),
               overwrite = TRUE)
}
generateReport <- function() {
  images <- summaries() |>
  saveWB()
}



# Connect to astro-pc/users to get the most recent copy of the images
#

username <- Sys.getenv("AstroPCUsername")
password <- Sys.getenv("AstroPCPassword")
mbp14 <- Sys.getenv("mbp14")
users <- paste0("open 'smb://", username, ":", password, "@es127/users'")
laptop <- paste0("open 'smb://", username, ":", password, "@", mbp14, "/briancarter/Astronomy'")
system(laptop) # MBP14 connection
system(users) # ES127 connection
rm(users, username, password, mbp14, laptop)

testit(10) # wait a few seconds for the drives to mount


# Locations
# 1.  MBP13 - external SSD - "Astro-SSD"
# 2.  ES127 - external SSD - " Samsung-SSD"
# 3.  MBP14 - internal - "Astronomy"

astroSSD <- "/Volumes/Astro-SSD"
mbp14 <- "/Volumes/Astronomy"
es127 <- "/Volumes/users/Brian Carter/Astronomy/In Progress"

# Sub locations
# 1. astroSSD/ASI533MC
# 2. astroSSD/ASI2600MC

ASI533 <- file.path(astroSSD, "ASI533MC")
ASI2600 <- file.path(astroSSD, "ASI2600MC")



# Copy nightly data from scope and generate a report
file.copy(file.path(es127, "BabyScope"), ASI2600, recursive = TRUE, overwrite = FALSE)
file.copy(file.path(es127, "ES127"), ASI2600, recursive = TRUE, overwrite = FALSE)
generateReport()

