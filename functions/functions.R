
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
    df$filter[grep("HAO3", df$file)] <- "HAO3"
    df$filter[grep("SIIOIII", df$file)] <- "SIIOIII"
    df$filter[grep("S2O3", df$file)] <- "S2O3"
    df$filter[grep("SO", df$file)] <- "SO"
    df$filter[grep("HO", df$file)] <- "HO"
    
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
  
  ASI533 <- "/Volumes/Astro-SSD/ASI533MC"
  ASI2600 <- "/Volumes/Astro-SSD/ASI2600MC"
  
  
  allFiles <- getFiles(ASI533) |>
    dplyr::bind_rows(getFiles(ASI2600)) |>
    dplyr::select(-fits) |>
    dplyr::filter(object != "FlatWizard") |>
    dplyr::rename(Length = time) 
  # allFiles <- allFiles[-grep("Flat", allFiles$object),]
  drop <- c(file.path(ASI2600, "ES127/Future Projects"),
            file.path(ASI2600, "ES127/FlatWizard"),
            file.path(ASI2600, "ES127/SkyFlats"),
            file.path(ASI2600, "ES127/darks")) |>
    paste(collapse = "|")
  allFiles <- allFiles[-grep(drop, allFiles$Folder),]
  
  
  
  sessions <- allFiles |>
    dplyr::select(Camera, Scope, object, date, filter, Length) |>
    dplyr::group_by(Camera, Scope, object, date, filter) |>
    dplyr::summarize(Subs = n(),
                     Time = sum(Length, na.rm = TRUE) /60/60) |>
    dplyr::ungroup() |>
    dplyr::left_join(allFiles, by = c("Camera", "Scope", "object", "date", "filter")) |>
    dplyr::distinct(Camera, Scope, object, date, filter, Subs, Time, .keep_all = TRUE) |>
    dplyr::select(Camera, Scope, Object=object, Date = date, Filter = filter, Subs, Length, Time) |>
    dplyr::mutate(Filter = ifelse(Filter == "HAOIII", "LULTIMATE", Filter))
  
  totals <- sessions |>
    dplyr::group_by(Camera, Scope, Object) |>
    dplyr::summarize(Subs = sum(Subs),
                     Time = sum(Time))
  
  list(allFiles = allFiles,
       sessions = sessions,
       totals = totals)
}
saveWB <- function(images) {
  username <- Sys.getenv("username")
  password <- Sys.getenv("password")
  mbp13 <- Sys.getenv("mbp13")
  laptop <- paste0("open 'smb://", username, ":", password, "@", mbp13, "/Volumes/Astro-SSD'")
  system(laptop) # MBP13 connection
  rm(username, password, mbp14, laptop)
  
  testit(5)
  
  astroSSD <- "/Volumes/Astro-SSD"
  mbp14 <- "/Volumes/Astronomy"
  
  
  allFiles <- images$allFiles
  sessions <- images$sessions |>
    dplyr::mutate(Time = round(Time, 2))
  totals <- images$totals |>
    dplyr::mutate(Time = round(Time, 2))
  
  
  delete <- list.files(mbp14, pattern = "Imaging Log", full.names = TRUE)
  file.remove(delete)
  
  suppressMessages({
    new <- sessions 
  })
  
 
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
runES127 <- function() {
  username <- Sys.getenv("AstroPCUsername")
  password <- Sys.getenv("AstroPCPassword")
  users <- paste0("open 'smb://", username, ":", password, "@es127/users/Brian Carter/Astronomy/ASI2600MC/ES127'")
  system(users) # ES127 connection
  rm(users, username, password)
  
  testit(5)
  
  astroSSD <- "/Volumes/Astro-SSD"
  es127 <- "/Volumes/ES127"
  
  # Sub locations
  ASI2600 <- file.path(astroSSD, "ASI2600MC")
  
  # Copy nightly data from scope and generate a report
  file.copy(es127, ASI2600, recursive = TRUE, overwrite = FALSE) # copy from big scope
  
  # unmount the directories
  system(glue::glue("diskutil unmount {es127}"))
  
}
runBaby <- function() {
  
  username <- Sys.getenv("AstroPCUsername")
  password <- Sys.getenv("AstroPCPassword")
  babyscopeIP <- Sys.getenv("BabyScopeIP")
  babyscopeuser <- Sys.getenv("BabyScopeUsername")
  babyscopepassword <- Sys.getenv("BabyScopePassword")
  babyscope <- paste0("open 'smb://", babyscopeuser, ":", babyscopepassword, "@", babyscopeIP, "/Users/bcart/Astronomy/ASI2600MC/BabyScope'")
  system(babyscope) # BabyScope connection
  rm(username, password, babyscopeIP, babyscopeuser, babyscopepassword, babyscope)
  
  testit(15) # wait a few seconds for the drives to mount
  
  
  astroSSD <- "/Volumes/Astro-SSD"
  babyscope <- "/Volumes/BabyScope"
  ASI2600 <- file.path(astroSSD, "ASI2600MC")
  
  
  
  # Copy nightly data from scope and generate a report
  # Only run the one that is connected
  file.copy(babyscope, ASI2600, recursive = TRUE, overwrite = FALSE) # copy from babyscope
  
  # unmount the directories
  system(glue::glue("diskutil unmount {babyscope}"))
} 
