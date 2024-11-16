library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)



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



# metadata
getMetadata <- function(path) {
  
 list.files(path, pattern = "object_info.csv", recursive = TRUE, full.names = TRUE) %>%
    lapply(function(x) {
    readr::read_csv(x)
    }) %>%
    bind_rows()
  
}
countFiles <- function(path) {
  
fits <- list.files(path, pattern = "fit", recursive = TRUE) %>%
  basename()
calibration <- fits[grep("flat", fits, ignore.case = TRUE)]
dark <- fits[grep("dark", fits, ignore.case = TRUE)]
fits <- fits[!fits %in% c(dark, calibration)]

df <- data.frame(fits = fits)
df$fits <- sub(".fits", "", df$fits)

df2 <- df |>
  separate_wider_delim(fits, delim = "_", 
                       names = c("Object", "Grouping", "Filter", "Exposure", "Sequence"))

meta <- getMetadata(path)

nightly <- df2 %>%
  dplyr::group_by(Object, Grouping, Filter, Exposure) %>%
  dplyr::summarize(Subs = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(Exposure = as.numeric(Exposure)) %>%
  left_join(meta, by = "Object") %>%
  dplyr::select(Object, Name, Constellation, Type, Filter, Grouping, Exposure, Subs) %>%
  dplyr::mutate(Time = Exposure * Subs / 60 / 60) %>%
  dplyr::mutate(Time = round(Time, 2))


summaries <- nightly %>%
  dplyr::group_by(Object, Filter) %>%
  dplyr::summarize(TotalTime = sum(Time),
                   TotalSubs = sum(Subs)) %>%
  dplyr::ungroup() %>%
  dplyr::left_join(meta, by = "Object") %>%
  dplyr::select(Object, Name, Constellation, Type, Filter, TotalSubs, TotalTime) %>%
  dplyr::mutate(TotalTime = round(TotalTime, 2))

return(list (nightly = nightly, summaries = summaries))

}
saveWB <- function(inputs) {
  
  nightly <- inputs$nightly
  summaries <- inputs$summaries

  wb <- createWorkbook(glue::glue("{laptop}/Imaging Log - {Sys.Date()}.xlsx"))
  addWorksheet(wb, "Objects")
  addWorksheet(wb, "Summary")
  writeData(wb, "Objects", nightly)
  writeData(wb, "Summary", summaries)
  saveWorkbook(wb, glue::glue("{laptop}/Imaging Log - {Sys.Date()}.xlsx"),
               overwrite = TRUE)
  
}



path <- "/Volumes/Astro-SSD/In Progress/ES127/"
countFiles(path) %>%
  saveWB()


foo <- countFiles(path)


