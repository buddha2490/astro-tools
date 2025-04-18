

### BuildDB
library(dplyr)
library(DBI)
library(RSQLite)
library(stringr)
library(magrittr)

wait <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1
}
getFiles <- function(dir) {
  list.files(dir, 
             pattern = "fits", 
             ignore.case = TRUE, 
             full.names = TRUE, 
             recursive = TRUE)
}
buildDB <- function(group) {
  df <- data.frame(path = group) %>%
    mutate(filename = basename(path)) %>%
    mutate(dirname = dirname(path)) %>%
    mutate(object = basename(dirname(dirname))) %>%
    mutate(subdir = basename(dirname(dirname(dirname(path))))) %>%
    mutate(date = stringr::str_replace(basename(dirname), glue::glue("{object}_"), "")) %>%
    select(path, filename, object, subdir, date)
  
  filter <- df %>%
    mutate(filter = stringr::str_replace(filename, glue::glue("{object}_{date}_"), ""))
  
  filter$newfilter <- ""
  filter$newfilter[grep("UVIR", filter$filter)] <- "UVIR"
  filter$newfilter[grep("LPRO", filter$filter)] <- "LPRO"
  filter$newfilter[grep("HO", filter$filter)] <- "HO"
  filter$newfilter[grep("SO", filter$filter)] <- "SO"
  filter$newfilter[grep("SIIOIII", filter$filter)] <- "SO"
  filter$newfilter[grep("LULTIMATE", filter$filter)] <- "LULTIMATE"
  filter$newfilter[grep("HAO3", filter$filter)] <- "HO"
  filter <- filter %>%
    select(-filter) %>%
    rename(filter = newfilter)
  
  
  
  exposure <- filter
  exposure$exposure <- NA_real_
  exposure$exposure[grepl("30", exposure$filename)] <- 30
  exposure$exposure[grepl("60", exposure$filename)] <- 60
  exposure$exposure[grepl("120", exposure$filename)] <- 120
  exposure$exposure[grepl("180", exposure$filename)] <- 180
  exposure$exposure[grepl("240", exposure$filename)] <- 240
  exposure$exposure[grepl("300", exposure$filename)] <- 300
  exposure$exposure[grepl("600", exposure$filename)] <- 600
  
  final <- exposure
  
  myDB <- DBI::dbConnect(RSQLite::SQLite(), 
                         dbname = file.path(mbp14, "objectDB.db"))
  
  oldTable <- tbl(myDB, "objectDB") %>% collect()
  
  newObjects <- final %>%
    anti_join(oldTable, by = c("path"))
  
  DBI::dbWriteTable(myDB, "objectDB", newObjects, append = TRUE)
  
  dbDisconnect(myDB)
}

# Get object info
username <- Sys.getenv("AstroPCUsername")
password <- Sys.getenv("AstroPCPassword")
mbp14 <- Sys.getenv("mbp14")
con <- glue::glue("open 'smb://{username}:{password}@{mbp14}/briancarter/Astronomy'") %>% as.character()
system(con); 
wait(5)
rm(con, username, password, mbp14)

# MB14 path for the database file
mbp14 <- "/Volumes/Astronomy"

# Inventory my files - this takes a minute
ssdPath <- file.path("/Volumes/Astro-SSD")  
galaxies <- file.path(ssdPath, "Galaxies") %>% getFiles()
nebulae <- file.path(ssdPath, "Nebulae") %>% getFiles()
globs <- file.path(ssdPath, "Messier Globs") %>% getFiles()
messier <- file.path(ssdPath, "Messier Objects") %>% getFiles()
group <- c(galaxies, nebulae, globs, messier)
rm(galaxies, nebulae, globs, messier, ssdPath)

buildDB(group)

saveRDS(group, file= file.path(mbp14, "dbjects.rds"))
