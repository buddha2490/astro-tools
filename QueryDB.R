library(dplyr)
library(magrittr)
library(DBI)
library(RSQLite)

wait <- function(x) {
  p1 <- proc.time()
  Sys.sleep(x)
  proc.time() - p1 # The cpu usage should be negligible
}
recentUpdate <- function() {
  # find the most recent update where objects were added
  # note: database is updated hourly
  foo <- tbl(myDB, "timestamp") %>%
    arrange(Date, time) %>%
    collect() %>%
    mutate(Date = as.Date(Date)) %>%
    mutate(time = as.POSIXct(time, format = "%H:%M:%S")) %>%
    filter(newobjects != 0) %>%
    rename(newSubs = newobjects) %>%
    slice(1)
  
  if (nrow(foo) == 0) {
    warning("Warning - no new objects have ever been added to the database")
  } else {
    foo
  }
}
connect <- function() {
username <- Sys.getenv("username")
password <- Sys.getenv("password")
mbp13 <- Sys.getenv("mbp13")
laptop <- paste0("open 'smb://", username, ":", password, "@", mbp13, "/Astro-SSD'")
system(laptop) # MBP13 connection
rm(username, password, mbp13, laptop)
wait(5)
# global variables
path <<- file.path("/Volumes/Astro-SSD")
myDB <<- dbConnect(RSQLite::SQLite(), file.path(path, "objectDB.db"))
}
getAstrobin <- function(myObject) {
  
  filterLab <- c("Baader UV/IR Cut Luminance (CMOS Optimized) 2",
                 "Optolong L-Pro 2",
                 'Altair Ha Oiii DualBand ULTRA 4nm CERTIFIED CMOS Filter 2"',
                 'Altair SII OIII DualBand Ultra 4nm CERTIFIED CMOS Filter 2"')
  
  tbl(myDB, "images") %>%
    filter(object == myObject) %>%
    group_by(date, filter) %>%
    collect() %>%
    summarize(
      Number = n(),
      Duration = sum(exposure) / Number) %>%
    ungroup() %>%
    rename(Date = date, Filter = filter) %>%
    mutate(Filter = factor(Filter, levels = c("UVIR", "LPRO", "HO", "SO"), labels = filterLab))
}
getObject <- function(myObject) {
 a <-  tbl(myDB, "images") %>%
    filter(object == myObject) %>%
    group_by(filter, exposure) %>%
    collect() %>%
    summarize(
      Subs = n(),
      Duration = sum(exposure) / Subs,
      Hours = (Subs * Duration) / 60 / 60) %>%
    ungroup() %>%
    rename(Filter = filter, Exposure = exposure)
 
 b <- a %>%
   group_by(Filter) %>%
   summarize(Cum_Integratiopn = sum(Hours, na.rm = TRUE)) %>%
   ungroup()
 
 print(a); print(b)

}


# Function calls ----------------------------------------------------------

connect()


getAstrobin("NGC6888") %>% readr::write_csv(file.path("/users/briancarter/Desktop/astrobin-upload.csv"))

getObject("NGC6888") 


