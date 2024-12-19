library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)
options(dplyr.summarise.inform = FALSE)
source("functions/functions.R", echo = FALSE)


runES127()
runBaby()
generateReport()



# Rename files ------------------------------------------------------------
files <- list.files(ASI533, full.names = TRUE, recursive = TRUE, pattern = ".fits")
files <- files[!grepl("Flat", files)]
files <- files[grepl("LULTIMATE", files)]


files <- list.files(file.path(ASI2600,"ES127/IC405"), full.names = TRUE, recursive = TRUE, pattern = ".fits")
files <- files[!grepl("Flat", files)]
files <- files[grepl("HAOIII", files)]
newfiles <- stringr::str_replace(files, "HAOIII", "LULTIMATE")
file.rename(files, newfiles) 

