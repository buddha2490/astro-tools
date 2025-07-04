library(openxlsx)
library(magrittr)
library(dplyr)
library(tidyr)
options(dplyr.summarise.inform = FALSE)
source("functions/functions.R", echo = FALSE)


runES127()
# runBaby()
generateReport()



# Rename files ------------------------------------------------------------

ASI2600 <- file.path("/Volumes/Astro-SSD/ASI2600MC")

files <- list.files(ASI533, full.names = TRUE, recursive = TRUE, pattern = ".fits")
files <- files[!grepl("Flat", files)]
files <- files[grepl("LULTIMATE", files)]


files <- list.files(file.path(ASI2600,"ES127/IC405"), full.names = TRUE, recursive = TRUE, pattern = ".fits")
files <- files[!grepl("Flat", files)]
files <- files[grepl("HAOIII", files)]
newfiles <- stringr::str_replace(files, "HAOIII", "LULTIMATE")
file.rename(files, newfiles) 

files <- list.files(file.path(ASI2600, "BabyScope/NGC 281"), full.names = TRUE, recursive = TRUE, pattern = ".fits")
fit <- basename(files)
root <- dirname((files))
newfit <- stringr::str_replace(fit, "NGC 281", "NGC281")
for (i in 1:length(fit)) {
  file.rename(file.path(root[i], fit[i]), file.path(root[i], newfit[i]))
}
