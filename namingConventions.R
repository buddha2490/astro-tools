library(openxlsx)
library(magrittr)
library(dplyr)

officeSSD <- "/Volumes/Astro-SSD/In Progress/ES127" # this is my SSD attached to office laptop


foo <- function() {
  files <- data.frame(files = list.files(x, pattern = "fit"))
  files$number <- stringr::str_pad(1:nrow(files), 4, pad = 0)
  files$newname <- glue::glue("{object}_{grouping}_{filter}_{time}_{files$number}.fits")
  for (i in 1:nrow(files)) {
    file.rename(glue::glue("{x}/{files$files[i]}"), glue::glue("{x}/{files$newname[i]}"))
  }
} 
object <- "NGC891"
src <- glue::glue("{officeSSD}/{object}")
grouping <- "2024-11-12"
x <- glue::glue("{src}/{object}_{grouping}")
filter <- "LULTIMATE"
time <- "180.00"
foo()

