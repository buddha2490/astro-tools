library(dplyr)


src <- "/Volumes/Office-SSD/Astronomy/In Progress/M31StarCloud/M31StarCloud_2025-08-14"

keyword <- "M31StarCloud"
replacement <- "NGC206"

fit <- list.files(src, recursive = TRUE, pattern = "fit")
lapply(fit, function(x) {
  
  oldname <- basename(x)
  newname <- sub(keyword, replacement, oldname)
  file.rename(file.path(src, x), file.path(src, newname))
  
})

# change the names in the metadata file too
readr::read_csv(file.path(src, "ImageMetaData.csv")) %>%
  mutate(FilePath = stringr::str_replace_all(FilePath, keyword, replacement)) %>%
  readr::write_csv(file.path(src, "ImageMetaData.csv"))




# Bulk_rename()