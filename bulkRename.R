library(dplyr)


src <- "/Volumes/Astro-SSD/Transfer/NGC7331_SN2025rbs/NGC7331_SN2025rbs_2025-07-21"

keyword <- "NGC7331_SN2025rbs"
replacement <- "NGC7331"

fit <- list.files(src, recursive = TRUE, pattern = "fit")
lapply(fit, function(x) {
  
  oldname <- basename(x)
  newname <- sub(keyword, replacement, oldname)
  file.rename(file.path(src, x), file.path(src, newname))
  
})

list.files(src)

# Bulk_rename()