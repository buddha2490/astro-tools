library(dplyr)
library(magrittr)




# Solar renaming ----------------------------------------------------------

src <- "/users/briancarter/astronomy/Sun_8Aug"
fun <- function(src) {
  numbered <- (file.path(src, "numbered"))
  ppg <- (file.path(src, "ppg"))
  aligned <- (file.path(src, "aligned"))
  ps <- (file.path(src, "photoshop"))

  dir.create(numbered)
  dir.create(ppg)
  dir.create(aligned)
  dir.create(ps)

  conv <- list.files(src, pattern = "conv")
  lapply(conv, function(x) {
    file.copy(file.path(src, x), file.path(numbered))
  })

  old <- list.files(numbered)
  df <- data.frame(old = old)
  df$new <- paste0(1:nrow(df)) %>%
    stringr::str_pad(3, side = "left", pad = "0") %>%
    paste0(".tif")
  lapply(df$old, function(x) {
    file.rename(file.path(numbered, x), file.path(numbered, df$new[df$old == x]))
  })

  # cleanup
  lapply(list.files(src, pattern = ".tif", full.names = TRUE, include.dirs = FALSE), file.remove)

  dir.create(ppg)
  dir.create(aligned)
  dir.create(ps)

}



# File renaming DSO -------------------------------------------------------

# All the lights need to start with "lights_"
# sharpcap names with the object name, so we need to rename them

src <- "/volumes/SSD-Astro/In Progress/M33"
src <- file.path("/users/briancarter/astronomy/In Progress/ES102/M31/M31_night1")


fun <- function(prefix) {
  orig <- list.files(src, pattern = prefix)
  new <- sub(prefix, "lights", orig)
  lapply(1:length(orig), function(x) {
    file.rename(file.path(src, orig[x]), file.path(src, new[x]))
  })
}
fun("M31")


