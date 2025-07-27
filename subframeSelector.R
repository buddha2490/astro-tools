library(dplyr)
library(magrittr)
library(gt)
library(gtools)
options(dplyr.summarise.inform = FALSE)


cutpoints <- function(VAR) {
  ifelse(VAR < quantile(VAR, 0.10, na.rm = TRUE), "10%", ifelse(
      VAR > quantile(VAR, 0.9, na.rm = TRUE), "90%", ""))
}

### Subframe selector tool

getFilter <- function(dat) {
  dat %>%
    mutate(Filter = 
  ifelse(stringr::str_detect(Filename, "_L_") == TRUE, "L", ifelse(
    stringr::str_detect(Filename, "_R_") == TRUE, "R", ifelse(
      stringr::str_detect(Filename, "_G_") == TRUE, "G", ifelse(
        stringr::str_detect(Filename, "_B_") == TRUE, "B", ifelse(
          stringr::str_detect(Filename, "_H_") == TRUE, "H", ifelse(
            stringr::str_detect(Filename, "_S_") == TRUE, "S", ifelse(
              stringr::str_detect(Filename, "_O_") == TRUE, "O", "Problem")
            )))))))
}


subsdir <- file.path("/Volumes/Astro-SSD/Transfer/IC5146")

df <- glue::glue("{subsdir}/subframeSelector.csv") %>%
  readr::read_csv(skip = 22) %>%
  select(File, SNR = `PSF SNR`, FWHM, Eccentricity, Altitude, Stars) %>%
  mutate(Filename = basename(File),
         Directory = dirname(File)) %>%
  getFilter() %>%
  group_by(Filter) %>%
  mutate(SNRcut = cutpoints(SNR),
         FWHMcut = cutpoints(FWHM),
         Eccentricitycut = cutpoints(Eccentricity),
         Starscut = cutpoints(Stars)) %>%
  ungroup() %>%
  mutate(Exclusion = ifelse(SNRcut == "10%", "Low SNR", ifelse(
    FWHMcut == "10%", "Low FWHM", ifelse(
      Eccentricitycut == "90%", "High Eccentricity", ifelse(
        Starscut == "10%", "Low Stars", "Keep"))))) %>%
  mutate(Exclusion = factor(Exclusion, c("Low SNR", "Low FWHM", "High Eccentricity", "Low Stars", "Keep"))) 


examine <- df %>%
  filter(Exclusion != "Keep")

dir.create(file.path(subsdir, "Exclusions"), showWarnings = FALSE, recursive = TRUE)
returnStatus <- sapply(examine$File, function(x) {
  returnStatus <- file.copy(x, file.path(subsdir, "Exclusions"), recursive = TRUE, overwrite = TRUE)
  if (returnStatus == TRUE) file.remove(x)
})



