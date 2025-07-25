



library(dplyr)
library(magrittr)
library(readr)

df <- read_csv("data/subframeselector output.csv", skip = 22) %>%
  mutate(Filename = basename(File)) %>%
  mutate(Filter = substr(Filename, 26, 26)) %>%
  select(-Approved, -Locked) %>%
  select(Index, Filter, Filename, File, SNR, FWHM, Eccentricity, Stars)


head(df$Filename)


# Lets remove anything over 2 SD
results <- lapply(unique(df$Filter), function(x) {
  
  filter <- df %>%
    filter(Filter == x)
  
  
  # Calculate thresholds for 2sd
  stars <- mean(filter$Stars) - 2*sd(filter$Stars)
  snr <- mean(filter$SNR) - 2*sd(filter$SNR)
  fwhm <- mean(filter$FWHM) + 2*sd(filter$FWHM) # higher is worse
  eccentricity <- mean(filter$Eccentricity) + 2*sd(filter$Eccentricity) # higher is worse
  
  
  filter %>%
    mutate(Exclusion = ifelse(
      Stars < stars, 1, ifelse(
        SNR < snr, 2, ifelse(
          FWHM > fwhm, 3, ifelse(
            Eccentricity > eccentricity, 4, 99))))) %>%
    mutate(Exclusion = factor(Exclusion, 
                              levels = c(1, 2, 3, 4, 99), 
                              labels = c("Stars", "SNR", "FWHM", "Eccentricity", "None")))

}) %>%
  do.call("rbind", .)



table(results$Exclusion, results$Filter)


drop <- results %>%
  filter(Exclusion != "None") %>%
  select(File, Filter, Exclusion)

for (i in 1:length(drop)) {
  if (file.exists(drop$File[i])) file.remove(drop$File[i])
}



saveRDS(drop, "/Volumes/Astro-SSD/ES127/FilterList.RDS")





l <- df %>%
  filter(Filter == "L") %>%
  select(-Filter, -Index)


lm(Stars ~ SNR + FWHM + Eccentricity, data = l) %>%
  summary()



PC1 - SNR.Eccentricity are negative, Stars/Altitude are positive, FWHM is irrelevant



pca <- prcomp(l %>% select(-File), center = TRUE, scale. = TRUE)


plot(pca$x[, 1:2], col = l$Stars, pch = 19,
     xlab = "PC1", ylab = "PC2", main = "PCA of L Filter Stars")

head(pca$rotation)

names(pca$sdev)

names(pca)

?prcomp






  
events <- logReshaped %>%
  mutate(
    start_num = as.numeric(start),
    end_num   = as.numeric(end)
  )
head(raw)


# read the metadata excel file




# 2. Plot with explicit log‐breaks and custom labels
ggplot(events) +
  geom_segment(aes(x = start_num, xend = end_num, y = ROLE, yend = ROLE),
               size = 4) +
  scale_x_log10(
    breaks = log_breaks(n = 8),
    labels = function(x) {
      format(
        as.POSIXct(x, origin = "1970-01-01", tz = "UTC"),
        "%H:%M:%S"
      )
    }
  ) +
  labs(
    x     = "Time (log scale, EDT)",
    y     = "Events",
    title = glue::glue("Imaging log for {as.Date(events$start[1])}")
  ) +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank()
  )
