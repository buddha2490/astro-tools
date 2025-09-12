list.files("data/horizons")
library(magrittr)

df <- data.frame(
  HDG_DEG = 1:360) %>%
  left_join(
  readr::read_csv("data/horizons/DAV Horizon Raw.csv") %>%
    dplyr::arrange(HDG_DEG, desc(VERT)) %>%
    dplyr::distinct(HDG_DEG,.keep_all = TRUE), by = "HDG_DEG")
head(df)
df$VERT[1:4] <- 28.3
head(df)



df <- df %>%
  tidyr::fill(VERT, .direction = "down")
summary(df$VERT)


write.csv(df,"data/horizons/DAV Horizon Clean.csv", row.names = FALSE, col.names = FALSE)
