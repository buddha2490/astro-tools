# Build SN8 Horizon file
library(magrittr)
library(dplyr)
list.files("data/horizons")


df <- readr::read_csv("data/horizons/horizon_19Oct2024 v2.hrz", col_names = F)
colnames(df) <- c("AZ", "ALT")

df$NewAz <- 360- df$AZ
df$NewAlt <- round(df$ALT / 26 * 100)
df$NODE <- glue::glue("Node : {df$NewAz}, {df$NewAlt}")
df <- df %>%
  arrange(desc(NewAz)) %>%
  group_by(NewAz) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  select(NODE)


head(df)

write.table(df, "data/horizons/SN8_horizon.txt", row.names = F, col.names = F, quote = F)
