# Build SN8 Horizon file
library(magrittr)
library(dplyr)
list.files("data/horizons")


makegroup <- function(group_size = 5) {
  
  group <- data.frame()
  for (i in 1:69) {
    group <- data.frame(Group = rep(i, group_size)) %>% bind_rows(group)
  } 
  return(group)
} 

df <- readr::read_table("data/horizons/backyard.hrz", col_names = F)
colnames(df) <- c("AZ", "ALT")

df2 <- data.frame(AZ = as.numeric(1:360))

df <- full_join(df, df2, "AZ") %>%
  arrange(desc(AZ)) %>%
  mutate(ALT = ifelse(is.na(ALT), lag(ALT), ALT))


df$NewAz <- 360- df$AZ
df$NewAlt <- round(df$ALT / 26 * 100)
df <- df %>%
  bind_rows(data.frame(NewAz = 354, NewAlt = 212, AZ =6, ALT = 55)) %>%
  arrange(desc(NewAz)) %>%
  group_by(NewAz) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  arrange(desc(AZ))

df <- df %>%
  mutate(NODE = glue::glue("Node : {NewAz}, {NewAlt}")) %>%
  select(NODE)


write.table(df, "data/horizons/SN8_horizon.txt", row.names = F, col.names = F, quote = F)
