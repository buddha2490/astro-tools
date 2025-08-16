library(dplyr)
library(magrittr)
library(gt)
options(dplyr.summarise.inform = FALSE)
# Functions ---------------------------------------------------------------


astroBinSubs <- function(myObject) {
  
  astroDB <- connectDB()
  df <- tbl(astroDB, "astroSubs")
  
  labels <- c("Antlia 3nm Narrowband H-alpha 36 mm",
              "Antlia 3nm Narrowband Oxygen III 36 mm",
              "Antlia 3nm Narrowband Sulfur II 36 mm",
              "ZWO Blue 36 mm",
              "ZWO Green 36 mm",
              "ZWO Luminance 36 mm",
              "ZWO Red 36 mm")
  
  df %>% 
    dplyr::filter(Object == myObject) %>%
    dplyr::filter(Status != "Excluded") %>%
    group_by(Date, FilterName, Duration) %>%
    summarize(Number = n()) %>%
    ungroup() %>%
    select(Date, Filter = FilterName, Number, Duration) %>%
    collect() %>%
    mutate(Filter = factor(Filter,
                           levels = c("H", "O", "S", "B", "G", "L", "R",
                                      "HO", "UVIR"),
                           labels = c(labels, "HO", "UVIR"))) %>%
    readr::write_csv(glue::glue("/Users/briancarter/Desktop/{myObject}_astrobin_subs.csv"))
  
  dbDisconnect(astroDB)
}
tabulateSubs <- function(myObject) {
  
  astroDB <- connectDB()
  df <- tbl(astroDB, "astroSubs")
  
  labels <- c("Luminance",
              "Red",
              "Green",
              "Blue",
              "H-alpha",
              "Sulfur II",
              "Oxygen III")
  
  df %>% 
    dplyr::filter(Object == myObject) %>%
    dplyr::filter(Status != "Excluded") %>%
    group_by(FilterName) %>%
    summarize(Number = n(),
              Duration = sum(Duration) / 60) %>%
    ungroup() %>%
    collect() %>%
    mutate(Filter = factor(FilterName,
                           levels = c("L", "R", "G", "B", "S", "H", "O", "HO", "UVIR"),
                           labels = c(labels, "HO", "UVIR"))) %>%
    arrange(Filter) %>%
    mutate(Duration2 = cumsum(Duration)) %>%
    select(Filter, Number, `Duration\n(mins)` = Duration, `Duration\n(cum)` = Duration2) %>%
    gt(rowname_col = "row") %>%
    tab_header(
      title = md(glue::glue("Sub tally for {myObject} grouped by filter and sub duration"))
    ) 
  
}


# Run ------------------------------------------------------------------



myObject <- "M27"

astroBinSubs(myObject) # saves a CSV file on desktop for upload

tabulateSubs(myObject) # more useful function



