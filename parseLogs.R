library(dplyr)
library(stringr)
library(readr)
library(lubridate)
library(tidyr)
library(huxtable)
library(pharmaRTF)





# Functions ---------------------------------------------------------------
eventPairs <- function(df) {
  
  starts <- df %>%
    filter(str_detect(MESSAGE, "^Starting Category:")) %>%
    mutate(
      ROLE = str_match(MESSAGE, "Item: ([^,]+)")[,2],
      EVENT_ID = row_number(),  # temporary unique ID
      TYPE = "start"
    )
  
  finishes <- df %>%
    filter(str_detect(MESSAGE, "^Finishing Category:")) %>%
    mutate(
      ROLE = str_match(MESSAGE, "Item: ([^,]+)")[,2],
      TYPE = "end"
    )
  
  bind_rows(starts, finishes) %>%
    arrange(DATE) %>%
    filter(!is.na(ROLE)) %>%
    mutate(EVENT_ID = ifelse(is.na(EVENT_ID), lag(EVENT_ID), EVENT_ID)) %>%
    select(-LEVEL, -SOURCE, - MEMBER, -LINE)
  
}



# WORK --------------------------------------------------------------------


# TODO:  Need to figure out the naming conventions for the NINA logs so I pull the correct one.
log_file_path <- "Python/log.log"

# Read file and keep only the time entries
logfile <- read_lines(log_file_path)
logfile <- logfile[str_detect(logfile, "^\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+")]


# Main data file will have start and stop times of each event
# From there it should be easy to group_by()
df <- read_delim(
  paste(logfile, collapse = "\n"),
  delim = "|",
  col_names = c("DATE", "LEVEL", "SOURCE", "MEMBER", "LINE", "MESSAGE"),
  trim_ws = TRUE,
  col_types = cols(.default = "c")
)  %>% 
  mutate(DATE = ymd_hms(DATE, tz = "UTC", quiet = TRUE)) %>%
  eventPairs() %>%
  mutate(ROLE = ifelse(stringr::str_detect(MESSAGE, "Flat") == TRUE, "FlatWizard", ROLE)) %>%
  mutate(ROLE = ifelse(ROLE %in% c("CenterAndRotate", "Center"), "Slew, rotate, platesolve", ROLE))




### DEV SECTION
# DEV Log does not have start/stop flags
## Inserting them for dev purposes
dev <- TRUE
if(dev) {
start <- df[148:149,]  # "WaitforTIme" - will just replace


foo <- df %>%
  filter(ROLE == "Annotation" & 
           str_detect(MESSAGE, "This group here will be executed in parallel.") == TRUE) 

foo <- foo %>%
  mutate(MESSAGE = stringr::str_replace(MESSAGE,
                                        "This group here will be executed in parallel.",
                                        "START SEQUENCE NOW")) 
start$MESSAGE <- foo$MESSAGE
start$ROLE <- "Annotation"
start$DATE[1] <- start$DATE[2] # simulate them as equal
df[148:149,] <- start

end <- df[1710:1711,]

foo <- foo %>%
  mutate(MESSAGE = stringr::str_replace(MESSAGE,
                                        "START SEQUENCE NOW",
                                        "END SEQUENCE NOW"))
end$MESSAGE <- foo$MESSAGE
end$ROLE <- "Annotation"
df[1710:1711,] <- end
rm(start, end, foo)
}



# There's a lot of trash in these logs.  I have an annotation for start/end the sequence.
# I can use these to subset to only the active sequence section
starttime <- df %>% 
  filter(ROLE == "Annotation" & 
         str_detect(MESSAGE, "START SEQUENCE NOW") == TRUE &
         TYPE == "start") %>% 
  pull(DATE)

endtime <- df %>%
  filter(ROLE == "Annotation" & 
           str_detect(MESSAGE, "END SEQUENCE NOW") == TRUE &
           TYPE == "end") %>% 
  pull(DATE)

df <- df %>%
  filter(DATE >= starttime & DATE <= endtime) %>%
  group_by(EVENT_ID) %>%
  mutate(TIME = ifelse(TYPE == "end", difftime(DATE, lag(DATE), units = "secs"), 0)) %>%
  ungroup()


report <- df %>%
  group_by(ROLE) %>%
  summarise(
    TotalMinutes = sum(TIME, na.rm = TRUE) / 60,
    N_events = n() / 2
  ) %>%
  mutate(AverageTime = TotalMinutes / N_events)

tots <- data.frame(
  ROLE = c("", "Total Event Time", "Total Sequence Time"),
  TotalMinutes = c(NA_real_, 
                   sum(report$TotalMinutes, na.rm = TRUE),
                   difftime(endtime, starttime, units = "mins")))

report <- report %>%
  bind_rows(tots)
  
colnames(report) <- c("Role", "Total Minutes", "N Events", "Average Time (mins)")



# Create RTF document
ht <- report %>% 
  huxtable() %>%
  set_all_padding(0.0) %>%
  huxtable::set_bold(1, 1:ncol(report)) %>%
  huxtable::set_top_padding(6) %>%
  huxtable::set_bottom_padding(6) %>%
  huxtable::set_top_border(1, 1:ncol(report), 1) %>%
  huxtable::set_bottom_border(1, 1:ncol(report), 1) %>%
  huxtable::set_width(1.5) %>%
  huxtable::set_font("arial") %>%
  huxtable::set_font_size(12) %>%
  huxtable::map_align(huxtable::by_cols("center"))
  
  
ht %>%
  pharmaRTF::rtf_doc(header_rows = 1) %>%
  set_orientation("portrait") %>%
  set_pagesize(c(height = 11, width = 8.5)) %>%
  set_margins(c(top = 0.5, bottom = 0.5, left = 0.5, right = 0.5)) %>%
  set_header_height(0.3) %>%
  set_footer_height(0.3) %>%
  set_ignore_cell_padding(TRUE) %>%
  add_titles(hf_line("Imaging Report", bold = TRUE, align = "left")) %>%
  add_titles(hf_line(glue::glue("Date: {Sys.Date()}"), align = "left")) %>%
  add_titles(hf_line("")) %>%
  saveRDS(file = glue::glue("NINA_Report_{Sys.Date()}.rds"))




# Plot Gantt chart
library(forcats)

events <- subset %>%
  filter(ROLE != "SwitchFilter") %>%
  select(EVENT_ID, ROLE, TYPE, DATE) %>%
  pivot_wider(names_from = TYPE, values_from = DATE) %>%
  filter(!is.na(start) & !is.na(end))


ggplot(events, aes(y = fct_rev(factor(ROLE)), x = start, xend = end, yend = fct_rev(factor(ROLE)))) +
  geom_segment(size = 6, color = "steelblue") +
  labs(
    title = "Gantt Chart of NINA Sequence Events",
    x = "Time",
    y = "Event"
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))



