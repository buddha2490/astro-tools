log_file_path <- file.path("Python/log.log")


grep("2025-07-08T21:51:38.9862", lines)



parse_nina_log <- function(log_file_path) {
  library(stringr)
  library(lubridate)
  library(dplyr)

  # Read the log file
  lines <- readLines(log_file_path)

  lines <- lines[695:length(lines)] %>% data.frame()
  
  write.table(lines, file = "Python/log.txt")
  
  df <- data.table::fread("Python/log.txt") 
                 
  table(df$MEMBER)
  
  
  
  write.csv(df, file = "Python/log.csv", row.names = FALSE, quote = FALSE, na = "")
  
  # Define event patterns
  event_patterns <- list(
    image_capture = list(
      start = "Starting Exposure - Exposure Time:",
      end_next_event = TRUE
    ),
    plate_solving = list(
      start = "Platesolving with parameters",
      end = "Platesolve (successful|failed)"
    ),
    filter_wheel = list(
      start = "Starting Category: Filter Wheel, Item: SwitchFilter",
      end = "Finishing Category: Filter Wheel, Item: SwitchFilter"
    ),
    autofocus = list(
      start = "Starting Category: Focuser, Item: RunAutofocus",
      end = "Finishing Category: Focuser, Item: RunAutofocus"
    ),
    meridian_flip = list(
      start = "Meridian Flip.*initiated|Starting Meridian Flip",
      end = "Meridian Flip.*completed|Completed Meridian Flip"
    )
  )

  # Helper to extract timestamp from log line
  extract_time <- function(line) {
    # Example format: 2024-05-14T23:04:15.735
    time_str <- str_extract(line, "\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}\\.\\d+")
    ymd_hms(time_str)
  }

  # Parse events
  results <- list()
  current_events <- list()

  for (i in seq_along(lines)) {
    
    line <- lines[i]
    for (event in names(event_patterns)) {
      
      pattern <- event_patterns[[event]]
      
      if (str_detect(line, pattern = as.character((pattern$start)))) {
        
        start_time <- extract_time(line)
        current_events[[event]] <- list(start_time = start_time, start_line = i, start_text = line)
        
      } else if (!is.null(pattern$end) && str_detect(line, pattern = as.character(pattern$end))) {
      
        if (!is.null(current_events[[event]])) {
          
          end_time <- extract_time(line)
          
          results[[length(results) + 1]] <- data.frame(
            event = event,
            start_time = current_events[[event]]$start_time,
            end_time = end_time,
            duration_sec = as.numeric(difftime(end_time, current_events[[event]]$start_time, units = "secs"))
          )
          
          current_events[[event]] <- NULL
        }
      }
    }
  }

  # Optionally close open events based on 'end_next_event'
  for (event in names(current_events)) {
    evt <- current_events[[event]]
    if (!is.null(evt)) {
      results[[length(results) + 1]] <- data.frame(
        event = event,
        start_time = evt$start_time,
        end_time = NA,
        duration_sec = NA,
        stringsAsFactors = FALSE
      )
    }
  }

  bind_rows(results)
}


parse_nina_log(log_file_path) %>%
  group_by(event)


