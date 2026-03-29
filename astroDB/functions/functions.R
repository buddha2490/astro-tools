connectDB <- function () {
  DBI::dbConnect(
  RPostgres::Postgres(),
  host     = Sys.getenv("pi-host"),
  port     = 5432,
  dbname   = "briancarter",
  user     = Sys.getenv("username"),
  password = Sys.getenv("pi-db-password")
)
}


# This will change the file paths in the metadata files
# to match the /Volumes/Office-SSD location
changeMetaPath <- function(file, subsDf = subsDf) {
  
  # change only if necessary
  df <- readr::read_csv(file) %>%
    arrange(desc(ExposureNumber))
  
  name_order <- colnames(df)
  
  loc <- df %>%
    dplyr::filter(!is.na(FilePath)) %>% # drops excluded 
    arrange(desc(ExposureNumber)) %>%
    slice(1) %>%
    pull(FilePath)
  
  if ((stringr::str_detect(loc, "c:/users") == TRUE) |
      (stringr::str_detect(loc, "C:/Users") == TRUE)) {
    
    df <- df %>%
      mutate(Filename = basename(FilePath)) %>%
      mutate(Object = basename(dirname(dirname(FilePath)))) 
    
    
    subsList <- subsDf %>%
      dplyr::filter(Object == df$Object[1]) %>%
      dplyr::filter(Filename %in% df$Filename) %>%
      select(FilePathCorrect = FilePath, Filename)
    
    df %>%
      left_join(subsList, "Filename") %>%
      select(-FilePath, -Filename, -Object) %>%
      rename(FilePath = FilePathCorrect) %>%
      select(all_of(name_order)) %>% # re-order columns to match original
      readr::write_csv(file) # write out the new file paths to metadata file
  }
}

subsInventory <- function() {
  data.frame(FilePath = list.files(subdirs, all.files = TRUE,
                                   recursive = TRUE, full.names = TRUE,
                                   pattern = "\\.(fits?|xisf)$")) %>%
    dplyr::filter(!stringr::str_detect(FilePath, "master")) %>%   # exclude master calibration files
    mutate(Filename = basename(FilePath)) %>%
    # Extract Object and Date from filename (reliable regardless of subfolder depth)
    mutate(Object = stringr::str_extract(Filename, "^[^_]+")) %>%
    mutate(Date = stringr::str_extract(Filename, "\\d{4}-\\d{2}-\\d{2}")) %>%
    mutate(DurationSubs = ifelse(stringr::str_detect(Filename, "_30.00") == TRUE, 30, ifelse(
      stringr::str_detect(Filename, "_60.0") == TRUE, 60, ifelse(
        stringr::str_detect(Filename, "_120.0") == TRUE, 120, ifelse(
          stringr::str_detect(Filename, "_180.0") == TRUE, 180, ifelse(
            stringr::str_detect(Filename, "_300.0") == TRUE, 300, ifelse(
              stringr::str_detect(Filename, "_600.0") == TRUE, 600, NA_real_))))))) %>%
    mutate(FilterSubs = ifelse(stringr::str_detect(Filename, "_L_") == TRUE, "L", ifelse(
      stringr::str_detect(Filename, "_R_") == TRUE, "R", ifelse(
        stringr::str_detect(Filename, "_G_") == TRUE, "G", ifelse(
          stringr::str_detect(Filename, "_B_") == TRUE, "B", ifelse(
            stringr::str_detect(Filename, "_H_") == TRUE, "H", ifelse(
              stringr::str_detect(Filename, "_S_") == TRUE, "S", ifelse(
                stringr::str_detect(Filename, "_O_") == TRUE, "O", ifelse(
                  stringr::str_detect(Filename, "UVIR") == TRUE, "UVIR", ifelse(
                    stringr::str_detect(Filename, "_HO_") == TRUE, "HO", ""))))))))))
}

processMetadata <- function(dat) {
  # Start from the file system inventory (subsDf) as the source of truth.
  # Only files that actually exist on disk will be in the final result.

  # Prepare metadata from CSVs - extract Object and Filename for joining
  csvMeta <- dat %>%
    mutate(Object = basename(dirname(dirname(FilePath)))) %>%
    mutate(Filename = basename(FilePath)) %>%
    mutate(MetaDate = stringr::str_remove(basename(dirname(FilePath)), paste0(Object, "_"))) %>%
    # Deduplicate: CSVs can have duplicate entries for the same file
    # (e.g., session interrupted and restarted). Keep the last entry.
    group_by(Filename, Object) %>%
    slice_tail(n = 1) %>%
    ungroup()

  # Join: start with actual files on disk, left join to CSV metadata
  # This ensures only files that exist on disk are included
  matched <- subsDf %>%
    left_join(
      csvMeta %>% select(-FilePath),
      by = c("Filename", "Object")
    ) %>%
    mutate(Status = ifelse(!is.na(MetaDate), "Included", "Missing Metadata")) %>%
    # Use CSV metadata where available, fall back to filename-derived values
    mutate(FilterName = ifelse(is.na(FilterName), FilterSubs, FilterName)) %>%
    mutate(Duration = ifelse(is.na(Duration), DurationSubs, Duration)) %>%
    mutate(Date = ifelse(!is.na(MetaDate), MetaDate, Date)) %>%
    select(Object, Date, Filename, ExposureStart, FilterName, Duration, CameraTemp,
           Gain, ADUMean, DetectedStars, HFR, FWHM, Eccentricity, GuidingRMSArcSec,
           FocuserPosition, FocuserTemp, RotatorPosition, Status)

  return(matched)
}

fixMissingMeta <- function(metadata = metadata) {
  # Deal with missing metadata
  # Still missing for some "Excluded" - that's ok
  metadata$order <- seq_len(nrow(metadata))
  missing <- metadata %>% dplyr::filter(is.na(Object))
  notmissing <- metadata %>% anti_join(missing)
  
  missing$Object <- stringr::str_split(missing$Filename, "_") %>%
    sapply(function(x) x[1]) # get the object name from the filename
  
  missing$Date <- stringr::str_split(missing$Filename, "_") %>%
    sapply(function(x) x[2]) # get the date from the filename
  
  missing %>% bind_rows(notmissing) %>%
    arrange(order) %>%
    select(-order)
  
}

checkLogs <- function() {
  astroDB <- connectDB()
  suppressWarnings({
    tbl(astroDB, "astroDBLog") %>%
      arrange(desc(Timestamp)) %>%
      dplyr::filter(!Status %in% c("Added new metadata", "No new metadata")) %>%
      group_by(Status) %>%
      dplyr::filter(row_number() == 1) %>%
      collect() %>%
      mutate(Timestamp = as.POSIXct(Timestamp, tz = "EST")) %>%
      print() 
    cat("Printing most recent update time by Status\n")
    dbDisconnect(astroDB)
  })
}

astrobinCSV <- function(myObject, csv = FALSE) {
  
  
  astroDB <- connectDB()
  df <- tbl(astroDB, "astroSubs")
  
  # taken from the astrobin URL for each filter
  filterIDs <- c(4388, 4392, 4396, 5642, 5646, 5652, 5656, 6901, 13400)
  
  result <- df %>% 
    dplyr::filter(Object == myObject) %>%
    dplyr::filter(Status != "Excluded") %>%
    group_by(Date, FilterName, Duration) %>%
    summarize(number = n()) %>%
    ungroup() %>%
    select(date = Date, filter = FilterName, number, duration = Duration) %>%
    collect() %>%
    arrange((date)) %>%
    mutate(filter = factor(filter,
                           levels = c("H", "O", "S", "B", "G", "L", "R",
                                      "HO", "UVIR"),
                           labels = filterIDs))
  
  dbDisconnect(astroDB)
  
  # save as a txt so it will open in textedit
  if (csv == TRUE) {
  result %>% readr::write_csv(glue::glue("/Users/briancarter/Desktop/{myObject}_astrobin_subs.txt"))
  }
  
  # Print in the viewer
  result %>%
    mutate(filter = factor(filter,
                           levels = filterIDs,
                           labels = c("H", "O", "S", "B", "G", "L", "R",
                                      "HO", "UVIR"))) %>%
    gt(rowname_col = "row") %>%
    tab_header(
      title = md(glue::glue("Sub tally for {myObject} formatted for Astrobin")))
}

objectTotalIntegration <- function(myObject) {
  
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
                           levels = c("L", "R", "G", "B",  "H","S", "O","HO", "UVIR"),
                           labels = c(labels, "HO", "UVIR"))) %>%
    arrange(Filter) %>%
    mutate(Duration2 = cumsum(Duration)) %>%
    mutate(DurationHrs = round(Duration / 60, 1)) %>%
    mutate(DurationHrsCum = round(cumsum(DurationHrs), 1)) %>%
    select(Filter, Number, `Duration\n(mins)` = Duration, `Duration\n(cum)` = Duration2,
           `Duration\n(hours)` = DurationHrs, `Hours\n(cum)` = DurationHrsCum) %>%
    gt(rowname_col = "row") %>%
    tab_header(
      title = md(glue::glue("Sub tally for {myObject} grouped by filter and sub duration"))
    ) %>%
    cols_align(align = "left", columns = Filter) %>%
    cols_align(align = "center", columns = c(Number, `Duration\n(mins)`, `Duration\n(cum)`,
                                             `Duration\n(hours)`, `Hours\n(cum)`))
  
}

dbSummary <- function() {
  astroDB <- connectDB()
  df <- tbl(astroDB, "astroSubs")
  
  objects <- df %>%
    dplyr::filter(!is.na(Object)) %>%
    distinct(Object) %>%
    pull(Object) %>%
    as.character()
  
  result <- lapply(objects, function(myObject) {
    foo <-  objectTotalIntegration(myObject) %>%
      data.frame() %>%
      mutate(Object = myObject) %>%
      mutate(Filter = ifelse(Filter %in% c("Luminance", "Red", "Green", "Blue"), "LRGB", "Narrowband")) %>%
      group_by(Object, Filter) %>%
      summarize(integration = sum(as.numeric(Duration..mins.))) %>%
      ungroup() %>%
      tidyr::pivot_wider(id_cols= Object,  names_from = Filter, values_from = integration)
  }) %>%
    do.call("bind_rows", .) %>%
    rowwise() %>%
    mutate(`Total(mins)` = sum(LRGB, Narrowband, na.rm = TRUE)) %>%
    
    gt(rowname_col = "row") %>%
    tab_header(
      title = md(glue::glue("Total integration for all objects in astroDB"))) %>%
    sub_missing(
      columns = everything(),
      missing_text = ""
    )
  dbDisconnect(astroDB)
  return(result)
}


# glue friendly wrappers
meanFMT <- function(var) {
  format(round(mean(var, na.rm = TRUE), 1), nsmall = 1)
}
minFMT <- function(var) {
  format(round(min(var, na.rm = TRUE), 1), nsmall = 1)
}
maxFMT <- function(var) {
  format(round(max(var, na.rm = TRUE), 1), nsmall = 1)
}

# Other metdata for an object
objectMetaData <- function(myObject) {
  
  astroDB <- connectDB()
  
  df <- tbl(astroDB, "astroSubs") %>%
    dplyr::filter(!is.na(Object)) %>%
    dplyr::filter(Object == myObject) %>%
    group_by(FilterName) %>%
    collect() %>%
    summarize(
      Duration = sum(Duration, na.rm = TRUE) / 60,
      GuidingRMS = glue::glue("{meanFMT(GuidingRMSArcSec)}\n({minFMT(GuidingRMSArcSec)}-{maxFMT(GuidingRMSArcSec)})"),
      DetectedStars = glue::glue("{meanFMT(DetectedStars)}\n({minFMT(DetectedStars)}-{maxFMT(DetectedStars)})"),
      ADUMean = glue::glue("{meanFMT(ADUMean)}\n({minFMT(ADUMean)}-{maxFMT(ADUMean)})"),
      HFR = glue::glue("{meanFMT(HFR)}\n({minFMT(HFR)}-{maxFMT(HFR)})"),
      FWHM = glue::glue("{meanFMT(FWHM)}\n({minFMT(FWHM)}-{maxFMT(FWHM)})")) %>%
    select(Filter = FilterName, `Duration\n(mins)` = Duration, GuidingRMS, DetectedStars,
           ADUMean, HFR, FWHM) %>%
    mutate(Filter = factor(Filter, levels = c("L", "R", "G", "B", "H", "S", "O","UVIR", "HO"))) %>%
    arrange(Filter) 
  
  dbDisconnect(astroDB)
  
  df %>%
    gt(rowname_col = "row") %>%
    tab_header(
      title = md(glue::glue("Sub metadata for {myObject} grouped by filter")),
      subtitle = md("Mean, min, and max values for each filter")
    ) 
}
