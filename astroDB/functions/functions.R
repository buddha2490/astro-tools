connectDB <- function () {
  username <- Sys.getenv("username")
  password <- Sys.getenv("password")
  tryCatch({
    if (length(username) != 0 & length(password) != 0) {
      dbConnect(RPostgres::Postgres(), dbname = "astroDB", 
                host = "100.85.227.75", port = 5432, user = username, 
                password = password, sslmode = "prefer")
    }
    else {
      dbConnect(RPostgres::Postgres(), dbname = "astroDB", 
                host = "100.85.227.75", port = 5432, user = getPass::getPass("Username"), 
                password = getPass::getPass("Password"), sslmode = "prefer")
    }
  }, error = function(cond) {
    message("Can't connect to Raspberry PI")
    message(cond)
    return(NA)
  })
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
                                   pattern = ".fit")) %>%
    mutate(Filename = basename(FilePath)) %>%
    mutate(Object = basename(dirname(dirname(FilePath)))) %>%
    mutate(Date = stringr::str_extract(FilePath, "\\d{4}-\\d{2}-\\d{2}")) %>%
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
  dat %>%
    mutate(Object = basename(dirname(dirname(FilePath)))) %>%
    mutate(Filename = basename(FilePath)) %>%
    mutate(Date = stringr::str_remove(basename(dirname(FilePath)), paste0(Object, "_")) )%>%
    full_join(subsDf %>% 
                select(FilePathSubs = FilePath, Filename, FilterSubs, DurationSubs) %>% 
                mutate(inSubs = TRUE), 
              c("Filename")) %>%
    mutate(Status = ifelse(is.na(inSubs), "Excluded", ifelse(
      is.na(Date), "Missing Metadata", "Included"))) %>%
    mutate(Object = basename(dirname(dirname(FilePath))))  %>%
    mutate(FilePath = ifelse(is.na(FilePath), FilePathSubs, FilePath)) %>%
    mutate(FilterName = ifelse(is.na(FilterName), FilterSubs, FilterName)) %>%
    mutate(Duration = ifelse(is.na(Duration), DurationSubs, Duration)) %>%
    select(Object, Date, Filename, ExposureStart, FilterName, Duration, CameraTemp,
           Gain, ADUMean, DetectedStars, HFR, FWHM, Eccentricity, GuidingRMSArcSec,
           FocuserPosition, FocuserTemp, RotatorPosition, Status)
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
      group_by(Status) %>%
      dplyr::filter(row_number() == 1) %>%
      print()
    cat("Printing most recent update time by Status\n")
    dbDisconnect(astroDB)
  })
}

astrobinCSV <- function(myObject) {
  
  
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
  result %>% readr::write_csv(glue::glue("/Users/briancarter/Desktop/{myObject}_astrobin_subs.txt"))
  
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
    select(Filter, Number, `Duration\n(mins)` = Duration, `Duration\n(cum)` = Duration2) %>%
    gt(rowname_col = "row") %>%
    tab_header(
      title = md(glue::glue("Sub tally for {myObject} grouped by filter and sub duration"))
    ) 
  
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
  
  tbl(astroDB, "astroSubs") %>%
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
    arrange(Filter) %>%
    gt(rowname_col = "row") %>%
    tab_header(
      title = md(glue::glue("Sub metadata for {myObject} grouped by filter")),
      subtitle = md("Mean, min, and max values for each filter")
    ) 
  
}
