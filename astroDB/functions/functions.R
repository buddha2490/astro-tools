connectDB <- function () {
    DBI::dbConnect(
    RPostgres::Postgres(),
    dbname     = "briancarter",      # case-sensitive — must be exactly this
    host       = "aria-bot",
    port       = 5432,
    user       = "briancarter",
    password   = Sys.getenv("sql-db-password"),
    gssencmode = "disable"
  )
}



# Inventory one night: read EVERY row from ImageMetaData.csv, reconstruct the
# expected .xisf filename from the row columns (FilePath is often empty), then
# flag which files actually survived in the SubFrameSelected folder.
# nightDir example: ~/Astronomy/subs/M16/M16_2026-05-11
inventoryNight <- function(nightDir) {
  metaFile <- file.path(nightDir, "ImageMetaData.csv")
  sfsDir   <- file.path(nightDir, "SubFrameSelected")

  if (!file.exists(metaFile)) return(NULL)

  meta <- suppressMessages(readr::read_csv(metaFile, show_col_types = FALSE))
  if (nrow(meta) == 0) return(NULL)

  # Older PixInsight metadata files (e.g. M8andM20) omit the ImageType column.
  # Everything under subs/<object>/<night>/ is a light frame.
  if (!"ImageType" %in% names(meta)) meta$ImageType <- "LIGHT"

  selectedFiles <- if (dir.exists(sfsDir)) {
    basename(list.files(sfsDir, pattern = "\\.(xisf|fits?)$"))
  } else {
    character(0)
  }
  selectedStems <- sub("\\.(xisf|fits?)$", "", selectedFiles)
  selectedExts  <- stringr::str_extract(selectedFiles, "\\.(xisf|fits?)$")

  object <- basename(dirname(nightDir))
  date   <- stringr::str_extract(basename(nightDir), "\\d{4}-\\d{2}-\\d{2}")

  meta %>%
    mutate(Stem = sprintf("%s_%s_%s_%s_%.2f_%04d",
                          object, date, ImageType, FilterName,
                          Duration, ExposureNumber),
           Ext = ifelse(Stem %in% selectedStems,
                        selectedExts[match(Stem, selectedStems)],
                        ".xisf"),
           Filename = paste0(Stem, Ext),
           Object = object,
           Date = date,
           SubFrameSelected = Filename %in% selectedFiles,
           Status = ifelse(SubFrameSelected, "Included", "Excluded")) %>%
    select(-Stem, -Ext)
}

# Strip the trailing "_a" from .xisf files inside every SubFrameSelected folder
# under rootDir. e.g. M16_..._0024_a.xisf -> M16_..._0024.xisf.
# Skips any rename where the target name already exists. Returns a data frame
# of (from, to, renamed, skipped_conflict) for review.
stripASuffix <- function(rootDir) {
  objectDirs <- list.dirs(rootDir, recursive = FALSE)
  nightDirs  <- unlist(lapply(objectDirs, list.dirs, recursive = FALSE))
  sfsDirs    <- file.path(nightDirs, "SubFrameSelected")
  sfsDirs    <- sfsDirs[dir.exists(sfsDirs)]

  files <- list.files(sfsDirs, pattern = "_a\\.xisf$", full.names = TRUE)
  if (length(files) == 0) {
    message("No _a-suffixed .xisf files found under ", rootDir)
    return(invisible(NULL))
  }

  new <- stringr::str_replace(files, "_a\\.xisf$", ".xisf")
  conflict <- file.exists(new)

  if (any(conflict)) {
    warning(sum(conflict), " target file(s) already exist; those will be skipped")
  }

  ok <- rep(FALSE, length(files))
  ok[!conflict] <- file.rename(files[!conflict], new[!conflict])

  message(sum(ok), " of ", length(files), " files renamed")

  data.frame(
    from = files,
    to = new,
    renamed = ok,
    skipped_conflict = conflict,
    stringsAsFactors = FALSE
  )
}

# Walk every <object>/<night> under rootDir and assemble a combined data
# frame matching the astroSubs table schema.
buildInventory <- function(rootDir) {
  objectDirs <- list.dirs(rootDir, recursive = FALSE)
  nightDirs  <- unlist(lapply(objectDirs, list.dirs, recursive = FALSE))

  lapply(nightDirs, inventoryNight) %>%
    bind_rows() %>%
    select(Object, Date, Filename, ExposureStart, FilterName, Duration,
           CameraTemp, Gain, ADUMean, DetectedStars, HFR, FWHM, Eccentricity,
           GuidingRMSArcSec, FocuserPosition, FocuserTemp, RotatorPosition,
           Status, SubFrameSelected)
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
  df <- tbl(astroDB, "astroSubs") %>%
    filter(SubFrameSelected == TRUE)
  
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
  df <- tbl(astroDB, "astroSubs") %>%
    filter(SubFrameSelected == TRUE)
  
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

dbSummary <- function() {
  astroDB <- connectDB()
  df <- tbl(astroDB, "astroSubs") %>%
    filter(SubFrameSelected == TRUE)
  
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
    filter(SubFrameSelected == TRUE) %>%
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
