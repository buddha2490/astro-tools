library(dplyr)
library(magrittr)
library(gt)
options(dplyr.summarise.inform = FALSE)


# Functions ---------------------------------------------------------------
"/Volumes/Office-SSD/Astronomy/astro-tools/astroDB/functions/functions.R" %>% source()



# quick access quality data
meta <- getObjectMetadata("IC2177")
head(meta[[1]])
lapply(meta, function(x) {
  summary(x$FWHM)
})


# Run ------------------------------------------------------------------

myObject <- "M51"



# Check the most recent logs
# Provides a summary of the most recent update time by Status
checkLogs() 

# Creates a .txt file on desktop for astrobin upload
astrobinCSV(myObject, csv = F)

# Query the total integration for an object
objectTotalIntegration(myObject="M51")

# Get the other metadata, guiding, HFR, FWHM, etc
objectMetaData(myObject = "M33")


# list all the objects and total integration time
dbSummary()

con <- connectDB()


df <- tbl(con, "astroSubs") %>%
  filter(Object == "M51") %>%
  collect() 

table(df$Object)




dbWriteTable(con, "astroSubs", df, overwrite = TRUE)
