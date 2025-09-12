library(dplyr)
library(magrittr)
library(gt)
options(dplyr.summarise.inform = FALSE)


# Functions ---------------------------------------------------------------
"/Volumes/Office-SSD/Astronomy/astro-tools/astroDB/functions/functions.R" %>% source()


# Run ------------------------------------------------------------------

myObject <- "LBN-357"

# Check the most recent logs
# Provides a summary of the most recent update time by Status
checkLogs() 

# Creates a .txt file on desktop for astrobin upload
astrobinCSV(myObject)

# Query the total integration for an object
objectTotalIntegration(myObject)

# Get the other metadata, guiding, HFR, FWHM, etc
objectMetaData(myObject)


# list all the objects and total integration time
dbSummary()