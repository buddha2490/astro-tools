library(dplyr)
library(magrittr)
library(gt)
options(dplyr.summarise.inform = FALSE)


# Functions ---------------------------------------------------------------
"astroDB/functions/functions.R" %>% source()

# Run ------------------------------------------------------------------

myObject <- "M16"

# Check the most recent logs
# Provides a summary of the most recent update time by Status
checkLogs() 

# Creates a .txt file on desktop for astrobin upload
astrobinCSV(myObject, csv = FALSE)

# Query the total integration for an object
objectTotalIntegration(myObject)

# Get the other metadata, guiding, HFR, FWHM, etc
objectMetaData(myObject)


# list all the objects and total integration time
dbSummary()
