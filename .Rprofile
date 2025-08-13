rm(list = ls())
gc()


# System options ----------------------------------------------------------

options( keep.source = TRUE )
options( show.error.locations = TRUE )
options( mc.cores = 4L)
options(scipen=10)


# Packages ----------------------------------------------------------------
myPackages <- c("dplyr", "magrittr", "parallel",
                "openxlsx", "DBI", "glue", "readr", "testthat", "utils",
                "getPass", "RPostgreSQL")

suppressMessages({
     lapply(myPackages, require, character.only = TRUE, quietly = TRUE)
})



# Connect to my PostgreSQL database ---------------------------------------

connectDB <- function() {

  username <- Sys.getenv("username")
  password <- Sys.getenv("password")

     tryCatch({

          if (length(username) != 0 & length(password) != 0) {
            dbConnect(RPostgres::Postgres(),
                      dbname = "astroDB",
                      host = "100.85.227.75",
                      port = 5432,
                      user = username,
                      password = password,
                      sslmode = "prefer")

          } else {
            dbConnect(RPostgres::Postgres(),
                      dbname = "astroDB",
                      host = "100.85.227.75",
                      port = 5432,
                      user = getPass::getPass("Username"),
                      password = getPass::getPass("Password"),
                      sslmode = "prefer")
          }
     },
     error = function(cond) {
          message("Can't connect to Raspberry PI")
          message(cond)
          return(NA)
     }
     )
}



# Cleanup -----------------------------------------------------------------
rm(myPackages)

