


# TODO:

1.  Implement postgresql to store data
2.  store metadata in the astroDB
3.  pull temperature, actual camera temperature, RMS, HFR/FWHM, see what I can add to the graphics.



install.packages(c("RPostgres"))



library(DBI)
library(RPostgreSQL)


username <- Sys.getenv("username")
password <- Sys.getenv("password")

foo <- dbConnect(dbDriver("PostgreSQL"),
                 dbname = "astro",
                 host = "bdcpi",
                 port = 5432,
                 user = "briancarter",
                 password = "Desiree1338",
                 sslmode = "require")
con <- DBI::dbConnect(
  RPostgres::Postgres(),
  host     = "bdcpi",   # or "bdcpi.local"
  port     = 5432,
  dbname   = "briancarter",          # don't leave dbname commented out
  user     = "briancarter",
  password = "Desiree1338",
  sslmode  = "require"         # matches a hostssl policy; remove if you allow non-SSL
)


dbListTables(con)

dbListTables(con)
dbCreateTable(con, "astroDB", data.frame())
dbWriteTable(con, "astroDB", data.frame(), row.names = FALSE, append = TRUE)
dbListTables(con)
dbDisconnect(con)
