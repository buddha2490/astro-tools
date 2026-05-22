# AstroDB — RETIRED (2026-05-21)
#
# Sub ingest and curation no longer live in R. The pipeline is now:
#   1. NINA captures a frame  -> the astrodb-explorer ingest worker writes it to
#      "astroSubs" live (Status='Captured').  See astrodb-explorer/docs/nina-integration.md
#   2. You cull subs and drop the survivors in a folder, then use the explorer's
#      "Update subs" button to mark each frame Kept/Culled (the "Final" column).
#      See astrodb-explorer/src/lib/curation.ts
#
# This script used to inventory SubFrameSelected folders and INSERT rows keyed on
# the full filename, which now DOUBLE-COUNTS frames NINA already ingested
# (.fits row + .xisf row). Running it would reintroduce duplicates, so it is
# disabled. The reporting helpers in functions/functions.R (astrobinCSV,
# dbSummary, objectTotalIntegration, objectMetaData) are unaffected and still
# usable from queryastroDB.R. Original ETL is recoverable from git history.

stop(
  "astroDB.R is retired: sub ingest/curation moved to the astrodb-explorer app ",
  "(NINA ingest worker + the 'Update subs' button). Do not run this — it would ",
  "reintroduce duplicate rows. See astrodb-explorer/docs/nina-integration.md."
)
