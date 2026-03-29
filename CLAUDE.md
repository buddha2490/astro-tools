# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

R-based toolkit for managing astrophotography data from N.I.N.A. (Nighttime Imaging 'N' Astronomy). The repo handles the full pipeline: post-processing raw subs from the telescope PC, inventorying files into a PostgreSQL database, querying imaging statistics, and analyzing NINA session logs.

## Architecture

### astroDB/ — Database inventory system
- **astroDB.R**: Main inventory script. Scans FITS/XISF files on disk (`/Volumes/Office-SSD/Astronomy/Astrophotography`), cross-references with NINA's `ImageMetaData.csv` files, and writes the merged result to the `astroSubs` table in PostgreSQL. Runs via cron (`Shell/astroDB.sh`). Always does a full table replace (DELETE + INSERT), not incremental upsert.
- **queryastroDB.R**: Interactive query workbook. Uses functions from `astroDB/functions/functions.R` to query integration times, metadata stats, generate Astrobin CSV exports, etc.
- **functions/functions.R**: All DB query functions (`objectTotalIntegration`, `astrobinCSV`, `dbSummary`, `objectMetaData`, `checkLogs`) plus the `processMetadata()` and `subsInventory()` functions used by the inventory pipeline.

### postprocessing/ — Telescope-side processing pipeline
- **postProcessing.R**: Runs on the telescope Windows PC (`ES127`). Renames files, flags bad subs (>2 SD on stars/HFR/FWHM, eccentricity >0.6, guiding RMS >1"), copies matched darks/biases, generates PixInsight WBPP batch scripts for flat calibration.
- **cleanup.R**: Runs after WBPP. Renames master flats, transfers processed data to the Mac (`Office-Mac`) or D: drive, archives NINA logs.
- **functions/functions.R**: Processing functions (`processObjects`, `cleanup`, `bulkRename`, `wbppFlats`, `renameFlats`) and log analysis functions shared with NINALogAnalyzer.

### NINALogAnalyzer/ — Shiny web app
- **app.R**: Deployed to shinyapps.io. Parses NINA `.log` files, extracts start/end event pairs, produces a Gantt-style timeline (plotly) and summary table showing time spent on each activity (exposures, slewing, autofocus, meridian flip, etc.).
- **functions/functions.R**: Standalone version of log parsing functions (`pullLogs`, `eventPairs`, `cleanupLogs`, `times`, `logChartDev`) — simplified vs the postprocessing copy (no API calls, no file system operations).

### Standalone scripts
- **splitByFilter.R**: Splits FITS files into per-filter subdirectories for review in PixInsight's Blink.
- **subframeSelector.R**: Reads PixInsight SubframeSelector CSV output, flags bottom-10%/top-90% outliers on SNR/FWHM/Eccentricity/Stars, moves flagged files to an Exclusions folder.
- **bulkRename.R**: Renames files and updates `ImageMetaData.csv` when an object needs to be renamed (e.g., M31StarCloud → NGC206).

## Database

PostgreSQL on a Raspberry Pi (Tailscale IP `100.85.227.75`, port 5432, database `astroDB` or `briancarter`). Two connection patterns exist:
- `.Rprofile`: `connectDB()` using env vars `username` and `password`
- `astroDB/functions/functions.R`: `connectDB()` using env vars `pi-host`, `username`, `pi-db-password`

Tables: `astroSubs` (main inventory), `astroDBLog` (change log).

## Running Scripts

```bash
# Run astroDB inventory (via cron or manually)
R CMD BATCH --no-save --no-restore astroDB/astroDB.R logs/astroDB.Rout

# Run interactively
R -e 'source("astroDB/queryastroDB.R")'

# NINALogAnalyzer (local dev)
cd NINALogAnalyzer && R -e 'shiny::runApp()'
```

Post-processing scripts (`postProcessing.R`, `cleanup.R`) are designed to run on the telescope Windows PC and reference Windows paths.

## Key Conventions

- Filter codes: L (Luminance), R, G, B, H (H-alpha), S (Sulfur II), O (Oxygen III), HO, UVIR
- Filename pattern: `{Object}_{Date}_{Duration}_{Filter}_{sequence}.fits`
- Filter extraction from filenames is done via repeated `str_detect` chains (e.g., `_L_`, `_R_`, `_G_`) — this pattern is duplicated across multiple files
- Machine detection via `Sys.info()["nodename"]` determines path configuration (MBP13, MBP14, OfficeMac, TELESCOPE/ES127)
- Uses `magrittr` pipe (`%>%`) throughout, not base R pipe (`|>`)
- Tables rendered with `gt` package for interactive use, `DT` for Shiny
