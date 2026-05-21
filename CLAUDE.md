# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

A personal workshop of **R scripts** (plus shell/bat glue) that automate an astrophotography rig and turn raw capture output into an inventory + reports, alongside a newer **Next.js web app** (`astrodb-explorer/`) that browses the resulting database. It is *not* a packaged library — scripts are invoked by hand, by `cron`, by Windows Task Scheduler, or via `R CMD BATCH` from shell wrappers, and most assume specific working directories, drive mounts, and machine names.

`OVERVIEW.md` is the detailed, hand-written technical tour of the R side (pipeline diagram, module-by-module, gotchas). **Read it before doing substantial work on the R scripts.** Note one drift: it describes the database as SQLite, but the current code (`astroDB/functions/functions.R`) uses PostgreSQL — see "Database" below.

## Commands

There is **no test suite, linter config, or CI for the R code**, and `renv`/`packrat` is not used (packages are `library()`'d directly; the base set loads from `.Rprofile`).

**R scripts** — run interactively in RStudio, or as batch:
```bash
Rscript astroDB/astroDB.R                              # ETL inventory run
R CMD BATCH astroDB/astroDB.R logs/astroDB.Rout        # how Shell/astroDB.sh invokes it (Mac cron)
```
`queryastroDB.R` is meant to be sourced interactively: set `myObject <- "NGC281"`, then call `dbSummary()`, `objectTotalIntegration(myObject)`, `astrobinCSV(myObject)`, `objectMetaData(myObject)`. Output is `gt` tables rendered in the RStudio Viewer.

**astrodb-explorer** (Next.js 15 / React 19 / TypeScript / Tailwind):
```bash
cd astrodb-explorer
npm install
npm run dev      # local dev server
npm run build    # production build
npm run lint     # next lint — the only linting in the repo
npm start        # serve the production build
```
Requires `astrodb-explorer/.env.local` with `PG*` connection vars (gitignored; never commit it). Uses `PGGSSENCMODE=disable` to match the R client's `gssencmode = "disable"`.

## Architecture

### The two halves and how they connect

```
R scripts (capture → ETL)  ──writes──►  PostgreSQL "briancarter" @ aria-bot  ◄──reads──  astrodb-explorer (Next.js)
```

- **R scripts** run on a Windows telescope PC (capture/postprocessing) and a Mac (ETL/reporting). See `OVERVIEW.md` for the full pipeline, machine-detection logic, and hard-coded paths.
- **astrodb-explorer** is a thin read-only web UI over the same Postgres DB. Its `src/lib/queries.ts` is a deliberate **TypeScript re-implementation of the R reporting functions** in `astroDB/functions/functions.R` (`dbSummary`, `objectTotalIntegration`, `astrobinCSV`). If you change query semantics on one side, mirror it on the other — they are meant to agree.

### Database (current state — supersedes OVERVIEW.md's SQLite description)

The live DB is **PostgreSQL**, database `briancarter`, host `aria-bot`, port `5432`, `gssencmode=disable`, no SSL. Both clients connect the same way:
- R: `astroDB/functions/functions.R::connectDB()`, password from `Sys.getenv("sql-db-password")`.
- TS: `astrodb-explorer/src/lib/db.ts` (shared `pg` Pool, stashed on `globalThis` to survive Next.js hot-reload), password from `process.env.PGPASSWORD`.

Main table `astroSubs` — one row per frame; **column names are case-sensitive and quoted** (`"Object"`, `"FilterName"`, `"Duration"`, `"Status"`, `"SubFrameSelected"`, `"Date"`). The canonical query selection used everywhere is:
`"SubFrameSelected" = true AND "Status" <> 'Excluded' AND "Object" IS NOT NULL`.

> NOTE: `.Rprofile` defines a *different* `connectDB()` (old Raspberry-Pi Postgres at `100.85.227.75`). It is overridden when `astroDB/functions/functions.R` is sourced. Don't be misled by it.

### Cross-cutting conventions (match existing style)

- **Filter and duration encoding** is parsed from filename substrings (`_L_`, `_H_`, `_300.0`, …) via nested `ifelse` chains that recur in several files (`astroDB/functions`, `subframeSelector.R`, `splitByFilter.R`, postprocessing). They are *not* identical (different fallbacks). If you change filter handling, audit every call site, including the `FILTER_ORDER` / `ASTROBIN_FILTER_ID` maps in `astrodb-explorer/src/lib/queries.ts`.
- **Astrobin filter IDs** are hardcoded numeric mappings duplicated in R (`astrobinCSV`) and TS (`ASTROBIN_FILTER_ID`). Keep them in sync.
- **`%>%` (magrittr) + `dplyr`** throughout the R code, not native `|>`.
- **`<<-` global assignment** in the log-parsing functions (`thisNight`, `starttime`, `endtime`, `allRoles`); downstream functions read these globals — don't refactor to args without tracing all uses.
- **Time zones**: NINA logs are UTC; everything is converted to `America/New_York` in the log-parsing code. Preserve this when touching log code.
- **PixInsight integration is shell-out only** — R writes a `.bat` invoking WBPP and executes it; there is no API.

## Conventions for changing the rig / adding filters

When editing the R entry-point scripts (`*.R`, not the `functions/` files), preserve the `setwd()` line — `source("functions/functions.R")` calls assume the working directory. Machine detection is a duplicated block at the top of each entry-point script; if you change it, change it everywhere (it's known tech debt). See `OVERVIEW.md` § "Suggested entry points" and § "Known dead / stale code" for where things live and what's vestigial.
