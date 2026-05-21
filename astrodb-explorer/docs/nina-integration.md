# NINA Integration

Live ingest of NINA captures into the `astroSubs` Postgres table, plus a typed
client for the [Advanced API plugin](https://github.com/christian-photo/ninaAPI).
Scoping/decisions: [`nina-integration-scoping.md`](./nina-integration-scoping.md).

## Status

- **Phase 1 — Ingest: built & verified.** Every LIGHT frame NINA saves is written
  to `astroSubs` within seconds via a persistent WebSocket.
- Phase 2 (live session UI), Phase 3 (backfill/reconciliation), Phase 4 (control):
  not started.

## Layout

```
src/lib/nina/
  config.ts            env-based connection config (NINA_HOST/PORT/API_KEY/ENABLED)
  types.ts             REST envelope, ImageStatistics, WS event union + guards
  client.ts            NinaClient — typed REST wrapper (+ binary image streaming)
  socket.ts            NinaSocket — WS client: auth handshake, reconnect, dispatch
  ingest/
    mapper.ts          IMAGE-SAVE → astroSubs record (pure, testable)
    store.ts           ImageStore interface + PostgresImageStore
    controller.ts      NinaIngest — wires socket → mapper → store
  index.ts             public exports
scripts/
  nina-verify.ts       live REST + WS verification against a running NINA
  nina-ingest.ts       the long-running worker entry point
  nina-ingest-verify.ts mapper + store tests (rolled-back txn, no permanent rows)
```

## Run

```bash
npm run nina:verify          # probe live NINA (REST + WS + a test capture)
npm run nina:ingest:verify   # verify mapper + store against the DB (rolls back)
npm run nina:ingest          # run the worker locally (uses .env.local)
```

Config: `NINA_HOST` (default `100.65.96.18`), `NINA_PORT` (1888), `NINA_API_KEY`
(none — the rig's plugin has auth disabled), `NINA_ENABLED`. DB creds from the
usual `PG*` vars in `.env.local`.

## Deploy (office-mac, launchd) — runbook

The worker runs on **office-mac** (always on, local SSD, reaches `aria-bot`). The
repo path there is `/Volumes/Office-SSD/Astronomy/astro-tools/astrodb-explorer`.

Prerequisites on office-mac:
1. **Run `npm install` ON office-mac** (don't rely on the laptop's). `tsx` pulls in
   `esbuild`, which has a platform-specific native binary; `node_modules` lives on
   the shared mount, so whichever machine ran `npm install` last wins. If you see an
   esbuild "wrong binary"/architecture error, `rm -rf node_modules && npm install` on office-mac.
2. `.env.local` exists with the `PG*` creds (gitignored — copy from the laptop or recreate).
3. NINA reachable: `curl -s http://100.65.96.18:1888/v2/api/version` returns JSON.

Install the service:
```bash
cp docs/nina-ingest.launchd.plist ~/Library/LaunchAgents/com.briancarter.nina-ingest.plist
launchctl load ~/Library/LaunchAgents/com.briancarter.nina-ingest.plist   # starts + runs at login
```

Verify it's live:
```bash
tail -f ~/Library/Logs/nina-ingest.log     # expect "DB ok" then "socket OPEN", heartbeat every 5 min
```
Then capture a LIGHT frame in NINA and watch for a `[nina-ingest] +` line. Confirm the
row in Postgres (`SELECT * FROM "astroSubs" WHERE "Status"='Captured' ORDER BY "ExposureStart" DESC LIMIT 1`).

Manage:
```bash
launchctl unload ~/Library/LaunchAgents/com.briancarter.nina-ingest.plist   # stop
launchctl kickstart -k gui/$(id -u)/com.briancarter.nina-ingest             # restart after a code change
```

> The plist uses the absolute nvm node path (`/Users/briancarter/.nvm/versions/node/v22.19.0/bin/node`)
> so launchd needs no shell init. If node is upgraded, update that path in the plist.

### Reliability note (learned the hard way)

The socket self-heals across NINA restarts/reboots: a *failed* reconnect attempt fires
only a WS `error` (no `close`), so `socket.ts` drives reconnect from **both** `error`
and `close` (idempotent). Don't "simplify" that to close-only — it silently kills the
reconnect loop after the first NINA outage.

## How ingest maps to `astroSubs`

`IMAGE-SAVE.Response.ImageStatistics` → row. **LIGHT frames only** (SNAPSHOT/FLAT/
DARK/BIAS skipped). Direct: `Object`←TargetName/filename, `Date`←filename token,
`Filename`, `ExposureStart`←Date, `Duration`←ExposureTime, `FilterName`←Filter,
`CameraTemp`←Temperature, `Gain`, `ADUMean`←Mean, `DetectedStars`←Stars, `HFR`.
Derived: `GuidingRMSArcSec` parsed from `RmsText`; `FocuserPosition/Temp` +
`RotatorPosition` fetched from the equipment endpoints at save time. NULL at
capture: `FWHM`, `Eccentricity` (PixInsight-only).

New rows are written with `Status = "Captured"` and `SubFrameSelected = true`, so
they appear in the explorer live while staying distinguishable from R-curated rows.

### Coexistence with the R pipeline ("NINA inserts, R updates")

NINA inserts the canonical row per frame, keyed on the **filename stem** (basename
minus extension minus a trailing `_a`). R builds the identical stem
(`{Object}_{Date}_{ImageType}_{FilterName}_{Duration:.2f}_{ExposureNumber:04d}`),
so both sides agree. Inserts are **idempotent**: `insertCapture` writes only if no
row with that stem exists, which makes reconnects safe and prevents duplicating a
frame the R pipeline already owns.

> **Pending R-side change (Brian):** update `astroDB/functions/functions.R` to
> *UPDATE* the existing NINA row by stem — filling `FWHM`/`Eccentricity`/
> `SubFrameSelected`/`Status` — instead of INSERTing a new `.xisf` row. Until then,
> a frame may exist as both a NINA `.fits` row and an R `.xisf` row.

## Known gaps

- No reconnect reconciliation yet (Phase 3): frames saved while the worker is down
  are not backfilled. The REST `image-history` endpoint is the intended source.
- `TS-NEWTARGETSTART` / `SEQUENCE-*` events are not yet consumed (TargetName comes
  directly off `IMAGE-SAVE`, so ingest doesn't need them).
