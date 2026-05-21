# Altitude Curve — Build Plan

A per-night sky-altitude chart for an object, modeled on the NINA target-altitude
graphic: the object's altitude vs. local clock time across one night, with
twilight/night shading and a hover readout that shows where individual sub-frames
were captured.

This document describes **how** to build it. It is deliberately **placement-agnostic**:
everything below is a self-contained module set with a single React component at the
edge that takes plain props, so it can be dropped onto an object page, a night page,
a modal, or anywhere else once that decision is made.

---

## 1. Goal

Reproduce (and slightly improve on) the NINA altitude widget:

- A smooth **altitude-vs-time curve** for one object, for one night, peaking at transit.
- **Shaded bands** for daylight → civil → nautical → astronomical twilight → full night
  (the graded light/dark regions in the reference image).
- A **hover tool** that follows the cursor along the curve; when it is over (or near)
  a captured frame it shows a small box with that frame's **exposure, filter, FWHM,
  and guiding RMS**.
- The curve is recomputed **per night** (it changes nightly).

Non-goals (for now): multi-night overlays, az/compass plots, moon avoidance, choosing
where the component lives in the app.

---

## 2. Decisions locked in

| Topic | Decision |
|---|---|
| RA/Dec source | **Hybrid**: bundled static catalog of our targets, with an online SIMBAD/Sesame name-resolver fallback for misses (result cached). |
| Sun-up / sun-down + twilight | **Computed locally** from lat/long + date. No online call. Produces graded civil/nautical/astronomical bands. |
| Astronomy math | **`astronomy-engine`** npm package (pure TS, MIT, no native build) for altitude, refraction, and sun events. |
| Observer site | Single fixed **Central-time** site, defined as a config constant (see §4.1). Exact lat/long still needed — see §12. |
| Timezone of frames | `ExposureStart` is treated as **naive local wall-clock** and plotted directly on the local-time x-axis (see §9). |

---

## 3. What the database already gives us (no R changes)

The `astroSubs` table already stores everything the hover box needs, per frame:

| Column | Use |
|---|---|
| `ExposureStart` | x-position of the frame on the time axis |
| `Duration` | "Exposure" in hover box |
| `FilterName` | "Filter" in hover box (color via existing `filters.ts`) |
| `FWHM` | hover box |
| `GuidingRMSArcSec` | "Guiding RMS" in hover box |
| `Object`, `Date` | scoping the query to one object + one night |

Query reuses the project-wide selection
`"SubFrameSelected" = true AND "Status" <> 'Excluded' AND "Object" IS NOT NULL`.

**What the DB does NOT have, and why this feature is computed:**

- **No RA/Dec** — the R ETL drops NINA's `MountRA`/`MountDec`. So the curve cannot be
  read from the DB; the app resolves coordinates from the object name (§6) and computes
  altitude itself (§7).
- **No clean UTC** — `ExposureStart` is stored as local clock time with a misleading `Z`
  suffix, and the true-UTC column is dropped. See §9 for how we handle this.

---

## 4. Module architecture

New code lives under `src/lib/astro/` (pure, server-safe logic) plus one client chart
component. Nothing here imports anything page-specific, so it stays pluggable.

```
src/lib/astro/
  site.ts          observer location + timezone config (one constant, swappable)
  coordinates.ts   object name -> {raDeg, decDeg}: static catalog + Sesame fallback + cache
  catalog.json     bundled static name -> RA/Dec table (our targets)
  altitude.ts      pure compute: altitude curve sampling + twilight bands (astronomy-engine)
  types.ts         astro-specific shapes (or fold into src/lib/types.ts — see below)

src/lib/queries.ts        + getNightFrames(object, date)   (DB read, follows existing style)
src/lib/types.ts          + NightFrame, AltSample, TwilightBands, AltitudeNight
src/app/api/object/[name]/night/[date]/altitude/route.ts   thin force-dynamic wrapper

src/app/components/charts/
  AltitudeCurveChart.tsx   the recharts client component (the only visible piece)
```

### 4.1 `site.ts` — observer config

```ts
export interface Site {
  name: string;
  latDeg: number;
  lonDeg: number;   // east-positive
  elevationM: number;
  timeZone: string; // IANA, e.g. "America/Chicago"
}

export const DEFAULT_SITE: Site = {
  name: "Central site",
  latDeg:  /* TBD — see §12 */,
  lonDeg:  /* TBD */,
  elevationM: 0,
  timeZone: "America/Chicago",
};
```

Keeping it a constant (not env) is fine for a single Central site; trivially promotable
to a lookup later if multiple sites appear.

### 4.2 `coordinates.ts` — name → RA/Dec (hybrid)

```ts
export interface SkyCoord { raDeg: number; decDeg: number; source: "catalog" | "sesame"; }
export async function resolveCoord(object: string): Promise<SkyCoord | null>;
```

Resolution order:

1. **Normalize** the name (`M16`, `M 16`, `Messier 16`, `NGC6611` → canonical key). One
   normalizer covering M/NGC/IC/Sh2/common-name spacing and case.
2. **Static catalog** (`catalog.json`): bundled map of our actual targets to J2000 RA/Dec
   in degrees. This is the primary path — offline, instant, deterministic. Seed it from
   the distinct `Object` values currently in the DB (one-time generation script, kept in
   `docs/` or `scripts/`).
3. **Sesame fallback**: for a name not in the catalog, query the CDS Sesame resolver
   (`https://cds.unistra.fr/cgi-bin/nph-sesame/...`) once, parse RA/Dec, **cache** the
   result (write-through into an in-memory map + optionally append to `catalog.json` via a
   dev script). Network failure → return `null` (chart degrades gracefully, §8).

This keeps normal operation fully offline while never hard-failing on a new target.

### 4.3 `altitude.ts` — the math (pure functions, no I/O)

```ts
// Altitude in degrees for one instant.
function altitudeAt(coord: SkyCoord, site: Site, whenUtc: Date): number;

// Sample the curve across a night. `nightLocalDate` is the civil date of the evening.
// Window runs local noon -> next local noon (matches the NINA 12->12 axis).
function altitudeCurve(coord, site, nightLocalDate, stepMinutes = 2): AltSample[];
//   AltSample = { tLocalMinutes: number; altDeg: number; isoUtc: string }

// Sun-event boundaries for the same window, as local-minute offsets.
function twilightBands(site, nightLocalDate): TwilightBands;
//   { sunset, civilEnd, nauticalEnd, astroEnd, astroStart, nauticalStart,
//     civilStart, sunrise }  // each = local minutes from window start (or null if N/A)
```

Implementation via `astronomy-engine`:
- Altitude: `Observer` + `Equator(Body/Star, ...)` → `Horizon(...)` with `"normal"`
  refraction, or `RotateVector` from an equatorial coord. For a fixed catalog RA/Dec we
  use the star-coordinate path (define an equatorial vector for the given RA/Dec and run
  `Horizon`). Sample every `stepMinutes` for a clean curve (~360–720 points/night at 2 min;
  cheap).
- Sun events: `SearchAltitude(Body.Sun, observer, dir, startTime, limitDays, altDeg)` at
  `-0.833°` (sunset/rise), `-6°` (civil), `-12°` (nautical), `-18°` (astronomical), for
  both the evening (descending) and morning (ascending) crossings. Each returned UTC time
  is converted to a local-minute offset for the x-axis.

### 4.4 `queries.ts` — the one DB read

Add, in the same style as `getObjectDetail`:

```ts
export async function getNightFrames(object: string, date: string): Promise<NightFrame[]>;
// SELECT "ExposureStart", "Duration", "FilterName", "FWHM", "GuidingRMSArcSec"
//   FROM "astroSubs"
//  WHERE <BASE_WHERE> AND "Object" = $1 AND "Date" = $2
//  ORDER BY "ExposureStart"
```

### 4.5 Server assembler

A single server-side builder (in `queries.ts` or a small `astro/night.ts`) ties it
together so callers get one object:

```ts
export async function getAltitudeNight(object, date): Promise<AltitudeNight | null> {
  const coord = await resolveCoord(object);
  if (!coord) return null;                 // unknown target -> caller shows a notice
  const frames = await getNightFrames(object, date);
  const curve  = altitudeCurve(coord, DEFAULT_SITE, date);
  const bands  = twilightBands(DEFAULT_SITE, date);
  const points = frames.map(f => ({ ...f, tLocalMinutes: localMinutes(f.ExposureStart),
                                    altDeg: sampleCurveAt(curve, ...) }));
  return { object, date, site: DEFAULT_SITE, coord, curve, bands, frames: points, transit };
}
```

### 4.6 API route (only if a client-fetch surface is wanted)

`src/app/api/object/[name]/night/[date]/altitude/route.ts` — thin `force-dynamic`
wrapper around `getAltitudeNight`, identical pattern to the existing `detail` route
(decode params, try/catch, 404 on null). If the host page is a server component, it can
call `getAltitudeNight` directly and skip the route; provide both so the component is
usable either way.

### 4.7 `AltitudeCurveChart.tsx` — the visible piece

Client component, props-only, no data fetching of its own:

```tsx
export default function AltitudeCurveChart({ data }: { data: AltitudeNight }) { ... }
```

This is the single integration point: whoever places the feature just renders
`<AltitudeCurveChart data={...} />` with data from `getAltitudeNight` (server) or the API
route (client). See §5 for its construction.

---

## 5. Chart construction (recharts)

Use a `ComposedChart` on shared x = local minutes-from-window-start, y = altitude (0–90°),
styled with the existing `ChartTheme.tsx` tokens so it matches the other charts.

Layers, back to front:

1. **Twilight shading** — `ReferenceArea` rectangles spanning the full y-range between the
   `twilightBands` boundaries, with increasing opacity from daylight → astronomical night
   (light gray → near-black), reproducing the graded look. Daylight regions before sunset
   and after sunrise get the lightest fill; astronomical night the darkest.
2. **Horizon / grid** — `CartesianGrid` horizontal lines; y ticks at 0/30/60/90 like the
   reference; x ticks every 3 hours formatted as local clock (`12, 15, 18, 21, 00, 03, 06,
   09, 12`).
3. **Altitude curve** — a single `Line` (no dots) over `data.curve`.
4. **Transit marker** — a `ReferenceDot` (or labeled dot) at the curve max, annotated with
   the peak altitude + "Transit", mirroring the screenshot.
5. **Captured frames** — a `Scatter` series over `data.frames` (x = `tLocalMinutes`, y =
   `altDeg`), small dots colored per filter via `filterColor()`. This is "where my subs
   were taken."
6. **"Now" line** — optional `ReferenceLine` at current local time when viewing tonight.

**Hover tool / readout:**

- A shared `Tooltip` with a vertical cursor line gives the slide-along-the-curve behavior
  for free; the cursor follows the mouse across x.
- A **custom tooltip renderer** decides what to show:
  - When the cursor's x is within a small threshold of a frame point → render the **frame
    box**: filter (colored chip + label), exposure (`Duration` s), FWHM (px), guiding RMS
    (″), and the local time.
  - Otherwise → a light readout of local time + interpolated altitude.
- To make frames easy to land on, either widen the Scatter hit area or snap the tooltip to
  the nearest frame within N minutes. (Recharts' active-shape / `activeDot` can drive the
  highlight on the focused frame.)

All colors/strings come from `ChartTheme.tsx` and `filters.ts`; no new palette.

---

## 6. Coordinate catalog generation (one-time, dev)

A small script (e.g. `scripts/build-catalog.ts`, run by hand) to seed `catalog.json`:

1. `SELECT DISTINCT "Object" FROM "astroSubs" WHERE "Object" IS NOT NULL`.
2. For each, normalize and resolve via Sesame.
3. Write `{ normalizedKey: { raDeg, decDeg, resolvedFrom } }` to `catalog.json`.
4. Commit the JSON. Re-run when new targets appear (or rely on the runtime fallback to
   cover them until the next regeneration).

This makes the static path cover 100% of existing targets out of the gate.

---

## 7. Altitude computation notes

- Coordinates are treated as **J2000**; `astronomy-engine` handles precession/refraction.
  Arcminute-level accuracy is far beyond what this chart needs.
- Sample at 2-minute steps for a visually smooth curve; interpolate linearly between
  samples when placing a frame's y from its exact time (`sampleCurveAt`).
- Altitudes below 0° (object under the horizon) are clamped/hidden so the curve sits on the
  axis like the reference (which flattens at 0).

---

## 8. Failure / edge handling

- **Unknown object** (no catalog hit, Sesame miss/offline): `getAltitudeNight` returns
  `null`; the component shows a tidy "altitude unavailable — coordinates not found" note
  instead of an empty chart. Everything else on the host page is unaffected.
- **Circumpolar / never-rises** targets: curve simply stays high / low; transit marker
  still placed at the max.
- **No frames that night**: curve + twilight render; no scatter dots. Still useful.

---

## 9. Timezone handling (the important gotcha)

`ExposureStart` in `astroSubs` is **local wall-clock time stored with a bogus `Z`** (the
true-UTC column was dropped by the ETL). Confirmed on a real file: a June M8 frame reads
`23:23Z` while true UTC was `03:23Z` (4 h off = Eastern local).

Approach, given the **Central-only** scope:

- Read `ExposureStart` as a **naive local datetime** (strip the `Z`; do not let JS treat it
  as UTC) and place it directly on the local-clock x-axis. No timezone conversion.
- The computed curve and twilight are in `DEFAULT_SITE`'s local time, so frame dots and
  curve align **as long as the capture site matches the configured site** — true for
  current Central data.
- **Known limitation:** historical nights captured at a non-Central site (e.g. the older
  Eastern sessions) will have their dots offset by the site's UTC difference relative to a
  Central-computed curve. Documented, accepted for v1. If we later need cross-site
  correctness, the fix is to capture true UTC + site per night (an R/ETL change, out of
  scope here) — leave a TODO marker noting this.

---

## 10. Caching & performance

- `getAltitudeNight` is deterministic given `(object, date)`; computing a night's curve is
  sub-millisecond-class work, so no caching is required for correctness. Optionally memoize
  per `(object, date)` in-process if a page renders many nights.
- Sesame results are cached in-memory (and ideally written back to `catalog.json` via the
  dev script) so the network is hit at most once per unknown name.

---

## 11. Dependency

Add to `astrodb-explorer/package.json`:

```
"astronomy-engine": "^2.x"
```

Pure TypeScript/JS, MIT, no native build step — safe for the Next.js build and Vercel-style
deploys. It is the only new runtime dependency.

---

## 12. Still needed from you

- **Exact observer lat/long** for the Central site (and elevation if handy). I've stubbed
  `DEFAULT_SITE` with `America/Chicago`; I just need the coordinates to fill in. A town name
  is enough — I can convert.

---

## 13. Suggested build sequence

1. Add `astronomy-engine`; write `site.ts` with the real coordinates.
2. `altitude.ts` — `altitudeCurve` + `twilightBands`; sanity-check against a known transit
   time for one target/date.
3. `coordinates.ts` + generate `catalog.json` from the DB's distinct objects.
4. `getNightFrames` + `getAltitudeNight` + types.
5. `AltitudeCurveChart.tsx` (curve + twilight + transit first; then scatter; then the hover
   box last).
6. API route (if a client surface is wanted).
7. Hand off `<AltitudeCurveChart data={...} />` for placement.

Each step is independently verifiable, and steps 1–4 have no UI dependency, so the math can
be validated before any chart work.
```
