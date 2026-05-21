// Pure altitude / twilight math. No I/O, no DB, no React — safe to unit-test or
// call from the server assembler. Uses astronomy-engine (pure JS, MIT) for the
// celestial mechanics and Intl for DST-aware timezone conversion.
//
// The x-axis everywhere is "minutes from local noon" of the night's civil date,
// running 0 → 1440 (local noon → next local noon), matching the NINA 12→12 plot.

import {
  Body,
  Observer,
  DefineStar,
  Equator,
  Horizon,
  SearchAltitude,
} from "astronomy-engine";
import type { Site } from "./site";
import type { SkyCoord } from "./coordinates";
import type { AltSample, TwilightBands } from "../types";

const MS_PER_MIN = 60_000;
const WINDOW_MIN = 24 * 60; // local noon -> next local noon

// --- timezone helpers -------------------------------------------------------
// We never trust JS Date's local zone (the server's zone is irrelevant); all
// conversions go through the site's IANA zone explicitly.

// Offset (localWall - UTC) in ms for the instant `utcMs`, in zone `tz`.
function tzOffsetMs(utcMs: number, tz: string): number {
  const dtf = new Intl.DateTimeFormat("en-US", {
    timeZone: tz,
    hourCycle: "h23",
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    minute: "2-digit",
    second: "2-digit",
  });
  const p: Record<string, string> = {};
  for (const part of dtf.formatToParts(new Date(utcMs))) p[part.type] = part.value;
  const asUtc = Date.UTC(
    +p.year,
    +p.month - 1,
    +p.day,
    +p.hour,
    +p.minute,
    +p.second,
  );
  return asUtc - utcMs;
}

// A local wall-clock instant (expressed as ms "as if UTC") -> true UTC ms.
// Refines once so DST transitions resolve correctly.
function wallToUtcMs(wallMs: number, tz: string): number {
  const off1 = tzOffsetMs(wallMs, tz);
  let utc = wallMs - off1;
  const off2 = tzOffsetMs(utc, tz);
  if (off2 !== off1) utc = wallMs - off2;
  return utc;
}

// Civil-date string ("YYYY-MM-DD") -> ms for that day's local noon, as-if-UTC.
function noonWallMs(nightLocalDate: string): number {
  const [y, mo, d] = nightLocalDate.split("-").map(Number);
  return Date.UTC(y, mo - 1, d, 12, 0, 0);
}

// Civil date ("YYYY-MM-DD") of the observing window active at `site` for the
// instant `nowUtc`. The window runs local noon → next local noon, so the small
// hours (local time before noon) belong to the *previous* calendar date.
export function activeNightDate(site: Site, nowUtc: Date = new Date()): string {
  const localWall = new Date(nowUtc.getTime() + tzOffsetMs(nowUtc.getTime(), site.timeZone));
  // localWall's UTC fields hold the site's wall-clock components.
  let y = localWall.getUTCFullYear();
  let mo = localWall.getUTCMonth();
  let d = localWall.getUTCDate();
  if (localWall.getUTCHours() < 12) {
    const prev = new Date(Date.UTC(y, mo, d) - 24 * 60 * MS_PER_MIN);
    y = prev.getUTCFullYear();
    mo = prev.getUTCMonth();
    d = prev.getUTCDate();
  }
  return `${y}-${String(mo + 1).padStart(2, "0")}-${String(d).padStart(2, "0")}`;
}

// Minutes-from-local-noon for a true-UTC instant.
export function utcToMinutesFromNoon(
  utc: Date,
  nightLocalDate: string,
  tz: string,
): number {
  const localWallMs = utc.getTime() + tzOffsetMs(utc.getTime(), tz);
  return (localWallMs - noonWallMs(nightLocalDate)) / MS_PER_MIN;
}

// --- core altitude ----------------------------------------------------------

function makeObserver(site: Site): Observer {
  return new Observer(site.latDeg, site.lonDeg, site.elevationM);
}

// Define the catalog star once; astronomy-engine keeps it in a slot we reuse.
function defineTarget(coord: SkyCoord): Body {
  // RA is given in hours by astronomy-engine; our catalog stores degrees.
  DefineStar(Body.Star1, coord.raDeg / 15, coord.decDeg, 1000);
  return Body.Star1;
}

// Apparent altitude (deg, with normal refraction) of `coord` at instant `whenUtc`.
export function altitudeAt(
  body: Body,
  observer: Observer,
  whenUtc: Date,
): number {
  const eq = Equator(body, whenUtc, observer, true, true);
  return Horizon(whenUtc, observer, eq.ra, eq.dec, "normal").altitude;
}

// Sample the altitude curve across one night (local noon -> next local noon).
export function altitudeCurve(
  coord: SkyCoord,
  site: Site,
  nightLocalDate: string,
  stepMinutes = 2,
): AltSample[] {
  const observer = makeObserver(site);
  const body = defineTarget(coord);
  const noon = noonWallMs(nightLocalDate);

  const samples: AltSample[] = [];
  for (let t = 0; t <= WINDOW_MIN; t += stepMinutes) {
    const utc = new Date(wallToUtcMs(noon + t * MS_PER_MIN, site.timeZone));
    samples.push({
      tLocalMinutes: t,
      altDeg: altitudeAt(body, observer, utc),
      isoUtc: utc.toISOString(),
    });
  }
  return samples;
}

// Altitude of the target at one arbitrary local-minute offset (for placing a
// captured frame's dot exactly on the curve).
export function altitudeAtLocalMinute(
  coord: SkyCoord,
  site: Site,
  nightLocalDate: string,
  tLocalMinutes: number,
): number {
  const observer = makeObserver(site);
  const body = defineTarget(coord);
  const utc = new Date(
    wallToUtcMs(noonWallMs(nightLocalDate) + tLocalMinutes * MS_PER_MIN, site.timeZone),
  );
  return altitudeAt(body, observer, utc);
}

// --- twilight bands ---------------------------------------------------------

// Sun-event boundaries for the night, as minutes-from-local-noon (null if the
// event doesn't occur within the window — e.g. no astronomical night in
// midsummer at high latitude; not an issue at this site, but handled).
export function twilightBands(site: Site, nightLocalDate: string): TwilightBands {
  const observer = makeObserver(site);
  const startUtc = new Date(wallToUtcMs(noonWallMs(nightLocalDate), site.timeZone));

  // direction: -1 = descending (evening), +1 = ascending (morning).
  const event = (direction: -1 | 1, altDeg: number): number | null => {
    const t = SearchAltitude(Body.Sun, observer, direction, startUtc, 1, altDeg);
    return t ? utcToMinutesFromNoon(t.date, nightLocalDate, site.timeZone) : null;
  };

  return {
    sunset: event(-1, -0.833),
    civilEnd: event(-1, -6),
    nauticalEnd: event(-1, -12),
    astroEnd: event(-1, -18),
    astroStart: event(1, -18),
    nauticalStart: event(1, -12),
    civilStart: event(1, -6),
    sunrise: event(1, -0.833),
  };
}
