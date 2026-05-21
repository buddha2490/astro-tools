import { query } from "./db";
import {
  FILTER_ORDER,
  FILTER_LABELS,
  ASTROBIN_FILTER_ID,
  NARROWBAND,
  filterRank,
} from "./filters";
import type {
  SummaryRow,
  IntegrationRow,
  AstrobinRow,
  DashboardData,
  ObjectCard,
  OverviewStats,
  FilterSlice,
  ObjectDetail,
  SessionRow,
  QualityStat,
  AltitudeNight,
  NightFrame,
  CatalogObject,
  CatalogListData,
} from "./types";
import { CATALOGS, type CatalogSlug } from "./catalog";
import { getObjectImagePaths } from "./images";
import { DEFAULT_SITE } from "./astro/site";
import { resolveCoord, normalizeName, type SkyCoord } from "./astro/coordinates";
import {
  altitudeCurve,
  twilightBands,
  altitudeAtLocalMinute,
  activeNightDate,
  utcToMinutesFromNoon,
} from "./astro/altitude";

// ---------------------------------------------------------------------------
// Translations of the R functions in astroDB/functions/functions.R.
//
// Every view starts from the same selection the R code uses: frames that were
// kept by SubFrameSelector, not excluded, and attached to a named object.
// Filter ordering / labels / Astrobin IDs live in ./filters.ts (shared with
// the client) — keep them in sync with the R encoding.
// ---------------------------------------------------------------------------

const BASE_WHERE = `"SubFrameSelected" = true AND "Status" <> 'Excluded' AND "Object" IS NOT NULL`;

const round1 = (n: number) => Math.round(n * 10) / 10;

// Round to 2 dp, tolerating null/undefined (returns null) — for quality means.
const round2 = (n: number | null | undefined): number | null =>
  n === null || n === undefined ? null : Math.round(n * 100) / 100;

// --- dbSummary --------------------------------------------------------------
// Per object, total integration (minutes) split into LRGB vs Narrowband.
export async function getSummary(): Promise<SummaryRow[]> {
  const rows = await query<{ object: string; grp: string; minutes: number }>(
    `SELECT "Object" AS object,
            CASE WHEN "FilterName" IN ('L','R','G','B') THEN 'lrgb'
                 ELSE 'narrowband' END AS grp,
            SUM("Duration")::float8 / 60 AS minutes
     FROM "astroSubs"
     WHERE ${BASE_WHERE}
     GROUP BY object, grp`,
  );

  const byObject = new Map<string, SummaryRow>();
  for (const r of rows) {
    const entry =
      byObject.get(r.object) ??
      { object: r.object, lrgb: null, narrowband: null, total: 0 };
    if (r.grp === "lrgb") entry.lrgb = r.minutes;
    else entry.narrowband = r.minutes;
    byObject.set(r.object, entry);
  }

  return Array.from(byObject.values())
    .map((e) => ({
      object: e.object,
      lrgb: e.lrgb === null ? null : round1(e.lrgb),
      narrowband: e.narrowband === null ? null : round1(e.narrowband),
      total: round1((e.lrgb ?? 0) + (e.narrowband ?? 0)),
    }))
    .sort((a, b) => b.total - a.total);
}

// --- Dashboard --------------------------------------------------------------
// Aggregate headline stats + per-object cards with their filter breakdown.
// Two queries: one rollup row, one per (object, filter) group.
export async function getDashboard(): Promise<DashboardData> {
  const [statsRows, breakdown] = await Promise.all([
    query<{
      hours: number;
      targets: number;
      frames: number;
      nights: number;
      first_date: string | null;
      last_date: string | null;
    }>(
      `SELECT SUM("Duration")::float8 / 3600        AS hours,
              COUNT(DISTINCT "Object")::int          AS targets,
              COUNT(*)::int                          AS frames,
              COUNT(DISTINCT "Date")::int            AS nights,
              MIN("Date")                            AS first_date,
              MAX("Date")                            AS last_date
       FROM "astroSubs"
       WHERE ${BASE_WHERE}`,
    ),
    query<{
      object: string;
      filter_name: string;
      minutes: number;
      frames: number;
      last_date: string | null;
    }>(
      `SELECT "Object"                       AS object,
              "FilterName"                   AS filter_name,
              SUM("Duration")::float8 / 60   AS minutes,
              COUNT(*)::int                  AS frames,
              MAX("Date")                    AS last_date
       FROM "astroSubs"
       WHERE ${BASE_WHERE}
       GROUP BY object, filter_name`,
    ),
  ]);

  const s = statsRows[0];
  const stats: OverviewStats = {
    totalHours: round1(s?.hours ?? 0),
    targetCount: s?.targets ?? 0,
    frameCount: s?.frames ?? 0,
    nightCount: s?.nights ?? 0,
    firstDate: s?.first_date ?? null,
    lastDate: s?.last_date ?? null,
  };

  const byObject = new Map<string, ObjectCard>();
  for (const r of breakdown) {
    const card =
      byObject.get(r.object) ??
      ({
        object: r.object,
        totalMinutes: 0,
        frames: 0,
        lastDate: null,
        isNarrowband: false,
        filters: [] as FilterSlice[],
      } satisfies ObjectCard);

    card.totalMinutes += r.minutes;
    card.frames += r.frames;
    if (r.last_date && (!card.lastDate || r.last_date > card.lastDate)) {
      card.lastDate = r.last_date;
    }
    card.filters.push({
      code: r.filter_name,
      minutes: round1(r.minutes),
      frames: r.frames,
    });
    byObject.set(r.object, card);
  }

  const objects = Array.from(byObject.values())
    .map((c) => {
      c.filters.sort((a, b) => filterRank(a.code) - filterRank(b.code));
      c.totalMinutes = round1(c.totalMinutes);
      c.isNarrowband = c.filters.some((f) => NARROWBAND.has(f.code));
      return c;
    })
    .sort((a, b) => b.totalMinutes - a.totalMinutes);

  // Tile thumbnails: imaged objects show the user's own uploaded image;
  // planned (catalog) targets show their downloaded DSS image. Then append
  // planned targets as empty tiles, skipping any we've actually imaged.
  const [planned, dssPaths, uploadPaths] = await Promise.all([
    getPlannedTargets(),
    getDSSImagePaths(),
    getObjectImagePaths(),
  ]);

  for (const card of objects) {
    card.imagePath = uploadPaths.get(card.object) ?? null;
  }

  const capturedNorm = new Set(objects.map((o) => normalizeName(o.object)));
  for (const p of planned) {
    if (capturedNorm.has(normalizeName(p.designation))) continue;
    objects.push({
      object: p.designation,
      totalMinutes: 0,
      frames: 0,
      lastDate: null,
      isNarrowband: false,
      filters: [],
      planned: true,
      catalogSlug: p.catalog_slug,
      objectType: p.object_type,
      constellation: p.constellation,
      imagePath: dssPaths.get(p.designation) ?? null,
    });
  }

  return { stats, objects };
}

// Returns a map of object name -> file_path for all downloaded DSS images.
// Silently returns empty map if the table doesn't exist yet.
async function getDSSImagePaths(): Promise<Map<string, string>> {
  try {
    const rows = await query<{ object: string; file_path: string }>(
      `SELECT object, file_path FROM dss_images`,
    );
    return new Map(rows.map((r) => [r.object, r.file_path]));
  } catch {
    return new Map();
  }
}

// --- Object detail ----------------------------------------------------------
// Everything the /object/[name] page needs: per-filter totals, a chronological
// session timeline (minutes per filter + nightly mean quality), and aggregate
// quality stats. Surfaces the per-frame metric columns the ETL ingests but the
// rest of the app doesn't use (HFR, FWHM, Eccentricity, GuidingRMSArcSec).
export async function getObjectDetail(object: string): Promise<ObjectDetail | null> {
  const [byDateFilter, byDate, overall] = await Promise.all([
    query<{ date: string; filter_name: string; minutes: number; frames: number }>(
      `SELECT "Date" AS date,
              "FilterName" AS filter_name,
              SUM("Duration")::float8 / 60 AS minutes,
              COUNT(*)::int AS frames
       FROM "astroSubs"
       WHERE ${BASE_WHERE} AND "Object" = $1
       GROUP BY date, filter_name`,
      [object],
    ),
    query<{
      date: string;
      hfr: number | null;
      fwhm: number | null;
      ecc: number | null;
      guiding: number | null;
    }>(
      `SELECT "Date" AS date,
              AVG("HFR")::float8           AS hfr,
              AVG("FWHM")::float8          AS fwhm,
              AVG("Eccentricity")::float8  AS ecc,
              AVG("GuidingRMSArcSec")::float8 AS guiding
       FROM "astroSubs"
       WHERE ${BASE_WHERE} AND "Object" = $1
       GROUP BY date`,
      [object],
    ),
    query<{
      minutes: number;
      frames: number;
      nights: number;
      first_date: string | null;
      last_date: string | null;
      hfr_avg: number | null; hfr_min: number | null; hfr_max: number | null;
      fwhm_avg: number | null; fwhm_min: number | null; fwhm_max: number | null;
      ecc_avg: number | null; ecc_min: number | null; ecc_max: number | null;
      guiding_avg: number | null; guiding_min: number | null; guiding_max: number | null;
      stars_avg: number | null; stars_min: number | null; stars_max: number | null;
    }>(
      `SELECT SUM("Duration")::float8 / 60 AS minutes,
              COUNT(*)::int                AS frames,
              COUNT(DISTINCT "Date")::int  AS nights,
              MIN("Date")                  AS first_date,
              MAX("Date")                  AS last_date,
              AVG("HFR")::float8 AS hfr_avg, MIN("HFR")::float8 AS hfr_min, MAX("HFR")::float8 AS hfr_max,
              AVG("FWHM")::float8 AS fwhm_avg, MIN("FWHM")::float8 AS fwhm_min, MAX("FWHM")::float8 AS fwhm_max,
              AVG("Eccentricity")::float8 AS ecc_avg, MIN("Eccentricity")::float8 AS ecc_min, MAX("Eccentricity")::float8 AS ecc_max,
              AVG("GuidingRMSArcSec")::float8 AS guiding_avg, MIN("GuidingRMSArcSec")::float8 AS guiding_min, MAX("GuidingRMSArcSec")::float8 AS guiding_max,
              AVG("DetectedStars")::float8 AS stars_avg, MIN("DetectedStars")::float8 AS stars_min, MAX("DetectedStars")::float8 AS stars_max
       FROM "astroSubs"
       WHERE ${BASE_WHERE} AND "Object" = $1`,
      [object],
    ),
  ]);

  if (!byDateFilter.length) return null;

  // Per-filter totals.
  const filterTotals = new Map<string, FilterSlice>();
  for (const r of byDateFilter) {
    const f = filterTotals.get(r.filter_name) ?? {
      code: r.filter_name,
      minutes: 0,
      frames: 0,
    };
    f.minutes += r.minutes;
    f.frames += r.frames;
    filterTotals.set(r.filter_name, f);
  }
  const filters = Array.from(filterTotals.values())
    .map((f) => ({ ...f, minutes: round1(f.minutes) }))
    .sort((a, b) => filterRank(a.code) - filterRank(b.code));

  // Sessions, chronological, with nightly mean quality merged in.
  const qualityByDate = new Map(byDate.map((r) => [r.date, r]));
  const sessions = new Map<string, SessionRow>();
  for (const r of byDateFilter) {
    const s =
      sessions.get(r.date) ??
      ({
        date: r.date,
        minutes: 0,
        frames: 0,
        byFilter: {},
        hfr: null,
        fwhm: null,
        eccentricity: null,
        guidingRms: null,
      } satisfies SessionRow);
    s.minutes += r.minutes;
    s.frames += r.frames;
    s.byFilter[r.filter_name] = round1((s.byFilter[r.filter_name] ?? 0) + r.minutes);
    sessions.set(r.date, s);
  }
  const sessionList = Array.from(sessions.values())
    .map((s) => {
      const q = qualityByDate.get(s.date);
      return {
        ...s,
        minutes: round1(s.minutes),
        hfr: round2(q?.hfr),
        fwhm: round2(q?.fwhm),
        eccentricity: round2(q?.ecc),
        guidingRms: round2(q?.guiding),
      };
    })
    .sort((a, b) => a.date.localeCompare(b.date));

  const o = overall[0];
  const quality: QualityStat[] = [
    qStat("HFR", "hfr", o?.hfr_avg, o?.hfr_min, o?.hfr_max, "px", true),
    qStat("FWHM", "fwhm", o?.fwhm_avg, o?.fwhm_min, o?.fwhm_max, "px", true),
    qStat("Eccentricity", "eccentricity", o?.ecc_avg, o?.ecc_min, o?.ecc_max, "", true),
    qStat("Guiding RMS", "guidingRms", o?.guiding_avg, o?.guiding_min, o?.guiding_max, "″", true),
    qStat("Detected Stars", "detectedStars", o?.stars_avg, o?.stars_min, o?.stars_max, "", false),
  ];

  return {
    object,
    totalMinutes: round1(o?.minutes ?? 0),
    frames: o?.frames ?? 0,
    nights: o?.nights ?? 0,
    firstDate: o?.first_date ?? null,
    lastDate: o?.last_date ?? null,
    isNarrowband: filters.some((f) => NARROWBAND.has(f.code)),
    filters,
    sessions: sessionList,
    quality,
  };
}

function qStat(
  metric: string,
  key: string,
  mean: number | null | undefined,
  min: number | null | undefined,
  max: number | null | undefined,
  unit: string,
  lowerIsBetter: boolean,
): QualityStat {
  return {
    metric,
    key,
    mean: round2(mean),
    min: round2(min),
    max: round2(max),
    unit,
    lowerIsBetter,
  };
}

// --- objectTotalIntegration -------------------------------------------------
// Per filter: frame count, minutes, and cumulative minutes in canonical order.
export async function getIntegration(object: string): Promise<IntegrationRow[]> {
  const rows = await query<{ filter_name: string; number: number; minutes: number }>(
    `SELECT "FilterName" AS filter_name,
            COUNT(*)::int AS number,
            SUM("Duration")::float8 / 60 AS minutes
     FROM "astroSubs"
     WHERE ${BASE_WHERE} AND "Object" = $1
     GROUP BY filter_name`,
    [object],
  );

  const ordered = rows
    .filter((r) => (FILTER_ORDER as readonly string[]).includes(r.filter_name))
    .sort((a, b) => filterRank(a.filter_name) - filterRank(b.filter_name));

  let cum = 0;
  return ordered.map((r) => {
    cum += r.minutes;
    return {
      filter: FILTER_LABELS[r.filter_name] ?? r.filter_name,
      number: r.number,
      durationMins: round1(r.minutes),
      durationCum: round1(cum),
    };
  });
}

// --- astrobinCSV ------------------------------------------------------------
// Per date/filter/duration tally, with filter mapped to Astrobin's IDs.
export async function getAstrobin(object: string): Promise<AstrobinRow[]> {
  const rows = await query<{
    date: string;
    filter_name: string;
    duration: number;
    number: number;
  }>(
    `SELECT "Date" AS date,
            "FilterName" AS filter_name,
            "Duration"::float8 AS duration,
            COUNT(*)::int AS number
     FROM "astroSubs"
     WHERE ${BASE_WHERE} AND "Object" = $1
     GROUP BY date, filter_name, duration
     ORDER BY date`,
    [object],
  );

  return rows.map((r) => ({
    date: r.date,
    filterId: ASTROBIN_FILTER_ID[r.filter_name] ?? -1,
    number: r.number,
    duration: r.duration,
  }));
}

// --- Altitude curve (per-night) ---------------------------------------------
// One night's kept frames for an object. ExposureStart is a timestamptz whose
// wall-clock is the (bogus-Z) local capture time — we read its UTC components
// as the naive local time (see altitude-curve plan §9).
type NightFrameRow = {
  exposure_start: Date;
  duration: number;
  filter_name: string;
  fwhm: number | null;
  guiding: number | null;
};

export async function getNightFrames(
  object: string,
  date: string,
): Promise<NightFrameRow[]> {
  return query<NightFrameRow>(
    `SELECT "ExposureStart" AS exposure_start,
            "Duration"::float8 AS duration,
            "FilterName" AS filter_name,
            "FWHM"::float8 AS fwhm,
            "GuidingRMSArcSec"::float8 AS guiding
     FROM "astroSubs"
     WHERE ${BASE_WHERE} AND "Object" = $1 AND "Date" = $2
     ORDER BY "ExposureStart"`,
    [object, date],
  );
}

const pad2 = (n: number) => String(n).padStart(2, "0");

// Position a frame on the local-noon→noon axis from ExposureStart's wall-clock
// (read via UTC components, since the stored Z is bogus local time).
function exposureToLocal(ts: Date, nightDate: string): { minutes: number; hhmm: string } {
  const wallMs = Date.UTC(
    ts.getUTCFullYear(),
    ts.getUTCMonth(),
    ts.getUTCDate(),
    ts.getUTCHours(),
    ts.getUTCMinutes(),
  );
  const [y, mo, d] = nightDate.split("-").map(Number);
  const noon = Date.UTC(y, mo - 1, d, 12, 0);
  return {
    minutes: Math.round((wallMs - noon) / 60_000),
    hhmm: `${pad2(ts.getUTCHours())}:${pad2(ts.getUTCMinutes())}`,
  };
}

// Server assembler: resolves coordinates, computes the curve + twilight bands,
// and places each captured frame on the curve. Returns null for an unknown
// target (no catalog hit + Sesame miss) so the caller can show a tidy notice.
// Resolve a captured object's J2000 coordinates from the catalog_* reference
// tables (authoritative + offline, no Sesame round-trip). The astroSubs
// "Object" name is matched to a catalog `designation` after normalizing
// (uppercase, strip whitespace) — e.g. "M81" -> catalog_messier. Returns null
// for names with no catalog hit (e.g. "M8andM20"), letting the caller fall
// back to the bundled catalog.json + Sesame path.
async function resolveCoordFromCatalog(object: string): Promise<SkyCoord | null> {
  const key = normalizeName(object);
  const rows = await query<{ ra_deg: number | null; dec_deg: number | null }>(
    `SELECT ra_deg, dec_deg FROM (
       SELECT designation, ra_deg, dec_deg FROM catalog_messier
       UNION ALL SELECT designation, ra_deg, dec_deg FROM catalog_top100
       UNION ALL SELECT designation, ra_deg, dec_deg FROM catalog_nebulae
       UNION ALL SELECT designation, ra_deg, dec_deg FROM catalog_dark_nebulae
     ) t
     WHERE upper(replace(designation, ' ', '')) = $1
       AND ra_deg IS NOT NULL AND dec_deg IS NOT NULL
     LIMIT 1`,
    [key],
  );
  const r = rows[0];
  if (!r || r.ra_deg == null || r.dec_deg == null) return null;
  return { raDeg: r.ra_deg, decDeg: r.dec_deg, source: "catalog" };
}

export async function getAltitudeNight(
  object: string,
  date: string,
): Promise<AltitudeNight | null> {
  // Prefer the catalog tables (offline, authoritative); fall back to the
  // bundled catalog.json + Sesame resolver for anything not catalogued.
  const coord = (await resolveCoordFromCatalog(object)) ?? (await resolveCoord(object));
  if (!coord) return null;

  const rows = await getNightFrames(object, date);
  const curve = altitudeCurve(coord, DEFAULT_SITE, date);
  const bands = twilightBands(DEFAULT_SITE, date);

  const frames: NightFrame[] = rows.map((r) => {
    const { minutes, hhmm } = exposureToLocal(r.exposure_start, date);
    return {
      tLocalMinutes: minutes,
      altDeg: round2(altitudeAtLocalMinute(coord, DEFAULT_SITE, date, minutes)) ?? 0,
      filter: r.filter_name,
      durationSec: r.duration,
      fwhm: round2(r.fwhm),
      guidingRms: round2(r.guiding),
      localTime: hhmm,
    };
  });

  const transit = curve.reduce<{ tLocalMinutes: number; altDeg: number } | null>(
    (best, s) =>
      !best || s.altDeg > best.altDeg
        ? { tLocalMinutes: s.tLocalMinutes, altDeg: round2(s.altDeg) ?? 0 }
        : best,
    null,
  );

  return {
    object,
    date,
    site: { name: DEFAULT_SITE.name, timeZone: DEFAULT_SITE.timeZone },
    coord: { raDeg: coord.raDeg, decDeg: coord.decDeg, source: coord.source },
    curve: curve.map((s) => ({ ...s, altDeg: round2(s.altDeg) ?? 0 })),
    bands,
    frames,
    transit,
  };
}

// ---------------------------------------------------------------------------
// Catalogs — read-only browse over the static catalog_* reference tables.
// Table name comes from the CATALOGS whitelist (never user input), so it is
// safe to interpolate; the designation is always parameterized.
// ---------------------------------------------------------------------------

type CatalogRow = {
  designation: string;
  common_name: string | null;
  object_type: string | null;
  simbad_otype: string | null;
  simbad_otype_label: string | null;
  ra_hours: number | null;
  ra_deg: number | null;
  dec_deg: number | null;
  constellation: string | null;
  magnitude: number | null;
  magnitude_band: string | null;
  size_major_arcmin: number | null;
  size_minor_arcmin: number | null;
  size_arcmin: string | null;
  simbad_main_id: string | null;
  difficulty?: string | null;
};

const CATALOG_COLUMNS = `designation, common_name, object_type, simbad_otype,
  simbad_otype_label, ra_hours, ra_deg, dec_deg, constellation, magnitude,
  magnitude_band, size_major_arcmin, size_minor_arcmin, size_arcmin, simbad_main_id`;

// Distinct, normalized designations we've actually imaged (the BASE_WHERE
// selection). Returned as a map normalized-name -> original "Object" string so
// a catalog entry can deep-link to its real /object/[name] page.
async function capturedObjectMap(): Promise<Map<string, string>> {
  const rows = await query<{ object: string }>(
    `SELECT DISTINCT "Object" AS object FROM "astroSubs" WHERE ${BASE_WHERE}`,
  );
  const m = new Map<string, string>();
  for (const r of rows) m.set(normalizeName(r.object), r.object);
  return m;
}

// Normalized designations currently on the dashboard as planned targets.
async function plannedDesignationSet(): Promise<Set<string>> {
  const rows = await query<{ designation: string }>(
    `SELECT designation FROM planned_targets`,
  );
  return new Set(rows.map((r) => normalizeName(r.designation)));
}

function toCatalogObject(
  r: CatalogRow,
  captured: Map<string, string>,
  planned: Set<string>,
): CatalogObject {
  const capturedAs = captured.get(normalizeName(r.designation)) ?? null;
  return {
    designation: r.designation,
    commonName: r.common_name,
    objectType: r.object_type,
    simbadOtype: r.simbad_otype,
    simbadOtypeLabel: r.simbad_otype_label,
    raHours: r.ra_hours,
    raDeg: r.ra_deg,
    decDeg: r.dec_deg,
    constellation: r.constellation,
    magnitude: r.magnitude,
    magnitudeBand: r.magnitude_band,
    sizeMajorArcmin: r.size_major_arcmin,
    sizeMinorArcmin: r.size_minor_arcmin,
    sizeArcmin: r.size_arcmin,
    simbadMainId: r.simbad_main_id,
    difficulty: r.difficulty ?? null,
    captured: capturedAs !== null,
    capturedAs,
    planned: planned.has(normalizeName(r.designation)),
  };
}

export async function getCatalog(slug: CatalogSlug): Promise<CatalogListData> {
  const meta = CATALOGS[slug];
  const cols = meta.hasDifficulty ? `${CATALOG_COLUMNS}, difficulty` : CATALOG_COLUMNS;
  const [rows, captured, planned] = await Promise.all([
    query<CatalogRow>(
      `SELECT ${cols} FROM "${meta.table}" ORDER BY designation`,
    ),
    capturedObjectMap(),
    plannedDesignationSet(),
  ]);
  return {
    slug: meta.slug,
    label: meta.label,
    blurb: meta.blurb,
    objects: rows.map((r) => toCatalogObject(r, captured, planned)),
  };
}

export async function getCatalogObject(
  slug: CatalogSlug,
  designation: string,
): Promise<CatalogObject | null> {
  const meta = CATALOGS[slug];
  const cols = meta.hasDifficulty ? `${CATALOG_COLUMNS}, difficulty` : CATALOG_COLUMNS;
  const [rows, captured, planned] = await Promise.all([
    query<CatalogRow>(
      `SELECT ${cols} FROM "${meta.table}" WHERE designation = $1`,
      [designation],
    ),
    capturedObjectMap(),
    plannedDesignationSet(),
  ]);
  if (!rows.length) return null;
  return toCatalogObject(rows[0], captured, planned);
}

// A catalog object's stored J2000 coordinates (for DSS fetches etc.).
export async function getCatalogCoord(
  slug: CatalogSlug,
  designation: string,
): Promise<{ raDeg: number; decDeg: number } | null> {
  const meta = CATALOGS[slug];
  const rows = await query<{ ra_deg: number | null; dec_deg: number | null }>(
    `SELECT ra_deg, dec_deg FROM "${meta.table}" WHERE designation = $1`,
    [designation],
  );
  const r = rows[0];
  if (!r || r.ra_deg == null || r.dec_deg == null) return null;
  return { raDeg: r.ra_deg, decDeg: r.dec_deg };
}

// Tonight's altitude curve for a catalog object (uses the row's stored J2000
// RA/Dec — no Sesame needed). Same AltitudeNight shape the object pages use,
// but with no captured frames and a `nowMinutes` marker for the live moment.
export async function getTonightSky(
  slug: CatalogSlug,
  designation: string,
): Promise<AltitudeNight | null> {
  const meta = CATALOGS[slug];
  const rows = await query<{ ra_deg: number | null; dec_deg: number | null }>(
    `SELECT ra_deg, dec_deg FROM "${meta.table}" WHERE designation = $1`,
    [designation],
  );
  const r = rows[0];
  if (!r || r.ra_deg == null || r.dec_deg == null) return null;

  const coord: SkyCoord = { raDeg: r.ra_deg, decDeg: r.dec_deg, source: "catalog" };
  const now = new Date();
  const date = activeNightDate(DEFAULT_SITE, now);
  const curve = altitudeCurve(coord, DEFAULT_SITE, date);
  const bands = twilightBands(DEFAULT_SITE, date);

  const transit = curve.reduce<{ tLocalMinutes: number; altDeg: number } | null>(
    (best, s) =>
      !best || s.altDeg > best.altDeg
        ? { tLocalMinutes: s.tLocalMinutes, altDeg: round2(s.altDeg) ?? 0 }
        : best,
    null,
  );

  return {
    object: designation,
    date,
    site: { name: DEFAULT_SITE.name, timeZone: DEFAULT_SITE.timeZone },
    coord: { raDeg: coord.raDeg, decDeg: coord.decDeg, source: coord.source },
    curve: curve.map((s) => ({ ...s, altDeg: round2(s.altDeg) ?? 0 })),
    bands,
    frames: [],
    transit,
    nowMinutes: Math.round(utcToMinutesFromNoon(now, date, DEFAULT_SITE.timeZone)),
  };
}

// --- Planned targets (the app's only DB write path) -------------------------
// A small wishlist table: targets the user added from a catalog to plan future
// imaging. They surface on the dashboard as empty (no-frames) tiles.

type PlannedRow = {
  designation: string;
  common_name: string | null;
  object_type: string | null;
  constellation: string | null;
  catalog_slug: string | null;
};

export async function getPlannedTargets(): Promise<PlannedRow[]> {
  return query<PlannedRow>(
    `SELECT designation, common_name, object_type, constellation, catalog_slug
     FROM planned_targets ORDER BY added_at DESC`,
  );
}

// Add a catalog object to the planned list, copying a snapshot of its metadata
// (so the dashboard tile renders without re-joining the catalog). Idempotent.
export async function addPlannedTarget(
  slug: CatalogSlug,
  designation: string,
): Promise<boolean> {
  const meta = CATALOGS[slug];
  const rows = await query<{
    designation: string;
    common_name: string | null;
    object_type: string | null;
    constellation: string | null;
    ra_deg: number | null;
    dec_deg: number | null;
  }>(
    `SELECT designation, common_name, object_type, constellation, ra_deg, dec_deg
     FROM "${meta.table}" WHERE designation = $1`,
    [designation],
  );
  if (!rows.length) return false;
  const r = rows[0];
  await query<Record<string, unknown>>(
    `INSERT INTO planned_targets
       (designation, common_name, object_type, constellation, ra_deg, dec_deg, catalog_slug)
     VALUES ($1, $2, $3, $4, $5, $6, $7)
     ON CONFLICT (designation) DO UPDATE SET
       common_name   = EXCLUDED.common_name,
       object_type   = EXCLUDED.object_type,
       constellation = EXCLUDED.constellation,
       ra_deg        = EXCLUDED.ra_deg,
       dec_deg       = EXCLUDED.dec_deg,
       catalog_slug  = EXCLUDED.catalog_slug`,
    [r.designation, r.common_name, r.object_type, r.constellation, r.ra_deg, r.dec_deg, slug],
  );
  return true;
}

export async function removePlannedTarget(designation: string): Promise<void> {
  await query<Record<string, unknown>>(
    `DELETE FROM planned_targets WHERE designation = $1`,
    [designation],
  );
}

export function astrobinToCsv(rows: AstrobinRow[]): string {
  const header = "date,filter,number,duration";
  const body = rows
    .map((r) => `${r.date},${r.filterId},${r.number},${r.duration}`)
    .join("\n");
  return `${header}\n${body}\n`;
}
