// Shared shapes used by both the server query layer and the client UI.

export interface SummaryRow {
  object: string;
  lrgb: number | null;
  narrowband: number | null;
  total: number;
}

export interface IntegrationRow {
  filter: string;
  number: number;
  durationMins: number;
  durationCum: number;
}

export interface AstrobinRow {
  date: string;
  filterId: number;
  number: number;
  duration: number;
}

// --- Dashboard ------------------------------------------------------------

export interface OverviewStats {
  totalHours: number;
  targetCount: number;
  frameCount: number;
  nightCount: number;
  firstDate: string | null;
  lastDate: string | null;
}

// One filter's contribution to an object (presentation derives color/label
// from the filter code via src/lib/filters.ts).
export interface FilterSlice {
  code: string;
  minutes: number;
  frames: number;
}

export interface ObjectCard {
  object: string;
  totalMinutes: number;
  frames: number;
  lastDate: string | null;
  isNarrowband: boolean;
  filters: FilterSlice[];
  // A planned (not-yet-imaged) target added from a catalog. Such a card has no
  // frames/integration; it links to its catalog page and shows planning hints.
  planned?: boolean;
  catalogSlug?: string | null;
  objectType?: string | null;
  constellation?: string | null;
  // Local path to a downloaded DSS reference GIF, e.g. "/dss/ngc_281.gif".
  // Null when the image hasn't been fetched yet.
  dssImagePath?: string | null;
}

export interface DashboardData {
  stats: OverviewStats;
  objects: ObjectCard[];
}

// --- Object detail --------------------------------------------------------

// One imaging session (a Date) for an object: minutes per filter that night,
// plus mean quality metrics across that night's kept frames.
export interface SessionRow {
  date: string;
  minutes: number;
  frames: number;
  byFilter: Record<string, number>; // filter code -> minutes that night
  hfr: number | null;
  fwhm: number | null;
  eccentricity: number | null;
  guidingRms: number | null;
}

// Aggregate quality across all kept frames for an object (mean/min/max).
export interface QualityStat {
  metric: string; // display label
  key: string; // hfr | fwhm | eccentricity | guidingRms | detectedStars
  mean: number | null;
  min: number | null;
  max: number | null;
  unit: string;
  lowerIsBetter: boolean;
}

export interface ObjectDetail {
  object: string;
  totalMinutes: number;
  frames: number;
  nights: number;
  firstDate: string | null;
  lastDate: string | null;
  isNarrowband: boolean;
  filters: FilterSlice[]; // per-filter totals, canonical order
  sessions: SessionRow[]; // chronological
  quality: QualityStat[];
}

// --- Catalogs (static reference target tables) ----------------------------

// One row from a catalog_* table, plus whether we've actually imaged it.
// `captured`/`capturedAs` link to a real /object/[name] page when the
// designation matches a target in astroSubs (normalized, whitespace-stripped).
export interface CatalogObject {
  designation: string;
  commonName: string | null;
  objectType: string | null;
  simbadOtype: string | null;
  simbadOtypeLabel: string | null;
  raHours: number | null;
  raDeg: number | null;
  decDeg: number | null;
  constellation: string | null;
  magnitude: number | null;
  magnitudeBand: string | null;
  sizeMajorArcmin: number | null;
  sizeMinorArcmin: number | null;
  sizeArcmin: string | null;
  simbadMainId: string | null;
  difficulty: string | null; // Messier only; null elsewhere
  captured: boolean;
  capturedAs: string | null;
  planned: boolean; // already on the dashboard as a planned target
}

export interface CatalogListData {
  slug: string;
  label: string;
  blurb: string;
  objects: CatalogObject[];
}

// --- Altitude curve (per-night sky position) ------------------------------

// One captured frame, scoped to a night, with the fields the hover box needs.
// `tLocalMinutes` is its position on the local-noon→noon axis; `altDeg` is the
// target's altitude at that instant (computed, not stored).
export interface NightFrame {
  tLocalMinutes: number;
  altDeg: number;
  filter: string;
  durationSec: number;
  fwhm: number | null;
  guidingRms: number | null;
  localTime: string; // "HH:MM" wall-clock
}

// One point on the sampled altitude curve.
export interface AltSample {
  tLocalMinutes: number; // 0..1440 from local noon
  altDeg: number;
  isoUtc: string;
}

// Sun-event boundaries as minutes-from-local-noon (null if not within window).
export interface TwilightBands {
  sunset: number | null;
  civilEnd: number | null;
  nauticalEnd: number | null;
  astroEnd: number | null;
  astroStart: number | null;
  nauticalStart: number | null;
  civilStart: number | null;
  sunrise: number | null;
}

// Everything AltitudeCurveChart needs for one object on one night.
export interface AltitudeNight {
  object: string;
  date: string;
  site: { name: string; timeZone: string };
  coord: { raDeg: number; decDeg: number; source: string };
  curve: AltSample[];
  bands: TwilightBands;
  frames: NightFrame[];
  transit: { tLocalMinutes: number; altDeg: number } | null;
  // Minutes-from-local-noon for "right now", set only for the live tonight
  // view (catalog pages); null/absent for historical imaging nights.
  nowMinutes?: number | null;
}
