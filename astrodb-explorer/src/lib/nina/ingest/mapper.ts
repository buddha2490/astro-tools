// Maps a NINA IMAGE-SAVE event to an astroSubs row. Pure and dependency-free so
// it can be unit-tested without a live NINA or DB. Equipment readings the event
// doesn't carry (focuser/rotator) are injected by the caller as a snapshot.
//
// Join key with the R pipeline = the filename STEM. R builds its filenames as
//   {Object}_{Date}_{ImageType}_{FilterName}_{Duration:.2f}_{ExposureNumber:04d}
// and stores stem+ext (.xisf or .fits); NINA writes the identical pattern as
// .fits. So stem = basename − extension − trailing "_a" makes both sides agree,
// which is what lets the R pipeline later UPDATE the row this worker inserts.

import type { NinaImageStatistics } from "../types";

// Mirrors the astroSubs columns (all nullable in the DB). Stem is carried for
// keying but is not a column.
export interface AstroSubRecord {
  Object: string | null;
  Date: string | null; // "YYYY-MM-DD"
  Filename: string;
  ExposureStart: Date | null;
  FilterName: string | null;
  Duration: number | null;
  CameraTemp: number | null;
  Gain: number | null;
  ADUMean: number | null;
  DetectedStars: number | null;
  HFR: number | null;
  FWHM: number | null; // PixInsight-only → null at capture
  Eccentricity: number | null; // PixInsight-only → null at capture
  GuidingRMSArcSec: number | null;
  FocuserPosition: number | null;
  FocuserTemp: number | null;
  RotatorPosition: number | null;
  Status: string; // "Captured" until the R pipeline curates it
  SubFrameSelected: boolean; // true so the frame shows live in the explorer
}

// Equipment readings fetched at save time (event omits them). Any may be null
// if the device is disconnected.
export interface EquipmentSnapshot {
  focuserPosition: number | null;
  focuserTemp: number | null;
  rotatorPosition: number | null;
}

export const EMPTY_EQUIPMENT: EquipmentSnapshot = {
  focuserPosition: null,
  focuserTemp: null,
  rotatorPosition: null,
};

/** True if this frame should be ingested into astroSubs (light frames only). */
export function isIngestableFrame(stats: NinaImageStatistics): boolean {
  return stats.ImageType?.toUpperCase() === "LIGHT";
}

// The join key with the R/curation side. Defined once in src/lib/stems.ts and
// re-exported here so the existing module surface (and index.ts) is unchanged.
export { stemOf } from "../../stems";

const DATE_TOKEN = /_(\d{4}-\d{2}-\d{2})_/;

/** The night date token embedded in the filename, matching R's folder-date. */
function dateFromFilename(filename: string): string | null {
  return DATE_TOKEN.exec(filename)?.[1] ?? null;
}

/** Everything before the "_YYYY-MM-DD_" token — the object/target name. */
function objectFromFilename(filename: string): string | null {
  const base = filename.replace(/^.*[/\\]/, "");
  const m = /^(.+?)_\d{4}-\d{2}-\d{2}_/.exec(base);
  return m?.[1] ?? null;
}

// RmsText looks like 'Tot: 0.45 (0.81")' — the value in parens is arcsec.
const RMS_ARCSEC = /\(([\d.]+)\s*[""]\)/;

/** Parse total guiding RMS in arcsec from RmsText; null if unparseable. */
export function parseRmsArcsec(rmsText: string | undefined | null): number | null {
  if (!rmsText) return null;
  const m = RMS_ARCSEC.exec(rmsText);
  if (!m) return null;
  const v = Number(m[1]);
  return Number.isFinite(v) ? v : null;
}

function num(v: unknown): number | null {
  const n = typeof v === "number" ? v : Number(v);
  return Number.isFinite(n) ? n : null;
}

function parseDate(iso: string | undefined): Date | null {
  if (!iso) return null;
  const d = new Date(iso);
  return Number.isNaN(d.getTime()) ? null : d;
}

export function mapImageSaveToRecord(
  stats: NinaImageStatistics,
  equipment: EquipmentSnapshot = EMPTY_EQUIPMENT,
): AstroSubRecord {
  const filename = stats.Filename;
  return {
    // Prefer the filename tokens for Object/Date so the values line up exactly
    // with what the R pipeline derives from the folder structure; fall back to
    // the event's own fields.
    Object: objectFromFilename(filename) ?? stats.TargetName ?? null,
    Date: dateFromFilename(filename) ?? stats.Date?.slice(0, 10) ?? null,
    Filename: filename,
    ExposureStart: parseDate(stats.Date),
    FilterName: stats.Filter ?? null,
    Duration: num(stats.ExposureTime),
    CameraTemp: num(stats.Temperature),
    Gain: num(stats.Gain),
    ADUMean: num(stats.Mean),
    DetectedStars: num(stats.Stars),
    HFR: num(stats.HFR),
    FWHM: null,
    Eccentricity: null,
    GuidingRMSArcSec: parseRmsArcsec(stats.RmsText),
    FocuserPosition: equipment.focuserPosition,
    FocuserTemp: equipment.focuserTemp,
    RotatorPosition: equipment.rotatorPosition,
    Status: "Captured",
    SubFrameSelected: true,
  };
}
