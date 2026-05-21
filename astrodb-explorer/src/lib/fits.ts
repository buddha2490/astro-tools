// Pure, dependency-free FITS / XISF header parsing for the bulk-add feature.
//
// Bulk-added subs do NOT follow NINA's filename convention, so unlike the NINA
// ingest mapper we read the metadata from the file's *header* rather than its
// name. Both formats keep that header at the very front of the file, so the
// caller need only read the first few KB (file.slice) — never the pixel data.
//
//   FITS  — ASCII 80-byte "cards" in 2880-byte blocks, terminated by an END card.
//   XISF  — "XISF0100" signature + uint32-LE header length + 4 reserved bytes,
//           then an XML header whose <FITSKeyword name=… value=…/> elements
//           carry the same values FITS would.
//
// These functions take a byte buffer (so they're testable in Node with no DOM
// and no File API) and produce an AstroSubRecord that matches what the NINA
// ingest worker writes, so bulk rows are shaped identically to captured rows.

import type { AstroSubRecord } from "./nina/ingest/mapper";

export type FitsValue = string | number | boolean;
export type FitsHeader = Record<string, FitsValue>;

// Decode bytes as latin1 (FITS/XISF headers are ASCII); chunked to avoid
// blowing the argument limit on String.fromCharCode for large buffers.
function latin1(b: Uint8Array): string {
  let s = "";
  const CHUNK = 0x8000;
  for (let i = 0; i < b.length; i += CHUNK) {
    s += String.fromCharCode(...b.subarray(i, i + CHUNK));
  }
  return s;
}

// Coerce a FITS value literal: 'quoted string', T/F boolean, or number; falls
// back to the trimmed raw string. Handles the '/' comment delimiter and the
// doubled-quote ('') escape inside string literals.
function parseFitsValue(raw: string): FitsValue | undefined {
  const s = raw.replace(/^\s+/, "");
  if (s.startsWith("'")) {
    let out = "";
    for (let i = 1; i < s.length; i++) {
      if (s[i] === "'") {
        if (s[i + 1] === "'") {
          out += "'";
          i++;
        } else break;
      } else out += s[i];
    }
    return out.replace(/\s+$/, "");
  }
  const slash = s.indexOf("/");
  const v = (slash >= 0 ? s.slice(0, slash) : s).trim();
  if (v === "") return undefined;
  if (v === "T") return true;
  if (v === "F") return false;
  const n = Number(v);
  return Number.isFinite(n) ? n : v;
}

/** Parse 80-byte FITS header cards from a buffer (stops at the END card). */
export function parseFitsHeader(b: Uint8Array): FitsHeader {
  const header: FitsHeader = {};
  const text = latin1(b);
  for (let i = 0; i + 80 <= text.length; i += 80) {
    const card = text.slice(i, i + 80);
    const keyword = card.slice(0, 8).trim();
    if (keyword === "END") break;
    if (!keyword || keyword === "COMMENT" || keyword === "HISTORY") continue;
    if (card[8] !== "=") continue; // no value-indicator → not a valued keyword
    const value = parseFitsValue(card.slice(9));
    if (value !== undefined) header[keyword.toUpperCase()] = value;
  }
  return header;
}

function attr(attrs: string, key: string): string | null {
  const m = new RegExp(`\\b${key}\\s*=\\s*"([^"]*)"`, "i").exec(attrs);
  return m ? m[1] : null;
}

/** Parse <FITSKeyword> elements out of an XISF monolithic file's XML header. */
export function parseXisfHeader(b: Uint8Array): FitsHeader {
  const header: FitsHeader = {};
  let start = 0;
  let end = b.length;
  if (b.length >= 16 && latin1(b.subarray(0, 8)) === "XISF0100") {
    const dv = new DataView(b.buffer, b.byteOffset, b.byteLength);
    const declared = dv.getUint32(8, true);
    start = 16;
    end = declared > 0 ? Math.min(16 + declared, b.length) : b.length;
  }
  const xml = latin1(b.subarray(start, end));
  const re = /<FITSKeyword\b([^>]*?)\/?>/gi;
  let m: RegExpExecArray | null;
  while ((m = re.exec(xml))) {
    const name = attr(m[1], "name");
    const rawValue = attr(m[1], "value");
    if (!name || rawValue == null) continue;
    // The attribute holds a FITS value literal (PixInsight wraps strings in
    // single quotes), so reuse the same coercion.
    const value = parseFitsValue(rawValue);
    if (value !== undefined) header[name.toUpperCase()] = value;
  }
  return header;
}

/** Dispatch on extension: .xisf → XISF, everything else → FITS. */
export function parseHeaderBytes(filename: string, b: Uint8Array): FitsHeader {
  return /\.xisf$/i.test(filename) ? parseXisfHeader(b) : parseFitsHeader(b);
}

/** True unless the header explicitly marks a non-light (dark/flat/bias) frame. */
export function isLightHeader(h: FitsHeader): boolean {
  const t = String(h.IMAGETYP ?? h.FRAME ?? "").toUpperCase();
  return t === "" || t.includes("LIGHT");
}

function str(v: FitsValue | undefined): string | null {
  if (typeof v === "string") return v.trim() || null;
  if (typeof v === "number") return String(v);
  return null;
}

function num(v: FitsValue | undefined): number | null {
  if (typeof v === "number") return v;
  if (typeof v === "string" && v.trim() !== "" && Number.isFinite(Number(v)))
    return Number(v);
  return null;
}

// FITS DATE-OBS is UTC by convention but usually omits a zone designator; if so
// we must pin it to UTC, else the JS Date constructor reads it as local time.
function parseUtcDate(s: string | null): Date | null {
  if (!s) return null;
  const hasZone = /[zZ]$|[+-]\d{2}:?\d{2}$/.test(s);
  const d = new Date(hasZone ? s : s + "Z");
  return Number.isNaN(d.getTime()) ? null : d;
}

// The "observing night": local time with a noon rollover (NINA's DATEMINUS12),
// so a post-midnight frame groups with the evening it belongs to. Computed in
// America/New_York to match the rest of the pipeline.
export function nightDateFromExposure(d: Date | null): string | null {
  if (!d || Number.isNaN(d.getTime())) return null;
  const parts = new Intl.DateTimeFormat("en-US", {
    timeZone: "America/New_York",
    year: "numeric",
    month: "2-digit",
    day: "2-digit",
    hour: "2-digit",
    hourCycle: "h23",
  }).formatToParts(d);
  const get = (t: string) => Number(parts.find((p) => p.type === t)?.value);
  let ms = Date.UTC(get("year"), get("month") - 1, get("day"));
  if (get("hour") < 12) ms -= 86_400_000; // before local noon → previous night
  const dt = new Date(ms);
  const p = (n: number) => String(n).padStart(2, "0");
  return `${dt.getUTCFullYear()}-${p(dt.getUTCMonth() + 1)}-${p(dt.getUTCDate())}`;
}

function basename(filename: string): string {
  return filename.replace(/^.*[/\\]/, "");
}

// Map a parsed header to an astroSubs row. Returns null when the frame can't be
// placed (no OBJECT). Quality metrics absent from raw subs stay null; the
// caller decides the curation state (bulk-add sets Final='Kept' in SQL).
export function headerToRecord(
  filename: string,
  h: FitsHeader,
): AstroSubRecord | null {
  const object = str(h.OBJECT);
  if (!object) return null;
  const exposureStart = parseUtcDate(str(h["DATE-OBS"]));
  return {
    Object: object,
    Date: nightDateFromExposure(exposureStart),
    Filename: basename(filename),
    ExposureStart: exposureStart,
    FilterName: str(h.FILTER),
    Duration: num(h.EXPTIME ?? h.EXPOSURE),
    CameraTemp: num(h["CCD-TEMP"] ?? h["CCD_TEMP"]),
    Gain: num(h.GAIN),
    ADUMean: null,
    DetectedStars: num(h.STARS),
    HFR: num(h.HFR),
    FWHM: num(h.FWHM),
    Eccentricity: num(h.ECCENTR ?? h.ECCENTRICITY),
    GuidingRMSArcSec: null,
    FocuserPosition: num(h.FOCPOS),
    FocuserTemp: num(h.FOCTEMP),
    RotatorPosition: num(h.ROTATANG ?? h.ROTATOR),
    Status: "Captured",
    SubFrameSelected: true,
  };
}
