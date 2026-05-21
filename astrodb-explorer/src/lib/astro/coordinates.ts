// Object name -> {raDeg, decDeg}. Hybrid resolution:
//   1. bundled static catalog (catalog.json) — our actual targets, offline & instant
//   2. CDS Sesame name-resolver fallback for misses, cached in-memory
// Network failure degrades gracefully to null (the chart shows a notice).

import rawCatalog from "./catalog.json";

export interface SkyCoord {
  raDeg: number;
  decDeg: number;
  source: "catalog" | "sesame";
}

interface CatalogEntry {
  raDeg: number;
  decDeg: number;
  resolvedFrom?: string;
}

const CATALOG = rawCatalog as Record<string, CatalogEntry>;

// In-memory cache for Sesame hits (and misses, to avoid re-querying).
const sesameCache = new Map<string, SkyCoord | null>();

// Canonical key: uppercase, strip whitespace, normalize a few catalog prefixes
// (Messier -> M). Matches how catalog.json keys are stored.
export function normalizeName(object: string): string {
  return object
    .trim()
    .toUpperCase()
    .replace(/\s+/g, "")
    .replace(/^MESSIER/, "M");
}

export async function resolveCoord(object: string): Promise<SkyCoord | null> {
  const key = normalizeName(object);

  const hit = CATALOG[key];
  if (hit) return { raDeg: hit.raDeg, decDeg: hit.decDeg, source: "catalog" };

  if (sesameCache.has(key)) return sesameCache.get(key) ?? null;

  const resolved = await resolveViaSesame(object);
  sesameCache.set(key, resolved);
  return resolved;
}

// Query CDS Sesame (-oI flat output) and pull the J2000 RA/Dec in degrees.
async function resolveViaSesame(object: string): Promise<SkyCoord | null> {
  const url = `https://cds.unistra.fr/cgi-bin/nph-sesame/-oI/SNV?${encodeURIComponent(object)}`;
  try {
    const res = await fetch(url, { signal: AbortSignal.timeout(6000) });
    if (!res.ok) return null;
    const text = await res.text();
    // Flat format emits a line like: "%J 274.700 -13.807 ..." (RA Dec in deg).
    const m = text.match(/%J\s+([-+]?\d+\.?\d*)\s+([-+]?\d+\.?\d*)/);
    if (!m) return null;
    const raDeg = Number(m[1]);
    const decDeg = Number(m[2]);
    if (!Number.isFinite(raDeg) || !Number.isFinite(decDeg)) return null;
    return { raDeg, decDeg, source: "sesame" };
  } catch {
    return null; // offline / timeout / blocked — caller handles null
  }
}
