// Single source of truth for filter ordering, display labels, palette colors,
// and Astrobin IDs. Mirrors the encoding baked into the R reporting functions
// (astroDB/functions/functions.R) — keep these maps in sync with that code.

export const FILTER_ORDER = ["L", "R", "G", "B", "H", "S", "O", "HO", "UVIR"] as const;
export type FilterCode = (typeof FILTER_ORDER)[number];

export const FILTER_LABELS: Record<string, string> = {
  L: "Luminance",
  R: "Red",
  G: "Green",
  B: "Blue",
  H: "H-alpha",
  S: "Sulfur II",
  O: "Oxygen III",
  HO: "HO",
  UVIR: "UV/IR",
};

// Hue-distinct palette, loosely tied to imaging convention. Each filter gets a
// solid color (for bars/legends) — Ha warm, OIII teal, SII gold, LRGB literal.
export const FILTER_COLORS: Record<string, string> = {
  L: "#d6def0",
  R: "#ef4d52",
  G: "#34c759",
  B: "#3b82f6",
  H: "#fb5e7e",
  S: "#f5a623",
  O: "#2dd4bf",
  HO: "#818cf8",
  UVIR: "#a855f7",
};

export const FILTER_FALLBACK_COLOR = "#64748b";

export const ASTROBIN_FILTER_ID: Record<string, number> = {
  H: 4388,
  O: 4392,
  S: 4396,
  B: 5642,
  G: 5646,
  L: 5652,
  R: 5656,
  HO: 6901,
  UVIR: 13400,
};

export const NARROWBAND = new Set(["H", "S", "O", "HO"]);

export function filterColor(code: string): string {
  return FILTER_COLORS[code] ?? FILTER_FALLBACK_COLOR;
}

export function filterLabel(code: string): string {
  return FILTER_LABELS[code] ?? code;
}

// Canonical sort index; unknown filters sort to the end but stay stable.
export function filterRank(code: string): number {
  const i = (FILTER_ORDER as readonly string[]).indexOf(code);
  return i === -1 ? FILTER_ORDER.length : i;
}
