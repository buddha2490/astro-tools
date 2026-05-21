// Observer location for all altitude / twilight computation.
//
// Fixed single site (central Texas). The user has stated this never changes, so
// it lives as a constant rather than config. `timeZone` is the IANA Central zone
// — it renders as CST in winter / CDT in summer, and being DST-aware is what
// keeps captured-frame dots aligned with the computed curve year-round (NINA's
// stored wall-clock timestamps follow DST; see the altitude-curve plan §9).
//
// Promotable to a per-night lookup later if the rig ever images from elsewhere.

export interface Site {
  name: string;
  latDeg: number;
  lonDeg: number; // east-positive
  elevationM: number;
  timeZone: string; // IANA, e.g. "America/Chicago"
}

export const DEFAULT_SITE: Site = {
  name: "Texas",
  latDeg: 31.5475,
  lonDeg: -99.38194444,
  elevationM: 0,
  timeZone: "America/Chicago",
};
