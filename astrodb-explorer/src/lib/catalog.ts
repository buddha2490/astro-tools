// Registry of the static reference catalogs in Postgres (built from the
// data-raw Excel + SIMBAD enrichment — see catalog-import scripts). Shared by
// the server query layer and the client UI, so it must stay import-clean (no
// DB / Node-only deps here). Table names are a fixed whitelist; query builders
// interpolate `table` directly, so never derive it from user input.

export interface CatalogMeta {
  slug: string;
  label: string;
  table: string;
  blurb: string;
  hasDifficulty: boolean;
}

export const CATALOGS = {
  messier: {
    slug: "messier",
    label: "Messier",
    table: "catalog_messier",
    blurb: "The 110 classic Messier objects.",
    hasDifficulty: true,
  },
  top100: {
    slug: "top100",
    label: "Top 100",
    table: "catalog_top100",
    blurb: "100 showpiece deep-sky targets.",
    hasDifficulty: false,
  },
  nebulae: {
    slug: "nebulae",
    label: "Nebulae",
    table: "catalog_nebulae",
    blurb: "100 planetary & emission nebulae.",
    hasDifficulty: false,
  },
  "dark-nebulae": {
    slug: "dark-nebulae",
    label: "Dark Nebulae",
    table: "catalog_dark_nebulae",
    blurb: "129 Barnard dark-nebula fields.",
    hasDifficulty: false,
  },
} as const satisfies Record<string, CatalogMeta>;

export type CatalogSlug = keyof typeof CATALOGS;

export const CATALOG_LIST: CatalogMeta[] = Object.values(CATALOGS);

export function isCatalogSlug(s: string): s is CatalogSlug {
  return s in CATALOGS;
}
