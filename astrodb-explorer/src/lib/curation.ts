// Sub curation, app-native (no R). After NINA ingests every captured frame as
// Status='Captured', the user culls subs in PixInsight and drops the survivors
// (.fits/.xisf) in a folder. "Update subs" then marks each existing astroSubs
// row for that night via the "Final" column — 'Kept' if its stem is in the
// folder, 'Culled' if not. Nothing is ever deleted.

import { query as defaultQuery } from "./db";
import { STEM_SQL } from "./stems";

// The data-access seam. Defaults to the shared pool's query(); the verify
// script injects a transaction-bound executor so its writes roll back.
export type Queryable = <T extends Record<string, unknown>>(
  text: string,
  params?: unknown[],
) => Promise<T[]>;

// Add the curation column and backfill it once from the legacy Status values
// (the old R workflow wrote Included/Excluded). Idempotent: the ALTER is
// IF NOT EXISTS and the backfill only touches rows with no Final value yet, so
// repeat calls are no-ops. Captured-but-uncurated rows keep Final = NULL.
export async function ensureFinalColumn(q: Queryable = defaultQuery): Promise<void> {
  await q(`ALTER TABLE "astroSubs" ADD COLUMN IF NOT EXISTS "Final" text`);
  await q(
    `UPDATE "astroSubs"
        SET "Final" = CASE "Status" WHEN 'Excluded' THEN 'Culled' ELSE 'Kept' END
      WHERE "Final" IS NULL AND "Status" IN ('Included', 'Excluded')`,
  );
}

export interface NightSummary {
  date: string;
  byFilter: Record<string, number>; // filter code -> sub count
  total: number;
  uncurated: number; // frames with Final IS NULL (not yet culled)
}

// Every night this object was imaged, newest first, with per-filter sub counts.
// Counts include all ingested frames (kept, culled, and uncurated) so the user
// sees the full night before choosing the final folder.
export async function getObjectNights(
  object: string,
  q: Queryable = defaultQuery,
): Promise<NightSummary[]> {
  await ensureFinalColumn(q);
  const rows = await q<{
    date: string;
    filter: string | null;
    n: string;
    uncurated: string;
  }>(
    `SELECT "Date" AS date, "FilterName" AS filter,
            count(*) AS n,
            count(*) FILTER (WHERE "Final" IS NULL) AS uncurated
       FROM "astroSubs"
      WHERE "Object" = $1 AND "Date" IS NOT NULL
      GROUP BY "Date", "FilterName"
      ORDER BY "Date" DESC`,
    [object],
  );

  const nights = new Map<string, NightSummary>();
  for (const r of rows) {
    const s =
      nights.get(r.date) ??
      { date: r.date, byFilter: {}, total: 0, uncurated: 0 };
    const cnt = Number(r.n);
    if (r.filter) s.byFilter[r.filter] = (s.byFilter[r.filter] ?? 0) + cnt;
    s.total += cnt;
    s.uncurated += Number(r.uncurated);
    nights.set(r.date, s);
  }
  return Array.from(nights.values());
}

export interface CurateResult {
  kept: number;
  culled: number;
  unmatched: string[]; // folder stems with no matching DB frame for this night
}

// Mark every frame of (object, date): 'Kept' if its stem is among the folder's
// stems, otherwise 'Culled'. Returns the split plus any folder stems that match
// no DB row (a sign a frame was never ingested).
export async function curateNight(
  object: string,
  date: string,
  folderStems: string[],
  q: Queryable = defaultQuery,
): Promise<CurateResult> {
  await ensureFinalColumn(q);
  const stems = Array.from(new Set(folderStems));

  // Folder files that match no ingested frame for this night.
  const existing = await q<{ stem: string }>(
    `SELECT DISTINCT ${STEM_SQL} AS stem
       FROM "astroSubs"
      WHERE "Object" = $1 AND "Date" = $2 AND "Filename" IS NOT NULL`,
    [object, date],
  );
  const dbStems = new Set(existing.map((r) => r.stem));
  const unmatched = stems.filter((s) => !dbStems.has(s));

  const updated = await q<{ Final: string }>(
    `UPDATE "astroSubs"
        SET "Final" = CASE WHEN ${STEM_SQL} = ANY($3::text[]) THEN 'Kept' ELSE 'Culled' END
      WHERE "Object" = $1 AND "Date" = $2
      RETURNING "Final"`,
    [object, date, stems],
  );
  const kept = updated.filter((r) => r.Final === "Kept").length;
  return { kept, culled: updated.length - kept, unmatched };
}
