// Server-side bulk insert for the dashboard "Bulk add" button. The client reads
// each file's header, maps it to an AstroSubRecord (src/lib/fits.ts), and POSTs
// the records here. Frames are inserted as Final='Kept' (bulk folders are the
// user's already-culled survivors), de-duplicated by filename STEM so re-running
// never doubles a frame NINA already ingested, and nothing is ever deleted.

import { query as defaultQuery } from "./db";
import { stemOf, STEM_SQL } from "./stems";
import { ensureFinalColumn, type Queryable } from "./curation";
import type { AstroSubRecord } from "./nina/ingest/mapper";

export interface BulkAddResult {
  inserted: number;
  duplicates: number;
  total: number;
}

// Insert each record unless its stem already exists (the same NOT EXISTS guard
// the NINA ingest store uses), tagging survivors Final='Kept'. Sequential rather
// than batched so a single bad row can't roll back the whole set.
export async function bulkInsertFrames(
  records: AstroSubRecord[],
  q: Queryable = defaultQuery,
): Promise<BulkAddResult> {
  await ensureFinalColumn(q);
  let inserted = 0;
  for (const r of records) {
    const stem = stemOf(r.Filename);
    const rows = await q<{ ok: number }>(
      `INSERT INTO "astroSubs" (
         "Object","Date","Filename","ExposureStart","FilterName","Duration",
         "CameraTemp","Gain","ADUMean","DetectedStars","HFR","FWHM",
         "Eccentricity","GuidingRMSArcSec","FocuserPosition","FocuserTemp",
         "RotatorPosition","Status","SubFrameSelected","Final"
       )
       SELECT $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19,'Kept'
       WHERE NOT EXISTS (
         SELECT 1 FROM "astroSubs" WHERE ${STEM_SQL} = $20
       )
       RETURNING 1 AS ok`,
      [
        r.Object,
        r.Date,
        r.Filename,
        r.ExposureStart,
        r.FilterName,
        r.Duration,
        r.CameraTemp,
        r.Gain,
        r.ADUMean,
        r.DetectedStars,
        r.HFR,
        r.FWHM,
        r.Eccentricity,
        r.GuidingRMSArcSec,
        r.FocuserPosition,
        r.FocuserTemp,
        r.RotatorPosition,
        r.Status,
        r.SubFrameSelected,
        stem,
      ],
    );
    if (rows.length > 0) inserted++;
  }
  return { inserted, duplicates: records.length - inserted, total: records.length };
}
