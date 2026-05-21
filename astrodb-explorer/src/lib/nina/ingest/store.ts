// Persistence boundary for the ingest worker. Per scoping §5.5 the integration
// module does not import the web app's db layer; it defines an interface and the
// worker supplies a concrete implementation. The Postgres impl below depends
// only on the `pg` package, not on src/lib/db.ts.

import type { Pool } from "pg";
import { stemOf, type AstroSubRecord } from "./mapper";

export interface ImageStore {
  /** True if any astroSubs row (NINA .fits or R .xisf, with/without _a) shares this stem. */
  imageExists(stem: string): Promise<boolean>;
  /** Insert a captured frame unless its stem already exists. Returns whether a row was written. */
  insertCapture(record: AstroSubRecord): Promise<{ inserted: boolean }>;
}

// SQL fragment that reduces a stored Filename to its stem (drop ext + trailing
// _a), matching mapper.stemOf so existence checks see R and NINA rows alike.
const STEM_SQL = `regexp_replace(regexp_replace("Filename", '\\.(xisf|fits?|fit)$', '', 'i'), '_a$', '', 'i')`;

export class PostgresImageStore implements ImageStore {
  constructor(private readonly pool: Pool) {}

  async imageExists(stem: string): Promise<boolean> {
    const res = await this.pool.query(
      `SELECT 1 FROM "astroSubs" WHERE ${STEM_SQL} = $1 LIMIT 1`,
      [stem],
    );
    return res.rowCount! > 0;
  }

  async insertCapture(record: AstroSubRecord): Promise<{ inserted: boolean }> {
    const stem = stemOf(record.Filename);
    // Atomic insert-if-absent: the NOT EXISTS guard makes this idempotent across
    // reconnects and prevents duplicating a frame the R pipeline already owns.
    const res = await this.pool.query(
      `INSERT INTO "astroSubs" (
         "Object","Date","Filename","ExposureStart","FilterName","Duration",
         "CameraTemp","Gain","ADUMean","DetectedStars","HFR","FWHM",
         "Eccentricity","GuidingRMSArcSec","FocuserPosition","FocuserTemp",
         "RotatorPosition","Status","SubFrameSelected"
       )
       SELECT $1,$2,$3,$4,$5,$6,$7,$8,$9,$10,$11,$12,$13,$14,$15,$16,$17,$18,$19
       WHERE NOT EXISTS (
         SELECT 1 FROM "astroSubs" WHERE ${STEM_SQL} = $20
       )`,
      [
        record.Object,
        record.Date,
        record.Filename,
        record.ExposureStart,
        record.FilterName,
        record.Duration,
        record.CameraTemp,
        record.Gain,
        record.ADUMean,
        record.DetectedStars,
        record.HFR,
        record.FWHM,
        record.Eccentricity,
        record.GuidingRMSArcSec,
        record.FocuserPosition,
        record.FocuserTemp,
        record.RotatorPosition,
        record.Status,
        record.SubFrameSelected,
        stem,
      ],
    );
    return { inserted: (res.rowCount ?? 0) > 0 };
  }
}
