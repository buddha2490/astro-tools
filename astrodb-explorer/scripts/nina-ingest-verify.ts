// Verifies the ingest mapper + Postgres store WITHOUT writing permanent rows.
// All DB work runs inside a single transaction that is rolled back at the end.
//
//   npm run nina:ingest:verify
//
// Covers: LIGHT-only filter, filename parsing, RmsText parse, Status/flags,
// insert-if-absent, reconnect dedup, and that an existing R-pipeline row's stem
// is recognised (so we never duplicate a frame the R side already owns).

import { Pool, type PoolClient } from "pg";
import {
  mapImageSaveToRecord,
  isIngestableFrame,
  stemOf,
} from "../src/lib/nina/ingest/mapper";
import { PostgresImageStore } from "../src/lib/nina/ingest/store";
import type { NinaImageStatistics } from "../src/lib/nina/types";

let pass = 0,
  fail = 0;
function assert(cond: boolean, label: string, detail = "") {
  if (cond) {
    pass++;
    console.log(`  ✅ ${label}${detail ? ` — ${detail}` : ""}`);
  } else {
    fail++;
    console.log(`  ❌ ${label}${detail ? ` — ${detail}` : ""}`);
  }
}

// A synthetic LIGHT frame in the same on-disk shape NINA writes.
const lightStats: NinaImageStatistics = {
  ExposureTime: 60,
  ImageType: "LIGHT",
  Filter: "L",
  RmsText: 'Tot: 0.45 (0.81")',
  Temperature: -10.2,
  CameraName: "ZWO ASI2600MM Pro",
  TargetName: "NGC_TEST",
  Gain: 100,
  Offset: 50,
  Date: "2099-01-01T22:30:15.5-06:00",
  TelescopeName: "ES127",
  FocalLength: 619,
  StDev: 30,
  Mean: 642.7,
  Median: 640,
  Min: 300,
  Max: 65535,
  Stars: 1234,
  HFR: 2.13,
  HFRStDev: 0.1,
  IsBayered: false,
  // object token "NGC_TEST" has an underscore — exercises the date-token parse
  Filename: "NGC_TEST_2099-01-01_LIGHT_L_60.00_9999.fits",
};

const snapStats: NinaImageStatistics = { ...lightStats, ImageType: "SNAPSHOT" };

async function main() {
  console.log("\nIngest verify — mapper (pure)\n");
  assert(isIngestableFrame(lightStats), "LIGHT is ingestable");
  assert(!isIngestableFrame(snapStats), "SNAPSHOT is skipped");

  const rec = mapImageSaveToRecord(lightStats, {
    focuserPosition: 19755,
    focuserTemp: 21.1,
    rotatorPosition: 0.5,
  });
  assert(rec.Object === "NGC_TEST", "Object from filename token", String(rec.Object));
  assert(rec.Date === "2099-01-01", "Date from filename token", String(rec.Date));
  assert(rec.Duration === 60, "Duration", String(rec.Duration));
  assert(rec.FilterName === "L", "FilterName", String(rec.FilterName));
  assert(rec.CameraTemp === -10.2, "CameraTemp", String(rec.CameraTemp));
  assert(rec.ADUMean === 642.7, "ADUMean←Mean", String(rec.ADUMean));
  assert(rec.DetectedStars === 1234, "DetectedStars←Stars", String(rec.DetectedStars));
  assert(rec.GuidingRMSArcSec === 0.81, "GuidingRMSArcSec from RmsText", String(rec.GuidingRMSArcSec));
  assert(rec.FocuserPosition === 19755, "FocuserPosition injected", String(rec.FocuserPosition));
  assert(rec.FWHM === null && rec.Eccentricity === null, "FWHM/Eccentricity null (PixInsight-only)");
  assert(rec.Status === "Captured", "Status=Captured", rec.Status);
  assert(rec.SubFrameSelected === true, "SubFrameSelected=true");
  assert(rec.ExposureStart instanceof Date && !Number.isNaN(rec.ExposureStart.getTime()), "ExposureStart parsed");
  assert(stemOf(rec.Filename) === "NGC_TEST_2099-01-01_LIGHT_L_60.00_9999", "stem", stemOf(rec.Filename));
  assert(stemOf("X_2026-05-18_LIGHT_L_60.00_0305_a.xisf") === "X_2026-05-18_LIGHT_L_60.00_0305", "stem strips _a + .xisf");

  console.log("\nIngest verify — store (rolled-back transaction)\n");
  const pool = new Pool({
    host: process.env.PGHOST ?? "aria-bot",
    port: Number(process.env.PGPORT ?? 5432),
    database: process.env.PGDATABASE ?? "briancarter",
    user: process.env.PGUSER ?? "briancarter",
    password: process.env.PGPASSWORD,
  });
  let client: PoolClient | null = null;
  try {
    client = await pool.connect();
    await client.query("BEGIN");
    // PostgresImageStore only calls .query — a PoolClient satisfies that, keeping
    // every write inside this transaction.
    const store = new PostgresImageStore(client as unknown as Pool);

    const before = await client.query(`SELECT count(*)::int n FROM "astroSubs"`);
    // Grab a genuine pre-existing R-pipeline filename BEFORE inserting anything,
    // so the "won't duplicate an R-owned frame" check tests a real row (not our
    // synthetic one, whose 2099 date would otherwise sort to the top).
    const real = await client.query(
      `SELECT "Filename" FROM "astroSubs" WHERE "Filename" IS NOT NULL ORDER BY "Date" DESC LIMIT 1`,
    );
    const realFn: string = real.rows[0].Filename;

    assert(stemOf(rec.Filename).length > 0, "have a synthetic stem");
    assert(!(await store.imageExists(stemOf(rec.Filename))), "synthetic stem absent pre-insert");

    const r1 = await store.insertCapture(rec);
    assert(r1.inserted === true, "first insert writes a row");
    assert(await store.imageExists(stemOf(rec.Filename)), "stem present after insert");

    const r2 = await store.insertCapture(rec);
    assert(r2.inserted === false, "second insert deduped (reconnect-safe)");

    // .fits vs .xisf for the same stem must also dedup.
    const xisfDup = { ...rec, Filename: rec.Filename.replace(/\.fits$/, "_a.xisf") };
    const r3 = await store.insertCapture(xisfDup);
    assert(r3.inserted === false, ".xisf/_a variant of same stem deduped");

    // An existing real R-pipeline row's stem must be recognised (realFn captured
    // before any insert above).
    assert(await store.imageExists(stemOf(realFn)), "existing R-row stem recognised", stemOf(realFn));
    const r4 = await store.insertCapture({ ...rec, Filename: realFn.replace(/\.\w+$/, ".fits") });
    assert(r4.inserted === false, "won't duplicate an existing R-owned frame");

    const after = await client.query(`SELECT count(*)::int n FROM "astroSubs"`);
    assert(after.rows[0].n === before.rows[0].n + 1, "exactly one net insert in txn", `${before.rows[0].n}→${after.rows[0].n}`);

    await client.query("ROLLBACK");
    const final = await pool.query(`SELECT count(*)::int n FROM "astroSubs"`);
    assert(final.rows[0].n === before.rows[0].n, "rollback left table unchanged", String(final.rows[0].n));
  } finally {
    try {
      await client?.query("ROLLBACK");
    } catch {
      /* already rolled back */
    }
    client?.release();
    await pool.end();
  }

  console.log(`\n${fail === 0 ? "ALL GREEN" : "FAILURES"} — ${pass} passed, ${fail} failed\n`);
  process.exit(fail === 0 ? 0 : 1);
}

main().catch((err) => {
  console.error("ingest verify crashed:", err);
  process.exit(1);
});
