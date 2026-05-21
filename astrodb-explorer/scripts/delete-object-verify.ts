// Verifies deleteObject WITHOUT permanently changing the table. All DB work runs
// inside one transaction that is rolled back at the end.
//
//   npm run delete-object:verify
//
// Covers: every row of the target object is removed regardless of curation
// state (Kept/Culled/uncurated), the returned count is right, OTHER objects are
// left untouched, and the rollback restores the original row count.

import { Pool, type PoolClient } from "pg";
import { deleteObject } from "../src/lib/queries";
import type { Queryable } from "../src/lib/curation";

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

const VICTIM = "ZZ_DELETE_TEST";
const BYSTANDER = "ZZ_KEEP_TEST";

async function main() {
  console.log("\nDelete-object verify — (rolled-back transaction)\n");
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
    const exec: Queryable = async (text, params) => {
      const r = await client!.query(text, params as never[]);
      return r.rows as never;
    };
    const before = await client.query(`SELECT count(*)::int n FROM "astroSubs"`);

    // VICTIM gets four frames spanning every curation state; BYSTANDER gets one.
    const seed = async (obj: string, fn: string, final: string | null) =>
      client!.query(
        `INSERT INTO "astroSubs" ("Object","Date","Filename","Final","Status")
         VALUES ($1,'2099-12-31',$2,$3,'Captured')`,
        [obj, fn, final],
      );
    await seed(VICTIM, "v_kept.fits", "Kept");
    await seed(VICTIM, "v_culled.fits", "Culled");
    await seed(VICTIM, "v_uncurated.fits", null);
    await seed(VICTIM, "v_kept2.xisf", "Kept");
    await seed(BYSTANDER, "b_kept.fits", "Kept");

    const seeded = await client.query(
      `SELECT count(*)::int n FROM "astroSubs" WHERE "Object"=$1`,
      [VICTIM],
    );
    assert(seeded.rows[0].n === 4, "seeded 4 victim rows", String(seeded.rows[0].n));

    const res = await deleteObject(VICTIM, exec);
    assert(res.deletedFrames === 4, "deleteObject reports 4 deleted", String(res.deletedFrames));

    const remaining = await client.query(
      `SELECT count(*)::int n FROM "astroSubs" WHERE "Object"=$1`,
      [VICTIM],
    );
    assert(remaining.rows[0].n === 0, "no victim rows remain (all states gone)", String(remaining.rows[0].n));

    const bystander = await client.query(
      `SELECT count(*)::int n FROM "astroSubs" WHERE "Object"=$1`,
      [BYSTANDER],
    );
    assert(bystander.rows[0].n === 1, "bystander object untouched", String(bystander.rows[0].n));

    const noop = await deleteObject("ZZ_NONEXISTENT_OBJECT", exec);
    assert(noop.deletedFrames === 0, "deleting a missing object deletes nothing", String(noop.deletedFrames));

    await client.query("ROLLBACK");
    const after = await pool.query(`SELECT count(*)::int n FROM "astroSubs"`);
    assert(after.rows[0].n === before.rows[0].n, "rollback left table unchanged", String(after.rows[0].n));
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
  console.error("delete-object verify crashed:", err);
  process.exit(1);
});
