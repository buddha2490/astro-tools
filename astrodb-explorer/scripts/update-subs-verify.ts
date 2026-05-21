// Verifies the "Update subs" curation logic WITHOUT writing permanent rows.
// All DB work runs inside a single transaction that is rolled back at the end.
//
//   npm run update-subs:verify
//
// Covers: stemOf rules, Kept/Culled split keyed on stem (incl. _a/.xisf vs
// .fits), unmatched-folder-file detection, no-row-loss, idempotency, and the
// per-night summary counts.

import { Pool, type PoolClient } from "pg";
import { stemOf } from "../src/lib/stems";
import {
  curateNight,
  getObjectNights,
  type Queryable,
} from "../src/lib/curation";

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

const OBJ = "ZZ_CURATE_TEST";
const DATE = "2099-12-31";
const stem = (n: string) => `${OBJ}_${DATE}_LIGHT_${n}`;
// Three frames: A (.fits, will be kept), B (.fits, will be culled),
// C (_a.xisf — its stem must still match the bare folder stem).
const A = `${stem("L_60.00_0001")}.fits`;
const B = `${stem("L_60.00_0002")}.fits`;
const C = `${stem("O_300.00_0003")}_a.xisf`;

async function main() {
  console.log("\nUpdate-subs verify — stemOf (pure)\n");
  assert(stemOf(A) === stem("L_60.00_0001"), "stem drops .fits", stemOf(A));
  assert(stemOf(C) === stem("O_300.00_0003"), "stem drops _a + .xisf", stemOf(C));
  assert(stemOf("/a/b/X_2026-05-18_LIGHT_L_60.00_0305.FITS") === "X_2026-05-18_LIGHT_L_60.00_0305", "stem drops path + uppercase ext");

  console.log("\nUpdate-subs verify — curation (rolled-back transaction)\n");
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

    // Seed three uncurated NINA-style frames for the test night.
    for (const [fn, filt] of [
      [A, "L"],
      [B, "L"],
      [C, "O"],
    ] as const) {
      await client.query(
        `INSERT INTO "astroSubs" ("Object","Date","Filename","FilterName","Status")
         VALUES ($1,$2,$3,$4,'Captured')`,
        [OBJ, DATE, fn, filt],
      );
    }

    // Summary sees all three before curation, all uncurated.
    const nights0 = await getObjectNights(OBJ, exec);
    assert(nights0.length === 1, "one night in summary", String(nights0.length));
    assert(nights0[0]?.total === 3, "night total = 3", String(nights0[0]?.total));
    assert(nights0[0]?.uncurated === 3, "all 3 uncurated", String(nights0[0]?.uncurated));
    assert(nights0[0]?.byFilter["L"] === 2 && nights0[0]?.byFilter["O"] === 1, "per-filter counts L:2 O:1");

    // Folder = A + C survive (C given as its bare stem), plus a phantom file.
    const phantom = stem("S_300.00_9999");
    const res = await curateNight(OBJ, DATE, [stemOf(A), stemOf(C), phantom], exec);
    assert(res.kept === 2, "2 kept (A,C)", String(res.kept));
    assert(res.culled === 1, "1 culled (B)", String(res.culled));
    assert(res.unmatched.length === 1 && res.unmatched[0] === phantom, "phantom reported unmatched");

    const rows = await client.query<{ Filename: string; Final: string }>(
      `SELECT "Filename","Final" FROM "astroSubs" WHERE "Object"=$1 AND "Date"=$2`,
      [OBJ, DATE],
    );
    const finalOf = (fn: string) => rows.rows.find((r) => r.Filename === fn)?.Final;
    assert(rows.rows.length === 3, "still 3 rows — nothing deleted", String(rows.rows.length));
    assert(finalOf(A) === "Kept", "A → Kept", String(finalOf(A)));
    assert(finalOf(B) === "Culled", "B → Culled", String(finalOf(B)));
    assert(finalOf(C) === "Kept", "C (_a.xisf) → Kept via stem", String(finalOf(C)));

    // Idempotent: same folder again yields the same split.
    const res2 = await curateNight(OBJ, DATE, [stemOf(A), stemOf(C)], exec);
    assert(res2.kept === 2 && res2.culled === 1, "re-run is idempotent");

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
  console.error("update-subs verify crashed:", err);
  process.exit(1);
});
