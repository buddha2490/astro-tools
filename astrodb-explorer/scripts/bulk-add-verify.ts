// Verifies the bulk-add pipeline WITHOUT writing permanent rows.
//
//   npm run bulk-add:verify
//
// Two halves:
//   1. Pure header parsing (no DB): synthesize FITS + XISF header buffers and
//      check parseFitsHeader/parseXisfHeader/headerToRecord/isLightHeader plus
//      the noon-rollover night-date and UTC DATE-OBS handling.
//   2. bulkInsertFrames in a rolled-back transaction: Final='Kept', stem dedup
//      (.fits vs _a.xisf collapse to one frame), and no-row-loss.

import { Pool, type PoolClient } from "pg";
import {
  parseFitsHeader,
  parseXisfHeader,
  parseHeaderBytes,
  headerToRecord,
  isLightHeader,
  nightDateFromExposure,
} from "../src/lib/fits";
import { bulkInsertFrames } from "../src/lib/bulk";
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

// --- Synthetic header builders -------------------------------------------

function bytes(s: string): Uint8Array {
  const u = new Uint8Array(s.length);
  for (let i = 0; i < s.length; i++) u[i] = s.charCodeAt(i) & 0xff;
  return u;
}

// Pad each 80-char card, append END, pad to a 2880-byte block.
function fitsBuffer(cards: string[]): Uint8Array {
  const text =
    cards.map((c) => c.padEnd(80, " ")).join("") + "END".padEnd(80, " ");
  const padded = text.padEnd(Math.ceil(text.length / 2880) * 2880, " ");
  return bytes(padded);
}

// XISF monolithic: "XISF0100" + uint32 LE header length + 4 reserved + XML.
function xisfBuffer(xml: string): Uint8Array {
  const xb = bytes(xml);
  const buf = new Uint8Array(16 + xb.length);
  buf.set(bytes("XISF0100"), 0);
  const len = xb.length;
  buf[8] = len & 0xff;
  buf[9] = (len >> 8) & 0xff;
  buf[10] = (len >> 16) & 0xff;
  buf[11] = (len >> 24) & 0xff;
  buf.set(xb, 16);
  return buf;
}

async function main() {
  console.log("\nBulk-add verify — FITS header parsing (pure)\n");

  const fits = fitsBuffer([
    "SIMPLE  =                    T / conforms to FITS",
    "OBJECT  = 'NGC 281 '           / target name",
    "IMAGETYP= 'LIGHT   '",
    "FILTER  = 'Ha      '",
    "EXPTIME =                300.0 / [s] exposure",
    "CCD-TEMP=                -10.0 / sensor temp",
    "GAIN    =                  100",
    "DATE-OBS= '2026-05-21T05:00:00' / UTC start",
  ]);
  const fh = parseFitsHeader(fits);
  assert(fh.OBJECT === "NGC 281", "FITS OBJECT trims padded string", String(fh.OBJECT));
  assert(fh.SIMPLE === true, "FITS boolean T parsed", String(fh.SIMPLE));
  assert(fh.EXPTIME === 300, "FITS EXPTIME numeric, comment stripped", String(fh.EXPTIME));
  assert(fh["CCD-TEMP"] === -10, "FITS negative number", String(fh["CCD-TEMP"]));
  assert(fh.IMAGETYP === "LIGHT" && isLightHeader(fh), "FITS IMAGETYP=LIGHT recognized");

  // A '/' inside a quoted string must NOT be treated as a comment delimiter.
  const slashy = parseFitsHeader(fitsBuffer(["OBJECT  = 'AB/CD   '"]));
  assert(slashy.OBJECT === "AB/CD", "slash inside quoted string preserved", String(slashy.OBJECT));

  const dark = parseFitsHeader(fitsBuffer(["IMAGETYP= 'DARK    '"]));
  assert(!isLightHeader(dark), "DARK frame is not a light");
  assert(isLightHeader(parseFitsHeader(fitsBuffer(["OBJECT  = 'X       '"]))), "missing IMAGETYP treated as light");

  console.log("\nBulk-add verify — XISF header parsing (pure)\n");
  const xisf = xisfBuffer(
    `<?xml version="1.0" encoding="UTF-8"?>
<xisf version="1.0"><Image geometry="100:100:1">
<FITSKeyword name="OBJECT" value="'M31     '" comment="target"/>
<FITSKeyword name="IMAGETYP" value="'LIGHT   '"/>
<FITSKeyword name="FILTER" value="'L       '"/>
<FITSKeyword name="EXPTIME" value="120" comment="s"/>
<FITSKeyword name="DATE-OBS" value="'2026-01-15T03:00:00'"/>
</Image></xisf>`,
  );
  const xh = parseXisfHeader(xisf);
  assert(xh.OBJECT === "M31", "XISF OBJECT unquoted from attribute", String(xh.OBJECT));
  assert(xh.EXPTIME === 120, "XISF EXPTIME numeric attribute", String(xh.EXPTIME));
  assert(xh.FILTER === "L", "XISF FILTER string", String(xh.FILTER));
  assert(parseHeaderBytes("x.xisf", xisf).OBJECT === "M31", "parseHeaderBytes dispatches XISF by ext");
  assert(parseHeaderBytes("x.fits", fits).OBJECT === "NGC 281", "parseHeaderBytes dispatches FITS by ext");

  console.log("\nBulk-add verify — headerToRecord + night date (pure)\n");
  const rec = headerToRecord("/imgs/sub_0001.fits", fh)!;
  assert(rec !== null, "record built when OBJECT present");
  assert(rec.Object === "NGC 281", "record Object", String(rec.Object));
  assert(rec.FilterName === "Ha", "record FilterName", String(rec.FilterName));
  assert(rec.Duration === 300, "record Duration", String(rec.Duration));
  assert(rec.CameraTemp === -10, "record CameraTemp", String(rec.CameraTemp));
  assert(rec.Gain === 100, "record Gain", String(rec.Gain));
  assert(rec.Filename === "sub_0001.fits", "record Filename is basename", rec.Filename);
  assert(rec.Status === "Captured" && rec.SubFrameSelected === true, "record carries Captured/selected defaults");
  // 05:00 UTC May 21 = 01:00 EDT May 21 → before local noon → night = May 20.
  assert(rec.Date === "2026-05-20", "DATE-OBS is UTC + noon rollover → 2026-05-20", String(rec.Date));

  assert(headerToRecord("x.fits", parseFitsHeader(fitsBuffer(["EXPTIME = 1"]))) === null, "no OBJECT → null record");
  // 23:30 UTC May 21 = 19:30 EDT May 21 → after local noon → night = May 21.
  assert(
    nightDateFromExposure(new Date("2026-05-21T23:30:00Z")) === "2026-05-21",
    "evening frame stays on its own date",
    String(nightDateFromExposure(new Date("2026-05-21T23:30:00Z"))),
  );

  console.log("\nBulk-add verify — bulkInsertFrames (rolled-back transaction)\n");
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

    // Two distinct frames + one whose stem collides with the first (.fits vs _a.xisf).
    const base = parseFitsHeader(
      fitsBuffer([
        "OBJECT  = 'ZZ_BULK_TEST'",
        "IMAGETYP= 'LIGHT   '",
        "FILTER  = 'L       '",
        "EXPTIME =                 60.0",
        "DATE-OBS= '2099-12-31T05:00:00'",
      ]),
    );
    const r1 = headerToRecord("ZZ_BULK_TEST_F1.fits", base)!;
    const r2 = headerToRecord("ZZ_BULK_TEST_F2.fits", base)!;
    const r1dup = headerToRecord("ZZ_BULK_TEST_F1_a.xisf", base)!; // same stem as r1

    const res = await bulkInsertFrames([r1, r2], exec);
    assert(res.inserted === 2 && res.duplicates === 0, "first run inserts both", JSON.stringify(res));

    const seeded = await client.query<{ Final: string; Status: string }>(
      `SELECT "Final","Status" FROM "astroSubs" WHERE "Object"='ZZ_BULK_TEST'`,
    );
    assert(seeded.rows.length === 2, "2 rows present", String(seeded.rows.length));
    assert(seeded.rows.every((r) => r.Final === "Kept"), "bulk rows are Final='Kept'");
    assert(seeded.rows.every((r) => r.Status === "Captured"), "bulk rows Status='Captured'");

    const res2 = await bulkInsertFrames([r1, r2, r1dup], exec);
    assert(res2.inserted === 0 && res2.duplicates === 3, "re-run dedups all (incl _a.xisf vs .fits)", JSON.stringify(res2));

    const finalCount = await client.query(`SELECT count(*)::int n FROM "astroSubs" WHERE "Object"='ZZ_BULK_TEST'`);
    assert(finalCount.rows[0].n === 2, "still exactly 2 rows after re-run", String(finalCount.rows[0].n));

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
  console.error("bulk-add verify crashed:", err);
  process.exit(1);
});
