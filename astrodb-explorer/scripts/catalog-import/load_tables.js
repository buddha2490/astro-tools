#!/usr/bin/env node
/*
 * Create and load the three astrophotography catalog tables from the enriched
 * JSON produced by extract_enrich.py. Idempotent: each table is dropped and
 * recreated, then loaded in a single transaction.
 *
 * Must run where Postgres @ aria-bot is reachable (office-mac), e.g.:
 *   ssh briancarter@office-mac 'source ~/.zshrc; \
 *     cd /Volumes/Office-SSD/Astronomy/astro-tools/astrodb-explorer && \
 *     node scripts/catalog-import/load_tables.js'
 */
const fs = require("fs");
const path = require("path");
const { Pool } = require("pg");

const HERE = __dirname;
const APP_ROOT = path.resolve(HERE, "..", "..");

// load .env.local (PG* vars) the same way the app expects them
const env = Object.fromEntries(
  fs
    .readFileSync(path.join(APP_ROOT, ".env.local"), "utf8")
    .split("\n")
    .filter((l) => l.trim() && !l.startsWith("#"))
    .map((l) => {
      const i = l.indexOf("=");
      return [l.slice(0, i).trim(), l.slice(i + 1).trim()];
    }),
);

const TABLES = [
  { file: "catalog_messier.json", hasDifficulty: true },
  { file: "catalog_top100.json", hasDifficulty: false },
  { file: "catalog_nebulae.json", hasDifficulty: false },
  { file: "catalog_dark_nebulae.json", hasDifficulty: false },
];

// column name -> SQL type. Order is the table column order.
function columns(hasDifficulty) {
  const cols = [
    ["designation", "TEXT PRIMARY KEY"],
    ["common_name", "TEXT"],
    ["object_type", "TEXT"],
    ["simbad_otype", "TEXT"],
    ["simbad_otype_label", "TEXT"],
    ["ra_hours", "DOUBLE PRECISION"],
    ["ra_deg", "DOUBLE PRECISION"],
    ["dec_deg", "DOUBLE PRECISION"],
    ["constellation", "TEXT"],
    ["magnitude", "DOUBLE PRECISION"],
    ["magnitude_band", "TEXT"],
    ["size_major_arcmin", "DOUBLE PRECISION"],
    ["size_minor_arcmin", "DOUBLE PRECISION"],
    ["size_arcmin", "TEXT"],
    ["simbad_main_id", "TEXT"],
  ];
  if (hasDifficulty) cols.push(["difficulty", "TEXT"]);
  return cols;
}

async function loadTable(client, cfg) {
  const data = JSON.parse(fs.readFileSync(path.join(HERE, cfg.file), "utf8"));
  const table = data.table;
  const cols = columns(cfg.hasDifficulty);
  const colNames = cols.map((c) => c[0]);

  await client.query(`DROP TABLE IF EXISTS "${table}"`);
  const colDefs = cols.map(([n, t]) => `  "${n}" ${t}`).join(",\n");
  await client.query(`CREATE TABLE "${table}" (\n${colDefs}\n)`);
  await client.query(
    `COMMENT ON TABLE "${table}" IS ` +
      `'Static catalog from ${data.source_file.replace(/'/g, "''")}; ` +
      `coords from workbook, type/magnitude/size from SIMBAD, ` +
      `constellation derived from RA/Dec.'`,
  );

  const placeholders = colNames.map((_, i) => `$${i + 1}`).join(", ");
  const quotedCols = colNames.map((n) => `"${n}"`).join(", ");
  const insertSQL = `INSERT INTO "${table}" (${quotedCols}) VALUES (${placeholders})`;

  for (const row of data.rows) {
    const vals = colNames.map((n) => (row[n] === undefined ? null : row[n]));
    await client.query(insertSQL, vals);
  }
  return { table, count: data.rows.length };
}

async function main() {
  const pool = new Pool({
    host: env.PGHOST,
    port: Number(env.PGPORT),
    database: env.PGDATABASE,
    user: env.PGUSER,
    password: env.PGPASSWORD,
  });
  const client = await pool.connect();
  try {
    await client.query("BEGIN");
    const results = [];
    for (const cfg of TABLES) results.push(await loadTable(client, cfg));
    await client.query("COMMIT");
    for (const r of results) console.log(`loaded "${r.table}": ${r.count} rows`);
  } catch (e) {
    await client.query("ROLLBACK");
    console.error("FAILED, rolled back:", e.message);
    process.exitCode = 1;
  } finally {
    client.release();
    await pool.end();
  }
}

main();
