// Long-running NINA ingest worker. Owns the persistent WebSocket to NINA and
// writes every captured LIGHT frame into astroSubs. Intended to run on office-mac
// under launchd (see docs/nina-ingest.launchd.plist).
//
//   npm run nina:ingest          # uses .env.local for PG creds; NINA_HOST defaults to the rig
//
// Env: PGHOST/PGPORT/PGDATABASE/PGUSER/PGPASSWORD (DB), NINA_HOST/NINA_PORT/
// NINA_API_KEY/NINA_ENABLED (NINA). Exits cleanly on SIGINT/SIGTERM.

import { Pool } from "pg";
import { loadNinaConfig } from "../src/lib/nina/config";
import { createNinaIngest } from "../src/lib/nina/ingest/controller";
import { PostgresImageStore } from "../src/lib/nina/ingest/store";

async function main() {
  const cfg = loadNinaConfig();
  if (!cfg.enabled) {
    console.warn("[nina-ingest] NINA_ENABLED=false — exiting");
    return;
  }

  const pool = new Pool({
    host: process.env.PGHOST ?? "aria-bot",
    port: Number(process.env.PGPORT ?? 5432),
    database: process.env.PGDATABASE ?? "briancarter",
    user: process.env.PGUSER ?? "briancarter",
    password: process.env.PGPASSWORD,
    max: 3,
    idleTimeoutMillis: 30_000,
  });
  pool.on("error", (err) => console.error(`[nina-ingest] pg pool error: ${err.message}`));

  // Fail fast if the DB is unreachable at startup.
  await pool.query("SELECT 1");
  console.info(
    `[nina-ingest] DB ok (${process.env.PGHOST ?? "aria-bot"}); NINA ${cfg.host}:${cfg.port}`,
  );

  const store = new PostgresImageStore(pool);
  const ingest = createNinaIngest(store, cfg);
  ingest.start();

  // Periodic heartbeat so the log shows liveness during quiet stretches.
  const heartbeat = setInterval(() => {
    const d = ingest.getDiagnostics();
    console.info(
      `[nina-ingest] heartbeat socket=${d.socket.state} events=${d.socket.totalEvents} ` +
        `ingested=${d.ingest.ingested} dedup=${d.ingest.deduped} ` +
        `skip=${d.ingest.skippedNonLight} err=${d.ingest.errors}`,
    );
  }, 300_000);

  const shutdown = (sig: string) => {
    console.info(`[nina-ingest] ${sig} — shutting down`);
    clearInterval(heartbeat);
    ingest.stop();
    pool.end().finally(() => process.exit(0));
  };
  process.on("SIGINT", () => shutdown("SIGINT"));
  process.on("SIGTERM", () => shutdown("SIGTERM"));
}

main().catch((err) => {
  console.error("[nina-ingest] fatal:", err);
  process.exit(1);
});
