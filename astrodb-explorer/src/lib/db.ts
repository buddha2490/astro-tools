import { Pool } from "pg";

// Single shared connection pool. In dev, Next.js hot-reloads modules, so we
// stash the pool on globalThis to avoid exhausting connections.
declare global {
  // eslint-disable-next-line no-var
  var _astroPool: Pool | undefined;
}

function createPool(): Pool {
  // gssencmode=disable in the R client maps to turning off SSL negotiation;
  // node-postgres connects without SSL by default, which matches the R config.
  return new Pool({
    host: process.env.PGHOST ?? "aria-bot",
    port: Number(process.env.PGPORT ?? 5432),
    database: process.env.PGDATABASE ?? "briancarter",
    user: process.env.PGUSER ?? "briancarter",
    password: process.env.PGPASSWORD,
    max: 5,
    idleTimeoutMillis: 30_000,
  });
}

export const pool: Pool = globalThis._astroPool ?? createPool();

if (process.env.NODE_ENV !== "production") {
  globalThis._astroPool = pool;
}

export async function query<T extends Record<string, unknown>>(
  text: string,
  params?: unknown[],
): Promise<T[]> {
  const res = await pool.query(text, params as never[]);
  return res.rows as T[];
}
