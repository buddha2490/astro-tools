// Shared DSS reference-image logic. Fetches a 60′×60′ GIF from STScI's
// Digitized Sky Survey for given J2000 coordinates, caches it under
// public/dss/, and records it in the dss_images table (keyed by the object /
// designation name). Used by both the captured-object route and the catalog
// route — they differ only in how they obtain the coordinates.

import { writeFile, mkdir } from "fs/promises";
import { join } from "path";
import { query } from "./db";

const DSS_DIR = join(process.cwd(), "public", "dss");

export async function ensureDssTable(): Promise<void> {
  await query(
    `CREATE TABLE IF NOT EXISTS dss_images (
      object        VARCHAR PRIMARY KEY,
      file_path     VARCHAR NOT NULL,
      downloaded_at TIMESTAMP DEFAULT NOW()
    )`,
  );
}

// Object / designation name -> a filesystem-safe basename ("NGC 7293" -> "ngc_7293").
export function sanitizeFilename(object: string): string {
  return object.toLowerCase().replace(/[^a-z0-9]/g, "_");
}

// The cached public path for a previously-downloaded image, or null.
export async function getDssPath(object: string): Promise<string | null> {
  await ensureDssTable();
  const rows = await query<{ file_path: string }>(
    `SELECT file_path FROM dss_images WHERE object = $1`,
    [object],
  );
  return rows[0]?.file_path ?? null;
}

// Download from STScI, write the GIF under public/dss/, and upsert the row.
// Returns the public path. Throws on network / HTTP failure with a message
// suitable to surface to the client.
export async function fetchAndStoreDss(
  object: string,
  raDeg: number,
  decDeg: number,
): Promise<string> {
  const dssUrl =
    `https://archive.stsci.edu/cgi-bin/dss_search` +
    `?r=${raDeg}&d=${decDeg}&h=60&w=60&f=GIF&v=2`;

  const res = await fetch(dssUrl, { signal: AbortSignal.timeout(30_000) });
  if (!res.ok) throw new Error(`STScI returned HTTP ${res.status}`);

  const buf = Buffer.from(await res.arrayBuffer());
  await mkdir(DSS_DIR, { recursive: true });

  const filename = `${sanitizeFilename(object)}.gif`;
  await writeFile(join(DSS_DIR, filename), buf);
  const filePath = `/dss/${filename}`;

  await ensureDssTable();
  await query(
    `INSERT INTO dss_images (object, file_path, downloaded_at)
     VALUES ($1, $2, NOW())
     ON CONFLICT (object) DO UPDATE
       SET file_path = EXCLUDED.file_path, downloaded_at = NOW()`,
    [object, filePath],
  );
  return filePath;
}
