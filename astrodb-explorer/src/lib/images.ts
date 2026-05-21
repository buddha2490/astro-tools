// User-uploaded object images (your own processed shots). Stored on disk under
// public/uploads/ and recorded in the object_images table, keyed by object
// name. No size limits — these are large (~50 MB) local files. Separate from
// the DSS feature (src/lib/dss.ts): nothing here touches astroSubs.

import { writeFile, mkdir, unlink } from "fs/promises";
import { join } from "path";
import { query } from "./db";

const UPLOAD_DIR = join(process.cwd(), "public", "uploads");

export async function ensureObjectImagesTable(): Promise<void> {
  await query(
    `CREATE TABLE IF NOT EXISTS object_images (
      object        VARCHAR PRIMARY KEY,
      file_path     VARCHAR NOT NULL,
      original_name VARCHAR,
      uploaded_at   TIMESTAMP DEFAULT NOW()
    )`,
  );
}

// Object name -> filesystem-safe basename ("M8andM20" -> "m8andm20").
function sanitize(object: string): string {
  return object.toLowerCase().replace(/[^a-z0-9]/g, "_");
}

// Extension from the original filename, sanitized; falls back to "img".
function extOf(originalName: string): string {
  const m = originalName.toLowerCase().match(/\.([a-z0-9]+)$/);
  return m ? m[1].replace(/[^a-z0-9]/g, "") : "img";
}

export async function getObjectImagePath(object: string): Promise<string | null> {
  await ensureObjectImagesTable();
  const rows = await query<{ file_path: string }>(
    `SELECT file_path FROM object_images WHERE object = $1`,
    [object],
  );
  return rows[0]?.file_path ?? null;
}

// Map of object name -> uploaded image path, for the dashboard tiles.
export async function getObjectImagePaths(): Promise<Map<string, string>> {
  try {
    const rows = await query<{ object: string; file_path: string }>(
      `SELECT object, file_path FROM object_images`,
    );
    return new Map(rows.map((r) => [r.object, r.file_path]));
  } catch {
    return new Map(); // table not created yet
  }
}

// Persist an uploaded image, replacing any previous one for this object.
// Returns the public path (/uploads/<file>).
export async function storeObjectImage(
  object: string,
  buf: Buffer,
  originalName: string,
): Promise<string> {
  await ensureObjectImagesTable();
  await mkdir(UPLOAD_DIR, { recursive: true });

  const filename = `${sanitize(object)}.${extOf(originalName)}`;
  const filePath = `/uploads/${filename}`;

  // If a prior upload used a different extension, remove the stale file.
  const prev = await getObjectImagePath(object);
  if (prev && prev !== filePath) {
    await unlink(join(process.cwd(), "public", prev)).catch(() => {});
  }

  await writeFile(join(UPLOAD_DIR, filename), buf);

  await query(
    `INSERT INTO object_images (object, file_path, original_name, uploaded_at)
     VALUES ($1, $2, $3, NOW())
     ON CONFLICT (object) DO UPDATE
       SET file_path = EXCLUDED.file_path,
           original_name = EXCLUDED.original_name,
           uploaded_at = NOW()`,
    [object, filePath, originalName],
  );
  return filePath;
}
