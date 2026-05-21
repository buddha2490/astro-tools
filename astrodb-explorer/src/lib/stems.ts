// Filename → join "stem": drop the directory, the extension (.xisf/.fits/.fit),
// and a trailing "_a" (a PixInsight artifact). The stem is the single key that
// lets a NINA-ingested ".fits" row and the user's culled ".xisf" collapse to
// one frame across the whole pipeline (ingest + curation).
//
// STEM_SQL below must stay byte-for-byte equivalent to stemOf() so the JS side
// and the SQL side always agree on what "the same frame" means.

/** Reduce a filename to its stem (basename − extension − trailing "_a"). */
export function stemOf(filename: string): string {
  const base = filename.replace(/^.*[/\\]/, "");
  return base.replace(/\.(xisf|fits?|fit)$/i, "").replace(/_a$/i, "");
}

/** SQL expression reducing a stored "Filename" to the same stem stemOf() yields. */
export const STEM_SQL = `regexp_replace(regexp_replace("Filename", '\\.(xisf|fits?|fit)$', '', 'i'), '_a$', '', 'i')`;
