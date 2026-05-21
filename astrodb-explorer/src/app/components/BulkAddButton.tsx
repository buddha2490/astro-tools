"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import {
  parseHeaderBytes,
  headerToRecord,
  isLightHeader,
} from "@/lib/fits";
import type { AstroSubRecord } from "@/lib/nina/ingest/mapper";

const SUB_EXT = /\.(xisf|fits?|fit)$/i;

// Read just the header of a sub. XISF declares its header length up front; FITS
// keeps it in the first handful of 2880-byte blocks. Either way the multi-MB
// pixel data is never read — we slice only the front of the file.
async function readHeaderBytes(file: File): Promise<Uint8Array> {
  if (/\.xisf$/i.test(file.name)) {
    const head = new Uint8Array(await file.slice(0, 16).arrayBuffer());
    const len =
      head[8] | (head[9] << 8) | (head[10] << 16) | (head[11] << 24);
    const total = Math.min(16 + (len > 0 ? len : 65536), file.size);
    return new Uint8Array(await file.slice(0, total).arrayBuffer());
  }
  const max = Math.min(2880 * 100, file.size); // ~288 KB covers any FITS header
  return new Uint8Array(await file.slice(0, max).arrayBuffer());
}

interface Summary {
  inserted: number;
  duplicates: number;
  skippedNonLight: number;
  skippedNoObject: number;
  errors: number;
}

// "Bulk add": ingest light frames from any folder (non-NINA images). Reads each
// file's FITS/XISF header client-side, keeps only lights, and sends the parsed
// records to be inserted as Final='Kept'. No image data leaves the browser.
export default function BulkAddButton({ onAdded }: { onAdded?: () => void }) {
  const [open, setOpen] = useState(false);
  const [busy, setBusy] = useState(false);
  const [progress, setProgress] = useState<{ done: number; total: number } | null>(null);
  const [summary, setSummary] = useState<Summary | null>(null);
  const [error, setError] = useState<string | null>(null);

  const inputRef = useRef<HTMLInputElement>(null);

  useEffect(() => {
    const el = inputRef.current;
    if (el) {
      el.setAttribute("webkitdirectory", "");
      el.setAttribute("directory", "");
    }
  }, [open]);

  useEffect(() => {
    if (!open) return;
    const onKey = (e: KeyboardEvent) => e.key === "Escape" && !busy && setOpen(false);
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [open, busy]);

  const onFolderChosen = useCallback(
    async (e: React.ChangeEvent<HTMLInputElement>) => {
      const files = Array.from(e.target.files ?? []).filter((f) => SUB_EXT.test(f.name));
      e.target.value = "";
      setSummary(null);
      setError(null);
      if (files.length === 0) {
        setError("No .fits/.xisf files found in that folder.");
        return;
      }

      setBusy(true);
      setProgress({ done: 0, total: files.length });
      const records: AstroSubRecord[] = [];
      let skippedNonLight = 0;
      let skippedNoObject = 0;
      let errors = 0;

      for (let i = 0; i < files.length; i++) {
        try {
          const header = parseHeaderBytes(files[i].name, await readHeaderBytes(files[i]));
          if (!isLightHeader(header)) {
            skippedNonLight++;
          } else {
            const rec = headerToRecord(files[i].name, header);
            if (rec) records.push(rec);
            else skippedNoObject++;
          }
        } catch {
          errors++;
        }
        setProgress({ done: i + 1, total: files.length });
      }

      try {
        const r = await fetch("/api/bulk-add", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ records }),
        });
        const d = await r.json();
        if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`);
        setSummary({
          inserted: d.inserted,
          duplicates: d.duplicates,
          skippedNonLight,
          skippedNoObject,
          errors,
        });
        if (d.inserted > 0) onAdded?.();
      } catch (err) {
        setError(String(err));
      } finally {
        setBusy(false);
        setProgress(null);
      }
    },
    [onAdded],
  );

  return (
    <>
      <button
        onClick={() => setOpen(true)}
        className="glass rounded-lg px-3.5 py-2 text-sm font-medium text-accentBright ring-1 ring-accent/30 transition-colors hover:bg-accent/10"
      >
        Bulk add
      </button>

      <input ref={inputRef} type="file" multiple className="hidden" onChange={onFolderChosen} />

      {open && (
        <div
          className="fixed inset-0 z-50 flex items-start justify-center overflow-y-auto bg-black/60 px-4 py-10 backdrop-blur-sm"
          onClick={() => !busy && setOpen(false)}
        >
          <div className="glass w-full max-w-lg rounded-2xl p-6 shadow-card" onClick={(e) => e.stopPropagation()}>
            <div className="mb-3 flex items-center justify-between">
              <h2 className="text-lg font-semibold text-fg">Bulk add subs</h2>
              <button
                onClick={() => !busy && setOpen(false)}
                className="text-muted transition-colors hover:text-fg"
                aria-label="Close"
              >
                ✕
              </button>
            </div>

            <p className="mb-4 text-sm text-muted">
              Add light frames that didn&apos;t come through NINA. Choose a folder
              of <code>.fits</code> or <code>.xisf</code> subs — the{" "}
              <strong>target, filter, exposure and date are read from each
              file&apos;s header</strong>, so filenames don&apos;t matter. Only
              light frames are added (darks/flats/bias are skipped), each is
              marked <span className="text-emerald-300">Kept</span>, and frames
              already in the database are skipped. Only headers are read — no
              images are uploaded.
            </p>

            <button
              onClick={() => inputRef.current?.click()}
              disabled={busy}
              className="w-full rounded-lg bg-accent/20 px-4 py-2.5 text-sm font-semibold text-accentBright ring-1 ring-accent/30 transition-colors hover:bg-accent/30 disabled:opacity-40"
            >
              {busy ? "Reading headers…" : "Choose folder…"}
            </button>

            {progress && (
              <p className="mt-3 text-sm text-muted">
                Reading {progress.done} / {progress.total} files…
              </p>
            )}

            {error && (
              <div className="mt-4 rounded-xl border border-red-800/60 bg-red-950/40 px-4 py-3 text-sm text-red-300">
                {error}
              </div>
            )}

            {summary && (
              <div className="mt-4 rounded-xl ring-1 ring-white/10">
                <ul className="divide-y divide-white/5 text-sm">
                  <li className="flex justify-between px-4 py-2">
                    <span className="text-emerald-300">Added</span>
                    <span className="tabular-nums text-fg">{summary.inserted}</span>
                  </li>
                  <li className="flex justify-between px-4 py-2">
                    <span className="text-muted">Already in database</span>
                    <span className="tabular-nums text-muted">{summary.duplicates}</span>
                  </li>
                  {summary.skippedNonLight > 0 && (
                    <li className="flex justify-between px-4 py-2">
                      <span className="text-muted">Skipped (not lights)</span>
                      <span className="tabular-nums text-muted">{summary.skippedNonLight}</span>
                    </li>
                  )}
                  {summary.skippedNoObject > 0 && (
                    <li className="flex justify-between px-4 py-2">
                      <span className="text-amber-300">Skipped (no OBJECT in header)</span>
                      <span className="tabular-nums text-amber-300">{summary.skippedNoObject}</span>
                    </li>
                  )}
                  {summary.errors > 0 && (
                    <li className="flex justify-between px-4 py-2">
                      <span className="text-red-300">Unreadable headers</span>
                      <span className="tabular-nums text-red-300">{summary.errors}</span>
                    </li>
                  )}
                </ul>
              </div>
            )}
          </div>
        </div>
      )}
    </>
  );
}
