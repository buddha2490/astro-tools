"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import { stemOf } from "@/lib/stems";
import { filterColor, filterRank } from "@/lib/filters";

interface NightSummary {
  date: string;
  byFilter: Record<string, number>;
  total: number;
  uncurated: number;
}

interface CurateResult {
  kept: number;
  culled: number;
  unmatched: string[];
}

const FINAL_EXT = /\.(xisf|fits?|fit)$/i;

// "Update subs": cull a night against a folder of final (kept) subs. Reads only
// the filenames in the chosen folder client-side — no file contents leave the
// browser — and sends their stems to the server to mark Kept/Culled.
export default function UpdateSubsButton({ object }: { object: string }) {
  const enc = encodeURIComponent(object);
  const [open, setOpen] = useState(false);
  const [nights, setNights] = useState<NightSummary[] | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [busyDate, setBusyDate] = useState<string | null>(null);
  const [status, setStatus] = useState<string | null>(null);

  const inputRef = useRef<HTMLInputElement>(null);
  const pendingDate = useRef<string | null>(null);

  // webkitdirectory is non-standard; set it on the element directly.
  useEffect(() => {
    const el = inputRef.current;
    if (el) {
      el.setAttribute("webkitdirectory", "");
      el.setAttribute("directory", "");
    }
  }, [open]);

  const loadNights = useCallback(() => {
    setError(null);
    setNights(null);
    fetch(`/api/object/${enc}/nights`)
      .then((r) => r.json())
      .then((d) => {
        if (d.error) throw new Error(d.error);
        setNights(d.nights);
      })
      .catch((e) => setError(String(e)));
  }, [enc]);

  useEffect(() => {
    if (open) loadNights();
  }, [open, loadNights]);

  // Close on Escape.
  useEffect(() => {
    if (!open) return;
    const onKey = (e: KeyboardEvent) => e.key === "Escape" && setOpen(false);
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [open]);

  function pickFolderFor(date: string) {
    pendingDate.current = date;
    setStatus(null);
    inputRef.current?.click();
  }

  async function onFolderChosen(e: React.ChangeEvent<HTMLInputElement>) {
    const date = pendingDate.current;
    const files = e.target.files;
    e.target.value = ""; // allow re-picking the same folder later
    if (!date || !files || files.length === 0) return;

    const stems = Array.from(
      new Set(
        Array.from(files)
          .map((f) => f.name)
          .filter((n) => FINAL_EXT.test(n))
          .map(stemOf),
      ),
    );
    if (stems.length === 0) {
      setStatus(`No .fits/.xisf files found in that folder.`);
      return;
    }

    setBusyDate(date);
    setStatus(null);
    try {
      const r = await fetch(`/api/object/${enc}/curate`, {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify({ date, stems }),
      });
      const d: CurateResult & { error?: string } = await r.json();
      if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`);
      const warn = d.unmatched.length
        ? ` · ${d.unmatched.length} folder file(s) had no matching frame`
        : "";
      setStatus(`${date}: ${d.kept} kept, ${d.culled} culled${warn}.`);
      loadNights();
    } catch (err) {
      setStatus(`Failed: ${String(err)}`);
    } finally {
      setBusyDate(null);
    }
  }

  const orderedFilters = (n: NightSummary) =>
    Object.keys(n.byFilter).sort((a, b) => filterRank(a) - filterRank(b));

  return (
    <>
      <button
        onClick={() => setOpen(true)}
        className="rounded-lg bg-emerald-500/15 px-3 py-1.5 text-sm font-medium text-emerald-300 ring-1 ring-emerald-500/30 transition-colors hover:bg-emerald-500/25"
      >
        Update subs
      </button>

      {/* Hidden directory picker, shared by every night's Update button. */}
      <input
        ref={inputRef}
        type="file"
        multiple
        className="hidden"
        onChange={onFolderChosen}
      />

      {open && (
        <div
          className="fixed inset-0 z-50 flex items-start justify-center overflow-y-auto bg-black/60 px-4 py-10 backdrop-blur-sm"
          onClick={() => setOpen(false)}
        >
          <div
            className="glass w-full max-w-2xl rounded-2xl p-6 shadow-card"
            onClick={(e) => e.stopPropagation()}
          >
            <div className="mb-3 flex items-center justify-between">
              <h2 className="text-lg font-semibold text-fg">
                Update subs — {object}
              </h2>
              <button
                onClick={() => setOpen(false)}
                className="text-muted transition-colors hover:text-fg"
                aria-label="Close"
              >
                ✕
              </button>
            </div>

            <p className="mb-4 text-sm text-muted">
              For a night, click <span className="text-red-300">Update</span> and
              choose the folder holding your <strong>final, kept subs</strong>{" "}
              (<code>.fits</code> or <code>.xisf</code>). Frames in that folder
              are marked <span className="text-emerald-300">Kept</span>; every
              other frame from that night is marked{" "}
              <span className="text-red-300">Culled</span>. Nothing is deleted,
              and only the filenames are read — no images are uploaded.
            </p>

            {error && (
              <div className="rounded-xl border border-red-800/60 bg-red-950/40 px-4 py-3 text-sm text-red-300">
                {error}
              </div>
            )}

            {!nights && !error && (
              <div className="glass h-40 animate-pulse rounded-xl" />
            )}

            {nights && nights.length === 0 && (
              <p className="text-sm text-faint">No imaged nights found.</p>
            )}

            {nights && nights.length > 0 && (
              <div className="overflow-hidden rounded-xl ring-1 ring-white/10">
                <table className="w-full text-sm">
                  <thead className="bg-white/5 text-left text-xs uppercase tracking-wide text-muted">
                    <tr>
                      <th className="px-3 py-2">Night</th>
                      <th className="px-3 py-2">Subs by filter</th>
                      <th className="px-3 py-2 text-right">Total</th>
                      <th className="px-3 py-2"></th>
                    </tr>
                  </thead>
                  <tbody>
                    {nights.map((n) => (
                      <tr key={n.date} className="border-t border-white/5">
                        <td className="whitespace-nowrap px-3 py-2 font-medium text-fg">
                          {n.date}
                          {n.uncurated > 0 && (
                            <span className="ml-2 rounded-full bg-amber-400/15 px-1.5 py-0.5 text-[10px] font-medium text-amber-300 ring-1 ring-amber-400/30">
                              {n.uncurated} new
                            </span>
                          )}
                        </td>
                        <td className="px-3 py-2">
                          <div className="flex flex-wrap gap-1.5">
                            {orderedFilters(n).map((f) => (
                              <span
                                key={f}
                                className="inline-flex items-center gap-1 rounded-md bg-white/5 px-1.5 py-0.5 text-xs text-muted"
                              >
                                <span
                                  className="inline-block h-2 w-2 rounded-full"
                                  style={{ backgroundColor: filterColor(f) }}
                                />
                                {f} {n.byFilter[f]}
                              </span>
                            ))}
                          </div>
                        </td>
                        <td className="px-3 py-2 text-right tabular-nums text-muted">
                          {n.total}
                        </td>
                        <td className="px-3 py-2 text-right">
                          <button
                            onClick={() => pickFolderFor(n.date)}
                            disabled={busyDate !== null}
                            className="rounded-lg bg-red-600 px-3 py-1.5 text-xs font-semibold text-white transition-colors hover:bg-red-500 disabled:opacity-40"
                          >
                            {busyDate === n.date ? "Updating…" : "Update"}
                          </button>
                        </td>
                      </tr>
                    ))}
                  </tbody>
                </table>
              </div>
            )}

            {status && (
              <p className="mt-4 text-sm text-accentBright">{status}</p>
            )}
          </div>
        </div>
      )}
    </>
  );
}
