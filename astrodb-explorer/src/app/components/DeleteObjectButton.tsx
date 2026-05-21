"use client";

import { useEffect, useState } from "react";
import { useRouter } from "next/navigation";

// "Delete object": permanently removes every frame for this object (and its
// uploaded image). Always behind a confirmation modal that warns it's
// destructive. On success it returns to the dashboard.
export default function DeleteObjectButton({ object }: { object: string }) {
  const router = useRouter();
  const [open, setOpen] = useState(false);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    if (!open) return;
    const onKey = (e: KeyboardEvent) => e.key === "Escape" && !busy && setOpen(false);
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, [open, busy]);

  async function confirmDelete() {
    setBusy(true);
    setError(null);
    try {
      const r = await fetch(`/api/object/${encodeURIComponent(object)}`, {
        method: "DELETE",
      });
      const d = await r.json();
      if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`);
      router.push("/");
      router.refresh();
    } catch (err) {
      setError(String(err));
      setBusy(false);
    }
  }

  return (
    <>
      <button
        onClick={() => setOpen(true)}
        className="rounded-lg bg-red-500/15 px-3 py-1.5 text-sm font-medium text-red-300 ring-1 ring-red-500/30 transition-colors hover:bg-red-500/25"
      >
        Delete object
      </button>

      {open && (
        <div
          className="fixed inset-0 z-50 flex items-start justify-center overflow-y-auto bg-black/60 px-4 py-10 backdrop-blur-sm"
          onClick={() => !busy && setOpen(false)}
        >
          <div
            className="glass w-full max-w-md rounded-2xl p-6 shadow-card"
            onClick={(e) => e.stopPropagation()}
          >
            <h2 className="text-lg font-semibold text-fg">Delete {object}?</h2>

            <div className="mt-3 rounded-xl border border-red-800/60 bg-red-950/40 px-4 py-3 text-sm text-red-200">
              <strong>This is destructive.</strong> Every frame recorded for{" "}
              <span className="font-medium">{object}</span> — kept, culled, and
              uncurated — will be permanently removed from the database, along
              with its uploaded image. This cannot be undone.
            </div>

            <p className="mt-3 text-sm text-muted">
              You can always add the object back later by re-ingesting from NINA
              or with <span className="text-accentBright">Bulk add</span>.
            </p>

            {error && (
              <div className="mt-3 rounded-xl border border-red-800/60 bg-red-950/40 px-4 py-2 text-sm text-red-300">
                {error}
              </div>
            )}

            <div className="mt-5 flex justify-end gap-2">
              <button
                onClick={() => setOpen(false)}
                disabled={busy}
                className="rounded-lg px-3 py-1.5 text-sm font-medium text-muted transition-colors hover:text-fg disabled:opacity-40"
              >
                Cancel
              </button>
              <button
                onClick={confirmDelete}
                disabled={busy}
                className="rounded-lg bg-red-600 px-3.5 py-1.5 text-sm font-semibold text-white transition-colors hover:bg-red-500 disabled:opacity-40"
              >
                {busy ? "Deleting…" : "Delete object"}
              </button>
            </div>
          </div>
        </div>
      )}
    </>
  );
}
