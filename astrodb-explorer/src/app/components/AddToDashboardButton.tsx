"use client";

import { useState } from "react";

// Toggles a catalog object's presence on the dashboard planned list.
// POST /api/planned to add, DELETE /api/planned/[designation] to remove.
export default function AddToDashboardButton({
  slug,
  designation,
  initialPlanned,
}: {
  slug: string;
  designation: string;
  initialPlanned: boolean;
}) {
  const [planned, setPlanned] = useState(initialPlanned);
  const [busy, setBusy] = useState(false);
  const [error, setError] = useState(false);

  async function toggle() {
    setBusy(true);
    setError(false);
    try {
      if (planned) {
        const r = await fetch(`/api/planned/${encodeURIComponent(designation)}`, {
          method: "DELETE",
        });
        if (!r.ok) throw new Error();
        setPlanned(false);
      } else {
        const r = await fetch("/api/planned", {
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({ slug, designation }),
        });
        if (!r.ok) throw new Error();
        setPlanned(true);
      }
    } catch {
      setError(true);
    } finally {
      setBusy(false);
    }
  }

  return (
    <button
      onClick={toggle}
      disabled={busy}
      className={`inline-flex items-center gap-1.5 rounded-lg px-3 py-1.5 text-sm font-medium ring-1 transition-colors disabled:opacity-50 ${
        planned
          ? "bg-accent/20 text-accentBright ring-accent/30 hover:bg-red-500/15 hover:text-red-300 hover:ring-red-500/30"
          : "bg-accent/15 text-accentBright ring-accent/30 hover:bg-accent/25"
      }`}
      title={planned ? "Remove from dashboard" : "Add to dashboard for planning"}
    >
      {error ? (
        "Try again"
      ) : busy ? (
        "…"
      ) : planned ? (
        <>
          <span aria-hidden>✓</span> On dashboard
        </>
      ) : (
        <>
          <span aria-hidden>＋</span> Add to dashboard
        </>
      )}
    </button>
  );
}
