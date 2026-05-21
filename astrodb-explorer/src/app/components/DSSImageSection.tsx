"use client";

import { useEffect, useState } from "react";

// `endpoint` is the GET/POST DSS URL (object pages and catalog pages have
// different routes); `object` is used only for alt text and the download name.
export default function DSSImageSection({
  object,
  endpoint,
}: {
  object: string;
  endpoint: string;
}) {
  const [path, setPath] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [fetching, setFetching] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetch(endpoint)
      .then((r) => r.json())
      .then((d) => setPath(d.path ?? null))
      .catch(() => setPath(null))
      .finally(() => setLoading(false));
  }, [endpoint]);

  async function handleFetch() {
    setFetching(true);
    setError(null);
    try {
      const r = await fetch(endpoint, { method: "POST" });
      const d = await r.json();
      if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`);
      setPath(d.path);
    } catch (e) {
      setError(String(e));
    } finally {
      setFetching(false);
    }
  }

  return (
    <section className="glass rounded-2xl p-5 shadow-card">
      <div className="mb-3 flex items-center justify-between">
        <h2 className="text-sm font-semibold uppercase tracking-wide text-muted">
          DSS Reference Image · 60′ × 60′
        </h2>
        <div className="flex items-center gap-2">
          {path && (
            <a
              href={path}
              download={`dss_${object}.gif`}
              className="rounded-lg bg-accent/15 px-3 py-1.5 text-sm font-medium text-accentBright ring-1 ring-accent/30 transition-colors hover:bg-accent/25"
            >
              Download GIF ↓
            </a>
          )}
          <button
            onClick={handleFetch}
            disabled={fetching}
            className="rounded-lg bg-white/5 px-3 py-1.5 text-sm font-medium text-muted ring-1 ring-white/10 transition-colors hover:bg-white/10 hover:text-fg disabled:opacity-40"
          >
            {fetching ? "Fetching…" : path ? "Re-fetch" : "Fetch DSS Image"}
          </button>
        </div>
      </div>

      {loading && (
        <div className="h-64 animate-pulse rounded-xl bg-white/5" />
      )}

      {!loading && path && (
        <img
          src={path}
          alt={`DSS 60′ field for ${object}`}
          className="w-full rounded-xl"
        />
      )}

      {!loading && !path && !fetching && (
        <div className="flex h-40 items-center justify-center rounded-xl bg-white/5 text-sm text-faint">
          No DSS image downloaded — click &ldquo;Fetch DSS Image&rdquo; above.
        </div>
      )}

      {fetching && (
        <div className="flex h-40 items-center justify-center rounded-xl bg-white/5 text-sm text-muted">
          Fetching from STScI…
        </div>
      )}

      {error && (
        <p className="mt-2 text-xs text-red-400">{error}</p>
      )}
    </section>
  );
}
