"use client";

import { useEffect, useMemo, useState } from "react";
import type { DashboardData, ObjectCard } from "@/lib/types";
import StatBand from "./components/StatBand";
import ObjectCardTile from "./components/ObjectCardTile";

type SortKey = "integration" | "frames" | "recent" | "name";

const SORTS: { key: SortKey; label: string }[] = [
  { key: "integration", label: "Integration" },
  { key: "frames", label: "Frames" },
  { key: "recent", label: "Most recent" },
  { key: "name", label: "Name" },
];

export default function Home() {
  const [data, setData] = useState<DashboardData | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [search, setSearch] = useState("");
  const [sort, setSort] = useState<SortKey>("integration");

  useEffect(() => {
    fetch("/api/dashboard")
      .then((r) => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      })
      .then(setData)
      .catch((e) => setError(String(e)));
  }, []);

  const handleRemove = (object: string) =>
    setData((d) =>
      d ? { ...d, objects: d.objects.filter((o) => o.object !== object) } : d,
    );

  const objects = useMemo(() => {
    if (!data) return [];
    const q = search.trim().toLowerCase();
    const filtered = q
      ? data.objects.filter((o) => o.object.toLowerCase().includes(q))
      : data.objects;
    return [...filtered].sort(sortFns[sort]);
  }, [data, search, sort]);

  return (
    <main className="mx-auto max-w-6xl px-6 py-10">
      <header className="mb-8 animate-fade-up">
        <h1 className="bg-gradient-to-r from-accentBright via-fg to-nebula bg-clip-text text-3xl font-extrabold tracking-tight text-transparent sm:text-4xl">
          astroDB Explorer
        </h1>
        <p className="mt-1.5 text-sm text-muted">
          A journey through the deep sky, one photon at a time.
        </p>
      </header>

      {error && (
        <div className="rounded-xl border border-red-800/60 bg-red-950/40 px-4 py-3 text-sm text-red-300">
          Failed to load: {error}
          <p className="mt-1 text-xs text-red-400/70">
            The database lives on <code>aria-bot</code> — check that it&apos;s
            reachable (Tailscale up) and <code>.env.local</code> is set.
          </p>
        </div>
      )}

      {!data && !error && (
        <div className="grid gap-5 sm:grid-cols-2 lg:grid-cols-3">
          {Array.from({ length: 6 }).map((_, i) => (
            <div
              key={i}
              className="glass h-48 animate-pulse rounded-2xl"
              style={{ animationDelay: `${i * 80}ms` }}
            />
          ))}
        </div>
      )}

      {data && (
        <>
          <section className="mb-8 animate-fade-up">
            <StatBand stats={data.stats} />
          </section>

          <section className="mb-5 flex flex-wrap items-center gap-3">
            <input
              type="search"
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              placeholder="Search targets…"
              className="glass w-full max-w-xs rounded-lg px-3.5 py-2 text-sm text-fg placeholder:text-faint focus:border-accent focus:outline-none focus:shadow-glowSoft sm:w-auto"
            />
            <div className="ml-auto flex items-center gap-2 text-sm">
              <span className="text-faint">Sort</span>
              <div className="flex gap-1 rounded-lg border border-border bg-surface/60 p-0.5">
                {SORTS.map((s) => (
                  <button
                    key={s.key}
                    onClick={() => setSort(s.key)}
                    className={`rounded-md px-2.5 py-1 text-xs font-medium transition-colors ${
                      sort === s.key
                        ? "bg-accent/20 text-accentBright"
                        : "text-muted hover:text-fg"
                    }`}
                  >
                    {s.label}
                  </button>
                ))}
              </div>
            </div>
          </section>

          {objects.length === 0 ? (
            <p className="py-16 text-center text-muted">
              No targets match “{search}”.
            </p>
          ) : (
            <section className="grid gap-5 sm:grid-cols-2 lg:grid-cols-3">
              {objects.map((card) => (
                <ObjectCardTile
                  key={card.object}
                  card={card}
                  onRemove={handleRemove}
                />
              ))}
            </section>
          )}
        </>
      )}
    </main>
  );
}

const sortFns: Record<SortKey, (a: ObjectCard, b: ObjectCard) => number> = {
  integration: (a, b) => b.totalMinutes - a.totalMinutes,
  frames: (a, b) => b.frames - a.frames,
  recent: (a, b) => (b.lastDate ?? "").localeCompare(a.lastDate ?? ""),
  name: (a, b) => a.object.localeCompare(b.object, undefined, { numeric: true }),
};
