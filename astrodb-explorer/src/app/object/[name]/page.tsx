"use client";

import { use, useEffect, useState } from "react";
import Link from "next/link";
import type { ObjectDetail } from "@/lib/types";
import { fmtHours, fmtInt, fmtDate } from "@/lib/format";
import CompositionBar, { FilterLegend } from "@/app/components/CompositionBar";
import QualityStatCards from "@/app/components/QualityStatCards";
import SessionChart from "@/app/components/charts/SessionChart";
import QualityTrendChart from "@/app/components/charts/QualityTrendChart";
import AltitudeSection from "@/app/components/AltitudeSection";
import ObjectImageSection from "@/app/components/ObjectImageSection";
import UpdateSubsButton from "@/app/components/UpdateSubsButton";

export default function ObjectPage({
  params,
}: {
  params: Promise<{ name: string }>;
}) {
  const { name } = use(params);
  const object = decodeURIComponent(name);
  const enc = encodeURIComponent(object);

  const [detail, setDetail] = useState<ObjectDetail | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetch(`/api/object/${enc}/detail`)
      .then((r) => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      })
      .then(setDetail)
      .catch((e) => setError(String(e)));
  }, [enc]);

  return (
    <main className="mx-auto max-w-5xl px-6 py-10">
      <Link
        href="/"
        className="inline-flex items-center gap-1.5 text-sm text-muted transition-colors hover:text-accentBright"
      >
        <span aria-hidden>←</span> All targets
      </Link>

      {error && (
        <div className="mt-6 rounded-xl border border-red-800/60 bg-red-950/40 px-4 py-3 text-sm text-red-300">
          Failed to load {object}: {error}
        </div>
      )}

      {!detail && !error && (
        <div className="mt-6 space-y-4">
          <div className="glass h-28 animate-pulse rounded-2xl" />
          <div className="glass h-64 animate-pulse rounded-2xl" />
        </div>
      )}

      {detail && (
        <div className="mt-5 space-y-8 animate-fade-up">
          {/* Header */}
          <header>
            <div className="flex flex-wrap items-center gap-3">
              <h1 className="bg-gradient-to-r from-accentBright via-fg to-nebula bg-clip-text text-3xl font-extrabold tracking-tight text-transparent sm:text-4xl">
                {detail.object}
              </h1>
              <span
                className={`rounded-full px-2.5 py-0.5 text-[11px] font-medium uppercase tracking-wide ring-1 ${
                  detail.isNarrowband
                    ? "bg-nebula/10 text-nebula ring-nebula/30"
                    : "bg-accent/10 text-accent ring-accent/30"
                }`}
              >
                {detail.isNarrowband ? "Narrowband" : "Broadband"}
              </span>
              <div className="ml-auto flex items-center gap-2">
                <UpdateSubsButton object={detail.object} />
                <a
                  href={`/api/object/${enc}/astrobin.csv`}
                  className="rounded-lg bg-accent/15 px-3 py-1.5 text-sm font-medium text-accentBright ring-1 ring-accent/30 transition-colors hover:bg-accent/25"
                >
                  Astrobin CSV ↓
                </a>
              </div>
            </div>
            <p className="mt-2 text-sm text-muted">
              {fmtHours(detail.totalMinutes)} hrs over {fmtInt(detail.nights)}{" "}
              {detail.nights === 1 ? "night" : "nights"} ·{" "}
              {fmtInt(detail.frames)} frames · {fmtDate(detail.firstDate)} →{" "}
              {fmtDate(detail.lastDate)}
            </p>
          </header>

          {/* Composition */}
          <section className="glass rounded-2xl p-5 shadow-card">
            <h2 className="mb-3 text-sm font-semibold uppercase tracking-wide text-muted">
              Filter Composition
            </h2>
            <CompositionBar filters={detail.filters} height={14} />
            <div className="mt-3">
              <FilterLegend filters={detail.filters} unit="hr" />
            </div>
          </section>

          {/* Quality stats */}
          <section>
            <h2 className="mb-3 text-sm font-semibold uppercase tracking-wide text-muted">
              Frame Quality
            </h2>
            <QualityStatCards quality={detail.quality} />
          </section>

          {/* Charts */}
          <div className="grid gap-6 lg:grid-cols-2">
            <section className="glass rounded-2xl p-5 shadow-card">
              <h2 className="mb-4 text-sm font-semibold uppercase tracking-wide text-muted">
                Integration by Night
              </h2>
              <SessionChart sessions={detail.sessions} filters={detail.filters} />
            </section>
            <section className="glass rounded-2xl p-5 shadow-card">
              <h2 className="mb-4 text-sm font-semibold uppercase tracking-wide text-muted">
                Quality Trend
              </h2>
              <QualityTrendChart sessions={detail.sessions} />
            </section>
          </div>

          {/* Full-width bottom row: per-night sky position */}
          <AltitudeSection
            object={detail.object}
            dates={detail.sessions.map((s) => s.date)}
          />

          {/* My uploaded image */}
          <ObjectImageSection object={detail.object} />
        </div>
      )}
    </main>
  );
}
