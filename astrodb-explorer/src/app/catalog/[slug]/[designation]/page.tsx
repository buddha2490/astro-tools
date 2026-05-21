"use client";

import { use, useEffect, useState } from "react";
import Link from "next/link";
import { notFound } from "next/navigation";
import type { CatalogObject } from "@/lib/types";
import { isCatalogSlug, CATALOGS } from "@/lib/catalog";
import TonightSkySection from "@/app/components/TonightSkySection";
import AddToDashboardButton from "@/app/components/AddToDashboardButton";
import DSSImageSection from "@/app/components/DSSImageSection";

// RA in hours -> "HHh MMm". Dec in deg -> "+DD° MM′".
function fmtRa(hours: number | null): string {
  if (hours == null) return "—";
  const h = Math.floor(hours);
  const m = Math.round((hours - h) * 60);
  return `${h}h ${String(m).padStart(2, "0")}m`;
}
function fmtDec(deg: number | null): string {
  if (deg == null) return "—";
  const sign = deg < 0 ? "−" : "+";
  const a = Math.abs(deg);
  const d = Math.floor(a);
  const m = Math.round((a - d) * 60);
  return `${sign}${d}° ${String(m).padStart(2, "0")}′`;
}

export default function CatalogObjectPage({
  params,
}: {
  params: Promise<{ slug: string; designation: string }>;
}) {
  const { slug, designation } = use(params);
  if (!isCatalogSlug(slug)) notFound();
  const meta = CATALOGS[slug];
  const desig = decodeURIComponent(designation);
  const enc = encodeURIComponent(desig);

  const [obj, setObj] = useState<CatalogObject | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetch(`/api/catalog/${slug}/${enc}`)
      .then((r) => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      })
      .then(setObj)
      .catch((e) => setError(String(e)));
  }, [slug, enc]);

  return (
    <main className="mx-auto max-w-4xl px-6 py-10">
      <Link
        href={`/catalog/${slug}`}
        className="inline-flex items-center gap-1.5 text-sm text-muted transition-colors hover:text-accentBright"
      >
        <span aria-hidden>←</span> {meta.label}
      </Link>

      {error && (
        <div className="mt-6 rounded-xl border border-red-800/60 bg-red-950/40 px-4 py-3 text-sm text-red-300">
          Failed to load {desig}: {error}
        </div>
      )}

      {!obj && !error && (
        <div className="mt-6 space-y-4">
          <div className="glass h-28 animate-pulse rounded-2xl" />
          <div className="glass h-64 animate-pulse rounded-2xl" />
        </div>
      )}

      {obj && (
        <div className="mt-5 space-y-8 animate-fade-up">
          {/* Header */}
          <header>
            <div className="flex flex-wrap items-center gap-3">
              <h1 className="bg-gradient-to-r from-accentBright via-fg to-nebula bg-clip-text text-3xl font-extrabold tracking-tight text-transparent sm:text-4xl">
                {obj.designation}
              </h1>
              {obj.objectType && (
                <span className="rounded-full bg-nebula/10 px-2.5 py-0.5 text-[11px] font-medium uppercase tracking-wide text-nebula ring-1 ring-nebula/30">
                  {obj.objectType}
                </span>
              )}
              <div className="ml-auto flex flex-wrap items-center gap-2">
                {obj.captured && obj.capturedAs && (
                  <Link
                    href={`/object/${encodeURIComponent(obj.capturedAs)}`}
                    className="inline-flex items-center gap-1.5 rounded-lg bg-nebulaPink/15 px-3 py-1.5 text-sm font-medium text-nebulaPink ring-1 ring-nebulaPink/30 transition-colors hover:bg-nebulaPink/25"
                  >
                    <span className="h-2 w-2 rounded-full bg-nebulaPink" />
                    View imaging data →
                  </Link>
                )}
                <AddToDashboardButton
                  slug={slug}
                  designation={obj.designation}
                  initialPlanned={obj.planned}
                />
              </div>
            </div>
            {obj.commonName && (
              <p className="mt-2 text-lg text-muted">{obj.commonName}</p>
            )}
          </header>

          {/* Metadata */}
          <section className="glass rounded-2xl p-5 shadow-card">
            <h2 className="mb-4 text-sm font-semibold uppercase tracking-wide text-muted">
              Metadata
            </h2>
            <dl className="grid grid-cols-1 gap-x-8 gap-y-4 sm:grid-cols-2">
              <Field label="Constellation" value={obj.constellation} />
              <Field
                label="Magnitude"
                value={
                  obj.magnitude == null
                    ? null
                    : `${obj.magnitude.toFixed(2)}${obj.magnitudeBand ? ` (${obj.magnitudeBand})` : ""}`
                }
              />
              <Field
                label="Size"
                value={obj.sizeArcmin ? `${obj.sizeArcmin} arcmin` : null}
              />
              <Field
                label="Right Ascension"
                value={`${fmtRa(obj.raHours)}${obj.raDeg != null ? `  ·  ${obj.raDeg.toFixed(3)}°` : ""}`}
              />
              <Field label="Declination" value={fmtDec(obj.decDeg)} />
              <Field
                label="SIMBAD type"
                value={
                  obj.simbadOtypeLabel
                    ? `${obj.simbadOtypeLabel}${obj.simbadOtype ? ` (${obj.simbadOtype})` : ""}`
                    : obj.simbadOtype
                }
              />
              <Field label="SIMBAD ID" value={obj.simbadMainId} mono />
              {meta.hasDifficulty && (
                <Field label="Difficulty" value={obj.difficulty} />
              )}
            </dl>
          </section>

          {/* DSS reference image */}
          <DSSImageSection
            object={obj.designation}
            endpoint={`/api/catalog/${slug}/${enc}/dss`}
          />

          {/* Tonight's sky */}
          <TonightSkySection slug={slug} designation={obj.designation} />
        </div>
      )}
    </main>
  );
}

function Field({
  label,
  value,
  mono,
}: {
  label: string;
  value: string | null;
  mono?: boolean;
}) {
  return (
    <div>
      <dt className="text-xs uppercase tracking-wide text-faint">{label}</dt>
      <dd className={`mt-0.5 text-sm text-fg ${mono ? "font-mono text-[13px]" : ""}`}>
        {value ?? "—"}
      </dd>
    </div>
  );
}
