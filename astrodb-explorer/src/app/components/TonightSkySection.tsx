"use client";

import { useEffect, useMemo, useState } from "react";
import type { AltitudeNight } from "@/lib/types";
import AltitudeCurveChart from "./charts/AltitudeCurveChart";

// "Tonight" altitude panel for a catalog object: reuses the per-night altitude
// chart (twilight bands + curve) but with no captured frames and a live "now"
// marker. A small summary answers "is it worth shooting tonight?" — peak
// altitude during astronomical darkness and where it sits right now.

function minutesToClock(min: number): string {
  const h = (12 + Math.floor(min / 60)) % 24;
  const m = ((min % 60) + 60) % 60;
  return `${String(h).padStart(2, "0")}:${String(m).padStart(2, "0")}`;
}

interface Summary {
  peakDarkAlt: number | null; // max altitude during astronomical night
  peakDarkTime: string | null;
  nowAlt: number | null; // altitude at the live moment (if within window)
}

function summarize(d: AltitudeNight): Summary {
  const { astroEnd, astroStart } = d.bands;
  let peakDarkAlt: number | null = null;
  let peakDarkTime: string | null = null;
  if (astroEnd != null && astroStart != null) {
    for (const s of d.curve) {
      if (s.tLocalMinutes < astroEnd || s.tLocalMinutes > astroStart) continue;
      if (peakDarkAlt == null || s.altDeg > peakDarkAlt) {
        peakDarkAlt = s.altDeg;
        peakDarkTime = minutesToClock(s.tLocalMinutes);
      }
    }
  }

  let nowAlt: number | null = null;
  if (d.nowMinutes != null) {
    let best = Infinity;
    for (const s of d.curve) {
      const gap = Math.abs(s.tLocalMinutes - d.nowMinutes);
      if (gap < best) {
        best = gap;
        nowAlt = s.altDeg;
      }
    }
  }
  return { peakDarkAlt, peakDarkTime, nowAlt };
}

export default function TonightSkySection({
  slug,
  designation,
}: {
  slug: string;
  designation: string;
}) {
  const enc = encodeURIComponent(designation);
  const [data, setData] = useState<AltitudeNight | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    fetch(`/api/catalog/${slug}/${enc}/tonight`)
      .then((r) => {
        if (r.status === 404) throw new Error("coords-not-found");
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      })
      .then((d: AltitudeNight) => !cancelled && setData(d))
      .catch((e) => !cancelled && setError(String(e.message ?? e)));
    return () => {
      cancelled = true;
    };
  }, [slug, enc]);

  const summary = useMemo(() => (data ? summarize(data) : null), [data]);

  return (
    <section className="glass rounded-2xl p-5 shadow-card">
      <div className="mb-4 flex flex-wrap items-center gap-x-4 gap-y-1">
        <h2 className="text-sm font-semibold uppercase tracking-wide text-muted">
          Tonight&apos;s Sky
        </h2>
        {data && summary && (
          <span className="text-xs text-faint">
            transit {data.transit ? `${Math.round(data.transit.altDeg)}°` : "—"}
            {summary.peakDarkAlt != null && (
              <>
                {" · "}peak in darkness {Math.round(summary.peakDarkAlt)}° @{" "}
                {summary.peakDarkTime}
              </>
            )}
            {summary.nowAlt != null && (
              <>
                {" · "}now {Math.round(summary.nowAlt)}°
              </>
            )}
          </span>
        )}
      </div>

      {error === "coords-not-found" ? (
        <p className="py-16 text-center text-sm text-faint">
          No coordinates on file for {designation}.
        </p>
      ) : error ? (
        <p className="py-16 text-center text-sm text-red-300">
          Failed to load tonight&apos;s sky: {error}
        </p>
      ) : !data ? (
        <div className="h-[340px] animate-pulse rounded-xl bg-white/[0.03]" />
      ) : (
        <>
          <AltitudeCurveChart data={data} />
          <p className="mt-2 text-center text-[11px] text-faint">
            {data.date} · local time at {data.site.name} ({data.site.timeZone}) ·
            shaded = twilight, clear = astronomical darkness
          </p>
        </>
      )}
    </section>
  );
}
