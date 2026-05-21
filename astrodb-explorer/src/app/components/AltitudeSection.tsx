"use client";

import { useEffect, useState } from "react";
import type { AltitudeNight } from "@/lib/types";
import { fmtDate } from "@/lib/format";
import AltitudeCurveChart from "./charts/AltitudeCurveChart";

// Full-width "sky position" panel: pick a night and see the object's altitude
// curve, twilight bands, and where each sub-frame landed. Fetches per night so
// the curve (which changes nightly) is always recomputed for the chosen date.
export default function AltitudeSection({
  object,
  dates,
}: {
  object: string;
  dates: string[]; // imaging-night civil dates, chronological
}) {
  const enc = encodeURIComponent(object);
  // Default to the most recent night.
  const [date, setDate] = useState(() => dates[dates.length - 1] ?? "");
  const [data, setData] = useState<AltitudeNight | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(false);

  useEffect(() => {
    if (!date) return;
    let cancelled = false;
    setLoading(true);
    setError(null);
    fetch(`/api/object/${enc}/night/${date}/altitude`)
      .then((r) => {
        if (r.status === 404) throw new Error("coords-not-found");
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      })
      .then((d: AltitudeNight) => !cancelled && setData(d))
      .catch((e) => !cancelled && setError(String(e.message ?? e)))
      .finally(() => !cancelled && setLoading(false));
    return () => {
      cancelled = true;
    };
  }, [enc, date]);

  return (
    <section className="glass rounded-2xl p-5 shadow-card">
      <div className="mb-4 flex flex-wrap items-center gap-3">
        <h2 className="text-sm font-semibold uppercase tracking-wide text-muted">
          Sky Position
        </h2>
        {data && (
          <span className="text-xs text-faint">
            transit {data.transit ? `${Math.round(data.transit.altDeg)}°` : "—"}
            {" · "}
            RA {data.coord.raDeg.toFixed(1)}° / Dec {data.coord.decDeg.toFixed(1)}°
          </span>
        )}
        <label className="ml-auto flex items-center gap-2 text-xs text-muted">
          Night
          <select
            value={date}
            onChange={(e) => setDate(e.target.value)}
            className="rounded-lg border border-border bg-surfaceAlt px-2.5 py-1.5 text-sm text-fg outline-none ring-accent/40 focus:ring-2"
          >
            {[...dates].reverse().map((d) => (
              <option key={d} value={d}>
                {fmtDate(d)}
              </option>
            ))}
          </select>
        </label>
      </div>

      {error === "coords-not-found" ? (
        <p className="py-16 text-center text-sm text-faint">
          Altitude unavailable — coordinates not found for {object}.
        </p>
      ) : error ? (
        <p className="py-16 text-center text-sm text-red-300">
          Failed to load altitude curve: {error}
        </p>
      ) : !data || loading ? (
        <div className="h-[340px] animate-pulse rounded-xl bg-white/[0.03]" />
      ) : (
        <>
          <AltitudeCurveChart data={data} />
          <p className="mt-2 text-center text-[11px] text-faint">
            Local time at {data.site.name} ({data.site.timeZone}) · dots are
            captured sub-frames, colored by filter
          </p>
        </>
      )}
    </section>
  );
}
