import type { QualityStat } from "@/lib/types";

// Aggregate quality metrics shown as "mean (min–max)" tiles, echoing the R
// objectMetaData() report.
export default function QualityStatCards({ quality }: { quality: QualityStat[] }) {
  const fmt = (n: number | null) => (n === null ? "—" : n.toLocaleString());
  return (
    <div className="grid grid-cols-2 gap-3 sm:grid-cols-3 lg:grid-cols-5">
      {quality.map((q) => (
        <div key={q.key} className="glass rounded-xl px-3.5 py-3 shadow-card">
          <div className="text-[11px] font-medium uppercase tracking-wide text-faint">
            {q.metric}
          </div>
          <div className="mt-1 flex items-baseline gap-1">
            <span className="text-xl font-bold tabular-nums text-fg">
              {fmt(q.mean)}
            </span>
            {q.unit && q.mean !== null && (
              <span className="text-xs text-muted">{q.unit}</span>
            )}
          </div>
          <div className="mt-0.5 text-[11px] tabular-nums text-faint">
            {q.mean === null ? "no data" : `${fmt(q.min)}–${fmt(q.max)}`}
          </div>
        </div>
      ))}
    </div>
  );
}
