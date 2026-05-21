import type { OverviewStats } from "@/lib/types";
import { fmtInt, fmtDate } from "@/lib/format";

// Headline aggregate stats across the whole database.
export default function StatBand({ stats }: { stats: OverviewStats }) {
  const items: { label: string; value: string; accent?: boolean }[] = [
    { label: "Total Integration", value: `${fmtInt(Math.round(stats.totalHours))} hrs`, accent: true },
    { label: "Targets", value: fmtInt(stats.targetCount) },
    { label: "Frames", value: fmtInt(stats.frameCount) },
    { label: "Nights", value: fmtInt(stats.nightCount) },
  ];

  return (
    <div className="grid grid-cols-2 gap-3 sm:grid-cols-4">
      {items.map((it) => (
        <div
          key={it.label}
          className="glass rounded-xl px-4 py-3.5 shadow-card"
        >
          <div
            className={`text-2xl font-bold tabular-nums sm:text-3xl ${
              it.accent
                ? "text-accentBright [text-shadow:0_0_22px_rgba(122,162,255,0.45)]"
                : "text-fg"
            }`}
          >
            {it.value}
          </div>
          <div className="mt-0.5 text-xs font-medium uppercase tracking-wide text-faint">
            {it.label}
          </div>
        </div>
      ))}
      {(stats.firstDate || stats.lastDate) && (
        <p className="col-span-2 text-xs text-faint sm:col-span-4">
          Spanning {fmtDate(stats.firstDate)} → {fmtDate(stats.lastDate)}
        </p>
      )}
    </div>
  );
}
