import { filterColor, filterLabel } from "@/lib/filters";
import type { FilterSlice } from "@/lib/types";

// Stacked horizontal bar showing each filter's share of an object's total
// integration, segments colored from the shared filter palette.
export default function CompositionBar({
  filters,
  height = 10,
}: {
  filters: FilterSlice[];
  height?: number;
}) {
  const total = filters.reduce((s, f) => s + f.minutes, 0) || 1;

  return (
    <div
      className="flex w-full overflow-hidden rounded-full bg-white/5 ring-1 ring-white/5"
      style={{ height }}
      role="img"
      aria-label={filters
        .map((f) => `${filterLabel(f.code)} ${f.minutes} minutes`)
        .join(", ")}
    >
      {filters.map((f) => (
        <div
          key={f.code}
          title={`${filterLabel(f.code)} — ${f.minutes.toLocaleString()} min`}
          style={{
            width: `${(f.minutes / total) * 100}%`,
            backgroundColor: filterColor(f.code),
          }}
        />
      ))}
    </div>
  );
}

// Compact legend of filter chips with per-filter minutes (or hours).
export function FilterLegend({
  filters,
  unit = "min",
}: {
  filters: FilterSlice[];
  unit?: "min" | "hr";
}) {
  return (
    <div className="flex flex-wrap gap-x-3 gap-y-1.5">
      {filters.map((f) => {
        const val =
          unit === "hr"
            ? `${(f.minutes / 60).toFixed(1)}h`
            : `${Math.round(f.minutes)}m`;
        return (
          <span
            key={f.code}
            className="inline-flex items-center gap-1.5 text-xs text-muted"
          >
            <span
              className="inline-block h-2 w-2 rounded-full"
              style={{ backgroundColor: filterColor(f.code) }}
            />
            <span className="font-medium text-fg/90">{f.code}</span>
            <span className="tabular-nums">{val}</span>
          </span>
        );
      })}
    </div>
  );
}
