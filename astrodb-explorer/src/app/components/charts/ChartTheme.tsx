// Shared Recharts styling tokens + a dark-theme tooltip used by the object
// page charts. Keeping these in one place so every chart reads consistently.

export const AXIS_TICK = { fill: "#8a96b4", fontSize: 11 };
export const GRID_STROKE = "rgba(51, 67, 107, 0.35)";

export function tooltipStyle() {
  return {
    contentStyle: {
      background: "rgba(15, 22, 38, 0.95)",
      border: "1px solid rgba(51, 67, 107, 0.6)",
      borderRadius: 10,
      color: "#e8edf9",
      fontSize: 12,
      boxShadow: "0 8px 30px -12px rgba(0,0,0,0.8)",
    },
    labelStyle: { color: "#8a96b4", marginBottom: 4 },
    itemStyle: { color: "#e8edf9" },
  };
}

// "2025-07-26" -> "Jul 26" for compact axis labels.
export function shortDate(d: string): string {
  const dt = new Date(`${d}T00:00:00`);
  if (Number.isNaN(dt.getTime())) return d;
  return dt.toLocaleDateString(undefined, { month: "short", day: "numeric" });
}
