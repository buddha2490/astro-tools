// Small presentation helpers shared across the client UI.

export const minutesToHours = (m: number) => m / 60;

export function fmtHours(minutes: number, digits = 1): string {
  return minutesToHours(minutes).toLocaleString(undefined, {
    minimumFractionDigits: digits,
    maximumFractionDigits: digits,
  });
}

export function fmtInt(n: number): string {
  return n.toLocaleString();
}

// "2025-07-26" -> "Jul 26, 2025". Tolerates null / odd values.
export function fmtDate(d: string | null): string {
  if (!d) return "—";
  const dt = new Date(`${d}T00:00:00`);
  if (Number.isNaN(dt.getTime())) return d;
  return dt.toLocaleDateString(undefined, {
    year: "numeric",
    month: "short",
    day: "numeric",
  });
}
