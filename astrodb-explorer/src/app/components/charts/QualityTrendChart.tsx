"use client";

import { useState } from "react";
import {
  LineChart,
  Line,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
} from "recharts";
import type { SessionRow } from "@/lib/types";
import { AXIS_TICK, GRID_STROKE, tooltipStyle, shortDate } from "./ChartTheme";

type MetricKey = "hfr" | "fwhm" | "eccentricity" | "guidingRms";

const METRICS: { key: MetricKey; label: string; color: string; unit?: string }[] = [
  { key: "hfr", label: "HFR", color: "#7aa2ff", unit: "px" },
  { key: "fwhm", label: "FWHM", color: "#2dd4bf", unit: "px" },
  { key: "eccentricity", label: "Eccentricity", color: "#f5a623" },
  { key: "guidingRms", label: "Guiding RMS", color: "#fb5e7e", unit: "″" },
];

// Per-session mean quality over time. One metric at a time, toggled by chips.
// Lower is better for all of these (sharper, rounder, tighter guiding).
export default function QualityTrendChart({ sessions }: { sessions: SessionRow[] }) {
  const [metric, setMetric] = useState<MetricKey>("hfr");
  const active = METRICS.find((m) => m.key === metric)!;

  const data = sessions
    .map((s) => ({ date: shortDate(s.date), value: s[metric] }))
    .filter((d) => d.value !== null);

  return (
    <div className="flex flex-col gap-3">
      <div className="flex flex-wrap gap-1.5">
        {METRICS.map((m) => (
          <button
            key={m.key}
            onClick={() => setMetric(m.key)}
            className={`rounded-md px-2.5 py-1 text-xs font-medium transition-colors ${
              metric === m.key
                ? "text-space"
                : "text-muted hover:text-fg"
            }`}
            style={
              metric === m.key
                ? { backgroundColor: m.color }
                : { backgroundColor: "rgba(255,255,255,0.04)" }
            }
          >
            {m.label}
          </button>
        ))}
      </div>

      {data.length === 0 ? (
        <p className="py-12 text-center text-sm text-faint">
          No {active.label} data recorded for this target.
        </p>
      ) : (
        <ResponsiveContainer width="100%" height={240}>
          <LineChart data={data} margin={{ top: 8, right: 8, bottom: 0, left: -8 }}>
            <CartesianGrid stroke={GRID_STROKE} vertical={false} />
            <XAxis dataKey="date" tick={AXIS_TICK} tickLine={false} axisLine={false} minTickGap={16} />
            <YAxis
              tick={AXIS_TICK}
              tickLine={false}
              axisLine={false}
              width={48}
              unit={active.unit ?? ""}
              domain={["auto", "auto"]}
            />
            <Tooltip {...tooltipStyle()} formatter={(v) => [`${v}${active.unit ?? ""}`, active.label]} />
            <Line
              type="monotone"
              dataKey="value"
              name={active.label}
              stroke={active.color}
              strokeWidth={2}
              dot={{ r: 2.5, fill: active.color }}
              activeDot={{ r: 4 }}
            />
          </LineChart>
        </ResponsiveContainer>
      )}
    </div>
  );
}
