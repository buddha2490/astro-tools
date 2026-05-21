"use client";

import {
  BarChart,
  Bar,
  XAxis,
  YAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
} from "recharts";
import { filterColor, filterLabel } from "@/lib/filters";
import type { SessionRow, FilterSlice } from "@/lib/types";
import { AXIS_TICK, GRID_STROKE, tooltipStyle, shortDate } from "./ChartTheme";

// Stacked bar: minutes captured per filter on each imaging night.
export default function SessionChart({
  sessions,
  filters,
}: {
  sessions: SessionRow[];
  filters: FilterSlice[];
}) {
  const codes = filters.map((f) => f.code);
  const data = sessions.map((s) => ({
    date: shortDate(s.date),
    ...Object.fromEntries(codes.map((c) => [c, s.byFilter[c] ?? 0])),
  }));

  return (
    <ResponsiveContainer width="100%" height={260}>
      <BarChart data={data} margin={{ top: 8, right: 8, bottom: 0, left: -8 }}>
        <CartesianGrid stroke={GRID_STROKE} vertical={false} />
        <XAxis dataKey="date" tick={AXIS_TICK} tickLine={false} axisLine={false} minTickGap={16} />
        <YAxis
          tick={AXIS_TICK}
          tickLine={false}
          axisLine={false}
          unit="m"
          width={48}
        />
        <Tooltip {...tooltipStyle()} cursor={{ fill: "rgba(122,162,255,0.06)" }} />
        {codes.map((c) => (
          <Bar
            key={c}
            dataKey={c}
            name={filterLabel(c)}
            stackId="mins"
            fill={filterColor(c)}
            radius={[2, 2, 0, 0]}
          />
        ))}
      </BarChart>
    </ResponsiveContainer>
  );
}
