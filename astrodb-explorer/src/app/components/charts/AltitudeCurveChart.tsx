"use client";

import { useState } from "react";
import {
  ComposedChart,
  Line,
  Scatter,
  XAxis,
  YAxis,
  CartesianGrid,
  ReferenceArea,
  ReferenceDot,
  ReferenceLine,
  ResponsiveContainer,
} from "recharts";
import { filterColor, filterLabel } from "@/lib/filters";
import type { AltitudeNight, TwilightBands, NightFrame } from "@/lib/types";
import { AXIS_TICK, GRID_STROKE } from "./ChartTheme";

// X axis is minutes from local noon (0 → 1440). Convert to a wall-clock label.
function minutesToClock(min: number): string {
  const h = (12 + Math.floor(min / 60)) % 24;
  const m = ((min % 60) + 60) % 60;
  return `${String(h).padStart(2, "0")}:${String(m).padStart(2, "0")}`;
}

// Twilight shading: a pale overlay that's brightest in daylight and fades to
// nothing across astronomical night, so the dark imaging window reads clearly.
const BAND_FILL = "#aab9e8";
function buildBands(b: TwilightBands): { x1: number; x2: number; op: number }[] {
  // Canonical boundaries (noon → noon) with the opacity of each segment.
  const order: [number | null, number][] = [
    [0, 0.18], // daylight
    [b.sunset, 0.12], // civil
    [b.civilEnd, 0.07], // nautical
    [b.nauticalEnd, 0.035], // astronomical twilight
    [b.astroEnd, 0], // astronomical night (clear)
    [b.astroStart, 0.035],
    [b.nauticalStart, 0.07],
    [b.civilStart, 0.12],
    [b.sunrise, 0.18], // daylight again
    [1440, 0],
  ];
  // Replace any missing boundary with the previous one (zero-width segment).
  const xs: number[] = [];
  let prev = 0;
  for (const [x] of order) {
    const v = x ?? prev;
    xs.push(v);
    prev = v;
  }
  const out: { x1: number; x2: number; op: number }[] = [];
  for (let i = 0; i < order.length - 1; i++) {
    const x1 = xs[i];
    const x2 = xs[i + 1];
    const op = order[i][1];
    if (x2 > x1 && op > 0) out.push({ x1, x2, op });
  }
  return out;
}

interface FrameDatum extends NightFrame {
  __frame: true;
}

// What we track while hovering a sub-frame dot.
interface Hover {
  f: FrameDatum;
  cx: number; // pixel position within the chart container
  cy: number;
}

export default function AltitudeCurveChart({ data }: { data: AltitudeNight }) {
  // We drive the hover box ourselves from each dot's own mouse events rather
  // than recharts' shared tooltip — the 721-point curve line dominates axis
  // snapping, so a shared tooltip almost never resolves to a frame.
  const [hover, setHover] = useState<Hover | null>(null);

  // Curve clamped to the horizon so it sits on the axis when below 0°.
  const curve = data.curve.map((s) => ({
    tLocalMinutes: s.tLocalMinutes,
    alt: Math.max(0, s.altDeg),
  }));

  const bands = buildBands(data.bands);

  // One scatter series per filter so each frame dot gets its filter color.
  const byFilter = new Map<string, FrameDatum[]>();
  for (const f of data.frames) {
    const arr = byFilter.get(f.filter) ?? [];
    arr.push({ ...f, __frame: true });
    byFilter.set(f.filter, arr);
  }

  // Sub-frame dot: a generous transparent hit circle (easy to land on) carrying
  // the mouse handlers, plus a small visible colored dot that ignores events so
  // hovering its center doesn't flicker the box.
  function FrameDot(props: { cx?: number; cy?: number; payload?: FrameDatum }) {
    const { cx, cy, payload } = props;
    if (cx == null || cy == null || !payload) return null;
    return (
      <g>
        <circle
          cx={cx}
          cy={cy}
          r={10}
          fill="#fff"
          fillOpacity={0}
          pointerEvents="all"
          style={{ cursor: "pointer" }}
          onMouseEnter={() => setHover({ f: payload, cx, cy })}
          onMouseLeave={() => setHover(null)}
        />
        <circle
          cx={cx}
          cy={cy}
          r={3.5}
          fill={filterColor(payload.filter)}
          fillOpacity={0.95}
          stroke="#0b1024"
          strokeWidth={0.75}
          pointerEvents="none"
        />
      </g>
    );
  }

  const ticks = [0, 180, 360, 540, 720, 900, 1080, 1260, 1440];

  return (
    <div className="relative">
      <ResponsiveContainer width="100%" height={340}>
        <ComposedChart margin={{ top: 12, right: 12, bottom: 0, left: -8 }}>
          {bands.map((b, i) => (
            <ReferenceArea
              key={i}
              x1={b.x1}
              x2={b.x2}
              y1={0}
              y2={90}
              fill={BAND_FILL}
              fillOpacity={b.op}
              stroke="none"
              ifOverflow="hidden"
            />
          ))}

          <CartesianGrid stroke={GRID_STROKE} vertical={false} />
          <XAxis
            type="number"
            dataKey="tLocalMinutes"
            domain={[0, 1440]}
            ticks={ticks}
            tickFormatter={(v: number) => minutesToClock(v).slice(0, 2)}
            tick={AXIS_TICK}
            tickLine={false}
            axisLine={false}
            allowDuplicatedCategory={false}
          />
          <YAxis
            type="number"
            domain={[0, 90]}
            ticks={[0, 30, 60, 90]}
            tick={AXIS_TICK}
            tickLine={false}
            axisLine={false}
            width={40}
            unit="°"
          />

          <Line
            data={curve}
            dataKey="alt"
            name="altitude"
            stroke="#7aa2ff"
            strokeWidth={2}
            dot={false}
            activeDot={false}
            isAnimationActive={false}
          />

          {data.nowMinutes != null &&
            data.nowMinutes >= 0 &&
            data.nowMinutes <= 1440 && (
              <ReferenceLine
                x={data.nowMinutes}
                stroke="#ff7ab8"
                strokeWidth={1.5}
                strokeDasharray="4 3"
                label={{
                  value: "now",
                  position: "insideTopRight",
                  fill: "#ff7ab8",
                  fontSize: 11,
                }}
              />
            )}

          {data.transit && (
            <ReferenceDot
              x={data.transit.tLocalMinutes}
              y={data.transit.altDeg}
              r={4}
              fill="#a9c4ff"
              stroke="#0b1024"
              strokeWidth={2}
              label={{
                value: `Transit ${Math.round(data.transit.altDeg)}°`,
                position: "top",
                fill: "#a9c4ff",
                fontSize: 11,
              }}
            />
          )}

          {Array.from(byFilter.entries()).map(([code, frames]) => (
            <Scatter
              key={code}
              data={frames}
              dataKey="altDeg"
              name={filterLabel(code)}
              shape={<FrameDot />}
              isAnimationActive={false}
            />
          ))}
        </ComposedChart>
      </ResponsiveContainer>

      {/* Hover box: this sub's filter, exposure, and FWHM (+ capture time). */}
      {hover && (
        <div
          className="pointer-events-none absolute z-10 w-max rounded-lg border border-borderGlow/60 bg-surface/95 px-3 py-2 text-xs text-fg shadow-card backdrop-blur"
          style={{
            left: hover.cx,
            top: hover.cy,
            transform: "translate(-50%, calc(-100% - 12px))",
          }}
        >
          <div className="mb-1.5 flex items-center gap-1.5 font-medium">
            <span
              className="h-2.5 w-2.5 rounded-full"
              style={{ backgroundColor: filterColor(hover.f.filter) }}
            />
            {filterLabel(hover.f.filter)}
            <span className="ml-3 text-faint">{hover.f.localTime}</span>
          </div>
          <dl className="grid grid-cols-[auto,1fr] gap-x-4 gap-y-0.5 text-muted">
            <dt>Exposure</dt>
            <dd className="text-right text-fg">{hover.f.durationSec}s</dd>
            <dt>FWHM</dt>
            <dd className="text-right text-fg">{hover.f.fwhm ?? "—"} px</dd>
          </dl>
        </div>
      )}
    </div>
  );
}
