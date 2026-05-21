"use client";

import { useMemo, useState } from "react";
import Link from "next/link";
import type { CatalogObject } from "@/lib/types";

// Dense, sortable, filterable table of one catalog's objects. Rows link to the
// object's metadata page; a small dot flags targets we've actually imaged.

type SortKey =
  | "designation"
  | "commonName"
  | "objectType"
  | "constellation"
  | "magnitude";

interface Column {
  key: SortKey;
  label: string;
  align?: "right";
  numeric?: boolean;
}

const COLUMNS: Column[] = [
  { key: "designation", label: "Designation" },
  { key: "commonName", label: "Common name" },
  { key: "objectType", label: "Type" },
  { key: "constellation", label: "Constellation" },
  { key: "magnitude", label: "Mag", align: "right", numeric: true },
];

const collator = new Intl.Collator(undefined, { numeric: true, sensitivity: "base" });

// Compare with nulls always sorted last, then asc/desc applied.
function cmp(a: CatalogObject, b: CatalogObject, key: SortKey, numeric: boolean): number {
  const av = a[key];
  const bv = b[key];
  if (av == null && bv == null) return 0;
  if (av == null) return 1;
  if (bv == null) return -1;
  if (numeric) return (av as number) - (bv as number);
  return collator.compare(String(av), String(bv));
}

export default function CatalogTable({
  slug,
  objects,
  hasDifficulty,
}: {
  slug: string;
  objects: CatalogObject[];
  hasDifficulty: boolean;
}) {
  const [search, setSearch] = useState("");
  const [type, setType] = useState("");
  const [constellation, setConstellation] = useState("");
  const [capturedOnly, setCapturedOnly] = useState(false);
  const [sortKey, setSortKey] = useState<SortKey>("designation");
  const [asc, setAsc] = useState(true);

  const types = useMemo(
    () => uniqueSorted(objects.map((o) => o.objectType)),
    [objects],
  );
  const constellations = useMemo(
    () => uniqueSorted(objects.map((o) => o.constellation)),
    [objects],
  );
  const capturedCount = useMemo(
    () => objects.filter((o) => o.captured).length,
    [objects],
  );

  const rows = useMemo(() => {
    const q = search.trim().toLowerCase();
    const numeric = COLUMNS.find((c) => c.key === sortKey)?.numeric ?? false;
    const filtered = objects.filter((o) => {
      if (type && o.objectType !== type) return false;
      if (constellation && o.constellation !== constellation) return false;
      if (capturedOnly && !o.captured) return false;
      if (!q) return true;
      return (
        o.designation.toLowerCase().includes(q) ||
        (o.commonName?.toLowerCase().includes(q) ?? false) ||
        (o.objectType?.toLowerCase().includes(q) ?? false)
      );
    });
    const sorted = [...filtered].sort((a, b) => cmp(a, b, sortKey, numeric));
    return asc ? sorted : sorted.reverse();
  }, [objects, search, type, constellation, capturedOnly, sortKey, asc]);

  function toggleSort(key: SortKey) {
    if (key === sortKey) setAsc((v) => !v);
    else {
      setSortKey(key);
      setAsc(true);
    }
  }

  const columns: Column[] = hasDifficulty
    ? [...COLUMNS, { key: "designation", label: "Difficulty" }] // placeholder; rendered specially
    : COLUMNS;

  return (
    <div>
      {/* Controls */}
      <div className="mb-4 flex flex-wrap items-center gap-3">
        <input
          type="search"
          value={search}
          onChange={(e) => setSearch(e.target.value)}
          placeholder="Search name or type…"
          className="glass w-full max-w-xs rounded-lg px-3.5 py-2 text-sm text-fg placeholder:text-faint focus:border-accent focus:outline-none focus:shadow-glowSoft sm:w-auto"
        />
        <Select value={type} onChange={setType} label="All types" options={types} />
        <Select
          value={constellation}
          onChange={setConstellation}
          label="All constellations"
          options={constellations}
        />
        <label className="flex items-center gap-2 text-xs text-muted">
          <input
            type="checkbox"
            checked={capturedOnly}
            onChange={(e) => setCapturedOnly(e.target.checked)}
            className="accent-accent"
          />
          Imaged only
          <span className="text-faint">({capturedCount})</span>
        </label>
        <span className="ml-auto text-xs tabular-nums text-faint">
          {rows.length} of {objects.length}
        </span>
      </div>

      {/* Table */}
      <div className="glass overflow-hidden rounded-2xl shadow-card">
        <div className="overflow-x-auto">
          <table className="w-full border-collapse text-sm">
            <thead>
              <tr className="border-b border-border text-left text-xs uppercase tracking-wide text-faint">
                {columns.map((c, i) => {
                  const isDifficulty = hasDifficulty && i === columns.length - 1;
                  const active = !isDifficulty && c.key === sortKey;
                  return (
                    <th
                      key={c.label}
                      className={`px-4 py-3 font-medium ${c.align === "right" ? "text-right" : ""}`}
                    >
                      {isDifficulty ? (
                        <span>Difficulty</span>
                      ) : (
                        <button
                          onClick={() => toggleSort(c.key)}
                          className={`inline-flex items-center gap-1 transition-colors hover:text-fg ${
                            active ? "text-accentBright" : ""
                          }`}
                        >
                          {c.label}
                          {active && <span aria-hidden>{asc ? "▲" : "▼"}</span>}
                        </button>
                      )}
                    </th>
                  );
                })}
                <th className="px-4 py-3" />
              </tr>
            </thead>
            <tbody>
              {rows.map((o) => (
                <tr
                  key={o.designation}
                  className="group border-b border-white/[0.04] transition-colors hover:bg-white/[0.03] last:border-0"
                >
                  <td className="px-4 py-2.5">
                    <Link
                      href={`/catalog/${slug}/${encodeURIComponent(o.designation)}`}
                      className="flex items-center gap-2 font-medium text-fg transition-colors group-hover:text-accentBright"
                    >
                      {o.captured && (
                        <span
                          className="h-2 w-2 shrink-0 rounded-full bg-nebulaPink shadow-[0_0_8px_rgba(255,122,184,0.7)]"
                          title={`Imaged as ${o.capturedAs}`}
                        />
                      )}
                      {o.designation}
                    </Link>
                  </td>
                  <td className="px-4 py-2.5 text-muted">{o.commonName ?? "—"}</td>
                  <td className="px-4 py-2.5 text-muted">{o.objectType ?? "—"}</td>
                  <td className="px-4 py-2.5 text-muted">{o.constellation ?? "—"}</td>
                  <td className="px-4 py-2.5 text-right tabular-nums text-muted">
                    {o.magnitude == null ? "—" : o.magnitude.toFixed(2)}
                  </td>
                  {hasDifficulty && (
                    <td className="px-4 py-2.5 text-muted">{o.difficulty ?? "—"}</td>
                  )}
                  <td className="px-4 py-2.5 text-right text-faint">
                    <span className="opacity-0 transition-opacity group-hover:opacity-100">
                      →
                    </span>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
        {rows.length === 0 && (
          <p className="py-12 text-center text-sm text-muted">No objects match the filters.</p>
        )}
      </div>
    </div>
  );
}

function uniqueSorted(values: (string | null)[]): string[] {
  return Array.from(new Set(values.filter((v): v is string => !!v))).sort((a, b) =>
    collator.compare(a, b),
  );
}

function Select({
  value,
  onChange,
  label,
  options,
}: {
  value: string;
  onChange: (v: string) => void;
  label: string;
  options: string[];
}) {
  return (
    <select
      value={value}
      onChange={(e) => onChange(e.target.value)}
      className="rounded-lg border border-border bg-surfaceAlt px-2.5 py-2 text-sm text-fg outline-none ring-accent/40 focus:ring-2"
    >
      <option value="">{label}</option>
      {options.map((o) => (
        <option key={o} value={o}>
          {o}
        </option>
      ))}
    </select>
  );
}
