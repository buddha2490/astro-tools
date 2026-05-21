"use client";

import Link from "next/link";
import type { ObjectCard } from "@/lib/types";
import { fmtHours, fmtInt, fmtDate } from "@/lib/format";
import CompositionBar, { FilterLegend } from "./CompositionBar";

// A target tile. Two flavors:
//  - imaged: name, total hours, frame count, filter-composition bar + legend,
//    last imaged date; links to the detail page.
//  - planned: an empty wishlist tile (no frames) added from a catalog; shows
//    type/constellation, links to its catalog page, and offers a remove button.
export default function ObjectCardTile({
  card,
  onRemove,
}: {
  card: ObjectCard;
  onRemove?: (object: string) => void;
}) {
  if (card.planned) return <PlannedTile card={card} onRemove={onRemove} />;

  return (
    <Link
      href={`/object/${encodeURIComponent(card.object)}`}
      className="glass group flex flex-col gap-4 rounded-2xl p-5 text-left shadow-card transition-all duration-200 hover:-translate-y-0.5 hover:border-borderGlow hover:shadow-glowSoft focus:outline-none focus-visible:ring-2 focus-visible:ring-accent"
    >
      {card.imagePath && (
        <div className="-mx-5 -mt-5 mb-0 h-28 overflow-hidden rounded-t-2xl">
          {/* eslint-disable-next-line @next/next/no-img-element */}
          <img
            src={card.imagePath}
            alt={`Image of ${card.object}`}
            className="h-full w-full object-cover opacity-50 transition-opacity duration-300 group-hover:opacity-70"
          />
        </div>
      )}
      <div className="flex items-start justify-between gap-3">
        <h3 className="text-lg font-semibold leading-tight text-fg transition-colors group-hover:text-accentBright">
          {card.object}
        </h3>
        <span
          className={`shrink-0 rounded-full px-2 py-0.5 text-[10px] font-medium uppercase tracking-wide ring-1 ${
            card.isNarrowband
              ? "bg-nebula/10 text-nebula ring-nebula/30"
              : "bg-accent/10 text-accent ring-accent/30"
          }`}
        >
          {card.isNarrowband ? "Narrowband" : "Broadband"}
        </span>
      </div>

      <div className="flex items-baseline gap-2">
        <span className="text-3xl font-bold tabular-nums text-fg [text-shadow:0_0_18px_rgba(122,162,255,0.35)]">
          {fmtHours(card.totalMinutes)}
        </span>
        <span className="text-sm text-muted">hrs</span>
        <span className="ml-auto text-xs tabular-nums text-faint">
          {fmtInt(card.frames)} frames
        </span>
      </div>

      <CompositionBar filters={card.filters} />

      <FilterLegend filters={card.filters} unit="hr" />

      <div className="mt-auto border-t border-white/5 pt-3 text-xs text-faint">
        Last imaged{" "}
        <span className="text-muted">{fmtDate(card.lastDate)}</span>
      </div>
    </Link>
  );
}

function PlannedTile({
  card,
  onRemove,
}: {
  card: ObjectCard;
  onRemove?: (object: string) => void;
}) {
  const href = card.catalogSlug
    ? `/catalog/${card.catalogSlug}/${encodeURIComponent(card.object)}`
    : "#";

  async function handleRemove(e: React.MouseEvent) {
    e.preventDefault();
    e.stopPropagation();
    try {
      const r = await fetch(`/api/planned/${encodeURIComponent(card.object)}`, {
        method: "DELETE",
      });
      if (r.ok) onRemove?.(card.object);
    } catch {
      /* leave the tile in place on failure */
    }
  }

  return (
    <Link
      href={href}
      className="glass group relative flex flex-col gap-4 rounded-2xl border-dashed p-5 text-left shadow-card transition-all duration-200 hover:-translate-y-0.5 hover:border-borderGlow hover:shadow-glowSoft focus:outline-none focus-visible:ring-2 focus-visible:ring-accent"
      style={{ borderStyle: "dashed" }}
    >
      <div className="flex items-start justify-between gap-3">
        <h3 className="text-lg font-semibold leading-tight text-fg transition-colors group-hover:text-accentBright">
          {card.object}
        </h3>
        <span className="shrink-0 rounded-full bg-nebulaPink/10 px-2 py-0.5 text-[10px] font-medium uppercase tracking-wide text-nebulaPink ring-1 ring-nebulaPink/30">
          Planned
        </span>
      </div>

      <div className="flex items-baseline gap-2">
        <span className="text-3xl font-bold text-faint">☆</span>
        <span className="text-sm text-muted">
          {card.objectType ?? "Target"}
        </span>
      </div>

      <div className="flex h-2.5 w-full items-center rounded-full bg-white/5 ring-1 ring-white/5" />

      <p className="text-xs text-faint">
        {card.constellation ? `In ${card.constellation} · ` : ""}Not yet imaged
      </p>

      <div className="mt-auto flex items-center justify-between border-t border-white/5 pt-3 text-xs text-faint">
        <span>Planning target</span>
        {onRemove && (
          <button
            onClick={handleRemove}
            className="rounded px-1.5 py-0.5 text-faint transition-colors hover:bg-red-500/15 hover:text-red-300"
            title="Remove from dashboard"
            aria-label={`Remove ${card.object} from dashboard`}
          >
            Remove
          </button>
        )}
      </div>
    </Link>
  );
}
