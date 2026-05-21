import Link from "next/link";
import { CATALOG_LIST } from "@/lib/catalog";

// Tab strip linking the four catalogs. `active` is the current slug.
export default function CatalogTabs({ active }: { active: string }) {
  return (
    <nav className="flex flex-wrap gap-1.5">
      {CATALOG_LIST.map((c) => {
        const on = c.slug === active;
        return (
          <Link
            key={c.slug}
            href={`/catalog/${c.slug}`}
            className={`rounded-lg px-3.5 py-1.5 text-sm font-medium transition-colors ${
              on
                ? "bg-accent/20 text-accentBright ring-1 ring-accent/30"
                : "text-muted hover:bg-white/[0.04] hover:text-fg"
            }`}
          >
            {c.label}
          </Link>
        );
      })}
    </nav>
  );
}
