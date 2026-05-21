"use client";

import Link from "next/link";
import { usePathname } from "next/navigation";

// Slim global nav. "Catalogs" stays active across all /catalog/* routes.
const LINKS = [
  { href: "/", label: "Dashboard", match: (p: string) => p === "/" },
  { href: "/catalog", label: "Catalogs", match: (p: string) => p.startsWith("/catalog") },
];

export default function Nav() {
  const pathname = usePathname();
  return (
    <header className="sticky top-0 z-20 border-b border-border/60 bg-space/70 backdrop-blur">
      <nav className="mx-auto flex max-w-6xl items-center gap-1 px-6 py-3">
        <Link
          href="/"
          className="mr-3 text-sm font-semibold tracking-tight text-fg/90 transition-colors hover:text-accentBright"
        >
          astroDB
        </Link>
        {LINKS.map((l) => {
          const on = l.match(pathname);
          return (
            <Link
              key={l.href}
              href={l.href}
              className={`rounded-lg px-3 py-1.5 text-sm font-medium transition-colors ${
                on ? "text-accentBright" : "text-muted hover:text-fg"
              }`}
            >
              {l.label}
            </Link>
          );
        })}
      </nav>
    </header>
  );
}
