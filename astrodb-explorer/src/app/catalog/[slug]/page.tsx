"use client";

import { use, useEffect, useState } from "react";
import { notFound } from "next/navigation";
import type { CatalogListData } from "@/lib/types";
import { isCatalogSlug, CATALOGS } from "@/lib/catalog";
import CatalogTabs from "@/app/components/CatalogTabs";
import CatalogTable from "@/app/components/CatalogTable";

export default function CatalogBrowsePage({
  params,
}: {
  params: Promise<{ slug: string }>;
}) {
  const { slug } = use(params);
  if (!isCatalogSlug(slug)) notFound();
  const meta = CATALOGS[slug];

  const [data, setData] = useState<CatalogListData | null>(null);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    setData(null);
    setError(null);
    fetch(`/api/catalog/${slug}`)
      .then((r) => {
        if (!r.ok) throw new Error(`HTTP ${r.status}`);
        return r.json();
      })
      .then(setData)
      .catch((e) => setError(String(e)));
  }, [slug]);

  return (
    <main className="mx-auto max-w-6xl px-6 py-10">
      <header className="mb-6 animate-fade-up">
        <h1 className="bg-gradient-to-r from-accentBright via-fg to-nebula bg-clip-text text-3xl font-extrabold tracking-tight text-transparent sm:text-4xl">
          Catalogs
        </h1>
        <p className="mt-1.5 text-sm text-muted">{meta.blurb}</p>
      </header>

      <div className="mb-6">
        <CatalogTabs active={slug} />
      </div>

      {error && (
        <div className="rounded-xl border border-red-800/60 bg-red-950/40 px-4 py-3 text-sm text-red-300">
          Failed to load catalog: {error}
        </div>
      )}

      {!data && !error && (
        <div className="glass h-96 animate-pulse rounded-2xl" />
      )}

      {data && (
        <div className="animate-fade-up">
          <CatalogTable
            slug={slug}
            objects={data.objects}
            hasDifficulty={meta.hasDifficulty}
          />
        </div>
      )}
    </main>
  );
}
