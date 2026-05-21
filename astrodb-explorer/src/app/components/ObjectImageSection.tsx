"use client";

import { useEffect, useRef, useState } from "react";

// Upload + display your own processed image for an object. The image also
// becomes the object's dashboard tile thumbnail. No size limit (these are big).
export default function ObjectImageSection({ object }: { object: string }) {
  const enc = encodeURIComponent(object);
  const inputRef = useRef<HTMLInputElement>(null);
  const [path, setPath] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);
  const [uploading, setUploading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    fetch(`/api/object/${enc}/image`)
      .then((r) => r.json())
      .then((d) => setPath(d.path ?? null))
      .catch(() => setPath(null))
      .finally(() => setLoading(false));
  }, [enc]);

  async function handleFile(e: React.ChangeEvent<HTMLInputElement>) {
    const file = e.target.files?.[0];
    if (!file) return;
    setUploading(true);
    setError(null);
    try {
      const body = new FormData();
      body.append("file", file);
      const r = await fetch(`/api/object/${enc}/image`, { method: "POST", body });
      const d = await r.json();
      if (!r.ok) throw new Error(d.error ?? `HTTP ${r.status}`);
      // Cache-bust so the new image replaces the old one in the browser.
      setPath(`${d.path}?t=${Date.now()}`);
    } catch (err) {
      setError(String(err));
    } finally {
      setUploading(false);
      if (inputRef.current) inputRef.current.value = "";
    }
  }

  return (
    <section className="glass rounded-2xl p-5 shadow-card">
      <div className="mb-3 flex items-center justify-between">
        <h2 className="text-sm font-semibold uppercase tracking-wide text-muted">
          My Image
        </h2>
        <div className="flex items-center gap-2">
          {path && (
            <a
              href={path}
              download
              className="rounded-lg bg-accent/15 px-3 py-1.5 text-sm font-medium text-accentBright ring-1 ring-accent/30 transition-colors hover:bg-accent/25"
            >
              Download ↓
            </a>
          )}
          <button
            onClick={() => inputRef.current?.click()}
            disabled={uploading}
            className="rounded-lg bg-white/5 px-3 py-1.5 text-sm font-medium text-muted ring-1 ring-white/10 transition-colors hover:bg-white/10 hover:text-fg disabled:opacity-40"
          >
            {uploading ? "Uploading…" : path ? "Replace image" : "Upload image"}
          </button>
          <input
            ref={inputRef}
            type="file"
            accept="image/*"
            className="hidden"
            onChange={handleFile}
          />
        </div>
      </div>

      {loading && <div className="h-64 animate-pulse rounded-xl bg-white/5" />}

      {!loading && path && (
        // eslint-disable-next-line @next/next/no-img-element
        <img
          src={path}
          alt={`Image of ${object}`}
          className="w-full rounded-xl"
        />
      )}

      {!loading && !path && !uploading && (
        <button
          onClick={() => inputRef.current?.click()}
          className="flex h-40 w-full items-center justify-center rounded-xl bg-white/5 text-sm text-faint transition-colors hover:bg-white/10 hover:text-muted"
        >
          No image yet — click to upload your shot of {object}.
        </button>
      )}

      {uploading && (
        <div className="flex h-40 items-center justify-center rounded-xl bg-white/5 text-sm text-muted">
          Uploading…
        </div>
      )}

      {error && <p className="mt-2 text-xs text-red-400">{error}</p>}
    </section>
  );
}
