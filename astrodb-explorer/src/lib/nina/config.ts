// NINA Advanced API connection config. All values come from the environment so
// the same module works in dev, in a worker, or in a Next.js route. Mirrors the
// env-var convention used by db.ts.
//
// Verified against NINA 2.2.15.1 @ 100.65.96.18:1888 on 2026-05-21:
//   - REST and WebSocket both require NO API key on this instance (auth is
//     disabled in the plugin). The key is sent only if NINA_API_KEY is set.

export interface NinaConfig {
  host: string;
  port: number;
  apiKey: string | null;
  enabled: boolean;
  /** Per-request REST timeout in ms. */
  timeoutMs: number;
}

export function loadNinaConfig(): NinaConfig {
  const host = process.env.NINA_HOST ?? "100.65.96.18";
  const port = Number(process.env.NINA_PORT ?? 1888);
  const apiKey = process.env.NINA_API_KEY?.trim() || null;
  const enabled = (process.env.NINA_ENABLED ?? "true").toLowerCase() !== "false";
  const timeoutMs = Number(process.env.NINA_TIMEOUT_MS ?? 10_000);

  // Security posture (scoping §3.3 / §6): warn — don't fail — when pointing at a
  // non-localhost host with no key. The plugin itself may have auth disabled.
  const isLocal = host === "localhost" || host === "127.0.0.1";
  if (!isLocal && !apiKey) {
    console.warn(
      `[nina] connecting to non-local host ${host} without an API key; ` +
        `set NINA_API_KEY if the Advanced API plugin requires authentication`,
    );
  }

  return { host, port, apiKey, enabled, timeoutMs };
}

export function restBaseUrl(cfg: NinaConfig): string {
  return `http://${cfg.host}:${cfg.port}/v2/api`;
}

export function socketUrl(cfg: NinaConfig): string {
  return `ws://${cfg.host}:${cfg.port}/v2/socket`;
}
