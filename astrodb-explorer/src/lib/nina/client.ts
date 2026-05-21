// REST client for the NINA Advanced API. Thin typed wrapper over fetch (global
// in Node 22 and the browser). Every endpoint here was probed against the live
// instance on 2026-05-21 and returns the documented shape.
//
// Failure model: NINA returns HTTP 200 even for logical failures, signalling via
// Success:false + Error. `request()` throws NinaError in that case so callers
// get a uniform try/catch surface. Network errors and 5xx are retried.

import {
  loadNinaConfig,
  restBaseUrl,
  type NinaConfig,
} from "./config";
import type {
  NinaApiResponse,
  NinaImageStatistics,
  CameraInfo,
  MountInfo,
  FilterWheelInfo,
  FocuserInfo,
  GuiderInfo,
  ConnectedInfo,
  ProfileInfo,
} from "./types";

export class NinaError extends Error {
  constructor(
    message: string,
    readonly statusCode?: number,
    readonly path?: string,
  ) {
    super(message);
    this.name = "NinaError";
  }
}

export interface ImageFetchOptions {
  /** Return binary instead of base64-in-JSON. Always true in our helpers. */
  resize?: boolean;
  /** "WxH", e.g. "400x267". Resizes (preserves aspect). */
  size?: string;
  /** JPEG quality 1-100 when resizing; omit for PNG. */
  quality?: number;
  /** Apply NINA's auto-stretch before encoding. */
  autoPrepare?: boolean;
}

export interface CaptureOptions {
  duration?: number; // seconds
  gain?: number;
  save?: boolean;
}

export class NinaClient {
  private readonly cfg: NinaConfig;
  private readonly base: string;

  constructor(cfg: NinaConfig = loadNinaConfig()) {
    this.cfg = cfg;
    this.base = restBaseUrl(cfg);
  }

  // ── core ───────────────────────────────────────────────────────────────
  private headers(): HeadersInit {
    const h: Record<string, string> = {};
    // Header name confirmed from plugin docs; harmless when no key is set and
    // the instance doesn't require one.
    if (this.cfg.apiKey) h["apikey"] = this.cfg.apiKey;
    return h;
  }

  /** Raw GET returning the parsed NINA envelope's Response, with retry on 5xx/network. */
  private async request<T>(path: string, retries = 2): Promise<T> {
    const url = `${this.base}/${path}`;
    let lastErr: unknown;
    for (let attempt = 0; attempt <= retries; attempt++) {
      try {
        const res = await this.withTimeout((signal) =>
          fetch(url, { headers: this.headers(), signal }),
        );
        if (res.status >= 500) {
          lastErr = new NinaError(`HTTP ${res.status}`, res.status, path);
          await backoff(attempt);
          continue;
        }
        const body = (await res.json()) as NinaApiResponse<T>;
        if (!body.Success) {
          // Logical failure (e.g. "No images available") — do not retry.
          throw new NinaError(body.Error || "request failed", body.StatusCode, path);
        }
        return body.Response;
      } catch (err) {
        if (err instanceof NinaError && err.statusCode && err.statusCode < 500) {
          throw err; // 4xx / logical failure — surface immediately
        }
        lastErr = err;
        await backoff(attempt);
      }
    }
    throw lastErr instanceof Error
      ? lastErr
      : new NinaError(`request to ${path} failed`, undefined, path);
  }

  /** GET returning the raw binary body (image stream). */
  private async requestBinary(path: string): Promise<{ bytes: Uint8Array; contentType: string }> {
    const url = `${this.base}/${path}`;
    const res = await this.withTimeout(
      (signal) => fetch(url, { headers: this.headers(), signal }),
      30_000, // images are large; longer ceiling
    );
    if (!res.ok) throw new NinaError(`HTTP ${res.status}`, res.status, path);
    const contentType = res.headers.get("content-type") ?? "application/octet-stream";
    if (contentType.includes("application/json")) {
      // NINA fell back to base64-in-JSON (e.g. stream not honoured) — treat as error
      const body = (await res.json()) as NinaApiResponse<unknown>;
      throw new NinaError(body.Error || "expected binary, got JSON", body.StatusCode, path);
    }
    const buf = new Uint8Array(await res.arrayBuffer());
    return { bytes: buf, contentType };
  }

  private withTimeout<T>(
    fn: (signal: AbortSignal) => Promise<T>,
    ms = this.cfg.timeoutMs,
  ): Promise<T> {
    const ctrl = new AbortController();
    const t = setTimeout(() => ctrl.abort(), ms);
    return fn(ctrl.signal).finally(() => clearTimeout(t));
  }

  // ── status / version ─────────────────────────────────────────────────────
  /** Plugin/API version string, e.g. "2.2.15.1". */
  getVersion(): Promise<string> {
    return this.request<string>("version");
  }

  // ── equipment ──────────────────────────────────────────────────────────
  getCameraInfo(): Promise<CameraInfo> {
    return this.request<CameraInfo>("equipment/camera/info");
  }
  getMountInfo(): Promise<MountInfo> {
    return this.request<MountInfo>("equipment/mount/info");
  }
  getFilterWheelInfo(): Promise<FilterWheelInfo> {
    return this.request<FilterWheelInfo>("equipment/filterwheel/info");
  }
  getFocuserInfo(): Promise<FocuserInfo> {
    return this.request<FocuserInfo>("equipment/focuser/info");
  }
  getGuiderInfo(): Promise<GuiderInfo> {
    return this.request<GuiderInfo>("equipment/guider/info");
  }
  getRotatorInfo(): Promise<ConnectedInfo> {
    return this.request<ConnectedInfo>("equipment/rotator/info");
  }
  getWeatherInfo(): Promise<ConnectedInfo> {
    return this.request<ConnectedInfo>("equipment/weather/info");
  }
  getDomeInfo(): Promise<ConnectedInfo> {
    return this.request<ConnectedInfo>("equipment/dome/info");
  }
  getSafetyMonitorInfo(): Promise<ConnectedInfo & { IsSafe: boolean }> {
    return this.request<ConnectedInfo & { IsSafe: boolean }>("equipment/safetymonitor/info");
  }
  getFlatDeviceInfo(): Promise<ConnectedInfo> {
    return this.request<ConnectedInfo>("equipment/flatdevice/info");
  }
  getSwitchInfo(): Promise<ConnectedInfo> {
    return this.request<ConnectedInfo>("equipment/switch/info");
  }

  // ── sequence / profile ───────────────────────────────────────────────────
  /** Full sequence tree (compact). */
  getSequenceJson(): Promise<unknown> {
    return this.request<unknown>("sequence/json");
  }
  /** Detailed running state of the sequence. */
  getSequenceState(): Promise<unknown> {
    return this.request<unknown>("sequence/state");
  }
  /** Saved sequence template names. */
  listAvailableSequences(): Promise<string[]> {
    return this.request<string[]>("sequence/list-available");
  }
  getProfiles(): Promise<ProfileInfo[]> {
    return this.request<ProfileInfo[]>("profile/show");
  }

  // ── image history ──────────────────────────────────────────────────────
  /** All frames captured since NINA started (or since the active sequence). */
  getImageHistory(): Promise<NinaImageStatistics[]> {
    return this.request<NinaImageStatistics[]>("image-history?all=true");
  }
  /** Number of frames in history. */
  getImageHistoryCount(): Promise<number> {
    return this.request<number>("image-history?count=true");
  }

  // ── image retrieval ──────────────────────────────────────────────────────
  // Always uses stream=true to get raw bytes — the base64-in-JSON path is being
  // removed by the plugin maintainer (scoping §3.2). Index is the position in
  // image-history; the latest frame is index (count - 1).
  async getImage(index: number, opts: ImageFetchOptions = {}): Promise<{ bytes: Uint8Array; contentType: string }> {
    const qs = new URLSearchParams({ stream: "true" });
    if (opts.resize) qs.set("resize", "true");
    if (opts.size) qs.set("size", opts.size);
    if (opts.quality != null) qs.set("quality", String(opts.quality));
    if (opts.autoPrepare) qs.set("autoPrepare", "true");
    return this.requestBinary(`image/${index}?${qs.toString()}`);
  }

  /** Convenience: small stretched JPEG thumbnail for the latest frame. */
  async getThumbnail(
    index: number,
    size = "400x267",
    quality = 80,
  ): Promise<{ bytes: Uint8Array; contentType: string }> {
    return this.getImage(index, { resize: true, size, quality, autoPrepare: true });
  }

  // ── control (test-only for now) ────────────────────────────────────────
  /** Trigger a snapshot. Used for verification; not part of Phase 1 ingest. */
  async captureSnapshot(opts: CaptureOptions = {}): Promise<string> {
    const qs = new URLSearchParams({ getResult: "false" });
    if (opts.duration != null) qs.set("duration", String(opts.duration));
    if (opts.gain != null) qs.set("gain", String(opts.gain));
    if (opts.save != null) qs.set("save", String(opts.save));
    return this.request<string>(`equipment/camera/capture?${qs.toString()}`);
  }
}

async function backoff(attempt: number): Promise<void> {
  const ms = Math.min(1000 * 2 ** attempt, 8000);
  await new Promise((r) => setTimeout(r, ms));
}
