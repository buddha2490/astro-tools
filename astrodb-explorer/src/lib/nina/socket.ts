// WebSocket client for the NINA Advanced API event stream (/v2/socket).
//
// Verified against the live instance (2026-05-21): the socket connects WITHOUT a
// key on this config and immediately streams events — no subscribe message is
// required. When NINA_API_KEY is set we send {"ApiKey":"..."} on open per the
// plugin's documented handshake, which is a no-op on an unauthenticated server.
//
// Uses the global WebSocket (Node 22+ and browsers), so no `ws` dependency.

import { loadNinaConfig, socketUrl, type NinaConfig } from "./config";
import type { NinaEvent, NinaSocketMessage } from "./types";

export type EventHandler = (event: NinaEvent, raw: NinaSocketMessage) => void;
export type ConnectionState = "idle" | "connecting" | "open" | "reconnecting" | "closed";

export interface SocketDiagnostics {
  state: ConnectionState;
  connectedAt: number | null;
  lastEventAt: number | null;
  reconnectCount: number;
  eventCounts: Record<string, number>;
  totalEvents: number;
}

const MAX_BACKOFF_MS = 30_000;

export class NinaSocket {
  private readonly cfg: NinaConfig;
  private ws: WebSocket | null = null;
  private handlers = new Set<EventHandler>();
  private stopped = false;
  private reconnectTimer: ReturnType<typeof setTimeout> | null = null;

  private state: ConnectionState = "idle";
  private connectedAt: number | null = null;
  private lastEventAt: number | null = null;
  private reconnectCount = 0;
  private reconnectAttempt = 0;
  private eventCounts: Record<string, number> = {};
  private totalEvents = 0;

  constructor(cfg: NinaConfig = loadNinaConfig()) {
    this.cfg = cfg;
  }

  /** Subscribe to every event. Returns an unsubscribe function. */
  onEvent(handler: EventHandler): () => void {
    this.handlers.add(handler);
    return () => this.handlers.delete(handler);
  }

  start(): void {
    this.stopped = false;
    this.connect();
  }

  stop(): void {
    this.stopped = true;
    if (this.reconnectTimer) clearTimeout(this.reconnectTimer);
    this.reconnectTimer = null;
    this.state = "closed";
    try {
      this.ws?.close();
    } catch {
      /* ignore */
    }
    this.ws = null;
  }

  getDiagnostics(): SocketDiagnostics {
    return {
      state: this.state,
      connectedAt: this.connectedAt,
      lastEventAt: this.lastEventAt,
      reconnectCount: this.reconnectCount,
      eventCounts: { ...this.eventCounts },
      totalEvents: this.totalEvents,
    };
  }

  // ── internals ────────────────────────────────────────────────────────────
  private connect(): void {
    if (this.stopped) return;
    const url = socketUrl(this.cfg);
    this.state = this.reconnectAttempt > 0 ? "reconnecting" : "connecting";
    console.info(`[nina] socket ${this.state} → ${url}`);

    let ws: WebSocket;
    try {
      ws = new WebSocket(url);
    } catch (err) {
      console.error(`[nina] socket construct failed: ${String(err)}`);
      this.scheduleReconnect();
      return;
    }
    this.ws = ws;

    ws.addEventListener("open", () => {
      this.state = "open";
      this.connectedAt = Date.now();
      this.reconnectAttempt = 0;
      console.info("[nina] socket OPEN");
      if (this.cfg.apiKey) {
        ws.send(JSON.stringify({ ApiKey: this.cfg.apiKey }));
      }
    });

    ws.addEventListener("message", (ev: MessageEvent) => {
      this.handleMessage(typeof ev.data === "string" ? ev.data : "");
    });

    // A FAILED connection attempt (e.g. NINA still down) fires only "error" and
    // no "close" in Node's global WebSocket, while a dropped live connection
    // fires "close". Both must drive reconnect, but exactly once per attempt —
    // `settled` guards against error+close double-firing for the same socket.
    let settled = false;
    const onDown = (why: string) => {
      if (settled || this.stopped) return;
      settled = true;
      console.warn(`[nina] socket down (${why}); scheduling reconnect`);
      this.scheduleReconnect();
    };

    ws.addEventListener("error", () => onDown("error"));
    ws.addEventListener("close", (ev: CloseEvent) => onDown(`close code=${ev.code}`));
  }

  private handleMessage(data: string): void {
    if (!data) return;
    let msg: NinaSocketMessage;
    try {
      msg = JSON.parse(data) as NinaSocketMessage;
    } catch {
      console.warn(`[nina] unparseable socket frame: ${data.slice(0, 120)}`);
      return;
    }
    const event = msg.Response;
    if (!event || typeof event.Event !== "string") {
      // Not an event envelope (e.g. handshake ack) — ignore quietly.
      return;
    }
    this.lastEventAt = Date.now();
    this.totalEvents++;
    this.eventCounts[event.Event] = (this.eventCounts[event.Event] ?? 0) + 1;

    for (const h of this.handlers) {
      try {
        h(event, msg);
      } catch (err) {
        // A handler must never kill the socket reader (scoping §5.2).
        console.error(`[nina] event handler threw for ${event.Event}: ${String(err)}`);
      }
    }
  }

  private scheduleReconnect(): void {
    if (this.stopped) return;
    this.ws = null;
    this.state = "reconnecting";
    this.reconnectCount++;
    const base = Math.min(1000 * 2 ** this.reconnectAttempt, MAX_BACKOFF_MS);
    const jitter = Math.random() * 0.3 * base;
    const delay = base + jitter;
    this.reconnectAttempt++;
    if (this.reconnectTimer) clearTimeout(this.reconnectTimer);
    this.reconnectTimer = setTimeout(() => this.connect(), delay);
    console.info(`[nina] reconnect #${this.reconnectCount} in ${Math.round(delay)}ms`);
  }
}
