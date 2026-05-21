// Wires the WebSocket event stream to the IMAGE-SAVE → astroSubs ingest path.
// Construct with injected deps (client, socket, store) so it's testable without
// live infrastructure. This is the public entry for Phase 1 ingest.

import { NinaClient } from "../client";
import { NinaSocket } from "../socket";
import { loadNinaConfig, type NinaConfig } from "../config";
import { isImageSaveEvent } from "../types";
import {
  mapImageSaveToRecord,
  isIngestableFrame,
  EMPTY_EQUIPMENT,
  type EquipmentSnapshot,
} from "./mapper";
import type { ImageStore } from "./store";

export interface IngestDiagnostics {
  ingested: number;
  skippedNonLight: number;
  deduped: number;
  errors: number;
  lastFilename: string | null;
  lastIngestAt: number | null;
}

export interface IngestDeps {
  client: NinaClient;
  socket: NinaSocket;
  store: ImageStore;
}

export class NinaIngest {
  private readonly client: NinaClient;
  private readonly socket: NinaSocket;
  private readonly store: ImageStore;
  private unsubscribe: (() => void) | null = null;

  private diag: IngestDiagnostics = {
    ingested: 0,
    skippedNonLight: 0,
    deduped: 0,
    errors: 0,
    lastFilename: null,
    lastIngestAt: null,
  };

  constructor(deps: IngestDeps) {
    this.client = deps.client;
    this.socket = deps.socket;
    this.store = deps.store;
  }

  start(): void {
    this.unsubscribe = this.socket.onEvent((event) => {
      if (!isImageSaveEvent(event)) return;
      // Handlers must not block the socket reader (§5.2) — run async, swallow.
      void this.handleImageSave(event.ImageStatistics);
    });
    this.socket.start();
    console.info("[nina-ingest] started");
  }

  stop(): void {
    this.unsubscribe?.();
    this.unsubscribe = null;
    this.socket.stop();
    console.info("[nina-ingest] stopped");
  }

  getDiagnostics(): { ingest: IngestDiagnostics; socket: ReturnType<NinaSocket["getDiagnostics"]> } {
    return { ingest: { ...this.diag }, socket: this.socket.getDiagnostics() };
  }

  private async handleImageSave(stats: Parameters<typeof mapImageSaveToRecord>[0]): Promise<void> {
    try {
      if (!isIngestableFrame(stats)) {
        this.diag.skippedNonLight++;
        console.debug(`[nina-ingest] skip ${stats.ImageType} ${stats.Filename}`);
        return;
      }
      const equipment = await this.fetchEquipment();
      const record = mapImageSaveToRecord(stats, equipment);
      const { inserted } = await this.store.insertCapture(record);
      if (inserted) {
        this.diag.ingested++;
        this.diag.lastFilename = record.Filename;
        this.diag.lastIngestAt = Date.now();
        console.info(
          `[nina-ingest] + ${record.Object} ${record.FilterName} ${record.Duration}s ` +
            `HFR=${record.HFR} stars=${record.DetectedStars} (${record.Filename})`,
        );
      } else {
        this.diag.deduped++;
        console.info(`[nina-ingest] = dedup ${record.Filename}`);
      }
    } catch (err) {
      this.diag.errors++;
      console.error(`[nina-ingest] failed for ${stats.Filename}: ${String(err)}`);
    }
  }

  // Best-effort focuser/rotator readings at save time. Disconnected devices map
  // to null rather than their idle 0 values.
  private async fetchEquipment(): Promise<EquipmentSnapshot> {
    const eq: EquipmentSnapshot = { ...EMPTY_EQUIPMENT };
    try {
      const f = await this.client.getFocuserInfo();
      if (f.Connected) {
        eq.focuserPosition = numOrNull(f.Position);
        eq.focuserTemp = numOrNull(f.Temperature);
      }
    } catch {
      /* leave null */
    }
    try {
      const r = await this.client.getRotatorInfo();
      if (r.Connected) eq.rotatorPosition = numOrNull(r.Position);
    } catch {
      /* leave null */
    }
    return eq;
  }
}

function numOrNull(v: unknown): number | null {
  const n = typeof v === "number" ? v : Number(v);
  return Number.isFinite(n) ? n : null;
}

/** Convenience factory used by the worker entry point. */
export function createNinaIngest(store: ImageStore, cfg: NinaConfig = loadNinaConfig()): NinaIngest {
  return new NinaIngest({
    client: new NinaClient(cfg),
    socket: new NinaSocket(cfg),
    store,
  });
}
