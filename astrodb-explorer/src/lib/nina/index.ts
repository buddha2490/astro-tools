// Public surface of the NINA integration module. Phase 1 = read-only API access
// (REST client + event socket). Ingest/DB wiring and UI come in later phases.

export { loadNinaConfig, restBaseUrl, socketUrl, type NinaConfig } from "./config";
export { NinaClient, NinaError, type ImageFetchOptions, type CaptureOptions } from "./client";
export {
  NinaSocket,
  type EventHandler,
  type ConnectionState,
  type SocketDiagnostics,
} from "./socket";
export * from "./types";

// Phase 1 ingest
export {
  mapImageSaveToRecord,
  isIngestableFrame,
  stemOf,
  parseRmsArcsec,
  type AstroSubRecord,
  type EquipmentSnapshot,
} from "./ingest/mapper";
export { type ImageStore, PostgresImageStore } from "./ingest/store";
export {
  NinaIngest,
  createNinaIngest,
  type IngestDiagnostics,
  type IngestDeps,
} from "./ingest/controller";
