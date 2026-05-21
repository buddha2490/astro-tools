// Types for the NINA Advanced API, hand-written and corrected against the live
// wire format (NINA 2.2.15.1, 2026-05-21). The OpenAPI spec does not cover the
// WebSocket payloads, and the scoping doc's field list for IMAGE-SAVE was flat —
// in reality the stats are nested under Response.ImageStatistics. See below.

// ── REST envelope ──────────────────────────────────────────────────────────
// Every REST call returns this shape. On failure the API still returns HTTP 200
// with Success:false and a non-empty Error (e.g. "No images available"), so
// callers must check Success, not just the HTTP status.
export interface NinaApiResponse<T> {
  Response: T;
  Error: string;
  StatusCode: number;
  Success: boolean;
  Type: string; // "API"
}

// ── Image statistics ────────────────────────────────────────────────────────
// This exact shape appears in TWO places: each entry of GET /image-history, and
// the IMAGE-SAVE WebSocket event (under Response.ImageStatistics). Treating them
// as one type is intentional — it's the natural ingest record. `Filename` is a
// BARE BASENAME (no directory), e.g. "Snapshot_2026-05-21_SNAPSHOT_O_8.00_0000.fits".
// Natural key for idempotency: Filename + Date (scoping §4.1.6).
export interface NinaImageStatistics {
  ExposureTime: number;
  ImageType: string; // "LIGHT" | "SNAPSHOT" | "FLAT" | "DARK" | "BIAS" | ...
  Filter: string;
  RmsText: string;
  Temperature: number;
  CameraName: string;
  TargetName: string;
  Gain: number;
  Offset: number;
  Date: string; // ISO 8601 with offset, e.g. "2026-05-21T13:07:45.13-05:00"
  TelescopeName: string;
  FocalLength: number;
  StDev: number;
  Mean: number;
  Median: number;
  Min: number;
  Max: number;
  Stars: number;
  HFR: number;
  HFRStDev: number;
  IsBayered: boolean;
  Filename: string;
}

// ── Equipment info (subset of fields we actually use; NINA returns more) ─────
export interface CameraInfo {
  Connected?: boolean;
  CameraName?: string;
  Temperature: number;
  TargetTemp: number;
  AtTargetTemp: boolean;
  CoolerOn: boolean;
  CoolerPower: number;
  Gain: number;
  Offset: number;
  CameraState: string; // "Idle" | "Exposing" | ...
  IsExposing: boolean;
  XSize: number;
  YSize: number;
  PixelSize: number;
  SensorType: string; // "Monochrome" | "RGGB" | ...
  [k: string]: unknown;
}

export interface MountInfo {
  Connected?: boolean;
  TrackingEnabled: boolean;
  TrackingMode: string;
  RightAscensionString: string;
  DeclinationString: string;
  Altitude: number;
  Azimuth: number;
  AtPark: boolean;
  AtHome: boolean;
  Slewing: boolean;
  TimeToMeridianFlip: number;
  SideOfPier: string;
  [k: string]: unknown;
}

export interface FilterInfo {
  Name: string;
  Id: number;
}

export interface FilterWheelInfo {
  Connected: boolean;
  Name: string;
  IsMoving: boolean;
  SelectedFilter: FilterInfo | null;
  AvailableFilters: FilterInfo[];
  [k: string]: unknown;
}

export interface FocuserInfo {
  Connected: boolean;
  Position: number;
  Temperature: number | "NaN";
  IsMoving: boolean;
  [k: string]: unknown;
}

export interface GuiderInfo {
  Connected: boolean;
  PixelScale: number;
  [k: string]: unknown;
}

// Generic equipment-connection probe — every device info payload has Connected
// (camera/mount omit it but report state otherwise).
export interface ConnectedInfo {
  Connected: boolean;
  [k: string]: unknown;
}

export interface ProfileInfo {
  Id: string;
  Name: string;
  Description: string;
  Location: string;
  IsActive: boolean;
  LastUsed: string;
}

// ── WebSocket events ─────────────────────────────────────────────────────────
// All socket frames share this envelope. Type is always "Socket".
export interface NinaSocketMessage<R = NinaEvent> {
  Response: R;
  Error: string;
  StatusCode: number;
  Success: boolean;
  Type: string; // "Socket"
}

// Most events carry only { Event } (a bare identifier). IMAGE-SAVE additionally
// carries ImageStatistics. We model the union on the `Event` discriminant and
// keep an open fallback so unknown events parse instead of throwing (§5.2).
export interface BaseEvent {
  Event: string;
}

export interface ImageSaveEvent extends BaseEvent {
  Event: "IMAGE-SAVE";
  ImageStatistics: NinaImageStatistics;
}

// Target Scheduler new-target start. Fields per scoping §3.5 — not yet observed
// live (sequence was in "Wait for Time"); shape is best-effort and tolerant.
export interface TsNewTargetStartEvent extends BaseEvent {
  Event: "TS-NEWTARGETSTART";
  TargetName?: string;
  ProjectName?: string;
  Coordinates?: unknown;
  Rotation?: number;
  TargetEndTime?: string;
}

export type NinaEvent = ImageSaveEvent | TsNewTargetStartEvent | BaseEvent;

// ── Type guards ──────────────────────────────────────────────────────────────
export function isImageSaveEvent(e: NinaEvent): e is ImageSaveEvent {
  return (
    e.Event === "IMAGE-SAVE" &&
    typeof (e as ImageSaveEvent).ImageStatistics === "object" &&
    (e as ImageSaveEvent).ImageStatistics !== null
  );
}

export function isTsNewTargetStartEvent(e: NinaEvent): e is TsNewTargetStartEvent {
  return e.Event === "TS-NEWTARGETSTART";
}
