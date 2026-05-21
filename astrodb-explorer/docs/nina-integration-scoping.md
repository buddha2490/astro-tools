# NINA Advanced API Integration — Scoping Document

**Status:** Scoping
**Author:** Brian Carter (via Claude)
**Intended consumer:** Claude Code (delivery agent), Brian (reviewer)
**Last updated:** 2026-05-21

---

## 1. Objective

Integrate an existing TypeScript astronomy image database / browser application with [N.I.N.A.](https://nighttime-imaging.eu/) (Nighttime Imaging 'N' Astronomy) via the [Advanced API plugin](https://github.com/christian-photo/ninaAPI), so that:

1. Every frame N.I.N.A. captures is automatically ingested into the database with full metadata (target, filter, exposure, HFR, RMS, etc.).
2. The application surfaces live session state from N.I.N.A. (current target, current frame, sequence progress, equipment status, errors).
3. Future phases support backfill, thumbnails, and bidirectional control (catalog → N.I.N.A. target push).

N.I.N.A. is the authoritative imaging system. **The application is a consumer of N.I.N.A. state, never a controller of equipment in Phase 1–3.** No ASCOM, no direct hardware. All interaction goes through the Advanced API plugin.

---

## 2. Existing System

Claude Code must inspect the application codebase before designing the integration. Key things to determine and document at the start of work:

- Runtime: Node, browser, Electron, or other? (Affects WebSocket library choice and threading.)
- Database engine and schema for images / sessions / targets / equipment.
- Existing type generation tooling (e.g., `openapi-typescript`, `zod`, manual types).
- HTTP client convention (`fetch`, `axios`, custom).
- Secrets / config management pattern (env, dotfile, secure store).
- Existing logging / observability conventions.
- Where the application runs *relative to* the N.I.N.A. host (see §7).

**Action for Claude Code:** Produce a one-page summary of these findings before writing any integration code. Confirm with Brian.

---

## 3. External Dependency: NINA Advanced API

### 3.1 Plugin & versioning

- Plugin: `Advanced API` by christian-photo, installed via NINA's Plugins tab.
- Repo: <https://github.com/christian-photo/ninaAPI>
- API doc (rendered): <https://christian-photo.github.io/github-page/projects/ninaAPI/v2/doc/api>
- API doc (mirrored, 2.2.13): <https://bump.sh/christian-photo/doc/advanced-api/>
- WebSocket doc: <https://github.com/christian-photo/ninaAPI/wiki/Websocket-V2>
- Current version at scoping time: 2.2.15.1 (May 2026). Major version `v2` is in the URL path; assume `v2` and plan for `v3` URL change as the only breaking surface.

### 3.2 Transports

Two surfaces, both on the NINA host on port 1888 by default:

| Surface     | Base URL                                    | Use                                     |
| ----------- | ------------------------------------------- | --------------------------------------- |
| REST        | `http://{nina-host}:1888/v2/api/`           | Status, history, image retrieval, push  |
| WebSocket   | `ws://{nina-host}:1888/v2/socket`           | General event stream                    |
| WebSocket   | `ws://{nina-host}:1888/v2/tppa`             | Three-Point Polar Alignment (out of scope for v1) |
| WebSocket   | `ws://{nina-host}:1888/v2/mount`            | Manual mount control (out of scope)     |
| WebSocket   | `ws://{nina-host}:1888/v2/filterwheel`      | Networked manual FW (out of scope)      |
| WebSocket   | `ws://{nina-host}:1888/v2/rotator`          | Networked manual rotator (out of scope) |

**Image transport:** The plugin supports base64 image encoding but the maintainer has stated it will be removed. **Use streaming image endpoints, not base64.** Claude Code should confirm the exact streaming endpoint path from the OpenAPI spec.

### 3.3 Authentication

- API key configured in NINA Options → Plugins → Advanced API.
- WebSocket: first message after connect must be `{"ApiKey": "..."}`. The connection is otherwise rejected/idle.
- REST: header-based key (Claude Code: confirm exact header name from the OpenAPI spec — likely `apikey` or `X-API-Key`).
- Auth must be optional in the client wrapper so unauthenticated localhost configs still work, but defaulted on.

### 3.4 Event envelope

All WebSocket messages share a single envelope shape:

```json
{
  "Response": { "Event": "<EVENT-NAME>", "...": "..." },
  "Error": "string",
  "StatusCode": 200,
  "Success": true,
  "Type": "Socket"
}
```

Some events carry only `Response.Event` (a string identifier). Others carry a structured `Response` object. The client must handle both. A TypeScript discriminated union keyed on `Response.Event` is the natural representation.

### 3.5 Key events for this integration

**Primary (must handle in Phase 1):**

- `IMAGE-SAVE` — fires after a frame is written to disk. Payload includes:
  `ExposureTime`, `Index`, `Filter`, `RmsText`, `Temperature`, `CameraName`, `Gain`, `Offset`, `Date`, `TelescopeName`, `FocalLength`, `StDev`, `Mean`, `Median`, `Stars`, `HFR`, `HFRStDev`, `IsBayered`, `Min`, `Max`, `TargetName`, `Filename`.
  This is the single most important event in this project.
- `IMAGE-PREPARED` — fires when an image is ready for retrieval via the streaming endpoint, before/around `IMAGE-SAVE`.
- `TS-NEWTARGETSTART` — Target Scheduler plugin event. Carries `TargetName`, `ProjectName`, `Coordinates` (RA/Dec, epoch), `Rotation`, `TargetEndTime`. Use for session/project tagging.
- `SEQUENCE-STARTING` / `SEQUENCE-FINISHED` — session boundaries.

**Secondary (Phase 2):**

- `AUTOFOCUS-STARTING`, `AUTOFOCUS-FINISHED`, `AUTOFOCUS-POINT-ADDED` (carries position + HFR)
- `FILTERWHEEL-CHANGED` (carries previous / new filter)
- `GUIDER-START`, `GUIDER-STOP`, `GUIDER-DITHER`
- `MOUNT-BEFORE-FLIP`, `MOUNT-AFTER-FLIP`, `MOUNT-PARKED`, `MOUNT-UNPARKED`
- `SAFETY-CHANGED` (carries `IsSafe`)
- `STACK-UPDATED`, `STACK-STATUS` (only if Livestack plugin is installed; tolerate absence)
- `ERROR-AF`, `ERROR-PLATESOLVE`, `SEQUENCE-ENTITY-FAILED`
- All `*-CONNECTED` / `*-DISCONNECTED` equipment events

**Out of scope for this project:** TS-WAITSTART (informational), ROTATOR-MOVED (control), TPPA / mount / FW / rotator PUB channels.

---

## 4. Functional Requirements

Delivered in phases. Each phase ships independently and produces user-visible value.

### 4.1 Phase 1 — Ingest (MVP)

**Goal:** every frame N.I.N.A. captures appears as a row in the application's image database within seconds, with full metadata, without manual import.

Requirements:

1. Persistent WebSocket connection to `/v2/socket` with API-key handshake.
2. Auto-reconnect with exponential backoff (cap at ~30s) on disconnect.
3. Handler for `IMAGE-SAVE` that maps the event payload to the application's existing image schema and persists.
4. Handler for `TS-NEWTARGETSTART` that maintains a "current target/project" cache; subsequent `IMAGE-SAVE` events inherit this context.
5. Handler for `SEQUENCE-STARTING` / `SEQUENCE-FINISHED` that opens/closes a session record (schema and concept of "session" TBD with Brian).
6. Idempotency: receiving the same `IMAGE-SAVE` twice (e.g., after reconnect) must not duplicate. Use `Filename` + `Date` as a natural key.
7. Configuration: NINA host, port, API key, enable/disable flag — all via existing config mechanism.
8. Structured logging of every event received, with INFO for connection lifecycle, DEBUG for individual events, ERROR for failures.

Explicitly **not** in Phase 1: image file retrieval, thumbnails, REST polling, UI work beyond a status indicator, any control endpoint.

### 4.2 Phase 2 — Live session context

**Goal:** the application UI shows what N.I.N.A. is doing right now.

Requirements:

1. In-memory session state object updated by WebSocket events: current target, current filter, last frame stats (HFR/Stars/RMS), sequence progress, equipment connection state, last error.
2. Push state changes to the UI (mechanism per existing app architecture — observable, event emitter, etc.).
3. Handle the Phase 2 event list from §3.5 (autofocus, guider, mount, safety, errors).
4. Thumbnail-on-capture: after `IMAGE-SAVE`, fetch a stretched preview from the streaming image endpoint and store as thumbnail. Time-bounded — skip if the next frame arrives first.
5. Graceful degradation when N.I.N.A. is offline (UI shows "disconnected" state, ingest queue if applicable).

### 4.3 Phase 3 — Backfill & reconciliation

**Goal:** catalog history pre-dating Phase 1 deployment and self-heal gaps from missed events.

Requirements:

1. REST-based importer that walks N.I.N.A.'s image history endpoint and inserts missing rows. Idempotent against the Phase 1 natural key.
2. On WebSocket reconnect, run a bounded reconciliation: list images saved during the disconnect window via REST and ingest any missing.
3. CLI / admin entry point to trigger a full backfill on demand.

### 4.4 Phase 4 — Bidirectional (control)

**Goal:** catalog → N.I.N.A. push.

Requirements (to be detailed in a follow-up scoping pass):

1. "Send target to N.I.N.A. framing" action from the catalog UI.
2. Load a saved sequence template.
3. Optionally: queue a target into Target Scheduler.

This phase touches control endpoints and requires more careful safety review (no slewing without confirmation, no overriding an active sequence, etc.). Do not begin until Phases 1–2 are stable.

---

## 5. Technical Approach

### 5.1 Module structure

Proposed layout (adapt to existing conventions):

```
src/nina/
  client.ts          # REST wrapper
  socket.ts          # WS client with auth, reconnect, event dispatch
  events.ts          # Typed event union + type guards
  types.ts           # Generated from OpenAPI + hand-written event types
  config.ts          # Host, port, API key, feature flags
  ingest/
    image-save.ts    # IMAGE-SAVE → DB
    sequence.ts      # session lifecycle handlers
    target.ts        # TS-NEWTARGETSTART context cache
  session/
    state.ts         # in-memory live session state (Phase 2)
  backfill/
    importer.ts      # Phase 3
  index.ts           # public API of the module
```

Single public entry point exporting:
- `createNinaIntegration(config, deps)` — returns a controller with `start()`, `stop()`, `getState()`, event subscription hooks.
- Dependencies (DB client, logger, clock) injected, not imported directly. This keeps the module testable without a live N.I.N.A.

### 5.2 WebSocket client requirements

- Library: `ws` for Node, native `WebSocket` for browser. Branch via environment or abstract.
- Auth handshake on every (re)connect: send `{"ApiKey": "..."}` immediately after `open`.
- Reconnect: exponential backoff, jitter, max 30s. Never give up; the app should self-heal on its own.
- Heartbeat: confirm whether the plugin sends ping frames or expects client pings (check the WS doc and source). If neither, implement client-side liveness via "no event in N seconds + status fetch fails = reconnect."
- Event dispatch: parse envelope, switch on `Response.Event`, route to typed handler. Unknown event names are logged at DEBUG and ignored, never thrown.
- Backpressure / queue: handlers must be fast or async-offloaded; do not block the socket reader.

### 5.3 REST client requirements

- Thin wrapper over `fetch` (or existing HTTP client) with:
  - Base URL composition from config
  - API key header injection
  - Timeout (default 10s, configurable per call)
  - Typed responses from generated types
  - Retry on 5xx and network errors with backoff; no retry on 4xx
- Image streaming endpoint should be exposed as a function returning a stream/blob, not parsed JSON.

### 5.4 Type generation

- Pull the OpenAPI spec from the plugin's published doc URL.
- Run `openapi-typescript` (or equivalent) as a build step. Check the output into the repo so Claude Code and humans can read it.
- Hand-write the WebSocket event union — the OpenAPI doesn't cover WS payloads. Source of truth: the WebSocket V2 wiki page.
- Add a runtime validator (`zod` or similar) at the WS boundary so a schema change in NINA produces a logged warning, not a silent corruption.

### 5.5 Persistence layer interface

The integration module **must not** import the application's DB layer directly. Instead, define an interface:

```ts
interface ImageStore {
  upsertImage(image: NinaImageRecord): Promise<void>;
  imageExists(naturalKey: { filename: string; date: string }): Promise<boolean>;
  // ... session, target, equipment-state methods as needed
}
```

The application provides a concrete implementation. This keeps the integration testable with a mock store and decoupled from schema decisions.

---

## 6. Non-functional requirements

- **Reliability:** the integration runs continuously, possibly for nights or weeks at a time, without leaking memory or accumulating dead connections.
- **Observability:** every connection lifecycle event, every reconnect attempt, every event class count is loggable. A `getDiagnostics()` method returns connection state, last event timestamp, event counts by type, reconnect count.
- **Testability:** all WS and REST interactions go through interfaces that can be mocked. Include a `MockNinaServer` for integration tests that replays canned event sequences.
- **Security:** API key is never logged. Connection to a non-localhost host without an API key configured produces a startup warning.
- **No global state:** the module instance is created and held by the application, not constructed at import time.

---

## 7. Open decisions (require Brian's input before/during work)

These are real choices, not nits. Claude Code should surface them explicitly and not assume.

1. **N.I.N.A. host location relative to the app.** Local LAN? Remote site over VPN/Tailscale? This affects timeout defaults, reconnect tolerance, and whether file paths from `IMAGE-SAVE.Filename` are resolvable on the app host.
2. **File ownership model.** Does the database store paths to FITS files, copies of the files, or both? If paths: how does the app host see them (network share, sync, mount)? If copies: where do they go and what's the retention model?
3. **Thumbnail strategy.** Phase 2 — fetch via streaming endpoint, or generate locally from the synced FITS, or skip entirely?
4. **Session model.** Is a "session" a NINA sequence run, a calendar night, or a user-defined unit? The data model differs.
5. **Target identity.** Does the app already have a notion of "target" / "object" that `TargetName` should resolve to, or is target text free-form?
6. **Multi-instance.** Will there ever be more than one N.I.N.A. host (e.g., remote scope + local scope)? If yes, configuration becomes a list, not a single host.
7. **Livestack plugin.** Installed? If yes, surface `STACK-*` events. If no, ignore.
8. **Auth posture.** API key required, or trust the network? Default should be "required" unless host is `localhost`.

---

## 8. Out of scope

- Any control surface (slew, focus, capture, sequence start/stop) — deferred to Phase 4.
- ASCOM / INDI / direct hardware integration.
- N.I.N.A. plugin development. We are a consumer of an existing plugin, not extending it.
- PixInsight / Siril / DSS automation. Frame-level analysis stays in the app's existing image-processing path.
- Authentication systems beyond the plugin's API key.
- Multi-user / collaborative features.

---

## 9. Acceptance criteria

### Phase 1 (MVP)

- [ ] Starting the integration with a valid config produces a connected, authenticated WebSocket within 5 seconds on a healthy network.
- [ ] Capturing a frame in N.I.N.A. produces exactly one new row in the image database within 10 seconds of `IMAGE-SAVE`.
- [ ] All 21 documented `IMAGE-SAVE` fields are persisted (or explicitly mapped to "ignore" with a comment).
- [ ] Frames captured during a Target Scheduler run carry the correct `TargetName` and `ProjectName` from the most recent `TS-NEWTARGETSTART`.
- [ ] Killing N.I.N.A. and restarting it produces a clean reconnect with no duplicate rows.
- [ ] Killing the app and restarting it produces a clean reconnect with no duplicate rows for frames captured during the outage (assuming reconciliation isn't built yet, document the gap).
- [ ] Running with no API key against a plugin that requires one produces a clear, actionable error in logs.
- [ ] Unit tests cover the event parser, the IMAGE-SAVE → record mapper, and the reconnect logic. Integration tests use the mock server.

### Phase 2

- [ ] UI shows current target, current filter, last frame HFR/Stars/RMS, sequence state, and connection state.
- [ ] All Phase 2 events from §3.5 are handled without errors.
- [ ] Thumbnails appear in the catalog within ~10 seconds of `IMAGE-SAVE`.
- [ ] Disconnecting N.I.N.A. shows a clear "disconnected" state in the UI; reconnecting restores live state without manual refresh.

---

## 10. References

- Plugin source: <https://github.com/christian-photo/ninaAPI>
- REST API docs: <https://christian-photo.github.io/github-page/projects/ninaAPI/v2/doc/api>
- WebSocket docs: <https://github.com/christian-photo/ninaAPI/wiki/Websocket-V2>
- Reference implementation (Python, similar use case): <https://github.com/quake101/CosmosCollection> — see `NINAIntegration.py` and `NINADashboard.py`
- Reference implementation (JS/Vue, control-focused): <https://github.com/Touch-N-Stars/Touch-N-Stars>
- Reference implementation (JS, minimal WS): <https://github.com/sshh12/astro-app-nina-api>
- Home Assistant integration write-up (useful endpoint examples): <https://astroaf.space/integrating-ninas-advanced-api-with-home-assistant-a-step-by-step-guide/>

---

## Appendix A — Suggested first commits for Claude Code

1. Read this document. Read the linked WebSocket V2 wiki and at least scan the OpenAPI spec.
2. Inspect the application repo and produce the §2 summary. **Stop and confirm with Brian before proceeding.**
3. Set up `src/nina/` skeleton, OpenAPI type generation, and config plumbing.
4. Implement `socket.ts` with auth + reconnect. Verify against a live N.I.N.A. or the mock server.
5. Implement `IMAGE-SAVE` handler and the `ImageStore` interface. Wire to a stubbed in-memory store first; integrate with the real DB only after the mapping is reviewed by Brian.
6. Add structured logging and `getDiagnostics()`.
7. Tests, then ship Phase 1.
