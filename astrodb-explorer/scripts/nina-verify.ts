// Live verification harness for the NINA integration client. Exercises every
// REST call and the WebSocket event stream against a real NINA instance, then
// triggers a saved capture and asserts the IMAGE-SAVE event parses through the
// actual socket code (not a mock).
//
//   NINA_HOST=100.65.96.18 npx tsx scripts/nina-verify.ts
//
// Exits non-zero if any check fails. Safe to run while a sequence is armed: it
// only fires short snapshots when the camera is idle.

import { NinaClient } from "../src/lib/nina/client";
import { NinaSocket } from "../src/lib/nina/socket";
import { loadNinaConfig } from "../src/lib/nina/config";
import { isImageSaveEvent, type NinaEvent } from "../src/lib/nina/types";

let pass = 0;
let fail = 0;

function ok(label: string, detail = "") {
  pass++;
  console.log(`  ✅ ${label}${detail ? ` — ${detail}` : ""}`);
}
function bad(label: string, err: unknown) {
  fail++;
  console.log(`  ❌ ${label} — ${err instanceof Error ? err.message : String(err)}`);
}
async function check<T>(label: string, fn: () => Promise<T>, fmt?: (v: T) => string): Promise<T | undefined> {
  try {
    const v = await fn();
    ok(label, fmt ? fmt(v) : "");
    return v;
  } catch (err) {
    bad(label, err);
    return undefined;
  }
}

async function main() {
  const cfg = loadNinaConfig();
  console.log(`\nNINA verify → ${cfg.host}:${cfg.port} (apiKey: ${cfg.apiKey ? "set" : "none"})\n`);
  const client = new NinaClient(cfg);

  console.log("REST — status & equipment");
  await check("version", () => client.getVersion(), (v) => v);
  const cam = await check("camera/info", () => client.getCameraInfo(), (c) => `${c.CameraName ?? "?"} state=${c.CameraState} temp=${c.Temperature}`);
  await check("mount/info", () => client.getMountInfo(), (m) => `track=${m.TrackingEnabled} home=${m.AtHome}`);
  await check("filterwheel/info", () => client.getFilterWheelInfo(), (f) => `sel=${f.SelectedFilter?.Name} of ${f.AvailableFilters.length}`);
  await check("focuser/info", () => client.getFocuserInfo(), (f) => `connected=${f.Connected}`);
  await check("guider/info", () => client.getGuiderInfo(), (g) => `connected=${g.Connected}`);
  await check("rotator/info", () => client.getRotatorInfo());
  await check("weather/info", () => client.getWeatherInfo());
  await check("dome/info", () => client.getDomeInfo());
  await check("safetymonitor/info", () => client.getSafetyMonitorInfo(), (s) => `isSafe=${s.IsSafe}`);
  await check("flatdevice/info", () => client.getFlatDeviceInfo());
  await check("switch/info", () => client.getSwitchInfo());

  console.log("\nREST — sequence & profile");
  await check("sequence/json", () => client.getSequenceJson(), () => "ok");
  await check("sequence/state", () => client.getSequenceState(), () => "ok");
  await check("sequence/list-available", () => client.listAvailableSequences(), (l) => `${l.length} templates`);
  await check("profile/show", () => client.getProfiles(), (p) => `${p.length}, active=${p.find((x) => x.IsActive)?.Name}`);

  console.log("\nREST — image history & retrieval");
  const count0 = await check("image-history?count", () => client.getImageHistoryCount(), (n) => `${n} frames`);
  await check("image-history?all", () => client.getImageHistory(), (h) => `${h.length} entries`);

  // ── WebSocket: connect, then provoke an IMAGE-SAVE and assert it parses ──
  console.log("\nWebSocket — event stream + live IMAGE-SAVE");
  const socket = new NinaSocket(cfg);
  const imageSavePromise = new Promise<NinaEvent>((resolve, reject) => {
    const timer = setTimeout(() => reject(new Error("no IMAGE-SAVE within 60s")), 60_000);
    socket.onEvent((event) => {
      if (isImageSaveEvent(event)) {
        clearTimeout(timer);
        resolve(event);
      }
    });
  });
  socket.start();
  await waitForState(socket, "open", 8_000).then(
    () => ok("socket open"),
    (e) => bad("socket open", e),
  );

  // Only fire a test capture if the camera is idle (don't disturb a real sequence).
  if (cam && cam.CameraState === "Idle" && !cam.IsExposing) {
    await check("trigger 6s saved capture", () => client.captureSnapshot({ duration: 6, save: true }), (r) => r);
    try {
      const ev = await imageSavePromise;
      if (isImageSaveEvent(ev)) {
        const s = ev.ImageStatistics;
        ok("IMAGE-SAVE parsed", `${s.Filename} filter=${s.Filter} HFR=${s.HFR.toFixed(2)} stars=${s.Stars}`);
        // retrieve the just-saved frame as a thumbnail
        const newCount = await client.getImageHistoryCount();
        const idx = newCount - 1;
        await check(`thumbnail image/${idx}`, () => client.getThumbnail(idx), (img) => `${img.contentType} ${img.bytes.byteLength}B`);
        await check(`full stream image/${idx}`, () => client.getImage(idx), (img) => `${img.contentType} ${(img.bytes.byteLength / 1e6).toFixed(1)}MB`);
      }
    } catch (err) {
      bad("IMAGE-SAVE parsed", err);
    }
  } else {
    console.log(`  ⏭️  camera not idle (state=${cam?.CameraState}); skipping live capture/IMAGE-SAVE`);
  }

  const diag = socket.getDiagnostics();
  console.log(`\nSocket diagnostics: ${JSON.stringify(diag.eventCounts)} (total ${diag.totalEvents})`);
  socket.stop();
  void count0;

  console.log(`\n${fail === 0 ? "ALL GREEN" : "FAILURES"} — ${pass} passed, ${fail} failed\n`);
  process.exit(fail === 0 ? 0 : 1);
}

function waitForState(socket: NinaSocket, want: string, timeoutMs: number): Promise<void> {
  return new Promise((resolve, reject) => {
    const start = Date.now();
    const iv = setInterval(() => {
      if (socket.getDiagnostics().state === want) {
        clearInterval(iv);
        resolve();
      } else if (Date.now() - start > timeoutMs) {
        clearInterval(iv);
        reject(new Error(`state never reached "${want}"`));
      }
    }, 200);
  });
}

main().catch((err) => {
  console.error("verify harness crashed:", err);
  process.exit(1);
});
