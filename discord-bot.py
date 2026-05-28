#!/usr/bin/env python3
"""
Roof-status monitor for Starfront Observatories (SFRO).

Watches the SFRO Discord roof-announcement channel and maintains a local status
file the imaging PC can poll to decide whether it's safe to keep imaging.

Two facts about the source drive the design:
  1. SFRO posts status as Discord *embeds* ("✅ Roofs Opening" / "❌ Roofs
     Closing"), so we scan embed title/description/footer, not just
     message.content. Reading another bot's content/embeds requires the
     privileged "Message Content Intent" (enable it in the Developer Portal).
  2. The roof opens at dusk and closes at dawn — ~two messages a night. The
     state can sit unchanged for hours, so "no message recently" must NOT be
     read as unsafe.

Fail-safe therefore keys on MONITOR HEALTH, not message recency: a heartbeat
timestamp is refreshed every ROOF_HEARTBEAT_SECONDS *only while connected to
Discord*. If this process dies or loses its connection, the heartbeat goes
stale — and the PC-side consumer must treat a stale heartbeat (or state=UNKNOWN)
as UNSAFE and shut down.

Status file format (key=value, one per line, atomically replaced):
    state=SAFE|UNSAFE|UNKNOWN
    state_since=<ISO8601 of last state change>
    last_heartbeat=<ISO8601, refreshed while connected>
    connected=true|false
    source=<short text of the message that set the state>
"""

import os
import sys
import datetime
import logging
from pathlib import Path

BASE_DIR = Path(__file__).resolve().parent


def load_env(filename=".env"):
    """Minimal .env loader (KEY=VALUE lines) — avoids a python-dotenv dependency."""
    path = BASE_DIR / filename
    if not path.exists():
        return
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#") or "=" not in line:
            continue
        key, value = line.split("=", 1)
        os.environ.setdefault(key.strip(), value.strip())


load_env()

TOKEN = os.environ.get("DISCORD_BOT_TOKEN", "").strip()
CHANNEL_ID = int(os.environ.get("ROOF_CHANNEL_ID", "0") or "0")
STATUS_PATH = (BASE_DIR / os.environ.get("ROOF_STATUS_PATH", "roofstatus.txt")).resolve() \
    if not os.path.isabs(os.environ.get("ROOF_STATUS_PATH", "roofstatus.txt")) \
    else Path(os.environ["ROOF_STATUS_PATH"])
# Single-line file the NINA ASCOM Generic File SafetyMonitor polls. Lives next to
# the status file unless ROOF_SAFE_PATH overrides it. Format mirrors Starfront's
# convention exactly ("Roof Status: OPEN/CLOSED") so NINA config is copy-paste.
SAFE_PATH = Path(os.environ["ROOF_SAFE_PATH"]) if os.environ.get("ROOF_SAFE_PATH") \
    else STATUS_PATH.with_name("roofsafe.txt")
HEARTBEAT_SECONDS = int(os.environ.get("ROOF_HEARTBEAT_SECONDS", "60"))
LOG_PATH = os.environ.get("ROOF_LOG_PATH", "discord-bot.log")

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
    handlers=[logging.StreamHandler(sys.stdout), logging.FileHandler(BASE_DIR / LOG_PATH)],
)
log = logging.getLogger("roofmon")


# --- classification ---------------------------------------------------------

def message_text(msg):
    """All human-readable text in a message, including embeds."""
    parts = [msg.content or ""]
    for embed in msg.embeds:
        parts.append(embed.title or "")
        parts.append(embed.description or "")
        if embed.footer and embed.footer.text:
            parts.append(embed.footer.text)
        for field in embed.fields:
            parts.append(field.name or "")
            parts.append(field.value or "")
    return " ".join(p for p in parts if p).strip()


def classify(text):
    """Map message text to SAFE / UNSAFE / None (unrecognized → no state change).

    Anchored on SFRO's exact wording ("Roofs Opening/Closing", "All Roofs are
    OPENING/CLOSING", ✅/❌) rather than bare "open"/"close" substrings, so a
    hypothetical negation ("roofs will NOT be opening") doesn't flip us to SAFE.
    Close is checked first — a "closing" message is the one you can't miss.
    """
    low = text.lower()
    if "❌" in text or "roofs closing" in low or "are closing" in low \
            or "are closed" in low or "unsafe" in low or "weather hold" in low:
        return "UNSAFE"
    if "✅" in text or "roofs opening" in low or "are opening" in low \
            or "are open" in low:
        return "SAFE"
    return None


# --- state + status file ----------------------------------------------------

status = {"state": "UNKNOWN", "state_since": None, "source": ""}


def now_iso():
    return datetime.datetime.now(datetime.timezone.utc).astimezone().isoformat(timespec="seconds")


def _atomic_write(path, text):
    """Write text to path via a temp file + rename so a reader never sees a
    half-written file. Resilient: the target lives on a network mount, so if the
    volume is unavailable we log and return rather than raise — the process stays
    alive (launchd KeepAlive) and the file simply goes stale. We deliberately do
    NOT mkdir the path: creating folders under an unmounted /Volumes share leaves
    phantom local directories that prevent the real share from remounting.
    """
    tmp = path.with_name(path.name + ".tmp")
    try:
        tmp.write_text(text)
        os.replace(tmp, path)
        return True
    except OSError as exc:
        log.warning("Could not write %s: %s (is the network volume mounted?)", path, exc)
        return False


def write_status(connected):
    """Write both output files: the rich human/debug status file, and the
    single-line safety file the NINA ASCOM Generic File SafetyMonitor reads.

    The safety file maps SAFE -> OPEN and everything else (UNSAFE *and* the
    initial UNKNOWN) -> CLOSED, so an unseeded or unrecognized state fails safe.
    """
    lines = [
        f"state={status['state']}",
        f"state_since={status['state_since'] or ''}",
        f"last_heartbeat={now_iso()}",
        f"connected={'true' if connected else 'false'}",
        f"source={status['source']}",
    ]
    _atomic_write(STATUS_PATH, "\n".join(lines) + "\n")

    roof = "OPEN" if status["state"] == "SAFE" else "CLOSED"
    _atomic_write(SAFE_PATH, f"Roof Status: {roof}\n")


def apply_message(msg, *, reason="message"):
    """Reclassify a message in the watched channel and update state if it changed."""
    if msg.channel.id != CHANNEL_ID:
        return
    new_state = classify(message_text(msg))
    if new_state is None:
        return
    short = " ".join(message_text(msg).split())[:120]
    if new_state != status["state"]:
        status["state"] = new_state
        status["state_since"] = now_iso()
        status["source"] = short
        log.info("STATE -> %s (%s): %s", new_state, reason, short)
    write_status(connected=True)


# --- discord client ---------------------------------------------------------

import discord
from discord.ext import tasks

intents = discord.Intents.default()
intents.message_content = True  # privileged: required to read the roof bot's embeds
client = discord.Client(intents=intents)


@tasks.loop(seconds=HEARTBEAT_SECONDS)
async def heartbeat():
    # Only refresh the heartbeat while truly connected. When disconnected we skip
    # the write so the timestamp ages out and the consumer fails safe.
    if client.is_ready():
        write_status(connected=True)


@client.event
async def on_ready():
    log.info("Connected as %s. Watching channel %s.", client.user, CHANNEL_ID)
    channel = client.get_channel(CHANNEL_ID)
    if channel is None:
        try:
            channel = await client.fetch_channel(CHANNEL_ID)
        except Exception as exc:
            log.error("Cannot access channel %s: %s. Is the bot in that server?", CHANNEL_ID, exc)
            return

    # Seed state from history so we know the roof's status even if we started
    # mid-night (history is newest-first; first classifiable message wins).
    if status["state"] == "UNKNOWN":
        try:
            async for msg in channel.history(limit=50):
                state = classify(message_text(msg))
                if state:
                    status["state"] = state
                    status["state_since"] = msg.created_at.astimezone().isoformat(timespec="seconds")
                    status["source"] = " ".join(message_text(msg).split())[:120]
                    log.info("Seeded state from history -> %s: %s", state, status["source"])
                    break
        except discord.Forbidden:
            log.warning("No 'Read Message History' permission — cannot seed initial state.")

    write_status(connected=True)
    if not heartbeat.is_running():
        heartbeat.start()


@client.event
async def on_message(msg):
    apply_message(msg, reason="new message")


@client.event
async def on_message_edit(_before, after):
    apply_message(after, reason="edit")


@client.event
async def on_disconnect():
    log.warning("Disconnected from Discord — heartbeat will go stale (consumer should fail safe).")


@client.event
async def on_resumed():
    log.info("Reconnected to Discord.")


def main():
    if not TOKEN or TOKEN == "PASTE_NEW_TOKEN_HERE":
        sys.exit("DISCORD_BOT_TOKEN is not set. Put a fresh token in .env (see README/setup steps).")
    if not CHANNEL_ID:
        sys.exit("ROOF_CHANNEL_ID is not set in .env.")
    log.info("Starting roof monitor. Status file: %s | NINA safety file: %s", STATUS_PATH, SAFE_PATH)
    client.run(TOKEN, log_handler=None)  # reuse our logging config


if __name__ == "__main__":
    main()
