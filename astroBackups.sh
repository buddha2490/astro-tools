#!/bin/bash
# astro_backup.sh — mirror Astro-SSD to shared/astro-ssd

set -euo pipefail

# ---- config ----
SOURCE="/Volumes/Astro-SSD/"
DEST="/Volumes/shared/astro-ssd/"
LOG="${HOME}/astro-tools/logs/astro_backup.log"
LOCK="/tmp/astro_backup.lock"
PATH="/usr/local/bin:/opt/homebrew/bin:/usr/bin:/bin:/usr/sbin:/sbin"

# ---- lock (prevent overlap) ----
if ! mkdir "$LOCK" 2>/dev/null; then
  echo "$(date '+%Y-%m-%d %H:%M:%S') Another backup is already running." >> "$LOG"
  exit 0
fi
trap 'rmdir "$LOCK"' EXIT

# ---- prechecks ----
if [[ ! -d "$SOURCE" ]]; then
  echo "$(date '+%Y-%m-%d %H:%M:%S') Source not mounted: $SOURCE" >> "$LOG"
  exit 1
fi

mkdir -p "$DEST"

# ---- backup (mirror) ----
echo "========== $(date '+%Y-%m-%d %H:%M:%S') backup start ==========" >> "$LOG"

# Built-in macOS rsync works; Homebrew rsync (v3) is fine too.
# Trailing slash on SOURCE copies its *contents* into DEST.
rsync -aE --delete --human-readable \
  --log-file="$LOG" \
  "$SOURCE" "$DEST"

RC=$?
echo "$(date '+%Y-%m-%d %H:%M:%S') backup finished (rc=$RC)" >> "$LOG"
exit $RC
