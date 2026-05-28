#!/bin/bash
# Launches the roof monitor under /bin/bash. macOS TCC attributes file access to
# the responsible (parent) process, and /bin/bash already has Full Disk Access,
# so the Python child inherits permission to write the status files on the SMB
# network volume. (Granting FDA directly to the CommandLineTools framework Python
# is unreliable because the real executable is hidden inside Python.app.)
#
# Do NOT `exec` python here: keeping bash as the live parent is what makes the
# TCC responsibility inheritance work. The trailing log line also prevents bash's
# "last command" exec optimization from replacing bash with python.
cd /Users/briancarter/Rdata/astro-tools || exit 1
./.venv/bin/python discord-bot.py
echo "roofmonitor: python exited with status $?" >&2
