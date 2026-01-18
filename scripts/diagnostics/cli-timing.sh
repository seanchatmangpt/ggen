#!/usr/bin/env bash
set -euo pipefail

# Lightweight CLI timing wrapper with timeout and wall-clock measurement.
# Usage: scripts/diagnostics/cli-timing.sh [timeout] -- <command> [args...]
# Example: scripts/diagnostics/cli-timing.sh 15s -- ggen marketplace search --query "foo"

DEFAULT_TIMEOUT="30s"
TIMEOUT_DURATION="${1:-$DEFAULT_TIMEOUT}"

shift || true
if [[ "${1:-}" == "--" ]]; then
  shift
fi

if [[ $# -eq 0 ]]; then
  echo "Usage: scripts/diagnostics/cli-timing.sh [timeout] -- <command> [args...]" >&2
  exit 1
fi

CMD=("$@")

START_NS=$(date +%s%N)
if ! timeout "${TIMEOUT_DURATION}" /usr/bin/time -p "${CMD[@]}"; then
  echo "command_timed_out=true timeout=${TIMEOUT_DURATION}" >&2
  exit 124
fi
END_NS=$(date +%s%N)

ELAPSED_MS=$(( (END_NS - START_NS) / 1000000 ))
echo "elapsed_ms=${ELAPSED_MS}"
