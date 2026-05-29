#!/usr/bin/env bash
# ==============================================================================
# 06_scan_forbidden_surfaces.sh
# Scans source files to verify no forbidden system boundaries are bypassed.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [06] Scanning for Forbidden Surfaces ==="
# Locate workspace root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

if ! bash scripts/gall/forbidden_surface.sh; then
    echo "FAIL: Forbidden surfaces detected." >&2
    exit 1
fi
echo "PASS: No forbidden surfaces detected."
exit 0
