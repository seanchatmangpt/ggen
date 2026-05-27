#!/usr/bin/env bash
# ==============================================================================
# 07_check_anti_fake.sh
# Scans files to verify no mocking or stubbing is performed.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [07] Scanning for Anti-Fake Violations ==="
# Locate workspace root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

if ! bash scripts/gall/anti_fake_implementation.sh; then
    echo "FAIL: Anti-fake violations detected." >&2
    exit 1
fi
echo "PASS: No anti-fake violations detected."
exit 0
