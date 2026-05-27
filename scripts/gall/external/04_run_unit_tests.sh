#!/usr/bin/env bash
# ==============================================================================
# 04_run_unit_tests.sh
# Runs ggen-graph unit tests.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [04] Running Unit Tests ==="
if ! cargo test --lib -p ggen-graph; then
    echo "FAIL: Unit tests failed." >&2
    exit 1
fi
echo "PASS: Unit tests passed successfully."
exit 0
