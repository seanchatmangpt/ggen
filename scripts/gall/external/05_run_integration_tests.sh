#!/usr/bin/env bash
# ==============================================================================
# 05_run_integration_tests.sh
# Runs ggen-graph integration tests.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [05] Running Integration Tests ==="
if ! cargo test --tests -p ggen-graph; then
    echo "FAIL: Integration tests failed." >&2
    exit 1
fi
echo "PASS: Integration tests passed successfully."
exit 0
