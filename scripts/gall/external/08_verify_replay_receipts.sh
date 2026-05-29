#!/usr/bin/env bash
# ==============================================================================
# 08_verify_replay_receipts.sh
# Verifies that cryptographic receipts can be successfully replayed.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [08] Verifying Replay Receipts ==="
if ! cargo test --test receipt_replay -p ggen-graph; then
    echo "FAIL: Receipt replay verification failed." >&2
    exit 1
fi
echo "PASS: Receipt replay verified."
exit 0
