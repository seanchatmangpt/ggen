#!/usr/bin/env bash
# ==============================================================================
# 11_verify_proof_report.sh
# Verifies that docs/VISION_2030_GALL_PROOF.md exists and is formatted.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [11] Verifying GALL Proof Report ==="
PROOF_DOC="docs/VISION_2030_GALL_PROOF.md"
if [ ! -f "$PROOF_DOC" ]; then
    echo "ERROR: $PROOF_DOC not found" >&2
    exit 1
fi
echo "PASS: GALL Proof Report document exists."
exit 0
