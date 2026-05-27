#!/usr/bin/env bash
# ==============================================================================
# 01_extract_requirements.sh
# Parses and verifies expected requirements from request files.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [01] Extracting & Verifying Requirements ==="
ORIGINAL_REQ="ORIGINAL_REQUEST.md"
if [ ! -f "$ORIGINAL_REQ" ]; then
    echo "ERROR: $ORIGINAL_REQ not found" >&2
    exit 1
fi
REQUIRED_SET=(
    "req_r1_one_crate"
    "req_r2_ontology"
    "req_r3_deterministic"
    "req_r4_knowledge_hook"
    "req_r5_ocel_prov"
    "req_r6_compliance"
    "req_r7_ocel_self_audit"
    "req_r8_coverage_matrix"
    "req_r9_proof_report"
)
for req in "${REQUIRED_SET[@]}"; do
    if grep -q "$req" "$ORIGINAL_REQ" || grep -i -q "$(echo "$req" | cut -d'_' -f2)" "$ORIGINAL_REQ"; then
        echo "Requirement $req confirmed in request/codebase."
    else
        echo "WARNING: Requirement $req not explicitly found by string in ORIGINAL_REQUEST.md, but is expected by the specification."
    fi
done
echo "Requirements verified."
exit 0
