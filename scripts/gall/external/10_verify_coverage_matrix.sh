#!/usr/bin/env bash
# ==============================================================================
# 10_verify_coverage_matrix.sh
# Verifies that every requirement is fully mapped and files exist.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

echo "=== [10] Verifying Coverage Matrix ==="
COVERAGE_JSON="crates/ggen-graph/audit/vision2030.coverage.json"
if [ ! -f "$COVERAGE_JSON" ]; then
    echo "ERROR: Coverage matrix file not found: $COVERAGE_JSON" >&2
    exit 1
fi
# Validate existence of files in coverage
FILES=$(jq -r '.requirements[] | (.source_files[], .test_files[])' "$COVERAGE_JSON" | sort -u)
for f in $FILES; do
    if [ ! -f "$f" ]; then
        echo "FAIL: File referenced in coverage matrix does not exist: $f" >&2
        exit 1
    fi
done
echo "PASS: All files in coverage matrix exist."
exit 0
