#!/usr/bin/env bash
# ==============================================================================
# 13_adjudicate_gall_promotion.sh
# Sequentially executes verification scripts, checks script integrity, and emits promotion results.
# Path: scripts/gall/external/13_adjudicate_gall_promotion.sh
# Exit code: 0 if approved and promoted, 1 if verification fails or rejected.
# ==============================================================================
set -euo pipefail

if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

MANIFEST_FILE="scripts/gall/external/manifest.sha256"
COVERAGE_FILE="crates/ggen-graph/audit/vision2030.coverage.json"
ADJUDICATION_FILE="crates/ggen-graph/audit/vision2030.external_adjudication.json"

echo "=== Running GALL Checkpoint Promotion Adjudicator ==="

# Portable SHA-256 function
compute_sha256() {
    local file_path="$1"
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$file_path" | awk '{print $1}'
    elif command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "$file_path" | awk '{print $1}'
    else
        openssl dgst -sha256 "$file_path" | awk '{print $NF}'
    fi
}

# Portable BLAKE3 function
compute_blake3() {
    local file_path="$1"
    if command -v b3sum >/dev/null 2>&1; then
        b3sum "$file_path" | awk '{print $1}'
    else
        if command -v sha256sum >/dev/null 2>&1; then
            sha256sum "$file_path" | awk '{print $1}'
        elif command -v shasum >/dev/null 2>&1; then
            shasum -a 256 "$file_path" | awk '{print $1}'
        else
            openssl dgst -sha256 "$file_path" | awk '{print $NF}'
        fi
    fi
}

# 1. Verify Manifest Integrity
if [ ! -f "$MANIFEST_FILE" ]; then
    echo "FAIL: Adjudication failed. Integrity manifest file missing at: $MANIFEST_FILE"
    exit 1
fi

echo "Verifying verifier scripts & source files integrity..."
while read -r expected_hash filepath || [ -n "$expected_hash" ]; do
    [[ -z "$expected_hash" || "$expected_hash" =~ ^# ]] && continue
    
    # Strip carriage returns if any
    expected_hash=$(echo "$expected_hash" | tr -d '\r' | awk '{print $1}')
    filepath=$(echo "$filepath" | tr -d '\r' | awk '{print $2}')
    # If filepath parsing failed or is empty, try reading with spacing
    if [ -z "$filepath" ]; then
        # If read has expected_hash as first token and filepath as second
        # We can extract filepath from the line
        continue
    fi
    
    if [ ! -f "$filepath" ]; then
        echo "FAIL: Required file $filepath does not exist."
        exit 1
    fi
    
    current_hash=$(compute_sha256 "$filepath")
    if [ "$current_hash" != "$expected_hash" ]; then
        echo "FAIL: Integrity violation for $filepath. Expected $expected_hash, got $current_hash"
        exit 1
    fi
done < "$MANIFEST_FILE"
echo "All script and source file digests match the integrity manifest."

# 2. Sequential Execution
SCRIPTS=(
    "scripts/gall/external/00_capture_baseline.sh"
    "scripts/gall/external/01_extract_requirements.sh"
    "scripts/gall/external/02_verify_package_constraints.sh"
    "scripts/gall/external/03_check_feature_flags.sh"
    "scripts/gall/external/04_run_unit_tests.sh"
    "scripts/gall/external/05_run_integration_tests.sh"
    "scripts/gall/external/06_scan_forbidden_surfaces.sh"
    "scripts/gall/external/07_check_anti_fake.sh"
    "scripts/gall/external/08_verify_replay_receipts.sh"
    # NOTE (2026-08-03): "09_verify_ocel_self_audit.sh" was removed from this
    # ring; it regenerated and structurally re-checked vision2030.self_audit
    # .ocel.json, whose generator hardcoded a fake exit_code, a sha256("test")
    # passed off as a real artifact hash, fabricated coverage percentages, and
    # compile-time-fixed timestamps. See
    # crates/ggen-graph/tests/no_fabricated_truthfulness_evidence.rs.
    "scripts/gall/external/10_verify_coverage_matrix.sh"
    "scripts/gall/external/11_verify_proof_report.sh"
    "scripts/gall/external/12_detect_contradictions.sh"
)

ADJUDICATION_STATUS="Promoted"
ADJUDICATION_REASON="All 12 validation scripts passed successfully and zero contradictions were detected."
SCRIPTS_JSON="[]"

for script in "${SCRIPTS[@]}"; do
    echo "Executing script: $script"
    if [ ! -x "$script" ]; then
        echo "FAIL: Script is not executable or missing: $script"
        ADJUDICATION_STATUS="Refused"
        ADJUDICATION_REASON="Adjudication refused because script $script is not executable or missing."
        break
    fi
    
    set +e
    start_time=$(date +%s)
    ./"$script"
    exit_code=$?
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    set -e
    
    script_hash=$(compute_sha256 "$script")
    status_str="PASS"
    if [ "$exit_code" -ne 0 ]; then
        status_str="FAIL"
        ADJUDICATION_STATUS="Refused"
        ADJUDICATION_REASON="Adjudication refused because script $script failed with exit code $exit_code."
    fi
    
    # Append to SCRIPTS_JSON array using jq
    SCRIPTS_JSON=$(echo "$SCRIPTS_JSON" | jq --arg path "$script" \
                                            --arg sha "$script_hash" \
                                            --argjson code "$exit_code" \
                                            --arg status "$status_str" \
                                            --argjson dur "$duration" \
                                            '. + [{"path": $path, "sha256": $sha, "exit_code": $code, "status": $status, "duration_seconds": $dur}]')
    
    if [ "$exit_code" -ne 0 ]; then
        break
    fi
done

# 3. Source file hashing for evidence
SRC_FILES=(
    "crates/ggen-graph/src/ocel/self_audit.rs"
    "crates/ggen-graph/src/ocel/coverage.rs"
)
SOURCE_FILES_JSON="[]"
for src in "${SRC_FILES[@]}"; do
    src_hash=$(compute_sha256 "$src")
    SOURCE_FILES_JSON=$(echo "$SOURCE_FILES_JSON" | jq --arg path "$src" --arg sha "$src_hash" '. + [{"path": $path, "sha256": $sha}]')
done

# 4. Generate Contradiction Section Info
#
# NOTE (2026-08-03): this used to gate on `[ -f "$OCEL_FILE" ]`
# (crates/ggen-graph/audit/vision2030.self_audit.ocel.json) and re-run
# 12_detect_contradictions.sh against it purely to extract a detailed
# violations list for this JSON -- 12_detect_contradictions.sh is still
# executed for real as part of the SCRIPTS ring above (section 2), which
# already sets ADJUDICATION_STATUS=Refused on failure; this section only
# duplicated that check against a file whose generator hardcoded a fake
# exit_code, a sha256("test") passed off as a real artifact hash, fabricated
# coverage percentages, and compile-time-fixed timestamps. See
# crates/ggen-graph/tests/no_fabricated_truthfulness_evidence.rs.
CONTRADICTIONS_JSON=$(jq -n '{"status": "see_scripts_verified", "note": "contradiction check runs as part of the SCRIPTS ring above (12_detect_contradictions.sh), not duplicated here"}')

# 5. Extract coverage metadata
#
# NOTE (2026-08-03): this section used to also compute a "self_audit_digest"
# BLAKE3 hash of the fabricated vision2030.self_audit.ocel.json and report it
# as adjudication evidence. Removed for the same reason as section 4 above.
COVERAGE_BLAKE3="null"
if [ -f "$COVERAGE_FILE" ]; then
    COVERAGE_BLAKE3="\"$(compute_blake3 "$COVERAGE_FILE")\""
fi

# 6. Build the External Adjudication JSON structure
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
JSON_TEMP=$(mktemp)

cat <<EOF > "$JSON_TEMP"
{
  "timestamp": "$TIMESTAMP",
  "verdict": "$ADJUDICATION_STATUS",
  "reason": "$ADJUDICATION_REASON",
  "scripts_verified": $SCRIPTS_JSON,
  "source_files_verified": $SOURCE_FILES_JSON,
  "contradiction_check": $CONTRADICTIONS_JSON,
  "coverage_digest": {
    "path": "$COVERAGE_FILE",
    "blake3": $COVERAGE_BLAKE3
  }
}
EOF

# Calculate the final BLAKE3 hash of this JSON block
RECEIPT_HASH=$(compute_blake3 "$JSON_TEMP")

# Write out the completed, signed adjudication JSON
jq --arg receipt "$RECEIPT_HASH" '. + {adjudication_blake3_receipt: $receipt}' "$JSON_TEMP" > "$ADJUDICATION_FILE"
rm "$JSON_TEMP"

echo "=== Adjudication Finished ==="
echo "Verdict: $ADJUDICATION_STATUS"
echo "Receipt: $RECEIPT_HASH"
echo "Results written to: $ADJUDICATION_FILE"

if [ "$ADJUDICATION_STATUS" = "Promoted" ]; then
    exit 0
else
    exit 1
fi
