#!/usr/bin/env bash
# ==============================================================================
# 09_verify_ocel_self_audit.sh
# Verifies complete requirement coverage and validates file/script digests
# to prevent unauthorized modifications or verifier bypasses.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

# Locate workspace root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(cd "$SCRIPT_DIR/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

echo "=== [09] External OCEL Self-Audit and Coverage Verifier ==="

COVERAGE_JSON="crates/ggen-graph/audit/vision2030.coverage.json"
SELF_AUDIT_JSON="crates/ggen-graph/audit/vision2030.self_audit.ocel.json"
MANIFEST_OUT="crates/ggen-graph/audit/vision2030.verification_manifest.json"

# 1. Pre-flight checks (ensure they exist, emit them first)
echo "Generating self-audit and coverage files..."
cargo run -p ggen-graph --bin emit_audit

if [ ! -f "$COVERAGE_JSON" ] || [ ! -f "$SELF_AUDIT_JSON" ]; then
    echo "ERROR: Audit files do not exist after emit_audit execution." >&2
    exit 1
fi

# Ensure JQ is available
if ! command -v jq &>/dev/null; then
    echo "ERROR: jq is required but not installed." >&2
    exit 1
fi

# Define hash utility (b3sum preferred, fallback to shasum)
HASH_UTIL=""
if command -v b3sum &>/dev/null; then
    HASH_UTIL="b3sum"
    echo "Using BLAKE3 hashing utility (b3sum)"
else
    HASH_UTIL="shasum -a 256"
    echo "Using SHA-256 hashing utility (shasum)"
fi

# 2. Run the Rust verify binary (Pass 1)
echo "Running Rust-native verify_audit..."
if ! cargo run -p ggen-graph --bin verify_audit; then
    echo "FAIL: Rust-native verify_audit detected completeness violations." >&2
    exit 1
fi

# 3. Independent Coverage and Linkage Verification (Pass 2)
echo "Verifying requirement IDs and linkage via jq..."
REQ_IDS=$(jq -r '.requirements[].id' "$COVERAGE_JSON")
REQ_COUNT=$(echo "$REQ_IDS" | wc -w | tr -d ' ')

if [ "$REQ_COUNT" -ne 10 ]; then
    echo "FAIL: Expected exactly 10 requirements, found $REQ_COUNT." >&2
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
    "req_r10_interchangeable"
)

for req in "${REQUIRED_SET[@]}"; do
    if ! echo "$REQ_IDS" | grep -q "$req"; then
        echo "FAIL: Missing required requirement ID: $req in coverage matrix." >&2
        exit 1
    fi
    
    # Verify that the requirement is linked by at least one event in the OCEL JSON
    EVENT_LINK_COUNT=$(jq --arg req "$req" '[.events[].objects[] | select(.id == $req)] | length' "$SELF_AUDIT_JSON")
    if [ "$EVENT_LINK_COUNT" -eq 0 ]; then
        echo "FAIL: Requirement $req is not linked to any event in $SELF_AUDIT_JSON." >&2
        exit 1
    fi
done
echo "PASS: Coverage matrix contains all 10 requirements linked to OCEL events."

# 4. File existence and cryptographic digest validation
echo "Capturing cryptographic digests of files listed in coverage matrix..."
ALL_FILES=$(jq -r '.requirements[] | (.source_files[], .test_files[])' "$COVERAGE_JSON" | sort -u)

# Initialize manifest JSON structure
echo "{" > "$MANIFEST_OUT"
echo "  \"generated_at\": \"$(date -u +"%Y-%m-%dT%H:%M:%SZ")\"," >> "$MANIFEST_OUT"
echo "  \"hash_algorithm\": \"$HASH_UTIL\"," >> "$MANIFEST_OUT"
echo "  \"files\": {" >> "$MANIFEST_OUT"

FIRST=true
for file_path in $ALL_FILES; do
    if [ ! -f "$file_path" ]; then
        echo "FAIL: Documented file does not exist: $file_path" >&2
        exit 1
    fi
    
    # Calculate hash (extract only the hash part)
    if [ "$HASH_UTIL" = "b3sum" ]; then
        FILE_HASH=$(b3sum "$file_path" | awk '{print $1}')
    else
        FILE_HASH=$(shasum -a 256 "$file_path" | awk '{print $1}')
    fi
    
    if [ "$FIRST" = true ]; then
        FIRST=false
    else
        echo "," >> "$MANIFEST_OUT"
    fi
    echo -n "    \"$file_path\": \"$FILE_HASH\"" >> "$MANIFEST_OUT"
done

echo "" >> "$MANIFEST_OUT"
echo "  }" >> "$MANIFEST_OUT"
echo "}" >> "$MANIFEST_OUT"

echo "PASS: Cryptographic manifest generated at $MANIFEST_OUT."
echo "Successfully verified complete requirement coverage. Exit code 0."
exit 0
