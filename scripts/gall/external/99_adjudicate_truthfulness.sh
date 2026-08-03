#!/usr/bin/env bash
# ==============================================================================
# 99_adjudicate_truthfulness.sh
# Adjudicates agent truthfulness by validating the T0-T8/T10-T13/T20 external
# verifier ring.
# Path: scripts/gall/external/99_adjudicate_truthfulness.sh
# Exit code: 0 on Promoted, 1 on Refused/Failure
#
# NOTE (2026-08-03): this script used to also parse
# crates/ggen-graph/audit/vision2030.self_audit.ocel.json (via
# scripts/gall/external/09_verify_ocel_self_audit.sh and the Rust
# emit_audit/verify_audit binaries) and run "cardinality"/"causality" checks
# over it. That file's generator (ggen_graph::ocel::self_audit::
# generate_self_audit_log) hardcoded literal fields presented as observations
# -- a fake exit_code, a sha256 that is actually sha256("test"), fabricated
# coverage percentages, and compile-time-fixed event timestamps -- so those
# checks were only ever verifying that the same generator produced
# self-consistent fake data, never that any command actually ran, any test
# actually passed, or any coverage was actually measured. That whole path
# (self_audit.rs's OCEL fixture, emit_audit.rs, verify_audit.rs,
# 09_verify_ocel_self_audit.sh) has been removed from this adjudication ring;
# self_audit.rs's log generator remains only as fixture input for the
# ocel_self_audit.rs graph-projection round-trip test, and is documented as
# such there. See crates/ggen-graph/tests/no_fabricated_truthfulness_evidence.rs.
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

INVENTORY_FILE="crates/ggen-graph/audit/worktree_inventory.json"
MANIFEST_FILE="scripts/gall/external/manifest.sha256"
TRUTHFULNESS_FILE="crates/ggen-graph/audit/agent_truthfulness.external_adjudication.json"

echo "=== Running Agent Truthfulness Adjudicator (Verifier 99) ==="

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

VIOLATIONS=0
VERDICT="Promoted"
REASON="T0-T8/T10-T13/T20 verifier ring checks pass."

# 1. Pre-flight checks
if [ ! -f "$INVENTORY_FILE" ]; then
    echo "FAIL: Worktree inventory missing at $INVENTORY_FILE"
    exit 1
fi

# 2. Verify Manifest Integrity
if [ ! -f "$MANIFEST_FILE" ]; then
    echo "FAIL: Adjudication failed. Integrity manifest file missing at: $MANIFEST_FILE"
    exit 1
fi

echo "Verifying verifier scripts & source files integrity against manifest..."
while read -r expected_hash filepath || [ -n "$expected_hash" ]; do
    [[ -z "$expected_hash" || "$expected_hash" =~ ^# ]] && continue
    
    expected_hash=$(echo "$expected_hash" | tr -d '\r' | awk '{print $1}')
    filepath=$(echo "$filepath" | tr -d '\r' | awk '{print $2}')
    if [ -z "$filepath" ]; then
        continue
    fi
    
    if [ ! -f "$filepath" ]; then
        echo "FAIL: Required file $filepath does not exist."
        VIOLATIONS=$((VIOLATIONS + 1))
        continue
    fi
    
    # Compute SHA-256
    if command -v sha256sum >/dev/null 2>&1; then
        current_hash=$(sha256sum "$filepath" | awk '{print $1}')
    elif command -v shasum >/dev/null 2>&1; then
        current_hash=$(shasum -a 256 "$filepath" | awk '{print $1}')
    else
        current_hash=$(openssl dgst -sha256 "$filepath" | awk '{print $NF}')
    fi
    
    if [ "$current_hash" != "$expected_hash" ]; then
        echo "FAIL: Integrity violation for $filepath. Expected $expected_hash, got $current_hash"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi
done < "$MANIFEST_FILE"

# 3. Execute and verify the external script ring (T0-T8, T10-T13, T20)
echo "Executing and verifying external script ring (T0-T8, T10-T13, T20)..."
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
    "scripts/gall/external/10_verify_coverage_matrix.sh"
    "scripts/gall/external/11_verify_proof_report.sh"
    "scripts/gall/external/12_detect_contradictions.sh"
    "scripts/gall/external/13_adjudicate_gall_promotion.sh"
    "scripts/gall/external/20_capture_full_worktree_inventory.sh"
)

# Unset TRANSCRIPT_WRAPPED so sub-scripts can wrap themselves and write clean transcripts
for script in "${SCRIPTS[@]}"; do
    if [ ! -x "$script" ]; then
        echo "FAIL: Script missing or not executable: $script"
        VIOLATIONS=$((VIOLATIONS + 1))
        continue
    fi
    set +e
    TRANSCRIPT_WRAPPED=false ./"$script" > /dev/null 2>&1
    code=$?
    set -e
    if [ $code -ne 0 ]; then
        echo "FAIL: Verifier $script returned exit code $code"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi
done

# 4. Adjudication Summary & File Output
if [ "$VIOLATIONS" -gt 0 ]; then
    VERDICT="Refused"
    REASON="Truthfulness adjudication refused. $VIOLATIONS violation(s) detected in execution and causal checks."
    echo "VERDICT: $VERDICT"
    echo "$REASON"
    rm -f "$TRUTHFULNESS_FILE"
    exit 1
fi

TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
TEMP_JSON=$(mktemp)

cat <<EOF > "$TEMP_JSON"
{
  "timestamp": "$TIMESTAMP",
  "verdict": "$VERDICT",
  "reason": "$REASON",
  "verifier_ring_violations": $VIOLATIONS
}
EOF

RECEIPT_HASH=$(compute_blake3 "$TEMP_JSON")
jq --arg receipt "$RECEIPT_HASH" '. + {adjudication_blake3_receipt: $receipt}' "$TEMP_JSON" > "$TRUTHFULNESS_FILE"
rm "$TEMP_JSON"

echo "=== Adjudication Completed ==="
echo "Verdict: $VERDICT"
echo "Receipt: $RECEIPT_HASH"
echo "Results written to $TRUTHFULNESS_FILE"

exit 0
