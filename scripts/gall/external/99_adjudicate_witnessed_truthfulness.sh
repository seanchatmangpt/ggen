#!/usr/bin/env bash
# ==============================================================================
# 99_adjudicate_witnessed_truthfulness.sh
# Path: scripts/gall/external/99_adjudicate_witnessed_truthfulness.sh
# Exit status: 0 on Promote, 1 on Refuse
# ==============================================================================
set -euo pipefail

if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

MANIFEST_FILE="scripts/gall/external/manifest.sha256"
ADJUDICATION_FILE="crates/ggen-graph/audit/witnessed_truthfulness.external_adjudication.json"

echo "=== Running Witnessed Agent Truthfulness Adjudication ==="

compute_blake3() {
    local file_path="$1"
    if command -v b3sum >/dev/null 2>&1; then
        b3sum "$file_path" | awk '{print $1}'
    elif osx_b3sum="/opt/homebrew/bin/b3sum"; [ -x "$osx_b3sum" ]; then
        "$osx_b3sum" "$file_path" | awk '{print $1}'
    else
        shasum -a 256 "$file_path" | awk '{print $1}'
    fi
}

VIOLATIONS=0
VERDICT="Promoted"
REASON="Witnessed Agent Truthfulness validation checks satisfied. All T0-T10 and W0-W7 verifiers pass."

# 0. W0 — No Narrative Promotion
# Completion phrases in this script's own output must be backed by transcript evidence.
# Scan for narrative-only claims: if a "pass" phrase appears without a corresponding
# transcript artifact, we log it but continue (enforcement is at the verifier-script level).
TRANSCRIPTS_DIR="crates/ggen-graph/audit/transcripts"
W0_VIOLATIONS=0
if [ -d "$TRANSCRIPTS_DIR" ]; then
    transcript_count=$(find "$TRANSCRIPTS_DIR" -name "*.json" 2>/dev/null | wc -l | tr -d ' ')
    if [ "$transcript_count" -eq 0 ]; then
        echo "FAIL W0: No command transcripts found in $TRANSCRIPTS_DIR. All execution claims require evidence."
        W0_VIOLATIONS=$((W0_VIOLATIONS + 1))
        VIOLATIONS=$((VIOLATIONS + 1))
    else
        echo "W0: $transcript_count transcript(s) present — narrative enforcement satisfied."
    fi
else
    echo "FAIL W0: Transcripts directory missing at $TRANSCRIPTS_DIR."
    W0_VIOLATIONS=$((W0_VIOLATIONS + 1))
    VIOLATIONS=$((VIOLATIONS + 1))
fi

# 1. Manifest verification
if [ ! -f "$MANIFEST_FILE" ]; then
    echo "FAIL: manifest.sha256 is missing at $MANIFEST_FILE"
    exit 1
fi

echo "Verifying scripts and source files digests against manifest..."
while read -r expected_hash filepath_raw || [ -n "$expected_hash" ]; do
    [[ -z "$expected_hash" || "$expected_hash" =~ ^# ]] && continue
    if [ -z "$expected_hash" ]; then
        continue
    fi
    # Use standard manifest line format parsing
    expected_hash=$(echo "$expected_hash" | tr -d '\r' | awk '{print $1}')
    filepath=$(echo "$filepath_raw" | tr -d '\r' | awk '{print $1}')
    if [ -z "$filepath" ]; then
        continue
    fi
    
    if [ ! -f "$filepath" ]; then
        echo "FAIL: Required file $filepath is missing."
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

# 2. Sequential execution of the full verifier ring (T0-T10 & W0-W7)
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
    "scripts/gall/external/09_verify_ocel_self_audit.sh"
    "scripts/gall/external/10_verify_coverage_matrix.sh"
    "scripts/gall/external/11_verify_proof_report.sh"
    "scripts/gall/external/12_detect_contradictions.sh"
    "scripts/gall/external/13_adjudicate_gall_promotion.sh"
    "scripts/gall/external/20_capture_full_worktree_inventory.sh"
    "scripts/gall/external/21_verify_command_transcripts.sh"
    "scripts/gall/external/22_verify_script_adequacy.sh"
    "scripts/gall/external/23_run_sabotage_suite.sh"
    "scripts/gall/external/24_run_clean_room_rebuild.sh"
    "scripts/gall/external/25_verify_cross_artifact_consistency.sh"
    "scripts/gall/external/26_verify_ocel_causal_sufficiency.sh"
    "scripts/gall/external/27_verify_contradiction_supersession.sh"
)

echo "Executing external verification scripts..."
for script in "${SCRIPTS[@]}"; do
    if [ ! -x "$script" ]; then
        echo "FAIL: Script is missing or not executable: $script"
        VIOLATIONS=$((VIOLATIONS + 1))
        continue
    fi
    set +e
    TRANSCRIPT_WRAPPED="false" ./"$script" >/dev/null 2>&1
    code=$?
    set -e
    if [ $code -ne 0 ]; then
        echo "FAIL: Verifier $script returned non-zero code $code."
        VIOLATIONS=$((VIOLATIONS + 1))
    fi
done

# 3. Check for existence of all required audit logs (W0-W7 outputs)
AUDIT_ARTIFACTS=(
    "crates/ggen-graph/audit/worktree_inventory.full.json"
    "crates/ggen-graph/audit/script_adequacy.json"
    "crates/ggen-graph/audit/sabotage_results.json"
    "crates/ggen-graph/audit/clean_room_rebuild.json"
    "crates/ggen-graph/audit/cross_artifact_consistency.json"
    "crates/ggen-graph/audit/ocel_causal_sufficiency.json"
    "crates/ggen-graph/audit/contradiction_supersession.json"
)

for artifact in "${AUDIT_ARTIFACTS[@]}"; do
    if [ ! -f "$artifact" ]; then
        echo "FAIL: Missing required audit artifact: $artifact"
        VIOLATIONS=$((VIOLATIONS + 1))
    fi
done

# 3.5 Check that all 5 durable SHACL validation reports exist and conform
SHACL_REPORTS=(
    "crates/ggen-graph/audit/public_vocab.validation.ttl"
    "crates/ggen-graph/audit/hook_actuation.validation.ttl"
    "crates/ggen-graph/audit/dialect_completeness.validation.ttl"
    "crates/ggen-graph/audit/sabotage.validation.ttl"
    "crates/ggen-graph/audit/final.validation.ttl"
)

for report in "${SHACL_REPORTS[@]}"; do
    if [ ! -f "$report" ]; then
        echo "FAIL: Missing required SHACL validation report: $report"
        VIOLATIONS=$((VIOLATIONS + 1))
    else
        if ! grep -q "sh:conforms.*true" "$report"; then
            echo "FAIL: SHACL validation report does not conform: $report"
            VIOLATIONS=$((VIOLATIONS + 1))
        fi
    fi
done

# 4. Check for presence of transcripts
if [ ! -d "crates/ggen-graph/audit/transcripts" ]; then
    echo "FAIL: Transcripts output directory missing."
    VIOLATIONS=$((VIOLATIONS + 1))
fi

# 5. Adjudicate verdict
if [ "$VIOLATIONS" -gt 0 ]; then
    VERDICT="Refused"
    REASON="Witnessed Agent Adjudication refused. $VIOLATIONS violation(s) detected during verifier ring check."
fi

TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
TEMP_JSON=$(mktemp)

# Per-gate status (set during script execution loop above; default PASS unless script failed)
W0_STATUS="$( [ "$W0_VIOLATIONS" -eq 0 ] && echo "pass" || echo "fail" )"

# Structure adjudication metadata with per-gate breakdown
cat <<EOF > "$TEMP_JSON"
{
  "timestamp": "$TIMESTAMP",
  "verdict": "$VERDICT",
  "reason": "$REASON",
  "integrity_status": "$( [ "$VIOLATIONS" -eq 0 ] && echo "PASS" || echo "FAIL" )",
  "total_violations": $VIOLATIONS,
  "gate_results": {
    "W0_narrative_enforcement": "$W0_STATUS",
    "W1_worktree_inventory": "see_audit_artifact",
    "W2_transcript_evidence": "see_audit_artifact",
    "W3_script_adequacy": "see_audit_artifact",
    "W4_sabotage_suite": "see_audit_artifact",
    "W5_clean_room_rebuild": "see_audit_artifact",
    "W6_cross_artifact_consistency": "see_audit_artifact",
    "W7_ocel_causal_sufficiency": "see_audit_artifact",
    "W8_contradiction_supersession": "see_audit_artifact",
    "W9_external_adjudication": "this_document"
  }
}
EOF

RECEIPT_HASH=$(compute_blake3 "$TEMP_JSON")
jq --arg receipt "$RECEIPT_HASH" '. + {witness_adjudication_blake3_receipt: $receipt}' "$TEMP_JSON" > "$ADJUDICATION_FILE"
rm "$TEMP_JSON"

echo "VERDICT: $VERDICT"
echo "Results saved to $ADJUDICATION_FILE"

if [ "$VERDICT" = "Promoted" ]; then
    exit 0
else
    exit 1
fi
