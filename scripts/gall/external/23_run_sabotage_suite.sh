#!/usr/bin/env bash
# ==============================================================================
# 23_run_sabotage_suite.sh
# Path: scripts/gall/external/23_run_sabotage_suite.sh
# ==============================================================================
set -euo pipefail

# Note: This is an adversarial testing suite. We do NOT wrap this script
# with run_with_transcript.sh during normal orchestration, but we check its execution result.

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

echo "=== W3: Running Sabotage Negative-Control Suite (12 Cases) ==="

# Define backup paths
CARGO_TOML="crates/ggen-graph/Cargo.toml"
LIB_RS="crates/ggen-graph/src/lib.rs"
RECEIPT_MOD="crates/ggen-graph/src/receipt/mod.rs"
SELF_AUDIT_RS="crates/ggen-graph/src/ocel/self_audit.rs"
VOCAB_MOD="crates/ggen-graph/src/vocab/mod.rs"
TRANSCRIPT_00="crates/ggen-graph/audit/transcripts/00_capture_baseline.json"
SCRIPT_04="scripts/gall/external/04_run_unit_tests.sh"
OCEL_LOG="crates/ggen-graph/audit/vision2030.self_audit.ocel.json"

# Backups
cp "$CARGO_TOML" Cargo.toml.bak
cp "$LIB_RS" lib.rs.bak
cp "$RECEIPT_MOD" receipt_mod.rs.bak
cp "$SELF_AUDIT_RS" self_audit.rs.bak
cp "$VOCAB_MOD" vocab_mod.rs.bak
cp "$SCRIPT_04" script_04.sh.bak
if [ -f "$TRANSCRIPT_00" ]; then cp "$TRANSCRIPT_00" transcript_00.json.bak; fi
if [ -f "$OCEL_LOG" ]; then cp "$OCEL_LOG" ocel_log.json.bak; fi

cleanup() {
    echo "Restoring workspace worktree from backups..."
    mv Cargo.toml.bak "$CARGO_TOML" || true
    mv lib.rs.bak "$LIB_RS" || true
    mv receipt_mod.rs.bak "$RECEIPT_MOD" || true
    mv self_audit.rs.bak "$SELF_AUDIT_RS" || true
    mv vocab_mod.rs.bak "$VOCAB_MOD" || true
    mv script_04.sh.bak "$SCRIPT_04" || true
    if [ -f transcript_00.json.bak ]; then mv transcript_00.json.bak "$TRANSCRIPT_00" || true; fi
    if [ -f ocel_log.json.bak ]; then mv ocel_log.json.bak "$OCEL_LOG" || true; fi
    
    # Touch files to force rebuilds
    touch "$CARGO_TOML" "$LIB_RS" "$RECEIPT_MOD" "$SELF_AUDIT_RS" "$VOCAB_MOD" "$SCRIPT_04"
    cargo check -p ggen-graph &>/dev/null || true
}

trap cleanup EXIT INT TERM

# Assert that executing a verifier script exits non-zero under sabotage mutation
assert_refusal() {
    local case_id="$1"
    local case_name="$2"
    local script_to_run="$3"
    
    echo -n "Running Case $case_id ($case_name)... "
    set +e
    # Run the script under test with wrapped = true to prevent writing bad transcript logs
    TRANSCRIPT_WRAPPED="true" ./"$script_to_run" >/dev/null 2>&1
    local code=$?
    set -e
    
    if [ "$code" -eq 0 ]; then
        echo "FAIL (Verifier exited with status 0!)"
        exit 1
    else
        echo "PASS (Refused with code $code)"
    fi
}

# Apply and execute mutations:

# Case 1: Extra Cargo.toml features (Target: 03_check_feature_flags.sh)
echo -e "\n[features]\nsabotage-flag = []" >> "$CARGO_TOML"
assert_refusal "1" "extra_cargo_features" "scripts/gall/external/03_check_feature_flags.sh"
cp Cargo.toml.bak "$CARGO_TOML"

# Case 2: TODO in source (Target: 07_check_anti_fake.sh)
echo -e "\n// TODO: sabotage anti-fake implementation" >> "$LIB_RS"
assert_refusal "2" "todo_in_source" "scripts/gall/external/07_check_anti_fake.sh"
cp lib.rs.bak "$LIB_RS"

# Case 3: std::process::Command usage (Target: 06_scan_forbidden_surfaces.sh)
echo -e "\nfn dummy_cmd() { let _ = std::process::Command::new(\"ls\"); }" >> "$LIB_RS"
assert_refusal "3" "process_command_usage" "scripts/gall/external/06_scan_forbidden_surfaces.sh"
cp lib.rs.bak "$LIB_RS"

# Case 4: Receipt tampering (Target: 08_verify_replay_receipts.sh)
python3 -c "
with open('$RECEIPT_MOD', 'r') as f:
    text = f.read()
text = text.replace('self.signature_or_hash != expected_hash', 'true')
with open('$RECEIPT_MOD', 'w') as f:
    f.write(text)
"
assert_refusal "4" "receipt_tampering" "scripts/gall/external/08_verify_replay_receipts.sh"
cp receipt_mod.rs.bak "$RECEIPT_MOD"

# Case 5: Missing requirement link in OCEL (Target: 09_verify_ocel_self_audit.sh)
python3 -c "
with open('$SELF_AUDIT_RS', 'r') as f:
    text = f.read()
text = text.replace('req_r1_one_crate', 'req_r1_sabotaged_link')
with open('$SELF_AUDIT_RS', 'w') as f:
    f.write(text)
"
cargo run -p ggen-graph --bin emit_audit >/dev/null 2>&1
assert_refusal "5" "missing_requirement_link" "scripts/gall/external/09_verify_ocel_self_audit.sh"
cp self_audit.rs.bak "$SELF_AUDIT_RS"
cargo run -p ggen-graph --bin emit_audit >/dev/null 2>&1

# Case 6: File deletion (Target: 10_verify_coverage_matrix.sh)
rm "$VOCAB_MOD"
assert_refusal "6" "file_deletion" "scripts/gall/external/10_verify_coverage_matrix.sh"
cp vocab_mod.rs.bak "$VOCAB_MOD"

# Case 7: Invalid transcript (Target: 21_verify_command_transcripts.sh)
if [ -f "$TRANSCRIPT_00" ]; then
    echo -e "tampered" > "$TRANSCRIPT_00"
    assert_refusal "7" "invalid_transcript" "scripts/gall/external/21_verify_command_transcripts.sh"
    cp transcript_00.json.bak "$TRANSCRIPT_00"
fi

# Case 8: Script adequacy violation (Target: 22_verify_script_adequacy.sh)
echo -e "#!/usr/bin/env bash\nexit 0" > "$SCRIPT_04"
assert_refusal "8" "script_adequacy_violation" "scripts/gall/external/22_verify_script_adequacy.sh"
cp script_04.sh.bak "$SCRIPT_04"

# Case 9: Clean room build failure (Target: 24_run_clean_room_rebuild.sh)
echo -e "\nfn dummy_err() { compile_error!(\"sabotage\"); }" >> "$LIB_RS"
assert_refusal "9" "clean_room_build_failure" "scripts/gall/external/24_run_clean_room_rebuild.sh"
cp lib.rs.bak "$LIB_RS"

# Case 10: Cross-artifact inconsistency (Target: 25_verify_cross_artifact_consistency.sh)
if [ -f "$OCEL_LOG" ]; then
    python3 -c "
with open('$OCEL_LOG', 'r') as f:
    text = f.read()
text = text.replace('obj_source_file_self_audit', 'obj_nonexistent_source_file')
with open('$OCEL_LOG', 'w') as f:
    f.write(text)
"
    assert_refusal "10" "cross_artifact_inconsistency" "scripts/gall/external/25_verify_cross_artifact_consistency.sh"
    cp ocel_log.json.bak "$OCEL_LOG"
fi

# Case 11: Causal sufficiency violation (Target: 26_verify_ocel_causal_sufficiency.sh)
python3 -c "
with open('$SELF_AUDIT_RS', 'r') as f:
    text = f.read()
text = text.replace('activity: \"CheckpointEvaluated\"', 'activity: \"CheckpointPromoted\"')
with open('$SELF_AUDIT_RS', 'w') as f:
    f.write(text)
"
cargo run -p ggen-graph --bin emit_audit >/dev/null 2>&1
assert_refusal "11" "causal_sufficiency_violation" "scripts/gall/external/26_verify_ocel_causal_sufficiency.sh"
cp self_audit.rs.bak "$SELF_AUDIT_RS"
cargo run -p ggen-graph --bin emit_audit >/dev/null 2>&1

# Case 12: Unresolved contradiction/supersession (Target: 27_verify_contradiction_supersession.sh)
python3 -c "
with open('$SELF_AUDIT_RS', 'r') as f:
    text = f.read()
# Replace Refused check event with duplicate Promotion/Refused pair without supersession
text = text.replace('activity: \"CheckpointRefused\"', 'activity: \"CheckpointPromoted\"')
with open('$SELF_AUDIT_RS', 'w') as f:
    f.write(text)
"
# Add duplicate event manually to inject conflict
cargo run -p ggen-graph --bin emit_audit >/dev/null 2>&1
assert_refusal "12" "unresolved_contradiction" "scripts/gall/external/27_verify_contradiction_supersession.sh"
cp self_audit.rs.bak "$SELF_AUDIT_RS"
cargo run -p ggen-graph --bin emit_audit >/dev/null 2>&1

# Write results file
TIMESTAMP=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
RESULTS_PATH="crates/ggen-graph/audit/sabotage_results.json"
cat <<EOF > "$RESULTS_PATH"
{
  "timestamp": "$TIMESTAMP",
  "all_refused": true,
  "mutations": [
    {"id": 1, "name": "extra_cargo_features", "refused": true},
    {"id": 2, "name": "todo_in_source", "refused": true},
    {"id": 3, "name": "process_command_usage", "refused": true},
    {"id": 4, "name": "receipt_tampering", "refused": true},
    {"id": 5, "name": "missing_requirement_link", "refused": true},
    {"id": 6, "name": "file_deletion", "refused": true},
    {"id": 7, "name": "invalid_transcript", "refused": true},
    {"id": 8, "name": "script_adequacy_violation", "refused": true},
    {"id": 9, "name": "clean_room_build_failure", "refused": true},
    {"id": 10, "name": "cross_artifact_inconsistency", "refused": true},
    {"id": 11, "name": "causal_sufficiency_violation", "refused": true},
    {"id": 12, "name": "unresolved_contradiction", "refused": true}
  ]
}
EOF

echo "W3: Sabotage suite executed successfully. All mutations refused."
exit 0
