# External Observer Contradiction Detection & Adjudication Design

This report designs the contradiction detection and promotion adjudication logic for the External Observer Script Ring (`00` to `13`) under `scripts/gall/external/`. It provides fully realized, production-ready Shell/JQ code for `12_detect_contradictions.sh` and `13_adjudicate_gall_promotion.sh` to secure the GALL checkpoint lifecycle from self-verification, tampering, and logical inconsistency.

---

## 1. Observation

During read-only inspection of the repository, the following relevant files and structures were identified:

### 1.1 Existing Self-Audit Log & Coverage Schema
- **Crate Binary Targets**: `crates/ggen-graph/src/bin/emit_audit.rs` and `crates/ggen-graph/src/bin/verify_audit.rs`.
- **Verify Logic**: `crates/ggen-graph/src/bin/verify_audit.rs` enforces 5 Completeness Rules:
  1. *Requirements Have Evidence*: Every requirement in `vision2030.coverage.json` must map to non-empty files/tests/commands, and have corresponding events in the self-audit log.
  2. *Checkpoints Have Command Evidence*: Every `CheckpointEvaluated` event must link to `Command` or `CommandRun` objects.
  3. *Prior Evaluations Exist*: Every `CheckpointPromoted` or `CheckpointRefused` event must be preceded chronologically by a `CheckpointEvaluated` event for the same checkpoint.
  4. *Anti-Fake is Audited*: Event stream contains both `AntiFakeScanned` and `ForbiddenSurfaceScanned` events.
  5. *Unsupported Capabilities are Linked*: `UnsupportedCapabilityDeclared` events must link to `UnsupportedCapability` and a requirement.

### 1.2 Observed Self-Audit Log Contradictions
We analyzed `crates/ggen-graph/audit/vision2030.self_audit.ocel.json` (lines 264–297):
```json
    {
      "id": "ev_checkpoint_promoted",
      "activity": "CheckpointPromoted",
      "timestamp": "2026-05-26T10:10:00Z",
      "objects": [
        {
          "id": "obj_promotion_decision_1",
          "type": "PromotionDecision",
          "qualifier": "--decides-->"
        },
        {
          "id": "obj_cp_verify",
          "type": "GALLCheckpoint"
        }
      ],
      "attributes": {}
    },
    {
      "id": "ev_checkpoint_refused",
      "activity": "CheckpointRefused",
      "timestamp": "2026-05-26T10:15:00Z",
      "objects": [
        {
          "id": "obj_promotion_decision_1",
          "type": "PromotionDecision",
          "qualifier": "--decides-->"
        },
        {
          "id": "obj_cp_verify",
          "type": "GALLCheckpoint"
        }
      ],
      "attributes": {}
    },
```
- **Observation A**: The checkpoint `obj_cp_verify` is associated with both a `CheckpointPromoted` event AND a `CheckpointRefused` event. This is a direct decision contradiction.
- **Observation B**: `ev_test_failed` occurs at `09:35:00Z`, following `ev_test_passed` at `09:30:00Z`. The promotion event `ev_checkpoint_promoted` occurs at `10:10:00Z` without any subsequent `TestPassed` remediation event in the log. This represents a promotion despite an active failure state contradiction.

---

## 2. Logic Chain

### 2.1 Contradiction Detection Rationale
To ensure the logical consistency of checkpoint states, `12_detect_contradictions.sh` must scan the log for multiple classes of logical anomalies using deterministic `jq` filters:
1. **Conflicting Decisions**: Intersecting the set of promoted checkpoints with refused checkpoints must yield an empty set.
2. **Missing Evaluations**: For every adjudication event (promotion/refusal), there must exist an evaluation event referencing the same checkpoint that occurred chronologically before.
3. **Missing Decisions**: A checkpoint that has been evaluated must eventually receive an adjudication decision.
4. **Declared but never Evaluated**: A checkpoint defined in the objects section must have at least one evaluation event.
5. **Promotion despite Failure**: A checkpoint cannot be promoted if the latest test event preceding the promotion is a `TestFailed` event.
6. **Redundant Adjudications**: A checkpoint must not have multiple promotion events.
7. **Adjudication Object ID Reuse**: A single `PromotionDecision` object ID must not be reused across multiple distinct checkpoints.

### 2.2 Adjudication Flow Security
To prevent bypass or mutation of the verification pipeline (per R1 of External Observer Script Ring), the final adjudicator `13_adjudicate_gall_promotion.sh` must:
1. Validate script integrity using SHA-256 hashes of all scripts `00` through `12` against a secure manifest (`manifest.sha256`).
2. Validate source/config file integrity (`self_audit.rs`, `coverage.rs`, `verify_audit.rs`).
3. Execute validation scripts sequentially. If any script fails, halt the pipeline immediately.
4. Run `12_detect_contradictions.sh`. If contradictions are found, reject the promotion.
5. Compute a BLAKE3 receipt over the resulting adjudication metadata and emit the signed audit report to `crates/ggen-graph/audit/vision2030.external_adjudication.json`.

---

## 3. Caveats
- **Verification Environment**: The script designs assume that `jq` and `b3sum` are installed and available in the system PATH. Our environment checks verified that `jq 1.7.1` and `b3sum 1.8.5` are installed on the local system.
- **Manifest Updates**: Since the scripts `00` through `12` do not exist yet, their SHA-256 hashes are not permanently fixed. The implementation team must generate the initial hashes using `sha256sum` or equivalent tools once the scripts are finalized, and populate `manifest.sha256`.

---

## 4. Conclusion & Technical Design

### 4.1 Contradiction Detector (`12_detect_contradictions.sh`)
Below is the complete, production-ready bash script design for `12_detect_contradictions.sh`.

```bash
#!/usr/bin/env bash
# Compliance script to detect event log contradictions and missing evaluations.
# Path: scripts/gall/external/12_detect_contradictions.sh
# Exit code: 0 if no contradictions, 1 if contradictions found.

set -euo pipefail

OCEL_FILE="${1:-crates/ggen-graph/audit/vision2030.self_audit.ocel.json}"

echo "=== Running Contradiction & Missing Evaluation Scanner ==="
echo "Target OCEL file: $OCEL_FILE"

if [ ! -f "$OCEL_FILE" ]; then
    echo "FAIL: OCEL self-audit log file not found at: $OCEL_FILE"
    exit 1
fi

VIOLATIONS=0

# Helper to run a jq check and increment violations if it outputs errors
run_check() {
    local check_name="$1"
    local jq_query="$2"
    echo -n "Running $check_name... "
    
    local results
    results=$(jq -r "$jq_query" "$OCEL_FILE")
    
    if [ -n "$results" ]; then
        echo "FAIL"
        echo "$results"
        local count
        count=$(echo "$results" | grep -c "^FAIL" || true)
        if [ "$count" -eq 0 ]; then
            count=1
        fi
        VIOLATIONS=$((VIOLATIONS + count))
    else
        echo "PASS"
    fi
}

# Check 1: Conflicting Decisions
run_check "Check 1: Conflicting Decisions" '
  [ .events[] | select(.activity == "CheckpointPromoted") | .objects[] | select(.type == "GALLCheckpoint") | .id ] as $promoted
  | [ .events[] | select(.activity == "CheckpointRefused") | .objects[] | select(.type == "GALLCheckpoint") | .id ] as $refused
  | ($promoted | unique) as $u_promoted
  | ($refused | unique) as $u_refused
  | ($u_promoted | map(select(. as $x | $u_refused | index($x) != null)))
  | if length > 0 then "FAIL: Checkpoint " + join(", ") + " has both CheckpointPromoted and CheckpointRefused events" else empty end
'

# Check 2: Decision without Evaluation
run_check "Check 2: Decision without Evaluation" '
  [ .events[] | select(.activity == "CheckpointPromoted" or .activity == "CheckpointRefused") ] as $adjs
  | [ .events[] | select(.activity == "CheckpointEvaluated") ] as $evals
  | $adjs[] | . as $adj
  | ($adj.objects[] | select(.type == "GALLCheckpoint") | .id) as $cp_id
  | [ $evals[] | select(.objects[] | select(.type == "GALLCheckpoint" and .id == $cp_id)) ] as $cp_evals
  | if ($cp_evals | length) == 0 then
      "FAIL: Checkpoint \($cp_id) adjudicated in event \($adj.id) but has no corresponding CheckpointEvaluated event"
    else
      [ $cp_evals[] | select(.timestamp < $adj.timestamp) ] as $prior_evals
      | if ($prior_evals | length) == 0 then
          "FAIL: Checkpoint \($cp_id) adjudicated in event \($adj.id) at \($adj.timestamp) but has no chronologically preceding CheckpointEvaluated event"
        else empty
        end
    end
'

# Check 3: Evaluation without Adjudication
run_check "Check 3: Evaluation without Adjudication" '
  [ .events[] | select(.activity == "CheckpointEvaluated") | .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $evaluated
  | [ .events[] | select(.activity == "CheckpointPromoted" or .activity == "CheckpointRefused") | .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $adjudicated
  | ($evaluated | map(select(. as $x | $adjudicated | index($x) == null)))
  | if length > 0 then "FAIL: Checkpoint " + join(", ") + " was evaluated but has no promotion or refusal decision" else empty end
'

# Check 4: Declared but never Evaluated
run_check "Check 4: Declared but never Evaluated" '
  [ .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $declared
  | [ .events[] | select(.activity == "CheckpointEvaluated") | .objects[] | select(.type == "GALLCheckpoint") | .id ] | unique as $evaluated
  | ($declared | map(select(. as $x | $evaluated | index($x) == null)))
  | if length > 0 then "FAIL: Declared checkpoint " + join(", ") + " has never been evaluated in the event log" else empty end
'

# Check 5: Promotion despite un-remediated failure
run_check "Check 5: Promotion despite un-remediated failure" '
  . as $root
  | $root.events[] | select(.activity == "CheckpointPromoted") | . as $promo
  | [ $root.events[] | select(.activity == "TestPassed" and .timestamp < $promo.timestamp) ] | last as $last_pass
  | [ $root.events[] | select(.activity == "TestFailed" and .timestamp < $promo.timestamp) ] | last as $last_fail
  | if ($last_fail != null) and ($last_pass == null or $last_fail.timestamp > $last_pass.timestamp) then
      "FAIL: Checkpoint promoted in event \($promo.id) despite preceding un-remediated test failure in event \($last_fail.id) at \($last_fail.timestamp)"
    else empty end
'

# Check 6: Redundant Decisions
run_check "Check 6: Redundant Decisions" '
  [ .events[] | select(.activity == "CheckpointPromoted") | .objects[] | select(.type == "GALLCheckpoint") | .id ]
  | group_by(.) | map(select(length > 1)) | map(.[0])
  | if length > 0 then "FAIL: Multiple promotion decision events found for checkpoints: " + join(", ") else empty end
'

# Check 7: PromotionDecision ID reuse
run_check "Check 7: PromotionDecision ID reuse" '
  [ .events[] | select(.activity == "CheckpointPromoted" or .activity == "CheckpointRefused") | {decision: (.objects[] | select(.type == "PromotionDecision").id), checkpoint: (.objects[] | select(.type == "GALLCheckpoint").id)} ]
  | group_by(.decision) | .[] | select(map(.checkpoint) | unique | length > 1)
  | "FAIL: PromotionDecision ID " + .[0].decision + " is reused across multiple checkpoints: " + ([.[].checkpoint] | unique | join(", "))
'

echo "=== Contradiction Scan Summary ==="
if [ "$VIOLATIONS" -eq 0 ]; then
    echo "PASS: Zero contradictions detected in OCEL log."
    exit 0
else
    echo "FAIL: $VIOLATIONS contradiction(s) found in OCEL log."
    exit 1
fi
```

### 4.2 Promotion Adjudication Flow (`13_adjudicate_gall_promotion.sh`)
This script executes the sequential validation ring and computes the BLAKE3 cryptographic verification receipt.

```bash
#!/usr/bin/env bash
# Sequentially executes verification scripts, checks script integrity, and emits promotion results.
# Path: scripts/gall/external/13_adjudicate_gall_promotion.sh
# Exit code: 0 if approved and promoted, 1 if verification fails or rejected.

set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

MANIFEST_FILE="scripts/gall/external/manifest.sha256"
OCEL_FILE="crates/ggen-graph/audit/vision2030.self_audit.ocel.json"
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
    b3sum "$file_path" | awk '{print $1}'
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
    expected_hash=$(echo "$expected_hash" | tr -d '\r')
    filepath=$(echo "$filepath" | tr -d '\r')
    
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
    "scripts/gall/external/09_verify_ocel_self_audit.sh"
    "scripts/gall/external/10_verify_coverage_matrix.sh"
    "scripts/gall/external/11_verify_proof_report.sh"
    "scripts/gall/external/12_detect_contradictions.sh"
)

ADJUDICATION_STATUS="Promoted"
ADJUDICATION_REASON="All 13 validation scripts passed successfully and zero contradictions were detected."
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
    "crates/ggen-graph/src/bin/verify_audit.rs"
)
SOURCE_FILES_JSON="[]"
for src in "${SRC_FILES[@]}"; do
    src_hash=$(compute_sha256 "$src")
    SOURCE_FILES_JSON=$(echo "$SOURCE_FILES_JSON" | jq --arg path "$src" --arg sha "$src_hash" '. + [{"path": $path, "sha256": $sha}]')
done

# 4. Generate Contradiction Section Info
CONTRADICTION_STATUS="PASS"
VIOLATIONS_COUNT=0
VIOLATIONS_LIST="[]"

if [ -f "$OCEL_FILE" ]; then
    set +e
    CONTRADICTION_OUT=$(./scripts/gall/external/12_detect_contradictions.sh 2>&1)
    CONTRADICTION_CODE=$?
    set -e
    if [ "$CONTRADICTION_CODE" -ne 0 ]; then
        CONTRADICTION_STATUS="FAIL"
        ADJUDICATION_STATUS="Refused"
        # Parse JQ error lines from scanner output
        VIOLATIONS_COUNT=$(echo "$CONTRADICTION_OUT" | grep -c "FAIL: " || true)
        VIOLATIONS_LIST=$(echo "$CONTRADICTION_OUT" | grep "FAIL: " | jq -R . | jq -s .)
        ADJUDICATION_REASON="Adjudication refused due to logical contradictions in self-audit log."
    fi
fi

CONTRADICTIONS_JSON=$(jq -n \
  --arg status "$CONTRADICTION_STATUS" \
  --argjson count "$VIOLATIONS_COUNT" \
  --argjson list "$VIOLATIONS_LIST" \
  '{"status": $status, "violations_count": $count, "violations": $list}')

# 5. Extract self-audit and coverage metadata
SELF_AUDIT_BLAKE3="null"
if [ -f "$OCEL_FILE" ]; then
    SELF_AUDIT_BLAKE3="\"$(compute_blake3 "$OCEL_FILE")\""
fi
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
  "self_audit_digest": {
    "path": "$OCEL_FILE",
    "blake3": $SELF_AUDIT_BLAKE3
  },
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
```

### 4.3 `vision2030.external_adjudication.json` Output Schema
The schema layout emitted to `crates/ggen-graph/audit/vision2030.external_adjudication.json` guarantees multi-surface accountability by linking exit status, script digests, file digests, and the final signature hash:

```json
{
  "timestamp": "2026-05-26T23:47:00Z",
  "verdict": "Promoted",
  "reason": "All 13 validation scripts passed successfully and zero contradictions were detected.",
  "scripts_verified": [
    {
      "path": "scripts/gall/external/00_capture_baseline.sh",
      "sha256": "8f3cfb06e8b8398e09e13dcd9658b1a37c98b688c6bb8cf03a3d5e236e785e13",
      "exit_code": 0,
      "status": "PASS",
      "duration_seconds": 1
    }
  ],
  "source_files_verified": [
    {
      "path": "crates/ggen-graph/src/ocel/self_audit.rs",
      "sha256": "4b92d8e488f4c1e09658e3881a3d5e23cbb8cf03ffdcf82309e7116744f2ef08a"
    }
  ],
  "contradiction_check": {
    "status": "PASS",
    "violations_count": 0,
    "violations": []
  },
  "self_audit_digest": {
    "path": "crates/ggen-graph/audit/vision2030.self_audit.ocel.json",
    "blake3": "cb104f7c8b8398e09e13dcd9658b1a3cbb8cf03ffdcf82309e7116744f2ef08eb"
  },
  "coverage_digest": {
    "path": "crates/ggen-graph/audit/vision2030.coverage.json",
    "blake3": "fe92e7c8b8398e09e13dcd9658b1a3cbb8cf03ffdcf82309e7116744f2ef08eb"
  },
  "adjudication_blake3_receipt": "eb3a28b0f4e1f72da89f2cf0f95b9d3bb8cf03ffdcf82309e7116744f2ef08eb"
}
```

### 4.4 Proposed Updates to `docs/VISION_2030_GALL_PROOF.md`
To align the promotion proof report with the External Observer Doctrine, the promotion decision section must be rewritten to establish strict dependency on the external verifier scripts' adjudication results.

#### Before (Lines 17–32):
```markdown
## 2. Checkpoint Promotion Decision

The checkpoint promotion decision is derived directly from the self-audit log as follows:

1. **Self-Audit Log Evidence**: 
   - An event with ID `ev_checkpoint_promoted` is recorded in `vision2030.self_audit.ocel.json` with the activity `CheckpointPromoted` at timestamp `2026-05-26T10:10:00Z`.
...
Based on this verified evidence, the final promotion decision is formally declared as:
$$\text{Decision} = \mathbf{PROMOTED}$$
```

#### After (Strict Dependency Declaration):
```markdown
## 2. Checkpoint Promotion Decision

The checkpoint promotion decision is strictly declared as dependent on the External Observer Script Ring's adjudication results. The Rust package and local agent are prohibited from self-verifying; instead, verification must be independently audited, hashed, and validated by the scripts `00_` through `13_` under `scripts/gall/external/`.

The final promotion decision is derived directly from the external adjudication log `crates/ggen-graph/audit/vision2030.external_adjudication.json` as follows:

1. **External Observer Adjudication Receipt**:
   - The file `vision2030.external_adjudication.json` contains a valid cryptographic `adjudication_blake3_receipt` proving execution integrity of the entire verification ring.
   - The field `verdict` is set to `"Promoted"`, indicating that all 13 external validation scripts passed with exit status `0`.
2. **Script Ring Integrity (No Bypass/Mutation)**:
   - All 13 verifier script digests were checked against `manifest.sha256` and verified to be un-mutated.
3. **Contradiction Verification**:
   - The contradiction check status in the external adjudication log is `"PASS"`, proving that the self-audit event log contains zero conflicting checkpoint decisions, zero chronological anomalies, and zero un-remediated failure states.

Based on this independent, externally adjudicated proof, the final promotion decision is formally declared as:
$$\text{Decision} = \text{ExternalAdjudication.verdict} \implies \mathbf{PROMOTED}$$
```

---

## 5. Verification Method

To verify the contradiction detection and adjudication design:
1. **Self-Audit Contradiction Validation**:
   - Copy the proposed `12_detect_contradictions.sh` script to `scripts/gall/external/12_detect_contradictions.sh` and make it executable.
   - Run the script: `bash scripts/gall/external/12_detect_contradictions.sh`.
   - The command should exit with status `1` and print the exact failures (Checkpoint double decisions, test failure remediation gap) due to the current state of `vision2030.self_audit.ocel.json`. This demonstrates that the contradictions are successfully caught.
2. **Adjudication Execution Validation**:
   - Generate the initial `manifest.sha256` containing current SHA-256 hashes of the files.
   - Run `bash scripts/gall/external/13_adjudicate_gall_promotion.sh`.
   - It will abort due to the contradiction failures from script `12_detect_contradictions.sh`.
   - Correct the `self_audit.rs` file so that only one promotion decision is emitted, and ensure the test passed event occurs after any test failures. Regenerate the audit log with `cargo run -p ggen-graph --bin emit_audit`.
   - Rerun `13_adjudicate_gall_promotion.sh`. It must exit with status `0` and write the valid JSON log containing the `adjudication_blake3_receipt` to `crates/ggen-graph/audit/vision2030.external_adjudication.json`.
