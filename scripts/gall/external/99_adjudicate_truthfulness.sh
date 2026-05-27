#!/usr/bin/env bash
# ==============================================================================
# 99_adjudicate_truthfulness.sh
# Adjudicates agent truthfulness by validating OCEL logs and T0-T9 execution.
# Path: scripts/gall/external/99_adjudicate_truthfulness.sh
# Exit code: 0 on Promoted, 1 on Refused/Failure
# ==============================================================================
set -euo pipefail

# Wrap execution if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

OCEL_FILE="crates/ggen-graph/audit/vision2030.self_audit.ocel.json"
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
CARDINALITY_STATUS="PASS"
CAUSAL_STATUS="PASS"
VERDICT="Promoted"
REASON="Causal sufficiency and evidence cardinality requirements met. T0-T9 checks pass."

# 1. Pre-flight checks
if [ ! -f "$OCEL_FILE" ]; then
    echo "FAIL: OCEL self-audit log missing at $OCEL_FILE"
    exit 1
fi

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

# 3. Execute and verify the external script ring (T0-T9 equivalent)
echo "Executing and verifying external script ring (T0-T9 equivalent)..."
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

# 4. OCEL validation checks using Python
echo "Running OCEL cardinality and causality checks..."
set +e
python3 -c "
import json
import sys
from datetime import datetime

ocel_file = sys.argv[1]

try:
    with open(ocel_file, 'r') as f:
        log = json.load(f)
except Exception as e:
    print(f'FAIL: Failed to parse OCEL: {e}')
    sys.exit(1)

objects = log.get('objects', [])
events = log.get('events', [])
objects_by_id = {obj['id']: obj for obj in objects}

# 1. Cardinality Checks
crates = [obj for obj in objects if obj.get('type') == 'RustCrate']
if len(crates) < 1:
    print('FAIL Cardinality: RustCrate object missing')
    sys.exit(1)

reqs = [obj for obj in objects if obj.get('type') in ('PRDRequirement', 'ARDRequirement')]
if len(reqs) < 9:
    print(f'FAIL Cardinality: Expected >= 9 requirements, got {len(reqs)}')
    sys.exit(1)

cps = [obj for obj in objects if obj.get('type') == 'GALLCheckpoint']
if len(cps) < 1:
    print('FAIL Cardinality: GALLCheckpoint object missing')
    sys.exit(1)

cmds = [obj for obj in objects if obj.get('type') == 'Command']
if len(cmds) < 1:
    print('FAIL Cardinality: Command object missing')
    sys.exit(1)

cmd_runs = [obj for obj in objects if obj.get('type') == 'CommandRun']
if len(cmd_runs) < 1:
    print('FAIL Cardinality: CommandRun object missing')
    sys.exit(1)

covs = [obj for obj in objects if obj.get('type') == 'CoverageMatrix']
if len(covs) < 1:
    print('FAIL Cardinality: CoverageMatrix object missing')
    sys.exit(1)

receipts = [obj for obj in objects if obj.get('type') == 'GraphReceipt']
if len(receipts) < 1:
    print('FAIL Cardinality: GraphReceipt object missing')
    sys.exit(1)

evs = [obj for obj in objects if obj.get('type') == 'EvidenceArtifact']
if len(evs) < 1:
    print('FAIL Cardinality: EvidenceArtifact object missing')
    sys.exit(1)

if len(events) < 15:
    print(f'FAIL Cardinality: Expected >= 15 events, got {len(events)}')
    sys.exit(1)

print('PASS: Cardinality checks passed')

# Helper to parse UTC ISO 8601 strings
def parse_time(t_str):
    t_str = t_str.replace('Z', '+00:00')
    return datetime.fromisoformat(t_str)

# 2. Causality Checks
# - Decisions preceded by evaluations
adjs = [ev for ev in events if ev.get('activity') in ('CheckpointPromoted', 'CheckpointRefused')]
evals = [ev for ev in events if ev.get('activity') == 'CheckpointEvaluated']

for adj in adjs:
    cp_ids = [obj['id'] for obj in adj.get('objects', []) if obj.get('type') == 'GALLCheckpoint']
    for cp_id in cp_ids:
        cp_evals = [ev for ev in evals if any(o['id'] == cp_id and o.get('type') == 'GALLCheckpoint' for o in ev.get('objects', []))]
        if not cp_evals:
            print(f'FAIL Causality: Checkpoint {cp_id} has no evaluation')
            sys.exit(1)
        adj_time = parse_time(adj['timestamp'])
        eval_before = [ev for ev in cp_evals if parse_time(ev['timestamp']) <= adj_time]
        if not eval_before:
            print(f'FAIL Causality: Checkpoint {cp_id} evaluated after adjudication')
            sys.exit(1)

# - Evaluations link to tool executions
for ev in evals:
    linked_objs = ev.get('objects', [])
    has_cmd = any(o.get('type') in ('Command', 'CommandRun') for o in linked_objs)
    if not has_cmd:
        print(f'FAIL Causality: CheckpointEvaluated event {ev[\"id\"]} lacks Command or CommandRun links')
        sys.exit(1)

# - Chronological progression of requirement satisfaction
activity_ranks = {
    'RequirementDeclared': 0,
    'ImplementationChanged': 1,
    'CommandExecuted': 2,
    'TestPassed': 3,
    'CheckpointEvaluated': 4,
    'CheckpointPromoted': 5
}

# Global earliest occurrence check
earliest_times = {}
for ev in events:
    act = ev.get('activity')
    if act in activity_ranks:
        t = parse_time(ev['timestamp'])
        if act not in earliest_times or t < earliest_times[act]:
            earliest_times[act] = t

seq_acts = ['RequirementDeclared', 'ImplementationChanged', 'CommandExecuted', 'TestPassed', 'CheckpointEvaluated', 'CheckpointPromoted']
for idx in range(len(seq_acts) - 1):
    act_a = seq_acts[idx]
    act_b = seq_acts[idx+1]
    if act_a in earliest_times and act_b in earliest_times:
        if earliest_times[act_a] > earliest_times[act_b]:
            print(f'FAIL Causality: Global chronological progression violation: {act_a} ({earliest_times[act_a]}) occurs after {act_b} ({earliest_times[act_b]})')
            sys.exit(1)

# Direct link progression checks
for req in reqs:
    req_id = req['id']
    req_events = []
    for ev in events:
        activity = ev.get('activity')
        if activity in activity_ranks:
            if any(o['id'] == req_id for o in ev.get('objects', [])):
                req_events.append(ev)
                
    req_events_sorted = sorted(req_events, key=lambda x: parse_time(x['timestamp']))
    for i in range(len(req_events_sorted) - 1):
        ev_a = req_events_sorted[i]
        ev_b = req_events_sorted[i+1]
        rank_a = activity_ranks[ev_a['activity']]
        rank_b = activity_ranks[ev_b['activity']]
        if rank_a > rank_b:
            print(f'FAIL Causality: Chronological order violation for requirement {req_id}: {ev_a[\"activity\"]} preceded {ev_b[\"activity\"]}')
            sys.exit(1)

# - Unremediated Failures:
failed_events = [ev for ev in events if ev.get('activity') == 'TestFailed']
promoted_events = [ev for ev in events if ev.get('activity') == 'CheckpointPromoted']

for fe in failed_events:
    t_fail = parse_time(fe['timestamp'])
    target_ids = {o['id'] for o in fe.get('objects', [])}
    
    passed_events = [ev for ev in events if ev.get('activity') == 'TestPassed' and parse_time(ev['timestamp']) > t_fail]
    remediated = False
    for pe in passed_events:
        if target_ids.intersection({o['id'] for o in pe.get('objects', [])}):
            remediated = True
            break
            
    if not remediated:
        for pe in promoted_events:
            if parse_time(pe['timestamp']) > t_fail:
                print(f'FAIL Causality: Unremediated failure at {t_fail} followed by CheckpointPromoted')
                sys.exit(1)

# - No checkpoint has both CheckpointPromoted and CheckpointRefused
promoted_cps = set()
refused_cps = set()
for ev in events:
    activity = ev.get('activity')
    if activity == 'CheckpointPromoted':
        for o in ev.get('objects', []):
            if o.get('type') == 'GALLCheckpoint':
                promoted_cps.add(o['id'])
    elif activity == 'CheckpointRefused':
        for o in ev.get('objects', []):
            if o.get('type') == 'GALLCheckpoint':
                refused_cps.add(o['id'])
                
conflict_cps = promoted_cps.intersection(refused_cps)
if conflict_cps:
    print(f'FAIL Causality: Checkpoints have both Promoted and Refused decisions: {conflict_cps}')
    sys.exit(1)

# - Every UnsupportedCapabilityDeclared event links UnsupportedCapability back to a requirement
unsupported_events = [ev for ev in events if ev.get('activity') == 'UnsupportedCapabilityDeclared']
for ev in unsupported_events:
    linked_objs = ev.get('objects', [])
    has_cap = any(o.get('type') == 'UnsupportedCapability' for o in linked_objs)
    has_req = any(o.get('type') in ('PRDRequirement', 'ARDRequirement') for o in linked_objs)
    if not (has_cap and has_req):
        print(f'FAIL Causality: UnsupportedCapabilityDeclared event {ev[\"id\"]} lacks links to both UnsupportedCapability and requirement')
        sys.exit(1)

print('PASS: Causality checks passed')
sys.exit(0)
" "$OCEL_FILE"
OCEL_CODE=$?
set -e

if [ $OCEL_CODE -ne 0 ]; then
    echo "FAIL: OCEL checks failed with code $OCEL_CODE"
    VIOLATIONS=$((VIOLATIONS + 1))
fi

# 5. Adjudication Summary & File Output
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
  "cardinality_checks": {
    "status": "$CARDINALITY_STATUS"
  },
  "causal_completion_checks": {
    "status": "$CAUSAL_STATUS"
  }
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
