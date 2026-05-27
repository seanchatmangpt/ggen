#!/usr/bin/env bash
# ==============================================================================
# 26_verify_ocel_causal_sufficiency.sh
# Path: scripts/gall/external/26_verify_ocel_causal_sufficiency.sh
# ==============================================================================
set -euo pipefail

if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"

python3 -c "
import os
import sys
import json
from datetime import datetime, timezone

workspace_root = sys.argv[1]
ocel_file = os.path.join(workspace_root, 'crates/ggen-graph/audit/vision2030.self_audit.ocel.json')
output_file = os.path.join(workspace_root, 'crates/ggen-graph/audit/ocel_causal_sufficiency.json')

if not os.path.exists(ocel_file):
    print('FAIL W6: OCEL self-audit log file missing.')
    sys.exit(1)

with open(ocel_file, 'r') as f:
    log = json.load(f)

objects = log.get('objects', [])
events = log.get('events', [])
violations = 0

# 1. Cardinality checks
crates = [o for o in objects if o.get('type') == 'RustCrate']
reqs = [o for o in objects if o.get('type') in ('PRDRequirement', 'ARDRequirement')]
checkpoints = [o for o in objects if o.get('type') == 'GALLCheckpoint']
commands = [o for o in objects if o.get('type') == 'Command']

cardinality_ok = (len(crates) >= 1) and (len(reqs) >= 9) and (len(checkpoints) >= 1) and (len(commands) >= 1) and (len(events) >= 15)
if not cardinality_ok:
    print(f'FAIL W6: Cardinality check failed. Crates: {len(crates)}, Reqs: {len(reqs)}, Events: {len(events)}')
    violations += 1

# 2. Causality checks (Chronology of events)
def parse_time(t_str):
    return datetime.fromisoformat(t_str.replace('Z', '+00:00'))

evals = [e for e in events if e.get('activity') == 'CheckpointEvaluated']
proms = [e for e in events if e.get('activity') == 'CheckpointPromoted']

# decisions preceded by evaluations
for pr in proms:
    pr_time = parse_time(pr['timestamp'])
    cp_ids = {o['id'] for o in pr.get('objects', []) if o.get('type') == 'GALLCheckpoint'}
    
    # Check that there is a preceding CheckpointEvaluated event for the same Checkpoint ID
    has_preceding_eval = False
    for ev in evals:
        ev_time = parse_time(ev['timestamp'])
        ev_cp_ids = {o['id'] for o in ev.get('objects', []) if o.get('type') == 'GALLCheckpoint'}
        if cp_ids.intersection(ev_cp_ids) and ev_time <= pr_time:
            has_preceding_eval = True
            break
    if not has_preceding_eval:
        print(f'FAIL W6: CheckpointPromoted event occurs without preceding CheckpointEvaluated.')
        violations += 1

# sequential progression of requirement
activity_ranks = {
    'RequirementDeclared': 0,
    'ImplementationChanged': 1,
    'CommandExecuted': 2,
    'TestPassed': 3,
    'CheckpointEvaluated': 4,
    'CheckpointPromoted': 5
}
earliest_times = {}
for e in events:
    act = e.get('activity')
    if act in activity_ranks:
        t = parse_time(e['timestamp'])
        if act not in earliest_times or t < earliest_times[act]:
            earliest_times[act] = t

for a1, r1 in activity_ranks.items():
    for a2, r2 in activity_ranks.items():
        if r1 < r2 and a1 in earliest_times and a2 in earliest_times:
            if earliest_times[a1] > earliest_times[a2]:
                print(f'FAIL W6: Chronological sequence violation: {a1} occurred after {a2}.')
                violations += 1

status = 'PASS' if violations == 0 else 'FAIL'

payload = {
    'timestamp': datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),
    'cardinality_checks_passed': cardinality_ok,
    'causal_chronology_passed': (violations == 0),
    'status': status
}

with open(output_file, 'w') as f:
    json.dump(payload, f, indent=2)

if violations > 0:
    sys.exit(1)
else:
    print('W6: OCEL causal sufficiency verified successfully.')
    sys.exit(0)
" "$WORKSPACE_ROOT"

exit 0
