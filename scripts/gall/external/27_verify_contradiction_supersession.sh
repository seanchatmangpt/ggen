#!/usr/bin/env bash
# ==============================================================================
# 27_verify_contradiction_supersession.sh
# Path: scripts/gall/external/27_verify_contradiction_supersession.sh
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
output_file = os.path.join(workspace_root, 'crates/ggen-graph/audit/contradiction_supersession.json')

if not os.path.exists(ocel_file):
    print('FAIL W7: OCEL self-audit log file missing.')
    sys.exit(1)

with open(ocel_file, 'r') as f:
    log = json.load(f)

events = log.get('events', [])

def parse_time(t_str):
    return datetime.fromisoformat(t_str.replace('Z', '+00:00'))

promotions = {}
refusals = {}

for e in events:
    act = e.get('activity')
    t = parse_time(e['timestamp'])
    cp_ids = [o['id'] for o in e.get('objects', []) if o.get('type') == 'GALLCheckpoint']
    
    for cp in cp_ids:
        if act == 'CheckpointPromoted':
            if cp not in promotions or t > promotions[cp]:
                promotions[cp] = t
        elif act == 'CheckpointRefused':
            if cp not in refusals or t > refusals[cp]:
                refusals[cp] = t

violations = 0
resolutions = []

for cp in set(list(promotions.keys()) + list(refusals.keys())):
    if cp in promotions and cp in refusals:
        # We have a contradiction (both promoted and refused at some point)
        # Default to refused unless promotions occurred strictly after refusals
        if promotions[cp] <= refusals[cp]:
            print(f'FAIL W7: Unresolved contradiction for checkpoint {cp}. Promoted at {promotions[cp]} but Refused later at {refusals[cp]}.')
            violations += 1
        else:
            print(f'W7: Resolved contradiction for checkpoint {cp}. Promoted at {promotions[cp]} after preceding Refused at {refusals[cp]}.')
            resolutions.append({
                'checkpoint': cp,
                'refused_time': refusals[cp].strftime('%Y-%m-%dT%H:%M:%SZ'),
                'promoted_time': promotions[cp].strftime('%Y-%m-%dT%H:%M:%SZ'),
                'status': 'Resolved'
            })

status = 'PASS' if violations == 0 else 'FAIL'

payload = {
    'timestamp': datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),
    'contradiction_violations': violations,
    'superseded_contradictions': resolutions,
    'status': status
}

with open(output_file, 'w') as f:
    json.dump(payload, f, indent=2)

if violations > 0:
    sys.exit(1)
else:
    print('W7: Contradiction supersession constraints satisfied.')
    sys.exit(0)
" "$WORKSPACE_ROOT"

exit 0
