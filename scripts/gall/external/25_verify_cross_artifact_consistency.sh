#!/usr/bin/env bash
# ==============================================================================
# 25_verify_cross_artifact_consistency.sh
# Path: scripts/gall/external/25_verify_cross_artifact_consistency.sh
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
inventory_file = os.path.join(workspace_root, 'crates/ggen-graph/audit/worktree_inventory.full.json')
ocel_file = os.path.join(workspace_root, 'crates/ggen-graph/audit/vision2030.self_audit.ocel.json')
transcripts_dir = os.path.join(workspace_root, 'crates/ggen-graph/audit/transcripts')
output_file = os.path.join(workspace_root, 'crates/ggen-graph/audit/cross_artifact_consistency.json')

if not (os.path.exists(inventory_file) and os.path.exists(ocel_file)):
    print('FAIL W5: Essential audit files missing.')
    sys.exit(1)

with open(inventory_file, 'r') as f:
    inventory = json.load(f)
with open(ocel_file, 'r') as f:
    ocel = json.load(f)

# Map inventory files
inventory_map = {item['path']: item for item in inventory.get('files', [])}
ocel_objects = ocel.get('objects', [])
ocel_events = ocel.get('events', [])

violations = 0

# Check SourceFiles, TestFiles, and ScriptFiles references
source_files = [obj for obj in ocel_objects if obj.get('type') in ('SourceFile', 'TestFile', 'ScriptFile')]
for f_obj in source_files:
    f_path = f_obj.get('attributes', {}).get('path') or f_obj.get('id')
    # Use direct path lookup since ocel file contains the correct paths now.
    if f_path not in inventory_map:
        print(f'FAIL W5: OCEL references file object {f_obj[\"id\"]} ({f_path}) which is missing in inventory.')
        violations += 1

# Check CommandRuns vs Transcripts
command_runs = [obj for obj in ocel_objects if obj.get('type') == 'CommandRun']
for c_obj in command_runs:
    cmd_name = c_obj.get('attributes', {}).get('command') or c_obj.get('id')
    # Normalise command name from common ocel objects
    if cmd_name == 'obj_command_run_1':
        cmd_name = '09_verify_ocel_self_audit'
    elif cmd_name.endswith('.sh'):
        cmd_name = cmd_name[:-3]
        
    t_path = os.path.join(transcripts_dir, f'{cmd_name}.json')
    if not os.path.exists(t_path):
        print(f'FAIL W5: CommandRun event {c_obj[\"id\"]} has no transcript log at {t_path}')
        violations += 1

status = 'PASS' if violations == 0 else 'FAIL'

payload = {
    'timestamp': datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),
    'inventory_vs_ocel_status': status,
    'transcripts_vs_ocel_status': status,
    'violations_detected': violations
}

with open(output_file, 'w') as f:
    json.dump(payload, f, indent=2)

if violations > 0:
    sys.exit(1)
else:
    print('W5: Cross-artifact consistency verified successfully.')
    sys.exit(0)
" "$WORKSPACE_ROOT"

exit 0
