#!/usr/bin/env bash
# ==============================================================================
# 21_verify_command_transcripts.sh
# Path: scripts/gall/external/21_verify_command_transcripts.sh
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
import hashlib

workspace_root = sys.argv[1]
transcripts_dir = os.path.join(workspace_root, 'crates/ggen-graph/audit/transcripts')

# Complete list of expected script executions
expected_commands = [
    '00_capture_baseline',
    '01_extract_requirements',
    '02_verify_package_constraints',
    '03_check_feature_flags',
    '04_run_unit_tests',
    '05_run_integration_tests',
    '06_scan_forbidden_surfaces',
    '07_check_anti_fake',
    '08_verify_replay_receipts',
    '09_verify_ocel_self_audit',
    '10_verify_coverage_matrix',
    '11_verify_proof_report',
    '12_detect_contradictions',
    '13_adjudicate_gall_promotion',
    '20_capture_full_worktree_inventory',
    # W-series scripts are verified on second-pass adjudication runs;
    # first-pass omits self-referential check of 21_verify_command_transcripts.
    '22_verify_script_adequacy',
    '23_run_sabotage_suite',
    '24_run_clean_room_rebuild',
    '25_verify_cross_artifact_consistency',
    '26_verify_ocel_causal_sufficiency',
    '27_verify_contradiction_supersession',
]

def check_file_sha256(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        h.update(f.read())
    return h.hexdigest()

violations = 0

for cmd in expected_commands:
    json_path = os.path.join(transcripts_dir, f'{cmd}.json')
    stdout_path = os.path.join(transcripts_dir, f'{cmd}.stdout')
    stderr_path = os.path.join(transcripts_dir, f'{cmd}.stderr')
    
    if not (os.path.exists(json_path) and os.path.exists(stdout_path) and os.path.exists(stderr_path)):
        print(f'FAIL W1: Missing transcript files for: {cmd}')
        violations += 1
        continue
        
    try:
        with open(json_path, 'r') as f:
            data = json.load(f)
    except Exception as e:
        print(f'FAIL W1: Invalid JSON for command {cmd}: {e}')
        violations += 1
        continue
        
    # Verify metadata fields
    if not all(k in data for k in ['command', 'argv', 'cwd', 'exit_code', 'duration_ms', 'stdout_sha256', 'stderr_sha256']):
        print(f'FAIL W1: Incomplete fields in transcript JSON for: {cmd}')
        violations += 1
        continue
        
    if data['duration_ms'] <= 0.0:
        print(f'FAIL W1: Invalid (non-positive) duration recorded for: {cmd}')
        violations += 1
        
    # Verify hashes
    out_hash = check_file_sha256(stdout_path)
    err_hash = check_file_sha256(stderr_path)
    
    if out_hash != data['stdout_sha256']:
        print(f'FAIL W1: Stdout content mismatch for {cmd}. Expected {data[\"stdout_sha256\"]}, got {out_hash}')
        violations += 1
        
    if err_hash != data['stderr_sha256']:
        print(f'FAIL W1: Stderr content mismatch for {cmd}. Expected {data[\"stderr_sha256\"]}, got {err_hash}')
        violations += 1

if violations > 0:
    print(f'W1: Completed with {violations} verification failures.')
    sys.exit(1)
else:
    print('W1: All command transcripts verified successfully.')
    sys.exit(0)
" "$WORKSPACE_ROOT"

exit 0
