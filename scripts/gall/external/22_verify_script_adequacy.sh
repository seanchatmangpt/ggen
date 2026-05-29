#!/usr/bin/env bash
# ==============================================================================
# 22_verify_script_adequacy.sh
# Path: scripts/gall/external/22_verify_script_adequacy.sh
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
scripts_dir = os.path.join(workspace_root, 'scripts/gall/external')
output_file = os.path.join(workspace_root, 'crates/ggen-graph/audit/script_adequacy.json')

scripts_to_check = [
    '00_capture_baseline.sh',
    '01_extract_requirements.sh',
    '02_verify_package_constraints.sh',
    '03_check_feature_flags.sh',
    '04_run_unit_tests.sh',
    '05_run_integration_tests.sh',
    '06_scan_forbidden_surfaces.sh',
    '07_check_anti_fake.sh',
    '08_verify_replay_receipts.sh',
    '09_verify_ocel_self_audit.sh',
    '10_verify_coverage_matrix.sh',
    '11_verify_proof_report.sh',
    '12_detect_contradictions.sh',
    '13_adjudicate_gall_promotion.sh',
    '20_capture_full_worktree_inventory.sh',
    '21_verify_command_transcripts.sh',
    '24_run_clean_room_rebuild.sh',
    '25_verify_cross_artifact_consistency.sh',
    '26_verify_ocel_causal_sufficiency.sh',
    '27_verify_contradiction_supersession.sh',
    '99_adjudicate_witnessed_truthfulness.sh'
]

results = []
adequacy_failed = 0

for s_name in scripts_to_check:
    path = os.path.join(scripts_dir, s_name)
    if not os.path.exists(path):
        results.append({
            'path': f'scripts/gall/external/{s_name}',
            'status': 'FAIL',
            'reason': 'Script file missing'
        })
        adequacy_failed += 1
        continue
        
    with open(path, 'r') as f:
        content = f.read()
        
    # Structural adequacy criteria
    has_safety = ('set -e' in content) or ('set -euo pipefail' in content)
    has_wrapper = 'run_with_transcript.sh' in content
    no_early_bypass = not (content.startswith('#!/usr/bin/env bash\nexit 0') or content.startswith('#!/bin/bash\nexit 0'))
    
    # Identify if safety or wrappers are bypassed
    is_adequate = has_safety and has_wrapper and no_early_bypass
    
    status = 'PASS' if is_adequate else 'FAIL'
    reason = []
    if not has_safety: reason.append('missing set -e / safety shell options')
    if not has_wrapper: reason.append('missing run_with_transcript.sh wrapping')
    if not no_early_bypass: reason.append('contains immediate exit success bypass')
    
    if status == 'FAIL':
        adequacy_failed += 1
        
    results.append({
        'path': f'scripts/gall/external/{s_name}',
        'has_safety_flags': has_safety,
        'uses_wrapper': has_wrapper,
        'no_early_bypass': no_early_bypass,
        'status': status,
        'reason': '; '.join(reason) if reason else 'structural constraints met'
    })

payload = {
    'timestamp': datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),
    'adequate_scripts_count': len(scripts_to_check) - adequacy_failed,
    'total_scripts_count': len(scripts_to_check),
    'scripts': results
}

with open(output_file, 'w') as f:
    json.dump(payload, f, indent=2)

if adequacy_failed > 0:
    print(f'W2: Script adequacy check failed with {adequacy_failed} script violations.')
    sys.exit(1)
else:
    print('W2: All scripts verified to be structurally adequate.')
    sys.exit(0)
" "$WORKSPACE_ROOT"

exit 0
