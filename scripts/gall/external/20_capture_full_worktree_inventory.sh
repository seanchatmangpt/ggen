#!/usr/bin/env bash
# ==============================================================================
# 20_capture_full_worktree_inventory.sh
# Generates crates/ggen-graph/audit/worktree_inventory.full.json
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
import hashlib
import json
import subprocess
from datetime import datetime, timezone

workspace_root = sys.argv[1]
files_metadata = []

def compute_sha256(path):
    h = hashlib.sha256()
    with open(path, 'rb') as f:
        for chunk in iter(lambda: f.read(65536), b''):
            h.update(chunk)
    return h.hexdigest()

def get_utc_iso(mtime):
    dt = datetime.fromtimestamp(mtime, timezone.utc)
    return dt.strftime('%Y-%m-%dT%H:%M:%SZ')

# Determine file paths to process (using git ls-files for speed & accuracy)
try:
    git_files = subprocess.check_output(['git', 'ls-files', '-z'], cwd=workspace_root).decode('utf-8').split('\x00')
    git_files = [f for f in git_files if f]
except Exception:
    # Fallback to os.walk if git is not initialized
    git_files = []
    for root, dirs, files in os.walk(workspace_root):
        dirs[:] = [d for d in dirs if d not in ('.git', '.agents', '.gemini', '.antigravitycli', 'target')]
        for file in files:
            full_path = os.path.join(root, file)
            rel_path = os.path.relpath(full_path, workspace_root)
            git_files.append(rel_path.replace(os.sep, '/'))

# Include dynamically generated audit logs that may not be tracked by git
additional_audit_files = [
    'crates/ggen-graph/audit/vision2030.self_audit.ocel.json',
    'crates/ggen-graph/audit/vision2030.coverage.json',
    'crates/ggen-graph/audit/vision2030.self_audit.summary.md',
    'crates/ggen-graph/audit/vision2030.verification_manifest.json'
]

all_paths = sorted(list(set(git_files + additional_audit_files)))
all_paths = [p for p in all_paths if p != 'crates/ggen-graph/audit/worktree_inventory.full.json']
all_paths = [p for p in all_paths if not p.startswith('crates/ggen-graph/audit/transcripts/')]

# Batch compute BLAKE3 hashes via CLI tool
blake3_hashes = {}
batch_size = 500
b3sum_bin = '/opt/homebrew/bin/b3sum' if os.path.exists('/opt/homebrew/bin/b3sum') else 'b3sum'

for i in range(0, len(all_paths), batch_size):
    batch = all_paths[i:i+batch_size]
    try:
        res = subprocess.run([b3sum_bin] + batch, capture_output=True, text=True, check=True, cwd=workspace_root)
        for line in res.stdout.splitlines():
            if line.strip():
                parts = line.split(None, 1)
                if len(parts) == 2:
                    blake3_hashes[parts[1].strip()] = parts[0].strip()
    except Exception:
        pass

for rel_path in all_paths:
    full_path = os.path.join(workspace_root, rel_path)
    if not os.path.exists(full_path) or os.path.isdir(full_path):
        continue
        
    stat_info = os.stat(full_path)
    size = stat_info.st_size
    mtime = stat_info.st_mtime
    
    # Categorize file and assign status/reason
    inclusion_status = 'included'
    inclusion_reason = 'unsupported format'
    
    if rel_path.endswith('.rs'):
        if 'tests/' in rel_path:
            inclusion_reason = 'test file'
        elif 'examples/' in rel_path:
            inclusion_reason = 'example file'
        else:
            inclusion_reason = 'source file'
    elif rel_path.endswith('.sh') or rel_path.endswith('.py'):
        inclusion_reason = 'script file'
    elif rel_path.endswith('.ttl') or rel_path.endswith('.sparql') or rel_path.endswith('.owl'):
        inclusion_reason = 'schema / ontology / query'
    elif rel_path.endswith('.md') or rel_path == 'LICENSE' or rel_path == 'README':
        inclusion_reason = 'documentation file'
    elif rel_path.endswith('.json'):
        if 'audit/' in rel_path:
            inclusion_reason = 'audit artifact'
        else:
            inclusion_reason = 'configuration file'
    elif rel_path.endswith('.toml'):
        inclusion_reason = 'configuration file'
    else:
        inclusion_status = 'excluded'
        inclusion_reason = 'untracked or temporary file'
        
    sha = compute_sha256(full_path)
    b3 = blake3_hashes.get(rel_path, sha) # Default to sha if b3sum call failed
    
    files_metadata.append({
        'path': rel_path,
        'size_bytes': size,
        'modified_time': get_utc_iso(mtime),
        'sha256': sha,
        'blake3': b3,
        'inclusion_status': inclusion_status,
        'inclusion_reason': inclusion_reason
    })

output_data = {
    'timestamp': datetime.now(timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),
    'files': files_metadata
}

output_path = os.path.join(workspace_root, 'crates/ggen-graph/audit/worktree_inventory.full.json')
with open(output_path, 'w') as f:
    json.dump(output_data, f, indent=2)

print(f'W0: Successfully inventoried {len(files_metadata)} files.')
" "$WORKSPACE_ROOT"

exit 0
