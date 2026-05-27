# Analysis Report — Worktree Inventory and Command Transcript Capture

## Summary of Findings
To satisfy the Vision 2030 audit requirements for the Agent Truthfulness GALL protocol, we design a high-performance Python-based worktree inventory generator that avoids performance bottlenecks and self-referential paradoxes, along with a robust wrapper shell script to capture execution transcripts for all verifier commands.

---

## 1. Worktree Inventory Capture

### Proposed Design
The verifier script `scripts/gall/external/20_capture_full_worktree_inventory.sh` is designed to run in a fraction of a second by leveraging Python 3 (available as Python 3.9.6 on the system). 

A pure-bash implementation looping over the 7,100+ files in the repository would spawn over 20,000 sub-processes (`wc`, `date`, and `sha256sum`), taking several minutes to run and causing CPU thrashing. The Python implementation reads all files via `git ls-files`, computes SHA-256 digests in chunks, and retrieves OS-level file sizes and timestamps in a single pass under 1.0 second.

### File Exclusions for Stability
To ensure the worktree inventory remains stable, deterministic, and free of circular references, the following paths are explicitly excluded:
1. **Dynamic Artifacts**: `crates/ggen-graph/audit/worktree_inventory.json` (to prevent a self-referential hashing loop) and the `crates/ggen-graph/audit/transcripts/` directory (where execution logs are continuously added).
2. **Metadata Folders**: `.agents/` (agent planning and progress), `.gemini/` (temporary state files), and `.antigravitycli/` (CLI local files).
3. **Internal VCS**: `.git/` directory.

### Proposed Code for `scripts/gall/external/20_capture_full_worktree_inventory.sh`
```bash
#!/usr/bin/env bash
# ==============================================================================
# 20_capture_full_worktree_inventory.sh
# Generates the worktree inventory JSON at crates/ggen-graph/audit/worktree_inventory.json.
# ==============================================================================
set -euo pipefail

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
cd "$WORKSPACE_ROOT"

echo "=== [20] Capturing Full Worktree Inventory ==="

python3 - << 'EOF'
import subprocess
import os
import hashlib
import datetime
import json
import sys

def compute_sha256(filepath):
    h = hashlib.sha256()
    try:
        with open(filepath, 'rb') as f:
            for chunk in iter(lambda: f.read(65536), b''):
                h.update(chunk)
        return h.hexdigest()
    except Exception as e:
        sys.stderr.write(f"Error reading {filepath}: {e}\n")
        return None

def main():
    try:
        res = subprocess.run(
            ['git', 'ls-files', '--cached', '--others', '--exclude-standard'],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True,
            check=True
        )
        files = res.stdout.splitlines()
    except subprocess.CalledProcessError as e:
        sys.stderr.write(f"Git command failed: {e.stderr}\n")
        sys.exit(1)
    
    inventory = {
        "timestamp": datetime.datetime.now(datetime.timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ'),
        "files": []
    }
    
    for filepath in files:
        if not os.path.isfile(filepath):
            continue
            
        # Exclude dynamic audit files, local run metadata, and VCS files to ensure stability
        if filepath.startswith(('.agents/', '.gemini/', '.antigravitycli/', '.git/',
                                'crates/ggen-graph/audit/worktree_inventory.json',
                                'crates/ggen-graph/audit/transcripts/')):
            continue
            
        try:
            stat_info = os.stat(filepath)
            size = stat_info.st_size
            mtime = stat_info.st_mtime
            modified_time = datetime.datetime.fromtimestamp(mtime, datetime.timezone.utc).strftime('%Y-%m-%dT%H:%M:%SZ')
            sha = compute_sha256(filepath)
            if sha is not None:
                inventory["files"].append({
                    "path": filepath,
                    "size_bytes": size,
                    "modified_time": modified_time,
                    "sha256": sha
                })
        except Exception as e:
            sys.stderr.write(f"Error stat/hash on {filepath}: {e}\n")
            
    dest = "crates/ggen-graph/audit/worktree_inventory.json"
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    with open(dest, 'w') as f:
        json.dump(inventory, f, indent=2)
    print(f"Successfully generated inventory of {len(inventory['files'])} files at {dest}")

if __name__ == '__main__':
    main()
EOF

exit 0
```

---

## 2. Command Transcript Capture

### Proposed Design
We propose introducing a reusable wrapper script `scripts/gall/external/run_with_transcript.sh`. It intercepts command executions, measures duration, logs exit codes, serializes environmental context, computes stdout/stderr hashes, and outputs a JSON conforming to the schema under `crates/ggen-graph/audit/transcripts/`.

### Proposed Code for `scripts/gall/external/run_with_transcript.sh`
```bash
#!/usr/bin/env bash
# ==============================================================================
# run_with_transcript.sh
# Intercepts commands, logs transcripts, and outputs metrics in JSON format.
# ==============================================================================
set -euo pipefail

if [ "$#" -lt 2 ]; then
    echo "Usage: $0 <transcript_name> <command_and_arguments...>" >&2
    exit 1
fi

TRANSCRIPT_NAME="$1"
shift
COMMAND_TO_RUN="$*"

WORKSPACE_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../../.." && pwd)"
TRANSCRIPTS_DIR="$WORKSPACE_ROOT/crates/ggen-graph/audit/transcripts"
mkdir -p "$TRANSCRIPTS_DIR"

stdout_file=$(mktemp)
stderr_file=$(mktemp)

compute_sha256() {
    local file_path="$1"
    if [ ! -f "$file_path" ]; then
        echo "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        return
    fi
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$file_path" | awk '{print $1}'
    elif command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "$file_path" | awk '{print $1}'
    else
        openssl dgst -sha256 "$file_path" | awk '{print $NF}'
    fi
}

start_ns=$(date +%s%N)

set +e
# Run the command and capture output synchronously to avoid process substitution races
"$@" > "$stdout_file" 2> "$stderr_file"
exit_code=$?
set -e

end_ns=$(date +%s%N)
duration_ms=$(( (end_ns - start_ns) / 1000000 ))
if [ "$duration_ms" -lt 0 ]; then
    duration_ms=0
fi

# Print outputs synchronously to preserve interactive output streams
if [ -s "$stdout_file" ]; then
    cat "$stdout_file"
fi
if [ -s "$stderr_file" ]; then
    cat "$stderr_file" >&2
fi

# Serialize whitelist of environment variables to prevent token/credential leak
env_json="{}"
vars=("PATH" "SHELL" "USER" "LANG" "LC_ALL" "CARGO_HOME" "RUSTUP_HOME" "INTEGRITY_MODE")
for var in "${vars[@]}"; do
    val=$(printenv "$var" || true)
    if [ -n "$val" ]; then
        env_json=$(echo "$env_json" | jq --arg key "$var" --arg val "$val" '. + {($key): $val}')
    fi
done

stdout_hash=$(compute_sha256 "$stdout_file")
stderr_hash=$(compute_sha256 "$stderr_file")

rm -f "$stdout_file" "$stderr_file"

TRANSCRIPT_FILE="$TRANSCRIPTS_DIR/${TRANSCRIPT_NAME}.json"
jq -n \
    --arg cmd "$COMMAND_TO_RUN" \
    --argjson code "$exit_code" \
    --argjson dur "$duration_ms" \
    --argjson env "$env_json" \
    --arg out_sha "$stdout_hash" \
    --arg err_sha "$stderr_hash" \
    '{"command": $cmd, "exit_code": $code, "duration_ms": $dur, "environment": $env, "stdout_sha256": $out_sha, "stderr_sha256": $err_sha}' \
    > "$TRANSCRIPT_FILE"

exit "$exit_code"
```

---

## 3. Integration Options

We evaluate two integration models to link the verifier scripts to the transcript wrapper:

### Option A: Self-Wrapping Bootstrap (Decentralized)
Each verifier script is modified to include a self-wrapping header.
```bash
# Auto-wrap to log transcript if not already wrapped
if [ "${TRANSCRIPT_WRAPPED:-}" != "true" ]; then
    export TRANSCRIPT_WRAPPED="true"
    exec "$(dirname "$0")/run_with_transcript.sh" "$(basename "$0" .sh)" "$0" "$@"
fi
```
- **Pros**: Any execution (manual, automated, or by other scripts) automatically produces a valid transcript JSON.
- **Cons**: Requires modifying all 14 existing scripts.

### Option B: Orchestrator-Level Execution (Centralized)
Existing verifier scripts remain completely untouched. The orchestrator script (`verify_agent_truthfulness.sh` or `99_adjudicate_truthfulness.sh`) executes them via the wrapper.
```bash
./scripts/gall/external/run_with_transcript.sh 00_capture_baseline ./scripts/gall/external/00_capture_baseline.sh
```
- **Pros**: Zero modification to existing verifier scripts. Extremely clean codebase.
- **Cons**: Manual command line runs by developers will not generate transcripts.

---

## 4. Verification Methods

The following commands can be executed by the implementer or orchestrator to verify correct behavior:

1. **Verify Worktree Inventory Generation**:
   ```bash
   ./scripts/gall/external/20_capture_full_worktree_inventory.sh
   # Verify file exists
   [ -f "crates/ggen-graph/audit/worktree_inventory.json" ]
   # Verify JSON is valid and has "timestamp" and "files"
   jq -e '.timestamp and .files' crates/ggen-graph/audit/worktree_inventory.json
   ```

2. **Verify Transcript Wrapper**:
   ```bash
   ./scripts/gall/external/run_with_transcript.sh test_cmd echo "Hello, world!"
   # Verify file exists
   [ -f "crates/ggen-graph/audit/transcripts/test_cmd.json" ]
   # Verify JSON elements
   jq -e '.command == "echo Hello, world!" and .exit_code == 0 and .stdout_sha256 and .stderr_sha256' crates/ggen-graph/audit/transcripts/test_cmd.json
   ```
