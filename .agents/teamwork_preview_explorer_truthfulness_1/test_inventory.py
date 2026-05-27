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
    # Run git ls-files to get files in the worktree
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
        # Exclude dynamic audit artifacts and metadata to ensure inventory stability
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
            
    # Output to our local folder for verification
    dest = "/Users/sac/ggen/.agents/teamwork_preview_explorer_truthfulness_1/test_worktree_inventory.json"
    os.makedirs(os.path.dirname(dest), exist_ok=True)
    with open(dest, 'w') as f:
        json.dump(inventory, f, indent=2)
    print(f"Successfully generated inventory of {len(inventory['files'])} files at {dest}")

if __name__ == '__main__':
    main()
