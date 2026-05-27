#!/usr/bin/env python3
# ==============================================================================
# run_with_transcript.sh (Python-implemented execution wrapper)
# Path: scripts/gall/external/run_with_transcript.sh
# ==============================================================================
import sys
import os
import subprocess
import time
import hashlib
import json
import tempfile

def main():
    if len(sys.argv) < 3:
        print("Usage: run_with_transcript.sh <cmd_name> <script_path> [args...]", file=sys.stderr)
        sys.exit(1)
        
    cmd_name = sys.argv[1]
    script_path = sys.argv[2]
    args = sys.argv[3:]
    
    # Configure environment to prevent infinite wrapping loops
    env = os.environ.copy()
    env["TRANSCRIPT_WRAPPED"] = "true"
    
    start_time = time.perf_counter()
    
    # Create temp files to capture stdout/stderr separately
    with tempfile.NamedTemporaryFile(delete=False) as out_f, tempfile.NamedTemporaryFile(delete=False) as err_f:
        out_name = out_f.name
        err_name = err_f.name
        
    try:
        res = subprocess.run([script_path] + args, env=env, stdout=open(out_name, 'wb'), stderr=open(err_name, 'wb'))
        exit_code = res.returncode
    except Exception as e:
        exit_code = -1
        with open(err_name, 'ab') as err_f:
            err_f.write(f"\nFailed to execute script: {e}\n".encode('utf-8'))
            
    end_time = time.perf_counter()
    duration_ms = (end_time - start_time) * 1000.0
    
    # Calculate stdout digest and emit to console stdout stream
    sha_out = hashlib.sha256()
    with open(out_name, 'rb') as f:
        stdout_content = f.read()
        sha_out.update(stdout_content)
        sys.stdout.buffer.write(stdout_content)
        sys.stdout.buffer.flush()
        
    # Calculate stderr digest and emit to console stderr stream
    sha_err = hashlib.sha256()
    with open(err_name, 'rb') as f:
        stderr_content = f.read()
        sha_err.update(stderr_content)
        sys.stderr.buffer.write(stderr_content)
        sys.stderr.buffer.flush()
        
    # Identify workspace repository root
    script_dir = os.path.dirname(os.path.abspath(__file__))
    repo_root = os.path.abspath(os.path.join(script_dir, "..", "..", ".."))
    transcripts_dir = os.path.join(repo_root, "crates/ggen-graph/audit/transcripts")
    os.makedirs(transcripts_dir, exist_ok=True)
    
    # Write separate physical output logs
    stdout_file_path = os.path.join(transcripts_dir, f"{cmd_name}.stdout")
    stderr_file_path = os.path.join(transcripts_dir, f"{cmd_name}.stderr")
    
    with open(stdout_file_path, 'wb') as f:
        f.write(stdout_content)
    with open(stderr_file_path, 'wb') as f:
        f.write(stderr_content)
        
    # Clean temporary OS file descriptors
    try:
        os.remove(out_name)
        os.remove(err_name)
    except OSError:
        pass
        
    # Build complete metadata transcript file
    transcript = {
        "command": cmd_name,
        "argv": [script_path] + args,
        "cwd": os.getcwd(),
        "exit_code": exit_code,
        "duration_ms": duration_ms,
        "stdout_path": f"crates/ggen-graph/audit/transcripts/{cmd_name}.stdout",
        "stderr_path": f"crates/ggen-graph/audit/transcripts/{cmd_name}.stderr",
        "stdout_sha256": sha_out.hexdigest(),
        "stderr_sha256": sha_err.hexdigest(),
        "environment": {k: v for k, v in env.items() if not k.startswith("SECRET_")} # Sanitized environment serialization
    }
    
    transcript_path = os.path.join(transcripts_dir, f"{cmd_name}.json")
    with open(transcript_path, 'w') as f:
        json.dump(transcript, f, indent=2)
        
    sys.exit(exit_code)

if __name__ == "__main__":
    main()
