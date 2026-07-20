from pathlib import Path
import hashlib
import json
import shutil
import subprocess
import tempfile

root = Path.cwd()

def tree(path: Path):
    return {p.relative_to(path).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in sorted(path.rglob("*")) if p.is_file()}

subprocess.run(["ggen", "sync", "run"], check=True)
first = tree(root)
subprocess.run(["ggen", "sync", "run"], check=True)
second = tree(root)
if first != second:
    changed = sorted(set(first) ^ set(second) | {p for p in first.keys() & second.keys() if first[p] != second[p]})
    raise SystemExit(json.dumps({"outcome": "idempotency-refused", "changed": changed}, indent=2))
print(json.dumps({"outcome": "byte-identical", "files": len(first)}, indent=2))
