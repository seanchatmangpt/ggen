#!/usr/bin/env python3
"""TICKET-054: hash a directory tree (queries/ or templates/) with BLAKE3.

Real, not fabricated: walks the given directory in sorted order, hashes every
regular file's bytes, and computes one combined hash over (relpath, hash)
pairs so the output changes if any file's content OR its filename changes.
Prints JSON to stdout: {"root": ..., "file_count": N, "files": [...], "combined_hash": ...}
"""
import json
import sys
from pathlib import Path

import blake3


def hash_file(path: Path) -> str:
    h = blake3.blake3()
    h.update(path.read_bytes())
    return h.hexdigest()


def main() -> int:
    if len(sys.argv) != 2:
        print("usage: hash-tree.py <dir>", file=sys.stderr)
        return 2
    root = Path(sys.argv[1]).resolve()
    if not root.is_dir():
        print(json.dumps({"root": str(root), "file_count": 0, "files": [], "combined_hash": None, "note": "directory does not exist"}))
        return 0

    files = sorted(p for p in root.rglob("*") if p.is_file())
    entries = []
    combined = blake3.blake3()
    for path in files:
        rel = str(path.relative_to(root))
        digest = hash_file(path)
        entries.append({"path": rel, "blake3": digest, "bytes": path.stat().st_size})
        combined.update(rel.encode("utf-8"))
        combined.update(bytes.fromhex(digest))

    print(json.dumps({
        "root": str(root.relative_to(root.parent.parent)) if root.parent.parent.exists() else str(root),
        "file_count": len(entries),
        "files": entries,
        "combined_hash": combined.hexdigest() if entries else None,
    }, indent=2))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
