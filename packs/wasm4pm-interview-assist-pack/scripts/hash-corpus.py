#!/usr/bin/env python3
"""TICKET-001: hash the admitted InterviewAssist ontology corpus with BLAKE3.

Emits corpus-manifest.json: per-file hash + one combined corpus hash, computed
over a fixed, sorted file order so the output is reproducible run-to-run.
"""
import json
import sys
from pathlib import Path

import blake3

PACK_ROOT = Path(__file__).resolve().parent.parent

CORPUS_FILES = [
    "ontology/00-document.ttl",
    "ontology/10-product.ttl",
    "ontology/20-requirements.ttl",
    "ontology/30-capabilities.ttl",
    "ontology/40-events-workflow.ttl",
    "ontology/50-policy.ttl",
    "ontology/60-provenance-receipts.ttl",
    "ontology/70-packs-datasets.ttl",
    "ontology/80-acceptance.ttl",
    "shapes/interview-assist.shacl.ttl",
]


def hash_file(path: Path) -> str:
    h = blake3.blake3()
    h.update(path.read_bytes())
    return h.hexdigest()


def main() -> int:
    entries = []
    combined = blake3.blake3()
    for rel in CORPUS_FILES:
        path = PACK_ROOT / rel
        if not path.is_file():
            print(f"ERROR: missing corpus file {rel}", file=sys.stderr)
            return 1
        digest = hash_file(path)
        entries.append({"path": rel, "blake3": digest, "bytes": path.stat().st_size})
        combined.update(digest.encode("ascii"))

    manifest = {
        "pack": "wasm4pm-interview-assist-pack",
        "ticket": "TICKET-001",
        "algorithm": "BLAKE3",
        "file_order": CORPUS_FILES,
        "files": entries,
        "combined_hash": combined.hexdigest(),
    }

    out_path = PACK_ROOT / "corpus-manifest.json"
    out_path.write_text(json.dumps(manifest, indent=2) + "\n")
    print(f"wrote {out_path}")
    print(f"combined_hash: {manifest['combined_hash']}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
