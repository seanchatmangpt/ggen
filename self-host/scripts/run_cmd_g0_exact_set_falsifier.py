#!/usr/bin/env python3
"""Remove one observed tracked path and prove the independent verifier refuses it."""
from __future__ import annotations

import argparse
import hashlib
import json
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Any

SCHEMA = "ggen.cmd.g0-falsifier.v1"


def sha256(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def canonical_bytes(value: Any) -> bytes:
    return json.dumps(value, sort_keys=True, separators=(",", ":"), ensure_ascii=True).encode("utf-8")


def pretty_bytes(value: Any) -> bytes:
    return (json.dumps(value, sort_keys=True, indent=2, ensure_ascii=True) + "\n").encode("utf-8")


def publish(path: Path, data: bytes) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    if path.exists():
        if path.read_bytes() != data:
            raise RuntimeError(f"existing falsifier evidence diverges: {path}")
        return
    fd, temporary = tempfile.mkstemp(prefix=".falsifier-", dir=path.parent)
    try:
        with os.fdopen(fd, "wb") as handle:
            handle.write(data)
        os.replace(temporary, path)
    finally:
        if os.path.exists(temporary):
            os.unlink(temporary)


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, required=True)
    parser.add_argument("--expected-revision", required=True)
    parser.add_argument("--evidence-dir", type=Path, required=True)
    parser.add_argument("--report-path", type=Path, required=True)
    args = parser.parse_args()
    root = args.root.resolve()
    evidence_dir = args.evidence_dir.resolve()
    verifier = Path(__file__).with_name("verify_cmd_g0.py")

    with tempfile.TemporaryDirectory(prefix="cmd-g0-omission-") as temporary:
        tampered = Path(temporary) / "evidence"
        shutil.copytree(evidence_dir, tampered)
        repository_path = tampered / "repository.json"
        repository = json.loads(repository_path.read_text(encoding="utf-8"))
        objects = repository.get("objects", [])
        removable = next(
            (
                item
                for item in objects
                if isinstance(item, dict) and item.get("object_type") != "tree"
            ),
            None,
        )
        if removable is None:
            print("REFUSED: CMD-G0-FALSIFIER-SUBJECT: no tracked leaf object", file=sys.stderr)
            return 1
        removed_path = str(removable["path"])
        repository["objects"] = [
            item for item in objects if not (isinstance(item, dict) and item.get("path") == removed_path)
        ]
        repository_path.write_bytes(pretty_bytes(repository))
        result = subprocess.run(
            [
                sys.executable,
                str(verifier),
                "--root",
                str(root),
                "--expected-revision",
                args.expected_revision,
                "--evidence-dir",
                str(tampered),
            ],
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
    stderr = result.stderr.decode("utf-8", errors="replace")
    marker = "REFUSED: CMD-G0-EXACT-SET"
    if result.returncode == 0 or marker not in stderr:
        print(stderr, file=sys.stderr)
        print("REFUSED: CMD-G0-FALSIFIER-ESCAPED: omission was not refused correctly", file=sys.stderr)
        return 1

    report: dict[str, Any] = {
        "schema": SCHEMA,
        "subject_revision": args.expected_revision,
        "sabotage": "remove-one-tracked-leaf-from-copied-observation",
        "removed_path": removed_path,
        "verifier_exit_code": result.returncode,
        "observed_refusal": "CMD-G0-EXACT-SET",
        "verifier_stderr_sha256": sha256(result.stderr),
        "canonical_evidence_mutated": False,
    }
    report["report_digest_sha256"] = sha256(canonical_bytes(report))
    try:
        publish(args.report_path.resolve(), pretty_bytes(report))
    except (OSError, RuntimeError) as error:
        print(f"REFUSED: CMD-G0-FALSIFIER-REPORT: {error}", file=sys.stderr)
        return 1
    print(json.dumps(report, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
