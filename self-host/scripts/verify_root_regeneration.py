#!/usr/bin/env python3
"""Verify that running root `ggen sync` changes only declared root outputs.

This verifier is intentionally independent of ggen's renderer. It reads the authored
root manifest, observes Git worktree changes, and refuses any unowned consequence.
"""
from __future__ import annotations

import argparse
import json
import subprocess
import sys
import tomllib
from pathlib import Path, PurePosixPath
from typing import Any


def git(root: Path, *args: str) -> bytes:
    return subprocess.run(
        ["git", "-C", str(root), *args],
        check=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
    ).stdout


def declared_outputs(root: Path) -> dict[str, str]:
    manifest = tomllib.loads((root / "ggen.toml").read_text(encoding="utf-8"))
    generation = manifest.get("generation", {})
    rules = generation.get("rules", []) if isinstance(generation, dict) else []
    outputs: dict[str, str] = {}
    for index, rule in enumerate(rules):
        if not isinstance(rule, dict) or "output_file" not in rule:
            continue
        output = PurePosixPath(str(rule["output_file"]))
        if output.is_absolute() or ".." in output.parts:
            raise SystemExit(f"unsafe root generation output: {output}")
        path = output.as_posix()
        owner = str(rule.get("name", f"rule-{index:03d}"))
        if path in outputs:
            raise SystemExit(f"duplicate root output owner: {path}: {outputs[path]} and {owner}")
        outputs[path] = owner
    if not outputs:
        raise SystemExit("root ggen.toml declares zero generation outputs")
    return outputs


def nul_paths(raw: bytes) -> set[str]:
    return {item.decode("utf-8") for item in raw.split(b"\0") if item}


def changed_paths(root: Path) -> list[str]:
    paths = set()
    paths |= nul_paths(git(root, "diff", "--name-only", "-z"))
    paths |= nul_paths(git(root, "diff", "--cached", "--name-only", "-z"))
    paths |= nul_paths(git(root, "ls-files", "--others", "--exclude-standard", "-z"))
    return sorted(paths)


def verify(root: Path) -> dict[str, Any]:
    outputs = declared_outputs(root)
    changed = changed_paths(root)
    unauthorized = [path for path in changed if path not in outputs]
    missing = [path for path in outputs if not (root / path).is_file()]
    return {
        "schema": "ggen.root-regeneration.verification.v1",
        "revision": git(root, "rev-parse", "HEAD").decode("ascii").strip(),
        "declared_output_count": len(outputs),
        "changed_paths": changed,
        "changed_owners": {path: outputs[path] for path in changed if path in outputs},
        "unauthorized_paths": unauthorized,
        "missing_outputs": sorted(missing),
        "passed": not unauthorized and not missing,
    }


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--root", type=Path, default=Path(__file__).resolve().parents[2])
    parser.add_argument("--report", type=Path, default=Path("self-host/observed/root-regeneration.json"))
    args = parser.parse_args()
    root = args.root.resolve()
    result = verify(root)
    report = args.report if args.report.is_absolute() else root / args.report
    report.parent.mkdir(parents=True, exist_ok=True)
    report.write_text(json.dumps(result, indent=2, sort_keys=True) + "\n", encoding="utf-8")
    if not result["passed"]:
        print(json.dumps(result, indent=2, sort_keys=True), file=sys.stderr)
        return 1
    print(json.dumps(result, sort_keys=True))
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
