#!/usr/bin/env python3
"""Qualification harness for the generic multi-projection pack.

This file is intentionally repository-native and may be executed by a later
verification lane. Manufacture mode authors it but does not execute it.
"""

from __future__ import annotations

import hashlib
import json
import shutil
import subprocess
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
REPO = ROOT.parents[1]
EXPECTED = (
    "projection.rs",
    "projection.py",
    "projection.ts",
    "projection.sql",
    "projection.proto",
    "projection.graphql",
    "projection.schema.json",
    "projection.context.jsonld",
    "projection.manifest.json",
)


def runner() -> list[str]:
    return ["ggen"] if shutil.which("ggen") else ["cargo", "run", "--quiet", "-p", "ggen-cli", "--"]


def sync() -> None:
    subprocess.run(
        runner() + ["sync", "--manifest", str(ROOT / "ggen.toml")],
        cwd=REPO,
        check=True,
    )


def digest(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def snapshot() -> dict[str, str]:
    generated = ROOT / "generated"
    missing = [name for name in EXPECTED if not (generated / name).is_file()]
    assert not missing, f"missing generated projections: {missing}"
    return {name: digest(generated / name) for name in EXPECTED}


def main() -> None:
    shutil.rmtree(ROOT / "generated", ignore_errors=True)
    sync()
    first = snapshot()
    sync()
    second = snapshot()
    assert first == second, "second generation must be byte-identical"

    manifest = json.loads((ROOT / "generated" / "projection.manifest.json").read_text())
    schema = json.loads((ROOT / "generated" / "projection.schema.json").read_text())
    context = json.loads((ROOT / "generated" / "projection.context.jsonld").read_text())

    names = [field["name"] for field in manifest["fields"]]
    assert names == ["id", "name", "enabled"], names
    assert len(names) == len(set(names)), "field-name collision"
    assert schema["x-semantic-subject"] == manifest["semantic_subject"]
    assert context["@id"] == manifest["semantic_subject"]

    print(json.dumps({"second_run_byte_identical": True, "sha256": second}, sort_keys=True))


if __name__ == "__main__":
    main()
