#!/usr/bin/env python3
"""Repository-native qualification court for RFC-CS2-001 projections."""

from __future__ import annotations

import hashlib
import json
import shutil
import subprocess
import tempfile
from pathlib import Path

PROJECT = Path(__file__).resolve().parents[1]
REPO = PROJECT.parents[1]
EXACT_SUBJECT = "https://chatman.ai/cs2#RFC-CS2-001"
AUTHORITY_CEILING = "CONSTRUCT"


def _runner() -> list[str]:
    if shutil.which("ggen"):
        return ["ggen"]
    return ["cargo", "run", "--quiet", "-p", "ggen-cli", "--"]


def _sync(project: Path) -> None:
    subprocess.run(
        _runner() + ["sync", "--manifest", str(project / "ggen.toml")],
        cwd=REPO,
        check=True,
    )


def _sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def _assert_semantic_parity(json_path: Path, elixir_path: Path) -> None:
    payload = json.loads(json_path.read_text())
    rows = payload["rows"]
    assert rows, "canonical subject must manufacture at least one consumer"

    assert {row["subject"] for row in rows} == {EXACT_SUBJECT}
    assert {row["authority"] for row in rows} == {AUTHORITY_CEILING}

    order = lambda row: (row["consumer"], row["work_id"], row["projection"])
    assert rows == sorted(rows, key=order), "SPARQL projection must be stable-ordered"
    assert len({order(row) for row in rows}) == len(rows), "duplicate projection row"

    elixir = elixir_path.read_text()
    for row in rows:
        for key in ("subject", "authority", "consumer", "work_id", "projection"):
            assert str(row[key]) in elixir, f"{key} lost across JSON -> Elixir parity witness"


def _assert_divergent_subject_refused() -> None:
    with tempfile.TemporaryDirectory(prefix="ggen-cs2-negative-") as tmp:
        negative = Path(tmp) / "cs2-projections"
        shutil.copytree(
            PROJECT,
            negative,
            ignore=shutil.ignore_patterns("generated", "__pycache__"),
        )
        shutil.copyfile(
            negative / "fixtures" / "divergent-subject.ttl",
            negative / "canonical.ttl",
        )

        _sync(negative)

        generated = negative / "generated"
        json_path = generated / "consumers.json"
        elixir_path = generated / "consumers.exs"

        if json_path.exists():
            payload = json.loads(json_path.read_text())
            assert payload.get("rows", []) == [], "divergent subject minted canonical JSON"
        if elixir_path.exists():
            assert EXACT_SUBJECT not in elixir_path.read_text(), (
                "divergent subject minted canonical Elixir projection"
            )


def main() -> None:
    generated = PROJECT / "generated"
    shutil.rmtree(generated, ignore_errors=True)

    _sync(PROJECT)
    json_path = generated / "consumers.json"
    elixir_path = generated / "consumers.exs"
    assert json_path.is_file() and elixir_path.is_file(), "both projections must exist"

    _assert_semantic_parity(json_path, elixir_path)
    first = (_sha256(json_path), _sha256(elixir_path))

    _sync(PROJECT)
    second = (_sha256(json_path), _sha256(elixir_path))
    assert first == second, "second run must be byte-identical"

    _assert_semantic_parity(json_path, elixir_path)
    _assert_divergent_subject_refused()

    receipt = {
        "subject": EXACT_SUBJECT,
        "authority_ceiling": AUTHORITY_CEILING,
        "canonical_sha256": _sha256(PROJECT / "canonical.ttl"),
        "json_sha256": second[0],
        "elixir_sha256": second[1],
        "second_run_byte_identical": True,
        "divergent_subject_refused": True,
    }
    print(json.dumps(receipt, sort_keys=True))


if __name__ == "__main__":
    main()
