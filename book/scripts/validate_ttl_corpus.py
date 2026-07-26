#!/usr/bin/env python3
"""Validate active Turtle and enforce a bounded archive quarantine.

This is an independent state oracle: it reads the actual repository tree,
parses every Turtle file with rdflib, and verifies that the observed invalid set
is exactly the declared archive-only quarantine.
"""
from __future__ import annotations

import argparse
import json
import tomllib
from dataclasses import asdict, dataclass
from pathlib import Path

from rdflib import Graph

ROOT = Path(__file__).resolve().parents[2]
QUARANTINE = ROOT / "book/ttl-quarantine.toml"


@dataclass(frozen=True)
class Finding:
    path: str
    error: str


@dataclass(frozen=True)
class Result:
    files: int
    active_valid: int
    triples: int
    quarantined: int
    unexpected_invalid: tuple[Finding, ...]
    stale_quarantine: tuple[str, ...]
    illegal_quarantine: tuple[str, ...]
    live_references: tuple[str, ...]

    @property
    def ok(self) -> bool:
        return not (
            self.unexpected_invalid
            or self.stale_quarantine
            or self.illegal_quarantine
            or self.live_references
        )


def load_quarantine(root: Path = ROOT) -> dict[str, str]:
    data = tomllib.loads((root / "book/ttl-quarantine.toml").read_text(encoding="utf-8"))
    entries = data.get("entry", [])
    result: dict[str, str] = {}
    for entry in entries:
        path = str(entry["path"])
        reason = str(entry["reason"]).strip()
        if path in result:
            raise ValueError(f"duplicate quarantine path: {path}")
        if not reason:
            raise ValueError(f"empty quarantine reason: {path}")
        result[path] = reason
    return result


def live_text_files(root: Path) -> list[Path]:
    files: list[Path] = []
    for base in (root / "packs", root / "marketplace", root / "examples"):
        if not base.exists():
            continue
        for path in base.rglob("*"):
            if not path.is_file():
                continue
            relative = path.relative_to(root)
            if len(relative.parts) >= 2 and relative.parts[:2] == ("examples", "archive"):
                continue
            if path.suffix.lower() in {".toml", ".ttl", ".md", ".tera", ".json", ".yaml", ".yml"}:
                files.append(path)
    return sorted(files)


def validate(root: Path = ROOT) -> Result:
    quarantine = load_quarantine(root)
    turtle_files = sorted(set((root / "packs").rglob("*.ttl")) | set((root / "examples").rglob("*.ttl")))
    observed_invalid: dict[str, str] = {}
    triples = 0
    active_valid = 0

    for path in turtle_files:
        relative = path.relative_to(root).as_posix()
        graph = Graph()
        try:
            graph.parse(path, format="turtle")
            triples += len(graph)
            if relative not in quarantine:
                active_valid += 1
        except Exception as error:  # noqa: BLE001 - census records every parser failure
            observed_invalid[relative] = str(error)

    declared = set(quarantine)
    observed = set(observed_invalid)
    unexpected = tuple(
        Finding(path, observed_invalid[path]) for path in sorted(observed - declared)
    )
    stale = tuple(sorted(declared - observed))
    illegal = tuple(
        sorted(
            path
            for path in declared
            if not path.startswith("examples/archive/")
            or not (root / path).is_file()
        )
    )

    references: list[str] = []
    for source in live_text_files(root):
        text = source.read_text(encoding="utf-8", errors="replace")
        for quarantined in sorted(declared):
            # Require the exact repository-relative path. Basename matching is
            # unsound for generic names such as ontology.ttl or domain.ttl.
            if quarantined in text:
                references.append(
                    f"{source.relative_to(root).as_posix()} -> {quarantined}"
                )

    return Result(
        files=len(turtle_files),
        active_valid=active_valid,
        triples=triples,
        quarantined=len(declared),
        unexpected_invalid=unexpected,
        stale_quarantine=stale,
        illegal_quarantine=illegal,
        live_references=tuple(sorted(set(references))),
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()
    result = validate()
    if args.json:
        print(json.dumps(asdict(result), indent=2, sort_keys=True))
    else:
        print(
            "TTL_CORPUS "
            f"files={result.files} active_valid={result.active_valid} "
            f"triples={result.triples} quarantined={result.quarantined} "
            f"unexpected_invalid={len(result.unexpected_invalid)} "
            f"stale_quarantine={len(result.stale_quarantine)} "
            f"illegal_quarantine={len(result.illegal_quarantine)} "
            f"live_references={len(result.live_references)}"
        )
        for finding in result.unexpected_invalid:
            print(f"UNEXPECTED_INVALID {finding.path}: {finding.error}")
        for path in result.stale_quarantine:
            print(f"STALE_QUARANTINE {path}")
        for path in result.illegal_quarantine:
            print(f"ILLEGAL_QUARANTINE {path}")
        for reference in result.live_references:
            print(f"LIVE_REFERENCE {reference}")
    return 0 if result.ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
