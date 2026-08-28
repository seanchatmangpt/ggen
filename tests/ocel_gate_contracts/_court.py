#!/usr/bin/env python3
"""Executable exact-stem court for Project #2 OCEL semantic gates."""
from __future__ import annotations

import argparse
from pathlib import Path

from rdflib import Graph

ROOT = Path(__file__).resolve().parents[2]
PACK = ROOT / "packs" / "ggen-ecosystem-ocel-pack"
GATES = PACK / "gates"
WITNESSES = PACK / "witnesses"
ONTOLOGY = PACK / "ontology.ttl"


def _graph(witness: Path) -> Graph:
    graph = Graph()
    graph.parse(ONTOLOGY, format="turtle")
    graph.parse(witness, format="turtle")
    return graph


def run_gate(stem: str) -> None:
    gate = GATES / f"{stem}.rq"
    passed = WITNESSES / f"{stem}_pass.ttl"
    failed = WITNESSES / f"{stem}_fail.ttl"
    for path in (gate, passed, failed):
        if not path.is_file():
            raise AssertionError(f"missing exact-stem court subject: {path.relative_to(ROOT)}")
    query = gate.read_text(encoding="utf-8")
    pass_rows = list(_graph(passed).query(query))
    fail_rows = list(_graph(failed).query(query))
    if pass_rows:
        raise AssertionError(f"{stem}: positive witness produced {len(pass_rows)} violation row(s)")
    if not fail_rows:
        raise AssertionError(f"{stem}: negative witness did not falsify the gate")


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("stem")
    args = parser.parse_args()
    run_gate(args.stem)
    print(f"ALIVE gate={args.stem} pass_rows=0 fail_rows=>=1")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
