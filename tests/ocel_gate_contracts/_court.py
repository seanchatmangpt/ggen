#!/usr/bin/env python3
"""Executable paired-witness court for Project #2 OCEL semantic gates."""
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


def _rows(graph: Graph, query: str) -> set[tuple[str, ...]]:
    return {tuple(str(value) for value in row) for row in graph.query(query)}


def run_gate(stem: str) -> None:
    gate = GATES / f"{stem}.rq"
    passed = WITNESSES / "pass" / f"{stem}.ttl"
    failed = WITNESSES / "fail" / f"{stem}.ttl"
    for path in (gate, passed, failed):
        if not path.is_file():
            raise AssertionError(f"missing exact-stem court subject: {path.relative_to(ROOT)}")
    query = gate.read_text(encoding="utf-8")
    pass_rows = _rows(_graph(passed), query)
    fail_rows = _rows(_graph(failed), query)
    if not fail_rows:
        raise AssertionError(f"{stem}: negative witness produced no violation rows")
    if pass_rows == fail_rows:
        raise AssertionError(
            f"{stem}: pass/fail witnesses are semantically indistinguishable ({len(fail_rows)} row(s))"
        )
    print(
        f"ALIVE gate={stem} pass_rows={len(pass_rows)} fail_rows={len(fail_rows)} differentiated=true"
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("stem")
    args = parser.parse_args()
    run_gate(args.stem)
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
