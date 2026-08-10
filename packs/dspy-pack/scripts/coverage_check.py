#!/usr/bin/env python3
"""Real, runnable cross-reference of dspy-pack's admitted vocabulary against the actual
dspy 3.1.3 source tree (evidence/dspy-source-coverage.ttl) via a real SPARQL query
(queries/coverage_cross_reference.rq), executed with rdflib -- not a hand-written table.

Two checks, both real:
  1. Drift guard: re-parse gates/010_admission.rq's closed VALUES lists (the actual admission
     gate, not a copy of it) and assert they match the dspyaud:AdmittedKind facts in
     evidence/dspy-source-coverage.ttl exactly. If someone edits the gate's admitted-kind set
     without updating the coverage ledger, this fails loudly instead of silently drifting.
  2. Prints the real SPARQL query result as the coverage checklist.

Usage: python3 scripts/coverage_check.py
"""
from __future__ import annotations

import re
import sys
from pathlib import Path

import rdflib

PACK_ROOT = Path(__file__).resolve().parent.parent
GATE_PATH = PACK_ROOT / "gates" / "010_admission.rq"
EVIDENCE_PATH = PACK_ROOT / "evidence" / "dspy-source-coverage.ttl"
QUERY_PATH = PACK_ROOT / "queries" / "coverage_cross_reference.rq"


def parse_admitted_kinds_from_gate(gate_text: str) -> dict[str, set[str]]:
    """Re-parse the live gate's two closed VALUES lists: module kind and optimizer kind."""
    module_match = re.search(
        r'dspy:Module ; dspy:kind \?value \.\s*\n\s*FILTER\(STR\(\?value\) NOT IN \(([^)]+)\)\)',
        gate_text,
    )
    optimizer_match = re.search(
        r'dspy:Optimizer ; dspy:kind \?value \.\s*\n\s*FILTER\(STR\(\?value\) NOT IN \(([^)]+)\)\)',
        gate_text,
    )
    if not module_match or not optimizer_match:
        raise AssertionError(
            "coverage_check.py could not locate the Module/Optimizer kind VALUES lists in "
            f"{GATE_PATH} -- the gate's shape changed; update this regex."
        )

    def extract(group: str) -> set[str]:
        return {v.strip().strip('"') for v in group.split(",")}

    return {
        "Module": extract(module_match.group(1)),
        "Optimizer": extract(optimizer_match.group(1)),
    }


def admitted_kinds_from_ledger(graph: rdflib.Graph) -> dict[str, set[str]]:
    q = """
    PREFIX dspyaud: <http://seanchatmangpt.github.io/packs/dspy/audit#>
    SELECT ?role ?pythonSymbol WHERE { ?c a dspyaud:AdmittedKind ; dspyaud:role ?role ; dspyaud:pythonSymbol ?pythonSymbol . }
    """
    out: dict[str, set[str]] = {"Module": set(), "Optimizer": set()}
    for row in graph.query(q):
        role = str(row.role)  # type: ignore[attr-defined]
        out[role].add(str(row.pythonSymbol))  # type: ignore[attr-defined]
    return out


def main() -> int:
    gate_text = GATE_PATH.read_text()
    from_gate = parse_admitted_kinds_from_gate(gate_text)

    graph = rdflib.Graph()
    graph.parse(EVIDENCE_PATH, format="turtle")
    from_ledger = admitted_kinds_from_ledger(graph)

    drift = False
    for role in ("Module", "Optimizer"):
        if from_gate[role] != from_ledger[role]:
            drift = True
            print(
                f"DRIFT: gate admits {role} kinds {sorted(from_gate[role])} but "
                f"evidence/dspy-source-coverage.ttl records {sorted(from_ledger[role])}",
                file=sys.stderr,
            )
    if drift:
        print("\nFAIL: coverage ledger has drifted from the live admission gate.", file=sys.stderr)
        return 1
    print(f"OK: admitted-kind ledger matches the live gate exactly: {from_gate}\n")

    query_text = QUERY_PATH.read_text()
    rows = list(graph.query(query_text))

    total = len(rows)
    admitted = sum(1 for r in rows if str(r.admitted) == "true")  # type: ignore[attr-defined]
    print(f"# DSPy source-tree coverage checklist ({admitted}/{total} real classes admitted)\n")
    print(f"{'Role':<10} {'Admitted':<9} {'Symbol':<36} {'Tutorials':<20} Source")
    print("-" * 110)
    for row in rows:
        role = str(row.role)  # type: ignore[attr-defined]
        admitted_mark = "YES" if str(row.admitted) == "true" else "no"  # type: ignore[attr-defined]
        symbol = str(row.pythonSymbol)  # type: ignore[attr-defined]
        tutorials = str(row.tutorials) or "-"  # type: ignore[attr-defined]
        source = row.sourceFile  # type: ignore[attr-defined]
        print(f"{role:<10} {admitted_mark:<9} {symbol:<36} {tutorials:<20} {source}")

    return 0


if __name__ == "__main__":
    raise SystemExit(main())
