#!/usr/bin/env python3
"""run_gates.py -- the pack's real SPARQL-gate admission runner.

WHY THIS EXISTS (a real bug this script fixes, not a hypothetical one):
this pack's admission mechanism used to be SHACL (`shapes.ttl`, checked via
`ggen graph validate --shapes`). Commit ad9106702 ("feat: L5 pack ecosystem
-- SPARQL gates on both engines ...", 2026-07-19) migrated every pack in
this repo off SHACL onto SPARQL "gates" (`gates/*.rq`: one `# MESSAGE:`
header + one SELECT per file, where any RETURNED ROW is a violation) and
DELETED `packs/self-monitoring-pack/shapes.ttl` as part of that migration
-- but `scripts/capture_and_validate.sh`'s admission stage was never
updated to match. Confirmed live, this pass, before writing this script:

    $ scripts/capture_and_validate.sh <transcript> <out.ttl>
    ERROR: CLI execution failed: Command execution failed: shapes file
    `.../packs/self-monitoring-pack/shapes.ttl` unreadable: No such file or
    directory (os error 2)

`ggen graph validate` (the CLI) only understands SHACL `--shapes`; it has
no "run these SPARQL gate files" mode. `gates/*.rq` files ARE evaluated
automatically, but only inside `ggen sync run`'s law-evaluation stage for a
pack that is wired into a CONSUMING project's `ggen.toml [packs]` (see
e.g. wasm4pm-facts-pack/WIRING.md) -- this pack is deliberately never
consumed that way (README.md: "packs are data, not crates"; its real
verification harness is Rust integration tests + these standalone Python
scripts, never `ggen sync`). So there is no existing runner anywhere in
this repo that evaluates THIS pack's `gates/*.rq` files against arbitrary
input. This script is that runner -- built using the SAME rdflib SPARQL
1.1 engine every other script in this pack already treats as its
independent cross-check engine (`actuate_escalation.py`,
`measure_fire_precision_multi_session.py`), reading each gate file
VERBATIM (never re-typed, same discipline as `extract_hook_actions`
elsewhere in this pack).

SEMANTICS: loads ontology.ttl + one input Turtle file into one rdflib
Graph, then runs every `gates/*.rq` file (sorted by filename, so
010/020/030/040 run in their numbered order) against it. A gate's `SELECT`
query returns its VIOLATIONS -- zero rows means that gate passes. Exits 0
only if every gate returns zero rows; exits 1 and prints every violating
row (plus that gate's own `# MESSAGE:` line) otherwise. Never silently
accepts malformed/non-conforming input, mirroring the fail-closed
discipline `capture_and_validate.sh`'s header already documents for the
(now-removed) SHACL path.

USAGE:
    python3 scripts/run_gates.py --input fixtures/pattern-fires.ttl
    python3 scripts/run_gates.py --input <captured.ttl> --format json
"""

from __future__ import annotations

import argparse
import json
import sys
from pathlib import Path

try:
    import rdflib
    from rdflib.plugins.sparql import prepareQuery
except ImportError:  # pragma: no cover
    print("ERROR: rdflib is required (pip install rdflib)", file=sys.stderr)
    sys.exit(1)

PACK_DIR = Path(__file__).resolve().parent.parent


def gate_message(gate_text: str) -> str:
    for line in gate_text.splitlines():
        line = line.strip()
        if line.startswith("# MESSAGE:"):
            return line[len("# MESSAGE:") :].strip()
    return "(no # MESSAGE: header found)"


def run_gates(ontology_path: Path, input_path: Path, gates_dir: Path) -> dict:
    result: dict = {
        "ontology": str(ontology_path),
        "input": str(input_path),
        "gates_dir": str(gates_dir),
        "gates": [],
        "conforms": True,
    }

    g = rdflib.Graph()
    try:
        g.parse(str(ontology_path), format="turtle")
        g.parse(str(input_path), format="turtle")
    except Exception as exc:  # noqa: BLE001 -- a parse failure IS a gate failure
        result["conforms"] = False
        result["parse_error"] = str(exc)
        return result

    gate_files = sorted(gates_dir.glob("*.rq"))
    if not gate_files:
        result["conforms"] = False
        result["parse_error"] = f"no gate files found under {gates_dir}"
        return result

    for gate_path in gate_files:
        gate_text = gate_path.read_text()
        gate_entry: dict = {
            "gate": gate_path.name,
            "message": gate_message(gate_text),
        }
        try:
            q = prepareQuery(gate_text)
            rows = list(g.query(q))
        except Exception as exc:  # noqa: BLE001 -- a gate that fails to run is a hard fail
            gate_entry["error"] = str(exc)
            gate_entry["conforms"] = False
            result["conforms"] = False
            result["gates"].append(gate_entry)
            continue

        violations = []
        for row in rows:
            violations.append([str(v) for v in row])
        gate_entry["violation_count"] = len(rows)
        gate_entry["violations"] = violations
        gate_entry["conforms"] = len(rows) == 0
        if not gate_entry["conforms"]:
            result["conforms"] = False
        result["gates"].append(gate_entry)

    return result


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--input", required=True, type=Path, help="Turtle file to admit")
    ap.add_argument(
        "--ontology",
        type=Path,
        default=PACK_DIR / "ontology.ttl",
        help="ontology.ttl to load alongside --input (default: this pack's own)",
    )
    ap.add_argument(
        "--gates-dir",
        type=Path,
        default=PACK_DIR / "gates",
        help="directory of gates/*.rq files (default: this pack's own gates/)",
    )
    ap.add_argument("--format", choices=["text", "json"], default="text")
    args = ap.parse_args()

    result = run_gates(args.ontology, args.input, args.gates_dir)

    if args.format == "json":
        print(json.dumps(result, indent=2))
    else:
        print(f"admission check: {args.input}")
        print(f"  ontology: {args.ontology}")
        print(f"  gates:    {args.gates_dir}")
        for gate_entry in result["gates"]:
            status = "PASS" if gate_entry.get("conforms") else "FAIL"
            print(f"  [{status}] {gate_entry['gate']}: {gate_entry['message']}")
            if gate_entry.get("error"):
                print(f"           ERROR running gate: {gate_entry['error']}")
            for v in gate_entry.get("violations", []):
                print(f"           violation: {v}")
        if result.get("parse_error"):
            print(f"  PARSE ERROR: {result['parse_error']}")
        print(f"conforms: {result['conforms']}")

    return 0 if result["conforms"] else 1


if __name__ == "__main__":
    raise SystemExit(main())
