#!/usr/bin/env python3
"""Witness-court aggregator for this pack (EXPL2R strict contract).

Runs the exact-stem runner (runners/semantic_runner.py) over EVERY
(gate x witness) cell -- pass witnesses must be silent under all gates,
fail witnesses must fire EXACTLY their own gate and no other. Exit 0 only
when every cell is green; any REFUSED_MORE_THAN_ONE_GATE_FIRED /
REFUSED_GATE_ROWS_ON_PASS fails the court. Chicago discipline: real rdflib
engine, real graphs, no mocks.
"""
from __future__ import annotations

import json
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
RUNNER = ROOT / "runners" / "semantic_runner.py"
GATES = sorted((ROOT / "gates").glob("*.rq"))

def main() -> int:
    cells = []
    failures = []
    for gate in GATES:
        stem = gate.stem
        for expectation, witness in (("pass", ROOT / "witnesses" / "pass" / f"{stem}.ttl"),
                                     ("fail", ROOT / "witnesses" / "fail" / f"{stem}.ttl")):
            if not witness.is_file():
                failures.append(f"{stem}/{expectation}: missing witness")
                continue
            proc = subprocess.run(
                [sys.executable, str(RUNNER), "--gate", str(gate),
                 "--witness", str(witness), "--expectation", expectation],
                capture_output=True, text=True)
            verdict = "PASS" if proc.returncode == 0 else "REFUSED"
            cells.append({"case": stem, "expectation": expectation, "verdict": verdict})
            if proc.returncode != 0:
                failures.append(f"{stem}/{expectation}: {proc.stderr.strip()}")
    report = {
        "schema": "ggen.semantic-gate-witness-court/1",
        "pack": ROOT.name,
        "cells": cells,
        "standing": "REFUSED" if failures else "ALIVE",
    }
    print(json.dumps(report, indent=2))
    if failures:
        for failure in failures:
            print(f"REFUSED:GATE_WITNESS_COURT:{failure}", file=sys.stderr)
        return 2
    return 0

if __name__ == "__main__":
    raise SystemExit(main())
