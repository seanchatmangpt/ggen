#!/usr/bin/env python3
"""Consumer court for semantic-cad-bim: run every pack gate against the
consumer design graph. Zero rows across all gates = ADMIT (exit 0);
any row = REFUSE (exit 2, with the offending rows printed).
"""
from __future__ import annotations

import json
import os
import sys
from pathlib import Path

from rdflib import Graph

HERE = Path(__file__).resolve().parent.parent
PACK = Path(
    os.environ.get(
        "CAD_BIM_PACK",
        (HERE / "../../../../ggen-marketplace/packs/industry-semantic-cad-bim").resolve(),
    )
)


def main() -> int:
    data = Graph()
    data.parse((HERE / "ontology.ttl").as_posix(), format="turtle")
    data.parse((PACK / "ontology.ttl").as_posix(), format="turtle")

    refusals = []
    for gate in sorted((PACK / "gates").glob("*.rq")):
        rows = [dict(r.asdict()) for r in data.query(gate.read_text(encoding="utf-8"))]
        refusals.extend({"gate": gate.stem, **row} for row in rows)

    verdict = "ADMIT" if not refusals else "REFUSE"
    print(json.dumps({
        "schema": "ggen.industry-cad-bim.consumer-court/1",
        "subject": (HERE / "ontology.ttl").as_posix(),
        "gates_run": len(sorted((PACK / "gates").glob("*.rq"))),
        "refusals": refusals,
        "verdict": verdict,
    }, indent=2))
    return 0 if not refusals else 2


if __name__ == "__main__":
    raise SystemExit(main())
