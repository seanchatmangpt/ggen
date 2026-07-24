#!/usr/bin/env python3
"""TICKET-002: SHACL pre-projection validation gate.

Runs pyshacl.validate() against the union of the 9 ontology files and
interview-assist.shacl.ttl. Exits non-zero and prints the violating
resource(s) on CONFORMS: False. Writes the full results_text to
shacl-validation-report.txt on every run (pass or fail) so failures leave
evidence behind, not just a terminal message.
"""
import sys
from pathlib import Path

import pyshacl
import rdflib

PACK_ROOT = Path(__file__).resolve().parent.parent

ONTOLOGY_FILES = [
    "ontology/00-document.ttl",
    "ontology/10-product.ttl",
    "ontology/20-requirements.ttl",
    "ontology/30-capabilities.ttl",
    "ontology/40-events-workflow.ttl",
    "ontology/50-policy.ttl",
    "ontology/60-provenance-receipts.ttl",
    "ontology/70-packs-datasets.ttl",
    "ontology/80-acceptance.ttl",
]
SHAPES_FILE = "shapes/interview-assist.shacl.ttl"


def main() -> int:
    data_graph = rdflib.Graph()
    for rel in ONTOLOGY_FILES:
        data_graph.parse(str(PACK_ROOT / rel), format="turtle")

    conforms, results_graph, results_text = pyshacl.validate(
        data_graph,
        shacl_graph=str(PACK_ROOT / SHAPES_FILE),
        data_graph_format="turtle",
        shacl_graph_format="turtle",
    )

    report_path = PACK_ROOT / "shacl-validation-report.txt"
    report_path.write_text(results_text)

    print(f"CONFORMS: {conforms}")
    print(f"triples: {len(data_graph)}")
    print(f"report written to {report_path}")

    if not conforms:
        print("VIOLATIONS FOUND:", file=sys.stderr)
        print(results_text, file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
