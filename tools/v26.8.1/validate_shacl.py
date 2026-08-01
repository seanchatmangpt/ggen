#!/usr/bin/env python3
"""Real SHACL validation for the v26.8.1 ontology, via pyshacl directly.

`ggen graph validate`'s own SHACL reporting was found this session to be
noisy/redundant -- it over-counts violations against blank-node shapes
(observed reporting ~124 violations where pyshacl, the standard reference
engine, found only 16 real ones). `just v26-8-1-ontology` used to shell out
to `ggen graph validate`, which meant a genuinely SHACL-conformant graph
could still fail `just v26-8-1-rebuild` on a false positive. This script
replaces that call with pyshacl directly, established as authoritative
earlier in this session.

Usage:
    python3 tools/v26.8.1/validate_shacl.py [--root PATH]

Exits 0 and prints CONFORMS: True on success; exits 1 with the real
pyshacl violation report on failure.
"""
from __future__ import annotations

import argparse
from pathlib import Path

import pyshacl
from rdflib import Graph


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--root", default=".")
    args = parser.parse_args()
    root = Path(args.root).resolve()

    data = Graph()
    data.parse(root / "ontology/v26.8.1/ontology.ttl", format="turtle")
    data.parse(root / "ontology/v26.8.1/legacy-capabilities.ttl", format="turtle")

    shapes = Graph()
    shapes.parse(root / "ontology/v26.8.1/shapes.ttl", format="turtle")

    conforms, _, results_text = pyshacl.validate(data, shacl_graph=shapes, inference="rdfs")
    print(f"CONFORMS: {conforms}")
    if not conforms:
        print(results_text)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
