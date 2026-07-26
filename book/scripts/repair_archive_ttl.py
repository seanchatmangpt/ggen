#!/usr/bin/env python3
"""Deterministically repair the eleven malformed archived Turtle fixtures.

Chicago TDD boundary: this module changes real file state, then callers observe
that state through an independent RDF parser. It contains no parser mocks and no
success-by-string-match shortcuts.
"""
from __future__ import annotations

import argparse
import re
from pathlib import Path
from typing import Callable

ROOT = Path(__file__).resolve().parents[2]


def add_prefix(text: str, prefix: str, iri: str) -> str:
    declaration = f"@prefix {prefix}: <{iri}> ."
    if re.search(rf"(?m)^@prefix\s+{re.escape(prefix)}:\s*<", text):
        return text
    matches = list(re.finditer(r"(?m)^@prefix\s+[^\n]+\.\s*$", text))
    if not matches:
        return declaration + "\n" + text
    at = matches[-1].end()
    return text[:at] + "\n" + declaration + text[at:]


def comma_string_collection(text: str, predicate: str) -> str:
    pattern = re.compile(
        rf"({re.escape(predicate)}\s*\()(?P<body>.*?)(\)\s*[;.])",
        re.DOTALL,
    )

    def replace(match: re.Match[str]) -> str:
        body = re.sub(r'"\s*,\s*"', '" "', match.group("body"))
        body = re.sub(r",\s*(?=\))", "", body)
        return match.group(1) + body + match.group(3)

    return pattern.sub(replace, text)


def semicolon_collection(text: str, predicate: str) -> str:
    pattern = re.compile(
        rf"({re.escape(predicate)}\s*\()(?P<body>.*?)(\)\s*[;.])",
        re.DOTALL,
    )

    def replace(match: re.Match[str]) -> str:
        body = re.sub(r";\s*(?=(?:[A-Za-z_:]|\"))", "\n        ", match.group("body"))
        return match.group(1) + body + match.group(3)

    return pattern.sub(replace, text)


def bracket_string_collection(text: str, predicate: str) -> str:
    pattern = re.compile(
        rf"({re.escape(predicate)}\s*)\[(?P<body>.*?)\](\s*[;.])",
        re.DOTALL,
    )

    def replace(match: re.Match[str]) -> str:
        values = re.findall(r'"(?:[^"\\]|\\.)*"', match.group("body"))
        if not values:
            return match.group(0)
        return match.group(1) + "(\n        " + "\n        ".join(values) + "\n    )" + match.group(3)

    return pattern.sub(replace, text)


def repair_validation_rules(text: str) -> str:
    return add_prefix(text, "", "http://ggen.example.org/shapes#")


def repair_bree(text: str) -> str:
    return add_prefix(text, "", "http://ggen.example.org/bree/jobs#")


def repair_showcase(text: str) -> str:
    return bracket_string_collection(text, "ggen:businessRules")


def repair_product_catalog(text: str) -> str:
    return re.sub(
        r'(:rustType\s+"Currency"\s*);(\s*#\s*Enum\s*\n\s*\n\s*:hasQuantity)',
        r'\1 .\2',
        text,
        count=1,
    )


def repair_otel(text: str) -> str:
    return add_prefix(text, "affiliate", "http://ggen.example.org/affiliate#")


def repair_fastapi(text: str) -> str:
    return comma_string_collection(text, "api:enumValues")


def repair_components(text: str) -> str:
    return semicolon_collection(text, "ea:rules")


def repair_deployment(text: str) -> str:
    return semicolon_collection(text, "ea:modules")


def repair_sku(text: str) -> str:
    pattern = re.compile(r"(sku:policies\s*\()(?P<body>.*?)(\)\s*[;.])", re.DOTALL)

    def replace(match: re.Match[str]) -> str:
        body = re.sub(r",\s*", "\n        ", match.group("body"))
        return match.group(1) + body.rstrip() + "\n    " + match.group(3)

    return pattern.sub(replace, text)


def repair_maturity(text: str) -> str:
    return add_prefix(text, "xsd", "http://www.w3.org/2001/XMLSchema#")


def repair_self_play(text: str) -> str:
    return bracket_string_collection(text, "ggen:dependencies")


REPAIRS: dict[str, Callable[[str], str]] = {
    "examples/archive/_validation_rules.ttl": repair_validation_rules,
    "examples/archive/bree-semantic-scheduler/bree-paas-generation.ttl": repair_bree,
    "examples/archive/comprehensive-rust-showcase/data/domain.ttl": repair_showcase,
    "examples/archive/event-horizon/02-data-model/rdf-first/product-catalog.ttl": repair_product_catalog,
    "examples/archive/factory-paas/templates/otel_sparql_queries.ttl": repair_otel,
    "examples/archive/fastapi-from-rdf/domain.ttl": repair_fastapi,
    "examples/archive/gcp-erlang-autonomics/.specify/specs/010-erlang-autonomic-c4/c4-components.ttl": repair_components,
    "examples/archive/gcp-erlang-autonomics/.specify/specs/010-erlang-autonomic-c4/c4-deployment.ttl": repair_deployment,
    "examples/archive/gcp-erlang-autonomics/.specify/specs/010-erlang-autonomic-c4/sku-mapping.ttl": repair_sku,
    "examples/archive/maturity-matrix-showcase/level2-small/ontology.ttl": repair_maturity,
    "examples/archive/self-play/ontology.ttl": repair_self_play,
}


def apply(root: Path, *, write: bool) -> list[Path]:
    changed: list[Path] = []
    for relative, repair in REPAIRS.items():
        path = root / relative
        original = path.read_text(encoding="utf-8")
        repaired = repair(original)
        if repaired != original:
            changed.append(path)
            if write:
                path.write_text(repaired, encoding="utf-8", newline="\n")
    return changed


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--write", action="store_true", help="write canonical repairs")
    parser.add_argument("--check", action="store_true", help="refuse if repairs remain")
    args = parser.parse_args()
    changed = apply(ROOT, write=args.write)
    for path in changed:
        print(path.relative_to(ROOT))
    if args.check and changed:
        print(f"TTL_REPAIR_REQUIRED count={len(changed)}")
        return 1
    print(f"TTL_REPAIR changed={len(changed)} mode={'write' if args.write else 'inspect'}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
