#!/usr/bin/env python3
"""Verify the AutoFDE federated semantic registry and emit a machine-readable receipt."""

from __future__ import annotations

import argparse
import hashlib
import importlib.util
import json
import re
import sys
import time
from collections import Counter, defaultdict
from pathlib import Path

try:
    from rdflib import Graph, Literal, Namespace, RDF, URIRef
except ModuleNotFoundError as exc:
    print(json.dumps({"standing": "UNSUPPORTED", "reason": "rdflib is required", "error": str(exc)}))
    raise SystemExit(3)

try:
    import yaml
except ModuleNotFoundError as exc:
    print(json.dumps({"standing": "UNSUPPORTED", "reason": "PyYAML is required", "error": str(exc)}))
    raise SystemExit(3)

REG = Namespace("https://ggen.io/ontology/autofde-registry/v1#")
PACK = "https://ggen.io/ontology/autofde-registry/v1/pack/"
DCTERMS = Namespace("http://purl.org/dc/terms/")
DCAT = Namespace("http://www.w3.org/ns/dcat#")


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def load_builder(path: Path):
    spec = importlib.util.spec_from_file_location("autofde_build_aggregate", path)
    if spec is None or spec.loader is None:
        raise RuntimeError(f"cannot load aggregate builder: {path}")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def clean_md(value: str) -> str:
    value = value.strip().replace("`", "")
    match = re.fullmatch(r"\*\*(.*?)\*\*", value)
    return match.group(1).strip() if match else value


def basic_slug(label: str) -> str:
    import unicodedata

    value = unicodedata.normalize("NFKD", label).encode("ascii", "ignore").decode("ascii").lower()
    value = value.replace("&", " and ")
    return re.sub(r"[^a-z0-9]+", "-", value).strip("-")


ALIASES = {
    "att-ck": "mitre-att-ck",
    "d3fend": "mitre-d3fend",
    "dqv": "data-quality-vocabulary-dqv",
    "dcterms": "dublin-core-terms",
    "dpv": "w3c-data-privacy-vocabulary-dpv",
    "focus": "focus-1-4",
    "focus-cost-and-usage": "focus-1-4",
    "geosparql-regions": "geosparql",
    "iec-cim": "iec-common-information-model",
    "ocel": "ocel-2-0",
    "odrl": "odrl-2-2",
    "org": "w3c-org",
    "oscal": "nist-oscal",
    "owl-time-reporting-periods": "owl-time",
    "qudt-units": "qudt",
    "rdf-rdfs-owl": "rdf-rdfs-owl-2",
    "saref": "etsi-saref",
    "schema-org-financial-terms": "schema-org",
    "schema-org-product-and-offer": "schema-org",
    "sosa-observations": "sosa-ssn",
    "w3c-sosa-ssn": "sosa-ssn",
    "spdx": "spdx-3-x",
    "stix": "stix-2-1",
    "tosca": "oasis-tosca-2-0",
    "cloud-carbon-footprint-resource-mappings": "cloud-carbon-footprint-model",
    "kubernetes-openapi-and-crds": "kubernetes-openapi",
}


def canonical_slug(label: str) -> str:
    slug = basic_slug(label)
    return ALIASES.get(slug, slug)


def extract_expected_mentions(markdown: str) -> list[tuple[int, str, str]]:
    lines = markdown.splitlines()
    mentions: list[tuple[int, str, str]] = []
    section: str | None = None

    for line_number, line in enumerate(lines, 1):
        if line.startswith("# "):
            section = line[2:].strip()
            continue
        match = re.match(r"(\d+)\.", section or "")
        section_number = int(match.group(1)) if match else None
        if not section_number or not 1 <= section_number <= 14:
            continue

        if line.startswith("|") and not re.match(r"^\|\s*[-: ]+\|", line):
            cells = [cell.strip() for cell in line.strip().strip("|").split("|")]
            if not cells or cells[0] in {
                "Public source",
                "Provider",
                "Ontology",
                "Standard/model",
                "Public model",
                "Public ontology/model",
                "Standard",
                "Ontology/model",
            }:
                continue
            if not cells[0].startswith("**") and section_number != 11:
                continue
            label = clean_md(cells[0])
            mentions.append((line_number, label, canonical_slug(label)))
        elif line.startswith("* ") and section_number in (2, 13, 14):
            label = clean_md(line[2:])
            mentions.append((line_number, label, canonical_slug(label)))

    for line_number in range(494, 519):
        match = re.match(r"\d+\.\s+(.*)", lines[line_number - 1])
        if match:
            label = clean_md(match.group(1))
            mentions.append((line_number, label, canonical_slug(label)))

    for line_number in range(523, 531):
        label = clean_md(lines[line_number - 1].strip())
        if label and label != "```":
            mentions.append((line_number, label, canonical_slug(label)))

    return mentions


def one(graph: Graph, subject: URIRef, predicate: URIRef):
    values = list(graph.objects(subject, predicate))
    return values[0] if len(values) == 1 else None


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--receipt", type=Path)
    args = parser.parse_args()

    started = time.perf_counter()
    pack_root = Path(__file__).resolve().parents[1]
    aggregate_path = pack_root / "ontology.ttl"
    inventory_path = pack_root / "source-inventory.md"
    shapes_path = pack_root / "shapes/source-registry.shacl.ttl"
    builder = load_builder(pack_root / "gates/build_aggregate.py")

    failures: list[dict[str, object]] = []
    checks: list[dict[str, object]] = []

    expected_aggregate = builder.build(pack_root)
    actual_aggregate = aggregate_path.read_text(encoding="utf-8")
    aggregate_ok = actual_aggregate == expected_aggregate
    checks.append({"name": "aggregate-drift", "standing": "ALIVE" if aggregate_ok else "REFUSED_GENERATED_DRIFT"})
    if not aggregate_ok:
        failures.append({"type": "REFUSED_GENERATED_DRIFT", "subject": str(aggregate_path)})

    graph = Graph()
    try:
        graph.parse(data=actual_aggregate, format="turtle", publicID=aggregate_path.as_uri())
        checks.append({"name": "aggregate-rdf-syntax", "standing": "ALIVE", "triples": len(graph)})
    except Exception as exc:
        failures.append({"type": "BUILD_BROKEN", "subject": str(aggregate_path), "error": str(exc)})
        graph = Graph()

    try:
        shapes_graph = Graph()
        shapes_graph.parse(shapes_path, format="turtle")
        checks.append({"name": "shacl-shapes-syntax", "standing": "ALIVE", "triples": len(shapes_graph)})
    except Exception as exc:
        failures.append({"type": "BUILD_BROKEN", "subject": str(shapes_path), "error": str(exc)})

    module_triples: dict[str, int] = {}
    for name in builder.MODULES:
        module_path = pack_root / "ontology" / name
        try:
            module_graph = Graph()
            module_graph.parse(module_path, format="turtle")
            module_triples[name] = len(module_graph)
        except Exception as exc:
            failures.append({"type": "BUILD_BROKEN", "subject": str(module_path), "error": str(exc)})
    checks.append(
        {
            "name": "module-rdf-syntax",
            "standing": "ALIVE" if len(module_triples) == len(builder.MODULES) else "BUILD_BROKEN",
            "modules": len(module_triples),
        }
    )

    source_type = REG.PublicSemanticSource
    pack_type = REG.SemanticPack
    sources = sorted(set(graph.subjects(RDF.type, source_type)), key=str)
    packs = sorted(set(graph.subjects(RDF.type, pack_type)), key=str)

    template_paths = sorted((pack_root / "templates").glob("*.tmpl"))
    template_query_rows: dict[str, dict[str, int]] = {}
    template_failures: list[dict[str, object]] = []
    for template_path in template_paths:
        try:
            template_text = template_path.read_text(encoding="utf-8")
            if not template_text.startswith("---\n"):
                raise ValueError("template is missing YAML frontmatter")
            _, frontmatter, body = template_text.split("---", 2)
            metadata = yaml.safe_load(frontmatter)
            if not isinstance(metadata, dict) or not isinstance(metadata.get("to"), str):
                raise ValueError("template frontmatter must declare string `to`")
            queries = metadata.get("sparql")
            if not isinstance(queries, dict) or not queries:
                raise ValueError("template frontmatter must declare at least one SPARQL query")
            if not body.strip():
                raise ValueError("template body is empty")
            query_counts: dict[str, int] = {}
            for query_name, query_text in sorted(queries.items()):
                if not isinstance(query_text, str) or not query_text.strip():
                    raise ValueError(f"SPARQL query `{query_name}` is empty")
                query_counts[str(query_name)] = sum(1 for _ in graph.query(query_text))
            template_query_rows[template_path.name] = query_counts
        except Exception as exc:
            template_failures.append({"template": str(template_path), "error": str(exc)})
    templates_ok = bool(template_paths) and not template_failures
    checks.append(
        {
            "name": "ggen-pack-template-boundary",
            "standing": "ALIVE" if templates_ok else "BUILD_BROKEN",
            "templates": len(template_paths),
            "query_rows": template_query_rows,
        }
    )
    failures.extend({"type": "GGEN_PACK_TEMPLATE_INVALID", **item} for item in template_failures)
    if not template_paths:
        failures.append({"type": "FM-PACK-005", "subject": str(pack_root / "templates")})

    expected_pack_ids = {name.removesuffix(".ttl") for name in builder.MODULES}
    actual_pack_ids = {str(value) for pack in packs for value in graph.objects(pack, DCTERMS.identifier)}
    pack_ok = actual_pack_ids == expected_pack_ids
    checks.append({"name": "sixteen-pack-closure", "standing": "ALIVE" if pack_ok else "PARTIAL_ALIVE", "count": len(actual_pack_ids)})
    if not pack_ok:
        failures.append(
            {
                "type": "PACK_CLOSURE_MISMATCH",
                "missing": sorted(expected_pack_ids - actual_pack_ids),
                "unexpected": sorted(actual_pack_ids - expected_pack_ids),
            }
        )

    inventory_text = inventory_path.read_text(encoding="utf-8")
    expected_mentions = extract_expected_mentions(inventory_text)
    expected_line_to_slug = {line: slug for line, _, slug in expected_mentions}
    actual_line_to_sources: dict[int, list[URIRef]] = defaultdict(list)
    for source in sources:
        for value in graph.objects(source, REG.inventoryLine):
            actual_line_to_sources[int(value)].append(source)

    expected_lines = set(expected_line_to_slug)
    actual_lines = set(actual_line_to_sources)
    line_ok = expected_lines == actual_lines and all(len(values) == 1 for values in actual_line_to_sources.values())
    checks.append(
        {
            "name": "inventory-line-closure",
            "standing": "ALIVE" if line_ok else "PARTIAL_ALIVE",
            "expected_mentions": len(expected_mentions),
            "observed_mentions": sum(len(values) for values in actual_line_to_sources.values()),
        }
    )
    if not line_ok:
        failures.append(
            {
                "type": "INVENTORY_COVERAGE_MISMATCH",
                "missing_lines": sorted(expected_lines - actual_lines),
                "unexpected_lines": sorted(actual_lines - expected_lines),
                "multiply_mapped_lines": sorted(line for line, values in actual_line_to_sources.items() if len(values) != 1),
            }
        )

    identity_mismatches = []
    for line, expected_slug in expected_line_to_slug.items():
        subjects = actual_line_to_sources.get(line, [])
        if len(subjects) != 1:
            continue
        actual_identifier = one(graph, subjects[0], DCTERMS.identifier)
        if actual_identifier is None or str(actual_identifier) != expected_slug:
            identity_mismatches.append(
                {"line": line, "expected": expected_slug, "actual": str(actual_identifier) if actual_identifier else None}
            )
    checks.append(
        {
            "name": "inventory-identity-alignment",
            "standing": "ALIVE" if not identity_mismatches else "PARTIAL_ALIVE",
            "mismatches": len(identity_mismatches),
        }
    )
    failures.extend({"type": "IDENTITY_ALIGNMENT_MISMATCH", **item} for item in identity_mismatches)

    required_single = [
        DCTERMS.identifier,
        DCTERMS.title,
        REG.transformation,
        REG.primaryPack,
        REG.recordStanding,
        REG.sourceStanding,
        REG.projectionStanding,
        REG.validationStanding,
        REG.canonicalLocationStatus,
        REG.versionStatus,
        REG.licenseStatus,
        REG.retrievalStatus,
        REG.sourceDigestStatus,
        REG.projectionDigestStatus,
    ]
    source_invariant_failures = []
    pack_counts: Counter[str] = Counter()
    source_standing_counts: Counter[str] = Counter()
    projection_standing_counts: Counter[str] = Counter()
    strategy_counts: Counter[str] = Counter()
    kind_counts: Counter[str] = Counter()

    for source in sources:
        missing = [str(predicate) for predicate in required_single if one(graph, source, predicate) is None]
        if not list(graph.objects(source, REG.sourceKind)):
            missing.append(str(REG.sourceKind))
        if not list(graph.objects(source, REG.memberOfPack)):
            missing.append(str(REG.memberOfPack))
        if not list(graph.objects(source, REG.inventoryLine)):
            missing.append(str(REG.inventoryLine))
        if not list(graph.objects(source, REG.listedAs)):
            missing.append(str(REG.listedAs))
        record_standing = one(graph, source, REG.recordStanding)
        if record_standing != REG.ALIVE:
            missing.append("recordStanding=ALIVE")

        primary_pack = one(graph, source, REG.primaryPack)
        if primary_pack is not None:
            pack_counts[str(primary_pack).removeprefix(PACK)] += 1
        source_standing = one(graph, source, REG.sourceStanding)
        projection_standing = one(graph, source, REG.projectionStanding)
        transformation = one(graph, source, REG.transformation)
        if source_standing is not None:
            source_standing_counts[str(source_standing).split("#")[-1]] += 1
        if projection_standing is not None:
            projection_standing_counts[str(projection_standing).split("#")[-1]] += 1
        if transformation is not None:
            strategy_counts[str(transformation).split("#")[-1]] += 1
        for kind in graph.objects(source, REG.sourceKind):
            kind_counts[str(kind).split("#")[-1]] += 1

        if source_standing == REG.ALIVE:
            alive_requirements = {
                "canonical_location": list(graph.objects(source, DCAT.landingPage)),
                "version": list(graph.objects(source, DCTERMS.hasVersion)),
                "license": list(graph.objects(source, DCTERMS.license)),
                "retrieved_at": list(graph.objects(source, REG.retrievedAt)),
                "source_digest": list(graph.objects(source, REG.sourceDigest)),
                "validation_alive": [value for value in graph.objects(source, REG.validationStanding) if value == REG.ALIVE],
            }
            absent = sorted(name for name, values in alive_requirements.items() if not values)
            if absent:
                missing.append("ALIVE_GUARD:" + ",".join(absent))

        if missing:
            source_invariant_failures.append({"source": str(source), "missing_or_invalid": missing})

    checks.append(
        {
            "name": "source-record-invariants",
            "standing": "ALIVE" if not source_invariant_failures else "PARTIAL_ALIVE",
            "sources": len(sources),
            "violations": len(source_invariant_failures),
        }
    )
    failures.extend({"type": "SOURCE_RECORD_INVARIANT", **item} for item in source_invariant_failures)

    p0_expected = {
        canonical_slug(label)
        for line, label, _ in expected_mentions
        if 494 <= line <= 518
    }
    provider_expected = {
        canonical_slug(label)
        for line, label, _ in expected_mentions
        if 523 <= line <= 530
    }
    p0_actual = {
        str(identifier)
        for source in graph.subjects(REG.priority, REG.P0)
        for identifier in graph.objects(source, DCTERMS.identifier)
    }
    provider_actual = {
        str(identifier)
        for source in graph.subjects(REG.priority, REG.P0Provider)
        for identifier in graph.objects(source, DCTERMS.identifier)
    }
    p0_ok = p0_actual == p0_expected
    provider_ok = provider_actual == provider_expected
    checks.append({"name": "p0-closure", "standing": "ALIVE" if p0_ok else "PARTIAL_ALIVE", "count": len(p0_actual)})
    checks.append(
        {"name": "provider-projection-closure", "standing": "ALIVE" if provider_ok else "PARTIAL_ALIVE", "count": len(provider_actual)}
    )
    if not p0_ok:
        failures.append(
            {"type": "P0_CLOSURE_MISMATCH", "missing": sorted(p0_expected - p0_actual), "unexpected": sorted(p0_actual - p0_expected)}
        )
    if not provider_ok:
        failures.append(
            {
                "type": "PROVIDER_CLOSURE_MISMATCH",
                "missing": sorted(provider_expected - provider_actual),
                "unexpected": sorted(provider_actual - provider_expected),
            }
        )

    inventory_sha = sha256_bytes(inventory_path.read_bytes())
    aggregate_sha = sha256_bytes(aggregate_path.read_bytes())
    elapsed_ms = round((time.perf_counter() - started) * 1000, 3)
    standing = "ALIVE" if not failures else "PARTIAL_ALIVE"
    receipt = {
        "subject": "autofde-semantic-registry-pack",
        "standing": standing,
        "identity": {
            "source_inventory_sha256": inventory_sha,
            "aggregate_sha256": aggregate_sha,
            "source_count": len(sources),
            "inventory_mention_count": len(expected_mentions),
            "pack_count": len(actual_pack_ids),
            "triple_count": len(graph),
        },
        "checks": checks,
        "counts": {
            "primary_pack": dict(sorted(pack_counts.items())),
            "source_standing": dict(sorted(source_standing_counts.items())),
            "projection_standing": dict(sorted(projection_standing_counts.items())),
            "strategy": dict(sorted(strategy_counts.items())),
            "kind": dict(sorted(kind_counts.items())),
            "module_triples": dict(sorted(module_triples.items())),
            "template_query_rows": template_query_rows,
        },
        "failures": failures,
        "elapsed_ms": elapsed_ms,
        "replay": [
            "python3 packs/autofde-semantic-registry-pack/gates/build_aggregate.py --check",
            "python3 packs/autofde-semantic-registry-pack/gates/verify_registry.py --receipt /tmp/autofde-semantic-registry-receipt.json",
        ],
    }

    payload = json.dumps(receipt, indent=2, sort_keys=True) + "\n"
    print(payload, end="")
    if args.receipt:
        args.receipt.parent.mkdir(parents=True, exist_ok=True)
        args.receipt.write_text(payload, encoding="utf-8")

    return 0 if standing == "ALIVE" else 1


if __name__ == "__main__":
    raise SystemExit(main())
