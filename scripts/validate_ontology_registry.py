#!/usr/bin/env python3
"""Validate the ggen federated semantic registry and emit a bounded receipt."""
from __future__ import annotations

import argparse
import hashlib
import json
import tomllib
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any

from rdflib import Graph

ROOT = Path(__file__).resolve().parents[1]
REGISTRY = ROOT / "ontology/registry.toml"
REQUIRED_SOURCE_FIELDS = {
    "source_identity",
    "display_name",
    "source_kind",
    "priority",
    "purpose",
    "canonical_location",
    "version",
    "retrieval_timestamp",
    "license",
    "source_digest",
    "transformation",
    "generated_projection_digest",
    "validation_result",
    "supersedes",
    "standing",
}
SOURCE_KINDS = {"O", "S", "K", "P"}
STANDINGS = {
    "UNKNOWN",
    "PARTIAL_ALIVE",
    "ALIVE",
    "BLOCKED",
    "BUILD_BROKEN",
    "UNSUPPORTED",
}
PRIORITIES = {"P0", "P1", "GENERATED", "INDUSTRY", "PROFILE"}
UNKNOWN_EVIDENCE = {"UNKNOWN", "NOT_RUN"}


@dataclass(frozen=True)
class Result:
    packs: int
    sources: int
    external_sources: int
    local_profiles: int
    p0_sources: int
    provider_sources: int
    local_rdf_files: int
    local_rdf_triples: int
    errors: tuple[str, ...]

    @property
    def ok(self) -> bool:
        return not self.errors


def read_toml(path: Path) -> dict[str, Any]:
    return tomllib.loads(path.read_text(encoding="utf-8"))


def digest(path: Path) -> str:
    return "sha256:" + hashlib.sha256(path.read_bytes()).hexdigest()


def validate(root: Path = ROOT) -> Result:
    errors: list[str] = []

    if not REGISTRY.is_file():
        return Result(0, 0, 0, 0, 0, 0, 0, 0, ("missing ontology/registry.toml",))
    registry = read_toml(REGISTRY)

    declared_pack_paths = tuple(registry.get("pack_paths", []))
    declared_pack_count = registry.get("pack_count")
    if declared_pack_count != 16:
        errors.append(f"registry pack_count must be 16, got {declared_pack_count!r}")
    if len(declared_pack_paths) != 16:
        errors.append(f"registry pack_paths must contain 16 paths, got {len(declared_pack_paths)}")
    if len(set(declared_pack_paths)) != len(declared_pack_paths):
        errors.append("registry pack_paths contains duplicates")

    pack_ids: set[str] = set()
    all_sources: dict[str, tuple[str, dict[str, Any]]] = {}
    all_references: list[tuple[str, str]] = []
    local_rdf_files = 0
    local_rdf_triples = 0

    for relative in declared_pack_paths:
        path = root / relative
        if not path.is_file():
            errors.append(f"missing pack manifest: {relative}")
            continue
        data = read_toml(path)
        pack = data.get("pack", {})
        pack_id = pack.get("id")
        expected_id = path.parent.name
        if pack_id != expected_id:
            errors.append(f"{relative}: pack id {pack_id!r} does not match directory {expected_id!r}")
        if not isinstance(pack_id, str):
            errors.append(f"{relative}: missing string pack id")
            continue
        if pack_id in pack_ids:
            errors.append(f"duplicate pack id: {pack_id}")
        pack_ids.add(pack_id)

        raw_sources = data.get("source", [])
        defaults = data.get("source_defaults", {})
        sources = [{**defaults, **source} for source in raw_sources]
        if pack.get("source_count") != len(sources):
            errors.append(
                f"{relative}: source_count={pack.get('source_count')!r} but observed {len(sources)}"
            )
        references = pack.get("references", [])
        if not isinstance(references, list):
            errors.append(f"{relative}: references must be an array")
            references = []
        for reference in references:
            all_references.append((pack_id, reference))

        for source in sources:
            identity = source.get("source_identity")
            if not isinstance(identity, str) or not identity:
                errors.append(f"{relative}: source_identity must be a non-empty string")
                continue
            if identity in all_sources:
                other_pack = all_sources[identity][0]
                errors.append(f"duplicate source identity {identity}: {other_pack}, {pack_id}")
                continue
            all_sources[identity] = (pack_id, source)

            missing = sorted(REQUIRED_SOURCE_FIELDS - set(source))
            if missing:
                errors.append(f"{identity}: missing fields {missing}")

            kinds = source.get("source_kind")
            if not isinstance(kinds, list) or not kinds or not set(kinds) <= SOURCE_KINDS:
                errors.append(f"{identity}: invalid source_kind {kinds!r}")

            if source.get("priority") not in PRIORITIES:
                errors.append(f"{identity}: invalid priority {source.get('priority')!r}")
            standing = source.get("standing")
            if standing not in STANDINGS:
                errors.append(f"{identity}: invalid standing {standing!r}")

            location = source.get("canonical_location")
            if not isinstance(location, str) or not location:
                errors.append(f"{identity}: canonical_location must be a non-empty string")
            if location == "UNKNOWN" and standing != "UNKNOWN":
                errors.append(f"{identity}: UNKNOWN location requires UNKNOWN standing")

            if standing in {"PARTIAL_ALIVE", "ALIVE"}:
                for field in ("version", "license", "source_digest", "validation_result"):
                    value = source.get(field)
                    if value in UNKNOWN_EVIDENCE:
                        errors.append(f"{identity}: {standing} requires concrete {field}")
                if location.startswith("repo:"):
                    local_path = root / location.removeprefix("repo:")
                    if not local_path.is_file():
                        errors.append(f"{identity}: missing local ontology {local_path.relative_to(root)}")
                    else:
                        expected = source.get("source_digest")
                        observed = digest(local_path)
                        if expected != observed:
                            errors.append(
                                f"{identity}: source digest mismatch expected={expected} observed={observed}"
                            )
                        graph = Graph()
                        try:
                            graph.parse(local_path, format="turtle")
                            local_rdf_files += 1
                            local_rdf_triples += len(graph)
                        except Exception as error:  # noqa: BLE001
                            errors.append(f"{identity}: RDF parse failed: {error}")

    for pack_id, reference in all_references:
        if reference not in all_sources:
            errors.append(f"{pack_id}: unresolved cross-pack reference {reference}")
        elif all_sources[reference][0] == pack_id:
            errors.append(f"{pack_id}: reference {reference} points inside the same pack")

    observed_sources = len(all_sources)
    if registry.get("source_count") != observed_sources:
        errors.append(
            f"registry source_count={registry.get('source_count')!r} but observed {observed_sources}"
        )

    local_profiles = sum(
        1 for _, source in all_sources.values() if source.get("priority") == "PROFILE"
    )
    external_sources = observed_sources - local_profiles
    if registry.get("local_profile_count") != local_profiles:
        errors.append(
            f"registry local_profile_count={registry.get('local_profile_count')!r} "
            f"but observed {local_profiles}"
        )
    if registry.get("external_source_count") != external_sources:
        errors.append(
            f"registry external_source_count={registry.get('external_source_count')!r} "
            f"but observed {external_sources}"
        )

    required_p0 = tuple(registry.get("required_p0", []))
    if len(required_p0) != 25 or len(set(required_p0)) != 25:
        errors.append("required_p0 must contain exactly 25 unique identities")
    for identity in required_p0:
        observed = all_sources.get(identity)
        if observed is None:
            errors.append(f"missing required P0 source: {identity}")
        elif observed[1].get("priority") != "P0":
            errors.append(f"{identity}: required P0 source is not priority P0")

    provider_sources = tuple(registry.get("generated_provider_sources", []))
    if len(provider_sources) != 8 or len(set(provider_sources)) != 8:
        errors.append("generated_provider_sources must contain exactly 8 unique identities")
    for identity in provider_sources:
        observed = all_sources.get(identity)
        if observed is None:
            errors.append(f"missing generated provider source: {identity}")
        elif observed[0] != "02-cloud-resources":
            errors.append(f"{identity}: provider source must belong to 02-cloud-resources")

    # Parse the SHACL graph as RDF syntax even though pySHACL execution is a separate boundary.
    shape_path = root / "ontology/00-foundation/public-semantic-source.shacl.ttl"
    if not shape_path.is_file():
        errors.append("missing PublicSemanticSource SHACL shape")
    else:
        graph = Graph()
        try:
            graph.parse(shape_path, format="turtle")
            local_rdf_files += 1
            local_rdf_triples += len(graph)
        except Exception as error:  # noqa: BLE001
            errors.append(f"PublicSemanticSource SHACL RDF parse failed: {error}")

    return Result(
        packs=len(pack_ids),
        sources=observed_sources,
        external_sources=external_sources,
        local_profiles=local_profiles,
        p0_sources=len(required_p0),
        provider_sources=len(provider_sources),
        local_rdf_files=local_rdf_files,
        local_rdf_triples=local_rdf_triples,
        errors=tuple(errors),
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--json", action="store_true")
    args = parser.parse_args()

    result = validate()
    if args.json:
        print(json.dumps(asdict(result), indent=2, sort_keys=True))
    else:
        print(
            "ONTOLOGY_REGISTRY "
            f"packs={result.packs} sources={result.sources} "
            f"external={result.external_sources} profiles={result.local_profiles} "
            f"p0={result.p0_sources} providers={result.provider_sources} "
            f"rdf_files={result.local_rdf_files} rdf_triples={result.local_rdf_triples} "
            f"errors={len(result.errors)}"
        )
        for error in result.errors:
            print(f"ERROR {error}")

    return 0 if result.ok else 1


if __name__ == "__main__":
    raise SystemExit(main())
