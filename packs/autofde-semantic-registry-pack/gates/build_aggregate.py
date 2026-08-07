#!/usr/bin/env python3
"""Build or verify the deterministic aggregate ontology."""

from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path

MODULES = [
    "00-foundation.ttl",
    "01-cloud-reference.ttl",
    "02-cloud-resources.ttl",
    "03-organization.ttl",
    "04-identity-authority.ttl",
    "05-security-threat.ttl",
    "06-controls-compliance.ttl",
    "07-events-observability.ttl",
    "08-process-decision.ttl",
    "09-data-governance.ttl",
    "10-cost-finops.ttl",
    "11-software-supply-chain.ttl",
    "12-ai-agent.ttl",
    "13-physical-digital-twin.ttl",
    "14-sustainability.ttl",
    "15-industry.ttl",
]


def sha256_bytes(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def build(pack_root: Path) -> str:
    ontology_dir = pack_root / "ontology"
    inventory = (pack_root / "source-inventory.md").read_bytes()
    inventory_sha = sha256_bytes(inventory)
    header = (
        "# GENERATED AGGREGATE — DO NOT EDIT DIRECTLY.\n"
        "# Canonical inputs: ontology/schema.ttl and ontology/00-foundation.ttl … ontology/15-industry.ttl.\n"
        "# Replay: python3 gates/build_aggregate.py\n"
        f"# Source inventory SHA-256: {inventory_sha}\n\n"
    )
    parts = [header, (ontology_dir / "schema.ttl").read_text(encoding="utf-8")]
    for name in MODULES:
        path = ontology_dir / name
        parts.append(f"\n# ===== MODULE: {name} =====\n\n")
        parts.append(path.read_text(encoding="utf-8"))
    return "".join(parts)


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--check", action="store_true", help="Refuse aggregate drift instead of writing.")
    args = parser.parse_args()

    pack_root = Path(__file__).resolve().parents[1]
    output = pack_root / "ontology.ttl"
    expected = build(pack_root)
    actual = output.read_text(encoding="utf-8") if output.exists() else None

    if args.check:
        ok = actual == expected
        receipt = {
            "subject": str(output.relative_to(pack_root)),
            "standing": "ALIVE" if ok else "REFUSED_GENERATED_DRIFT",
            "expected_sha256": sha256_bytes(expected.encode()),
            "actual_sha256": sha256_bytes(actual.encode()) if actual is not None else None,
        }
        print(json.dumps(receipt, sort_keys=True))
        return 0 if ok else 2

    output.write_text(expected, encoding="utf-8")
    print(
        json.dumps(
            {
                "subject": str(output.relative_to(pack_root)),
                "standing": "ALIVE",
                "sha256": sha256_bytes(expected.encode()),
            },
            sort_keys=True,
        )
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
