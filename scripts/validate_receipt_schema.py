#!/usr/bin/env python3
"""Schema court for the ggen portable receipt envelope (RFC-GPACK-001 §54-56).

Validates:
  1. the published JSON Schema
     (crates/ggen-engine/schema/receipt-portable.schema.json) self-consistency
     under draft 2020-12;
  2. embedded fixture envelopes derived field-for-field from the Serialize
     structs in crates/ggen-engine/src/portable_receipt.rs (packless-UNKNOWN
     and full ALIVE shapes, incl. a typed REFUSED standing);
  3. the real envelope emitted by the Rust unit test
     `envelope_serialization_matches_published_schema`
     (/tmp/ggen-portable-receipt-fixture.json) when present.

Chicago: real schema, real serialized envelope, python `jsonschema` library.
Exit 0 only if every court passes.

Usage:
  python3 scripts/validate_receipt_schema.py [envelope.json ...]
"""

from __future__ import annotations

import json
import os
import pathlib
import sys

import jsonschema

REPO = pathlib.Path(__file__).resolve().parent.parent
SCHEMA_PATH = REPO / "crates/ggen-engine/schema/receipt-portable.schema.json"
FIXTURE_PATH = (
    pathlib.Path(os.environ.get("TMPDIR", "/tmp"))
    / "ggen-portable-receipt-fixture.json"
)

SCHEMA_URI = "https://ggen.dev/receipt/pack/v1"
HEX64 = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef"
HEX64_B = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde0"
HEX64_C = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde1"
HEX64_D = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde2"
HEX64_E = "0123455789abcdef0123456789abcdef0123456789abcdef0123456789abcde3"
HEX64_F = "0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcde4"
SHA = "sha256:" + HEX64

FIXTURE_ENVELOPES = [
    # Full ALIVE envelope: field-for-field mirror of PortableReceiptEnvelope.
    {
        "schema": SCHEMA_URI,
        "spec": "RFC-GPACK-001-v26.9.17",
        "engine": {"name": "ggen", "version": "26.9.17"},
        "toolchain": {
            "rustc": "rustc 1.0.0 test",
            "cargo": "cargo 1.0.0 test",
        },
        "environment": {
            "os": "macos",
            "arch": "aarch64",
            "family": "unix",
            "variables_count": 2,
            "variables_sha256": SHA,
        },
        "subject": {
            "pack": "fixture-pack",
            "version": "0.1.0",
            "pack_digest": SHA,
        },
        "dependencies": [
            {
                "name": "fixture-dep",
                "version": "0.2.0",
                "digest": "sha256:" + HEX64_B,
                "scope": ["SEMANTICS", "LAW", "PROJECTION"],
            }
        ],
        "composition": {
            "resolved_packs": [
                {
                    "name": "fixture-pack",
                    "version": "0.1.0",
                    "digest": "sha256:" + HEX64_B,
                }
            ]
        },
        "graph": {"canonical_digest": HEX64_C},
        "admission": {
            "gates_attempted": ["fixture-gate"],
            "refusals": [],
        },
        "consequences": [
            {
                "target": "generated/fixture.txt",
                "operation": "MANAGED_WRITE",
                "sha256": HEX64_D,
            }
        ],
        "work_order": {"source": "work-order.ttl", "canonical_digest": HEX64_E},
        "replay": {"status": "UNKNOWN"},
        "standing": "ALIVE",
    },
    # Packless run: all-UNKNOWN subject and work order, never invented.
    {
        "schema": SCHEMA_URI,
        "spec": "RFC-GPACK-001-v26.9.17",
        "engine": {"name": "ggen", "version": "26.9.17"},
        "toolchain": {
            "rustc": "rustc 1.0.0 test",
            "cargo": "cargo 1.0.0 test",
        },
        "environment": {
            "os": "macos",
            "arch": "aarch64",
            "family": "unix",
            "variables_count": 0,
            "variables_sha256": SHA,
        },
        "subject": {
            "pack": "UNKNOWN",
            "version": "UNKNOWN",
            "pack_digest": "UNKNOWN",
        },
        "dependencies": [],
        "composition": {"resolved_packs": []},
        "graph": {"canonical_digest": "UNKNOWN"},
        "admission": {"gates_attempted": [], "refusals": []},
        "consequences": [],
        "work_order": {"source": "UNKNOWN", "canonical_digest": "UNKNOWN"},
        "replay": {"status": "UNKNOWN"},
        "standing": "REFUSED:GATE_VIOLATION",
    },
]


def main(argv: list[str]) -> int:
    print(f"schema: {SCHEMA_PATH}")
    schema = json.loads(SCHEMA_PATH.read_text())
    jsonschema.Draft202012Validator.check_schema(schema)
    print("PASS: schema self-consistent (draft 2020-12)")

    validator = jsonschema.Draft202012Validator(schema)
    failures = 0

    courts: list[tuple[str, object]] = [
        ("embedded fixture: full ALIVE", FIXTURE_ENVELOPES[0]),
        ("embedded fixture: packless REFUSED", FIXTURE_ENVELOPES[1]),
    ]
    if FIXTURE_PATH.is_file():
        courts.append(
            (f"rust-emitted fixture: {FIXTURE_PATH}", json.loads(FIXTURE_PATH.read_text()))
        )
    for extra in argv:
        p = pathlib.Path(extra)
        courts.append((f"argument envelope: {p}", json.loads(p.read_text())))

    for label, envelope in courts:
        errors = sorted(validator.iter_errors(envelope), key=lambda e: e.path)
        if errors:
            failures += 1
            print(f"FAIL: {label}")
            for err in errors:
                print(f"  - {list(err.path)}: {err.message}")
        else:
            print(f"PASS: {label}")

    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main(sys.argv[1:]))