#!/usr/bin/env python3
"""Minimal consumer court for the generated CS2 JSON-LD projection."""

import json
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
SUBJECT = "https://chatman.ai/cs2#RFC-CS2-001"

payload = json.loads((ROOT / "generated" / "consumers.jsonld").read_text())
rows = payload["@graph"]
assert rows, "JSON-LD projection must contain consumers"
assert {row["subject"] for row in rows} == {SUBJECT}
assert {row["authority_ceiling"] for row in rows} == {"CONSTRUCT"}
assert payload["@context"]["cs2"] == "https://chatman.ai/cs2#"
keys = ("consumer", "work_id", "expected_projection")
assert len({tuple(row[key] for key in keys) for row in rows}) == len(rows)
print(json.dumps({"subject": SUBJECT, "rows": len(rows), "jsonld": True}, sort_keys=True))
