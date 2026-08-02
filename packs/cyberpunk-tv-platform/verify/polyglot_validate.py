#!/usr/bin/env python3
from __future__ import annotations

import json
import pathlib
import re
import sys

ROOT = pathlib.Path(__file__).resolve().parents[1]
required = [
    "ontology/platform.ttl", "ontology/platform-shapes.ttl", "ontology/vision2030.ttl",
    "queries/extract-vision2030.rq", "rules/escrow.n3", "rules/settlement.dl",
    "verify/chicago-tdd.mjs", "ggen.toml",
]
checks: list[dict[str, object]] = []

def check(name: str, condition: bool, detail: str) -> None:
    checks.append({"id": name, "state": "PASS" if condition else "FAIL", "detail": detail})

for rel in required:
    path = ROOT / rel
    check(f"path:{rel}", path.is_file(), str(path))

vision = (ROOT / "ontology/vision2030.ttl").read_text()
capability_ids = re.findall(r'tv:capabilityId\s+"([^"]+)"', vision)
check("capabilities:nonempty", bool(capability_ids), f"count={len(capability_ids)}")
check("capabilities:unique", len(capability_ids) == len(set(capability_ids)), f"count={len(capability_ids)};unique={len(set(capability_ids))}")

query = (ROOT / "queries/extract-vision2030.rq").read_text()
check("sparql:explicit-projection", "SELECT *" not in query.upper(), "SELECT * forbidden")
check("sparql:deterministic-order", "ORDER BY" in query.upper(), "ORDER BY required")

n3 = (ROOT / "rules/escrow.n3").read_text()
datalog = (ROOT / "rules/settlement.dl").read_text()
check("n3:nonempty", bool(n3.strip()), f"bytes={len(n3.encode())}")
check("datalog:nonempty", bool(datalog.strip()), f"bytes={len(datalog.encode())}")
check("n3:no-host-actuation", not re.search(r'\b(exec|spawn|system|socket)\b', n3, re.I), "declarative escrow only")
check("datalog:no-host-actuation", not re.search(r'\b(exec|spawn|system|socket)\b', datalog, re.I), "declarative closure only")

failed = [item for item in checks if item["state"] != "PASS"]
report = {
    "schema": "ggen.cyberpunk-tv.polyglot-python.v1",
    "language": "python",
    "capabilityCount": len(capability_ids),
    "checks": checks,
    "failed": len(failed),
    "standing": "PARTIAL_ALIVE" if not failed else "BUILD_BROKEN",
}
target = ROOT / ".ggen/evidence/polyglot-python.json"
target.parent.mkdir(parents=True, exist_ok=True)
target.write_text(json.dumps(report, indent=2, sort_keys=True) + "\n")
print(json.dumps(report, sort_keys=True))
sys.exit(0 if not failed else 1)
