#!/usr/bin/env python3
"""Project catalog/vision-2030-capabilities.json (ggen.vision2030.catalog.v1)
into a manifest the live `ggen vision2030` CLI can actually parse
(ggen.vision2030.program.v1, the `Manifest` struct in
crates/ggen-cli/src/cmds/vision2030/mod.rs).

This closes step 1 of the README's "If you are picking this pack back up"
list. It is a deterministic, lossless-where-possible projection:

  catalog capability field   -> Manifest Capability field
  ------------------------------------------------------
  id, iri, domain, horizon, blue_ocean_move, authority, summary  (unchanged)
  depends_on                 -> dependencies
  (absent)                   -> evidence: {}   (honest: no evidence exists yet)

Every capability lands with an EMPTY evidence map. The live evaluator
(evaluation.rs) classifies a capability with no observed evidence as
"DESIGNED", never as ALIVE -- so the resulting manifest validates as a
100%-DESIGNED program. That is the true current state of this pack, not an
overclaim: catalog entries are design commitments only until bound to a
real SBB report, receipt, replay, and independent acceptance (the catalog's
own `note` field says exactly this).

Re-run this script whenever the catalog changes; the output is committed so
a stale manifest is a visible `git diff`, not a silent drift.
"""
from __future__ import annotations

import json
import sys
from pathlib import Path

PACK_ROOT = Path(__file__).resolve().parent.parent
CATALOG = PACK_ROOT / "catalog" / "vision-2030-capabilities.json"
OUTPUT = PACK_ROOT / "catalog" / "vision-2030-program.manifest.json"

# Mirrors the constants in crates/ggen-cli/src/cmds/vision2030/mod.rs. If
# those change, this projection must change with them -- the live CLI's
# `validate` verb is the falsifier that catches drift here.
MANIFEST_SCHEMA = "ggen.vision2030.program.v1"
REQUIRED_DOMAINS = [
    "dx",
    "qol",
    "doctor",
    "healthcare",
    "marketplace",
    "mcp-plus",
    "planning",
    "runtime",
    "coordination",
    "process-intelligence",
    "governance",
    "manufacturing",
]
HORIZONS = [2026, 2027, 2028, 2029, 2030]


def main() -> int:
    catalog = json.loads(CATALOG.read_text())
    if catalog.get("schema") != "ggen.vision2030.catalog.v1":
        print(f"refusing: unexpected catalog schema {catalog.get('schema')!r}", file=sys.stderr)
        return 2

    capabilities = []
    for cap in catalog["capabilities"]:
        capabilities.append(
            {
                "id": cap["id"],
                "iri": cap["iri"],
                "domain": cap["domain"],
                "horizon": cap["horizon"],
                "blue_ocean_move": cap["blue_ocean_move"],
                "authority": cap["authority"],
                "summary": cap["summary"],
                "dependencies": list(cap.get("depends_on", [])),
                # Honest: no evidence is bound to any catalog entry yet.
                "evidence": {},
            }
        )

    # Sanity: the catalog claims to cover every required domain; make that a
    # hard check here rather than discovering it later in the evaluator.
    covered = {c["domain"] for c in capabilities}
    missing = [d for d in REQUIRED_DOMAINS if d not in covered]
    if missing:
        print(f"refusing: catalog does not cover required domains {missing}", file=sys.stderr)
        return 3

    manifest = {
        "schema": MANIFEST_SCHEMA,
        "program": {
            "id": "vision-2030-phase-change",
            # Tracks pack.toml's version; bump both together.
            "version": "26.8.3",
            "target_year": 2030,
            # 1000x phase-change target, per
            # docs/architecture/VISION-2030-PHASE-CHANGE-ARD-PRD-v26.8.3.md
            # (program_multiplier >= phase_change_target).
            "phase_change_target": 1000,
            # Empty on purpose: no independent acceptance authority has
            # registered a key for this program yet. The evaluator refuses
            # any external_acceptance signed by an unregistered issuer, so an
            # empty registry is the correct, conservative starting state.
            "trusted_issuers": {},
            "trusted_brokers": {},
        },
        "required_domains": REQUIRED_DOMAINS,
        # minimum_alive_capabilities = 1 per horizon: the smallest non-vacuous
        # gate. With zero evidence bound, every horizon is unmet and the
        # program reports DESIGNED -- which is true.
        "horizons": [{"year": y, "minimum_alive_capabilities": 1} for y in HORIZONS],
        "capabilities": capabilities,
    }

    OUTPUT.write_text(json.dumps(manifest, indent=2, sort_keys=False) + "\n")
    print(f"wrote {OUTPUT.relative_to(PACK_ROOT)}: {len(capabilities)} capabilities, all evidence={{}} (DESIGNED)")
    return 0


if __name__ == "__main__":
    sys.exit(main())
