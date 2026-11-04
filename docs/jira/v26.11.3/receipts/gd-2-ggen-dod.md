# GD-2: ggen definition-of-done projection (v26.11.3)

Date: 2026-11-03. Lane: GD-2 (implementation). Standing: ALIVE — every claim
below carries a real command + exit.

## Subject

`/Users/sac/ggen/dod-projection/` — a self-hosting ggen projection of the
v26.11.3 publish DoD as queryable RDF.

## Tree

- `ggen.toml` — pack-composition (frontmatter) schema (the only schema that
  runs pack.toml `[validation]` SPARQL gates at sync time; FM-CONFIG-101
  makes schemas mutually exclusive).
- `ontology/data.ttl` — dod: vocabulary + 8 PubGate + 5 PubDefect
  individuals.
- `queries/extract.rq` — SELECT all PubGate + PubDefect ORDER BY ?gateId,
  explicit COALESCE bindings on every column (no Tera fill-forward leak).
- `templates/dod-report.md.tmpl` — frontmatter `to: dod-report.md`, two
  sparql queries (gates, defects), renders the publish-readiness tables +
  UNMET verdict section.
- `packs/dod-pack/` — `pack.toml` with 4 `[validation]` SPARQL gates,
  `ontology.ttl` (FM-PACK-004 requires one), `templates/pack-marker.txt.tmpl`
  (FM-PACK-005 requires ≥1 pack template; ggen refuses symlinked pack
  templates).

## Real-state population (re-read on disk 2026-11-03)

| gate | status | basis (command / receipt) |
|---|---|---|
| pr-824-ci | UNMET | PR #824 CI triage list — 17+ workflows failing |
| workspace-version-26113 | MET | `grep -m1 '^version' Cargo.toml` → `26.11.3` |
| fm-registry-parity | MET | `docs/FM_REGISTRY.md` = 118 rows (counted) |
| sjira-suite-211 | MET | receipts x1-sjira-kernel.md (211/0) |
| control-plan-wired | MET | receipts ga-7-control-wiring.md |
| stress-trio-landed | UNMET | ab-13-kernel-stress.md + ab-14-chain-scale.md exist; `ls receipts/ab-20-*.md` → no match (AB-20 in-flight) |
| clippy-clean | UNMET | per-crate receipts only (ds-2.md, ds-8.md); no workspace-wide clean receipt |
| changelog-present | UNMET | `grep '26\.11\.3' CHANGELOG.md` → 0 hits |

Verdict rendered into dod-report.md: 4 MET / 4 UNMET →
PUBLISH_BLOCKED_UNTIL_ABOVE_GATES_MET.

## Gates witnessed (ggen 26.10.11 installed binary)

1. `ggen sync run` #1: exit 0; `written: [dod-report.md,
   .ggen/dod-pack-marker.txt]`; all 4 validation gates passed; graph_hash
   5d3c458881d3835348d363fef24e37c76ea7c26ff8e2ed9e41cb7d54f4aa443cf;
   `ggen receipt verify` → `"valid": true` (signed).
2. `ggen sync run` #2: exit 0; `written: []`; both outputs
   `unchanged: content identical` → drift-clean ×2; dod-report.md sha256
   byte-identical across runs (54406398...3d43d5839).
3. All queries ORDER BY everywhere; zero GROUP_CONCAT (known-broken on
   26.10.11 — avoided).

## Shim courts (2, both flipped)

Baseline sha256 → shim → sync must refuse → byte-exact restore →
post-restore green.

1. **010+020 court**: dropped the `dod:evidenceReceipt` triple from
   control-plan-wired → sync exit 1 (refused).
2. **030 court**: flipped hex-prefix-lottery status to "DROPPED" → typed
   refusal, exit 1:
   `[FM-PACK-013] pack dod-pack gate 030_defect_carried.rq refused the sync
   against the union graph: a PubDefect is always CARRIED — never silently
   dropped.: SELECT returned 1 row(s); first row: { ?defect =
   urn:ggen:dod:hex-prefix-lottery }.`
3. Restores byte-exact (data.ttl sha256 afb4c4be...44b60bf2 both times);
   post-restore sync exit 0.

## Disclosed notes

- The canonical template is `templates/dod-report.md.tmpl`, rendered via
  the consumer `[templates] dir` under schema 2 (witnessed). The pack also
  ships `pack-marker.txt.tmpl` only because ggen refuses a zero-template
  pack and refuses symlinked pack templates.
- ggen refused a pack without `ontology.ttl` (`[FM-PACK-004]`); the pack
  ships a minimal one — real vocabulary + individuals stay in the consumer
  `ontology/data.ttl`.
- clippy-clean and changelog-present were not pinned in the lane brief;
  pinned UNMET from real on-disk state.

## Files (absolute)

- /Users/sac/ggen/dod-projection/ggen.toml
- /Users/sac/ggen/dod-projection/ontology/data.ttl
- /Users/sac/ggen/dod-projection/queries/extract.rq
- /Users/sac/ggen/dod-projection/templates/dod-report.md.tmpl
- /Users/sac/ggen/dod-projection/packs/dod-pack/pack.toml
- /Users/sac/ggen/dod-projection/packs/dod-pack/ontology.ttl
- /Users/sac/ggen/dod-projection/packs/dod-pack/gates/010_gate_metadata.rq
- /Users/sac/ggen/dod-projection/packs/dod-pack/gates/020_met_requires_receipt.rq
- /Users/sac/ggen/dod-projection/dod-report.md
- /Users/sac/ggen/docs/jira/v26.11.3/receipts/gd-2-ggen-dod.md

Standing: ALIVE. Sync ×2 drift-clean, both shim courts flipped with typed
refusals and byte-exact restores, signed sync receipt valid. No git commands
run.
