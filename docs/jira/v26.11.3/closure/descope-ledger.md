# descope-ledger

Typed descope rulings for the v26.11.3 closure wave (coordinator, 2026-11-03).
Source: consolidated dispatch graph (session PG-8) reconciled against gap
reports XG-1..XG-8 and port roadmap PG-2. Each ruling carries a typed rationale
per the standing-vocabulary law; parked items are re-openable by a later work
order, never silently.

## D-1: ggen-sjira unported modules — parked-no-consumer

The 22 unported ggen_igniter semantic_jira modules (see receipts/sj-1.md for
the ported subset: lib.rs doc fix, unwitnessed-refusal courts, promote.rs,
descriptor.rs) remain parked. Rationale: `parked-no-consumer` — the ported
kernel (work_order/admit/receipts/frontier/ledger/authority/edits_bridge/
controller/execute/lease/epoch + this ledger's slices) has no in-flight
consumer requiring the remaining modules (bootstrap/*, cs2_*, cli.ex,
kernel_differential, observation/prov_events/r_projection/prose,
git_ground_truth, pack_health full phase). Re-open condition: any work order
whose acceptance criteria consumes one of these surfaces.

## D-2: ggen-lsp color provider — UNSUPPORTED (by design)

`textDocument/documentColor` is not implemented and must not be advertised.
Rationale: no color-bearing surface exists in the ggen domain
(.ttl/.rq/.tera/generated Rust carry no color literals); advertising it would
be a fake-success advertisement. signature_help and documentHighlight are
deferred to a later wave (buildable, not sealing-critical); color is
permanently descoped unless the domain gains a color surface.

## D-3: lsp-types-max pin 26.6.24 — pinned-by-law

All four manifests (root Cargo.toml:47,154, lsp-max-protocol:22,
lsp-max-lsif:19, playground:39) stay on lsp-types-max 26.6.24.
Rationale: 26.11.3 is not published to crates.io (crates.io max = 26.6.24);
no bump is evaluable on this host. Consequence: static
typeHierarchyProvider advertisement is impossible (field absent at this
version) — ggen-lsp uses dynamic registration as the lawful path (gl-1).
"bumped all crates to 26.11.3" claims are scoped to workspace-internal
crates only. Adjacent external pins (affidavit 26.6.22, wasm4pm* 26.6.x,
clap-noun-verb 26.6.2) documented, not bumped.

## D-4: ar-1 gate-witness scope — 8 of 8 declared gates proven

Gates 010-080 proven executed and PASS via flipping-shim courts
(receipts/ar-1-gates-witness.md). PG-1's proposed 090/100 register-closure
gates were NOT minted this wave — recorded as future work, not a gap in the
declared gate set (pack.toml declares 010-080 only).

## Open drift NOT descoped (carried to the next wave)

- WO-file vs RECEIPTS.md standing drift (WO-01/03/05/07/08/09 scaffold-era
  UNVERIFIED rows vs closure-table ALIVE) — the WO files are the narrow
  claims; the closure tables the wide ones. Next lane reconciles or narrows.
- XG-2 findings: sync.rs:431 artifact_baseline_generation hardcode,
  sbb about-strings, generated_commands.rs.backup in src/,
  receipt_chain_seam allow(dead_code), 55 undocumented + 37 unexercised FM
  codes, blast_radius/transactional write-path wiring (GAP-1, size L).
- ggen-marketplace baseline.json: smalt-core evidence[1] + 23 other entries
  missing consequence_sha256 (secondary-evidence tier; WO-08's own law says
  use run digests, so this is completeness debt, not a blocker).
- `--skip-ea-admission` CLI flag claimed in sync.rs docs but unwired in
  ggen-cli (filed by k2-test-fixes.md).
