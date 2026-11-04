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

## v2 update (2026-11-03, seal pass) — resolved items moved to closed-with-cites

CLOSED this wave (cite, not claim):
- 55 undocumented FM codes → CLOSED by `docs/FM_REGISTRY.md` (118 codes,
  census correction over XG-2's 87) + `fm_registry_parity_test` court
  (receipt sa-16.md). GAP-3's 37 unexercised codes carried as
  `tested_by: none` markers — still open, narrowed.
- sbb about-strings → CLOSED on disk (crates/ggen-abb-sbb/src/lib.rs:1157
  region; landed by SA-7, receipt sa-7.md).
- generated_commands.rs.backup → CLOSED on disk (removed by SA-7 after
  byte-identity proof; receipt sa-7.md).
- receiptctl example BLOCKED → CLOSED (receipt ga-9-receiptctl.md:
  wasm4pm-facts-pack dialects defect deleted, sync ×2 deterministic,
  168/168 cargo tests).
- Deterministic reds → CLOSED (receipt sx-1-reds.md: manifest version bump,
  perf-test EA fixture, arcus G3 build-if-absent).

STILL OPEN (carried forward, unchanged or narrowed):
- WO-file vs RECEIPTS.md standing drift (WO-01/03/05/07/08/09).
- sync.rs:431 artifact_baseline_generation — RESOLVED-DISCLOSED
  (zd-11-sync431.md, 2026-11-04): it is `artifact_baseline_generation: 0`
  in the EA SBB admission Request, the kernel's own "untracked" sentinel
  (StaleLease gate refuses only when baseline > 0, ggen-abb-sbb
  lib.rs:827-836); ggen-engine's receipt log tracks no per-artifact
  baseline, so no equivalent generic path exists — wiring the global
  generation would refuse every EA admission. Pinned-by-law, not drift.
- receipt_chain_seam allow(dead_code) (verified open).
- blast_radius/transactional write-path wiring (GAP-1, size L) — modules
  ALIVE, zero production callers (SV-1 confirmed unchanged).
- baseline.json consequence_sha256 completeness debt — PARTIALLY closed
  (SA-14 backfilled 297 provenance-labeled digests; aoq-pack entry itself
  is next-wave qualification per PF-2's descope ruling).
- `--skip-ea-admission` CLI flag unwired (verified open by PF-1).
- FM-LAW-015 class: 4 pre-existing sync_surface_test failures surfaced
  post-census (sx-1 disclosure) — never given the 45-remediation pass.
- sjira acceptance vacuous findings DS-7-D1/D2/D3 (execute exhaustion +
  replay guards unpinned; lease granted_at coverage indirect) — recorded
  by ds-7-mutation.md / sjira-mutation-battery.md, tests owned by shared
  court files (lane coordination required to tighten).
- clap-noun-verb FINDING-1: `cmds/*/mod.rs` directory-module nouns
  silently register zero verbs (ds-8.md) — audit of other cmds/* dirs
  is a named follow-on.
- ggen-lsp diagnostic_relay_probe CARRIED-OPEN (sx-1-reds.md: 20s
  publishDiagnostics timeout 2/2; cl-f attribution: hardcoded external
  binary dependency).

## AoQ next-wave handoff (PF-3 rulings, binding on Wave-1 dispatch)

- R-K1/R-K2: kernel crypto bodies in cgap-crypto; AoQ seams kernel-side.
- R-H1: GA-1's landed ontology.ttl is the header authority (GGM-SRC-002
  inverse) — Rust header.rs matches IT, not AOQ-02 prose.
- R-H2: reuse IpcError for framing classes; new AoQ crypto variants join
  the same enum. PG-A1's standalone AoqError framing classes superseded.
- R-C1: membranes deferred to Wave 2 (coordinator ruling — dissolves the
  PG-A9 cgo/sidecar conflict for Wave 1).
- ML-KEM vendor pin: offline `vendors/` pinning + hash receipt.
- Crate name: `cgap-aoq` (ruled once; PG-A1/A5/A7/A8 already use it).
- Everything beyond Wave 0 (ontology, pack, witnesses, consumer wiring,
  court) is DESCOPE next-wave-handoff: no cgap-aoq crate exists in the
  workspace at seal time (verified repo-wide zero AoQ code symbols).
