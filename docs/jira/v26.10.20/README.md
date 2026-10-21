# v26.10.20 Wave Index

ggen v26.10.20 — pattern-impl lanes (A01-A20), integration seams (I1-I3),
dialect+wasm prep (V01-V20), finishers (G01-G20), docs (D01-D08).
Canonical checkout: `/Users/sac/ggen` (graphlaw sibling: `/Users/sac/graphlaw`).
No worktrees; coordinator owns git; no lane runs git commands.

## Wave documents

| Doc | Role |
|---|---|
| [_LANES.md](_LANES.md) | Wave-1 read-only lane partition: 15 lanes (L01-L15) across five patterns (envelope, canonical identity, fresh-epoch lease, vacuity court, typed refusal) × reticle/engine/court surfaces |
| [RESOLUTIONS.md](RESOLUTIONS.md) | Wave-2 seam decisions: sole-writer file ownership, refusal-code vocabulary, lease-slot and receipt-generation contracts, court-compiler seam, coordinator integration rulings |
| [RECEIPTS.md](RECEIPTS.md) | Integration-level gates: 7 court families GREEN, dialect slice-1 verdict, wasm differential/size/version-skew results, dry-run publish exits, known follow-ups, cross-lane disclosures |
| [DIALECT-ADMISSION.md](DIALECT-ADMISSION.md) | ADMITTED 2026-10-20: RDF dialect admission matrix (Chomsky layering, JSON-LD reduced, N3 constrained), slice plan 1-3, permanent guardrails (25 MiB wasm ceiling, no new parser deps) |
| [FINAL_STATUS.md](FINAL_STATUS.md) | G20 finisher receipt: PRD capability matrix, 51 wave test files inventoried on disk (32 ggen + 19 graphlaw), out-of-scope grep receipts, follow-ups, publish-status caveats |

## Lane completion table

Status is cited from RECEIPTS.md verdicts and disk artifacts (mtimes / file
existence checked 2026-10-20 by D05); tests were NOT re-run by D05.

### A — pattern implementation

| Lane | Scope | Status |
|---|---|---|
| A01 | Envelope reticle/TTL | GREEN — `.specify/envelope-shapes.ttl` on disk; RECEIPTS envelope row (I1/I2) |
| A02 | envelope.rs + error variant | GREEN — envelope courts 4 + e2e 5 + binding 2 (RECEIPTS envelope row) |
| A03 | envelope refusal courts | GREEN — envelope row cites A03 seam pinned |
| A04 | Canonical identity reticle | GREEN — canonical courts 9 (ggen-config) on canonical row |
| A05 | sync.rs envelope + epoch wiring | GREEN — sole-writer seam per RESOLUTIONS; envelope/fresh-epoch rows GREEN |
| A06 | write.rs lease slots (FM-WRITE-012) | GREEN — fresh-epoch row cites A06 |
| A07 | abb-sbb Refusal::StaleLease | GREEN — fresh-epoch row cites A17/I1; Refusal enum per RESOLUTIONS |
| A08 | ggen-graph receipt v2 | GREEN — dataset seam row: 43 suites (V17 domain separation) |
| A09 | canonical frontmatter schema | GREEN — schema parity 5 (canonical row) |
| A10 | canonical codegen/digests | GREEN — codegen 6, canonical row cites A10 |
| A11 | canonical exhaustiveness courts | GREEN — canonical courts 9 + parity 5 |
| A12 | court compiler seam | GREEN — `crates/ggen-engine/src/court/{mod,catalog,compile}.rs` on disk; vacuity KAT/witness 10 |
| A13 | vacuity consumer courts | GREEN — vacuity row: self-ref 9, KAT/fail-closed/witness 10 |
| A14 | vacuity reticle schema | GREEN — `.specify/shapes/vacuity-court-schema.ttl` + `.specify/gates/vacuity-court.n3` on disk |
| A15 | refusal registry (L13 surface) | GREEN — typed-refusal row cites A15; `ontologies/shapes/boundary-refusal.shacl.ttl` on disk |
| A16 | shell-safety + fail-open guard | GREEN — shell-safety 4, fail-open 4 (typed-refusal row) |
| A17 | shim court (stale lease) | GREEN — fresh-epoch row: shim-court BLOCKED-exit-2, restore sha-verified |
| A18 | portable receipt generation field | GREEN — receipt epoch e2e / fresh-epoch row (I1) |
| A19 | census staleness court | GREEN — census 11 codes 0-drift (typed-refusal row); head-vs-log note logged as follow-up 4 |
| A20 | compile-fail projection | GREEN — compile-fail 3 (typed-refusal row) |

### I — integration seams

| Lane | Scope | Status |
|---|---|---|
| I1 | fresh-epoch integration (A06/A17/A18) | GREEN — RECEIPTS fresh-epoch row |
| I2 | envelope integration (A01-A03) | GREEN — RECEIPTS envelope row |
| I3 | canonical integration (A04/A05/A09/A10) | GREEN — RECEIPTS canonical row |

### V — dialect slices + wasm prep

| Lane | Scope | Status |
|---|---|---|
| V01 | dialect dispatch engine | GREEN — dialect dispatch row: dispatch 16 |
| V02 | pack dialect gates | GREEN — typed-refusal row cites V02; schema-TTL registration noted as follow-up 3 |
| V03 | receipt dialect | GREEN — receipt_dialect 10 (dialect dispatch row) |
| V04 | parser capability probe | GREEN — purrdf 2.0.2 7/7, oxrdfio REJECTED, remote contexts fail-closed |
| V05 | cross-dialect RDFC-1.0 identity court | GREEN — 6/6, blank-node stable; non-vacuity caught real mutant |
| V06 | W3C RDF 1.1 corpus conformance | GREEN — 325 entries, 9 divergences registered loudly; generator self-contained |
| V07 | error taxonomy / refusal crosswalk | GREEN — law_engine parse errors 4 (dialect dispatch row) |
| V08 | RuleSet refactor | PARTIAL — Step::DeriveN3 raw strings remain (follow-up 1) |
| V09 | wasm resource bounds | GREEN — 3 gaps closed, 8/8 boundary courts |
| V10 | wasm size court | GREEN — 6.34 MiB / 25 MiB (artifact measured on disk, FINAL_STATUS §1) |
| V11 | ABI manifest + court | GREEN — 10-test court (slice-1 verdict) |
| V12 | wasm-native differential harness | GREEN — 35 cases, zero divergences |
| V13 | dialect refusal registry wiring | GREEN — typed-refusal row cites V13 |
| V14 | vacuity reconcile | GREEN — self-ref 9 reconciled (vacuity row) |
| V15 | dialect law-engine parse errors | GREEN — parse errors 4 (dialect dispatch row) |
| V16 | law_engine dialect wiring | GREEN — dispatch row cites V16 |
| V17 | dataset seam (ggen-graph) | GREEN — 43 suites incl. dataset_intent 6 |
| V18 | wasm runbooks/docs | GREEN — docs runbooks landed (slice-1 verdict) |
| V19 | version skew + pin | GREEN — 26.10.5/26.10.13 closed, b3sum byte-exact; artifact re-pin noted as follow-up 2 |
| V20 | CI wasm-court job | GREEN — wired skip-loud |

### G — finishers (G20 receipt)

| Lane | Scope | Status |
|---|---|---|
| G01-G19 | finisher checks (PRD matrix rows) | GREEN/PARTIAL as recorded in FINAL_STATUS §1 — all court-landed; G20 did not re-execute runs, cited receipts |
| G20 | final assembly + on-disk evidence sweep | DONE — FINAL_STATUS.md: 51 wave test files inventoried, wasm size ALIVE, N3-not-codegen grep receipt, publish dry-run files NOT FOUND in /tmp (noted, not cited) |

### D — docs

| Lane | Scope | Status |
|---|---|---|
| D01-D07 | wave docs (lanes/resolutions/receipts/dialect/status) | LANDED — the five files indexed above exist in this directory |
| D08 | this index | DONE — README.md |

## Known follow-ups (from RECEIPTS.md §"Known follow-ups")

0. Flake candidate: `fresh_epoch_lease_court_test::backdated_receipt_refused_stale_lease`
   (1 failure under parallel load, 3/3 isolated passes) — needs a load-bearing fence.
1. `Step::DeriveN3` raw rule strings (V08 RuleSet slice pending).
2. wasm artifact re-pin pending rebuild.
3. dialects-schema registration in ggen-pack-spec-pack TTL (V02).
4. `ggen receipt verify` checks internally only (head-vs-log comparison missing).
