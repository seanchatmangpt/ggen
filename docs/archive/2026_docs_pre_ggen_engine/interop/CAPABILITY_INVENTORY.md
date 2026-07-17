<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Capability Inventory](#capability-inventory)
  - ["What can this code already do?" — Non-Deletion Completion Protocol](#what-can-this-code-already-do--non-deletion-completion-protocol)
  - [LIVE Capabilities (working, tested, receipted)](#live-capabilities-working-tested-receipted)
  - [PARTIAL Capabilities (real but unfinished)](#partial-capabilities-real-but-unfinished)
  - [CAPABILITY_SEED (pattern present, not wired)](#capability_seed-pattern-present-not-wired)
  - [BROKEN_BUT_REAL (isolated for repair)](#broken_but_real-isolated-for-repair)
  - [DOC_ONLY (described, not implemented)](#doc_only-described-not-implemented)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Capability Inventory
## "What can this code already do?" — Non-Deletion Completion Protocol

Status: LIVE (2026-05-27)

---

## LIVE Capabilities (working, tested, receipted)

| Capability | Owner crate | Evidence |
|---|---|---|
| Pair2 layout-safe construction | genesis-core-v2 | `size_of::<Pair2>() == 2` compile-time assertion |
| RelationPage<N> predicate-fixed storage | genesis-core-v2 | 34 tests passing |
| Construct8 8-lane bounded packing | genesis-core-v2 | Layout assertion + tests |
| Real BLAKE3 receipt generation | genesis-core-v2 | `Receipt::generate()` with blake3 crate |
| ReplayCursor causal chain validation | genesis-core-v2 | 8 E2E receipt chain tests |
| Need9 split law | genesis-core-v2 | `need9_split()` with refusal evidence |
| Need257 split law | genesis-core-v2 | `need257_split()` with chained receipts |
| Tamper-detection replay | genesis-core-v2 | Bit-flip → ReceiptMismatch refusal |
| Revelation doctrine types | genesis-core-v2 | Church, Seal, Plague, PlagueRecord, JerusalemGate |
| ArtifactStatus taxonomy | genesis-core-v2 | 9-status enum, no DELETE category |
| SHACL validation (ggen-core) | ggen-core | `validation/shacl.rs` with tests |
| CLI doctor command | ggen-cli | `cmds/doctor.rs` — LIVE |
| CLI wizard command | ggen-cli | `cmds/wizard.rs` — LIVE |
| OCEL type definitions | ggen-graph | `ocel/ocel_types.rs` |
| OCEL projection from graph | ggen-graph | `ocel/projection.rs` |
| PROV vocabulary types | ggen-graph | `vocab/prov.rs` |
| RDF triple parsing (Turtle) | ggen-membrane | TurtleParser integration |
| SymbolPageBuilder (256-byte limit) | ggen-membrane | `register()` with ByteTableOverflow |
| MembraneError wrapping Refusal | ggen-membrane | `From<Refusal> for MembraneError` |
| GenesisAdapter trait (WP4) | ggen-membrane | Boundary-clean, no serde_json in signature |

---

## PARTIAL Capabilities (real but unfinished)

| Capability | Owner | Gap | Smallest finish |
|---|---|---|---|
| Truex receipt chain | ggen-core (scattered) | No dedicated crate, placeholder receipts | Extract to `crates/genesis-lockchain/` |
| Pipeline receipt | ggen-core | `pipeline_engine/receipt.rs` not wired to BLAKE3 | Replace with genesis-core-v2 Receipt |
| Membrane OCEL binding | ggen-core | `membrane/ocel.rs` not connected to receipt chain | Wire to PlagueRecord::from_refusal |
| SHACL membrane enforcement | ggen-core | `membrane/shacl.rs` does not emit Refusal | Add `-> Result<(), PlagueRecord>` return |
| Graph doctor | ggen-graph | `doctor/mod.rs` partial | Connect to ChurchJudgment output |
| WASM shell | genesis-wasm-shell | Not in workspace, not compiled | Register in workspace |

---

## CAPABILITY_SEED (pattern present, not wired)

| Seed | File | Implied contract |
|---|---|---|
| Construct8Packet (richer) | `genesis-construct8/src/models.rs` | GenesisAdapter impl target |
| Shard admission | `genesis-construct8/src/admission.rs` | Shard::admit() → Receipt |
| Corpus hierarchy | `genesis-construct8/src/hierarchy.rs` | Corpus::compose() requires receipt chain |
| Replay stream | `genesis-construct8/src/replay.rs` | Connect to ReplayCursor |
| OCEL self-audit | `ggen-graph/src/ocel/self_audit.rs` | Output ChurchJudgment records |
| PROV membrane | `ggen-core/src/membrane/prov.rs` | Bind to Receipt as provenance |
| Telco routing | `tests/integration/test_telco_routing.rs` | Complete as integration test |

---

## BROKEN_BUT_REAL (isolated for repair)

| Artifact | File | Intent | Fix |
|---|---|---|---|
| Truex T0-T6 chain test | `genesis-core-v2/tests/receipt_chain_verification_test.rs` | Ed25519 + BLAKE3 chain verification | **RECOVERED** — chrono+tempfile deps added, 6 tests now passing |
| WASM shell | `genesis-wasm-shell/src/lib.rs` | Custody boundary for AtomVM parts | Add to workspace, fix imports |

---

## DOC_ONLY (described, not implemented)

| Claim | Document | Nearest seed |
|---|---|---|
| wasm4pm OCEL projection | `docs/interop/08_PROCESS_INTELLIGENCE_SPEC.md` | `ggen-graph/src/ocel/projection.rs` |
| AtomVM custody | `docs/interop/06_PART_RUNTIME_SPEC.md` | `genesis-wasm-shell/src/lib.rs` |
| Truex lifecycle crate | `docs/interop/07_TRUEX_LIFECYCLE_SPEC.md` | `genesis-lockchain/src/lib.rs` |
| Public vocab sync | `docs/interop/10_PUBLIC_VOCABULARY_GALL.md` | `ggen-core/src/validation/shacl.rs` |
