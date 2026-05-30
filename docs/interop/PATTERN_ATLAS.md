<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Pattern Atlas](#pattern-atlas)
  - ["Find every repeated structure. Map where it appears. Classify it. Connect it."](#find-every-repeated-structure-map-where-it-appears-classify-it-connect-it)
  - [Pattern: Receipt](#pattern-receipt)
  - [Pattern: Construct8](#pattern-construct8)
  - [Pattern: RelationPage](#pattern-relationpage)
  - [Pattern: Replay](#pattern-replay)
  - [Pattern: OCEL / Process Evidence](#pattern-ocel--process-evidence)
  - [Pattern: SHACL / Vocabulary Validation](#pattern-shacl--vocabulary-validation)
  - [Pattern: Shard / Corpus / Segment](#pattern-shard--corpus--segment)
  - [Pattern: Doctor / Wizard / Telco](#pattern-doctor--wizard--telco)
  - [Pattern: PROV](#pattern-prov)
  - [Summary: Pattern Coverage](#summary-pattern-coverage)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Pattern Atlas
## "Find every repeated structure. Map where it appears. Classify it. Connect it."

Status: LIVE (updated from scan evidence 2026-05-27)

---

## Pattern: Receipt

**Intended role:** Cryptographic proof of a state transition. BLAKE3 output binding inputs to outputs.

| File | Location | Status |
|---|---|---|
| `crates/genesis-core-v2/src/primitives.rs` | `struct Receipt { signature: [u8;32] }` + `Receipt::generate()` w/ real BLAKE3 | **LIVE** |
| `crates/genesis-core-v2/src/revelation.rs` | `PlagueRecord::from_refusal()` generates receipted plague records | **LIVE** |
| `crates/genesis-core-v2/tests/receipt_chain_test.rs` | 8 E2E receipt chain tests w/ tamper detection | **LIVE** |
| `crates/genesis-core-v2/tests/receipt_chain_verification_test.rs` | Truex T0-T6 chain, Ed25519, BLAKE3 verification | **LIVE** (recovered) |
| `crates/genesis-construct8/src/receipt.rs` | Independent receipt implementation | **CAPABILITY_SEED** |
| `crates/ggen-config/src/receipt/` | `chain.rs`, `envelope.rs`, `receipt_impl.rs` | **CAPABILITY_SEED** |
| `crates/ggen-core/src/pipeline_engine/receipt.rs` | Pipeline-level receipt | **PARTIAL** |
| `crates/ggen-core/src/semantic_bit/receipt.rs` | Semantic-bit receipt variant | **CAPABILITY_SEED** |
| `crates/ggen-graph/src/receipt/mod.rs` | Graph-level receipt | **CAPABILITY_SEED** |
| `crates/ggen-cli/src/receipt_manager.rs` | CLI receipt management | **PARTIAL** |
| `crates/ggen-a2a-mcp/src/a2a/receipt.rs` | A2A protocol receipt | **CAPABILITY_SEED** |

**Finish action:** Unify all seeds behind `genesis-core-v2::Receipt` as the single authority. Others become adapters or compatibility aliases.

---

## Pattern: Construct8

**Intended role:** Bounded lane packing — up to 8 Pair2 entries per construction act.

| File | Location | Status |
|---|---|---|
| `crates/genesis-core-v2/src/primitives.rs` | `#[repr(C)]` Construct8, 32 bytes, 8 lanes, valid_mask bitmap | **LIVE** |
| `crates/genesis-construct8/src/models.rs` | `Construct8Packet` — richer model with JSON | **CAPABILITY_SEED** |
| `crates/genesis-construct8/src/forge.rs` | Forge pattern for Construct8 | **CAPABILITY_SEED** |
| `crates/genesis-construct8/src/admission.rs` | Admission logic — Shard/Corpus management | **CAPABILITY_SEED** |
| `crates/genesis-construct8/src/replay.rs` | Replay for Construct8 stream | **CAPABILITY_SEED** |
| `crates/ggen-core/src/genesis.rs` | Construct8 referenced in ggen-core genesis.rs | **PARTIAL** |
| `crates/ggen-membrane/src/lib.rs` | Construct8Packet used via knhk-construct8 | **PARTIAL** |
| `crates/genesis-wasm-shell/src/lib.rs` | Construct8 referenced for WASM shell | **BROKEN_BUT_REAL** |

**Finish action:** `genesis-construct8` is the richest seed. Wire it to `genesis-core-v2::Construct8` via `GenesisAdapter`. The JSON surface stays in `genesis-construct8`; the pure bytes surface is in `genesis-core-v2`.

---

## Pattern: RelationPage

**Intended role:** Predicate-fixed binary relation context. Groups Pair2 entries sharing one predicate.

| File | Location | Status |
|---|---|---|
| `crates/genesis-core-v2/src/primitives.rs` | `#[repr(C)] RelationPage<CAP>`, const generic, 520 bytes at CAP=256 | **LIVE** |
| `crates/ggen-core/src/genesis.rs` | RelationPage referenced | **PARTIAL** |
| `crates/ggen-core/tests/genesis_primitives_test.rs` | 34 tests covering RelationPage | **LIVE** |
| `crates/ggen-core/tests/genesis_page_split_test.rs` | Page split tests | **LIVE** |
| `crates/ggen-membrane/src/lib.rs` | MembraneBuilder writes to RelationPage | **PARTIAL** |
| `crates/ggen-projection/src/lib.rs` | Projects RelationPage to output format | **CAPABILITY_SEED** |

**Finish action:** RelationPage is LIVE in genesis-core-v2. Connect ggen-membrane's builder to it via GenesisAdapter. Connect ggen-projection to it for OCEL output.

---

## Pattern: Replay

**Intended role:** Deterministic re-execution given initial state + receipt sequence.

| File | Location | Status |
|---|---|---|
| `crates/genesis-core-v2/src/primitives.rs` | `ReplayCursor` — validates Construct8 sequence | **LIVE** |
| `crates/genesis-core-v2/tests/receipt_chain_test.rs` | Replay validation tests | **LIVE** |
| `crates/genesis-construct8/src/replay.rs` | Richer replay with Shard/Corpus | **CAPABILITY_SEED** |
| `crates/ggen-graph/tests/dialect_replay.rs` | Graph dialect replay | **CAPABILITY_SEED** |
| `crates/ggen-graph/tests/receipt_replay.rs` | Receipt replay test | **CAPABILITY_SEED** |
| `crates/ggen-core/src/semantic_bit/phase.rs` | Phase-level replay | **CAPABILITY_SEED** |

**Finish action:** `genesis-construct8/src/replay.rs` is the richest seed. Connect it to `genesis-core-v2::ReplayCursor` as the verification layer.

---

## Pattern: OCEL / Process Evidence

**Intended role:** OCEL 2.0 JSON projection from receipts for pm4py/process mining.

| File | Location | Status |
|---|---|---|
| `crates/ggen-graph/src/ocel/ocel_types.rs` | OCEL type definitions | **CAPABILITY_SEED** |
| `crates/ggen-graph/src/ocel/projection.rs` | OCEL projection from graph | **CAPABILITY_SEED** |
| `crates/ggen-graph/src/ocel/coverage.rs` | Coverage analysis | **CAPABILITY_SEED** |
| `crates/ggen-graph/src/ocel/self_audit.rs` | Self-audit via OCEL | **CAPABILITY_SEED** |
| `crates/ggen-graph/src/ocel/prov_types.rs` | PROV types for OCEL | **CAPABILITY_SEED** |
| `crates/ggen-core/src/membrane/ocel.rs` | Membrane OCEL binding | **CAPABILITY_SEED** |
| `crates/ggen-graph/tests/ocel_diagnostics_doctor_test.rs` | OCEL doctor tests | **TEST_ONLY** |
| `crates/genesis-construct8/src/projectors.rs` | OCEL/SHACL projectors | **CAPABILITY_SEED** |

**Finish action:** `ggen-graph/src/ocel/` is the richest cluster. Wire it to receipt chain (from genesis-core-v2) to produce OCEL from real boundaries. Connect doctor test as the specification.

---

## Pattern: SHACL / Vocabulary Validation

**Intended role:** Enforce open-ontology constraints at the membrane boundary.

| File | Location | Status |
|---|---|---|
| `crates/ggen-core/src/validation/shacl.rs` | SHACL validator implementation | **PARTIAL** |
| `crates/ggen-core/src/validation/validator.rs` | Generic validator trait | **LIVE** |
| `crates/ggen-core/src/membrane/shacl.rs` | SHACL at membrane boundary | **CAPABILITY_SEED** |
| `crates/ggen-graph/src/vocab/shacl.rs` | SHACL vocabulary types | **CAPABILITY_SEED** |
| `crates/ggen-core/tests/shacl_validation_test.rs` | SHACL validation tests | **PARTIAL** |
| `crates/ggen-core/tests/normalization_shacl_tests.rs` | Normalization + SHACL | **PARTIAL** |
| `crates/genesis-construct8/src/projectors.rs` | SHACL projector | **CAPABILITY_SEED** |

**Finish action:** Wire `ggen-core/membrane/shacl.rs` to refuse (emit `PlagueRecord::Hail`) on violation. The tests already specify the behavior.

---

## Pattern: Shard / Corpus / Segment

**Intended role:** Compositional layer above RelationPage. Shard = bounded RelationPage collection. Corpus = composed Shards.

| File | Location | Status |
|---|---|---|
| `crates/genesis-construct8/src/admission.rs` | Shard admission logic | **CAPABILITY_SEED** |
| `crates/genesis-construct8/src/hierarchy.rs` | Shard/Corpus hierarchy | **CAPABILITY_SEED** |
| `crates/genesis-construct8/src/lib.rs` | Shard/Corpus exported types | **CAPABILITY_SEED** |
| `crates/genesis-construct8/src/replay.rs` | Shard-level replay | **CAPABILITY_SEED** |

**Finish action:** `genesis-construct8` owns Shard/Corpus. Register it in the workspace. Wire to genesis-core-v2 via `GenesisAdapter`. Corpus composition requires receipt chain.

---

## Pattern: Doctor / Wizard / Telco

**Intended role:** Agent verbs — Doctor audits, Wizard scaffolds, Telco routes.

| File | Location | Status |
|---|---|---|
| `crates/ggen-cli/src/cmds/doctor.rs` | CLI doctor command | **LIVE** |
| `crates/ggen-cli/src/cmds/wizard.rs` | CLI wizard command | **LIVE** |
| `crates/ggen-core/src/domain/utils/doctor.rs` | Doctor domain logic | **LIVE** |
| `crates/ggen-graph/src/doctor/mod.rs` | Graph doctor | **PARTIAL** |
| `crates/ggen-graph/tests/ocel_diagnostics_doctor_test.rs` | Doctor diagnostic tests | **TEST_ONLY** |
| `tests/integration/test_telco_routing.rs` | Telco routing test | **CAPABILITY_SEED** |
| `tests/otel_validation/capabilities.rs` | OTel capability validation | **CAPABILITY_SEED** |

**Finish action:** Doctor in CLI is LIVE. Connect graph doctor to emit `ChurchJudgment` records. Wire doctor output to Seven Churches audit format.

---

## Pattern: PROV

**Intended role:** W3C PROV-DM provenance trace from receipts.

| File | Location | Status |
|---|---|---|
| `crates/ggen-core/src/membrane/prov.rs` | PROV at membrane | **CAPABILITY_SEED** |
| `crates/ggen-graph/src/ocel/prov_types.rs` | PROV types | **CAPABILITY_SEED** |
| `crates/ggen-graph/src/vocab/prov.rs` | PROV vocabulary | **CAPABILITY_SEED** |

**Finish action:** Connect `membrane/prov.rs` to `genesis-core-v2::Receipt` as the provenance binding. PROV wasEntity/wasGeneratedBy map to Construct8 acts.

---

## Summary: Pattern Coverage

| Pattern | LIVE files | CAPABILITY_SEED files | PARTIAL files | Action |
|---|---|---|---|---|
| Receipt | 4 | 7 | 2 | Unify behind genesis-core-v2 |
| Construct8 | 2 | 5 | 2 | Wire via GenesisAdapter |
| RelationPage | 4 | 2 | 2 | Connect membrane + projection |
| Replay | 2 | 4 | 0 | Connect to ReplayCursor |
| OCEL | 0 | 7 | 0 | Wire to receipt chain |
| SHACL | 2 | 3 | 3 | Refuse on violation |
| Shard/Corpus | 0 | 4 | 0 | Register genesis-construct8 |
| Doctor/Wizard/Telco | 3 | 2 | 1 | Emit ChurchJudgment |
| PROV | 0 | 3 | 0 | Bind to Receipt |
