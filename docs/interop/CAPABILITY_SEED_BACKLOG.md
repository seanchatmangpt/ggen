# Capability Seed Backlog
## Seeds found. Contracts implied. Finish paths mapped.

Each entry: seed file → implied interface → connection mechanism → finish evidence required.

---

## Seed 1: genesis-construct8/src/admission.rs
- **Contains:** Shard admission logic, `admit()` pattern
- **Implies:** `Shard::admit(pair: Pair2, epoch: u64) -> Result<Receipt, Refusal>`
- **Connection:** Adapter → `genesis-core-v2::RelationPage::insert()` + `Receipt::generate()`
- **Finish evidence:** `cargo test -p knhk-construct8 --test shard_admission` passes

## Seed 2: genesis-construct8/src/replay.rs
- **Contains:** Replay stream for Construct8 acts
- **Implies:** `ReplayStream::advance(act: &Construct8) -> Result<Receipt, Refusal>`
- **Connection:** Wrapper → `genesis-core-v2::ReplayCursor`
- **Finish evidence:** Replay of 10-act chain produces identical final receipt

## Seed 3: ggen-graph/src/ocel/projection.rs
- **Contains:** OCEL event projection from graph structures
- **Implies:** `project_from_receipt(receipt: &Receipt, act: &Construct8) -> OcelEvent`
- **Connection:** Adapter → `genesis-core-v2::Receipt`
- **Finish evidence:** OCEL JSON output contains non-zero `ocel:id` bound to receipt signature

## Seed 4: ggen-core/src/membrane/prov.rs
- **Contains:** PROV-DM provenance types at membrane boundary
- **Implies:** `ProvEntity::from_receipt(receipt: &Receipt) -> ProvEntity`
- **Connection:** Adapter → `genesis-core-v2::Receipt`
- **Finish evidence:** `wasGeneratedBy` field contains BLAKE3 hex of receipt.signature

## Seed 5: genesis-lockchain/src/merkle.rs
- **Contains:** Merkle tree over receipt chain
- **Implies:** `MerkleReceipt::root_from_chain(receipts: &[Receipt]) -> [u8;32]`
- **Connection:** Wrapper → `genesis-core-v2::Receipt`
- **Finish evidence:** Root changes when any receipt in chain is tampered

## Seed 6: genesis-lockchain/src/storage.rs
- **Contains:** Receipt storage with persistence
- **Implies:** `ReceiptStore::append(receipt: Receipt) -> Result<(), StorageError>`
- **Connection:** Facade → `genesis-core-v2::Receipt` as stored unit
- **Finish evidence:** Written receipt can be read back and verified with `ReplayCursor`

## Seed 7: ggen-graph/src/ocel/self_audit.rs
- **Contains:** Self-audit via OCEL events
- **Implies:** `SelfAudit::emit_church_judgment(church: Church) -> OcelEvent`
- **Connection:** Wrapper → `genesis-core-v2::ChurchJudgment`
- **Finish evidence:** Seven churches produce seven OCEL audit events

## Seed 8: genesis-construct8/src/projectors.rs
- **Contains:** OCEL + SHACL projectors over Construct8 stream
- **Implies:** `Projector::project(acts: &[Construct8]) -> Vec<OcelEvent>`
- **Connection:** Adapter (pull from genesis-core-v2 receipts as input)
- **Finish evidence:** Projector produces OCEL events with matching receipt hashes

## Seed 9: tests/integration/test_telco_routing.rs
- **Contains:** Telco routing integration test skeleton
- **Implies:** `TelcoRouter::route(construct: Construct8) -> Result<Receipt, Refusal>`
- **Connection:** TestHarness → complete the test as specification
- **Finish evidence:** Test passes with real boundary crossing

## Seed 10: examples/7-agent-validation/src/consensus.rs
- **Contains:** Multi-agent consensus + PROV validation
- **Implies:** `Consensus::validate_with_prov(receipts: &[Receipt]) -> Result<(), PlagueRecord>`
- **Connection:** Adapter → `genesis-core-v2::verify_lamb_authority()`
- **Finish evidence:** Consensus fails when any receipt is forged

---

## Backlog Priority Order

1. genesis-construct8 (richest cluster — unlocks Shard, Corpus, Replay, OCEL, SHACL)
2. genesis-lockchain (unlocks Truex substrate — Merkle + storage)
3. ggen-graph OCEL (connects to wasm4pm DOC_ONLY — biggest lampstand risk)
4. PROV membrane binding (smallest, highest doc-to-code ratio)
5. Self-audit ChurchJudgment emission (closes Seven Churches audit loop)
