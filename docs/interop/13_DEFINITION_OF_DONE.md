# Definition of Done

## 1. Scope
Defines the absolute criteria for completion across three distinct release tiers: v0.1, v0.2, and Vision 2030.

## 2. v0.1 Definition of Done (Architecture & Algebra)
**Goal: The math is safe and the rules are written.**
- [x] Genesis Core Spec exists and explicitly isolates `genesis-core` from IO.
- [x] ggen Foundry Spec exists and claims all membrane/adapter logic.
- [x] Interchangeable Part Spec exists (AtomVM/WASM/Genesis bounds).
- [x] Proof Surfaces documented (Packet, Segment, Shard).
- [x] All docs in `docs/interop/` are complete and link to actual code/artifacts or denote missing status.
- [x] No broad renames occurred without an explicit tracking plan.

## 3. v0.2 Definition of Done (Runtime & Validation)
**Goal: The part runs and proves itself.**
- [ ] `genesis-core` crate is physically separated and compiles to `wasm32-unknown-unknown` without OS dependencies.
- [ ] `ggen-membrane` successfully maps a JSON fixture to a `RelationPage`.
- [ ] Genesis successfully processes a `Construct8` packet and emits a BLAKE3 Packet Receipt.
- [ ] The `Need257` and `Need9` sabotage tests correctly trigger Refusal artifacts.
- [ ] `ggen-projection` outputs the result as valid OCEL 2.0 and N-Quads.
- [ ] CI pipeline validates the N-Quads via QLever/DuckDB without errors.

## 4. Vision 2030 Definition of Done (Enterprise Scale)
**Goal: The Blue River Dam is active.**
- [ ] AtomVM parts are deployed at the edge (e.g., IoT gateways, local runners) capturing local source motion.
- [ ] Truex lifecycle successfully promotes Segment receipts into an enterprise Shard.
- [ ] `wasm4pm` continuously monitors OCEL streams projected by live ggen membranes.
- [ ] Replay Cursors can successfully audit and reconstruct a 1,000,000-event history completely deterministically.
