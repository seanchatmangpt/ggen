# Phase 5: Capability Recovery and Finish — Swarm Mission Brief
**Date**: 2026-05-27  
**Status**: Ready for Launch  
**Swarm Size**: 5 agents (Explore, Plan, Execute tiers)  
**Mission Duration**: ~156 hours (medium/low-priority gaps)

---

## Mission Context

**Phase 1-4 COMPLETE**: 
- ✅ All 5 high-priority blockers closed (B1-B5)
- ✅ 5,043 LOC delivered
- ✅ 279 tests passing
- ✅ Phase 4 classification receipts issued
- ✅ One test bug identified (B3, < 1 hour to fix)

**NEW LAW ADOPTED**: 
- Non-Deletion Completion Protocol (constitutional)
- Code is fossil evidence, never deleted
- Nine-status taxonomy (LIVE, PARTIAL, CAPABILITY_SEED, LEGACY_NAME, DORMANT, BROKEN_BUT_REAL, DOC_ONLY, TEST_ONLY, AMBIGUOUS)
- Finish mindset: classify, connect, verify, receipt

**PHASE 5 MISSION**: Recover and finish all remaining capabilities using the new protocol.

---

## Swarm Structure

### Parallel Agent Wave 1: Inventory & Discovery (5 Explore Agents)

Each agent explores one domain and inventories ALL related code. Output: `*.md` discovery files.

| Agent | Domain | Inventory Targets | Output File |
|-------|--------|-------------------|-------------|
| **E1** | Receipt System | All Receipt, ReceiptEnvelope, receipt_*.rs, receipt tests, receipt docs | `RECEIPT_INVENTORY.md` |
| **E2** | Replay & Verification | All Replay, replay_test.rs, verification logic, gate testing | `REPLAY_INVENTORY.md` |
| **E3** | Genesis Constructs | Pair2, RelationPage, Construct8, genesis_*.rs, all tests | `GENESIS_INVENTORY.md` |
| **E4** | Parts & Manufacturing | PartType, InterchangeablePart, parts_foundry, adapter_generator, compiler | `PARTS_INVENTORY.md` |
| **E5** | Process & Projection | OCEL, PROV, process mining, drift detection, event projection | `PROCESS_INVENTORY.md` |

**Proof Format**:
```bash
# E1 proof of work:
$ grep -r "Receipt" crates/ --include="*.rs" | wc -l
147 matches

$ find . -name "*receipt*" -type f | head -20
crates/ggen-receipt/src/lib.rs
crates/ggen-core/src/receipt.rs
...

$ wc -l crates/ggen-receipt/src/*.rs
1,247 total

# Output file:
$ ls -la docs/interop/RECEIPT_INVENTORY.md
```

---

### Parallel Agent Wave 2: Classification & Pattern Atlas (5 Plan Agents)

Each agent takes one inventory and classifies all artifacts, then contributes to PATTERN_ATLAS.

| Agent | Input | Classification Task | Output File |
|-------|-------|-------------------|-------------|
| **P1** | RECEIPT_INVENTORY.md | Classify all Receipt code by status; identify CAPABILITY_SEEDs | `RECEIPT_CLASSIFICATION.md` |
| **P2** | REPLAY_INVENTORY.md | Classify all Replay code; find unfinished capabilities | `REPLAY_CLASSIFICATION.md` |
| **P3** | GENESIS_INVENTORY.md | Classify Pair2/RelationPage/Construct8; unify if scattered | `GENESIS_CLASSIFICATION.md` |
| **P4** | PARTS_INVENTORY.md | Classify parts manufacturing pipeline by status | `PARTS_CLASSIFICATION.md` |
| **P5** | PROCESS_INVENTORY.md | Classify projection/drift/mining code; identify DORMANT | `PROCESS_CLASSIFICATION.md` |

**After all P agents complete, the lead agent (P1) aggregates into:**

### Master Artifacts

1. **`PATTERN_ATLAS.md`** (synthesized from all P outputs)
   
   Example rows:
   ```markdown
   | Pattern | Files | Status | Intended Role | Finish Action |
   |---------|-------|--------|---------------|---------------|
   | Receipt | `crates/ggen-receipt/`, `crates/ggen-core/src/receipt.rs` | PARTIAL | Proof surface | Unify facade, add manifest |
   | Replay | `tests/*/replay_test.rs`, `src/replay.rs` | CAPABILITY_SEED | Reproducibility | Wire to receipt, add harness |
   | Construct8 | `src/genesis.rs` | LIVE | Bounded construction | Document contract, export |
   | OCEL | `src/ocel/exporter.rs`, `tests/ocel_test.rs` | CAPABILITY_SEED | Process output | Connect to wasm4pm bridge |
   | DORMANT AtomVM | `erlang/supervisor.erl` | DORMANT | Actor shell | Archive manifest, preserve bytes |
   ```

2. **`LEGACY_NAME_MAP.md`** (cross-domain old→new naming)
   
   ```markdown
   | Old Name | New Name | Context | Migration Path |
   |----------|----------|---------|-----------------|
   | `emit_receipt()` | `receipt_emit()` | ggen-receipt | Alias exists, doc the change |
   | `Doctor` | `ReceiptValidator` | process | Re-export old name from new module |
   ```

3. **`DORMANT_CODE_REGISTER.md`** (preserved but not deployed)
   
   ```markdown
   | Code | Location | Last Status | Reason Dormant | Preservation Action |
   |------|----------|-------------|-----------------|---------------------|
   | AtomVM supervisor | `erlang/supervisor.erl` | Partial | Runtime not deployed | Keep in source, archive manifest |
   ```

4. **`CAPABILITY_SEED_BACKLOG.md`** (prioritized finish work)
   
   Ordered by effort and impact:
   ```markdown
   ## P0 (Quick wins, < 5 hours each)
   - [ ] Finish receipt facade (CAPABILITY_SEED → LIVE)
   - [ ] Wire replay to receipt integration test
   
   ## P1 (Medium, 5-20 hours)
   - [ ] Complete OCEL projection bridge to wasm4pm
   - [ ] Fix B3 receipt deduplication test bug
   
   ## P2 (Longer, 20+ hours)
   - [ ] Unify PROV and OCEL under common projection interface
   - [ ] Implement full drift detection pipeline
   ```

---

### Parallel Agent Wave 3: Finish Execution (5+ Execute Agents)

Each agent takes a capability seed from the backlog and finishes it using the Non-Deletion protocol.

**Per-Capability Workflow**:

```
TASK: Finish Receipt Facade (CAPABILITY_SEED → LIVE)

1. INVENTORY
   - Find all Receipt implementations
   - Count lines, tests, docs
   - Identify scattered definitions
   
2. CLASSIFY
   - ggen-receipt/src/lib.rs → LIVE
   - ggen-core/src/receipt.rs → PARTIAL
   - tests/receipt_test.rs → LIVE
   - docs/receipt.md → DOC_ONLY
   
3. RECOVER
   - Identify existing interface contract
   - Find nearest LIVE implementation
   - Retrieve test specification
   
4. CONNECT
   - Add facade module that wraps both impls
   - Export unified Receipt type
   - Add compatibility alias
   - Wire tests to new facade
   
5. VERIFY
   - cargo test --lib receipt 2>&1 | grep "test result"
   - cargo doc --no-deps --open (verify exported types)
   - cargo clippy (no warnings)
   
6. RECEIPT
   - Modified files: 3 (receipt.rs, facade.rs, tests)
   - New: 1 (facade.rs, 42 LOC)
   - Tests: 15 passing, 0 failing
   - Evidence: commit hash + git show
```

**Execution Order** (by impact/effort ratio):

1. Receipt facade (3h)
2. Replay integration test (2h)
3. Fix B3 deduplication bug (1h)
4. OCEL→wasm4pm bridge (8h)
5. PROV unification (10h)
6. Drift detection pipeline (20h)
... and 14 more gaps from PLAN_GAPS.md

---

## Success Criteria

✅ **Phase 5 Complete when:**

1. **All inventories done**
   - 5 inventory files exist (RECEIPT_, REPLAY_, GENESIS_, PARTS_, PROCESS_)
   - All related code found (grep verified)

2. **All classification done**
   - 5 classification files exist
   - PATTERN_ATLAS.md synthesized (all patterns mapped)
   - No AMBIGUOUS artifacts (all classified or investigation task created)

3. **All high-priority seeds finished**
   - CAPABILITY_SEED_BACKLOG.md P0 & P1 sections done (15+ items)
   - Compile, test, doc all passing
   - Receipts issued for each finished capability

4. **No code deleted**
   - All finished code is additive (no deletions)
   - DORMANT code archived-in-place with manifest
   - LEGACY_NAMEs aliased, not removed

5. **Full documentation**
   - CAPABILITY_INVENTORY.md (6 files, master index)
   - PATTERN_ATLAS.md (all patterns + finish actions)
   - LEGACY_NAME_MAP.md (migration docs)
   - DORMANT_CODE_REGISTER.md (preservation manifest)
   - CAPABILITY_SEED_BACKLOG.md (prioritized work)
   - ADDITIVE_FINISH_PLAN.md (smallest patches for each capability)

---

## Output Artifacts

After Phase 5, the swarm produces:

```
docs/interop/
├── CAPABILITY_INVENTORY.md        # What can the repo do?
├── PATTERN_ATLAS.md               # Where do patterns repeat?
├── LEGACY_NAME_MAP.md             # Old names → current
├── DORMANT_CODE_REGISTER.md       # Preserved but not promoted
├── CAPABILITY_SEED_BACKLOG.md     # Unfinished capabilities (prioritized)
├── ADDITIVE_FINISH_PLAN.md        # Smallest patches needed
├── RECEIPT_INVENTORY.md
├── RECEIPT_CLASSIFICATION.md
├── REPLAY_INVENTORY.md
├── REPLAY_CLASSIFICATION.md
├── GENESIS_INVENTORY.md
├── GENESIS_CLASSIFICATION.md
├── PARTS_INVENTORY.md
├── PARTS_CLASSIFICATION.md
├── PROCESS_INVENTORY.md
└── PROCESS_CLASSIFICATION.md
```

Plus all code changes committed to feat/autonomic-actuation:
- New facades, adapters, wrappers
- Completed PARTIAL implementations
- Fixed BROKEN_BUT_REAL code
- Wired CAPABILITY_SEEDs
- Tests added, all passing
- Receipts for each change

---

## The Operating Principle

> **Agents are archaeologists finishing a manufacturing line from parts already present.**

**Not**: "Clean up broken code by deleting it."

**Instead**: "Identify what's already growing, graft the living branches, fence the boundary, and make the system bear fruit."

---

## Agent Prerequisites

Before swarm deployment, every agent must:

- [✅] Read NON_DELETION_COMPLETION_PROTOCOL.md (50 minutes)
- [✅] Understand the 9-status taxonomy (10 minutes)
- [✅] Commit to zero deletions (binding oath)
- [✅] Practice classification on 1 example artifact (15 minutes)
- [✅] Understand RESEARCH→CLASSIFY→PATCH→VERIFY→RECEIPT→REFUSE loop (20 minutes)

---

## Timeline Estimate

| Phase | Agents | Duration | Key Output |
|-------|--------|----------|-----------|
| **Inventory** (E1-E5) | 5 Explore | 8-12 hours | 5 inventory files |
| **Classification** (P1-P5) | 5 Plan | 12-16 hours | PATTERN_ATLAS.md + 5 classifications |
| **Finish (Waves)** (X1-X5+) | 5-10 Execute | 120-156 hours | All P0/P1/P2 seeds done |
| **Total** | 15-20 agents | 140-184 hours | Full capability recovery |

**Parallel execution** (all phases can overlap):
- Start E agents immediately
- Start P agents after first 2 E agents finish
- Start X agents after first 2 P agents finish
- Overlap: E (8h) + P (12h) + X (120h) ≈ **140 hours wall clock** (not serial)

---

## Next Steps (When Ready)

1. **User approves Phase 5 scope** (this brief)
2. **Launch 5 Explore agents in parallel** (inventory all domains)
3. **Monitor and release phase 2 & 3 agents as phase 1 completes**
4. **Issue receipts for each finished capability**
5. **Synthesize final capability atlas**
6. **Prepare for release or Phase 6** (if needed)

---

**This brief is the mission contract for Phase 5 execution.**

Swarm standing by for deployment authorization. ⚙️

