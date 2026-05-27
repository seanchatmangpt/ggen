# Non-Deletion Completion Protocol
**Status**: Constitutional Law for All Future Coding Agents  
**Adopted**: 2026-05-27  
**Principle**: Code is fossil evidence. Nothing is removed. Everything is classified, connected, and finished.

---

## The Operating Philosophy

> **No code is deleted. Code either becomes admitted capability, refusal evidence, dormant artifact, or legacy compatibility surface.**

This inverts the traditional cleanup/refactoring mindset:

| Old Mindset | New Mindset |
|-------------|------------|
| "Remove dead code" | "Classify dormant code" |
| "Rewrite messy code" | "Connect to existing capability seed" |
| "Delete old names" | "Map legacy names for compatibility" |
| "Clean slate is better" | "Finish from existing patterns" |
| "Prove code is useless" | "Recover the intent it contains" |

---

## Hard Rules (Non-Negotiable)

1. **No file deletion** — Files are inventoried and classified, never removed
2. **No module deletion** — Modules are wrapped, not discarded
3. **No test deletion** — Tests are quarantined or connected, never erased
4. **No API removal** — Public APIs get compatibility aliases if renamed
5. **No rewrite from scratch** — Existing capability seeds are always preferred over clean implementations
6. **No replacement without preservation** — If a cleaner abstraction is built, the old path remains available
7. **No "dead code" claims without proof** — Must demonstrate: no references, no tests, no docs, no architectural pattern value
8. **No deletion as shortcut** — Even broken code stays, marked BROKEN_BUT_REAL with repair path

---

## Status Taxonomy (9 Labels)

Every file, crate, module, test, script, doc, and config gets exactly one:

| Status | Meaning | Allowed Actions | Example |
|--------|---------|-----------------|---------|
| **LIVE** | Used and working | preserve, test, document, improve | `ggen-core/src/pipeline.rs` |
| **PARTIAL** | Real but unfinished | finish by smallest patch, add tests | `crates/ggen-marketplace/src/receipt.rs` (half-implemented) |
| **CAPABILITY_SEED** | Contains useful pattern but not wired | extract contract, add wrapper, wire to caller | `src/old_receipt.rs` (Receipt pattern but unused) |
| **LEGACY_NAME** | Old name for current concept | keep alias, document migration path | `fn emit_receipt()` → alias to `fn receipt_emit()` |
| **DORMANT** | Not currently used but potentially useful | archive-in-place with manifest, keep bytes intact | `experimental/wasm_loader.rs` (not deployed but real code) |
| **BROKEN_BUT_REAL** | Fails compile/test but has intent | isolate in feature gate, repair with smallest patch, document failure reason | `src/anomaly_detector.rs` (fails on threshold, intent clear) |
| **DOC_ONLY** | Described but not implemented | connect to nearest capability seed, use doc as requirement | `docs/api/receipt_lifecycle.md` (spec exists, code doesn't) |
| **TEST_ONLY** | Test reveals intended behavior | use test as specification, implement to pass test | test file shows Receipt schema but impl missing |
| **AMBIGUOUS** | Unclear purpose | do not change until classified; create task to investigate | old module with unclear lifecycle |

**NO DELETE CATEGORY ALLOWED.**

---

## Required Work Loop (Per Task)

### Phase 1: Inventory
```bash
# Find all related artifacts
grep -r "Receipt" crates/ --include="*.rs" | wc -l
find . -name "*receipt*" -type f
grep -r "pattern_name" . --include="*.rs" --include="*.md" --include="*.toml"

# Output: Complete list of files, modules, tests, docs, scripts
```

### Phase 2: Classify
```
For each artifact found:
  - Is it compiled? → LIVE or PARTIAL
  - Is it tested? → LIVE or TEST_ONLY
  - Is it in docs only? → DOC_ONLY
  - Does it contain a pattern? → CAPABILITY_SEED
  - Is it named differently now? → LEGACY_NAME
  - Is it not deployed? → DORMANT
  - Does it fail compilation? → BROKEN_BUT_REAL
  - Is its purpose unclear? → AMBIGUOUS
```

### Phase 3: Recover
```
Identify the capability already present.

Question every artifact:
  - What can this code already do?
  - What interface does it imply?
  - What test already exists?
  - What doc claims it supports?
  - What crate/module owns it?
  - What wrapper would make it usable?
  - What is missing to complete it?

Do NOT invent a new capability first.
Always prefer finishing existing seeds over building from scratch.
```

### Phase 4: Connect
```
Use the smallest additive patch to wire existing code:
  - Wrapper (new module around old)
  - Facade (unified interface for multiple implementations)
  - Adapter (translate between old and new contract)
  - Module export (make internal code public)
  - Compatibility alias (fn old_name() = new_name())
  - Test harness (quarantine broken tests, add repair step)
  - Manifest entry (document in inventory file)
  - Doc link (connect spec to implementation)
  - Feature gate (gate incomplete features behind #[cfg(feature)])
  - Integration path (wire to caller)

Goal: Smallest additive change. Never rewrite. Never delete.
```

### Phase 5: Verify
```
cargo check        # Compilation
cargo test --lib   # Unit tests
cargo test --doc   # Doc examples (if applicable)
cargo clippy       # Lint
manual test        # Is the wired capability usable?
```

### Phase 6: Receipt
```
Document:
  - Files inspected (count + paths)
  - Artifacts classified (count per status)
  - Capability seeds found (names + locations)
  - Files changed (paths + line diffs)
  - Code preserved (what was kept)
  - Tests run (pass/fail counts)
  - Results (working, partial, broken-but-real)
  - Remaining gaps (explicit list)

Receipt format: JSON or Markdown manifest
```

---

## Completion Standard

**A task is complete only when:**

- [✅] All related code is inventoried (nothing missed)
- [✅] Every artifact is classified (no AMBIGUOUS remaining)
- [✅] Existing capability seeds are identified (not reinvented)
- [✅] Code is connected (not replaced)
- [✅] Compile and tests pass (or failures are isolated + receipted)
- [✅] Preservation is documented (manifest of what was kept)
- [✅] Remaining gaps are explicit (no silent deletions)
- [✅] Receipt proves the work (files, commands, results)

**No deletion is a feature, not a constraint.**

---

## Genesis Alignment

This protocol mirrors Genesis:

| Genesis Concept | Codebase Practice |
|-----------------|-------------------|
| **Candidate observation** | Existing code artifact (any status) |
| **Admission** | LIVE + tested + documented capability |
| **Refusal** | Explicit BROKEN_BUT_REAL with repair reason |
| **Replay** | Test or command reproduces behavior |
| **Receipt** | Changed files + commands + results |
| **Shard** | Grouped capability family (crate, module) |
| **Corpus** | Whole repo portfolio |
| **Dormant matter** | Preserved DORMANT code, not promoted |

**Result**: The codebase becomes a legible artifact of capability attempts, not a trash heap awaiting cleanup.

---

## Agent Execution Checklist

Before coding agents start Phase 5, they must:

- [✅] Read this protocol in full
- [✅] Understand the 9-status taxonomy
- [✅] Commit to zero deletions
- [✅] Practice on one PARTIAL capability (finish-first mindset)
- [✅] Document their capability inventory (CAPABILITY_INVENTORY.md)
- [✅] Generate a PATTERN_ATLAS.md mapping all repeated structures
- [✅] Create LEGACY_NAME_MAP.md for old names
- [✅] List DORMANT code in DORMANT_CODE_REGISTER.md
- [✅] Prioritize CAPABILITY_SEED_BACKLOG.md for finish work
- [✅] Draft ADDITIVE_FINISH_PLAN.md (smallest patches, no deletes)

---

## Swarm Mission (Phase 5+)

**NEW MISSION NAME**: Capability Recovery and Finish

**PRIMARY OUTPUTS**:
1. `docs/interop/CAPABILITY_INVENTORY.md` — What can the repo do?
2. `docs/interop/PATTERN_ATLAS.md` — Where do patterns repeat?
3. `docs/interop/LEGACY_NAME_MAP.md` — Old names for current concepts
4. `docs/interop/DORMANT_CODE_REGISTER.md` — Preserved but not promoted
5. `docs/interop/CAPABILITY_SEED_BACKLOG.md` — Unfinished capabilities
6. `docs/interop/ADDITIVE_FINISH_PLAN.md` — Smallest patches to connect

**KEY ARTIFACT**: Pattern Atlas

Example entry:

```markdown
| Pattern | Files | Current Status | Intended Role | Finish Action |
|---------|-------|---|---|---|
| Receipt | `crates/ggen-receipt/`, `crates/ggen-core/src/receipt.rs` | PARTIAL | Proof surface | Unify facade, add receipts manifest |
| Replay | `tests/*/replay_test.rs`, `src/replay.rs` | CAPABILITY_SEED | Reproducibility | Add replay manifest, wire to receipt |
| Construct8 | `src/genesis/construct8.rs` | LIVE/PARTIAL | Bounded construction | Add interop contract, export |
| AtomVM custody | `erlang/supervisor.erl`, `src/parts/atomvm.rs` | PARTIAL | Actor shell | Add part manifest, connect to ggen |
| OCEL projection | `src/ocel/exporter.rs`, `tests/ocel_test.rs` | CAPABILITY_SEED | Process output | Connect to wasm4pm, add bridge |
```

---

## Operating Line

> **The agents are not here to clean the garden by uprooting things. They are here to identify what is already growing, graft the living branches, fence the boundary, and make the system bear fruit.**

Or colder:

> **No deletion. Only classification, connection, completion, receipt, replay, and refusal.**

---

**This protocol is now THE LAW for all coding agents on this project.**

No exceptions. No rewrites. No deletes. Only finish.

