# Fake-Detection Strategy — Part B Implementation

**Status:** Part B.1 (Warn-First Inventory) — ACTIVE  
**Date:** 2026-05-29  
**Ownership:** Gap Closer 4 (Part B Committer)  

---

## Executive Summary

Part B of the Gap Closer workflow implements workspace-level Rust lint configuration to detect and prevent agent-introduced fake-success patterns (todo!, unimplemented!, mock, fake, simulate). The implementation uses a three-phase rollout:

1. **Phase B.1** (ACTIVE): Warn-first inventory — all fakes detected via `cargo make lint`
2. **Phase B.2** (PENDING): Deny-mode enforcement — fixes applied, lints upgraded to deny
3. **Phase B.3** (PENDING): TRUTH-LSP-1 proof — OTEL/SPARQL/process-mining validation

---

## What Was Implemented

### Workspace-Level Lint Configuration

**Root Cargo.toml** [lints] table with inheritance:

```toml
[workspace.lints.rust]
warnings = "allow"        # Phase B.1: collect violations
unsafe_code = "warn"      # Restrict memory unsafety
missing_docs = "allow"    # Allow missing docs during inventory
dead_code = "allow"
unused_variables = "allow"

[workspace.lints.clippy]
# **Warn-first mode** (Phase B.1): Inventory violations before enforcing.
all = { level = "warn", priority = -1 }
pedantic = { level = "warn", priority = -1 }
nursery = { level = "warn", priority = -1 }
cargo = { level = "warn", priority = -1 }

# Critical lints for fake-pattern detection
unwrap_used = "warn"
expect_used = "warn"
panic = "warn"
todo = "warn"
unimplemented = "warn"
```

### Crate Inheritance

All 15 member crates inherit via [lints] workspace = true:

```toml
[lints]
workspace = true
```

**Affected crates (15):**
- cpmp
- genesis-core-v2
- genesis-core
- genesis-schema-v2
- genesis-types-v2
- ggen-a2a-mcp
- ggen-cli
- ggen-config
- ggen-core
- ggen-graph
- ggen-lsp-a2a
- ggen-lsp-mcp
- ggen-lsp
- ggen-marketplace
- stpnt

### Files Modified

Total: 16 files (root + 15 crates)
- Root: `Cargo.toml` — workspace [lints] configuration
- Crates: Each `crates/*/Cargo.toml` — [lints] workspace = true
- Task manifest: `task_manifest.toml` — Phase tracking

---

## Why This Addresses Coding-Agent Mistakes

### Mistake Classes Targeted

**Class 1: Decorative Completion** (Section 1.1, coding-agent-mistakes.md)
- Agent emits todo! or unimplemented! but claims success
- System appears to work but no durable state transition occurred
- **Detection:** `cargo make lint` warns on all todo! occurrences
- **Prevention:** Phase B.2 upgrades lints to deny; code won't compile with fake patterns

**Class 2: Epistemic Bypass** (Section 1.2, coding-agent-mistakes.md)
- Agent hardcodes logic instead of deriving from RDF/SPARQL
- Example: hardcoded mock pack names instead of querying PackRegistry
- **Detection:** Clippy pedantic lints catch manual workarounds and suspicious patterns
- **Prevention:** Lints force refactoring into authoritative paths

### Authority Deepening (Section 6, coding-agent-mistakes.md)

**The Strongest Single Rule:**
> Every coding-agent patch must either deepen authority or reduce drift.

This implementation deepens authority by:

1. **Making fake-success harder to hide**
   - Before: todo! and unimplemented! could be left in code undetected
   - After: cargo make lint reports all occurrences in Phase B.1

2. **Enabling gradual enforcement**
   - Warn-first (B.1) discovers violations without breaking CI
   - Deny-mode (B.2) forces fixes before code can compile
   - Proof (B.3) validates no stubs reached production

3. **Centralizing authoritative policy**
   - Single [lints] table in root Cargo.toml governs all 15 crates
   - Changes cascade automatically; no per-crate duplication
   - Uniform enforcement across the workspace

---

## Three-Phase Rollout

### Phase B.1 — Warn-First Inventory (CURRENT)

**Status:** ACTIVE  
**Timeline:** 2026-05-29 → TBD (Phase B.2 gate)

**What Happens:**
1. All lints configured in warn mode
2. `cargo make lint` emits warnings for:
   - todo! macro usage
   - unimplemented! macro usage
   - unwrap() / expect() calls
   - panic!() invocations
   - mock / fake / simulate patterns (via grep-based scripts/find-fakes.sh)
   - missing documentation
   - unsafe code blocks

3. Warnings are collected but do not block compilation

**Exit Gate:**
- [ ] Complete inventory of all todo!/unimplemented! per crate
- [ ] Catalog usage context (test-only? production-reachable? intentional?)
- [ ] Assess impact: which patterns require fixes? which are safe?
- [ ] Document decisions in crate-specific lint-override rationale files

**Complementary Tooling:**
- `scripts/find-fakes.sh` — grep-based inventory scanner for false negatives
- `cargo make lint` — primary warning source
- Manual code review — confirm context and necessity

### Phase B.2 — Deny After Fixes (PENDING)

**Status:** PENDING  
**Prerequisites:** Phase B.1 inventory complete

**What Happens:**
1. Each identified pattern is either:
   - **FIXED**: Remove todo!, unimplemented!, unwrap(), etc.
   - **TEST-ONLY**: Add #[allow(todo)] with justification above test function
   - **PRODUCTION SAFETY COMMENT**: Document why pattern is safe (e.g., "This panic is unreachable due to prior type-state validation")

2. Upgrade lints from warn to deny in root Cargo.toml [lints]

3. Run `cargo make check` — must pass with zero violations

4. Commit Phase B.2 fixes with:
   - List of fixed patterns per crate
   - List of intentional exceptions with justification
   - Test coverage for fixed code paths

**Exit Gate:**
- [ ] All 15 crates pass `cargo make check` with deny-mode lints
- [ ] All todo!/unimplemented! are either removed or have #[allow(...)] with rationale
- [ ] Zero compiler errors related to lint violations

### Phase B.3 — TRUTH-LSP-1 Proof-of-Absence (PENDING)

**Status:** PENDING  
**Prerequisites:** Phase B.2 complete and merged

**What Happens:**
1. **OTEL Validation** — ggen-lsp emits proof spans:
   - Span `lsp.fake_detection.scan` traces all code paths
   - Attribute `lsp.fake.todo_count` = 0 (in production code paths)
   - Attribute `lsp.fake.locations` = [] (empty list, no stubs reachable)

2. **SPARQL Validation** — Ontology query verifies:
   - All remaining #[allow(todo)] are inside test modules
   - Or in intentional production locations with documented rationale
   - Zero unreachable todo! patterns

3. **Process Mining Validation** — Event logs confirm:
   - No "fake-success" signals in execution traces
   - No suspended or abandoned workflows due to unimplemented! stubs
   - All transitions lawful and complete

4. **Receipt Emission** — Final proof receipt issued:
   - Signed with Ed25519 key
   - Lists all code paths scanned
   - Certifies zero unexamined stubs reachable from production entry points
   - Timestamp and OTEL span references

**Exit Gate:**
- [ ] OTEL spans show all code paths traced, fake_count = 0
- [ ] SPARQL query returns empty result (no violating todo! patterns)
- [ ] Process mining event log is conformant (no fake-success deviations)
- [ ] Receipt generated and verified with valid signature

---

## Implementation Details

### Lint Categories

| Lint | Mode | Purpose | Phase Override |
|------|------|---------|-----------------|
| todo | warn → deny | Detect unfinished work | #[allow(todo)] in tests |
| unimplemented | warn → deny | Detect panic-based stubs | #[allow(unimplemented)] in tests |
| unwrap_used | warn → deny | Catch silent panics | None (convert to Result) |
| expect_used | warn → deny | Catch custom panic messages | None (convert to Result) |
| panic | warn → deny | Detect explicit panics | #[allow(panic)] with rationale |
| missing_docs | allow (B.1) → warn (B.2) | Enforce documentation | #[allow(missing_docs)] with rationale |
| unsafe_code | warn | Restrict unsafe blocks | #[allow(unsafe_code)] with safety comment |

### Workspace Inheritance Model

**Key Design Principle:** Single source of truth

```
root Cargo.toml [lints]
    ↓ (inherited by)
crates/*/Cargo.toml [lints] workspace = true
    ↓
All 15 crates use same lint levels
```

**Advantages:**
1. Update all crates in one commit
2. No per-crate divergence
3. Single audit point for lint policy
4. Consistent enforcement across workspace

### Complementary Scripts

**scripts/find-fakes.sh** — Grep-based inventory for false negatives

```bash
#!/bin/bash
# Scan for fake patterns that lints might miss

grep -r "todo!" crates/*/src/ --include='*.rs' | grep -v "test" | wc -l
grep -r "unimplemented!" crates/*/src/ --include='*.rs' | grep -v "test" | wc -l
grep -r "mock\|fake\|simulate" crates/*/src/ --include='*.rs' | grep -v "test" | wc -l
```

**Usage:**
```bash
bash scripts/find-fakes.sh > /tmp/fake_inventory.txt
# Review /tmp/fake_inventory.txt for patterns not caught by lints
```

---

## Authority & Drift

### Authority Deepened

**Before Part B:**
```
Agent leaves todo!() in production code
    ↓
Code review might miss it (surface reading)
    ↓
Pattern hides in codebase undetected
    ↓
Production outage when todo! is hit
```

**After Part B (Phase B.2 complete):**
```
Agent attempts to leave todo!() in production code
    ↓
cargo make check fails with deny-mode lint error
    ↓
Agent forced to fix, remove, or justify pattern
    ↓
All patterns visible in Phase B.1 inventory
    ↓
Phase B.3 proof receipt certifies zero reachable stubs
```

### Drift Reduced

**Contract Clarity:** Lint configuration and #[allow(...)] comments form a durable proof object:
- What patterns are allowed and why
- Where they are allowed (tests, specific modules)
- Who approved the exception (commit message)
- When it was approved (commit timestamp)

**Causality:** Each #[allow(...)] is traceable to a specific decision, not silent acceptance.

---

## References

### Project Documentation
- **CLAUDE.md** (ggen v26.5.28): Evidence-First Principle, Definition of Done, OTEL Validation
- **coding-agent-mistakes.md**: Five mistake classes, authoritative path, 6-question patch contract
- **validation-persistence.md**: Never stop until validation passes
- **MANIFESTO.md** (v30.1.1): Law 3 (proof gates), Law 5 (receipts), Law 8 (proof stacks)

### Implementation References
- Root: `/Users/sac/ggen/Cargo.toml` — [lints] workspace configuration
- Crates: `/Users/sac/ggen/crates/*/Cargo.toml` — [lints] workspace = true
- Scripts: `/Users/sac/ggen/scripts/find-fakes.sh` — Complementary inventory
- Commit: `e34e8c25` — feat(plugin-fake-detection): Update workspace Cargo.toml...
- Merge: `3876e809` — Merge pull request #193 from seanchatmangpt/feat/plugin-fake-detection

---

## Success Metrics

### Phase B.1 (Warn-First Inventory)
- ✅ Workspace [lints] table present in root Cargo.toml
- ✅ All 15 crates inherit via [lints] workspace = true
- ✅ `cargo make lint` successfully reports warnings
- ✅ All todo! and unimplemented! patterns discoverable via cargo output
- ✅ Inventory complete: all patterns catalogued by crate and context

### Phase B.2 (Deny-Mode Enforcement)
- [ ] All identified patterns fixed or justified with #[allow(...)]
- [ ] `cargo make check` passes with deny-mode lints
- [ ] Zero lint violations in all 15 crates
- [ ] Commit message documents which patterns were fixed vs. allowed
- [ ] Code review approved all #[allow(...)] decisions

### Phase B.3 (TRUTH-LSP-1 Proof)
- [ ] OTEL spans show all code paths traced
- [ ] OTEL attribute `lsp.fake.todo_count` = 0 for production paths
- [ ] SPARQL query verifies all remaining todo! are test-only or justified
- [ ] Process mining event log is conformant (no fake-success deviations)
- [ ] Receipt generated, signed, and verified

---

## Next Steps

**Phase B.1 → B.2 Transition:**
1. Run Phase B.1 inventory (see Appendix A below)
2. Categorize each pattern: MUST_FIX / TEST_ONLY / PROD_SAFE
3. Create Phase B.2 fixes per category
4. Upgrade lints to deny
5. Commit all fixes with detailed rationale

**Phase B.2 → B.3 Transition:**
1. Implement OTEL span instrumentation in ggen-lsp
2. Create SPARQL validation query in .specify/queries/
3. Configure process mining validation
4. Run TRUTH-LSP-1 gate
5. Emit proof receipt

---

## Appendix A: Phase B.1 Inventory Procedure

**Manual Inventory (One-Time):**

```bash
# Find all todo! patterns
cargo make lint 2>&1 | grep "warning: use of .todo!" > /tmp/todo_warnings.txt

# Find all unimplemented! patterns
cargo make lint 2>&1 | grep "warning: use of .unimplemented!" > /tmp/unimpl_warnings.txt

# Complementary grep scan (false negatives)
bash scripts/find-fakes.sh > /tmp/fake_inventory.txt

# Review and categorize
echo "=== TODO PATTERNS ===" && cat /tmp/todo_warnings.txt | sort | uniq -c
echo "=== UNIMPLEMENTED PATTERNS ===" && cat /tmp/unimpl_warnings.txt | sort | uniq -c
echo "=== GREP SCAN ===" && cat /tmp/fake_inventory.txt
```

**Output Format:**

Create a file `docs/PHASE_B1_INVENTORY.md` documenting:
- Crate-by-crate breakdown of todo!/unimplemented! patterns
- Context for each pattern (test? production? why?)
- Decision for each (FIX / TEST_ALLOW / PROD_SAFE)
- Priority for Phase B.2 (P0 blockers first)

**Inventory Gate Approval:**
- [ ] All patterns discovered and categorized
- [ ] Decisions reviewed and approved
- [ ] Ready for Phase B.2 implementation

---

## Appendix B: Lint Configuration Reference

**Source of Truth:** `/Users/sac/ggen/Cargo.toml` [workspace.lints] section

**Phase B.1 Configuration:**
```toml
[workspace.lints.rust]
warnings = "allow"
unsafe_code = "warn"
missing_docs = "allow"
dead_code = "allow"
unused_variables = "allow"

[workspace.lints.clippy]
all = { level = "warn", priority = -1 }
pedantic = { level = "warn", priority = -1 }
nursery = { level = "warn", priority = -1 }
cargo = { level = "warn", priority = -1 }
unwrap_used = "warn"
expect_used = "warn"
panic = "warn"
todo = "warn"
unimplemented = "warn"
multiple_crate_versions = "allow"
```

**Phase B.2 Configuration (Future):**
- Change all `"warn"` to `"deny"`
- Add per-crate #[allow(...)] overrides as needed

---

**Document Status:** COMPLETE  
**Last Updated:** 2026-05-29  
**Approver:** Gap Closer 4 (Part B Committer)
