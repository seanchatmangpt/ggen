<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Next Session Handoff](#next-session-handoff)
  - [Immediate State](#immediate-state)
    - [Branch Status](#branch-status)
    - [What's Committed](#whats-committed)
    - [What's Not Committed](#whats-not-committed)
  - [Work Queue Priority Order](#work-queue-priority-order)
    - [Tier 1: Immediate Unblocking (Choose One Path)](#tier-1-immediate-unblocking-choose-one-path)
      - [Path A: Rest-Gate Cleanup (Recommended First)](#path-a-rest-gate-cleanup-recommended-first)
      - [Path B: Wave 2 Phase 1 P0 Blockers (Parallel)](#path-b-wave-2-phase-1-p0-blockers-parallel)
      - [Path C: Foundation Factory Design (Strategic)](#path-c-foundation-factory-design-strategic)
  - [Lint Enforcement Roadmap](#lint-enforcement-roadmap)
    - [Phase B.1 Status: COMPLETE ✅](#phase-b1-status-complete-)
    - [Phase B.2: Deny-Enforcement (v26.5.30)](#phase-b2-deny-enforcement-v26530)
    - [Phase B.3: TRUTH-LSP-1 (v26.5.31+)](#phase-b3-truth-lsp-1-v26531)
  - [Git Cleanup](#git-cleanup)
  - [Documentation Anchors](#documentation-anchors)
  - [TRUTH-LSP-1 Deferral Note](#truth-lsp-1-deferral-note)
  - [Recommended Session Structure](#recommended-session-structure)
  - [One Last Check](#one-last-check)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Next Session Handoff

**From:** v26.5.29 closeout  
**To:** v26.5.30 or Wave 2 Phase 1  
**Date:** 2026-05-29  

---

## Immediate State

### Branch Status
- **Current:** main
- **HEAD:** commit 3876e809 (Merge PR #193: feat/plugin-fake-detection)
- **All feature branches merged to main**

### What's Committed
- Full Phase 5 Wave 2 planning (18+ design docs)
- Workspace-level fake-detection lints (all 15 crates configured)
- Part B: warn-mode lint foundation (Phase B.1 complete)
- v26.5.29 closeout receipt (this directory)

### What's Not Committed
- Untracked: `crates/test_med_findings/` — inspect if needed before cleanup
- No breaking changes
- No partial implementations

---

## Work Queue Priority Order

### Tier 1: Immediate Unblocking (Choose One Path)

#### Path A: Rest-Gate Cleanup (Recommended First)
**Time:** 12–16 hours  
**Files affected:** `tests/**/*.rs`, doctests, `examples/**`  
**Goal:** Make `cargo test --workspace` fully pass  
**Known issues:**
- 45 mechanical doctest failures (classes: imports, feature gates, fixtures)
- Categorized in earlier audit (docs/crate-audits/AUDIT_DASHBOARD.md)

**Entry command:**
```bash
# Triage the 45 failures by class
cargo test --workspace 2>&1 | grep "^test " | sort | uniq -c | head -20
```

#### Path B: Wave 2 Phase 1 P0 Blockers (Parallel)
**Time:** 29.5 hours (can run parallel to Path A)  
**Scope:** Fix 4 blockers:
- P0-01: SHACL validation (14h)
- P0-02: Pipeline architecture (7h)
- P0-03: Namespace conflicts (5h)
- P0-04: Error type losslessness (3.5h)

**Entry:** See docs/WAVE2_PHASE1_QUICK_START.md

#### Path C: Foundation Factory Design (Strategic)
**Time:** 20–24 hours (design phase only)  
**Scope:** Formalize Explore → Admission → Exploit lifecycle  
**Defers:** Rest-gate and P0 blockers; begins Phase 6

**Recommendation:** **Start with Path A** (mechanical cleanup is fastest), then **run Path B in parallel** (P0 blockers are independent).

---

## Lint Enforcement Roadmap

### Phase B.1 Status: COMPLETE ✅
- Workspace lints defined in root Cargo.toml
- All 15 crates configured with `[lints] workspace = true`
- Warn-mode active (not blocking compilation)
- Inventory of violations complete (~314 missing_docs warnings)

### Phase B.2: Deny-Enforcement (v26.5.30)
**Time:** 8–12 hours  
**Work:**
1. Fix top 100 missing_docs violations (add /// docs to public APIs)
2. Flip workspace.lints.rust/clippy from "warn" to "deny"
3. Run `cargo make check` to green
4. Commit: "feat(lints): enforce workspace-level fake-detection and documentation"

### Phase B.3: TRUTH-LSP-1 (v26.5.31+)
**Deferred.** See design in latest conversation.

---

## Git Cleanup

Before next session:
```bash
# Remove untracked experimental dir (if not needed)
rm -rf crates/test_med_findings/

# Verify clean tree
git status
```

---

## Documentation Anchors

**If resuming on Path A (Rest-Gate):**
- Audit: `docs/crate-audits/AUDIT_DASHBOARD.md`
- Doctest inventory: `docs/DOCTEST_INVENTORY.md` (if exists, else create)

**If resuming on Path B (P0 Blockers):**
- Quick start: `docs/WAVE2_PHASE1_QUICK_START.md`
- Harnesses: `docs/P0BlockerTestHarnesses.md`
- Blocker specs: `docs/ACTIVATION_PRIORITY_DESIGN.md`

**If resuming on Path C (Foundation Factory):**
- Previous design work in conversation context
- Recommendation: Create a new speckit spec for Phase 6 foundation

---

## TRUTH-LSP-1 Deferral Note

The LSP BS detector (TRUTH-LSP-1) has a complete design:
- 8 diagnostic codes (BS0001-BS0008)
- Surface-aware classification rules
- Code action routing via MCP repair
- Live typing detection + OCEL evidence

**Status:** Design complete, implementation deferred pending:
1. Completion of Path A (rest-gate) OR
2. Strategic decision on Path C (Foundation Factory priority)

**Location:** Conversation history up to 2026-05-29 17:40 UTC contains full TRUTH-LSP-1 specification.

---

## Recommended Session Structure

**Hour 1:** Choose path A, B, or C. If Path A: triage 45 failures by class, create task list.

**Hour 2–N:** Execute chosen path. Phase B P0 blocks (Path B) can run in parallel.

**Final Hour:** Write v26.5.30 closeout receipt.

---

## One Last Check

Before closing next session:
```bash
git log --oneline -5
git status
cargo make check  # Should pass with warnings if on Path A or B
```

If these three are clean, the next handoff is ready.

---

**Clock out and resume tomorrow. v26.5.29 is closed.**
