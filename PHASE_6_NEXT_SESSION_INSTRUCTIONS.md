# Phase 6: Next Session Setup Instructions
## Quick Start Guide for Phase 6 Continuation

**Date**: January 26, 2026
**Branch**: `claude/optimize-build-times-yi1XR`
**Status**: 🟢 Ready to continue - blocker resolved, path forward clear

---

## ⚡ Critical Setup (First Thing)

### Apply the Linker Fix

**TEMPORARY** (applies only to current shell session):
```bash
export RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld"
```

**PERMANENT** (add to ~/.bashrc or ~/.zshrc):
```bash
# Add this line to your shell configuration file
export RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld"
```

**PRODUCTION** (recommended - add to .cargo/config.toml):
```toml
[target.x86_64-unknown-linux-gnu]
linker = "gcc"
rustflags = ["-C", "link-arg=-fuse-ld=lld"]
```

### Verify Fix Is Applied
```bash
# Should compile proc-macros successfully
RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld" cargo check -p proc-macro2 2>&1 | head -20
# Expected: "Checking proc-macro2 v1.0.106" (or similar success message)

# If it shows "error: cannot produce proc-macro" → linker fix NOT applied
```

---

## 📋 Current Session Status

### ✅ Completed Work (Previous Session)

| Task | Status | Evidence |
|------|--------|----------|
| Blocker root cause found | ✅ RESOLVED | `RUSTFLAGS="-C linker=gcc"` fixes linker issue |
| Cargo.toml duplicate keys | ✅ FIXED | proptest, chicago-tdd-tools, fake consolidated |
| ggen-auth bitflags serde | ✅ FIXED | Added features=['serde'] to bitflags v2.4 |
| ggen-config Debug traits | ✅ FIXED | Added #[derive(Debug)] to 2 structs |
| Comprehensive documentation | ✅ COMPLETE | 4 docs + 6 commits ready |
| 25+ errors catalogued | ✅ COMPLETE | Prioritized by impact |

### 🔴 Current Blocker Status

```
🔴 BLOCKER: Proc-macro linker configuration
   - Affects: ALL proc-macro crates (async-trait, proc-macro2, serde, etc.)
   - Solution: RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld"
   - Status: RESOLVED (pending RUSTFLAGS application)
   - Root cause: Cargo default linker incompatible with container
```

### ⏳ In Progress / Pending

```
⏳ Phase 6 Post-Blocker Work (8-11 hours estimated)
   1. Fix ggen-dspy type annotations (17 errors - 1-2 hours) ← PRIORITY 1
   2. Fix remaining errors (20+ - 3-5 hours)
   3. Run test suite (350+ tests - 2-3 hours)
   4. Verify SLOs (11 metrics - 1 hour)
```

---

## 🎯 Recommended Next Steps

### Step 1: Verify Blocker Fix (5 minutes)
```bash
# Apply linker fix
export RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld"

# Verify workspace compiles
cargo check --workspace 2>&1 | tail -100

# Expected: Compilation proceeds through all proc-macros successfully
# Then fails on ggen-dspy with type annotation errors (expected - these are real errors to fix)
```

### Step 2: Fix ggen-dspy Type Annotations (1-2 hours)
**CRITICAL**: This is the highest-priority error because it blocks other crates from compiling.

```bash
# Check current error details
RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld" cargo check --workspace 2>&1 | grep -A 10 "error\[E0282\]"

# Expected errors in:
# - crates/ggen-dspy/src/modules/predictor.rs (lines 142, 156)
# - crates/ggen-dspy/src/modules/react.rs (lines 85, 93, 101)
# - Other closure type inference errors

# Fix pattern:
# Before: let field_name = output_field.name();
# After:  let field_name: OutputField = output_field.name();
#         (add explicit type annotation to closure parameters)
```

### Step 3: Complete Error Fixing (3-5 hours)
Once ggen-dspy is fixed, continue with:
- ggen-cli-lib: 5 import path errors (1-2 hours)
- ggen-payments: 7 unused variable warnings (<1 hour)
- ggen-e2e: 4 unused import warnings (<1 hour)
- ggen-auth: 2 unused imports (<1 hour)
- ggen-tps-andon: 14 debug/doc warnings (may be skipped if excluded)
- Global: 481 unwrap/expect violations (2-3 hours, if time permits)

### Step 4: Validation (3+ hours)
```bash
# Run full test suite
RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld" cargo make test

# Run SLO verification
RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld" cargo make slo-check

# Pre-commit verification
RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld" cargo make pre-commit
```

---

## 📚 Documentation Reference

All Phase 6 documentation is in the repo:

1. **PHASE_6_BLOCKER_ROOT_CAUSE_ANALYSIS.md** (230+ lines)
   - Deep technical investigation
   - 5 hypotheses tested
   - Evidence of workspace-specific nature

2. **PHASE_6_BLOCKER_RESOLVED.md** (180+ lines)
   - Solution guide with implementation options
   - Timeline for post-blocker work
   - Recommendations for permanent setup

3. **PHASE_6_SESSION_COMPLETE.md** (280+ lines)
   - Comprehensive session summary
   - All commits documented
   - Performance metrics and timelines

4. **PHASE_6_NEXT_SESSION_INSTRUCTIONS.md** (THIS FILE)
   - Quick setup guide
   - Immediate action items
   - Verification steps

---

## 🔧 Useful Commands (with Linker Fix Applied)

```bash
# Apply linker fix for this session
export RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld"

# Quick checks
cargo check --workspace           # Compilation check
cargo check -p ggen-dspy         # Single crate check
cargo check -p ggen-config       # Verify fixes are in place

# Testing
cargo test --lib                 # Unit tests only (fast)
cargo test                        # All tests (full)
cargo make test                  # Via Makefile.toml

# Linting and formatting
cargo fmt                         # Format code
cargo clippy -- -D warnings      # Clippy linting

# Build artifacts
cargo build --release            # Production build
cargo build --profile release-with-debug  # With debug symbols

# View specific errors
cargo check --workspace 2>&1 | grep "error\[" | head -20

# Git operations
git log --oneline -10            # Recent commits
git status                       # Current state
git diff HEAD~1                 # View last commit changes
```

---

## ✅ Session Start Checklist

Before starting Phase 6 post-blocker work:

- [ ] Read this document completely
- [ ] Apply RUSTFLAGS: `export RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld"`
- [ ] Verify fix with: `cargo check -p proc-macro2` (should compile)
- [ ] Read PHASE_6_BLOCKER_ROOT_CAUSE_ANALYSIS.md for context
- [ ] Read PHASE_6_BLOCKER_RESOLVED.md for solution details
- [ ] Check git status: `git log -1` and verify latest commits
- [ ] Understand error priorities: ggen-dspy = Priority 1 (blocks others)

---

## ⚠️ Common Issues & Solutions

### Issue: `error: cannot produce proc-macro`
**Solution**: RUSTFLAGS not applied. Run:
```bash
export RUSTFLAGS="-C linker=gcc -C link-arg=-fuse-ld=lld"
```

### Issue: `error: type annotations needed [E0282]`
**Cause**: Real pre-existing error in ggen-dspy closures
**Solution**: Add explicit type annotations to closure parameters
**Example**:
```rust
// Before:
if !signature.outputs.iter().any(|f| f.name() == "thought") {

// After:
if !signature.outputs.iter().any(|f: &OutputField| f.name() == "thought") {
```

### Issue: `warning: unused variable`
**Solution**: Prefix with underscore:
```rust
// Before: let temp_dir = fixture.copy_to_temp()?;
// After:  let _temp_dir = fixture.copy_to_temp()?;
```

### Issue: Tests still failing after fixes
**Solution**: Run with verbose output:
```bash
RUSTFLAGS="-C linker=gcc" cargo test -- --nocapture 2>&1 | head -100
```

---

## 📊 Estimated Timeline

| Phase | Task | Duration | Status |
|-------|------|----------|--------|
| 0 | Setup + blocker verification | 5-10 min | Ready |
| 1 | Fix ggen-dspy (17 errors) | 1-2 hrs | Next priority |
| 2 | Fix remaining errors (20+) | 3-5 hrs | Follows Phase 1 |
| 3 | Run test suite (350+ tests) | 2-3 hrs | Validation |
| 4 | SLO verification (11 metrics) | 1 hr | Final check |
| 5 | Review + commit | 30 min | Finalization |
| **Total** | **Phase 6 completion** | **8-11 hrs** | **Post-blocker work** |

**Previous Session**: 4 hours (blocker resolution)
**Overall Phase 6**: 12-15 hours total

---

## 🚀 Success Criteria

Phase 6 is complete when:

- ✅ All proc-macros compile successfully (RUSTFLAGS applied)
- ✅ All 25+ pre-existing errors are fixed
- ✅ 350+ Chicago TDD tests pass (0 failures)
- ✅ 11 SLO targets verified (build time, memory, etc.)
- ✅ No compiler warnings (clippy -D warnings)
- ✅ Code formatted (cargo fmt)
- ✅ All changes committed with evidence
- ✅ Comprehensive documentation updated

---

## 📞 Questions?

Refer to:
- **Root Cause Details**: PHASE_6_BLOCKER_ROOT_CAUSE_ANALYSIS.md
- **Solution Details**: PHASE_6_BLOCKER_RESOLVED.md
- **Session History**: PHASE_6_SESSION_COMPLETE.md
- **Error Priorities**: Look at git log for documented error analysis

---

**Ready to Continue?**
1. Apply RUSTFLAGS fix
2. Run `cargo check --workspace` to see current status
3. Start with ggen-dspy (Priority 1)
4. Follow the error fixing roadmap

Good luck! 🚀
