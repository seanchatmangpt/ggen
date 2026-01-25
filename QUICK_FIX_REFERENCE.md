# Quick Fix Reference - Build Optimization Production Validation

**Use this file as a quick checklist while fixing the critical blockers**

---

## BLOCKER #1: Fix Cargo.toml Duplicate Declarations

### Step 1: Fix ggen-ai duplicate

```bash
# View current state
grep -n "ggen-ai\s*=" /home/user/ggen/Cargo.toml

# You should see TWO lines (both need to be removed, keep only optional):
# Line ~151: ggen-ai = { path = "crates/ggen-ai", version = "0.2.0" }
# Line ~294: ggen-ai = { path = "crates/ggen-ai", version = "0.2.0", optional = true }

# Edit Cargo.toml:
# 1. REMOVE line 294 (optional = true version if it's duplicate)
# 2. CHANGE line 151 to include optional = true:
#    ggen-ai = { path = "crates/ggen-ai", version = "0.2.0", optional = true }
```

### Step 2: Fix ggen-dspy duplicate

```bash
# View current state
grep -n "ggen-dspy\s*=" /home/user/ggen/Cargo.toml

# You should see TWO lines:
# Line ~151: ggen-dspy = { path = "crates/ggen-dspy", version = "0.2.0" }
# Line ~383: ggen-dspy = { path = "crates/ggen-dspy", version = "0.2.0", optional = true }

# Edit Cargo.toml:
# 1. REMOVE line 383 (optional = true version if it's duplicate)
# 2. CHANGE line 151 to include optional = true:
#    ggen-dspy = { path = "crates/ggen-dspy", version = "0.2.0", optional = true }
```

### Step 3: Verify fixes

```bash
# Should return exactly 1 match for each
grep -c "ggen-ai\s*=" /home/user/ggen/Cargo.toml    # Should be: 1
grep -c "ggen-dspy\s*=" /home/user/ggen/Cargo.toml  # Should be: 1

# Should show optional = true for both
grep "ggen-ai\|ggen-dspy" /home/user/ggen/Cargo.toml | grep optional
```

### Step 4: Test Cargo.toml parses

```bash
timeout 15s cargo metadata --format-version 1 > /dev/null 2>&1
echo "Exit code: $?"
# Should be: 0 (success)
```

---

## BLOCKER #2: Install cargo-make

```bash
# Install
cargo install cargo-make --locked

# Verify (should print version)
cargo-make --version

# List tasks (should show many tasks)
cargo make --list | head -20
```

---

## BLOCKER #3: Verify Project Compiles

```bash
# Quick test (should complete without parse errors)
timeout 30s cargo check --lib --no-default-features 2>&1 | tail -20
```

---

## SECONDARY ISSUE #1: Fix Unwrap/Expect Violations

### Files to fix:

1. **crates/ggen-core/src/audit/mod.rs**
   ```rust
   // BEFORE:
   let json = audit.to_json().expect("Failed to serialize");

   // AFTER:
   let json = audit.to_json()?;
   ```

2. **crates/ggen-core/src/audit/writer.rs** (3 locations)
   ```rust
   // BEFORE:
   let temp_dir = TempDir::new().expect("Failed to create temp dir");
   AuditTrailWriter::write(&audit, &output_path).expect("Failed to write audit trail");
   let content = fs::read_to_string(&output_path).expect("Failed to read audit.json");

   // AFTER:
   let temp_dir = TempDir::new()?;
   AuditTrailWriter::write(&audit, &output_path)?;
   let content = fs::read_to_string(&output_path)?;
   ```

### Verify fixes:

```bash
# Should show 0 violations (excluding tests and #[allow] attributes)
grep -r "\.expect(" crates/ggen-core/src/audit --include="*.rs" | \
  grep -v "test\|#\[allow"
```

---

## SECONDARY ISSUE #2: Run Security Audit

```bash
# Install
cargo install cargo-audit

# Run
cargo audit

# Report any vulnerabilities found
```

---

## Quick Validation Checklist

```
PHASE 1: Critical Blockers
[ ] Cargo.toml: ggen-ai has only 1 declaration (with optional = true)
[ ] Cargo.toml: ggen-dspy has only 1 declaration (with optional = true)
[ ] Cargo.toml: cargo metadata succeeds
[ ] cargo-make installed and working
[ ] cargo check --lib completes (even if slow)

PHASE 3: Code Quality
[ ] audit/mod.rs: expect() replaced with Result
[ ] audit/writer.rs: 3x expect() replaced with Result
[ ] cargo lint passes
[ ] cargo audit passed

Ready to Merge When:
[ ] cargo make check works
[ ] cargo make test passes
[ ] cargo make lint passes
[ ] cargo audit clean
```

---

## Rollback If Needed

```bash
# If you mess up, revert to last known good state
git checkout HEAD -- Cargo.toml
git status  # Should be clean
```

---

## Quick Performance Check (After Phase 1)

```bash
# Time each operation
time cargo make check          # Should show time
time cargo make test-unit      # Should show time
time cargo make lint           # Should show time

# Compare to SLO targets:
# - check: target 5-10s (actual probably 30-60s or more)
# - test-unit: target 150s (actual probably 60-120s)
# - lint: target 90s (actual probably 30-60s)
```

---

## Git Workflow

```bash
# Create feature branch (if not already done)
git checkout -b fix/build-optimization-blockers

# After fixing Phase 1 blockers
git add Cargo.toml
git commit -m "fix(cargo): Resolve Cargo.toml duplicate dependency declarations

- Remove duplicate ggen-ai optional declaration
- Remove duplicate ggen-dspy optional declaration
- Verify cargo metadata parses successfully

Andon Signal: 🔴 → 🟡 (compilation now possible)"

# After Phase 3 fixes
git add crates/ggen-core/src/audit/
git commit -m "fix(audit): Replace unguarded expect() with Result<T,E>

- audit/mod.rs: serialization now returns Result
- audit/writer.rs: I/O operations now return Result

Andon Signal: 🟡 → 🟢 (no production panics)"

# After all phases complete
git push origin fix/build-optimization-blockers
# Create PR to main with full validation report attached
```

---

## Contact Points

**If you get stuck:**

1. Check `PRODUCTION_VALIDATION_REPORT_2026-01-25.md` for detailed analysis
2. Check `PRODUCTION_VALIDATION_REMEDIATION_CHECKLIST.md` for complete task breakdown
3. Review error messages carefully - they usually indicate exactly what's wrong
4. For Cargo.toml issues: `cargo metadata --format-version 1` gives best error messages
5. For code issues: `cargo clippy --fix` can auto-fix some violations

---

**Status**: Ready to start Phase 1
**Last Updated**: 2026-01-25
**Estimated Time to Complete**: 6-10 hours total, Phase 1 alone = 30 minutes
